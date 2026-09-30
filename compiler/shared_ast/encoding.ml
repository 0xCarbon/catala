(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria, contributor:
   Vincent Botbol <vincent.botbol@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
open Definitions
module Runtime = Catala_runtime
module Val = Runtime.Value
open Json_encoding

let bool_encoding : Val.t encoding =
  conv
    (function
      | Val.V (Bool, v) -> (v : bool)
      | v ->
        Message.error ~internal:true
          "Unexpected runtime value %a instead of bool while encoding to JSON"
          Val.format v)
    (fun v -> Val.V (Val.Bool, v))
    bool

let unit_encoding : Val.t encoding =
  conv
    (function
      | Val.V (Unit, ()) -> ()
      | v ->
        Message.error ~internal:true
          "Unexpected runtime value %a instead of unit while encoding to JSON"
          Val.format v)
    (fun () -> Val.V (Unit, ()))
    empty

(* JSON documents are read with {!Runtime.ParsedJson}, which keeps the literal of
   every number: integers, decimals and money are decoded from that literal,
   exactly, instead of from the binary float that [Json_encoding]'s generic
   view of a number provides. *)
module Exact_repr : Json_repr.Repr with type value = Runtime.ParsedJson.t =
struct
  type value = Runtime.ParsedJson.t

  let view : value -> value Json_repr.view = function
    | Null -> `Null
    | Bool b -> `Bool b
    | Number n -> `Float (float_of_string n)
    | String s -> `String s
    | Array l -> `A l
    | Object l -> `O l

  let repr : value Json_repr.view -> value = function
    | `Null -> Null
    | `Bool b -> Bool b
    | `Float f -> Number (Printf.sprintf "%.17g" f)
    | `String s -> String s
    | `A l -> Array l
    | `O l -> Object l

  let repr_uid = Json_repr.repr_uid ()
end

module Exact_encoding = Json_encoding.Make (Exact_repr)

let json_kind : Runtime.ParsedJson.t -> string = function
  | Null -> "null"
  | Bool _ -> "boolean"
  | Number _ -> "number"
  | String _ -> "string"
  | Array _ -> "array"
  | Object _ -> "object"

(* [Json_encoding.union] decodes with the first case that matches, but its schema
   combines the cases with [oneOf], which requires exactly one to match. Cases
   may overlap (a JSON integer is also a JSON number; an optional field of type
   [optional of T] accepts both a [T] and an optional), and JSON schema
   validators then refused inputs that Catala accepts, e.g. [{"amount": 12}]
   for money. Alternatives are described with [anyOf] instead. *)
let any_case ?description enc proj inj =
  (* Wrapped so that a definition is referenced, not inlined *)
  let schema = Json_encoding.schema (conv Fun.id Fun.id enc) in
  let schema =
    match description with
    | None -> schema
    | Some _ ->
      Json_schema.update { (Json_schema.root schema) with description } schema
  in
  schema, case ?description enc proj inj

let first_match (cases : (Json_schema.schema * 'a case) list) : 'a encoding =
  let schema =
    let defs, roots =
      List.fold_left
        (fun (defs, roots) (s, _) ->
          let defs, s = Json_schema.merge_definitions (defs, s) in
          defs, Json_schema.root s :: roots)
        (Json_schema.any, []) cases
    in
    Json_schema.update
      (Json_schema.element (Combine (Any_of, List.rev roots)))
      defs
  in
  conv Fun.id Fun.id ~schema (union (List.map snd cases))

(* Patterns of the numeric strings, as read by [Runtime.ParsedJson] *)
let number_pattern = "-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?"
let integer_pattern = "^-?[0-9]+$"
let money_pattern = "^" ^ number_pattern ^ "$"
let decimal_pattern = "^" ^ number_pattern ^ "$|^-?[0-9]+/[0-9]*[1-9][0-9]*$"

(** A Catala number read from a JSON number or a numeric string, both given by
    their text, strictly as the schema describes them. *)
let exact_numeric_encoding
    ~(number : Json_schema.element_kind list)
    ~(pattern : string)
    ~(expected : string)
    ~(write : Val.t -> Runtime.ParsedJson.t)
    ~(of_number : string -> Val.t option)
    ~(of_string : string -> Val.t option) : Val.t encoding =
  let schema =
    Json_schema.(
      create
        (element
           (Combine
              ( Any_of,
                List.map element number
                @ [
                    element (String { string_specs with pattern = Some pattern });
                  ] ))))
  in
  let unexpected got =
    raise
      (Json_encoding.Cannot_destruct
         ([], Json_encoding.Unexpected (got, expected)))
  in
  Exact_encoding.custom write
    (function
      | Number n -> (
        match of_number n with
        | Some v -> v
        | None -> unexpected (Printf.sprintf "number %s" n))
      | String s -> (
        match of_string s with
        | Some v -> v
        | None -> unexpected (Printf.sprintf "string %S" s))
      | j -> unexpected (json_kind j))
    ~schema

let int_encoding : Val.t encoding =
  def "integer" ~title:"Catala Integer"
  @@ exact_numeric_encoding
       ~number:Json_schema.[Integer numeric_specs]
       ~pattern:integer_pattern
       ~expected:"an integer (integral number or string of digits)"
       ~write:(function
         | Val.V (Integer, z) -> String (Z.to_string z)
         | v ->
           Message.error ~internal:true
             "Unexpected runtime value %a instead of int while encoding to JSON"
             Val.format v)
       ~of_number:(fun n ->
         Option.map
           (fun z -> Val.V (Integer, z))
           (Runtime.ParsedJson.integer_of_number n))
       ~of_string:(fun s ->
         Option.map
           (fun z -> Val.V (Integer, z))
           (Runtime.ParsedJson.integer_of_string s))

let money_encoding : Val.t encoding =
  def "money" ~title:"Catala Money"
  @@
  let q_100 = Q.of_int 100 in
  (* Amounts are in units; digits beyond the cent are truncated *)
  let money q = Val.V (Money, Q.to_bigint (Q.mul q q_100)) in
  exact_numeric_encoding
    ~number:Json_schema.[Number numeric_specs]
    ~pattern:money_pattern ~expected:"an amount (number or numeric string)"
    ~write:(function
      | Val.V (Money, z) -> String (Runtime.money_to_string z)
      | v ->
        Message.error ~internal:true
          "Unexpected runtime value %a instead of money while encoding to JSON"
          Val.format v)
    ~of_number:(fun n ->
      Option.map money (Runtime.ParsedJson.decimal_of_string n))
    ~of_string:(fun s ->
      if Runtime.ParsedJson.is_number_literal s then
        Option.map money (Runtime.ParsedJson.decimal_of_string s)
      else None)

let rat_encoding : Val.t encoding =
  def "decimal" ~title:"Catala Decimal"
  @@ exact_numeric_encoding
       ~number:Json_schema.[Number numeric_specs]
       ~pattern:decimal_pattern
       ~expected:"a decimal (number, numeric string or fraction)"
       ~write:(function
         | Val.V (Decimal, d) -> String (Q.to_string d)
         | v ->
           Message.error ~internal:true
             "Unexpected runtime value %a instead of decimal while encoding to \
              JSON"
             Val.format v)
       ~of_number:(fun n ->
         Option.map
           (fun q -> Val.V (Decimal, q))
           (Runtime.ParsedJson.decimal_of_string n))
       ~of_string:(fun s ->
         Option.map
           (fun q -> Val.V (Decimal, q))
           (Runtime.ParsedJson.decimal_of_string s))

(* A Catala integer in JSON held as a machine integer (the year of a date, the
   components of a duration). One that does not fit raises the runtime error
   [IntegerOverflow], whose bound depends on the backend. *)
let machine_int_encoding : int encoding =
  conv
    (fun i -> Val.V (Integer, Z.of_int i))
    (function
      | Val.V (Integer, z) when Z.fits_int z -> Z.to_int z
      | Val.V (Integer, _) ->
        raise
          (Json_encoding.Cannot_destruct
             ([], Runtime.Error (IntegerOverflow, [], None)))
      | v ->
        Message.error ~internal:true
          "Unexpected runtime value %a instead of int while decoding JSON"
          Val.format v)
    int_encoding

(* The strings [Dates_calc.date_of_string] reads: exactly those
   [Dates_calc.format_date] writes *)
let date_pattern =
  "^(?!-0000-)-?([0-9]{4}|[1-9][0-9]{4,})-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"

let date_encoding : Val.t encoding =
  let date_string =
    conv Fun.id Fun.id
      ~schema:
        Json_schema.(
          create
            (element (String { string_specs with pattern = Some date_pattern })))
      string
  in
  let date_obj =
    obj3
      (req "year" machine_int_encoding)
      (req "month" (ranged_int ~minimum:1 ~maximum:12 "months"))
      (req "day" (ranged_int ~minimum:1 ~maximum:31 "days"))
  in
  def "date" ~title:"Catala date"
  @@ first_match
       [
         any_case
           ~description:
             "Accepts strings with the following format: YYYY-MM-DD, e.g., \
              \"1970-01-31\", the year having more digits beyond 9999 and a \
              minus sign before 0 (\"-0738-02-03\")"
           date_string
           (function
             | Val.V (Date, d) ->
               Some (Format.asprintf "%a" Dates_calc.format_date d)
             | v ->
               Message.error ~internal:true
                 "Unexpected runtime value %a instead of date while encoding \
                  to JSON"
                 Val.format v)
           (fun s ->
             let fail e = raise (Json_encoding.Cannot_destruct ([], e)) in
             try Val.V (Date, Dates_calc.date_of_string s) with
             | Invalid_argument _ | Dates_calc.InvalidDate ->
               fail (Failure (Printf.sprintf "invalid date %S" s))
             | Dates_calc.Overflow ->
               fail (Runtime.Error (IntegerOverflow, [], None)));
         any_case
           ~description:
             "Accepts date objects: {\"year\":<int>, \"month\":<int>, \
              \"day\":<int>}"
           date_obj
           (function
             | Val.V (Date, d) -> Some (Dates_calc.date_to_ymd d)
             | v ->
               Message.error ~internal:true
                 "Unexpected runtime value %a instead of date while encoding \
                  to JSON"
                 Val.format v)
           (fun (year, month, day) ->
             Val.V (Date, Dates_calc.make_date ~year ~month ~day));
       ]

let duration_encoding : Val.t encoding =
  def "duration" ~title:"Catala duration"
  @@
  let encoding =
    obj3
      (dft "years" machine_int_encoding 0)
      (dft "months" machine_int_encoding 0)
      (dft "days" machine_int_encoding 0)
    |> conv
         (function
           | Val.V (Duration, d) -> Dates_calc.period_to_ymds d
           | v ->
             Message.error ~internal:true
               "Unexpected runtime value %a instead of duration while encoding \
                to JSON"
               Val.format v)
         (fun (years, months, days) ->
           Val.V (Duration, Dates_calc.make_period ~years ~months ~days))
  in
  encoding

let position_encoding =
  def "position" ~title:"Catala position"
  @@
  let p_encoding = obj2 (req "line" int32) (req "character" int32) in
  let range_encoding = obj2 (req "start" p_encoding) (req "end" p_encoding) in
  obj2 (req "file" string) (req "range" range_encoding)
  |> conv
       (function
         | Val.V (Position, pos) ->
           ( pos.filename,
             ( (Int32.of_int pos.start_line, Int32.of_int pos.start_column),
               (Int32.of_int pos.end_line, Int32.of_int pos.end_column) ) )
         | v ->
           Message.error ~internal:true
             "Unexpected runtime value %a instead of position while encoding \
              to JSON"
             Val.format v)
       (fun (file, ((sl, sc), (el, ec))) ->
         Val.V
           ( Position,
             {
               filename = file;
               start_line = Int32.to_int sl;
               start_column = Int32.to_int sc;
               end_line = Int32.to_int el;
               end_column = Int32.to_int ec;
               law_headings = [];
             } ))

let make_constant s : Val.t encoding =
  conv
    (function
      | Val.V (Unit, ()) -> ()
      | v ->
        Message.error ~internal:true
          "Unexpected runtime value %a instead of unit while encoding to JSON"
          Val.format v)
    (fun () -> Val.V (Unit, ()))
    (constant s)

let generate_lit_encoding (typ_lit : typ_lit) : Val.t encoding =
  match typ_lit with
  | TBool -> bool_encoding
  | TUnit -> unit_encoding
  | TInt -> int_encoding
  | TRat -> rat_encoding
  | TDate -> date_encoding
  | TDuration -> duration_encoding
  | TMoney -> money_encoding
  | TPos -> position_encoding

let rec generate_encoder (ctx : decl_ctx) (typ : typ) : Val.t encoding =
  match Mark.remove typ with
  | TError -> assert false
  | TLit tlit -> generate_lit_encoding tlit
  | TTuple [typ; (TLit TPos, _)] -> generate_encoder ctx typ
  | TTuple tl -> generate_tuple_encoder ctx tl
  | TStruct sname -> generate_struct_encoder ctx sname
  | TEnum ename -> generate_enum_encoder ctx ename
  | TOption typ -> generate_option_encoder ctx typ
  | TArray typ -> generate_array_encoder ctx typ
  | TArrow _ -> Message.error "Cannot convert functional values from JSON"
  | TDefault _ -> Message.error "Cannot encode 'default' types"
  | TVar _ -> Message.error "Cannot encode 'variable' types"
  | TForAll _ -> Message.error "Cannot encode 'for-all' types"
  | TClosureEnv -> Message.error "Cannot encode 'closure-env' types"
  | TAbstract _ -> Message.error "Cannot encode 'abstract' types"

and generate_array_encoder (ctx : decl_ctx) typ : Val.t encoding =
  let open Val in
  conv
    (function
      | V (Array t, elts) -> Array.map (embed t) elts
      | v ->
        Message.error ~internal:true
          "Unexpected runtime value %a instead of array while encoding to JSON"
          format v)
    (fun a -> V (Array Dynamic, a))
    (array (generate_encoder ctx typ))

and generate_option_encoder ctx typ =
  let open Val in
  let proj_none = function
    | V (Enum { name = "Optional" | "Optionnel"; constr; _ }, v) -> (
      match constr v with _, _, None -> Some (V (Unit, ())) | _ -> None)
    | _ -> None
  in
  let vtyp =
    Enum
      {
        name = "Optional";
        constr =
          (function None -> 0, "Absent", None | Some x -> 1, "Present", Some x);
        cases = [];
      }
  in
  let proj_null = function
    | V (Enum { name = "Optional" | "Optionnel"; constr; _ }, v) -> (
      match constr v with _, _, None -> Some () | _ -> None)
    | _ -> None
  in
  let inj_none _ = V (vtyp, None) in
  first_match
    [
      any_case unit_encoding proj_none inj_none;
      any_case null proj_null inj_none;
      any_case (make_constant "Absent") proj_none inj_none;
      any_case
        (obj1 (req "Present" (generate_encoder ctx typ)))
        (function
          | V (Enum en, v) -> (
            match en.constr v with _, _, Some x -> Some x | _ -> None)
          | _ -> None)
        (fun x -> V (vtyp, Some x));
    ]

and generate_tuple_encoder ctx typl =
  let open Val in
  assert (typl <> []);
  let first_tup_enc = tup1 (generate_encoder ctx (List.hd typl)) in
  let add_tuple (acc : t encoding) typ : t encoding =
    let bconv = merge_tups acc (tup1 (generate_encoder ctx typ)) in
    conv
      (function
        | V (Tuple (tf, _), elts) -> (
          match tf elts with
          | [x1; x2] -> x1, x2
          | arr ->
            let rarr = List.rev arr in
            ( V (Tuple (Fun.id, Unbuildable), List.rev (List.tl rarr)),
              List.hd rarr ))
        | v ->
          Message.error ~internal:true
            "Unexpected runtime value %a instead of tuple while encoding to \
             JSON"
            format v)
      (function
        | V (Tuple (tf, _), arr), rval ->
          V (Tuple (Fun.id, Unbuildable), tf arr @ [rval])
        | v, rval ->
          (* First element reached *)
          V (Tuple (Fun.id, Unbuildable), v :: [rval]))
      bconv
  in
  List.fold_left (fun e typ -> add_tuple e typ) first_tup_enc (List.tl typl)

and generate_struct_encoder (ctx : decl_ctx) (sname : StructName.t) =
  let open Val in
  let struc = StructName.Map.find sname ctx.ctx_structs in
  let bdgs = StructField.Map.bindings struc in
  let is_input_scope_struct =
    ScopeName.Map.exists
      (fun _ { in_struct_name; _ } -> StructName.equal sname in_struct_name)
      ctx.ctx_scopes
  in
  let rename_field f =
    let field_s = StructField.original_string f in
    if
      is_input_scope_struct
      && String.ends_with ~suffix:"_in" field_s
      && String.length field_s > 3
    then String.sub field_s 0 (String.length field_s - 3), field_s
    else field_s, field_s
  in
  let empty_struct_enc =
    conv
      (fun _ -> ())
      (fun () ->
        V
          ( Struct
              {
                name = StructName.original_base sname;
                fields = (fun _ -> []);
                build = Unbuildable;
              },
            () ))
      empty
  in
  let add_req_field (encoding : t encoding) (sf, typ) : t encoding =
    let field_label, field_s = rename_field sf in
    let bconv =
      merge_objs encoding (obj1 (req field_label (generate_encoder ctx typ)))
    in
    conv
      (function
        | V (Struct enc, data) as v ->
          let rval = List.assoc field_s (enc.fields data) in
          v, rval
        | v ->
          Message.error ~internal:true
            "Unexpected runtime value %a instead of struct while encoding to \
             JSON"
            format v)
      (function
        | V (Struct enc, data), rval ->
          V
            ( Struct { name = enc.name; fields = Fun.id; build = Unbuildable },
              (field_s, rval) :: enc.fields data )
        | _ -> assert false)
      bconv
  in
  let add_opt_field (encoding : t encoding) (sf, typ) : t encoding =
    let field_label, field_s = rename_field sf in
    let all_enc =
      let wrap_present v =
        V
          ( Enum
              {
                name = "Optional";
                constr = (fun _ -> 1, "Present", Some v);
                cases = [];
              },
            () )
      in
      let unwrap_present = function
        | V (Enum { name = "Optional"; constr; _ }, v) -> (
          match constr v with 1, "Present", Some v -> Some v | _ -> None)
        | _ -> None
      in
      first_match
        [
          any_case (generate_encoder ctx typ) unwrap_present wrap_present;
          any_case (generate_option_encoder ctx typ) Option.some Fun.id;
        ]
    in
    let bconv : (t * t option) encoding =
      merge_objs encoding (obj1 (opt field_label all_enc))
    in
    let proj : t -> t * t option = function
      | V (Struct enc, data) as v ->
        let rval = List.assoc_opt field_s (enc.fields data) in
        v, rval
      | _ -> assert false
    in
    let inj : t * t option -> t = function
      | V (Struct enc, data), None ->
        V
          ( Struct { name = enc.name; fields = Fun.id; build = Unbuildable },
            ( field_s,
              V
                ( Enum
                    {
                      name = "Optional";
                      constr = (fun _ -> 0, "Absent", None);
                      cases = [];
                    },
                  () ) )
            :: enc.fields data )
      | V (Struct enc, data), Some rval ->
        V
          ( Struct { name = enc.name; fields = Fun.id; build = Unbuildable },
            (field_s, rval) :: enc.fields data )
      | _ -> assert false
    in
    conv proj inj bconv
  in
  def (Format.asprintf "%a" StructName.format_shortpath sname)
  @@ List.fold_left
       (fun e (sf, typ) ->
         match Mark.remove typ with
         | TOption typ | TDefault typ -> add_opt_field e (sf, typ)
         | _ -> add_req_field e (sf, typ))
       empty_struct_enc bdgs

and generate_enum_encoder (ctx : decl_ctx) (ename : EnumName.t) =
  let open Val in
  let enum = EnumName.Map.find ename ctx.ctx_enums in
  let bdgs = EnumConstructor.Map.bindings enum in
  let ename_s = EnumName.original_base ename in
  let make_constructor_case idx (cstr, typ) :
      Json_schema.schema * t Json_encoding.case =
    let cstr_s = EnumConstructor.original_string cstr in
    match Mark.remove typ with
    | TLit TUnit ->
      any_case (constant cstr_s)
        (function
          | V (Enum enc, rval) ->
            let _, cstr_s', _ = enc.constr rval in
            if cstr_s = cstr_s' then Some () else None
          | v ->
            Message.error ~internal:true
              "Unexpected runtime value %a instead of enum while encoding to  \
               JSON"
              format v)
        (fun () ->
          V
            ( Enum { name = ename_s; constr = Fun.id; cases = [] },
              (idx, cstr_s, None) ))
    | _ ->
      any_case
        (obj1
           (req
              (EnumConstructor.original_string cstr)
              (generate_encoder ctx typ)))
        (function
          | V (Enum { name; constr; _ }, rval) ->
            let _, cstr_s', v = constr rval in
            if name = ename_s && cstr_s = cstr_s' then v else None
          | _ -> None)
        (fun v ->
          V
            ( Enum { name = ename_s; constr = Fun.id; cases = [] },
              (idx, cstr_s, Some v) ))
  in
  let enc =
    if List.for_all (fun (_, typ) -> Mark.remove typ = TLit TUnit) bdgs then
      (* This simplifies the JSON schema *)
      let bdgs_idx = List.mapi (fun i x -> i, x) bdgs in
      conv
        (function
          | V (Enum { constr; _ }, x) ->
            let idx, _, _ = constr x in
            idx
          | _ -> assert false)
        (fun idx ->
          let cstr, _ = List.assoc idx bdgs_idx in
          let cstr_s = EnumConstructor.original_string cstr in
          V
            ( Enum { name = ename_s; constr = Fun.id; cases = [] },
              (idx, cstr_s, None) ))
        (string_enum
           (List.mapi
              (fun idx (cstr, _) ->
                let cstr_s = EnumConstructor.original_string cstr in
                cstr_s, idx)
              bdgs))
    else List.mapi make_constructor_case bdgs |> first_match
  in
  def (Format.asprintf "%a" EnumName.format_shortpath ename) enc

let make_encoding (ctx : decl_ctx) (typ : typ) = generate_encoder ctx typ

let scope_input_encoding scope ctx typ =
  let scope_s = ScopeName.to_string scope in
  let title = Format.sprintf "Scope %s input" scope_s in
  let description = Format.sprintf "Input structure of scope %s" scope_s in
  let encoding = make_encoding ctx typ in
  def (scope_s ^ "_input") ~title ~description encoding

let scope_output_encoding scope ctx typ =
  let scope_s = ScopeName.to_string scope in
  let title = Format.sprintf "Scope %s output" scope_s in
  let description = Format.sprintf "Output structure of scope %s" scope_s in
  let encoding = make_encoding ctx typ in
  def (scope_s ^ "_output") ~title ~description encoding

let parse_json ?pos enc text =
  let json =
    try Runtime.ParsedJson.of_string text
    with Runtime.ParsedJson.Syntax_error (offset, msg) ->
      Message.error ?pos "@[<v 2>Failed to parse JSON:@ %s at byte %d@]" msg
        offset
  in
  (* A value of the schema that the runtime cannot hold, e.g. a duration
     component or a year beyond a machine integer, raises its runtime error,
     possibly under the alternatives of a union *)
  let rec runtime_error path = function
    | Json_encoding.Cannot_destruct (p, e) -> runtime_error (path @ p) e
    | Json_encoding.No_case_matched errs ->
      List.find_map (runtime_error path) errs
    | Runtime.Error (err, _, _) -> Some (path, err)
    | _ -> None
  in
  try Exact_encoding.destruct enc json with
  | e when Option.is_some (runtime_error [] e) ->
    let path, err = Option.get (runtime_error [] e) in
    Message.error ?pos
      "@[<v>@[<hov>During evaluation:@ %a.@]@,\
       @[<hov>Reading the JSON value at@ %s.@]@]"
      Format.pp_print_text
      (Runtime.error_message err)
      (match Json_query.json_pointer_of_path path with "" -> "/" | p -> p)
  | e ->
    let print_unknown fmt = function
      | Failure msg -> Format.pp_print_string fmt msg
      | e -> Format.pp_print_string fmt (Printexc.to_string e)
    in
    Message.error ?pos
      "@[<v 2>Failed to validate JSON:@ %a@]@\n\
       @\n\
       @[<v 2>Expected JSON object of the form:@ %a@]"
      (fun fmt -> Json_encoding.print_error ~print_unknown fmt)
      e Json_schema.pp (Json_encoding.schema enc)

(* The mark of the unit content of a constant constructor *)
let unit_mark mark = Expr.with_ty mark (TLit TUnit, Expr.mark_pos mark)

let rec convert_to_dcalc ctx (mark : 'm mark) (typ : typ) (rval : Val.t) :
    (dcalc, 'm) boxed_gexpr =
  let open Val in
  let mark = Expr.with_ty mark typ in
  let f = convert_to_dcalc ctx mark in
  match Mark.remove typ, rval with
  | TLit TUnit, V (Val.Unit, _) -> Expr.elit LUnit mark
  | TLit TBool, V (Bool, b) -> Expr.elit (LBool b) mark
  | TLit TMoney, V (Money, z) -> Expr.elit (LMoney z) mark
  | TLit TInt, V (Integer, z) -> Expr.elit (LInt z) mark
  | TLit TRat, V (Decimal, q) -> Expr.elit (LRat q) mark
  | TLit TDate, V (Date, d) -> Expr.elit (LDate d) mark
  | TLit TDuration, V (Duration, d) -> Expr.elit (LDuration d) mark
  | ( TLit TPos,
      V
        ( Position,
          {
            filename;
            start_line;
            start_column;
            end_line;
            end_column;
            law_headings;
          } ) ) ->
    Expr.epos
      Pos.(
        overwrite_law_info
          (from_info filename start_line start_column end_line end_column)
          law_headings)
      mark
  | TDefault typ, V (Enum { name = "Optional"; constr; _ }, v) -> begin
    match constr v with
    | 0, "Absent", None -> Expr.eempty mark
    | 1, "Present", Some rval -> Expr.epuredefault (f typ rval) mark
    | _ -> assert false
  end
  | TOption typ, V (Enum { name = "Optional"; constr; _ }, v) -> begin
    match constr v with
    | 0, "Absent", None ->
      Expr.einj ~name:ConstantNames.option_enum ~cons:ConstantNames.none_constr
        ~e:(Expr.elit LUnit (unit_mark mark))
        mark
    | 1, "Present", Some rval ->
      Expr.einj ~name:ConstantNames.option_enum ~cons:ConstantNames.some_constr
        ~e:(f typ rval) mark
    | _ -> assert false
  end
  | TEnum ename, V (Enum { constr; _ }, v) ->
    let _idx, cstr, v = constr v in
    let cons, typ_v =
      let enum = EnumName.Map.find ename ctx.ctx_enums in
      EnumConstructor.Map.bindings enum
      |> List.find (fun (cstr', _) ->
          EnumConstructor.original_string cstr' = cstr)
    in
    let e =
      match v with
      | None -> Expr.elit LUnit (unit_mark mark)
      | Some v -> f typ_v v
    in
    Expr.einj ~name:ename ~cons ~e mark
  | TStruct sname, V (Struct { fields; _ }, v) ->
    let fields =
      let struc = StructName.Map.find sname ctx.ctx_structs in
      let struc_fields = StructField.Map.bindings struc in
      let lookup_field sf =
        List.find
          (fun (sf', _) -> StructField.original_string sf' = sf)
          struc_fields
      in
      List.fold_left
        (fun sfm (sf, v) ->
          let sf, typ = lookup_field sf in
          StructField.Map.add sf (f typ v) sfm)
        StructField.Map.empty (fields v)
    in
    Expr.estruct ~name:sname ~fields mark
  | TArray typ, V (Array t, a) ->
    Expr.earray
      (Array.map (embed t) a |> Array.to_list |> List.map (f typ))
      mark
  | TTuple typl, V (Tuple (fl, _), a) ->
    Expr.etuple (fl a |> List.map2 (fun typ -> f typ) typl) mark
  | _t, r ->
    Message.error
      "Cannot convert runtime value to dcalc: expected value of type %a, got %a"
      Print.typ typ format r

let rec convert_to_lcalc ctx (mark : 'm mark) (typ : typ) (rval : Val.t) :
    (lcalc, 'm) boxed_gexpr =
  let open Val in
  let mark = Expr.with_ty mark typ in
  let f = convert_to_lcalc ctx mark in
  match Mark.remove typ, rval with
  | TLit TUnit, V (Val.Unit, _) -> Expr.elit LUnit mark
  | TLit TBool, V (Bool, b) -> Expr.elit (LBool b) mark
  | TLit TMoney, V (Money, z) -> Expr.elit (LMoney z) mark
  | TLit TInt, V (Integer, z) -> Expr.elit (LInt z) mark
  | TLit TRat, V (Decimal, q) -> Expr.elit (LRat q) mark
  | TLit TDate, V (Date, d) -> Expr.elit (LDate d) mark
  | TLit TDuration, V (Duration, d) -> Expr.elit (LDuration d) mark
  | ( TLit TPos,
      V
        ( Position,
          {
            filename;
            start_line;
            start_column;
            end_line;
            end_column;
            law_headings;
          } ) ) ->
    Expr.epos
      Pos.(
        overwrite_law_info
          (from_info filename start_line start_column end_line end_column)
          law_headings)
      mark
  | TTuple [typ; (TLit TPos, _)], rval ->
    Expr.etuple [f typ rval; Expr.epos Pos.void mark] mark
  | (TDefault typ | TOption typ), V (Enum { name = "Optional"; constr; _ }, v)
    -> begin
    match constr v with
    | 0, "Absent", None ->
      Expr.einj ~name:ConstantNames.option_enum ~cons:ConstantNames.none_constr
        ~e:(Expr.elit LUnit (unit_mark mark))
        mark
    | 1, "Present", Some rval ->
      Expr.einj ~name:ConstantNames.option_enum ~cons:ConstantNames.some_constr
        ~e:(f typ rval) mark
    | _ -> assert false
  end
  | TEnum ename, V (Enum { constr; _ }, v) ->
    let _idx, cstr, v = constr v in
    let cons, typ_v =
      let enum = EnumName.Map.find ename ctx.ctx_enums in
      EnumConstructor.Map.bindings enum
      |> List.find (fun (cstr', _) ->
          EnumConstructor.original_string cstr' = cstr)
    in
    let e =
      match v with
      | None -> Expr.elit LUnit (unit_mark mark)
      | Some v -> f typ_v v
    in
    Expr.einj ~name:ename ~cons ~e mark
  | TStruct sname, V (Struct { fields; _ }, v) ->
    let fields =
      let struc = StructName.Map.find sname ctx.ctx_structs in
      let struc_fields = StructField.Map.bindings struc in
      let lookup_field sf =
        List.find
          (fun (sf', _) -> StructField.original_string sf' = sf)
          struc_fields
      in
      List.fold_left
        (fun sfm (sf, v) ->
          let sf, typ = lookup_field sf in
          StructField.Map.add sf (f typ v) sfm)
        StructField.Map.empty (fields v)
    in
    Expr.estruct ~name:sname ~fields mark
  | TArray typ, V (Array t, a) ->
    Expr.earray
      (Array.map (embed t) a |> Array.to_list |> List.map (f typ))
      mark
  | TTuple typl, V (Tuple (fl, _), a) ->
    Expr.etuple (fl a |> List.map2 (fun typ -> f typ) typl) mark
  | _t, r ->
    Message.error
      "Cannot convert runtime value to lcalc: expected value of type %a, got %a"
      Print.typ typ format r
