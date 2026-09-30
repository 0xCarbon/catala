(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2026 Inria,
   contributor: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

type nonrec unit = unit
type nonrec bool = bool

(* An integer number of cents *)
type money = Z.t
type integer = Z.t
type decimal = Q.t
type date = Dates_calc.date

type date_rounding = Dates_calc.date_rounding =
  | RoundUp
  | RoundDown
  | AbortOnRound

type duration = Dates_calc.period

type code_location = {
  filename : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
  law_headings : string list;
}

type io_input = NoInput | OnlyInput | Reentrant
type io_log = { io_input : io_input; io_output : bool }

type error =
  | AssertionFailed
  | NoValue
  | Conflict
  | DivisionByZero
  | ListEmpty
  | NotSameLength
  | UncomparableValues
  | DateError of string
  | Impossible
  | IntegerOverflow

let error_to_string = function
  | AssertionFailed -> "AssertionFailed"
  | NoValue -> "NoValue"
  | Conflict -> "Conflict"
  | DivisionByZero -> "DivisionByZero"
  | ListEmpty -> "ListEmpty"
  | NotSameLength -> "NotSameLength"
  | UncomparableValues -> "UncomparableValues"
  | DateError s -> Printf.sprintf "DateError(%S)" s
  | Impossible -> "Impossible"
  | IntegerOverflow -> "IntegerOverflow"

let error_message = function
  | AssertionFailed -> "an assertion doesn't hold"
  | NoValue -> "no applicable rule to define this variable in this situation"
  | Conflict ->
    "conflict between multiple valid consequences for assigning the same \
     variable"
  | DivisionByZero ->
    "a value is being used as denominator in a division and it computed to zero"
  | ListEmpty -> "the list was empty"
  | NotSameLength -> "traversing multiple lists of different lengths"
  | UncomparableValues -> "attempting to compare values with uncomparable types"
  | DateError s -> s
  | Impossible -> "\"impossible\" computation reached"
  | IntegerOverflow -> "an integer is too large for this computation"

exception Error of error * code_location list * string option
exception Empty

let error ?note err pos = raise (Error (err, pos, note))

(* Register (fallback) exception printers *)
let () =
  let ppos () p =
    Printf.sprintf "%s:%d.%d-%d.%d" p.filename p.start_line p.start_column
      p.end_line p.end_column
  in
  let pposl () pl = String.concat ", " (List.map (ppos ()) pl) in
  Printexc.register_printer
  @@ function
  | Error (err, pos, note) ->
    Some
      (Printf.sprintf "At %a: %s%s" pposl pos (error_message err)
         (Option.fold ~none:"" ~some:(( ^ ) ". ") note))
  | _ -> None

let z2 = Z.of_int 2
let z10 = Z.of_int 10
let z100 = Z.of_int 100
let q100 = Q.of_int 100

let round (q : Q.t) : Z.t =
  (* The mathematical formula is [round(q) = sgn(q) * floor(abs(q) + 0.5)].
     However, Zarith's [Q.to_bigint] does not floor. Instead, it rounds towards
     0 (that is, [-0.1] is rounded to [0]). We work around this by using
     [Z.fdiv], integer division with rounding towards [-inf], and implementing
     the trick from
     https://gmplib.org/list-archives/gmp-discuss/2009-May/003767.html *)
  let sgn = Q.sign q in
  let abs = Q.abs q in
  let n = Q.num abs in
  let d = Q.den abs in
  let abs_round = Z.(fdiv ((z2 * n) + d) (z2 * d)) in
  Z.(of_int sgn * abs_round)

let money_of_cents_string (cents : string) : money = Z.of_string cents
let money_of_units_int (units : int) : money = Z.(of_int units * z100)
let money_of_cents_integer (cents : integer) : money = cents
let money_to_float (m : money) : float = Z.to_float m /. 100.

let money_of_decimal (d : decimal) : money =
  (* Turn units to cents then round to nearest cent *)
  round Q.(d * q100)

let money_of_integer (i : integer) : money = Z.(i * z100)

let money_to_string (m : money) : string =
  (* Printed from the integer number of cents: going through a float would lose
     cents beyond 2^46 units *)
  let units, cents = Z.div_rem (Z.abs m) z100 in
  Printf.sprintf "%s%s.%02d"
    (if Z.sign m < 0 then "-" else "")
    (Z.to_string units) (Z.to_int cents)

let money_to_cents m = m

let money_round (m : money) : money =
  (* Turn cents to units then round to nearest unit, and convert back *)
  let units = Q.(of_bigint m / q100) in
  Z.(round units * z100)

let decimal_of_string (d : string) : decimal = Q.of_string d
let decimal_to_float (d : decimal) : float = Q.to_float d
let decimal_of_float (d : float) : decimal = Q.of_float d
let decimal_of_integer (d : integer) : decimal = Q.of_bigint d

let decimal_to_string ~(max_prec_digits : int) (i : decimal) : string =
  let sign = Q.sign i in
  let n = Z.abs (Q.num i) in
  let d = Z.abs (Q.den i) in
  let int_part = Z.ediv n d in
  let n = ref (Z.erem n d) in
  let digits = ref [] in
  let leading_zeroes (digits : Z.t list) : int =
    match
      List.fold_right
        (fun digit num_leading_zeroes ->
          match num_leading_zeroes with
          | `End _ -> num_leading_zeroes
          | `Begin i -> if Z.(digit = zero) then `Begin (i + 1) else `End i)
        digits (`Begin 0)
    with
    | `End i -> i
    | `Begin i -> i
  in
  while
    !n <> Z.zero
    && List.length !digits - leading_zeroes !digits < max_prec_digits
  do
    n := Z.mul !n z10;
    digits := Z.ediv !n d :: !digits;
    n := Z.erem !n d
  done;
  Format.asprintf "%s%a.%a%s"
    (if sign < 0 then "-" else "")
    Z.pp_print int_part
    (Format.pp_print_list
       ~pp_sep:(fun _fmt () -> ())
       (fun fmt digit -> Format.fprintf fmt "%a" Z.pp_print digit))
    (List.rev !digits)
    (if List.length !digits - leading_zeroes !digits = max_prec_digits then "…"
     else "")

let decimal_round (q : decimal) : decimal = Q.of_bigint (round q)
let decimal_of_money (m : money) : decimal = Q.div (Q.of_bigint m) q100
let integer_of_string (s : string) : integer = Z.of_string s
let integer_to_string (i : integer) : string = Z.to_string i
let integer_to_int (i : integer) : int = Z.to_int i
let integer_of_int (i : int) : integer = Z.of_int i
let integer_of_decimal (d : decimal) : integer = Q.to_bigint d
let integer_of_money (m : money) : integer = round (decimal_of_money m)
let integer_exponentiation (i : integer) (e : int) : integer = Z.pow i e
let integer_log2 = Z.log2

let year_of_date (d : date) : integer =
  let y, _, _ = Dates_calc.date_to_ymd d in
  Z.of_int y

let month_number_of_date (d : date) : integer =
  let _, m, _ = Dates_calc.date_to_ymd d in
  Z.of_int m

let is_leap_year (y : integer) =
  (* On the integer itself: a year may not fit a machine integer *)
  let divides n = Z.equal (Z.erem y (Z.of_int n)) Z.zero in
  divides 4 && ((not (divides 100)) || divides 400)

let day_of_month_of_date (d : date) : integer =
  let _, _, d = Dates_calc.date_to_ymd d in
  Z.of_int d

(* This could fail, but is expected to only be called with known, already
   validated arguments by the generated code *)
let date_of_numbers (year : int) (month : int) (day : int) : date =
  try Dates_calc.make_date ~year ~month ~day
  with Dates_calc.InvalidDate -> failwith "date_of_numbers: invalid date"

let date_to_string (d : date) : string =
  Format.asprintf "%a" Dates_calc.format_date d

let date_to_years_months_days (d : date) : int * int * int =
  Dates_calc.date_to_ymd d

let first_day_of_month = Dates_calc.first_day_of_month
let last_day_of_month = Dates_calc.last_day_of_month

let duration_of_numbers (year : int) (month : int) (day : int) : duration =
  Dates_calc.make_period ~years:year ~months:month ~days:day

let duration_to_string (d : duration) : string =
  Format.asprintf "%a" Dates_calc.format_period d

let duration_to_years_months_days (d : duration) : int * int * int =
  Dates_calc.period_to_ymds d

(* Maybe should be integrated into dates_calc ? *)
let compare_periods pos p1 p2 =
  let y1, m1, d1 = Dates_calc.period_to_ymds p1 in
  let y2, m2, d2 = Dates_calc.period_to_ymds p2 in
  match y1, y2, m1, m2, d1, d2 with
  | _, _, _, _, 0, 0 -> Int.compare ((12 * y1) + m1) ((12 * y2) + m2)
  | 0, 0, 0, 0, d1, d2 -> Int.compare d1 d2
  | _ ->
    error
      (DateError
         "ambiguous comparison between durations in different units (e.g. \
          months vs. days)")
      [pos]

let equal_periods pos p1 p2 =
  Dates_calc.period_to_ymds p1 = Dates_calc.period_to_ymds p2
  || compare_periods pos p1 p2 = 0

(* -- Printing helpers -- *)

module Print = struct
  type lang = [ `En | `Fr | `Pl ]

  let lang = ref `En
  let max_decimals = ref 6
  let set_lang l = lang := l
  let get_lang () = !lang
  let set_precision n = max_decimals := n
  let get_precision () = !max_decimals

  (* Refs:
     https://en.wikipedia.org/wiki/Wikipedia:Manual_of_Style/Dates_and_numbers#Grouping_of_digits
     https://fr.wikipedia.org/wiki/Wikip%C3%A9dia:Conventions_concernant_les_nombres#Pour_un_comptage_ou_une_mesure *)
  let bigsep () =
    match !lang with `En -> ",", 3 | `Fr -> " ", 3 | `Pl -> ",", 3

  let decsep () = match !lang with `En -> "." | `Fr -> "," | `Pl -> "."
  let unit ppf () = Format.pp_print_string ppf "()"

  let bool ppf b =
    let s =
      match !lang, b with
      | `En, true -> "true"
      | `En, false -> "false"
      | `Fr, true -> "vrai"
      | `Fr, false -> "faux"
      | `Pl, true -> "prawda"
      | `Pl, false -> "falsz"
    in
    Format.pp_print_string ppf s

  let integer ppf n =
    let sep, nsep = bigsep () in
    let nsep = Z.pow z10 nsep in
    if Z.sign n < 0 then Format.pp_print_char ppf '-';
    let rec aux n =
      let a, b = Z.div_rem n nsep in
      if Z.equal a Z.zero then Z.pp_print ppf b
      else (
        aux a;
        Format.fprintf ppf "%s%03d" sep (Z.to_int b))
    in
    aux (Z.abs n)

  let money ppf n =
    let num = Z.abs n in
    let units, cents = Z.div_rem num z100 in
    if Z.sign n < 0 then Format.pp_print_char ppf '-';
    (match !lang with `En -> Format.pp_print_string ppf "$" | `Fr | `Pl -> ());
    integer ppf units;
    Format.pp_print_string ppf (decsep ());
    Format.fprintf ppf "%02d" (Z.to_int (Z.abs cents));
    match !lang with
    | `En -> ()
    | `Fr -> Format.fprintf ppf " @<1>%s" "€"
    | `Pl -> Format.pp_print_string ppf " PLN"

  let decimal ppf r =
    let den = Q.den r in
    let num = Z.abs (Q.num r) in
    let int_part, rem = Z.div_rem num den in
    let rem = Z.abs rem in
    (* Printing the integer part *)
    if Q.sign r < 0 then Format.pp_print_char ppf '-';
    integer ppf int_part;
    (* Printing the decimals *)
    let bigsep, nsep = bigsep () in
    let rec aux ndigit rem =
      let n, rem = Z.div_rem (Z.mul rem z10) den in
      if ndigit mod nsep = 0 then
        Format.pp_print_string ppf (if ndigit = 0 then decsep () else bigsep);
      Format.pp_print_int ppf (Z.to_int n);
      if Z.gt rem Z.zero then
        if ndigit + 1 >= !max_decimals then Format.pp_print_as ppf 1 "…"
        else aux (ndigit + 1) rem
    in
    aux 0 rem
  (* It would be nice to print ratios as % but that's impossible to guess.
     Trying would lead to inconsistencies where some comparable numbers are in %
     and some others not, adding confusion. *)

  let date ppf d =
    let y, m, d = date_to_years_months_days d in
    Format.fprintf ppf "|%04d-%02d-%02d|" y m d

  let duration ppf dr =
    let y, m, d = duration_to_years_months_days dr in
    let rec filter0 = function
      | (0, _) :: (_ :: _ as r) -> filter0 r
      | x :: r -> x :: List.filter (fun (n, _) -> n <> 0) r
      | [] -> []
    in
    let splur n s = if abs n > 1 then n, s ^ "s" else n, s in
    Format.pp_print_char ppf '[';
    (match !lang with
      | `En -> [splur y "year"; splur m "month"; splur d "day"]
      | `Fr -> [splur y "an"; m, "mois"; splur d "jour"]
      | `Pl -> [y, "rok"; m, "miesiac"; d, "dzien"])
    |> filter0
    |> Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
         (fun ppf (n, s) -> Format.fprintf ppf "%d %s" n s)
         ppf;
    Format.pp_print_char ppf ']'
end

(* -- JSON input -- *)

module ParsedJson = struct
  type t =
    | Null
    | Bool of bool
    | Number of string
    | String of string
    | Array of t list
    | Object of (string * t) list

  exception Syntax_error of int * string

  let is_digit = function '0' .. '9' -> true | _ -> false

  (* End of the JSON number literal (RFC 8259) starting at [i] in [s], or [-1]
     if there is none *)
  let number_end (s : string) (i : int) : int =
    let len = String.length s in
    let digits i =
      let j = ref i in
      while !j < len && is_digit s.[!j] do
        incr j
      done;
      if !j = i then -1 else !j
    in
    let i = if i < len && s.[i] = '-' then i + 1 else i in
    let i =
      if i < len && s.[i] = '0' then i + 1
      else if i < len && s.[i] >= '1' && s.[i] <= '9' then digits i
      else -1
    in
    let i = if i >= 0 && i < len && s.[i] = '.' then digits (i + 1) else i in
    if i >= 0 && i < len && (s.[i] = 'e' || s.[i] = 'E') then
      let j = i + 1 in
      let j = if j < len && (s.[j] = '+' || s.[j] = '-') then j + 1 else j in
      digits j
    else i

  let of_string (s : string) : t =
    let len = String.length s in
    let pos = ref 0 in
    let fail msg = raise (Syntax_error (!pos, msg)) in
    let peek () = if !pos < len then Some s.[!pos] else None in
    let skip_ws () =
      while
        match peek () with
        | Some (' ' | '\t' | '\n' | '\r') -> true
        | _ -> false
      do
        incr pos
      done
    in
    let expect c =
      if peek () = Some c then incr pos
      else fail (Printf.sprintf "expected '%c'" c)
    in
    let keyword w v =
      let n = String.length w in
      if !pos + n <= len && String.sub s !pos n = w then (
        pos := !pos + n;
        v)
      else fail "invalid literal"
    in
    let hex4 () =
      let hex = function
        | '0' .. '9' as c -> Char.code c - Char.code '0'
        | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
        | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
        | _ -> fail "invalid unicode escape"
      in
      if !pos + 4 > len then fail "invalid unicode escape";
      let n = ref 0 in
      for k = 0 to 3 do
        n := (!n * 16) + hex s.[!pos + k]
      done;
      pos := !pos + 4;
      !n
    in
    let string_lit () =
      let start = !pos in
      expect '"';
      let buf = Buffer.create 16 in
      let closed = ref false in
      while not !closed do
        match peek () with
        | None -> fail "unterminated string"
        | Some '"' ->
          incr pos;
          closed := true
        | Some '\\' -> (
          incr pos;
          let c = peek () in
          incr pos;
          match c with
          | Some '"' -> Buffer.add_char buf '"'
          | Some '\\' -> Buffer.add_char buf '\\'
          | Some '/' -> Buffer.add_char buf '/'
          | Some 'b' -> Buffer.add_char buf '\b'
          | Some 'f' -> Buffer.add_char buf '\012'
          | Some 'n' -> Buffer.add_char buf '\n'
          | Some 'r' -> Buffer.add_char buf '\r'
          | Some 't' -> Buffer.add_char buf '\t'
          | Some 'u' ->
            let u = hex4 () in
            let u =
              if u >= 0xD800 && u <= 0xDBFF then (
                if not (!pos + 2 <= len && s.[!pos] = '\\' && s.[!pos + 1] = 'u')
                then fail "unpaired surrogate";
                pos := !pos + 2;
                let lo = hex4 () in
                if lo < 0xDC00 || lo > 0xDFFF then fail "unpaired surrogate";
                0x10000 + ((u - 0xD800) lsl 10) + (lo - 0xDC00))
              else if u >= 0xDC00 && u <= 0xDFFF then fail "unpaired surrogate"
              else u
            in
            Buffer.add_utf_8_uchar buf (Uchar.of_int u)
          | _ -> fail "invalid escape")
        | Some '\000' .. '\031' -> fail "control character in string"
        | Some c ->
          Buffer.add_char buf c;
          incr pos
      done;
      let str = Buffer.contents buf in
      if not (String.is_valid_utf_8 str) then (
        pos := start;
        fail "invalid UTF-8 in string");
      str
    in
    let rec value () =
      skip_ws ();
      match peek () with
      | Some '{' ->
        incr pos;
        skip_ws ();
        if peek () = Some '}' then (
          incr pos;
          Object [])
        else
          let keys = Hashtbl.create 8 in
          let rec members acc =
            skip_ws ();
            let key_pos = !pos in
            let key = string_lit () in
            if Hashtbl.mem keys key then (
              pos := key_pos;
              fail (Printf.sprintf "duplicate key %S" key));
            Hashtbl.add keys key ();
            skip_ws ();
            expect ':';
            let v = value () in
            let acc = (key, v) :: acc in
            skip_ws ();
            match peek () with
            | Some ',' ->
              incr pos;
              members acc
            | Some '}' ->
              incr pos;
              Object (List.rev acc)
            | _ -> fail "expected ',' or '}'"
          in
          members []
      | Some '[' ->
        incr pos;
        skip_ws ();
        if peek () = Some ']' then (
          incr pos;
          Array [])
        else
          let rec elements acc =
            let acc = value () :: acc in
            skip_ws ();
            match peek () with
            | Some ',' ->
              incr pos;
              elements acc
            | Some ']' ->
              incr pos;
              Array (List.rev acc)
            | _ -> fail "expected ',' or ']'"
          in
          elements []
      | Some '"' -> String (string_lit ())
      | Some 't' -> keyword "true" (Bool true)
      | Some 'f' -> keyword "false" (Bool false)
      | Some 'n' -> keyword "null" Null
      | Some ('-' | '0' .. '9') ->
        let e = number_end s !pos in
        if e < 0 then fail "invalid number";
        let n = String.sub s !pos (e - !pos) in
        pos := e;
        Number n
      | Some _ -> fail "unexpected character"
      | None -> fail "unexpected end of input"
    in
    let v = value () in
    skip_ws ();
    if !pos <> len then fail "trailing characters";
    v

  let quote buf str =
    Buffer.add_char buf '"';
    String.iter
      (function
        | ('"' | '\\') as c ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf c
        | '\n' -> Buffer.add_string buf "\\n"
        | '\t' -> Buffer.add_string buf "\\t"
        | '\r' -> Buffer.add_string buf "\\r"
        | '\x00' .. '\x1F' as c -> Printf.bprintf buf "\\u%04x" (int_of_char c)
        | c -> Buffer.add_char buf c)
      str;
    Buffer.add_char buf '"'

  let to_string (v : t) : string =
    let buf = Buffer.create 64 in
    let rec aux = function
      | Null -> Buffer.add_string buf "null"
      | Bool b -> Buffer.add_string buf (string_of_bool b)
      | Number n -> Buffer.add_string buf n
      | String s -> quote buf s
      | Array l ->
        Buffer.add_char buf '[';
        List.iteri
          (fun i v ->
            if i > 0 then Buffer.add_char buf ',';
            aux v)
          l;
        Buffer.add_char buf ']'
      | Object l ->
        Buffer.add_char buf '{';
        List.iteri
          (fun i (k, v) ->
            if i > 0 then Buffer.add_char buf ',';
            quote buf k;
            Buffer.add_char buf ':';
            aux v)
          l;
        Buffer.add_char buf '}'
    in
    aux v;
    Buffer.contents buf

  (* Largest decimal exponent accepted in a number: bounds the size of the exact
     value (10^1000000 has about 3.3 million bits) *)
  let max_exponent = 1_000_000

  let is_number_literal (s : string) =
    s <> "" && number_end s 0 = String.length s

  (* The exact value of a valid number literal, [None] if its exponent is out of
     bounds *)
  let decimal_of_literal (n : string) : Q.t option =
    let mantissa, exponent =
      match String.index_opt n 'e', String.index_opt n 'E' with
      | Some i, _ | None, Some i ->
        ( String.sub n 0 i,
          int_of_string_opt (String.sub n (i + 1) (String.length n - i - 1)) )
      | None, None -> n, Some 0
    in
    match exponent with
    | Some e when abs e <= max_exponent ->
      let digits, scale =
        match String.index_opt mantissa '.' with
        | None -> mantissa, e
        | Some i ->
          ( String.sub mantissa 0 i
            ^ String.sub mantissa (i + 1) (String.length mantissa - i - 1),
            e - (String.length mantissa - i - 1) )
      in
      let m = Z.of_string digits in
      Some
        (if scale >= 0 then Q.of_bigint (Z.mul m (Z.pow z10 scale))
         else Q.make m (Z.pow z10 (-scale)))
    | _ -> None

  let number_to_decimal (n : string) : Q.t =
    if not (is_number_literal n) then invalid_arg "number_to_decimal";
    match decimal_of_literal n with
    | Some q -> q
    | None -> invalid_arg "number_to_decimal: exponent out of range"

  (* [-?[0-9]+] *)
  let is_integer_string (s : string) =
    let len = String.length s in
    let start = if len > 0 && s.[0] = '-' then 1 else 0 in
    len > start && String.for_all is_digit (String.sub s start (len - start))

  let integer_of_number (n : string) : integer option =
    if not (is_number_literal n) then None
    else
      match decimal_of_literal n with
      | Some q when Z.equal (Q.den q) Z.one -> Some (Q.num q)
      | _ -> None

  let integer_of_string (s : string) : integer option =
    if is_integer_string s then Some (Z.of_string s) else None

  let decimal_of_string (s : string) : decimal option =
    if is_number_literal s then decimal_of_literal s
    else
      match String.index_opt s '/' with
      | Some i ->
        let num = String.sub s 0 i in
        let den = String.sub s (i + 1) (String.length s - i - 1) in
        if
          is_integer_string num
          && den <> ""
          && String.for_all is_digit den
          && not (String.for_all (( = ) '0') den)
        then Some (Q.make (Z.of_string num) (Z.of_string den))
        else None
      | None -> None
end

(* -- Runtime types and embedding -- *)

module Value = struct
  type _ external_tag = ..

  module type External = sig
    type t
    type _ external_tag += T : t external_tag

    val name : string
    val equal : code_location -> t -> t -> bool
    val compare : code_location -> t -> t -> int
    val print : t -> string
    val to_json : t -> string
    val from_json : code_location -> string -> t
  end

  type _ ty =
    | Unit : unit ty
    | Bool : bool ty
    | Integer : integer ty
    | Money : money ty
    | Decimal : decimal ty
    | Date : date ty
    | Duration : duration ty
    | Position : code_location ty
    | Array : 'a ty -> 'a array ty
    | Tuple : ('a -> t list) * 'a build -> 'a ty
    | Struct : {
        name : string;
        fields : 'a -> (string * t) list;
            (* list order must be consistent with the representation *)
        build : 'a build;
      }
        -> 'a ty
    | Enum : {
        name : string;
        constr : 'a -> int * string * t option;
        cases : 'a case list;
      }
        -> 'a ty
    | External : (module External with type t = 'a) -> 'a ty
    | Function : 'a ty
    | Polymorphic : 'a ty
    | Dynamic : t ty

  and 'a build =
    | Build : ('f, 'a) components * 'f -> 'a build
    | Unbuildable : 'a build

  and (_, _) components =
    | Nil : ('a, 'a) components
    | Cons : string * 'c ty * ('f, 'a) components -> ('c -> 'f, 'a) components

  and 'a case = Case : string * 'c ty * ('c -> 'a) -> 'a case
  and t = V : 'a ty * 'a -> t

  let embed : type a. a ty -> a -> t =
   fun t v -> match t with Dynamic -> v | t -> V (t, v)

  exception Invalid_json of code_location * string

  let () =
    Printexc.register_printer
    @@ function
    | Invalid_json (pos, msg) ->
      Some
        (Printf.sprintf "At %s:%d.%d-%d.%d: invalid JSON value: %s" pos.filename
           pos.start_line pos.start_column pos.end_line pos.end_column msg)
    | _ -> None

  (* let unembed (type a) (V { t; v }): a ty * a =
   *   Obj.magic t, Obj.magic v *)

  let rec equal : code_location -> t -> t -> bool =
   fun pos rv1 rv2 ->
    match rv1, rv2 with
    | V (Dynamic, v1), v2 -> equal pos v1 v2
    | v1, V (Dynamic, v2) -> equal pos v1 v2
    | V (Unit, ()), V (Unit, ()) -> true
    | V (Bool, v1), V (Bool, v2) -> equal_values Bool pos v1 v2
    | V (Integer, v1), V (Integer, v2) -> equal_values Integer pos v1 v2
    | V (Money, v1), V (Money, v2) -> equal_values Money pos v1 v2
    | V (Decimal, v1), V (Decimal, v2) -> equal_values Decimal pos v1 v2
    | V (Date, v1), V (Date, v2) -> equal_values Date pos v1 v2
    | V (Duration, v1), V (Duration, v2) -> equal_values Duration pos v1 v2
    | V (Position, v1), V (Position, v2) -> equal_values Position pos v1 v2
    | V (Array t1, v1), V (Array t2, v2) ->
      Array.length v1 = Array.length v2
      && Array.for_all2 (equal pos)
           (Array.map (embed t1) v1)
           (Array.map (embed t2) v2)
    | V (Tuple (t1, _), v1), V (Tuple (t2, _), v2) ->
      List.for_all2 (equal pos) (t1 v1) (t2 v2)
    | V (Struct str1, v1), V (Struct str2, v2) ->
      str1.name = str2.name
      &&
      (* could be an assert if well-typed ? *)
      List.for_all2
        (fun (fld1, rv1) (fld2, rv2) -> fld1 = fld2 && equal pos rv1 rv2)
        (str1.fields v1) (str2.fields v2)
    | V (Enum en1, v1), V (Enum en2, v2) ->
      en1.name = en2.name
      &&
      (* could be an assert if well-typed ? *)
      let n1, _, x1 = en1.constr v1 in
      let n2, _, x2 = en2.constr v2 in
      n1 = n2 && Option.equal (equal pos) x1 x2
    | V (External (module E1), v1), V (External (module E2), v2) -> (
      match E1.T with E2.T -> E1.equal pos v1 v2 | _ -> false)
    | V (Function, _), V (Function, _) -> error UncomparableValues [pos]
    | V (Polymorphic, _), V (Polymorphic, _) -> error UncomparableValues [pos]
    (* The follwing shouldn't happen on well-typed terms *)
    | ( V
          ( ( Unit | Bool | Integer | Money | Decimal | Date | Duration
            | Position | Array _ | Tuple _ | Struct _ | Enum _ | External _
            | Function | Polymorphic ),
            _ ),
        _ ) ->
      false

  and equal_values : type a. a ty -> code_location -> a -> a -> bool =
   fun ty pos x1 x2 ->
    match ty with
    | Bool -> Bool.equal x1 x2
    | Unit -> true
    | Integer -> Z.equal x1 x2
    | Money -> Z.equal x1 x2
    | Decimal -> Q.equal x1 x2
    | Date -> Dates_calc.compare_dates x1 x2 = 0
    | Duration -> equal_periods pos x1 x2
    | Position -> x1 = x2
    | t -> equal pos (V (t, x1)) (V (t, x2))

  let rec compare : code_location -> t -> t -> int =
   fun pos rv1 rv2 ->
    let rec compare_lists l1 l2 =
      match l1, l2 with
      | x1 :: l1, x2 :: l2 -> (
        match compare pos x1 x2 with 0 -> compare_lists l1 l2 | n -> n)
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
    in
    match rv1, rv2 with
    | V (Dynamic, v1), v2 -> compare pos v1 v2
    | v1, V (Dynamic, v2) -> compare pos v1 v2
    | V (Unit, ()), V (Unit, ()) -> 0
    | V (Bool, v1), V (Bool, v2) -> compare_values Bool pos v1 v2
    | V (Integer, v1), V (Integer, v2) -> compare_values Integer pos v1 v2
    | V (Money, v1), V (Money, v2) -> compare_values Money pos v1 v2
    | V (Decimal, v1), V (Decimal, v2) -> compare_values Decimal pos v1 v2
    | V (Date, v1), V (Date, v2) -> compare_values Date pos v1 v2
    | V (Duration, v1), V (Duration, v2) -> compare_values Duration pos v1 v2
    | V (Array t1, v1), V (Array t2, v2) ->
      let rec aux i =
        if i >= Array.length v1 then if i >= Array.length v2 then 0 else -1
        else if i >= Array.length v2 then 1
        else
          match compare pos (embed t1 v1.(i)) (embed t2 v2.(i)) with
          | 0 -> aux (i + 1)
          | n -> n
      in
      aux 0
    | V (Tuple (to_list1, _), v1), V (Tuple (to_list2, _), v2) ->
      compare_lists (to_list1 v1) (to_list2 v2)
    | V (Struct str1, v1), V (Struct str2, v2) -> (
      match String.compare str1.name str2.name with
      | 0 ->
        compare_lists
          (List.map snd (str1.fields v1))
          (List.map snd (str2.fields v2))
      | n -> n (* could be assert false if well-typed ? *))
    | V (Enum en1, v1), V (Enum en2, v2) -> (
      match String.compare en1.name en2.name with
      | 0 -> (
        let n1, _, x1 = en1.constr v1 in
        let n2, _, x2 = en2.constr v2 in
        match Stdlib.compare n1 n2 with
        | 0 -> Option.compare (compare pos) x1 x2
        | n -> n)
      | n -> n (* could be assert false if well-typed ? *))
    | V (External (module E1), v1), V (External (module E2), v2) -> (
      match E1.T with
      | E2.T -> E1.compare pos v1 v2
      | _ -> error UncomparableValues [pos])
    | V (Function, _), _ | _, V (Function, _) -> error UncomparableValues [pos]
    | V (Polymorphic, _), _ | _, V (Polymorphic, _) ->
      error UncomparableValues [pos]
    (* The follwing shouldn't happen on well-typed terms *)
    | V (Unit, _), _ -> -1
    | _, V (Unit, _) -> 1
    | V (Bool, _), _ -> -1
    | _, V (Bool, _) -> 1
    | V (Integer, _), _ -> -1
    | _, V (Integer, _) -> 1
    | V (Money, _), _ -> -1
    | _, V (Money, _) -> 1
    | V (Decimal, _), _ -> -1
    | _, V (Decimal, _) -> 1
    | V (Position, _), _ -> -1
    | _, V (Position, _) -> 1
    | V (Date, _), _ -> -1
    | _, V (Date, _) -> 1
    | V (Duration, _), _ -> -1
    | _, V (Duration, _) -> 1
    | V (Array _, _), _ -> -1
    | _, V (Array _, _) -> 1
    | V (Tuple _, _), _ -> -1
    | _, V (Tuple _, _) -> 1
    | V (Struct _, _), _ -> -1
    | _, V (Struct _, _) -> 1
    | V (Enum _, _), _ -> -1
    | _, V (Enum _, _) -> 1
    | V (External _, _), _ -> .
    | _, V (External _, _) -> .

  and compare_values : type a. a ty -> code_location -> a -> a -> int =
   fun ty pos x1 x2 ->
    match ty with
    | Unit -> 0
    | Bool -> Bool.compare x1 x2
    | Money -> Z.compare x1 x2
    | Integer -> Z.compare x1 x2
    | Decimal -> Q.compare x1 x2
    | Date -> Dates_calc.compare_dates x1 x2
    | Duration -> compare_periods pos x1 x2
    | Position -> Stdlib.compare x1 x2
    | t -> compare pos (V (t, x1)) (V (t, x2))

  let format ppf v =
    (* Format performs indentation, but also alignment, which we want to disable
       here to be similar to the other backend printers. Hence the manual
       indentation printing within a single vbox *)
    let rec aux indent ppf =
      let nl n ppf = Format.fprintf ppf "@,%*s" (indent + n) "" in
      function
      | V (Unit, x) -> Print.unit ppf x
      | V (Bool, x) -> Print.bool ppf x
      | V (Money, x) -> Print.money ppf x
      | V (Integer, x) -> Print.integer ppf x
      | V (Decimal, x) -> Print.decimal ppf x
      | V (Date, x) -> Print.date ppf x
      | V (Duration, x) -> Print.duration ppf x
      | V (Enum en, v) -> (
        match en.constr v with
        | _, name, None -> Format.fprintf ppf "%s" name
        | _, name, Some v ->
          Format.fprintf ppf "%s %s %a" name
            (match !Print.lang with
            | `En -> "content"
            | `Fr -> "contenu"
            | `Pl -> "typu")
            (aux (indent + 2))
            v)
      | V (Struct str, v) ->
        Format.fprintf ppf "%s {" str.name;
        let fields = str.fields v in
        List.iter
          (fun (name, v) ->
            Format.fprintf ppf "%t-- %s: %a" (nl 2) name (aux (indent + 2)) v)
          fields;
        if fields <> [] then nl 0 ppf;
        Format.fprintf ppf "}"
      | V (Array t, v) ->
        Format.pp_print_char ppf '[';
        Array.iter
          (fun v ->
            Format.fprintf ppf "%t%a;" (nl 2) (aux (indent + 2)) (embed t v))
          v;
        if Array.length v > 0 then nl 0 ppf;
        Format.pp_print_string ppf "]"
      | V (Dynamic, v) -> aux indent ppf v
      | V (Tuple (destr, _), v) ->
        Format.fprintf ppf "(%a)"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
             (aux (indent + 1)))
          (destr v)
      | V (Position, pos) ->
        Format.fprintf ppf "%s:%d.%d-%d-%d" pos.filename pos.start_line
          pos.start_column pos.end_line pos.end_column
      | V (Function, _) -> Format.fprintf ppf "<function>"
      | V (Polymorphic, _) -> Format.fprintf ppf "<poly>"
      | V (External (module E), v) -> Format.pp_print_string ppf (E.print v)
    in
    Format.pp_open_vbox ppf 0;
    aux 0 ppf v;
    Format.pp_close_box ppf ()

  let is_optional_name = function
    | "Optional" | "Optionnel" | "Opcjonalny" -> true
    | _ -> false

  let is_unit : type a. a ty -> bool = function Unit -> true | _ -> false

  let is_optional : type a. a ty -> bool = function
    | Enum { name; _ } -> is_optional_name name
    | _ -> false

  (* Reads the JSON forms accepted for scope inputs by the interpreter
     (compiler/shared_ast/encoding.ml), so that compiled programs and the
     interpreter agree on every input. *)
  let from_json : type a. a ty -> code_location -> string -> a =
   fun ty pos text ->
    let fail path fmt =
      Printf.ksprintf
        (fun msg ->
          raise
            (Invalid_json
               ( pos,
                 if path = "" then msg else Printf.sprintf "at %s, %s" path msg
               )))
        fmt
    in
    let kind : ParsedJson.t -> string = function
      | Null -> "null"
      | Bool _ -> "a boolean"
      | Number _ -> "a number"
      | String _ -> "a string"
      | Array _ -> "an array"
      | Object _ -> "an object"
    in
    (* Numbers are read strictly and exactly, as by the interpreter *)
    let numeric path what ~of_number ~of_string (j : ParsedJson.t) =
      match j with
      | Number n -> (
        match of_number n with
        | Some v -> v
        | None -> fail path "expected %s, got the number %s" what n)
      | String s -> (
        match of_string s with
        | Some v -> v
        | None -> fail path "expected %s, got the string %S" what s)
      | j -> fail path "expected %s, got %s" what (kind j)
    in
    (* An object with keys among [keys]: returns its lookup function *)
    let fields path keys (j : ParsedJson.t) =
      match j with
      | Object kv ->
        List.iter
          (fun (k, _) ->
            if not (List.mem k keys) then fail path "unexpected field %S" k)
          kv;
        fun k -> List.assoc_opt k kv
      | j -> fail path "expected an object, got %s" (kind j)
    in
    let req path get k =
      match get k with Some v -> v | None -> fail path "missing field %S" k
    in
    let int_in path lo hi (j : ParsedJson.t) =
      match j with
      | Number n -> (
        match ParsedJson.integer_of_number n with
        | Some z when Z.geq z (Z.of_int lo) && Z.leq z (Z.of_int hi) ->
          Z.to_int z
        | _ -> fail path "expected an integer in [%d, %d], got %s" lo hi n)
      | j -> fail path "expected an integer, got %s" (kind j)
    in
    let no_json name =
      invalid_arg
        (Printf.sprintf "Value.from_json: %s values cannot be read from JSON"
           name)
    in
    let rec decode : type a. a ty -> string -> ParsedJson.t -> a =
     fun ty path j ->
      match ty with
      | Unit -> (
        match j with
        | Object [] -> ()
        | j -> fail path "expected {}, got %s" (kind j))
      | Bool -> (
        match j with
        | Bool b -> b
        | j -> fail path "expected a boolean, got %s" (kind j))
      | Integer ->
        numeric path "an integer" j ~of_number:ParsedJson.integer_of_number
          ~of_string:ParsedJson.integer_of_string
      | Money ->
        (* Amounts are in units; digits beyond the cent are truncated *)
        let money q = Q.to_bigint (Q.mul q q100) in
        numeric path "money" j
          ~of_number:(fun n ->
            Option.map money (ParsedJson.decimal_of_string n))
          ~of_string:(fun s ->
            if ParsedJson.is_number_literal s then
              Option.map money (ParsedJson.decimal_of_string s)
            else None)
      | Decimal ->
        numeric path "a decimal" j ~of_number:ParsedJson.decimal_of_string
          ~of_string:ParsedJson.decimal_of_string
      | Date -> (
        match j with
        | String s -> (
          try Dates_calc.date_of_string s
          with
          | Invalid_argument _ | Failure _ | End_of_file
          | Dates_calc.InvalidDate
          ->
            fail path "invalid date %S" s)
        | Object _ -> (
          let get = fields path ["year"; "month"; "day"] j in
          let comp k lo hi = int_in (path ^ "/" ^ k) lo hi (req path get k) in
          let year = comp "year" 0 9999 in
          let month = comp "month" 1 12 in
          let day = comp "day" 1 31 in
          try Dates_calc.make_date ~year ~month ~day
          with Dates_calc.InvalidDate -> fail path "invalid date")
        | j -> fail path "expected a date, got %s" (kind j))
      | Duration ->
        let get = fields path ["years"; "months"; "days"] j in
        (* Components are integers, as in the JSON schema; a duration holds
           them as machine integers *)
        let comp k =
          match get k with
          | None -> 0
          | Some v ->
            let z =
              numeric
                (path ^ "/" ^ k)
                "an integer" v ~of_number:ParsedJson.integer_of_number
                ~of_string:ParsedJson.integer_of_string
            in
            if Z.fits_int z then Z.to_int z
            else raise (Error (IntegerOverflow, [pos], None))
        in
        let years = comp "years" in
        let months = comp "months" in
        let days = comp "days" in
        Dates_calc.make_period ~years ~months ~days
      | Position ->
        let get = fields path ["file"; "range"] j in
        let filename =
          match req path get "file" with
          | String s -> s
          | j -> fail (path ^ "/file") "expected a string, got %s" (kind j)
        in
        let rpath = path ^ "/range" in
        let rget = fields rpath ["start"; "end"] (req path get "range") in
        let point k =
          let ppath = rpath ^ "/" ^ k in
          let get = fields ppath ["line"; "character"] (req rpath rget k) in
          let comp k =
            int_in
              (ppath ^ "/" ^ k)
              (Int32.to_int Int32.min_int)
              (Int32.to_int Int32.max_int)
              (req ppath get k)
          in
          let line = comp "line" in
          line, comp "character"
        in
        let start_line, start_column = point "start" in
        let end_line, end_column = point "end" in
        {
          filename;
          start_line;
          start_column;
          end_line;
          end_column;
          law_headings = [];
        }
      | Array t -> (
        match j with
        | Array l ->
          Array.of_list
            (List.mapi (fun i x -> decode t (path ^ "/" ^ string_of_int i) x) l)
        | j -> fail path "expected an array, got %s" (kind j))
      | Tuple (_, Build (Cons (_, t, Cons (_, Position, Nil)), make)) ->
        (* A value with its position is given as the value alone *)
        make (decode t path j) pos
      | Tuple (_, Build (components, make)) -> (
        match j with
        | Array l ->
          let n = arity components in
          if List.length l <> n then
            fail path "expected an array of %d elements, got %d" n
              (List.length l);
          let rec apply : type f. int -> (f, a) components -> f -> _ -> a =
           fun i components f l ->
            match components, l with
            | Cons (_, t, components), x :: l ->
              apply (i + 1) components
                (f (decode t (path ^ "/" ^ string_of_int i) x))
                l
            | Nil, _ -> f
            | Cons _, [] -> assert false (* the length was checked *)
          in
          apply 0 components make l
        | j -> fail path "expected an array, got %s" (kind j))
      | Tuple (_, Unbuildable) -> no_json "tuple"
      | Struct { build = Build (components, make); _ } -> (
        match j with
        | Object kv ->
          let rec labels : type f. (f, a) components -> string list = function
            | Nil -> []
            | Cons (label, _, components) -> label :: labels components
          in
          let labels = labels components in
          List.iter
            (fun (k, _) ->
              if not (List.mem k labels) then fail path "unexpected field %S" k)
            kv;
          let rec apply : type f. (f, a) components -> f -> a =
           fun components f ->
            match components with
            | Nil -> f
            | Cons (label, t, components) ->
              apply components (field t label (List.assoc_opt label kv) f)
          and field : type c r.
              c ty -> string -> ParsedJson.t option -> (c -> r) -> r =
           fun t label j f ->
            let path = path ^ "/" ^ label in
            match j with
            | None ->
              if is_optional t then f (absent t) else fail path "missing field"
            | Some j -> f (optional_field t path j)
          in
          apply components make
        | j -> fail path "expected an object, got %s" (kind j))
      | Struct { build = Unbuildable; name; _ } -> no_json name
      | Enum { name; _ } when is_optional_name name -> (
        match j with
        | Object [] | Null | String "Absent" -> absent ty
        | Object [("Present", x)] -> present ty (path ^ "/Present") x
        | j -> fail path "expected an optional value, got %s" (kind j))
      | Enum { name; cases = []; _ } -> no_json name
      | Enum { name; cases; _ } -> (
        let find k = List.find_opt (fun (Case (c, _, _)) -> c = k) cases in
        match j with
        | String k -> (
          match find k with
          | Some (Case (_, Unit, make)) -> make ()
          | Some _ -> fail path "constructor %s of %s expects a content" k name
          | None -> fail path "unknown constructor %S of %s" k name)
        | Object [(k, x)]
          when not (List.for_all (fun (Case (_, t, _)) -> is_unit t) cases) -> (
          match find k with
          | Some (Case (_, t, make)) when not (is_unit t) ->
            make (decode t (path ^ "/" ^ k) x)
          | Some _ -> fail path "constructor %s of %s has no content" k name
          | None -> fail path "unknown constructor %S of %s" k name)
        | j -> fail path "expected a constructor of %s, got %s" name (kind j))
      | External (module E) -> E.from_json pos (ParsedJson.to_string j)
      | Function -> no_json "function"
      | Polymorphic -> no_json "polymorphic"
      | Dynamic -> no_json "dynamically typed"
    and arity : type f r. (f, r) components -> int = function
      | Nil -> 0
      | Cons (_, _, c) -> 1 + arity c
    and absent : type a. a ty -> a = function
      | Enum { cases; _ } -> (
        match
          List.find_map
            (fun (Case (_, t, make)) ->
              match t with Unit -> Some (make ()) | _ -> None)
            cases
        with
        | Some v -> v
        | None -> assert false)
      | _ -> assert false
    and present : type a. a ty -> string -> ParsedJson.t -> a =
     fun ty path j ->
      match ty with
      | Enum { cases; _ } -> (
        match List.find_opt (fun (Case (_, t, _)) -> not (is_unit t)) cases with
        | Some (Case (_, t, make)) -> make (decode t path j)
        | None -> assert false)
      | _ -> assert false
    (* A structure field of an optional type accepts its content as well as
       the forms of an optional value, the first that matches *)
    and optional_field : type a. a ty -> string -> ParsedJson.t -> a =
     fun ty path j ->
      if is_optional ty then
        match present ty path j with
        | v -> v
        | exception (Invalid_json _ as e) -> (
          match decode ty path j with
          | v -> v
          | exception Invalid_json _ -> raise e)
      else decode ty path j
    in
    match ParsedJson.of_string text with
    | exception ParsedJson.Syntax_error (offset, msg) ->
      fail "" "%s at byte %d" msg offset
    | j -> decode ty "" j
end

let equal = Value.equal_values
let compare = Value.compare_values

(* Catala types utils *)

module type CatalaType = sig
  type t

  val rtype : t Value.ty
end

module Optional = struct
  type 'a t = Absent | Present of 'a

  let rtype t =
    Value.Enum
      {
        name =
          (match Print.get_lang () with
          | `En -> "Optional"
          | `Fr -> "Optionnel"
          | `Pl -> "Opcjonalny");
        constr =
          (function
          | Absent ->
            ( 0,
              (match Print.get_lang () with
              | `En | `Fr -> "Absent"
              | `Pl -> "Nieobecny"),
              None )
          | Present v ->
            ( 1,
              (match Print.get_lang () with
              | `En -> "Present"
              | `Fr -> "Présent"
              | `Pl -> "Obecny"),
              Some (Value.embed t v) ));
        (* The JSON forms are not localised *)
        cases =
          [
            Value.Case ("Absent", Value.Unit, fun () -> Absent);
            Value.Case ("Present", t, fun v -> Present v);
          ];
      }

  let of_option = function Some x -> Present x | None -> Absent
end

module type ExternalTypeSpec = sig
  type t
  (** The embedded type *)

  val name : string
  (** Catala name of the type (capitalised) *)

  val equal : code_location -> t -> t -> bool

  val compare : code_location -> t -> t -> int
  (** Standard [compare] function: must return -1, 0 or 1 depending on whether
      the left-hand side is respectively smaller, equal or greater than the
      right-hand side *)

  val print : t -> string
  (** User-directed printing of the value *)

  val to_json : t -> string
  val from_json : code_location -> string -> t
end

module ExternalType (Spec : ExternalTypeSpec) :
  CatalaType with type t = Spec.t = struct
  module E : Value.External with type t = Spec.t = struct
    include Spec

    type _ Value.external_tag += T : t Value.external_tag
  end

  type t = Spec.t

  let rtype = Value.External (module E)
end

(* -- *)

(** {1 Execution traces} *)

type trace_kind =
  | ScopeCall of trace_ident_decl
  | ScopeVarDef of { var : trace_ident_decl; io : io_log }
  | LocalVarDef of string
  | LocalTupDef of string list
  | FunCall of trace_ident_decl
  | BranchingCondition
  | IfBranching
  | MatchBranching of { constructor_name : string }
  | Assertion
  | Exception of {
      label : (string * code_location) option;
      cons_pos : code_location;
    }
  | Error of {
      error : error;
      locs : code_location list;
      message : string option;
    }

and trace_ident_decl = { name : string; decl_pos : code_location }

type trace_element = {
  kind : trace_kind;
  pos : code_location;
  value : Value.t option;
  sub_trace : trace;
}

and trace = trace_element list

type trace_node = {
  kind : trace_kind;
  pos : code_location;
  mutable value : Value.t option;
  mutable sub_rev_nodes : trace_node list;
  parent : trace_node option;
}

type trace_context = {
  mutable current_node : trace_node option;
  mutable root_rev_trace : trace_node list;
  mutable exception_handled : bool;
}

let trace_context =
  { current_node = None; root_rev_trace = []; exception_handled = false }

let begin_trace kind pos =
  let node =
    {
      kind;
      pos;
      sub_rev_nodes = [];
      value = None;
      parent = trace_context.current_node;
    }
  in
  (match trace_context.current_node with
  | None ->
    (* root node *)
    trace_context.root_rev_trace <- node :: trace_context.root_rev_trace
  | Some parent_node ->
    parent_node.sub_rev_nodes <- node :: parent_node.sub_rev_nodes);
  trace_context.current_node <- Some node

let end_trace ?value () =
  (* pop the scope *)
  Option.iter (fun c -> c.value <- value) trace_context.current_node;
  match trace_context.current_node with
  | None -> (* Best effort by doing nothing *) ()
  | Some { parent = None; _ } ->
    (* No parent: root node *)
    trace_context.current_node <- None
  | Some { parent = some_p; _ } -> trace_context.current_node <- some_p

let single_trace kind pos =
  begin_trace kind pos;
  end_trace ()

let dummy_pos =
  {
    filename = "none";
    start_line = -1;
    start_column = -1;
    end_line = -1;
    end_column = -1;
    law_headings = [];
  }

let with_trace ~embed kind pos f =
  begin_trace kind pos;
  let r =
    try f ()
    with
    | Error (error, locs, message) as e
    when trace_context.exception_handled = false
    ->
      trace_context.exception_handled <- true;
      let pos, locs = match locs with [] -> dummy_pos, [] | h :: t -> h, t in
      single_trace (Error { error; locs; message }) pos;
      raise e
  in
  let value =
    let v = embed r in
    match v with Value.V (Unit, _) -> None | x -> Some x
  in
  end_trace ?value ();
  r

let finalize_trace_context = function
  | { current_node = None; root_rev_trace; exception_handled = _ } ->
    let rec f : trace_node -> trace_element =
     fun { kind; pos; value; sub_rev_nodes; parent = _ } ->
      { kind; pos; value; sub_trace = List.rev_map f sub_rev_nodes }
    in
    List.rev_map f root_rev_trace
  | { current_node = Some _; _ } ->
    failwith "inconsistent trace context state: expected to be at root node"

let retrieve_trace () : trace =
  (* [trace_context.root_rev_trace] is empty when there are no errors *)
  let rec pop_trace () =
    if trace_context.current_node = None || trace_context.root_rev_trace = []
    then trace_context
    else (
      end_trace ();
      pop_trace ())
  in
  pop_trace () |> finalize_trace_context

let reset_trace () =
  trace_context.current_node <- None;
  trace_context.root_rev_trace <- []

module BufferedJson = struct
  let seq f buf sq =
    match Seq.uncons sq with
    | None -> ()
    | Some (x, r) ->
      f buf x;
      let rec aux sq =
        match Seq.uncons sq with
        | None -> ()
        | Some (x, r) ->
          Buffer.add_string buf ",";
          f buf x;
          aux r
      in
      aux r

  let list f buf l = seq f buf (List.to_seq l)

  let quote buf str =
    Buffer.add_char buf '"';
    String.iter
      (function
        | ('"' | '\\') as c ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf c
        | '\n' -> Buffer.add_string buf "\\n"
        | '\t' -> Buffer.add_string buf "\\t"
        | '\r' -> Buffer.add_string buf "\\r"
        | '\x00' .. '\x1F' as c -> Printf.bprintf buf "\\u%04x" (int_of_char c)
        | c -> Buffer.add_char buf c)
      str;
    Buffer.add_char buf '"'

  let decimal buf d =
    if Q.den d = Z.one then Z.bprint buf (Q.num d)
    else Printf.bprintf buf "%a/%a" Z.bprint (Q.num d) Z.bprint (Q.den d)

  let code_location buf pos =
    Printf.bprintf buf
      {|{"file":%a,"start":{"line":%d,"character":%d},"end":{"line":%d,"character":%d}|}
      quote pos.filename pos.start_line pos.start_column pos.end_line
      pos.end_column;
    if pos.law_headings <> [] then
      Printf.bprintf buf {|,"law_headings":[%a]|} (list quote) pos.law_headings;
    Printf.bprintf buf "}"

  let rec runtime_value buf = function
    | Value.V (Unit, ()) -> Buffer.add_string buf "{}"
    | V (Bool, b) -> Buffer.add_string buf (string_of_bool b)
    | V (Money, m) -> Printf.bprintf buf {|"%s"|} (money_to_string m)
    | V (Integer, i) -> Printf.bprintf buf {|"%s"|} (integer_to_string i)
    | V (Decimal, d) -> Printf.bprintf buf {|"%a"|} decimal d
    | V (Date, d) -> quote buf (date_to_string d)
    | V (Duration, d) ->
      let y, m, d = Dates_calc.period_to_ymds d in
      (* Exact integer strings, like integers *)
      let p buf (s, v) = Printf.bprintf buf {|"%s":"%d"|} s v in
      Printf.bprintf buf {|{%a}|} (list p) ["years", y; "months", m; "days", d]
    | V (Enum en, e) -> (
      let _, constr, value = en.constr e in
      match value with
      | None -> quote buf constr
      | Some v -> Printf.bprintf buf {|{%a:%a}|} quote constr runtime_value v)
    | V (Struct str, s) ->
      let fields = str.fields s in
      let pfield buf (name, v) =
        Printf.bprintf buf {|%a:%a|} quote name runtime_value v
      in
      Printf.bprintf buf {|{%a}|} (list pfield) fields
    | V (Array t, a) ->
      Printf.bprintf buf {|[%a]|}
        (seq (fun buf v -> runtime_value buf (Value.embed t v)))
        (Stdlib.Array.to_seq a)
    | V (Dynamic, v) -> runtime_value buf v
    | V (Tuple (destr, _), a) ->
      Printf.bprintf buf {|[%a]|} (list runtime_value) (destr a)
    | V (Position, pos) -> code_location buf pos
    | V ((Function | Polymorphic), _) -> Buffer.add_string buf {|"<function>"|}
    | V (External (module E), v) -> Buffer.add_string buf (E.to_json v)

  let rec trace buf (t : trace) =
    Printf.bprintf buf "[%a]" (list trace_element) t

  and trace_element buf { kind; pos; value; sub_trace } =
    let value buf =
      match value with
      | None -> ()
      | Some v -> Printf.bprintf buf {|,"value":%a|} runtime_value v
    in
    let sub_trace buf =
      if sub_trace = [] then ()
      else Printf.bprintf buf {|,"trace":%a|} trace sub_trace
    in
    Printf.bprintf buf {|{"element":%a,"pos":%a%t%t}|} trace_kind kind
      code_location pos value sub_trace

  and trace_kind buf : trace_kind -> unit =
    let append_ident_decl buf { name; decl_pos } =
      Printf.bprintf buf {|,"name":%a,"decl_pos":%a|} quote name code_location
        decl_pos
    in
    let append_svar_io buf { io_input; io_output } =
      Printf.bprintf buf {|,"input":%S,"output":%b|}
        (match io_input with
        | NoInput -> "no_input"
        | OnlyInput -> "only_input"
        | Reentrant -> "reentrant")
        io_output
    in
    function
    | ScopeCall idecl ->
      Printf.bprintf buf {|{"kind":"scope_call"%a}|} append_ident_decl idecl
    | ScopeVarDef { var = idecl; io } ->
      Printf.bprintf buf {|{"kind":"scope_var"%a%a}|} append_ident_decl idecl
        append_svar_io io
    | LocalVarDef name ->
      Printf.bprintf buf {|{"kind":"local_var","name":%a}|} quote name
    | LocalTupDef names ->
      Printf.bprintf buf {|{"kind":"local_tup","names":[%a]}|} (list quote)
        names
    | FunCall idecl ->
      Printf.bprintf buf {|{"kind":"function_call"%a}|} append_ident_decl idecl
    | BranchingCondition -> Printf.bprintf buf {|{"kind":"branch_condition"}|}
    | IfBranching -> Printf.bprintf buf {|{"kind":"if_branching"}|}
    | MatchBranching { constructor_name } ->
      Printf.bprintf buf {|{"kind":"match_branching","constructor":%a}|} quote
        constructor_name
    | Assertion -> Printf.bprintf buf {|{"kind":"assertion"}|}
    | Exception { label; cons_pos } ->
      let lbl buf =
        match label with
        | None -> ()
        | Some (label, pos) ->
          Printf.bprintf buf {|,"label":%a,"pos":%a|} quote label code_location
            pos
      in
      Printf.bprintf buf {|{"kind":"exception"%t,"cons_pos":%a}|} lbl
        code_location cons_pos
    | Error { error; locs; message } ->
      let locs buf =
        if locs = [] then ()
        else
          Printf.bprintf buf {|,"related_pos":[%a]|} (list code_location) locs
      in
      Printf.bprintf buf {|{"kind":"error","type":%a%t,"message":%a}|} quote
        (error_to_string error) locs quote
        (Option.value ~default:(error_message error) message)
end

module Json = struct
  let str f x =
    let buf = Buffer.create 800 in
    f buf x;
    Buffer.contents buf

  open BufferedJson

  let runtime_value = str runtime_value
  let trace = str trace
end

let () =
  Printexc.set_uncaught_exception_handler
  @@ fun exc bt ->
  if trace_context.exception_handled then begin
    (* We caught an exception while collecting the trace meaning we meant to
       print the trace *)
    let trace = retrieve_trace () in
    Printf.printf "%s\n%!" (Json.trace trace)
  end;
  Printf.eprintf "\x1b[1;31m[ERROR]\x1b[m %s\n%!" (Printexc.to_string exc);
  if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt
(* TODO: the backtrace will point to the OCaml code; but we could make it point
   to the Catala code if we add #line directives everywhere in the generated
   code. *)

let handle_exceptions (exceptions : ('a * code_location) Optional.t array) :
    ('a * code_location) Optional.t =
  let len = Array.length exceptions in
  let rec filt_except i =
    if i < len then
      match exceptions.(i) with
      | Optional.Present _ as new_val -> new_val :: filt_except (i + 1)
      | Optional.Absent -> filt_except (i + 1)
    else []
  in
  match filt_except 0 with
  | [] -> Optional.Absent
  | [res] -> res
  | res ->
    error Conflict
      (List.map
         (function Optional.Present (_, pos) -> pos | _ -> assert false)
         res)

module Oper = struct
  let o_not = Stdlib.not
  let o_length a = Z.of_int (Array.length a)
  let o_toint_rat = integer_of_decimal
  let o_toint_mon = integer_of_money
  let o_torat_int = decimal_of_integer
  let o_torat_mon = decimal_of_money
  let o_tomoney_rat = money_of_decimal
  let o_tomoney_int = money_of_integer
  let o_getDay = day_of_month_of_date
  let o_getMonth = month_number_of_date
  let o_getYear = year_of_date
  let o_firstDayOfMonth = first_day_of_month
  let o_lastDayOfMonth = last_day_of_month
  let o_round_mon = money_round
  let o_round_rat = decimal_round
  let o_minus_int i1 = Z.sub Z.zero i1
  let o_minus_rat i1 = Q.sub Q.zero i1
  let o_minus_mon m1 = Z.sub Z.zero m1

  (* The components of durations are machine integers *)
  let checked_period pos f d1 d2 =
    let y1, m1, d1 = Dates_calc.period_to_ymds d1 in
    let y2, m2, d2 = Dates_calc.period_to_ymds d2 in
    let c a b =
      let r = f (Z.of_int a) (Z.of_int b) in
      if Z.fits_int r then Z.to_int r else error IntegerOverflow [pos]
    in
    let years = c y1 y2 in
    let months = c m1 m2 in
    let days = c d1 d2 in
    Dates_calc.make_period ~years ~months ~days

  let o_minus_dur pos d =
    checked_period pos Z.sub
      (Dates_calc.make_period ~years:0 ~months:0 ~days:0)
      d

  let o_and = ( && )
  let o_or = ( || )
  let o_xor : bool -> bool -> bool = ( <> )
  let o_eq t pos x1 x2 = equal t pos x1 x2
  let o_lt t pos x1 x2 = compare t pos x1 x2 < 0
  let o_lte t pos x1 x2 = compare t pos x1 x2 <= 0
  let o_gt t pos x1 x2 = compare t pos x1 x2 > 0
  let o_gte t pos x1 x2 = compare t pos x1 x2 >= 0
  let o_map = Array.map

  let o_map2 pos f a b =
    try Array.map2 f a b with Invalid_argument _ -> error NotSameLength [pos]

  let o_reduce f a =
    let len = Array.length a in
    if len = 0 then Optional.Absent
    else
      let r = ref a.(0) in
      for i = 1 to len - 1 do
        r := f !r a.(i)
      done;
      Optional.Present !r

  let o_concat = Array.append
  let o_filter f a = Array.of_list (List.filter f (Array.to_list a))
  let o_add_int_int i1 i2 = Z.add i1 i2
  let o_add_rat_rat i1 i2 = Q.add i1 i2
  let o_add_mon_mon m1 m2 = Z.add m1 m2

  let o_add_dat_dur r pos da du =
    try Dates_calc.add_dates ~round:r da du with
    | Dates_calc.AmbiguousComputation ->
      error
        (DateError "ambiguous date computation with no rounding mode specified")
        [pos]
    | Dates_calc.Overflow -> error IntegerOverflow [pos]

  let o_add_dur_dur pos d1 d2 = checked_period pos Z.add d1 d2
  let o_sub_int_int i1 i2 = Z.sub i1 i2
  let o_sub_rat_rat i1 i2 = Q.sub i1 i2
  let o_sub_mon_mon m1 m2 = Z.sub m1 m2

  let o_sub_dat_dat pos d1 d2 =
    try Dates_calc.sub_dates d1 d2
    with Dates_calc.Overflow -> error IntegerOverflow [pos]

  let o_sub_dat_dur r pos dat dur =
    o_add_dat_dur r pos dat (o_minus_dur pos dur)

  let o_sub_dur_dur pos d1 d2 = checked_period pos Z.sub d1 d2
  let o_mult_int_int i1 i2 = Z.mul i1 i2
  let o_mult_rat_rat i1 i2 = Q.mul i1 i2

  let o_mult_mon_rat i1 i2 =
    (* Multiply then round to nearest cent *)
    let rat_result = Q.mul (Q.of_bigint i1) i2 in
    round rat_result

  let o_mult_mon_int i1 i2 = o_mult_mon_rat i1 (decimal_of_integer i2)

  let o_mult_dur_int pos d m =
    (* The components of durations are machine integers *)
    let y, mo, da = Dates_calc.period_to_ymds d in
    let mul c =
      let r = Z.mul (Z.of_int c) m in
      if Z.fits_int r then Z.to_int r else error IntegerOverflow [pos]
    in
    let years = mul y in
    let months = mul mo in
    let days = mul da in
    Dates_calc.make_period ~years ~months ~days

  let o_div_int_int pos i1 i2 =
    (* It's not on the ocamldoc, but Q.div likely already raises this ? *)
    if Z.zero = i2 then error DivisionByZero [pos]
    else Q.div (Q.of_bigint i1) (Q.of_bigint i2)

  let o_div_rat_rat pos i1 i2 =
    if Q.zero = i2 then error DivisionByZero [pos] else Q.div i1 i2

  let o_div_mon_mon pos m1 m2 =
    if Z.zero = m2 then error DivisionByZero [pos]
    else Q.div (Q.of_bigint m1) (Q.of_bigint m2)

  let o_div_mon_rat pos m1 r1 =
    if Q.zero = r1 then error DivisionByZero [pos]
    else o_mult_mon_rat m1 (Q.inv r1)

  let o_div_mon_int pos m1 i1 = o_div_mon_rat pos m1 (decimal_of_integer i1)

  let o_div_dur_dur pos d1 d2 =
    let i1, i2 =
      try
        ( integer_of_int (Dates_calc.period_to_days d1),
          integer_of_int (Dates_calc.period_to_days d2) )
      with Dates_calc.AmbiguousComputation ->
        error (DateError "dividing durations that are not in days") [pos]
    in
    o_div_int_int pos i1 i2

  let o_fold = Array.fold_left
  let o_find f a = Optional.of_option (Array.find_opt f a)

  let o_sort_asc t pos f a =
    let a = Array.copy a in
    Array.stable_sort (fun x1 x2 -> compare t pos (f x1) (f x2)) a;
    a

  let o_sort_desc t pos f a =
    let a = Array.copy a in
    Array.stable_sort (fun x1 x2 -> -compare t pos (f x1) (f x2)) a;
    a

  let o_toclosureenv = Obj.repr
  let o_fromclosureenv = Obj.obj
end

include Oper

type hash = string

let modules_table : (string, hash) Hashtbl.t = Hashtbl.create 13
let values_table : (string * string, Obj.t) Hashtbl.t = Hashtbl.create 13

let types_table : (string * string, (module CatalaType)) Hashtbl.t =
  Hashtbl.create 13

let register_module modname values ?(types = []) hash =
  Hashtbl.add modules_table modname hash;
  List.iter (fun (id, v) -> Hashtbl.add values_table (modname, id) v) values;
  List.iter (fun (id, e) -> Hashtbl.add types_table (modname, id) e) types

let check_module m h =
  let h1 = Hashtbl.find modules_table m in
  if String.equal h h1 then Ok () else Error h1

let lookup_value qid =
  try Hashtbl.find values_table qid
  with Not_found ->
    failwith ("Could not resolve reference to " ^ fst qid ^ "." ^ snd qid)

let lookup_type qid =
  try Hashtbl.find types_table qid
  with Not_found ->
    failwith ("Could not resolve reference to " ^ fst qid ^ "." ^ snd qid)
