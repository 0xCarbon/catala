module Path = Catala_utils.Path
module Common = Clerk_backend
module Backend_paths = Clerk_backend.Backend_paths

let check = Alcotest.(check string)
let check_list = Alcotest.(check (list string))

(* JSON inputs are parsed strictly, keeping the literal of numbers *)

module Json = Catala_runtime.ParsedJson

let json_error text =
  match Json.of_string text with
  | _ -> Alcotest.failf "accepted invalid JSON %S" text
  | exception Json.Syntax_error (offset, msg) ->
    Printf.sprintf "%s@%d" msg offset

let test_parsed_json_syntax () =
  List.iter
    (fun (text, expected) -> check text expected (json_error text))
    [
      "[1, 2,]", "unexpected character@6";
      "{\"a\": 1,}", "expected '\"'@8";
      "[1] // c", "trailing characters@4";
      "01", "trailing characters@1";
      "1.", "invalid number@0";
      ".5", "unexpected character@0";
      "NaN", "unexpected character@0";
      "{\"a\": 1, \"a\": 2}", "duplicate key \"a\"@9";
      "\"\\ud800\"", "unpaired surrogate@7";
      "\"a\nb\"", "control character in string@2";
      "\"\xff\"", "invalid UTF-8 in string@0";
      "", "unexpected end of input@0";
    ];
  check "surrogate pair and escapes" "\"\xf0\x9f\x98\x80/\\n\""
    (Json.to_string (Json.of_string {|"\ud83d\ude00\/\n"|}));
  check "numbers keep their literal" {|[-0,1.50,2E+3,{"k":null,"t":true}]|}
    (Json.to_string
       (Json.of_string {| [ -0, 1.50 , 2E+3, {"k": null, "t": true} ] |}))

let test_parsed_json_numbers () =
  List.iter
    (fun (literal, expected) ->
      check literal expected (Q.to_string (Json.number_to_decimal literal)))
    [
      "1234567890123", "1234567890123";
      "0.1", "1/10";
      "-12.34", "-617/50";
      "1.5e3", "1500";
      "25E-2", "1/4";
      "123456789012345678901234567890.5", "246913578024691357802469135781/2";
    ];
  let opt f = function None -> "none" | Some x -> f x in
  List.iter
    (fun (s, expected) ->
      check ("integer " ^ s) expected
        (opt Z.to_string (Json.integer_of_string s)))
    ["-0012", "-12"; "0x1F", "none"; "1_000", "none"; "+7", "none"; "", "none"];
  List.iter
    (fun (n, expected) ->
      check ("integral number " ^ n) expected
        (opt Z.to_string (Json.integer_of_number n)))
    ["3", "3"; "3.0", "3"; "1e3", "1000"; "1.5", "none"; "1e1000001", "none"];
  List.iter
    (fun (s, expected) ->
      check ("decimal " ^ s) expected
        (opt Q.to_string (Json.decimal_of_string s)))
    [
      "-1/3", "-1/3";
      "0.5", "1/2";
      "1/0", "none";
      "0/000", "none";
      "1/-3", "none";
      ".5", "none";
      "1e", "none";
      "abc", "none";
    ];
  Alcotest.check_raises "exponent bound"
    (Invalid_argument "number_to_decimal: exponent out of range") (fun () ->
      ignore (Json.number_to_decimal "1e1000001"))

(* Runs [f] with the target OS forced, so Windows behaviour is exercised on the
   Linux CI — where the reftests, running with '/', cannot catch it. Restores
   [Path.win32] afterwards so tests don't leak the setting to one another. *)
let with_os win32 f =
  let saved = !Path.win32 in
  Path.win32 := win32;
  Fun.protect ~finally:(fun () -> Path.win32 := saved) f

(* [include_flags]/[classpath] join with [Filename.concat], so on Windows a
   joined separator is '\'. These tests check quoting and the path separator, not
   slash direction; normalise '\' -> '/' first (no-op on Linux). *)
let fwd s = String.map (function '\\' -> '/' | c -> c) s

(* VS Code launches clerk with a lower-case drive cwd ("c:\\...") while the fs
   drive is upper-case; a case-sensitive prefix compare then failed to
   relativize, corrupting include_dirs into drive-stripped absolutes. *)

let test_remove_prefix_drive_case () =
  with_os true
  @@ fun () ->
  check "lower-case-drive prefix removed from upper-case-drive path"
    {|lib\src\data|}
    (Path.remove_prefix ~cwd:{|c:\proj|} {|c:\proj|} {|C:\proj\lib\src\data|})

let test_reverse_path_no_drive_strip () =
  with_os true
  @@ fun () ->
  check "include-dir relativized to project root (drive not stripped)"
    {|lib\src\data\enums|}
    (Path.reverse ~cwd:{|c:\proj|} ~from_dir:{|c:\proj|} ~to_dir:{|.|}
       {|C:\proj\lib\src\data\enums|})

let test_make_relative_to_drive_case () =
  with_os true
  @@ fun () ->
  check "make_relative_to across a drive-case mismatch" {|b\c|}
    (Path.make_relative_to ~cwd:{|c:\proj|} ~dir:{|C:\proj\a|} {|C:\proj\a\b\c|})

(* A drive path needs a leading slash before "C:" (else "C:" is the URL
   authority); a UNC path's server IS the authority, so no extra leading slash. *)

let test_file_url_drive () =
  with_os true
  @@ fun () ->
  check "windows drive path -> /C:/..." "/C:/proj/file.catala_en"
    (Path.url_of_absolute {|C:\proj\file.catala_en|})

let test_file_url_unc () =
  with_os true
  @@ fun () ->
  check "windows UNC path -> server/share/... (server is the authority)"
    "server/share/dir/file.catala_en"
    (Path.url_of_absolute {|\\server\share\dir\file.catala_en|})

(* The override border guards must fail loudly on any platform, not just
   spaced-dir Windows: stored quote chars double-quote at emission, refs
   would expand in direct exec but quote-glue at emission. (The old
   check_path property — no vector ref in a path — is now static:
   Var.ref only accepts scalars, and emission rejects Splice in paths.) *)

module CVar = Clerk_utils.Var

let raises_compiler_error f =
  try
    ignore (f ());
    false
  with Catala_utils.Message.CompilerError _ -> true

let test_override_rejects_quote () =
  Alcotest.(check bool)
    "quote char in an override value is rejected" true
    (raises_compiler_error (fun () ->
         CVar.binding_of_words_override CVar.catala_flags [{|"boom"|}]))

let test_override_rejects_ref () =
  Alcotest.(check bool)
    "variable reference in an override value is rejected" true
    (raises_compiler_error (fun () ->
         CVar.binding_of_words_override CVar.catala_flags ["${builddir}/x"]))

let test_override_accepts_clean () =
  check_list "clean vector override passes through" ["-O"; "--trace"]
    (CVar.binding_to_words
       (CVar.binding_of_words_override CVar.catala_flags ["-O"; "--trace"]))

(* the CLI splits override values on spaces before kinds are known, so a
   scalar must rejoin them: a spaced path is one value, not two words *)
let test_override_rejoins_spaced_scalar () =
  check_list "spaced scalar value is kept whole" ["/a b/catala"]
    (CVar.binding_to_words
       (CVar.binding_of_words_override CVar.catala_exe ["/a"; "b/catala"]))

let test_override_accepts_single_scalar () =
  check_list "single-word scalar value passes through" ["catala.exe"]
    (CVar.binding_to_words
       (CVar.binding_of_words_override CVar.catala_exe ["catala.exe"]))

(* include_flags feeds rule-scoped ninja bindings spliced into compile
   commands, which the shell re-parses: each dir must stay a single Word so
   that a spaced path (C:\Program Files\...) is quoted as one shell word at
   emission. *)

let expr_words e =
  List.map
    (function
      | Ninja_utils.Expr.Word w -> w
      | Ninja_utils.Expr.Splice v -> "${" ^ CVar.name v ^ "}"
      | Ninja_utils.Expr.Raw s -> s)
    e

let test_include_flags_single_words () =
  check_list "include_flags: one Word per flag and per dir"
    ["-I"; "${tdir}/ocaml"; "-I"; "/opt/some dir/ocaml"]
    (List.map fwd
       (expr_words
          (Common.Flags.include_flags ~name:"ocaml" [{|/opt/some dir|}])))

(* The separator differs by OS; the reftests only run on Linux, so the Windows
   case needs a unit test. *)

let test_classpath_separator () =
  with_os true (fun () ->
      check "Java classpath: ';' on Windows" {|${tdir}/java;/opt/lib a/java|}
        (fwd (Backend_paths.classpath ~backend:"java" [{|/opt/lib a|}])));
  with_os false (fun () ->
      check "Java classpath: ':' on Unix" {|${tdir}/java:/opt/lib a/java|}
        (fwd (Backend_paths.classpath ~backend:"java" [{|/opt/lib a|}])))

let test_pythonpath_separator () =
  with_os true (fun () ->
      check "PYTHONPATH: ';' on Windows" {|C:/build/python;C:/proj/tests|}
        (Backend_paths.pythonpath [{|C:/build/python|}; {|C:/proj/tests|}]));
  with_os false (fun () ->
      check "PYTHONPATH: ':' on Unix" {|/build/python:/proj/tests|}
        (Backend_paths.pythonpath [{|/build/python|}; {|/proj/tests|}]))

(* A position literal embeds the source filename; on Windows its backslashes are
   an illegal escape (Java/Python) or silently wrong (C/OCaml) in the target
   string literal unless [format_pos] escapes them. *)

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  n = 0 || go 0

let pos_escaped fmt_pos =
  let pos = Catala_utils.Pos.from_info {|C:\proj\mod.catala_fr|} 1 2 3 4 in
  contains ~sub:{|C:\\proj\\mod.catala_fr|} (Format.asprintf "%a" fmt_pos pos)

let backends_format_pos =
  [
    "Java", Scalc.To_java.format_pos;
    "Python", Scalc.To_python.format_pos;
    "C", Scalc.To_c.format_pos;
    "OCaml", Lcalc.To_ocaml.format_pos;
  ]

(* javac decodes source with the platform encoding (cp1252 on Windows), so a
   generated Java string literal must be pure ASCII. [\uXXXX] is translated back
   before lexing, so the value is unchanged; above the BMP Java wants a UTF-16
   surrogate pair. [format_pos] is the seam: it embeds the source path. *)

let test_java_literal_ascii_only () =
  let pos = Catala_utils.Pos.from_info {|C:\Impôts\🎯\x.catala_fr|} 1 2 3 4 in
  let s = Format.asprintf "%a" Scalc.To_java.format_pos pos in
  Alcotest.(check bool)
    "literal is pure ASCII" true
    (String.for_all (fun c -> Char.code c < 0x80) s);
  Alcotest.(check bool) "BMP char escaped" true (contains ~sub:{|\u00f4|} s);
  Alcotest.(check bool)
    "astral char becomes a surrogate pair" true
    (contains ~sub:{|\ud83c\udfaf|} s)

(* Same hazard in the trace the runtime emits: an unescaped backslash makes the
   JSON unparseable, so the trace viewer rejects every Windows trace. *)

let test_trace_json_escaping () =
  let pos =
    Catala_runtime.
      {
        filename = {|baremes\tests\N007.catala_fr|};
        start_line = 1;
        start_column = 2;
        end_line = 3;
        end_column = 4;
        law_headings = [];
      }
  in
  let json =
    Catala_runtime.Json.trace
      [{ kind = BranchingCondition; pos; value = None; sub_trace = [] }]
  in
  Alcotest.(check bool)
    "trace escapes backslashes in the position filename" true
    (contains ~sub:{|"file":"baremes\\tests\\N007.catala_fr"|} json)

(* Thousands of '-C <dir> <file>' pairs overflow the Windows command-line limit,
   so clerk spills them to a jar argfile; backslash escapes there, so Windows
   paths must be forward-slashed and quoted for spaces. *)

let test_jar_argfile_escaping () =
  check "jar argfile forward-slashes and quotes Windows paths"
    {|-C
"C:/Program Files/build/app/java"
"Outer$Inner.class"|}
    (Backend_paths.jar_argfile_content
       [{|C:\Program Files\build\app\java|}, {|Outer$Inner.class|}])

(* Only emitted under [Sys.win32], out of reach of the Linux testsuite. *)

let test_cmd_concat_operand () =
  check "cmd copy operand quotes each file"
    {|"nul"+"C:\build\a@test"+"C:\build\spaced dir\b@test"|}
    (CVar.cmd_concat_operand
       [{|C:\build\a@test|}; {|C:\build\spaced dir\b@test|}])

(* [Value.from_json] reads the JSON forms of scope inputs, for values of every
   runtime type. *)

module R = Catala_runtime
module V = R.Value

let pos : R.code_location =
  {
    filename = "test";
    start_line = 1;
    start_column = 1;
    end_line = 1;
    end_column = 2;
    law_headings = [];
  }

type color = Red | Rgb of (R.integer * R.integer * R.integer)

type person = {
  name_id : R.integer;
  rate : R.decimal;
  income : R.money;
  birth : R.date;
  notice : R.duration;
  resident : bool;
  color : color;
  scores : R.integer array;
  spouse : R.money R.Optional.t;
  context_value : (R.integer * R.code_location) R.Optional.t;
  unit_field : unit;
}

let rgb_ty : (R.integer * R.integer * R.integer) V.ty =
  V.Tuple
    ( (fun (r, g, b) ->
        [V.embed V.Integer r; V.embed V.Integer g; V.embed V.Integer b]),
      V.Build
        ( V.Cons
            ( "0",
              V.Integer,
              V.Cons ("1", V.Integer, V.Cons ("2", V.Integer, V.Nil)) ),
          fun r g b -> r, g, b ) )

let color_ty : color V.ty =
  V.Enum
    {
      name = "Color";
      constr =
        (function
        | Red -> 0, "Red", None
        | Rgb x -> 1, "Rgb", Some (V.embed rgb_ty x));
      cases =
        [
          V.Case ("Red", V.Unit, fun () -> Red);
          V.Case ("Rgb", rgb_ty, fun x -> Rgb x);
        ];
    }

let with_pos_ty : (R.integer * R.code_location) V.ty =
  V.Tuple
    ( (fun (x, p) -> [V.embed V.Integer x; V.embed V.Position p]),
      V.Build
        ( V.Cons ("0", V.Integer, V.Cons ("1", V.Position, V.Nil)),
          fun x p -> x, p ) )

(* Labels are JSON keys: [name_id] is read from "name", as the field of a scope
   input structure without its [_in] suffix *)
let person_ty : person V.ty =
  V.Struct
    {
      name = "Person";
      fields = (fun _ -> []);
      build =
        V.Build
          ( V.Cons
              ( "name",
                V.Integer,
                V.Cons
                  ( "rate",
                    V.Decimal,
                    V.Cons
                      ( "income",
                        V.Money,
                        V.Cons
                          ( "birth",
                            V.Date,
                            V.Cons
                              ( "notice",
                                V.Duration,
                                V.Cons
                                  ( "resident",
                                    V.Bool,
                                    V.Cons
                                      ( "color",
                                        color_ty,
                                        V.Cons
                                          ( "scores",
                                            V.Array V.Integer,
                                            V.Cons
                                              ( "spouse",
                                                R.Optional.rtype V.Money,
                                                V.Cons
                                                  ( "context_value",
                                                    R.Optional.rtype with_pos_ty,
                                                    V.Cons
                                                      ( "unit_field",
                                                        V.Unit,
                                                        V.Nil ) ) ) ) ) ) ) ) )
                  ) ),
            fun name_id
              rate
              income
              birth
              notice
              resident
              color
              scores
              spouse
              context_value
              unit_field
            ->
              {
                name_id;
                rate;
                income;
                birth;
                notice;
                resident;
                color;
                scores;
                spouse;
                context_value;
                unit_field;
              } );
    }

let from_json ty text = V.from_json ty pos text
let z = Z.of_string
let q = Q.of_string

let invalid ty text =
  match from_json ty text with
  | _ -> Alcotest.failf "accepted invalid JSON %s" text
  | exception V.Invalid_json (_, msg) -> msg

let test_from_json_struct () =
  let p =
    from_json person_ty
      {|{"name": 123456789012345678901234567890, "rate": 0.1, "income": 12.34,
         "birth": {"year": 2000, "month": 2, "day": 29}, "notice": {"days": -3},
         "resident": true, "color": {"Rgb": [1, "2", 3e0]}, "scores": [1, 2],
         "spouse": 5, "unit_field": {}}|}
  in
  Alcotest.(check string)
    "integer beyond 63 bits" "123456789012345678901234567890"
    (Z.to_string p.name_id);
  Alcotest.(check bool) "exact decimal" true (Q.equal p.rate (q "1/10"));
  Alcotest.(check string) "exact money" "1234" (Z.to_string p.income);
  Alcotest.(check string) "date object" "2000-02-29" (R.date_to_string p.birth);
  Alcotest.(check (list int))
    "duration defaults" [0; 0; -3]
    (let y, m, d = R.duration_to_years_months_days p.notice in
     [y; m; d]);
  Alcotest.(check bool)
    "enum payload" true
    (match p.color with
    | Rgb (r, g, b) -> Z.equal r Z.one && Z.equal g (z "2") && Z.equal b (z "3")
    | Red -> false);
  Alcotest.(check int) "array" 2 (Array.length p.scores);
  Alcotest.(check bool) "boolean and unit" true (p.resident && p.unit_field = ());
  Alcotest.(check bool)
    "optional field given its content" true
    (p.spouse = R.Optional.Present (z "500"));
  Alcotest.(check bool)
    "omitted optional field" true
    (p.context_value = R.Optional.Absent)

let test_from_json_optional () =
  let opt = R.Optional.rtype V.Money in
  List.iter
    (fun text ->
      Alcotest.(check bool) text true (from_json opt text = R.Optional.Absent))
    [{|null|}; {|{}|}; {|"Absent"|}];
  Alcotest.(check bool)
    "Present" true
    (from_json opt {|{"Present": "1.5"}|} = R.Optional.Present (z "150"));
  let ctx = R.Optional.rtype with_pos_ty in
  Alcotest.(check bool)
    "value with its position" true
    (match from_json ctx {|{"Present": 7}|} with
    | R.Optional.Present (x, p) -> Z.equal x (z "7") && p = pos
    | Absent -> false)

let test_from_json_errors () =
  let check_error what expected ty text =
    Alcotest.(check string) what expected (invalid ty text)
  in
  check_error "unknown field" {|unexpected field "extra"|} person_ty
    {|{"extra": 1}|};
  check_error "missing field" "at /birth, missing field" person_ty
    {|{"name": 1, "rate": 0, "income": 0, "notice": {}, "resident": true,
       "color": "Red", "scores": [], "unit_field": {}}|};
  check_error "non-integral integer" "expected an integer, got the number 1.5"
    V.Integer "1.5";
  check_error "integer string" {|expected an integer, got the string "0x1F"|}
    V.Integer {|"0x1F"|};
  check_error "zero denominator" {|expected a decimal, got the string "1/0"|}
    V.Decimal {|"1/0"|};
  check_error "money fraction" {|expected money, got the string "1/3"|} V.Money
    {|"1/3"|};
  check_error "unknown constructor" {|unknown constructor "Blue" of Color|}
    color_ty {|"Blue"|};
  check_error "tuple arity" "at /Rgb, expected an array of 3 elements, got 2"
    color_ty {|{"Rgb": [1, 2]}|};
  check_error "date range" "at /month, expected an integer in [1, 12], got 13"
    V.Date {|{"year": 2000, "month": 13, "day": 1}|};
  check_error "duplicate key" {|duplicate key "a" at byte 9|}
    (R.Optional.rtype V.Unit) {|{"a": 1, "a": 2}|};
  check_error "strict syntax" "unexpected character at byte 6"
    (V.Array V.Integer) {|[1, 2,]|};
  Alcotest.check_raises "no JSON form for dynamic values"
    (Invalid_argument
       "Value.from_json: dynamically typed values cannot be read from JSON")
    (fun () -> ignore (from_json (V.Array V.Dynamic) "[1]"))

(* Durations have machine-integer components: arithmetic on them raises
   IntegerOverflow instead of wrapping *)

let days n = R.duration_of_numbers 0 0 n

let overflows f =
  match f () with
  | _ -> false
  | exception R.Error (R.IntegerOverflow, [_], None) -> true

let test_duration_product () =
  let half = Z.of_int (max_int / 2) in
  Alcotest.(check bool)
    "largest product" true
    (R.duration_to_years_months_days (R.Oper.o_mult_dur_int pos (days 2) half)
    = (0, 0, 2 * (max_int / 2)));
  Alcotest.(check bool)
    "product beyond max_int" true
    (overflows (fun () -> R.Oper.o_mult_dur_int pos (days 2) (Z.succ half)));
  Alcotest.(check bool)
    "integer beyond 64 bits" true
    (overflows (fun () ->
         R.Oper.o_mult_dur_int pos (days 1) (Z.pow (Z.of_int 10) 30)))

let () =
  let open Alcotest in
  run "Unit tests"
    [
      ( "Runtime value decoding from JSON",
        [
          test_case "structures and every field type" `Quick
            test_from_json_struct;
          test_case "optional values" `Quick test_from_json_optional;
          test_case "errors" `Quick test_from_json_errors;
        ] );
      ( "Duration arithmetic overflow",
        [test_case "product" `Quick test_duration_product] );
      ( "Iota-reduction",
        [
          test_case "#1" `Quick Shared_ast.Optimizations.test_iota_reduction_1;
          test_case "#2" `Quick Shared_ast.Optimizations.test_iota_reduction_2;
        ] );
      ( "File paths (Windows drive-case)",
        [
          test_case "remove_prefix drive-case" `Quick
            test_remove_prefix_drive_case;
          test_case "reverse_path no drive strip" `Quick
            test_reverse_path_no_drive_strip;
          test_case "make_relative_to drive-case" `Quick
            test_make_relative_to_drive_case;
        ] );
      ( "File URLs (Windows drive + UNC)",
        [
          test_case "file_url drive path" `Quick test_file_url_drive;
          test_case "file_url UNC path" `Quick test_file_url_unc;
        ] );
      ( "Clerk override border guards",
        [
          test_case "override rejects quote char" `Quick
            test_override_rejects_quote;
          test_case "override rejects variable ref" `Quick
            test_override_rejects_ref;
          test_case "override passes clean vector words" `Quick
            test_override_accepts_clean;
          test_case "override rejoins spaced scalar" `Quick
            test_override_rejoins_spaced_scalar;
          test_case "override passes single-word scalar" `Quick
            test_override_accepts_single_scalar;
        ] );
      ( "Clerk include-dir quoting (spaces in install dir)",
        [
          test_case "include_flags keeps each -I dir a single word" `Quick
            test_include_flags_single_words;
        ] );
      ( "Backend path separators (Windows drive-colon)",
        [
          test_case "classpath separator" `Quick test_classpath_separator;
          test_case "PYTHONPATH separator" `Quick test_pythonpath_separator;
        ] );
      ( "Backend position-filename escaping (Windows backslash)",
        List.map
          (fun (name, fmt_pos) ->
            test_case (name ^ " escapes backslashes") `Quick (fun () ->
                Alcotest.(check bool) name true (pos_escaped fmt_pos)))
          backends_format_pos );
      ( "Java source encoding (non-ASCII in generated literals)",
        [
          test_case "position literal is pure ASCII" `Quick
            test_java_literal_ascii_only;
        ] );
      ( "Runtime trace JSON escaping (Windows backslash)",
        [
          test_case "trace escapes a Windows path" `Quick
            test_trace_json_escaping;
        ] );
      ( "Java jar @argfile (command-line length + path escaping)",
        [
          test_case "argfile escapes a Windows path" `Quick
            test_jar_argfile_escaping;
        ] );
      ( "Windows test-report concatenation (cmd copy)",
        [
          test_case "copy operand quotes each file" `Quick
            test_cmd_concat_operand;
        ] );
      ( "Strict JSON input with exact numbers",
        [
          test_case "syntax" `Quick test_parsed_json_syntax;
          test_case "exact numbers" `Quick test_parsed_json_numbers;
        ] );
    ]
