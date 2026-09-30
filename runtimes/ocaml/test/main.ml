(* Reads the scope input with Value.from_json and prints the scope output as
   JSON, like [catala interpret -F json] *)
let () =
  let open Catala_runtime in
  let pos =
    {
      filename = "input";
      start_line = 0;
      start_column = 0;
      end_line = 0;
      end_column = 0;
      law_headings = [];
    }
  in
  let input =
    Value.from_json From_json_e2e.S_in.rtype pos (In_channel.input_all stdin)
  in
  print_endline
    (Json.runtime_value
       (Value.embed From_json_e2e.S.rtype (From_json_e2e.s input)))
