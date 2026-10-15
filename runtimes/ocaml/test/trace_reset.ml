(* Trace collection is reusable after an evaluation failed. *)
open Catala_runtime
let pos = { filename="trace_reset"; start_line=1;start_column=0;end_line=1;end_column=1;law_headings=[] }
let fails error =
  reset_trace ();
  (try with_trace ~embed:(Value.embed Value.Unit) Assertion pos
    (fun () -> raise (Error (error,[pos],None))) with Error _ -> ());
  let rec recorded traces = List.find_map (fun (t:trace_element) ->
    match t.kind with Error {error;_} -> Some error | _ -> recorded t.sub_trace) traces in
  let observed=recorded (retrieve_trace ()) in
  reset_trace ();
  assert (observed=Some error)
let () =
  fails AssertionFailed;
  fails DivisionByZero;
  (* A successful evaluation must not retain a previous error either. *)
  with_trace ~embed:(Value.embed Value.Unit) Assertion pos (fun () -> ());
  assert (Json.trace (retrieve_trace ())<>"[]");
  reset_trace ();
  assert (retrieve_trace ()=[])
