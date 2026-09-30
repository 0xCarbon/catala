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

(** JSON encoding functions for Catala values *)

(** {1 Runtime values to JSON encodings correspondence}

    {v
    - Unit: {} (empty JSON object)

    - Bool: true | false (JSON boolean)

    - Money: 123 | 123.45 (JSON number) | "123.45" (JSON string holding a JSON
      number)
      Note:
        - all representations map to monetary units and not cents
        - digits beyond the cent are truncated

    - Integer: 123 | 1e3 (JSON number with an integral value) | "-123" (JSON
      string of decimal digits)

    - Decimal: 123.45 (JSON number) | "123.45" (JSON string holding a JSON
      number) | "1/3" (JSON string, fraction with a non-zero denominator)

    Numbers are decoded exactly from their literal, never through a binary
    float, and numeric strings follow the patterns of the JSON schema.

    - Date: "1970-01-31" (JSON string)
      Note: we rely on [Dates_calc.date_of_string]

    - Duration: "1 years", "2 months" or "12 days" (JSON string)

    - Enum:
      - Unit constructors: "A" (JSON string)
      - Non-unit constructors: {"B": <json value>} (JSON object)

    - Struct: { "x": <json value>, "y": <json value>, ...} (JSON object)

    - Array: [ <json value>, <json value>, ...] (JSON array)

    - Tuple: [ <json value>, <json value>, ...] (JSON array)
    v} *)

open Definitions
open Catala_runtime

val make_encoding : decl_ctx -> typ -> Value.t Json_encoding.encoding
(** Computes a JSON encoding of a Catala type using [Value.t] as an intermediate
    representation. *)

val scope_input_encoding :
  ScopeName.t -> decl_ctx -> typ -> Value.t Json_encoding.encoding
(** Same as [make_encoding] but adds a title and a description to the generated
    JSON-schema expliciting that this represent a scope input structure. *)

val scope_output_encoding :
  ScopeName.t -> decl_ctx -> typ -> Value.t Json_encoding.encoding
(** Same as [make_encoding] but adds a title and a description to the generated
    JSON-schema expliciting that this represent a scope output structure. *)

val parse_json :
  ?pos:Catala_utils.Pos.t -> Value.t Json_encoding.encoding -> string -> Value.t
(** Parse a JSON text using the given encoding as validation schema. *)

val convert_to_dcalc :
  decl_ctx -> 'm mark -> typ -> Value.t -> (dcalc, 'm) gexpr boxed
(** Conversion function from a [Value.t] to a default calculus expression. *)

val convert_to_lcalc :
  decl_ctx -> 'm mark -> typ -> Value.t -> (lcalc, 'm) gexpr boxed
(** Conversion function from a [Value.t] to a lambda calculus expression. *)
