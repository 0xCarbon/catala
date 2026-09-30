## Changes since 1.3.0

One line per change, be concise and explicit. Document only external changes
in behavior visible for the end-users of the tooling.

* Fix JSON inputs: numbers given for integers, decimals and money are decoded
  exactly from their literal instead of through a binary float
  (`1234567890123` was read as `1234567890120` for a decimal, `0.29` as
  `$0.28`, integers beyond 2^53 were rounded). Numeric inputs are strict and
  match their JSON schema, which now gives the patterns of numeric strings:
  integers are integral numbers or strings of digits (`"0x1F"` and `"1_000"`
  are refused) and decimal fractions need a non-zero denominator (`"1/0"` was
  accepted as an infinite decimal). The input is parsed as strict JSON
  (RFC 8259): comments and `NaN`/`Infinity` are now rejected.

* Fix money printing: amounts are printed from their integer number of cents
  instead of through a binary float, which printed wrong cents beyond 2^46
  units (`$90,071,992,547,409.93` was output as `90071992547409.94`). This
  affects the JSON output, traces and the literals of the Python backend.

* Fix `json-schema`: alternative encodings of a value are combined with
  `anyOf` instead of `oneOf`, since inputs are decoded with the first encoding
  that matches. Schema validators refused e.g. `{"amount": 12}` for money,
  which matched both the `integer` and the `number` alternatives.

* OCaml runtime: `Value.from_json` reads values of every type (it was only
  implemented for external types), in the forms accepted for scope inputs by
  the interpreter. `Value.ty` describes how to build tuples, structures
  (`build`) and enumerations (`cases`); arrays carry the type of their
  elements. JSON literals `#[json = "..."] T` work for every named type in the
  interpreter and the OCaml backend; the C, Java and Python runtimes still read
  JSON literals of external types only.

* Fix `clerk`: the OCaml runtime is compiled with its own include path only. A
  Catala module named like an OCaml standard module (e.g. `Bool`, `String`),
  once compiled, made the OCaml backend fail with `Unbound value Bool.equal`.

* Fix duration multiplication: `duration * integer` raises the new runtime
  error `IntegerOverflow` when a component of the product does not fit a
  machine integer, on every backend. It wrapped or truncated silently
  (`2 day * 4611686018427387903` was `-2 days`) and crashed the interpreter
  with an uncaught `Z.Overflow` beyond 63 bits. Components are machine
  integers: 63-bit natively in OCaml, 32-bit under js_of_ocaml and in Java,
  `long` in C; Python integers are unbounded.

* Fix duration addition, subtraction and negation: they raise
  `IntegerOverflow` instead of wrapping silently
  (`4611686018427387903 day + 1 day` was negative).

* Fix the standard library on integers beyond machine integers, in every
  runtime: `List.nth_element` and `List.remove_nth_element` treat them as out
  of range (C and Java truncated them), `Money.round_to_decimal` and
  `Decimal.round_to_decimal` round without computing powers of ten larger than
  the value (they hung or crashed), `Date.of_year_month_day` reports an invalid
  date, and `List.sequence` raises `IntegerOverflow`.

* Fix date arithmetic on large durations: adding or subtracting years,
  months and days, and the difference of two dates, take time independent of
  their size (they looped month by month: `|2000-01-01| + 1000000000000 day`
  did not terminate, and C and Java exhausted the stack); a resulting year
  beyond a machine integer raises `IntegerOverflow`.

## Changes since 1.2.0

One line per change, be concise and explicit. Document only external changes
in behavior visible for the end-users of the tooling.

* [#1082](https://github.com/CatalaLang/catala/pull/1082) New lint warning
  for local variables (`let ... in`) that are never used.

* [#1058](https://github.com/CatalaLang/catala/pull/1058) Fixes a bug
  in the JSON output format of enumerations yielding errors such as:
  `Invalid_argument("Json_encoding.construct: consequence of non
  exhaustive Json_encoding.string_enum` and
  `Invalid_argument("Json_encoding.construct: consequence of bad
  union")`

* [#1069](https://github.com/CatalaLang/catala/pull/1069) Revamp of
  the `--trace` mechanism:
  - Added support in the `Java` backend;
  - Added `clerk run --trace ...` options.

* [#1075](https://github.com/CatalaLang/catala/pull/1075) Fixes a bug
  where, for large Catala programs, the java generated code is too
  large and would yield "code too large" error. We now split large
  methods into smaller methods whenever necessary.
  Also, removed law headings from positions in Java (same as C).

* [#1072](https://github.com/CatalaLang/catala/pull/1072) Handling of
  dependencies between Clerk targets:
  - Changes in `clerk.toml`:
    * added a `target.dependencies` field
    * removed the `target.include_sources` and `target.include_objects` fields.
      The new `--obj` CLI flag can now be used to install compiled objects to
      the targets directory
    * added a global `include_sources` field (on by default)
    * added a global project `name` field
  - Multiple targets and/or backends can now be specified to `clerk` commands
    `build`, `test` and `run`
  - `clerk` now also allows `--backend all` to compile/test/run all compatible
    backends
  - When building targets defined in `clerk.toml`, `clerk build` now generates
    library definition files in OCaml and Python (Java (#1093) and C (#1099)
    have been added in subsequent PRs)
  - In general, better handling of caching and faster builds (made sure in
    particular that the underlying `ninja` build process is run only once)

* [#1087](https://github.com/CatalaLang/catala/pull/1087/) Escape
  non-ascii characters in Java backend strings as unicode. All Java
  strings are now agnostic of encodings.

* [#1092](https://github.com/CatalaLang/catala/pull/1092) Fixes a bug
  in the `--gen-external` template generator where the generated OCaml
  code did not match the `ExternalType` functor signature: `equal` and
  `compare` were missing the `_pos` parameter, and `from_json` had
  incorrect signature `_pos t` instead of `_pos _s`.

* [#1093](https://github.com/CatalaLang/catala/pull/1093) Improvements
  over java target generation:
  - Added in the generated java target files the proper package and
    import declaration relative to, resp., their own package and
    (transitive) dependencies.
  - Following [#1072](https://github.com/CatalaLang/catala/pull/1072),
    building java targets now also generates a `maven` file
    (`pom.xml`) which can be used to easily compile and generate `jar`
    files.

* [#1110](https://github.com/CatalaLang/catala/pull/1110) Changes in
  `clerk.toml`:
  - `include_dirs` is now recursive and defaults to the project root
  - `exclude_dirs` is now available
  - they no longer control visibility, but the directories that will
    be scanned for sources by `clerk`.
