#!/bin/sh
# Usage: run.sh BACKEND
# Runs scope S of far_dates.catala_en with -F json on BACKEND ("interpret" for
# the interpreter), with the clerk of the installed layout, from a temporary
# copy of the project, and checks its JSON output, whitespace aside.
if [ $# -ne 1 ]; then
  echo "run.sh: expected 1 argument, got $#: $*" >&2
  exit 2
fi
expected='{"future":"275790-09-13","past":"-0738-02-03","year_zero":"0000-01-01","year_minus_one":"-0001-01-01","year_10000":"10000-01-01","usual":"2024-02-29"}'
clerk="$(cd ../../../../install/default/bin && pwd)/clerk"
d=$(mktemp -d)
cp far_dates.catala_en "$d"
if [ "$1" = interpret ]; then backend=""; else backend="--backend $1"; fi
out=$(cd "$d" && "$clerk" run far_dates.catala_en --scope S $backend -F json 2>/dev/null)
code=$?
rm -rf "$d"
json=$(printf '%s\n' "$out" | sed -n '/^{/,$p' | tr -d ' \n')
if [ "$code" -ne 0 ] || [ "$json" != "$expected" ]; then
  printf 'dates on %s: exit %s, expected\n%s\ngot\n%s\n' "$1" "$code" "$expected" "$out"
  exit 1
fi
