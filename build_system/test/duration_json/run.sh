#!/bin/sh
# Usage: run.sh BACKEND
# Runs scope S of durations.catala_en with -F json on BACKEND ("interpret" for
# the interpreter), with the clerk of the installed layout, from a temporary
# copy of the project, and checks its JSON output, whitespace aside.
if [ $# -ne 1 ]; then
  echo "run.sh: expected 1 argument, got $#: $*" >&2
  exit 2
fi
expected='{"neg":{"years":"0","months":"0","days":"-2"},"big":{"years":"0","months":"0","days":"1073741824"},"mixed":{"years":"1","months":"2","days":"3"},"zero":{"years":"0","months":"0","days":"0"}}'
clerk="$(cd ../../../../install/default/bin && pwd)/clerk"
d=$(mktemp -d)
cp durations.catala_en "$d"
if [ "$1" = interpret ]; then backend=""; else backend="--backend $1"; fi
out=$(cd "$d" && "$clerk" run durations.catala_en --scope S $backend -F json 2>/dev/null)
code=$?
rm -rf "$d"
json=$(printf '%s\n' "$out" | sed -n '/^{/,$p' | tr -d ' \n')
if [ "$code" -ne 0 ] || [ "$json" != "$expected" ]; then
  printf 'durations on %s: exit %s, expected\n%s\ngot\n%s\n' "$1" "$code" "$expected" "$out"
  exit 1
fi
