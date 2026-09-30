#!/bin/sh
# Usage: run.sh BACKEND EXPECTED_EXIT EXPECTED_TEXT
# Runs scope Mult with the clerk of the installed layout, from a temporary copy
# of the project, and checks its exit code and output.
clerk="$(cd ../../../../install/default/bin && pwd)/clerk"
d=$(mktemp -d)
cp overflow.catala_en "$d"
out=$(cd "$d" && "$clerk" run overflow.catala_en --scope Mult --backend "$1" 2>&1)
code=$?
rm -rf "$d"
if [ "$code" -ne "$2" ] || ! printf '%s' "$out" | grep -q "$3"; then
  printf 'backend %s: exit %s, expected %s with "%s":\n%s\n' "$1" "$code" "$2" "$3" "$out"
  exit 1
fi
