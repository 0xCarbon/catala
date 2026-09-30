#!/bin/sh
# Usage: run.sh FILE BACKEND SCOPE EXPECTED_EXIT EXPECTED_TEXT
# Runs the scope with the clerk of the installed layout, from a temporary copy
# of the project, and checks its exit code and output.
if [ $# -ne 5 ]; then
  echo "run.sh: expected 5 arguments, got $#: $*" >&2
  exit 2
fi
clerk="$(cd ../../../../install/default/bin && pwd)/clerk"
d=$(mktemp -d)
f=$1; shift
cp "$f" "$d"
out=$(cd "$d" && "$clerk" run "$f" --scope "$2" --backend "$1" 2>&1)
code=$?
rm -rf "$d"
if [ "$code" -ne "$3" ] || ! printf '%s' "$out" | grep -q "$4"; then
  printf '%s on %s: exit %s, expected %s with "%s":\n%s\n' "$2" "$1" "$code" "$3" "$4" "$out"
  exit 1
fi
