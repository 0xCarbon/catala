#!/bin/sh
# Usage: run.sh MAIN_FILE EXPECTED_TEXT
# Runs explain --inline-mod-uses on MAIN_FILE, given relative to the current
# directory, in a temporary copy of this directory, with catala and its plugins
# in their installed layout, and checks that the output contains EXPECTED_TEXT.
if [ $# -ne 2 ]; then
  echo "run.sh: expected 2 arguments, got $#: $*" >&2
  exit 2
fi
bin="$(cd ../../../../../install/default/bin && pwd)"
d=$(mktemp -d)
cp ./*.catala_en ./*.catala_fr "$d"
out=$(cd "$d" && "$bin/clerk" start >/dev/null 2>&1 &&
  "$bin/catala" explain --stdlib _build/libcatala -s Test --dot -o - \
    --inline-mod-uses "$1" 2>&1)
code=$?
rm -rf "$d"
if [ "$code" -ne 0 ] || ! printf '%s' "$out" | grep -qF "$2"; then
  printf '%s: exit %s, expected 0 with "%s":\n%s\n' "$1" "$code" "$2" "$out"
  exit 1
fi
