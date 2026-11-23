#!/usr/bin/env bash
# Runs the L test programs in tests/ through the interpreter (-i) and the
# compiler (-c, then clang), and compares the results.
#
#   ./run_tests.sh              all tests, both modes
#   ./run_tests.sh -i           interpreter only
#   ./run_tests.sh -c           compiler only
#   ./run_tests.sh core errors  only these folders of tests/ (any mode flag may be combined)
#
# Test layout:
#   tests/<dir>/<name>.lcd   the program
#   tests/<dir>/<name>.out   expected stdout of a program that runs successfully
#   tests/<dir>/<name>.err   expected start of stderr of a program that must fail
#                            (e.g. "Type error"); the exit status must be non-zero
#
# Exits with status 1 if any test fails.

set -u
cd "$(dirname "$0")"

modes=()
dirs=()
for arg in "$@"; do
  case "$arg" in
    -i|-c) modes+=("$arg") ;;
    -h|--help) sed -n '2,16p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) dirs+=("tests/$arg") ;;
  esac
done
[ ${#modes[@]} -eq 0 ] && modes=(-i -c)
[ ${#dirs[@]} -eq 0 ] && dirs=(tests/*/)

dune build 2>&1 | grep -v '^ld: warning' >&2
LLANG="$PWD/_build/default/bin/main.exe"
[ -x "$LLANG" ] || { echo "build failed: $LLANG not found" >&2; exit 1; }

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

pass=0
fail=0
failed=()

if [ -t 1 ]; then green=$'\033[32m'; red=$'\033[31m'; reset=$'\033[0m'; else green=; red=; reset=; fi

report() { # report <PASS|FAIL> <test> <mode> [reason]
  if [ "$1" = PASS ]; then
    pass=$((pass + 1))
    printf '%sPASS%s %-40s [%s]\n' "$green" "$reset" "$2" "$3"
  else
    fail=$((fail + 1))
    failed+=("$2 [$3]")
    printf '%sFAIL%s %-40s [%s] %s\n' "$red" "$reset" "$2" "$3" "$4"
  fi
}

# run_mode <mode> <file.lcd>: runs the program, leaving stdout in $tmp/out,
# stderr in $tmp/err and the exit status in $status. Sets $stage to the step
# that produced them: llang, clang or run.
run_mode() {
  if [ "$1" = -i ]; then
    stage=llang
    "$LLANG" -i "$2" >"$tmp/out" 2>"$tmp/err"; status=$?
    return
  fi
  stage=llang
  "$LLANG" -c "$2" >"$tmp/prog.ll" 2>"$tmp/err"; status=$?
  [ $status -ne 0 ] && { : >"$tmp/out"; return; }
  stage=clang
  clang -w "$tmp/prog.ll" -o "$tmp/prog" >"$tmp/out" 2>"$tmp/err"; status=$?
  [ $status -ne 0 ] && return
  stage=run
  "$tmp/prog" >"$tmp/out" 2>"$tmp/err"; status=$?
}

first_line() { head -n 1 "$1" | cut -c 1-100; }

for dir in "${dirs[@]}"; do
  for src in "${dir%/}"/*.lcd; do
    [ -e "$src" ] || continue
    name="${src#tests/}"; name="${name%.lcd}"
    base="${src%.lcd}"
    for mode in "${modes[@]}"; do
      run_mode "$mode" "$src"
      if [ -f "$base.out" ]; then
        if [ $status -ne 0 ]; then
          case $stage in
            clang) report FAIL "$name" "$mode" "clang rejected the IR: $(first_line "$tmp/err")" ;;
            *)     report FAIL "$name" "$mode" "exit $status: $(first_line "$tmp/err")" ;;
          esac
        elif diff -q "$tmp/out" "$base.out" >/dev/null; then
          report PASS "$name" "$mode"
        else
          report FAIL "$name" "$mode" "expected '$(first_line "$base.out")', got '$(first_line "$tmp/out")'"
        fi
      elif [ -f "$base.err" ]; then
        expected=$(first_line "$base.err")
        if [ "$stage" = clang ]; then
          report FAIL "$name" "$mode" "clang rejected the IR: $(first_line "$tmp/err")"
        elif [ $status -eq 0 ]; then
          report FAIL "$name" "$mode" "expected '$expected...', but it succeeded and printed '$(first_line "$tmp/out")'"
        elif [ "$(first_line "$tmp/err" | cut -c 1-${#expected})" = "$expected" ]; then
          report PASS "$name" "$mode"
        else
          report FAIL "$name" "$mode" "expected '$expected...', got '$(first_line "$tmp/err")'"
        fi
      else
        report FAIL "$name" "$mode" "no $base.out or $base.err file"
      fi
    done
  done
done

echo
echo "$pass passed, $fail failed"
if [ $fail -ne 0 ]; then
  printf '  %s\n' "${failed[@]}"
  exit 1
fi
