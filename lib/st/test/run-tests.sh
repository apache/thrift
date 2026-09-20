#!/usr/bin/env bash
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#
# Files lib/st/thrift.st into a Pharo image together with the SUnit suites in
# this directory, runs them, and reports the result.
#
# lib/st has no build integration - it is not in configure.ac and not in a
# SUBDIRS - so there is no "make check" to hang this on. This script is it.
#
# usage: run-tests.sh --thrift <compiler> [--pharo <dir>] [--work <dir>]
#
#   --thrift  the thrift compiler to generate RecursionDepthTest.st with.
#   --pharo   a directory holding "pharo" and "Pharo.image". Defaults to
#             $PHARO_DIR, then to a "pharo" on PATH.
#   --work    scratch directory. Defaults to a temporary one, removed on exit.

set -euo pipefail

# Pharo reports a test failure on stdout and still exits 0, so these counts are
# the only signal there is. RUN is checked as well as PASS: a suite that fails
# to file in registers no tests at all and would otherwise report
# "RUN=0 PASS=0 FAIL=0" and read as success.
#
# Update these when adding or removing a test.
declare -A EXPECTED=(
    [TProtocolStringSizeLimitTest]=11
    [TProtocolRecursionDepthTest]=6
    [TTransportReadAllTest]=6
    [TProtocolShortReadTest]=10
)

THRIFT=""
PHARO_DIR="${PHARO_DIR:-}"
WORK=""
KEEP_WORK=0

die() { echo "error: $*" >&2; exit 1; }

while [ $# -gt 0 ]; do
    case "$1" in
        --thrift) THRIFT="${2:-}"; shift 2 ;;
        --pharo)  PHARO_DIR="${2:-}"; shift 2 ;;
        --work)   WORK="${2:-}"; KEEP_WORK=1; shift 2 ;;
        -h|--help) sed -n '19,30p' "$0"; exit 0 ;;
        *) die "unknown argument: $1" ;;
    esac
done

here="$(cd "$(dirname "$0")" && pwd)"
lib_st="$(dirname "$here")"

[ -n "$THRIFT" ] || die "no thrift compiler given; pass --thrift <path>"
[ -x "$THRIFT" ] || die "not an executable thrift compiler: $THRIFT"

if [ -z "$PHARO_DIR" ]; then
    command -v pharo >/dev/null 2>&1 || die "no Pharo found; pass --pharo <dir> or set PHARO_DIR"
    PHARO_DIR="$(dirname "$(command -v pharo)")"
fi
[ -x "$PHARO_DIR/pharo" ]     || die "no executable 'pharo' in $PHARO_DIR"
[ -f "$PHARO_DIR/Pharo.image" ] || die "no Pharo.image in $PHARO_DIR"
pharo_dir="$(cd "$PHARO_DIR" && pwd)"

if [ -z "$WORK" ]; then
    WORK="$(mktemp -d)"
    trap 'rm -rf "$WORK"' EXIT
fi
mkdir -p "$WORK"
work="$(cd "$WORK" && pwd)"

echo "Pharo:    $("$pharo_dir/pharo" "$pharo_dir/Pharo.image" eval "Smalltalk version" 2>/dev/null | tail -1)"
echo "compiler: $("$THRIFT" -version)"

cp "$lib_st/thrift.st" "$here"/*.st "$work/"

# TProtocolRecursionDepthTest drives the limit through generated struct
# read/write code, so the generated code has to exist first.
"$THRIFT" --gen st -out "$work" "$here/RecursionDepthTest.thrift"
[ -f "$work/RecursionDepthTest.st" ] || die "the compiler produced no RecursionDepthTest.st"

# run_suite <suite class> <file to file in>...
run_suite() {
    local suite="$1"; shift
    local files=("$@")
    local script="| res |"$'\n'
    local f
    for f in "${files[@]}"; do
        script+="CodeImporter evaluateFileNamed: '$f'."$'\n'
    done
    script+="res := (Smalltalk at: #$suite) suite run."$'\n'
    # This has to be the last expression: "pharo eval" prints the value of the
    # final statement and discards the rest.
    script+="String streamContents: [:s |
        s nextPutAll: 'THRIFT-ST RUN='; print: res runCount;
          nextPutAll: ' PASS='; print: res passedCount;
          nextPutAll: ' FAIL='; print: res failureCount;
          nextPutAll: ' ERROR='; print: res errorCount]"

    ( cd "$work" && "$pharo_dir/pharo" "$pharo_dir/Pharo.image" eval "$script" ) 2>&1
}

status=0
for suite in "${!EXPECTED[@]}"; do
    echo
    echo "=== $suite ==="
    case "$suite" in
        TProtocolRecursionDepthTest)
            out="$(run_suite "$suite" thrift.st RecursionDepthTest.st "$suite.st")" ;;
        *)
            out="$(run_suite "$suite" thrift.st "$suite.st")" ;;
    esac
    echo "$out"

    line="$(printf '%s\n' "$out" | grep -o "THRIFT-ST RUN=[0-9]* PASS=[0-9]* FAIL=[0-9]* ERROR=[0-9]*" | tail -1 || true)"
    if [ -z "$line" ]; then
        echo "  -> FAILED: the suite produced no result line at all" >&2
        status=1
        continue
    fi

    run=${line#*RUN=};   run=${run%% *}
    pass=${line#*PASS=}; pass=${pass%% *}
    fail=${line#*FAIL=}; fail=${fail%% *}
    err=${line#*ERROR=}; err=${err%% *}
    want=${EXPECTED[$suite]}

    if [ "$run" -ne "$want" ]; then
        echo "  -> FAILED: ran $run tests, expected $want." >&2
        echo "     Either the file-in did not take, or the suite changed and the" >&2
        echo "     EXPECTED table at the top of this script needs updating." >&2
        status=1
    elif [ "$pass" -ne "$run" ] || [ "$fail" -ne 0 ] || [ "$err" -ne 0 ]; then
        echo "  -> FAILED: $pass of $run passed, $fail failures, $err errors." >&2
        status=1
    else
        echo "  -> ok: $pass of $run passed."
    fi
done

echo
# Not "[ ... ] && echo": as the last command of the script that would make a
# false condition the exit status, under set -e.
if [ "$KEEP_WORK" -eq 1 ]; then
    echo "work directory kept at $work"
fi
if [ "$status" -ne 0 ]; then
    echo "Smalltalk tests FAILED." >&2
else
    echo "Smalltalk tests passed."
fi
exit "$status"
