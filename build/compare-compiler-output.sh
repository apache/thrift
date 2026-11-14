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

# Runs every generator of two thrift compilers on the same IDL files and
# fails if the exit status, the messages or the generated files differ.
#
#   build/compare-compiler-output.sh REFERENCE CANDIDATE IDL...
#
# The st generator stamps each method with the current minute and the java
# generator dates its @Generated annotation, so both trees have those removed
# before they are compared.

set -euo pipefail

if [ $# -lt 3 ]; then
  echo "usage: $0 REFERENCE CANDIDATE IDL..." >&2
  exit 2
fi

reference=$(realpath "$1")
candidate=$(realpath "$2")
shift 2

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# thrift --help exits with 1 after printing the generator list
generators() {
  { "$1" --help 2>&1 || true; } | sed -n 's/^  \([a-z_]*\) (.*/\1/p'
}

generators "$reference" > "$work/reference.gens"
generators "$candidate" > "$work/candidate.gens"
if ! diff "$work/reference.gens" "$work/candidate.gens"; then
  echo "The two compilers list different generators" >&2
  exit 1
fi
if [ ! -s "$work/reference.gens" ]; then
  echo "No generators found in the output of $reference --help" >&2
  exit 1
fi

# Runs one compiler and records its exit status, stdout and stderr next to
# its output. Both compilers run from the same path, so a message that names
# the program reads the same for both.
run() {
  local compiler=$1 out=$2 gen=$3 idl=$4 status=0
  mkdir -p "$out/gen"
  rm -f "$work/bin/thrift"
  cp "$compiler" "$work/bin/thrift"
  "$work/bin/thrift" -r -out "$out/gen" --gen "$gen" "$idl" > "$out/stdout" 2> "$out/stderr" || status=$?
  echo "$status" > "$out/status"
  find "$out/gen" -type f -exec sed -i.bak \
    -e "s/stamp: 'thrift [^']*'/stamp: 'thrift'/" \
    -e 's/, date = "[0-9-]*")/)/' {} +
  find "$out/gen" -name '*.bak' -delete
}

mkdir "$work/bin"

failed=0
checked=0
for idl in "$@"; do
  while read -r gen; do
    rm -rf "$work/reference" "$work/candidate"
    run "$reference" "$work/reference" "$gen" "$idl"
    run "$candidate" "$work/candidate" "$gen" "$idl"
    checked=$((checked + 1))
    if ! diff -r "$work/reference" "$work/candidate" > "$work/diff"; then
      echo "DIFFERENT: --gen $gen $idl"
      head -n 40 "$work/diff"
      failed=$((failed + 1))
    fi
  done < "$work/reference.gens"
done

echo "Compared $checked generator runs, $failed different"
[ "$failed" -eq 0 ]
