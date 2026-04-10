#!/bin/bash

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

set -e

# Check for SHARPFUZZ_DIR environment variable
if [ -z "$SHARPFUZZ_DIR" ]; then
    echo "Error: SHARPFUZZ_DIR environment variable is not set."
    echo "Please set SHARPFUZZ_DIR to the location of your SharpFuzz installation."
    echo "See README for installation instructions."
    exit 1
fi

# Verify libfuzzer-dotnet exists
LIBFUZZER="$SHARPFUZZ_DIR/libfuzzer-dotnet"
if [ ! -f "$LIBFUZZER" ]; then
    echo "Error: libfuzzer-dotnet not found at $LIBFUZZER"
    echo "Please ensure SharpFuzz is properly installed in $SHARPFUZZ_DIR"
    echo "See README for installation instructions."
    exit 1
fi

# Find the local Thrift compiler
THRIFT="$(dirname "$0")/../../compiler/cpp/thrift"
THRIFT=$(realpath "$THRIFT")

# Paths
THRIFT_FILE="$(dirname "$0")/../../test/FuzzTest.thrift"
GEN_DIR="$(dirname "$0")/Tests/Thrift.FuzzTests/gen-netstd"
FUZZERS_DIR="$(dirname "$0")/Tests/Thrift.FuzzTests"
OUTPUT_DIR="$FUZZERS_DIR/bin/Debug/net9.0"

# Step 1: Generate C# code from FuzzTest.thrift
if [ ! -x "$THRIFT" ]; then
  echo "Error: Thrift compiler not found at $THRIFT"
  exit 1
fi

# Clean and create the generated directory
rm -rf "$GEN_DIR"
mkdir -p "$GEN_DIR"

echo "[1/13] Generating C# code from $THRIFT_FILE ..."
"$THRIFT" --gen netstd:net9 -out "$GEN_DIR" "$THRIFT_FILE"
echo "C# code generated in $GEN_DIR."

# Step 2: Build all fuzzer projects
cd "$(dirname "$0")"

# Build all fuzzer combinations using the consolidated project
BUILD_COUNT=2
for protocol in Binary Compact Json; do
  for fuzzer_type_raw in Parse Roundtrip; do
    for engine in AFL Libfuzzer; do
      # Construct display name
      if [ "$fuzzer_type_raw" = "Parse" ]; then
        display_name="$protocol Protocol $engine"
        fuzzer_type=""
      else
        display_name="$protocol Protocol round-trip $engine"
        fuzzer_type="Roundtrip"
      fi
      
      echo "[$BUILD_COUNT/13] Building $display_name ..."
      dotnet build "$FUZZERS_DIR/Thrift.FuzzTests.csproj" \
        -p:Protocol=$protocol \
        -p:FuzzerType=$fuzzer_type_raw \
        -p:Engine=$engine
      
      BUILD_COUNT=$((BUILD_COUNT + 1))
    done
  done
done

# Step 3: Instrument the assemblies
echo "Instrumenting assemblies for fuzzing ..."

# Exclusions for instrumentation
EXCLUSIONS=("dnlib.dll" "SharpFuzz.dll" "SharpFuzz.Common.dll")

# Find and instrument fuzzing targets
while IFS= read -r -d '' dll; do
    dll_name=$(basename "$dll")
    skip=false
    for excl in "${EXCLUSIONS[@]}"; do
        if [[ "$dll_name" == "$excl" ]]; then
            skip=true
            break
        fi
    done
    if [[ "$dll_name" == System.*.dll ]]; then
        skip=true
    fi
    if [[ "$dll_name" == Thrift.FuzzTests.*.dll ]]; then
        skip=true  # Skip the fuzzer assemblies themselves
    fi
    if [ "$skip" = true ]; then
        echo "Skipping $dll_name"
        continue
    fi
    if [ "$skip" = false ]; then
        echo "Instrumenting $dll_name"
        sharpfuzz "$dll"
        if [ $? -ne 0 ]; then
            echo "An error occurred while instrumenting $dll"
            exit 1
        fi
    fi
done < <(find "$OUTPUT_DIR" -maxdepth 1 -type f -name "*.dll" -print0)

echo "Build and instrumentation complete." 