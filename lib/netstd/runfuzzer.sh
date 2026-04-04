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

# Check if a fuzzer name and type were provided
if [ $# -lt 2 ]; then
    echo "Usage: $0 <fuzzer_name> <fuzzer_type> [additional fuzzer arguments...]"
    echo "Available fuzzer names: json, compact, binary, json-roundtrip, compact-roundtrip, binary-roundtrip"
    echo "Available fuzzer types: libfuzzer, afl"
    exit 1
fi

FUZZER_NAME="$1"
FUZZER_TYPE="$2"
shift 2  # Remove the first two arguments, leaving only additional arguments

VALID_FUZZERS=("json" "compact" "binary" "json-roundtrip" "compact-roundtrip" "binary-roundtrip")
VALID_TYPES=("libfuzzer" "afl")

# Validate fuzzer name
VALID=0
for f in "${VALID_FUZZERS[@]}"; do
    if [ "$f" = "$FUZZER_NAME" ]; then
        VALID=1
        break
    fi
done

if [ $VALID -eq 0 ]; then
    echo "Invalid fuzzer name: $FUZZER_NAME"
    echo "Available fuzzers: json, compact, binary, json-roundtrip, compact-roundtrip, binary-roundtrip"
    exit 1
fi

# Validate fuzzer type
VALID=0
for t in "${VALID_TYPES[@]}"; do
    if [ "$t" = "$FUZZER_TYPE" ]; then
        VALID=1
        break
    fi
done

if [ $VALID -eq 0 ]; then
    echo "Invalid fuzzer type: $FUZZER_TYPE"
    echo "Available types: libfuzzer, afl"
    exit 1
fi

# Map fuzzer name and type to assembly name
# With the consolidated project, names follow the pattern:
# Thrift.FuzzTests.{Protocol}{FuzzerType}{Engine}
# where FuzzerType is "Parse" or "Roundtrip"
case "$FUZZER_NAME" in
    "json")
        if [ "$FUZZER_TYPE" = "libfuzzer" ]; then
            ASSEMBLY_NAME="Thrift.FuzzTests.JsonParseLibfuzzer"
        else
            ASSEMBLY_NAME="Thrift.FuzzTests.JsonParseAFL"
        fi
        ;;
    "compact")
        if [ "$FUZZER_TYPE" = "libfuzzer" ]; then
            ASSEMBLY_NAME="Thrift.FuzzTests.CompactParseLibfuzzer"
        else
            ASSEMBLY_NAME="Thrift.FuzzTests.CompactParseAFL"
        fi
        ;;
    "binary")
        if [ "$FUZZER_TYPE" = "libfuzzer" ]; then
            ASSEMBLY_NAME="Thrift.FuzzTests.BinaryParseLibfuzzer"
        else
            ASSEMBLY_NAME="Thrift.FuzzTests.BinaryParseAFL"
        fi
        ;;
    "json-roundtrip")
        if [ "$FUZZER_TYPE" = "libfuzzer" ]; then
            ASSEMBLY_NAME="Thrift.FuzzTests.JsonRoundtripLibfuzzer"
        else
            ASSEMBLY_NAME="Thrift.FuzzTests.JsonRoundtripAFL"
        fi
        ;;
    "compact-roundtrip")
        if [ "$FUZZER_TYPE" = "libfuzzer" ]; then
            ASSEMBLY_NAME="Thrift.FuzzTests.CompactRoundtripLibfuzzer"
        else
            ASSEMBLY_NAME="Thrift.FuzzTests.CompactRoundtripAFL"
        fi
        ;;
    "binary-roundtrip")
        if [ "$FUZZER_TYPE" = "libfuzzer" ]; then
            ASSEMBLY_NAME="Thrift.FuzzTests.BinaryRoundtripLibfuzzer"
        else
            ASSEMBLY_NAME="Thrift.FuzzTests.BinaryRoundtripAFL"
        fi
        ;;
esac

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
OUTPUT_DIR="$(dirname "$0")/Tests/Thrift.FuzzTests/bin/Debug/net9.0"
CORPUS_DIR="$(dirname "$0")/corpus/$FUZZER_NAME"

# Create corpus directory if it doesn't exist
mkdir -p "$CORPUS_DIR"

# Get project path
PROJECT_PATH="$OUTPUT_DIR/$ASSEMBLY_NAME.dll"

# Run the appropriate fuzzer
echo "Running $ASSEMBLY_NAME fuzzer..."
if [ "$FUZZER_TYPE" = "libfuzzer" ]; then
    "$LIBFUZZER" --target_path=dotnet --target_arg="$PROJECT_PATH" "$CORPUS_DIR" "$@"
else
    # For AFL, we need separate input and findings directories
    AFL_INPUT_DIR="$CORPUS_DIR/input"
    AFL_FINDINGS_DIR="$CORPUS_DIR/findings"
    mkdir -p "$AFL_INPUT_DIR"
    mkdir -p "$AFL_FINDINGS_DIR"

    # If input directory is empty, create a minimal test case
    if [ ! "$(ls -A $AFL_INPUT_DIR)" ]; then
        echo -n "test" > "$AFL_INPUT_DIR/test.txt"
    fi
    export AFL_SKIP_BIN_CHECK=1
    afl-fuzz -i "$AFL_INPUT_DIR" -o "$AFL_FINDINGS_DIR" -m none dotnet "$PROJECT_PATH" "$@"
fi