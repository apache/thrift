# Fuzzing Apache Thrift

This document describes the fuzzing infrastructure and goals for Apache Thrift. 

We use [OSS-Fuzz](https://github.com/google/oss-fuzz) as our primary fuzzing platform to continuously test and improve the robustness of Thrift's hand-written and generated code.

## Goals

With fuzzing, we are focusing on testing the following key aspects across supported languages:

1. Security - Testing how the generated code handles malformed/malicious input
2. Serialization round-trip correctness - Ensuring that data stays identical if we serialize then deserialize it.

## Supported Languages

We currently maintain fuzzers for the following languages:

- Go
- c_glib (partially supported, needs round-trip support)
- C++
- Java/JVM (and other JVM languages)
- JavaScript
- Python
- Rust
- Swift

We are working on adding fuzzers for the following languages:

- netstd

## Fuzzer Types

For each supported language, we implement at minimum:

1. **Deserializer Fuzzer**
   - Takes raw fuzzer input and attempts to deserialize it into Thrift structures
   - Tests handling of malformed/unexpected input
   - Implemented for each supported protocol (Binary, Compact, JSON where available)

2. **Round-Trip Fuzzer** 
   - Deserializes fuzzer input, then re-serializes and verifies it matches
   - Ensures data integrity through serialization cycles
   - Tests both serialization and deserialization code paths

## Building and Running the Fuzzers

Each language has its own fuzzers under the `lib/<language>/test/fuzz` directory.
The fuzzers are built when building the language-specific code (using the normal build system), as regular binaries (without fuzzing support enabled), to ensure that there are no build breakages.

To ensure fuzzing can find issues as soon as possible, we will enable fuzzing support in CI once the fuzzers are stable.

Currently the only convenient, formally supported build with fuzzing support enabled is the via the oss-fuzz workflow. For languages where local fuzzing is easy to do, documentation is provided along with the fuzzers.

## OSS-Fuzz Integration

Our fuzzers run continuously on OSS-Fuzz. To view build status:

1. Visit the [OSS-Fuzz Status Dashboard](https://oss-fuzz-build-logs.storage.googleapis.com/index.html)
2. Look for the "thrift" project

The source code for the oss-fuzz build is [available upstream](https://github.com/google/oss-fuzz/tree/master/projects/thrift).

We aim to improve the fuzzers through viewing the fuzz introspector reports, available [here](https://introspector.oss-fuzz.com/project-profile?project=thrift).

*NB: The oss-fuzz integration will be significantly updated once all the language specific fuzzers are committed here.

## Contributing to the fuzzers

To contribute to the fuzzing effort - please look at https://issues.apache.org/jira/browse/THRIFT-5855 for the latest status and planned work. Once the ticket is closed,
we would still appreciate contributions that:

1. Add new fuzzers for unsupported languages
2. Improve existing fuzzers
3. Add test cases to corpus

If you do add or change a fuzzer, please remember to make corresponding changes to the oss-fuzz build script in case they are needed.

Please see CONTRIBUTING.md for general contribution guidelines.