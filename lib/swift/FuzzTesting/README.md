# Swift Fuzzing README

The Swift Thrift implementation uses LLVM's libFuzzer for fuzzing.

## Fuzzer Structure

We currently have several fuzz targets that test different aspects of the Thrift implementation:

* FuzzParseBinary -- Tries to deserialize the code-generated FuzzTest struct from arbitrary input data using the binary protocol
* FuzzRoundtripBinary -- Tries to deserialize a FuzzTest struct and then tests roundtrip serialization/deserialization with the binary protocol
* FuzzParseCompact
* FuzzRoundtripCompact
* FuzzParseJSON
* FuzzRoundtripJSON

The fuzzers need a dummy main() to ensure that compilation in non-fuzzer modes doesn't regress.