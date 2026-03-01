# C++ Fuzzing README

To build the fuzz targets, run `make check` in this directory. The build system uses LLVM's libFuzzer for fuzzing the C++ Thrift implementation.

These are standard libFuzzer targets, so you can run them using the standard libFuzzer interface. After building, you can run a fuzzer using:
```bash
./<fuzzer_name>
```

We currently have six fuzz targets:

* FuzzParseBinary -- fuzzes the deserialization of the Binary protocol
* FuzzParseCompact -- fuzzes the deserialization of the Compact protocol
* FuzzParseJson -- fuzzes the deserialization of the JSON protocol
* FuzzRoundtripBinary -- fuzzes the roundtrip of the Binary protocol (i.e. serializes then deserializes and compares the result)
* FuzzRoundtripCompact -- fuzzes the roundtrip of the Compact protocol
* FuzzRoundtripJson -- fuzzes the roundtrip of the JSON protocol

The fuzzers use libFuzzer's built-in mutation engine to generate test cases. Each fuzzer implements the standard `LLVMFuzzerTestOneInput` interface and uses common testing code from `FuzzCommon.tcc`.

For more information about libFuzzer and its options, see the [libFuzzer documentation](https://llvm.org/docs/LibFuzzer.html). 

You can also use the corpus generator from the Rust implementation to generate initial corpus files that can be used with these C++ fuzzers, since the wire formats are identical between implementations.