# Python Fuzzing README

The Python Thrift implementation uses Atheris for fuzzing. Atheris is a coverage-guided, in-process fuzzer for Python that integrates with libFuzzer.

Unlike the C++ implementation, the Python fuzzers are not directly runnable in a local environment. Instead, Atheris generates Python programs that need to be executed through the appropriate build system.

We currently have several fuzz targets that test different aspects of the Thrift implementation:

* FuzzParseBinary -- fuzzes the deserialization of the Binary protocol
* FuzzParseBinaryAccelerated -- fuzzes the deserialization of the accelerated Binary protocol
* FuzzParseCompact -- fuzzes the deserialization of the Compact protocol
* FuzzParseCompactAccelerated -- fuzzes the deserialization of the accelerated Compact protocol
* FuzzRoundtripBinary -- fuzzes the roundtrip of the Binary protocol (i.e. serializes then deserializes and compares the result)
* FuzzRoundtripBinaryAccelerated -- fuzzes the roundtrip of the accelerated Binary protocol
* FuzzRoundtripCompact -- fuzzes the roundtrip of the Compact protocol
* FuzzRoundtripCompactAccelerated -- fuzzes the roundtrip of the accelerated Compact protocol

The fuzzers use Atheris's mutation engine to generate test cases. Each fuzzer implements the standard Atheris interface and uses common testing code from the fuzz test utilities in `fuzz_common.py`.

For more information about Atheris and its options, see the [Atheris documentation](https://github.com/google/atheris).

You can also use the corpus generator from the Rust implementation to generate initial corpus files that can be used with these Python fuzzers, since the wire formats are identical between implementations.
