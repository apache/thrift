# Java Fuzzing README

The Java Thrift implementation uses Jazzer for fuzzing. Jazzer is a coverage-guided, in-process fuzzer for the JVM.

Unlike the C++ implementation, the Java fuzzers are not directly runnable in a local environment. Instead, Jazzer generates Java programs that need to be executed through the appropriate build system.

We currently have several fuzz targets that test different aspects of the Thrift implementation:

* FuzzParseBinary -- fuzzes the deserialization of the Binary protocol
* FuzzParseCompact -- fuzzes the deserialization of the Compact protocol
* FuzzParseJson -- fuzzes the deserialization of the JSON protocol
* FuzzRoundtripBinary -- fuzzes the roundtrip of the Binary protocol (i.e. serializes then deserializes and compares the result)
* FuzzRoundtripCompact -- fuzzes the roundtrip of the Compact protocol
* FuzzRoundtripJson -- fuzzes the roundtrip of the JSON protocol

The fuzzers use Jazzer's mutation engine to generate test cases. Each fuzzer implements the standard Jazzer interface and uses common testing code from the fuzz test utilities.

For more information about Jazzer and its options, see the [Jazzer documentation](https://github.com/CodeIntelligenceTesting/jazzer).

You can also use the corpus generator from the Rust implementation to generate initial corpus files that can be used with these Java fuzzers, since the wire formats are identical between implementations.
