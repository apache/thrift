# C GLib Fuzzing README

To build the fuzz targets, run `make check` in this directory. The build system uses LLVM's libFuzzer for fuzzing the C GLib Thrift implementation.

These are standard libFuzzer targets, so you can run them using the standard libFuzzer interface. After building, you can run a fuzzer using:
```bash
./<fuzzer_name>
```

We currently have two fuzz targets:

* fuzz_parse_binary -- fuzzes the deserialization of the Binary protocol
* fuzz_parse_compact -- fuzzes the deserialization of the Compact protocol
* TODO: Add round trip fuzzers, similar to other languages.

The fuzzers use libFuzzer's built-in mutation engine to generate test cases. Each fuzzer implements the standard `LLVMFuzzerTestOneInput` interface.

For more information about libFuzzer and its options, see the [libFuzzer documentation](https://llvm.org/docs/LibFuzzer.html).

You can also use the corpus generator from the Rust implementation to generate initial corpus files that can be used with these C GLib fuzzers, since the wire formats are identical between implementations.
