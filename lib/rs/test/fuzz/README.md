# Rust fuzzing README

To build the fuzz targets, simply run `make check` in this directory

These are standard cargo fuzz targets, so you can use the [standard cargo fuzz commands](https://rust-fuzz.github.io/book/introduction.html) to build and run them. You can also build with cargo fuzz directly after the initial build with `make check`, e.g. run `cargo fuzz run $fuzzer_name`

We currently have six fuzz targets:

* parse_compact -- fuzzes the deserialization of the Compact protocol
* parse_binary -- fuzzes the deserialization of the Binary protocol
* roundtrip_compact -- fuzzes the roundtrip of the Compact protocol (i.e. serializes and then deserializes and compares the result to the original)
* roundtrip_binary -- fuzzes the roundtrip of the Binary protocol
* structured_roundtrip_compact -- roundtrip, but starts from a valid compact thrift structure
* structured_roundtrip_binary -- roundtrip, but starts from a valid binary thrift structure

Some of the roundtrip fuzzers are structure aware, i.e. they generate mostly valid thrift structures, so we can also test serialization in addition to deserialization. We do have non structure aware roundtrip fuzzers as well, to match what's present in other languages (and also handle some corner cases).

We also have a corpus generator script that can be used to generate a corpus of fuzz inputs. It can be run with `cargo run --bin corpus_generator -- --output-dir <output_dir> --protocol <binary|compact> --buffer-size <buffer_size> --random-size <random_size>`.

This is useful for generating corpora for the parsing fuzzers, and can be used across all languages (for cases where the other languages don't have good native structure aware fuzzing support).