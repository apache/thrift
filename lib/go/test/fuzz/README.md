# Go fuzzing README

To build the fuzz targets, simply run `make check` in this directory

To reproduce a bug, update the code in the fuzz_test.go file, pass the right input buffer in there and update the code to call the relevant function.

We currently have the following fuzz targets:

* FuzzTutorial -- a fuzzer which spins up an mini server and fuzzes it with random data, following the tutorial example
* FuzzParseCompact -- fuzzes the deserialization of the Compact protocol
* FuzzParseBinary -- fuzzes the deserialization of the Binary protocol
* FuzzParseJson -- fuzzes the deserialization of the JSON protocol
* FuzzRoundtripCompact -- fuzzes the roundtrip of the Compact protocol
* FuzzRoundtripBinary -- fuzzes the roundtrip of the Binary protocol
* FuzzRoundtripJson -- fuzzes the roundtrip of the JSON protocol
