# Node.js Fuzzing README

The Node.js Thrift implementation uses Jazzer.js for fuzzing. Jazzer.js is a coverage-guided, in-process fuzzer for JavaScript that integrates with libFuzzer.

## Setup

1. Install Jazzer.js:
```bash
npm install --save-dev @jazzer.js/core
```

## Available Fuzzers

The Node.js Thrift implementation currently supports the following fuzz targets:

* `fuzz_parse_TJSONProtocol.js` - fuzzes the deserialization of the JSON protocol
* `fuzz_roundtrip_TJSONProtocol.js` - fuzzes the roundtrip of the JSON protocol (serialize -> deserialize -> compare)
* `fuzz_parse_TBinaryProtocol.js` - fuzzes the deserialization of the Binary protocol
* `fuzz_roundtrip_TBinaryProtocol.js` - fuzzes the roundtrip of the Binary protocol
* `fuzz_parse_TCompactProtocol.js` - fuzzes the deserialization of the Compact protocol
* `fuzz_roundtrip_TCompactProtocol.js` - fuzzes the roundtrip of the Compact protocol

## Running Fuzzers

To run a fuzzer, use the Jazzer.js CLI:

```bash
npx jazzer ./fuzz_parse_TJSONProtocol.js --corpus=./corpus
```

Where:
- `--corpus` points to a directory containing seed inputs (optional)

## Corpus Generation

You can use the corpus generator from the Rust implementation to generate initial corpus files that can be used with these Node.js fuzzers. For JSON protocol fuzzers, ensure the corpus contains valid JSON data.

## Adding New Fuzzers

To add a new fuzzer:

1. Create a new file in the `fuzz` directory
2. Import the appropriate helper functions from `fuzz_common.js`
3. Export a `fuzz` function that takes a Buffer parameter
4. Use either `createParserFuzzer` or `createRoundtripFuzzer` with the appropriate protocol factory

Example:
```javascript
const { createParserFuzzer } = require('./fuzz_common');

module.exports.fuzz = createParserFuzzer((transport) => {
  return new thrift.TJSONProtocol(transport);
});
```

For more information about Jazzer.js and its options, see the [Jazzer.js documentation](https://github.com/CodeIntelligenceTesting/jazzer.js). 