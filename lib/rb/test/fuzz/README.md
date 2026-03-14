# Ruby Fuzzing README

The Ruby Thrift implementation uses [Ruzzy](https://github.com/trailofbits/ruzzy) for fuzzing. Ruzzy is a coverage-guided fuzzer for pure Ruby code and Ruby C extensions.

We currently have several fuzz targets that test different aspects of the Thrift implementation:

- `fuzz_parse_binary_protocol.rb` -- fuzzes deserialization of the Binary protocol
- `fuzz_parse_binary_protocol_accelerated.rb` -- fuzzes deserialization of the accelerated Binary protocol
- `fuzz_parse_compact_protocol.rb` -- fuzzes deserialization of the Compact protocol
- `fuzz_parse_json_protocol.rb` -- fuzzes JSON protocol messages
- `fuzz_roundtrip_binary_protocol.rb` -- fuzzes Binary roundtrips (deserialize, serialize, deserialize, compare)
- `fuzz_roundtrip_binary_protocol_accelerated.rb` -- fuzzes accelerated Binary roundtrips
- `fuzz_roundtrip_compact_protocol.rb` -- fuzzes Compact roundtrips
- `fuzz_roundtrip_json_protocol.rb` -- fuzzes JSON message roundtrips

The runnable files in this directory are tracer entrypoints. Ruzzy requires that pure Ruby fuzzing starts from a tracer script which then loads a separate harness, so do not invoke the matching `_harness.rb` files directly.

The fuzzers use Ruzzy's mutation engine to generate test cases. Each target uses common testing code from `fuzz_common.rb`.

For more information about Ruzzy and its options, see the [Ruzzy documentation](https://github.com/trailofbits/ruzzy).

You can also use the corpus generator from the Rust implementation to generate initial Binary and Compact corpora that can be reused by the Ruby fuzzers, since those wire formats are identical between implementations.

## Setup

Run these commands from the repository root:

```bash
apt install -y clang-19

# from https://github.com/trailofbits/ruzzy?tab=readme-ov-file#installing
MAKE="make --environment-overrides V=1" \
     CC="/usr/bin/clang-19" \
     CXX="/usr/bin/clang++-19" \
     LDSHARED="/usr/bin/clang-19 -shared" \
     LDSHAREDXX="/usr/bin/clang++-19 -shared" \
     gem install ruzzy

# Validate the fuzz directory.
# This generates test/fuzz/gen-rb, syntax-checks the fuzz scripts,
# and verifies that Fuzz::FuzzTest loads correctly.
make -C lib/rb/test/fuzz check
```

`make -C lib/rb check` now recurses into `lib/rb/test/fuzz`, so the same validation also runs as part of the normal Ruby `make check` flow.

## Running Fuzzers

The Makefile in this directory hides the `LD_PRELOAD` and `ASAN_OPTIONS` setup needed by Ruzzy.

Pure Ruby targets only need `fuzz-prepare`, which is the same work as `check`:

```bash
make -C lib/rb/test/fuzz fuzz-parse-binary
make -C lib/rb/test/fuzz fuzz-roundtrip-compact
make -C lib/rb/test/fuzz fuzz-parse-json
```

Accelerated targets rebuild `thrift_native` with sanitizer flags first:

```bash
make -C lib/rb/test/fuzz fuzz-parse-binary-accelerated
make -C lib/rb/test/fuzz fuzz-roundtrip-binary-accelerated
```

Use `CORPUS=...` to point at an input corpus. The path is resolved from `lib/rb`, which matches the old manual commands:

```bash
make -C lib/rb/test/fuzz fuzz-parse-binary \
    CORPUS=../rs/test/fuzz/corpus/binary

make -C lib/rb/test/fuzz fuzz-parse-compact \
    CORPUS=../rs/test/fuzz/corpus/compact
```

Use `FUZZ_ARGS=...` for extra libFuzzer-style arguments. For a short local run, the Makefile also provides bounded smoke targets:

```bash
make -C lib/rb/test/fuzz fuzz-smoke-parse-binary
make -C lib/rb/test/fuzz fuzz-smoke-parse-binary-accelerated \
    FUZZ_CC=/usr/bin/clang-19 \
    FUZZ_CXX=/usr/bin/clang++-19
```

If the default `clang` and `clang++` names are not correct for your system, override these variables for accelerated targets:

```bash
make -C lib/rb/test/fuzz fuzz-build-ext \
    FUZZ_CC=/usr/bin/clang-19 \
    FUZZ_CXX=/usr/bin/clang++-19
```

The underlying manual command from `lib/rb` is still:

```bash
# Memory allocation failures are common and low impact (DoS), so skip them for now.
# Like Python, the Ruby interpreter leaks data, so ignore these for now.
# Ruby recommends disabling sigaltstack.
export ASAN_OPTIONS="allocator_may_return_null=1:detect_leaks=0:use_sigaltstack=0"

LD_PRELOAD=$(ruby -e 'require "ruzzy"; print Ruzzy::ASAN_PATH') \
    ruby test/fuzz/fuzz_parse_binary_protocol.rb
```

## Rust Corpus Generator

We can use the Rust corpus generator to prepare corpora for the Binary and Compact protocol targets. Run it from the repository root:

```bash
cargo run --manifest-path lib/rs/test/fuzz/Cargo.toml --bin corpus_generator -- \
    --output-dir <output_dir> \
    --protocol <binary|compact> \
    --generate <num_files> \
    --buffer-size <buffer_size> \
    --random-size <random_size>
```

Reasonable values (determined empirically):

```bash
cargo run --manifest-path lib/rs/test/fuzz/Cargo.toml --bin corpus_generator -- \
    --output-dir ./lib/rs/test/fuzz/corpus/binary \
    --protocol binary \
    --generate 1000 \
    --buffer-size 65536 \
    --random-size 16384

cargo run --manifest-path lib/rs/test/fuzz/Cargo.toml --bin corpus_generator -- \
    --output-dir ./lib/rs/test/fuzz/corpus/compact \
    --protocol compact \
    --generate 1000 \
    --buffer-size 16384 \
    --random-size 16384
```

Then run a matching Ruby target:

```bash
make -C lib/rb/test/fuzz fuzz-parse-binary \
    CORPUS=../rs/test/fuzz/corpus/binary
```

The Rust corpus generator does not emit JSON protocol inputs, so use it only with the Binary and Compact Ruby targets.

## Troubleshooting

If libFuzzer prints `WARNING: no interesting inputs were found so far. Is the code instrumented for coverage?` and quickly shrinks the seed corpus to `corp: 1/1b`, coverage tracing is not active. That usually means the tracer script was bypassed or `ruzzy` was not installed with the expected sanitizer-enabled toolchain. Run the `.rb` files in this directory, not the `_harness.rb` files.

If an accelerated target starts using the pure Ruby implementation, rebuild the extension with `make -C lib/rb/test/fuzz fuzz-build-ext ...` and then rerun the accelerated target.
