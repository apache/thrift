# Ruby Protocol Benchmarks

This directory holds a small harness for quick Ruby protocol benchmarks.
Use it to spot read and write regressions in tree.

## Quick Start
Run the script with plain `ruby` from the repo root.

```sh
ruby test/rb/benchmarks/protocol_benchmark.rb
```

This runs the full benchmark set:

- Ruby binary
- Ruby compact
- Ruby JSON
- Header binary
- Header compact
- Header zlib
- C binary, if `thrift_native.so` loads

## Options
Use flags or env vars to tune a run.

```sh
ruby test/rb/benchmarks/protocol_benchmark.rb --large-runs 1 --small-runs 10000
```

- `--json`
- `THRIFT_BENCHMARK_LARGE_RUNS`
- `THRIFT_BENCHMARK_SMALL_RUNS`
- `THRIFT_BENCHMARK_SKIP_NATIVE=1`
- `THRIFT_BENCHMARK_SCENARIOS`

```sh
ruby test/rb/benchmarks/protocol_benchmark.rb --json > benchmark.json
```

> [!NOTE]
> `--json` keeps the warm-up pass, but only prints measured results.

> [!TIP]
> Set `THRIFT_BENCHMARK_SKIP_NATIVE=1` to force a pure-Ruby run.

## Scenario IDs
Use `--scenarios` or `THRIFT_BENCHMARK_SCENARIOS` to run only part of the matrix.

```sh
ruby test/rb/benchmarks/protocol_benchmark.rb --scenarios rb-bin-write-large,rb-json-read-large,hdr-zlib-read-small
```

Each ID has four parts:

- family: `rb`, `c`, or `hdr`
- protocol: `bin`, `cmp`, `json`, or `zlib`
- operation: `write` or `read`
- size: `large` or `small`

### Full Table

| ID | Family | Protocol | Operation | Size |
| --- | --- | --- | --- | --- |
| `rb-bin-write-large` | Ruby | binary | write | large |
| `rb-bin-read-large` | Ruby | binary | read | large |
| `c-bin-write-large` | C native | binary | write | large |
| `c-bin-read-large` | C native | binary | read | large |
| `rb-cmp-write-large` | Ruby | compact | write | large |
| `rb-cmp-read-large` | Ruby | compact | read | large |
| `rb-json-write-large` | Ruby | JSON | write | large |
| `rb-json-read-large` | Ruby | JSON | read | large |
| `rb-bin-write-small` | Ruby | binary | write | small |
| `rb-bin-read-small` | Ruby | binary | read | small |
| `c-bin-write-small` | C native | binary | write | small |
| `c-bin-read-small` | C native | binary | read | small |
| `rb-cmp-write-small` | Ruby | compact | write | small |
| `rb-cmp-read-small` | Ruby | compact | read | small |
| `rb-json-write-small` | Ruby | JSON | write | small |
| `rb-json-read-small` | Ruby | JSON | read | small |
| `hdr-bin-write-small` | Header | binary | write | small |
| `hdr-bin-read-small` | Header | binary | read | small |
| `hdr-cmp-write-small` | Header | compact | write | small |
| `hdr-cmp-read-small` | Header | compact | read | small |
| `hdr-zlib-write-small` | Header | zlib | write | small |
| `hdr-zlib-read-small` | Header | zlib | read | small |

> [!NOTE]
> Native-only IDs fail if `thrift_native.so` is not available.

## Reference

### What It Measures

- Large jobs serialize and deserialize one nested `Nested4` payload by default.
- Small jobs serialize and deserialize many `OneOfEach` payloads.
- Read jobs use payloads built before timing so they measure read cost, not payload construction.
- Header jobs flush after each struct so reads benchmark framed messages, not a buffered write that was never emitted.

### Files

- `protocol_benchmark.rb`: benchmark harness and scenario definitions
- `../fixtures/structs.rb`: sample structs used by the benchmark

### Notes
This harness is for quick in-tree checks.
Use `--json` if you want structured output for scripts, result diffs, or branch comparisons.
Run it more than once if you want a wider sample.
