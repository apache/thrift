# Go fuzzing README

There are two kinds of fuzz target in the Go tree, and they are built and run differently.

## Native targets (`go test -fuzz`)

These are ordinary `testing.F` targets. They need no external tooling, and they have a property
worth knowing about: **run without `-fuzz` they replay their seed corpus as normal test cases**,
so they act as regression tests in CI at no cost. `make check` runs them.

They live in two places:

* `lib/go/thrift/fuzz_test.go` — targets that need no generated code, driving the protocol and
  transport read paths directly. These run with the library's own tests (`make -C lib/go check`).
  * `FuzzReadBinary`, `FuzzReadBinaryNonStrict`, `FuzzReadBinaryStruct`
  * `FuzzReadCompact`, `FuzzReadCompactStruct`
  * `FuzzReadJSON`, `FuzzReadJSONStruct`, `FuzzReadSimpleJSON`
  * `FuzzReadHeader` — client type detection, transform list, info headers
  * `FuzzReadFramed` — the frame length taken off the wire
  * `FuzzServerDispatch` — a server side hop through `TMultiplexedProcessor`
  * `FuzzParseTuuid`
* `fuzz_native_test.go` in this directory — reading and round-tripping a generated struct.
  * `FuzzStructReadBinary`, `FuzzStructReadCompact`, `FuzzStructReadJSON`
  * `FuzzStructRoundtripBinary`, `FuzzStructRoundtripCompact`, `FuzzStructRoundtripJSON`

To fuzz one for real:

```bash
# in lib/go
go test ./thrift -run '^$' -fuzz FuzzReadHeader -fuzztime 2m

# in lib/go/test/fuzz (run "make check" once first, so gen-go exists)
go test -run '^$' -fuzz FuzzStructRoundtripCompact -fuzztime 2m
```

When a run fails, Go writes the input to `testdata/fuzz/<Target>/` next to the target. **Commit
that file with the fix** — from then on every `go test` replays it.

## go-fuzz targets (OSS-Fuzz)

`fuzz.go` holds the older [go-fuzz](https://github.com/dvyukov/go-fuzz) style targets, behind the
`gofuzz` build tag. These are what the OSS-Fuzz build consumes; `make check` only compiles and
smoke-tests them here.

* `FuzzTutorial` — spins up a mini server and feeds it random data, following the tutorial example
* `FuzzParseBinary`, `FuzzParseCompact`, `FuzzParseJson` — deserialization per protocol
* `FuzzRoundtripBinary`, `FuzzRoundtripCompact`, `FuzzRoundtripJson` — round trip per protocol

To reproduce a bug in one of these, update `fuzz_test.go` to pass the input buffer to the relevant
function.

## Building

Run `make check` in this directory. It generates the code the targets need, then runs both sets.

See `FUZZING.md` at the top of the repository for the wider fuzzing setup.
