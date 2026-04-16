# Apache Thrift netstd

Thrift client library for Microsoft .NET Standard 

# Build the library

## How to build on Windows
- Get Thrift IDL compiler executable, add to some folder and add path to this folder into PATH variable. 
- Alternatively, build from source by using the cmake target "copy-thrift-compiler", which places the binary to a suitable place.
- Open the Thrift.sln project with Visual Studio and build.
or 
- Build with scripts

## How to build on Unix/Linux
- Ensure you have a suitable .NET Core SDK installed, or use the [Ubuntu docker image](../../build/docker/README.md)
- Follow common automake build practice: `./ bootstrap && ./ configure && make`

## Known issues
- In trace logging mode you can see some not important internal exceptions

# Migration to netstd

## ... from netcore

If you are migrating your code from netcore library, you will have to:

- Switch to `thrift -gen netstd`
- the following compiler flags are no longer needed or supported: `hashcode` is now standard, while `nullable` is no longer supported.
- the `Thrift.Transport` and `Thrift.Protocol` namespaces now use the singular form
- add `using Thrift.Processor;` in the server code where appropriate
- rename all `T*ClientTransport` to `T*Transport` 
- rename all `TBaseServer` occurrences in your code to `TServer`
- the `SingletonTProcessorFactory` is now called `TSingletonProcessorFactory`
- and the `AsyncBaseServer` is now the `TSimpleAsyncServer`

You may wonder why we changed so many names. The naming scheme has been revised for two reasons: First, we want to get back the established, well-known naming consistency across the Thrift libraries which the netcore library did not fully respect. Second, by achieving that first objective, we get the additional benefit of making migration at least a bit easier for C# projects.

## ... from csharp

Because of the different environment requirements, migration from C# takes slightly more efforts. While the code changes related to Thrift itself are moderate, you may need to upgrade certain dependencies, components or even modules to more recent versions. 

1. Client and server applications must use at least framework 4.6.1, any version below will not work. 
1. Switch to `thrift -gen netstd`. The following compiler flags are no longer needed or supported: `hashcode` and `async` are now standard, while `nullable` is no longer supported.
1. [Familiarize yourself with the `async/await` model](https://msdn.microsoft.com/en-us/magazine/jj991977.aspx), if you have not already done so. As netstd does not support `ISync` anymore, async is mandatory. The synchronous model is simply no longer available (that's also the reason why we don't need the `async` flag anymore). 
1. Consider proper use of `cancellationToken` parameters. They are optional but may be quite helpful.
1. As you probably already guessed, there are a few names that have been changed:
- add `using Thrift.Processor;` in the server code where appropriate
- the `TServerSocket` is now called `TServerSocketTransport`
- change `IProtocolFactory` into `ITProtocolFactory`
- if you are looking for `TSimpleServer`, try `TSimpleAsyncServer` instead
- similarly, the `TThreadPoolServer` is now a `TThreadPoolAsyncServer` 
- the server's `Serve()` method does now `ServeAsync()`
- In case you are using Thrift server event handlers: the `SetEventHandler` method now starts with an uppercase letter
- and you will also have to revise the method names of all `TServerEventHandler` descendants you have in your code

# Fuzzing

We use [SharpFuzz](https://github.com/Metalnem/sharpfuzz) (and its libfuzzer variant) to fuzz the Thrift protocol parsers. This is **not** integrated with oss-fuzz, so all fuzzing must be run locally. **Supported platform: Linux only.** The fuzzers are opt-in and are **not** built by `make check`; run `make build-fuzzers` (or `./buildfuzzers.sh`) explicitly.

## Prerequisites

1. A .NET 10 SDK (same one used for the rest of `lib/netstd`).

2. The SharpFuzz IL-rewriter CLI, installed as a .NET global tool:

   ```bash
   dotnet tool install --global SharpFuzz.CommandLine
   export PATH="$PATH:$HOME/.dotnet/tools"
   ```

   Add the `PATH` export to your shell rc if you want it to persist.

3. The native `libfuzzer-dotnet` driver binary. Grab a prebuilt release from the
   [Metalnem/libfuzzer-dotnet releases page](https://github.com/Metalnem/libfuzzer-dotnet/releases)
   (or build it from source). Place it in a directory of your choice and point
   `SHARPFUZZ_DIR` at that directory:

   ```bash
   export SHARPFUZZ_DIR=/path/to/libfuzzer-dotnet-dir
   ```

   `buildfuzzers.sh` and `runfuzzer.sh` expect to find `$SHARPFUZZ_DIR/libfuzzer-dotnet`.

## A temporary note on `DOTNET_ROLL_FORWARD`

As of SharpFuzz.CommandLine 2.2.0, the global tool's `runtimeconfig.json` pins the
tool to .NET 9, which prevents it from running under a .NET 10-only host. Both
`buildfuzzers.sh` and `runfuzzer.sh` therefore export `DOTNET_ROLL_FORWARD=Major`
at the top of the script as a workaround. Upstream fix:
[SharpFuzz PR #72](https://github.com/Metalnem/sharpfuzz/pull/72) (merged, pending
release as SharpFuzz 2.3.0). Once that release ships, remove the `DOTNET_ROLL_FORWARD`
exports from both shell drivers and update the SharpFuzz package pin in
`Tests/Thrift.FuzzTests/Thrift.FuzzTests.csproj`.

## Running the fuzzers

Build all twelve fuzzer assemblies (3 protocols x 2 fuzzer types x 2 engines) and
instrument them with SharpFuzz:

```bash
./buildfuzzers.sh
```

Run one fuzzer:

```bash
./runfuzzer.sh <fuzzer-name> <engine> [extra-fuzzer-args...]
```

Where `<fuzzer-name>` is one of `binary`, `compact`, `json`, `binary-roundtrip`,
`compact-roundtrip`, `json-roundtrip`, and `<engine>` is `libfuzzer` or `afl`.
Any additional arguments are passed through to the underlying fuzzer engine, e.g.:

```bash
./runfuzzer.sh binary libfuzzer -runs=10000
```