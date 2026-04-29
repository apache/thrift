# Thrift Ruby Software Library

## License

Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements. See the NOTICE file
distributed with this work for additional information
regarding copyright ownership. The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied. See the License for the
specific language governing permissions and limitations
under the License.

# Using Thrift with Ruby

Ruby bindings for the Apache Thrift RPC system. The gem contains the runtime
types, transports, protocols, and servers used by generated Ruby code for both
clients and services.

## Compatibility

- Ruby MRI >= 2.7 (tested against current supported releases).
- JRuby works with the pure-Ruby implementation; the native extension is
  skipped automatically.
- For the repo-wide transport, protocol, and server support matrix, see
  [Language Feature Matrix](https://github.com/apache/thrift/blob/master/LANGUAGES.md). This README focuses on Ruby-specific
  behavior and migration notes.

## Installation

- Requirements: Ruby >= 2.7.
- From RubyGems: `gem install thrift`
- From source: `bundle install`, `gem build thrift.gemspec`, then install the
  resulting `thrift-*.gem`. The native accelerator is built when the gem is
  installed on supported runtimes.

## Generating Ruby Code

The Ruby library does not include the Thrift compiler. Use a compiler built
from the root of this repository to generate Ruby bindings:

```bash
thrift --gen rb path/to/service.thrift
# with namespaced modules
thrift --gen rb:namespaced --recurse path/to/service.thrift
```

Generated files are typically written to `gen-rb/` and can be required
directly from your application.

## Basic Client Usage

```ruby
$:.push File.expand_path('gen-rb', __dir__)
require 'thrift'
require 'calculator'

socket     = Thrift::Socket.new('localhost', 9090)
transport  = Thrift::BufferedTransport.new(socket)
protocol   = Thrift::BinaryProtocol.new(transport)
client     = Calculator::Client.new(protocol)

transport.open
puts client.add(1, 1)
transport.close
```

## Basic Server Usage

```ruby
$:.push File.expand_path('gen-rb', __dir__)
require 'thrift'
require 'calculator'

class CalculatorHandler
  def add(a, b)
    a + b
  end
end

handler            = CalculatorHandler.new
processor          = Calculator::Processor.new(handler)
server_transport   = Thrift::ServerSocket.new(9090)
transport_factory  = Thrift::BufferedTransportFactory.new
protocol_factory   = Thrift::BinaryProtocolFactory.new

server = Thrift::ThreadedServer.new(processor, server_transport,
                                    transport_factory, protocol_factory)
server.serve
```

## Development and Tests

- `bundle exec rake spec` runs the Ruby specs. It expects a built Thrift
  compiler at `../../compiler/cpp/thrift`.
- `bundle exec rake test` runs the cross-language test suite; it must be
  executed from a full Thrift checkout.
- `bundle exec rake build_ext` (implicit in the tasks above) compiles the
  optional native extension that accelerates protocols and buffers.

## More Ruby Code

- Tutorial client and server: `tutorial/rb/RubyClient.rb` and `tutorial/rb/RubyServer.rb`
- Runtime benchmarks: `lib/rb/benchmark`
- Protocol benchmark: `test/rb/benchmarks/protocol_benchmark.rb`
- Library specs: `lib/rb/spec`
- Fuzzing harnesses and notes: `lib/rb/test/fuzz`
- Cross-language and integration tests: `test/rb`

## Breaking Changes

### 0.24.0

Connect timeout handling changed for both `Thrift::Socket` and
`Thrift::SSLSocket`.

- `timeout == nil` and `timeout == 0` now use blocking connect/open semantics.
  Older releases already treated `nil` that way, but treated `0` differently
  across operations: `read` and `write` used the blocking path, plain TCP
  `open` used a zero-length poll, and TLS `open` could spin in the handshake
  retry loop.
- Positive timeouts now bound the whole connect/open operation instead of
  effectively applying a fresh timeout window at each wait or address attempt.
  For plain TCP this budget is shared across address fallback. For TLS it is
  shared across the TCP connect and the SSL handshake.
- Connect/open timeout expiry now raises
  `Thrift::TransportException::TIMED_OUT`. Older releases could report the same
  condition as `NOT_OPEN`, and `Thrift::SSLSocket` could keep retrying the
  handshake after the wait timed out.

If your application matched `NOT_OPEN` for connect timeout handling, update it
to handle `TIMED_OUT`. If you relied on `timeout = 0` meaning immediate failure
or on repeated retries extending the effective timeout during TCP fallback or
TLS handshake, update those call paths before upgrading.

### 0.23.0

The documented source-build flow now effectively requires Ruby `2.7+`.
The committed development bundle no longer resolves on Ruby `2.6`
(`json-2.18.1 requires ruby version >= 2.7`), so building and testing this
library from source should be treated as `2.7+`.

Generated structs and unions now consistently raise
`Thrift::ProtocolException::INVALID_DATA` for invalid payloads such as unset
required fields, invalid enum values, or invalid union state. If your
application or tests matched older exception types or messages, update them.

Regenerated Ruby clients now validate replies more strictly. Mismatched reply
message types, method names, or sequence IDs raise
`Thrift::ApplicationException::INVALID_MESSAGE_TYPE`,
`Thrift::ApplicationException::WRONG_METHOD_NAME`, or
`Thrift::ApplicationException::BAD_SEQUENCE_ID`. If you relied on older,
looser reply handling in servers, proxies, or tests, regenerate and update
those call paths together.

Generated Ruby clients have never been safe to share across concurrent
threads. A client tracks pending sequence IDs on a single reply stream, so use
one client/transport pair per thread or serialize access yourself.

Treat `Thrift::ApplicationException::BAD_SEQUENCE_ID` as a correctness bug
that needs immediate attention. It means the client read a reply whose
sequence ID did not match the next pending request, so the connection may
already be out of sync and you may be reading a reply intended for a
different call. The most common cause is sharing one client across threads,
but a buggy proxy or server can also cause it.

### 0.13.0

Ruby development and CI moved to Ruby `2.4+`, but the runtime still claimed
support for older interpreters. Treat Ruby `< 2.4` on the `0.13.x` line as
best-effort, not guaranteed.

- Historical note for very old releases: the Ruby runtime was rearranged to use
  more Ruby-like names, and generated files switched to underscored filenames.
  If you are upgrading very old code, regenerate your Ruby bindings and update
  any old `T*` constants or legacy require paths such as
  `TBinaryProtocol` -> `Thrift::BinaryProtocol`.
- `rb:namespaced` changes the generated file layout. Flat output from
  `thrift --gen rb` and namespaced output from `thrift --gen rb:namespaced`
  use different require paths, so switch them atomically with regenerated code.

  ```ruby
  # --gen rb
  require 'calculator'

  # --gen rb:namespaced
  require 'my_namespace/calculator'
  ```

## Migration Notes

- If you upgrade to `0.24.0`, treat `timeout` on `Thrift::Socket` and
  `Thrift::SSLSocket` as one budget for the whole open path. For
  `Thrift::SSLSocket`, that includes both the TCP connect and the TLS
  handshake.
- If you upgrade to `0.24.0`, handle connect/open timeout expiry as
  `Thrift::TransportException::TIMED_OUT` instead of `NOT_OPEN`.
- If you upgrade across the stricter reply-validation changes, regenerate all
  Ruby stubs and deploy them with the matching Ruby runtime. Do not mix old
  generated code, new generated code, and new runtime code on the same client
  path without testing that combination.
- If you receive `Thrift::ApplicationException::BAD_SEQUENCE_ID`, treat the
  connection as out of sync. Close it, create a new client/transport pair, and
  investigate the root cause before retrying.
- Do not share one generated Ruby client across concurrent threads. Use one
  client/transport pair per thread, or serialize access to a shared client.
- If you switch between `thrift --gen rb` and `thrift --gen rb:namespaced`,
  regenerate all Ruby output and update `require` paths in the same change.

## Runtime Notes

- Loading the `thrift_native` extension changes which implementation you are
  running. It replaces
  `Thrift::Struct`, `Thrift::Union`, and `Thrift::CompactProtocol` methods
  with C implementations in place. `Thrift::BinaryProtocol` remains available
  in pure Ruby, and the C-backed binary protocol is opt-in through
  `Thrift::BinaryProtocolAcceleratedFactory` or
  `Thrift::BinaryProtocolAccelerated` when that class is available.
- The native extension is optional. If it cannot be built or loaded, Thrift
  falls back to the pure-Ruby implementation. This mainly changes performance
  and implementation details.
- JRuby skips the native extension automatically and uses the pure-Ruby path.
- Do not share one client instance across concurrent threads. A client tracks
  request and reply state on a single transport stream.
- `Thrift::NonblockingServer` expects framed input. Use
  `Thrift::FramedTransport` with it on the wire.
- Client and server must agree on transport and protocol choices. If you
  switch to SSL, HTTP, header transport, compact protocol, or namespaced
  generated code, update both ends together.
- HTTPS client transport verifies peers by default, and `Thrift::SSLSocket`
  performs a hostname check against the host you pass in.
