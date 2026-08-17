# Thrift Zig Software Library

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


## Overview

> NOTE: This library is still quite new and experimental. There might be brekaing changes or just instability in general.

The thrift runtime library for the Zig language.

## Requirements

* [Zig](https://ziglang.org/) **0.16.0**.


## Using Thrift with Zig

### Generate code

Run the Thrift compiler with the Zig backend:

```
$ thrift -out gen-zig --gen zig my_service.thrift
$ ls gen-zig
my_service.zig
```

### Add the runtime library to your project

Create a module pointing to this library's root file:
```zig
const thrift = b.addModule("thrift", .{
    .root_source_file = b.path("<path to this directory>/src/root.zig"),
    .target = target,
    .optimize = optimize,
});
```

Create the module from the generated code and add the thrift library as a dependency:
```zig
const generated = b.addModule("my_service", .{
    .root_source_file = b.path("gen-zig/my_service.zig"),
});
generated.addImport("thrift", thrift);
```

## Using the library

Checkout `tutorial/zig/` and `test/zig/` (from the root of the thrift repository) for end-to-end usage examples.

### Handling missing fields from the wire
This implementation takes inspiration from the C++ implementation when dealing with
client-server version mismatch, implementing a similar `__isset` struct that
tracks which fields arrived through the wire.

**Non-optional struct fields that are missing on the wire will have their
`__isset` field set to `false`.** Nonetheless, when a field is missing from
the wire, the library loads a default value to avoid garbage data in the
struct.

When deserializing, the struct is first initialized via `initDefault(allocator)`,
which assigns these defaults to every field while leaving all `__isset` flags
at `false`. Fields present on the wire overwrite their slot and set the
corresponding `__isset` flag to `true`. After the wire payload is consumed,
any field declared with an IDL default value (for example `1: i32 count = 42`)
that was not sent on the wire is filled in from that default and its
`__isset` flag is set to `true`. 

Fields that are set as `required` but are still unset after the last step cause
`ProtocolError.MissingField`.

The defaults per Thrift type are:

| Thrift type | Default value |
|-------------|---------------|
| `bool` | `false` |
| `byte` / `i8` | `0` |
| `i16` | `0` |
| `i32` | `0` |
| `i64` | `0` |
| `double` | `0.0` |
| `string` | empty string (`String.initDefault`) |
| `binary` | empty bytes (`BinaryBytes.initDefault`) |
| `uuid` | `.empty` |
| `enum` | the first declared constant, or `@enumFromInt(0)` if the enum has no constants |
| `struct` / `exception` | nested `initDefault(allocator)` |
| `list` / `set` / `map` | empty container (`.init(allocator)`) |
| `optional` field | `null` |
| `typedef` | follows the underlying type |

Fields with an IDL default use that value instead of the type default listed
above when they are absent from the wire.

The flag is useful when a server is running a newer version of the IDL that
defines a new field in a struct, but clients are still running the older
version:
```zig
if (!s.__isset.message_id) {
    return error.MissingMessageId;
}
```

### Working with structs

Generated structs that contain heap-allocated fields (strings, binary, nested
structs, containers, and so on) store an `.allocator` field. Construct them
with `init`, which fills in type defaults for any omitted optional fields and
marks every supplied field in `__isset`:

```zig
// shared.thrift
struct SharedStruct {
  1: i32 key
  2: string value
}

var shared = try tutorial.shared.SharedStruct.init(allocator, .{
    .key = logId,
    .value = try .initFromSlice(allocator, "hello"),
});
defer shared.deinit();
```

You can also use a struct literal and set `.allocator` yourself, but `init` is
the usual entry point. Primitive-only structs have no allocator field and can
be created with a plain literal.

When you are the owner of a value, e.g. the result of a client RPC call, call
`deinit()` when you are done:

```zig
var getStructResult = try client.getStruct(2);
defer getStructResult.deinit();

const ss = try getStructResult.get();
std.debug.print("Received log: {d} => {f}\n", .{ ss.key, ss.value });
```

### Writing service handler methods

On the server, the generated processor deserializes incoming arguments, invokes
your handler, serializes the response, and then frees everything involved in
that call. You do not need to `deinit()` the request arguments or the result
you return, the processor does that after the response is written.

Each RPC method has a generated result type (for example
`CalculatorAddResult`) backed by `thrift.ServiceCallResult`. Return a normal
response with `.success(...)`:

```zig
pub fn add(self: *@This(), num1: i32, num2: i32) !tutorial.CalculatorAddResult {
    _ = self;
    return .success(num1 + num2);
}

pub fn getStruct(self: *@This(), logId: i32) !tutorial.shared.SharedServiceGetStructResult {
    return .success(try tutorial.shared.SharedStruct.init(self.allocator, .{
        .key = logId,
        .value = try .initFromSlice(
            self.allocator,
            self.sharedMap.get(logId) orelse &[0]u8{},
        ),
    }));
}
```

Construct heap-allocated return values with the handler's allocator so they
remain valid until serialization completes.

Wire your handler into the processor via the generated handler interface:

```zig
var serverHandler = CalculatorHandler.init(gpa, io);
defer serverHandler.deinit();
var handlerInterface = serverHandler.interface();
```

#### Declared exceptions

IDL `throws` clauses become a generated error union on the result type. For
example, given:

```thrift
exception InvalidOperation {
  1: i32 whatOp,
  2: string why
}

service Calculator {
  i32 calculate(1: i32 logid, 2: Work w) throws (1: InvalidOperation ouch),
}
```

the handler returns `CalculatorCalculateResult`, which is a union of a success
value and a `CalculatorCalculateResultError` union. Signal a declared exception
with `.fail(...)`, using the field name from the `throws` clause (`ouch` here):

```zig
pub fn calculate(self: *@This(), logid: i32, w: tutorial.Work) !tutorial.CalculatorCalculateResult {
    if (w.op == .DIVIDE and w.num2 == 0) {
        return .fail(.{
            .ouch = try tutorial.InvalidOperation.init(self.allocator, .{
                .whatOp = @intFromEnum(w.op),
                .why = try .initFromSlice(self.allocator, "Division by 0"),
            }),
        });
    }

    // ...
    return .success(res);
}
```

Build exception structs with the handler's allocator the same way as success
values; the processor frees them after the reply is sent. Thrift exceptions are
returned as `.fail(...)` values, not as Zig errors. If the handler returns a
Zig error via `!` instead (for example `error.OutOfMemory`), the server sends
that back as a `TApplicationException`.

On the client, RPC methods return a `*ClientResult` type (for example
`CalculatorCalculateClientResult`). Its error union contains every declared
exception plus `thrift.TApplicationException`, which covers any undeclared
remote failure, including Zig errors that escaped a handler through `!`.
Inspect `message` and `type_` on the exception for details.

```zig
var calcResult = try client.calculate(1, work);
defer calcResult.deinit();

switch (calcResult) {
    .result => |value| std.debug.print("result = {d}\n", .{value}),
    .err => |e| switch (e) {
        .ouch => |ex| std.debug.print("InvalidOperation: {f}\n", .{ex.why}),
        .TApplicationException => |appEx| {
            std.debug.print("remote error: {f}\n", .{appEx.message});
        },
    },
}
```

Methods with no `throws` clause still get a client error union containing only
`TApplicationException`.

### Collections
The library uses custom implementations of collections. Currently, they work as
wrapers around std lib containers. 

They're only intended to be used with thrift types and thrift generated structs
as they call into utility/generated code in the background, and implementations
of those are prone to change.

Generated code maps Thrift container types to `thrift.List(T)`,
`thrift.Set(T)`, and `thrift.Map(K, V)`. These are meant to be used as
immutable containers.

```zig
const thrift = @import("thrift");

// list<i32>
var ids = thrift.List(i32).init(allocator);
defer ids.deinit();
try ids.append(1);
try ids.append(2);
try ids.append(3);

var id_iter = ids.iterator();
while (id_iter.next()) |id| {
    std.debug.print("{d}\n", .{id.*});
}

// set<string>
var tags = thrift.Set(thrift.String).init(allocator);
defer tags.deinit();
try tags.put(try thrift.String.initFromSlice(allocator, "zig"));
try tags.put(try thrift.String.initFromSlice(allocator, "thrift"));

// map<i32, string>
var table = thrift.Map(i32, thrift.String).init(allocator);
defer table.deinit();
try table.put(1, try thrift.String.initFromSlice(allocator, "one"));
try table.put(2, try thrift.String.initFromSlice(allocator, "two"));
if (table.get(1)) |value| {
    std.debug.print("1 => {f}\n", .{value});
}

{
    // when passing collections into a generated struct, the ownership transfers to the struct
    var map = thrift.Map(i32, i32).init(allocator);
    try map.put(42, 100);

    const s = Record.init(allocator, .{
        .map = map,
    });
    defer s.deinit();
}
```

Check `src/collections/` for their source code.

### Strings, Binary and UUIDs
These types are implemented in `src/lib/types.zig`. Technically it's overkill
to wrap these in structs, but makes managing helper functions easier, and
simplifies the code-gen. Also, allows for changing the implementation without
touching too many things, e.g. more UTF support for strings.

`String` and `BinaryBytes` own their payload when created with
`initFromSlice`; use `initFromBorrowed` to wrap an existing slice without
copying (the wrapper must not outlive the slice). Both expose their data via
`.contents` and must be freed with `deinit()` when owned. `UUID` is a plain
value type and does not require an allocator.

```zig
const thrift = @import("thrift");

// String — copy a Zig string into an owned Thrift value
var name = try thrift.String.initFromSlice(allocator, "hello");
defer name.deinit();
try std.testing.expectEqualStrings("hello", name.contents);

// String — zero-copy borrow (valid only while the source slice is alive)
const borrowed = thrift.String.initFromBorrowed("temporary");
try std.testing.expectEqualStrings("temporary", borrowed.contents);

// Binary — same API as String, but for `binary` fields
var payload = try thrift.BinaryBytes.initFromSlice(allocator, &[_]u8{ 0xde, 0xad, 0xbe, 0xef });
defer payload.deinit();

// UUID — parse from the canonical hyphenated form
const id = try thrift.UUID.parse("74408f86-8b27-48b6-be24-e5cb804a7f95");
std.debug.print("id = {f}\n", .{id}); // prints 74408f86-8b27-48b6-be24-e5cb804a7f95

// Using these types in generated structs
const entry = try my_service.Entry.init(allocator, .{
    .name = try .initFromSlice(allocator, "widget"),
    .payload = try .initFromSlice(allocator, &[_]u8{ 0x01, 0x02 }),
    .id = try .parse("74408f86-8b27-48b6-be24-e5cb804a7f95"),
});
defer entry.deinit();
```

### Transports are not unbuffered
Transports are buffered by default as a consequence of the buffer requirement
from Zig's Readers and Writers. Sockets (the only implemented trasnport)
currently create 1KB (hardcoded setting) read and write buffers, that'll change
eventually with customizable settings in a future implementation.

Supported features
------------------

| Component | Supported |
|-----------|-----------|
| Transports | TCP socket (`TSocket`, `TServerSocket`), framed (`TFramedTransport`), buffered (direct socket I/O) |
| Protocols | Binary, compact, multiplex |
| Server | `TSimpleServer` |
| Code generator | structs, unions, exceptions, services (sync client and processor) |

Not yet implemented (may be added in follow-up contributions):

* JSON and header protocols
* HTTP transport
* SSL/TLS, zlib, domain sockets, and Windows pipes in the cross-test CLI
* Thread-pool, threaded, and nonblocking server types

Testing
-------

There are three layers of tests:

1. **Unit tests** embedded in the library sources and run via `zig build test`
   in `lib/zig/`.
2. **Generated-code integration tests** in `lib/zig/tests/`, which exercise
   the compiler output against the runtime library.
3. **Cross-language integration tests** in `test/zig/`, which implement the
   standard ThriftTest server and client used by the project-wide cross test
   harness.

To run the full cross test suite from the repository root:

    make cross

Check the README in `test/` for more information on those.

The tutorial under `tutorial/zig/` provides a smaller end-to-end example. From
that directory, after building the compiler:

    $ make
    $ zig-out/bin/tutorial_server &
    $ zig-out/bin/tutorial_client



