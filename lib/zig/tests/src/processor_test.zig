// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

const std = @import("std");
const thrift = @import("thrift");
const thrift_test = @import("thrift_test");

const t = std.testing;
const TMessageType = thrift.protocol.TMessageType;

const StubHandler = struct {
    pub fn secondtestString(_: *StubHandler, thing: thrift.String) !thrift_test.SecondServiceSecondtestStringResult {
        return .success(thing);
    }
};

test "processor unknown method sends exception instead of failing process" {
    var transport: thrift.transport.TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var config = thrift.TConfiguration.default;
    var write_prot: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer write_prot.deinit();

    try write_prot.writeMessageBegin(.{
        .allocator = undefined,
        .name = "noSuchMethod",
        .msgType = .Call,
        .sequenceNumber = 42,
    });
    try write_prot.writeStructBegin(.{
        .allocator = undefined,
        .name = "",
    });
    try write_prot.writeFieldStop();
    try write_prot.writeStructEnd();
    try write_prot.writeMessageEnd();
    try write_prot.flush();

    const request_len = transport.buffer.written().len;

    var stub: StubHandler = .{};
    var handler = thrift_test.SecondServiceHandler.init(&stub);
    var processor = thrift_test.SecondServiceProcessor{
        .allocator = t.allocator,
        .handler = &handler,
    };

    var read_prot_inst: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer read_prot_inst.deinit();
    var read_prot = read_prot_inst.interface();
    var write_prot_inst: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer write_prot_inst.deinit();
    var out_prot = write_prot_inst.interface();

    try processor.process(&read_prot, &out_prot);

    try t.expectEqual(request_len, read_prot_inst.reader.seek);

    try t.expect(transport.buffer.written().len > request_len);

    var response_prot_inst: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer response_prot_inst.deinit();
    const response_reader = try transport.reader();
    response_reader.seek = request_len;
    var response_prot = response_prot_inst.interface();

    var response_ident = try response_prot.readMessageBegin(t.allocator);
    defer response_ident.deinit();
    try t.expect(response_ident.msgType == TMessageType.Exception);
    try t.expectEqual(@as(i32, 42), response_ident.sequenceNumber);

    var exception = try thrift.TApplicationException.readFromProtocol(t.allocator, &response_prot);
    defer exception.deinit();
    try t.expectEqual(@as(i32, @intFromEnum(thrift.application_exception.TApplicationExcpetionType.UnknownMethod)), exception.type);
    try response_prot.readMessageEnd();
}

test "processor malformed args fails with InboundReadFailed" {
    var transport: thrift.transport.TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var config = thrift.TConfiguration.default;
    var write_prot: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer write_prot.deinit();

    try write_prot.writeMessageBegin(.{
        .allocator = undefined,
        .name = "secondtestString",
        .msgType = .Call,
        .sequenceNumber = 3,
    });
    try write_prot.writeStructBegin(.{
        .allocator = undefined,
        .name = "",
    });
    try write_prot.writeFieldBegin(.{
        .allocator = undefined,
        .name = "thing",
        .fieldType = .String,
        .id = 1,
    });
    // Length 10 but only one byte of payload → read fails mid-args.
    try write_prot.writeI32(10);
    try write_prot.writeByte(0x41);
    try write_prot.writeFieldEnd();
    try write_prot.writeFieldStop();
    try write_prot.writeStructEnd();
    try write_prot.writeMessageEnd();
    try write_prot.flush();

    const bytes_before_process = transport.buffer.written().len;

    var stub: StubHandler = .{};
    var handler = thrift_test.SecondServiceHandler.init(&stub);
    var processor = thrift_test.SecondServiceProcessor{
        .allocator = t.allocator,
        .handler = &handler,
    };

    var read_prot_inst: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer read_prot_inst.deinit();
    var read_prot = read_prot_inst.interface();
    var write_prot_inst: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer write_prot_inst.deinit();
    var out_prot = write_prot_inst.interface();

    try t.expectError(thrift.server.InboundReadFailed.InboundReadFailed, processor.process(&read_prot, &out_prot));
    try t.expect(transport.buffer.written().len > bytes_before_process);

    var response_prot_inst: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer response_prot_inst.deinit();
    const response_reader = try transport.reader();
    response_reader.seek = bytes_before_process;
    var response_prot = response_prot_inst.interface();

    var response_ident = try response_prot.readMessageBegin(t.allocator);
    defer response_ident.deinit();
    try t.expect(response_ident.msgType == TMessageType.Exception);
    try t.expectEqual(@as(i32, 3), response_ident.sequenceNumber);

    var exception = try thrift.TApplicationException.readFromProtocol(t.allocator, &response_prot);
    defer exception.deinit();
    try t.expectEqual(@as(i32, @intFromEnum(thrift.application_exception.TApplicationExcpetionType.ProtocolError)), exception.type);
    try t.expectEqualStrings("Error reading message", exception.message.contents);
    try response_prot.readMessageEnd();
}

test "processor unknown method consumes args payload" {
    var transport: thrift.transport.TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var config = thrift.TConfiguration.default;
    var write_prot: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer write_prot.deinit();

    try write_prot.writeMessageBegin(.{
        .allocator = undefined,
        .name = "noSuchMethod",
        .msgType = .Call,
        .sequenceNumber = 1,
    });
    try write_prot.writeStructBegin(.{
        .allocator = undefined,
        .name = "",
    });
    try write_prot.writeFieldBegin(.{
        .allocator = undefined,
        .name = "thing",
        .fieldType = .String,
        .id = 1,
    });
    var leftover = try thrift.String.initFromSlice(t.allocator, "leftover");
    defer leftover.deinit();
    try write_prot.writeString(leftover);
    try write_prot.writeFieldEnd();
    try write_prot.writeFieldStop();
    try write_prot.writeStructEnd();
    try write_prot.writeMessageEnd();
    try write_prot.flush();

    const request_len = transport.buffer.written().len;

    var stub: StubHandler = .{};
    var handler = thrift_test.SecondServiceHandler.init(&stub);
    var processor = thrift_test.SecondServiceProcessor{
        .allocator = t.allocator,
        .handler = &handler,
    };

    var read_prot_inst: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer read_prot_inst.deinit();
    var read_prot = read_prot_inst.interface();
    var write_prot_inst: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer write_prot_inst.deinit();
    var out_prot = write_prot_inst.interface();

    try processor.process(&read_prot, &out_prot);

    try t.expectEqual(request_len, read_prot_inst.reader.seek);
}
