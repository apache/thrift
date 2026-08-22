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

const protocol = @import("protocol.zig");
pub const TType = protocol.TType;
pub const TMessageType = protocol.TMessageType;
pub const TMessageIdentifier = protocol.TMessageIdentifier;
pub const TFieldIdentifier = protocol.TFieldIdentifier;
pub const TMapIdentifier = protocol.TMapIdentifier;
pub const TListIdentifier = protocol.TListIdentifier;
pub const TSetIdentifier = protocol.TSetIdentifier;
pub const TStructIdentifier = protocol.TStructIdentifier;
pub const TProtocol = protocol.TProtocol;
pub const TProtocolFactory = protocol.TProtocolFactory;
pub const ProtocolError = protocol.ProtocolError;

const binary_protocol = @import("binary_protocol.zig");
pub const TBinaryProtocol = binary_protocol.TBinaryProtocol;
pub const TBinaryProtocolFactory = binary_protocol.TBinaryProtocolFactory;

const compact_protocol = @import("compact_protocol.zig");
pub const TCompactProtocol = compact_protocol.TCompactProtocol;
pub const TCompactProtocolFactory = compact_protocol.TCompactProtocolFactory;

const logging_protocol = @import("logging_protocol.zig");
pub const TLoggingProtocol = logging_protocol.TLoggingProtocol;
pub const TLoggingProtocolFactory = logging_protocol.TLoggingProtocolFactory;

const stored_protocol = @import("stored_protocol.zig");
pub const TStoredProtocol = stored_protocol.TStoredProtocol;

const multiplexed_protocol = @import("multiplexed_protocol.zig");
pub const TMultiplexedProtocol = multiplexed_protocol.TMultiplexedProtocol;

test {
    _ = protocol;
    _ = @import("utils.zig");
    _ = compact_protocol;
    _ = multiplexed_protocol;
}
