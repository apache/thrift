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

use super::{TFieldIdentifier, TListIdentifier, TMapIdentifier, TMessageIdentifier, TMessageType, TProtocol, TSetIdentifier, TStructIdentifier};

/// A `TProtocol` that can send Thrift messages
/// over a single endpoint shared with other Thrift services.
/// This construct can only be used when paired with a
/// corresponding `TMultiplexedProcessor` at the receiver.
pub struct TMultiplexedProtocol<P: TProtocol> {
    service_name: String,
    wrapped: P,
}

impl <P: TProtocol> TMultiplexedProtocol<P> {
    /// Create a new `TMultiplexedProtocol` that:
    ///
    /// 1. Wraps an `wrapped` `TProtocol` (to which it delegates message
    /// serialization and deserialization)
    /// 2. Identifies outgoing service calls as originating from the
    /// `service_name` Thrift service
    pub fn new(service_name: &str, wrapped: P) -> TMultiplexedProtocol<P> {
        TMultiplexedProtocol { service_name: service_name.to_owned(), wrapped: wrapped }
    }
}

// FIXME: avoid passthrough methods
impl <P: TProtocol> TProtocol for TMultiplexedProtocol<P> {
    fn write_message_begin(&mut self, identifier: &TMessageIdentifier) -> ::Result<()> {
        match identifier.message_type { // FIXME: is there a better way to override identifier here?
            TMessageType::Call | TMessageType::OneWay => {
                let identifier = TMessageIdentifier { name: format!("{}:{}", self.service_name, identifier.name), .. *identifier };
                self.wrapped.write_message_begin(&identifier)
            },
            _ => {
                self.wrapped.write_message_begin(&identifier)
            },
        }
    }

    fn write_message_end(&mut self) -> ::Result<()> {
        self.wrapped.write_message_end()
    }

    fn write_struct_begin(&mut self, identifier: &TStructIdentifier) -> ::Result<()> {
        self.wrapped.write_struct_begin(identifier)
    }

    fn write_struct_end(&mut self) -> ::Result<()> {
        self.wrapped.write_struct_end()
    }

    fn write_field_begin(&mut self, identifier: &TFieldIdentifier) -> ::Result<()> {
       self.wrapped.write_field_begin(identifier)
    }

    fn write_field_end(&mut self) -> ::Result<()> {
        self.wrapped.write_field_end()
    }

    fn write_field_stop(&mut self) -> ::Result<()> {
        self.wrapped.write_field_stop()
    }

    fn write_bytes(&mut self, b: &[u8]) -> ::Result<()> {
        self.wrapped.write_bytes(b)
    }

    fn write_bool(&mut self, b: bool) -> ::Result<()> {
        self.wrapped.write_bool(b)
    }

    fn write_i8(&mut self, i: i8) -> ::Result<()> {
        self.wrapped.write_i8(i)
    }

    fn write_i16(&mut self, i: i16) -> ::Result<()> {
        self.wrapped.write_i16(i)
    }

    fn write_i32(&mut self, i: i32) -> ::Result<()> {
        self.wrapped.write_i32(i)
    }

    fn write_i64(&mut self, i: i64) -> ::Result<()> {
        self.wrapped.write_i64(i)
    }

    fn write_double(&mut self, d: f64) -> ::Result<()> {
        self.wrapped.write_double(d)
    }

    fn write_string(&mut self, s: &str) -> ::Result<()> {
        self.wrapped.write_string(s)
    }

    fn write_list_begin(&mut self, identifier: &TListIdentifier) -> ::Result<()> {
        self.wrapped.write_list_begin(identifier)
    }

    fn write_list_end(&mut self) -> ::Result<()> {
        self.wrapped.write_list_end()
    }

    fn write_set_begin(&mut self, identifier: &TSetIdentifier) -> ::Result<()> {
        self.wrapped.write_set_begin(identifier)
    }

    fn write_set_end(&mut self) -> ::Result<()> {
        self.wrapped.write_set_end()
    }

    fn write_map_begin(&mut self, identifier: &TMapIdentifier) -> ::Result<()> {
        self.wrapped.write_map_begin(identifier)
    }

    fn write_map_end(&mut self) -> ::Result<()> {
        self.wrapped.write_map_end()
    }

    fn flush(&mut self) -> ::Result<()> {
        self.wrapped.flush()
    }

    fn read_message_begin(&mut self) -> ::Result<TMessageIdentifier> {
       self.wrapped.read_message_begin()
    }

    fn read_message_end(&mut self) -> ::Result<()> {
        self.wrapped.read_message_end()
    }

    fn read_struct_begin(&mut self) -> ::Result<Option<TStructIdentifier>> {
        self.wrapped.read_struct_begin()
    }

    fn read_struct_end(&mut self) -> ::Result<()> {
        self.wrapped.read_struct_end()
    }

    fn read_field_begin(&mut self) -> ::Result<TFieldIdentifier> {
        self.wrapped.read_field_begin()
    }

    fn read_field_end(&mut self) -> ::Result<()> {
        self.wrapped.read_field_end()
    }

    fn read_bytes(&mut self) -> ::Result<Vec<u8>> {
        self.wrapped.read_bytes()
    }

    fn read_bool(&mut self) -> ::Result<bool> {
        self.wrapped.read_bool()
    }

    fn read_i8(&mut self) -> ::Result<i8> {
        self.wrapped.read_i8()
    }

    fn read_i16(&mut self) -> ::Result<i16> {
        self.wrapped.read_i16()
    }

    fn read_i32(&mut self) -> ::Result<i32> {
        self.wrapped.read_i32()
    }

    fn read_i64(&mut self) -> ::Result<i64> {
        self.wrapped.read_i64()
    }

    fn read_double(&mut self) -> ::Result<f64> {
        self.wrapped.read_double()
    }

    fn read_string(&mut self) -> ::Result<String> {
        self.wrapped.read_string()
    }

    fn read_list_begin(&mut self) -> ::Result<TListIdentifier> {
        self.wrapped.read_list_begin()
    }

    fn read_list_end(&mut self) -> ::Result<()> {
        self.wrapped.read_list_end()
    }

    fn read_set_begin(&mut self) -> ::Result<TSetIdentifier> {
        self.wrapped.read_set_begin()
    }

    fn read_set_end(&mut self) -> ::Result<()> {
        self.wrapped.read_set_end()
    }

    fn read_map_begin(&mut self) -> ::Result<TMapIdentifier> {
       self.wrapped.read_map_begin()
    }

    fn read_map_end(&mut self) -> ::Result<()> {
        self.wrapped.read_map_end()
    }

    //
    // utility
    //

    fn write_byte(&mut self, b: u8) -> ::Result<()> { // FIXME: remove
        self.wrapped.write_byte(b)
    }

    fn read_byte(&mut self) -> ::Result<u8> { // FIXME: remove
        self.wrapped.read_byte()
    }
}
