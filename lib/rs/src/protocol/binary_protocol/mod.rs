/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

use protocol;
use protocol::{ MessageType, Protocol, Type };
use transport::Transport;
use ThriftErr;
use TResult;
use std::num::FromPrimitive;

static BINARY_PROTOCOL_VERSION_1: u16 = 0x8001;

#[derive(Copy)]
pub struct BinaryProtocol;

impl BinaryProtocol {
    fn write_type(&self, transport: &mut Transport, type_: Type) {
        self.write_byte(transport, type_ as i8);
    }

    fn read_type(&self, transport: &mut Transport) -> TResult<Type> {
        let raw = try!(self.read_byte(transport));
        match FromPrimitive::from_i8(raw) {
            Some(type_) => Ok(type_),
            None => Err(ThriftErr::InvalidData),
        }
    }
}

impl Protocol for BinaryProtocol {
    fn write_message_begin(
        &self, transport: &mut Transport,
        name: &str,
        message_type: MessageType,
        sequence_id: i32
    ) {
        let version = ((BINARY_PROTOCOL_VERSION_1 as i32) << 16) | message_type as i32;
        self.write_i32(transport, version);
        self.write_str(transport, name);
        self.write_i32(transport, sequence_id);
    }

    fn write_message_end(&self, _transport: &mut Transport) { }

    fn write_struct_begin(&self, _transport: &mut Transport, _name: &str) { }

    fn write_struct_end(&self, _transport: &mut Transport) { }

    fn write_field_begin(
        &self, transport: &mut Transport,
        _name: &str,
        field_type: Type,
        field_id: i16
    ) {
        self.write_type(transport, field_type);
        self.write_i16(transport, field_id);
    }

    fn write_field_end(&self, _transport: &mut Transport) { }

    fn write_field_stop(&self, transport: &mut Transport) {
        self.write_byte(transport, protocol::Type::TStop as i8);
    }

    fn write_map_begin(&self, transport: &mut Transport, key_type: Type, value_type: Type, size: i32) {
        self.write_type(transport, key_type);
        self.write_type(transport, value_type);
        self.write_i32(transport, size);
    }

    fn write_map_end(&self, _transport: &mut Transport) { }

    fn write_list_begin(&self, transport: &mut Transport, elem_type: Type, size: i32) {
        self.write_type(transport, elem_type);
        self.write_i32(transport, size);
    }

    fn write_list_end(&self, _transport: &mut Transport) { }

    fn write_set_begin(&self, transport: &mut Transport, elem_type: Type, size: i32) {
        self.write_type(transport, elem_type);
        self.write_i32(transport, size);
    }

    fn write_set_end(&self, _transport: &mut Transport) { }

    fn write_bool(&self, transport: &mut Transport, value: bool) {
        self.write_byte(transport, value as i8);
    }

    fn write_byte(&self, transport: &mut Transport, value: i8) {
        match transport.write_i8(value) {
            Ok(_) => (),
            Err(e) => panic!(e),
        }
    }

    fn write_i16(&self, transport: &mut Transport, value: i16) {
        match transport.write_be_i16(value) {
            Ok(_) => (),
            Err(e) => panic!(e),
        }
    }

    fn write_i32(&self, transport: &mut Transport, value: i32) {
        match transport.write_be_i32(value) {
            Ok(_) => (),
            Err(e) => panic!(e),
        }
    }

    fn write_i64(&self, transport: &mut Transport, value: i64) {
        match transport.write_be_i64(value) {
            Ok(_) => (),
            Err(e) => panic!(e),
        }
    }

    fn write_double(&self, transport: &mut Transport, value: f64) {
        match transport.write_be_f64(value) {
            Ok(_) => (),
            Err(e) => panic!(e),
        }
    }

    fn write_str(&self, transport: &mut Transport, value: &str) {
        self.write_binary(transport, value.as_bytes());
    }

    fn write_string(&self, transport: &mut Transport, value: &String) {
        self.write_binary(transport, value.as_slice().as_bytes());
    }

    fn write_binary(&self, transport: &mut Transport, value: &[u8]) {
        self.write_i32(transport, value.len() as i32);
        match transport.write(value) {
            Ok(_) => (),
            Err(e) => panic!(e),
        }
    }

    fn read_message_begin(&self, transport: &mut Transport) -> TResult<(String, MessageType, i32)> {
        let header = try!(self.read_i32(transport));
        let version = (header >> 16) as u16;
        if version != BINARY_PROTOCOL_VERSION_1 {
            return Err(ThriftErr::BadVersion);
        };
        let name = try!(self.read_string(transport));
        let raw_type = header & 0xff;
        let message_type = match FromPrimitive::from_i32(raw_type) {
            Some(t) => t,
            None => return Err(ThriftErr::InvalidData),
        };
        let sequence_id = try!(self.read_i32(transport));
        Ok((name, message_type, sequence_id))
    }

    fn read_message_end(&self, _transport: &mut Transport) -> TResult<()> {
        Ok(())
    }

    fn read_struct_begin(&self, _transport: &mut Transport) -> TResult<String> {
        Ok(String::new())
    }

    fn read_struct_end(&self, _transport: &mut Transport) -> TResult<()> {
        Ok(())
    }

    fn read_field_begin(&self, transport: &mut Transport) -> TResult<(String, Type, i16)> {
        let field_type = try!(self.read_type(transport));
        let field_id = match field_type {
            protocol::Type::TStop => 0,
            _ => try!(self.read_i16(transport)),
        };
        Ok((String::new(), field_type, field_id))
    }

    fn read_field_end(&self, _transport: &mut Transport) -> TResult<()> {
        Ok(())
    }

    fn read_map_begin(&self, transport: &mut Transport) -> TResult<(Type, Type, i32)> {
        let key_type = try!(self.read_type(transport));
        let value_type = try!(self.read_type(transport));
        let size = try!(self.read_i32(transport));
        Ok((key_type, value_type, size))
    }

    fn read_map_end(&self, _transport: &mut Transport) -> TResult<()> {
        Ok(())
    }

    fn read_list_begin(&self, transport: &mut Transport) -> TResult<(Type, i32)> {
        let elem_type = try!(self.read_type(transport));
        let size = try!(self.read_i32(transport));
        Ok((elem_type, size))
    }

    fn read_list_end(&self, _transport: &mut Transport) -> TResult<()> {
        Ok(())
    }

    fn read_set_begin(&self, transport: &mut Transport) -> TResult<(Type, i32)> {
        let elem_type = try!(self.read_type(transport));
        let size = try!(self.read_i32(transport));
        Ok((elem_type, size))
    }

    fn read_set_end(&self, _transport: &mut Transport) -> TResult<()> {
        Ok(())
    }

    fn read_bool(&self, transport: &mut Transport) -> TResult<bool> {
        match try!(self.read_byte(transport)) {
            0 => Ok(false),
            _ => Ok(true),
        }
    }

    fn read_byte(&self, transport: &mut Transport) -> TResult<i8> {
        transport.read_i8().map_err(|e| ThriftErr::TransportError(e))
    }

    fn read_i16(&self, transport: &mut Transport) -> TResult<i16> {
        transport.read_be_i16().map_err(|e| ThriftErr::TransportError(e))
    }

    fn read_i32(&self, transport: &mut Transport) -> TResult<i32> {
        transport.read_be_i32().map_err(|e| ThriftErr::TransportError(e))
    }

    fn read_i64(&self, transport: &mut Transport) -> TResult<i64> {
        transport.read_be_i64().map_err(|e| ThriftErr::TransportError(e))
    }

    fn read_double(&self, transport: &mut Transport) -> TResult<f64> {
        transport.read_be_f64().map_err(|e| ThriftErr::TransportError(e))
    }

    fn read_string(&self, transport: &mut Transport) -> TResult<String> {
        let bytes = try!(self.read_binary(transport));
        String::from_utf8(bytes).map_err(|e| ThriftErr::InvalidUtf8(e.utf8_error()))
    }

    fn read_binary(&self, transport: &mut Transport) -> TResult<Vec<u8>> {
        let len = try!(self.read_i32(transport)) as usize;
        transport.read_exact(len).map_err(|e| ThriftErr::TransportError(e))
    }

    fn skip(&self, transport: &mut Transport, type_: Type) -> TResult<()> {
        match type_ {
            Type::TBool => { try!(self.read_bool(transport)); }
            Type::TByte => { try!(self.read_byte(transport)); }
            Type::TI16 => { try!(self.read_i16(transport)); }
            Type::TI32 => { try!(self.read_i32(transport)); }
            Type::TI64 => { try!(self.read_i64(transport)); }
            Type::TDouble => { try!(self.read_double(transport)); }
            Type::TString => { try!(self.read_binary(transport)); }
            Type::TStruct => { 
                try!(self.read_struct_begin(transport));
                loop {
                    let (_, field_type, _) = try!(self.read_field_begin(transport));
                    if field_type == Type::TStop {
                        break;
                    }
                    try!(self.skip(transport, field_type));
                    try!(self.read_field_end(transport));
                }
                try!(self.read_struct_end(transport));
            }
            Type::TMap => {
                let (key_type, value_type, size) = try!(self.read_map_begin(transport));
                for _ in range(0, size) {
                    try!(self.skip(transport, key_type));
                    try!(self.skip(transport, value_type));
                }
                try!(self.read_map_end(transport));
            }
            Type::TSet => {
                let (elem_type, size) = try!(self.read_set_begin(transport));
                for _ in range(0, size) {
                    try!(self.skip(transport, elem_type));
                }
                try!(self.read_set_end(transport));
            }
            Type::TList => {
                let (elem_type, size) = try!(self.read_list_begin(transport));
                for _ in range(0, size) {
                    try!(self.skip(transport, elem_type));
                }
                try!(self.read_list_end(transport));
            }
            Type::TVoid => { }
            Type::TStop => { }
        };
        Ok(())
    }
}

#[cfg(test)]
pub mod test;
