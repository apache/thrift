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

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use integer_encoding::{VarIntReader, VarIntWriter};
use std::cell::RefCell;
use std::convert::From;
use std::rc::Rc;
use std::io::{Read, Write};

use ::transport::TTransport;
use super::{TFieldIdentifier, TListIdentifier, TMapIdentifier, TMessageIdentifier, TProtocol, TProtocolFactory, TSetIdentifier, TStructIdentifier, TType};

const COMPACT_PROTOCOL_ID: u8 = 0x82;
const COMPACT_VERSION: u8 = 0x01;
//const COMPACT_VERSION_MASK: u8 = 0x1F;
//const COMPACT_TYPE_MASK: u8 = 0xE0;
//const COMPACT_TYPE_BITS: u8 = 0x07; // FIXME: do I need this?
//const COMPACT_TYPE_SHIFT_AMOUNT: u8 = 5; // FIXME: do I need this?

/// Sends messages over an underlying transport
/// `transport` using the Thrift Compact protocol
/// as described in THRIFT-110.
pub struct TCompactProtocol {
    /// Current write state of the protocol.
    write_state: WriteState,
    /// Identifier of the last field serialized for a struct.
    last_field_id: i16,
    /// Stack of the last write state and last-written field id.
    /// Supports the use case where structs are nested, since deltas
    /// are only calculated *within* a struct.
    previous_write_states: Vec<(WriteState, i16)>,
    /// Contains the field identifier of the boolean field to be written.
    /// This has to be saved because boolean field values and their identifier
    /// are written into the same byte.
    pending_bool_field_identifier: Option<TFieldIdentifier>,
    /// Underlying transport used for byte-level operations.
    pub transport: Rc<RefCell<Box<TTransport>>>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum WriteState {
    Clear,
    ValueWrite,
    FieldWrite,
    ContainerWrite,
    BoolWrite,
}

impl TCompactProtocol {
    pub fn new(transport: Rc<RefCell<Box<TTransport>>>) -> TCompactProtocol {
       TCompactProtocol {
           write_state: WriteState::Clear,
           pending_bool_field_identifier: None,
           last_field_id: 0,
           previous_write_states: Vec::new(),
           transport: transport
       }
    }

    fn verify_write_state(&self, expected: WriteState) -> ::Result<()> {
        if expected != self.write_state {
            panic!(
                "expected to be in {:?} write state, but in {:?} write state",
                expected,
                self.write_state
            );
        }

        Ok(())
    }

    fn verify_write_state_in(&self, allowed_states: &[WriteState]) -> ::Result<()> {
        if !allowed_states.contains(&self.write_state) {
            panic!(
                "expected to be in one of {:?} write state, but in {:?} write state",
                allowed_states,
                self.write_state
            );
        }

        Ok(())
    }

    // FIXME: field_type as unconstrained u8 is bad
    fn write_field_header(&mut self, field_type: u8, field_id: i16) -> ::Result<()> {
        let field_delta = self.last_field_id - field_id;
        if field_delta < 15 {
            try!(self.write_byte(((field_delta as u8) << 4) | field_type));
        } else {
            try!(self.write_byte(field_type));
            try!(self.write_i16(field_id));
        }

        self.write_state = WriteState::ValueWrite;
        self.last_field_id = field_id;

        Ok(())
    }

    fn write_list_set_begin(&mut self, element_type: TType, element_count: i32) -> ::Result<()> {
        let elem_identifier = field_type_to_u8(element_type);
        if element_count <= 14 {
            let header = (element_count as u8) << 4 | elem_identifier;
            self.write_byte(header)
        } else {
            let header = 0xF0 | elem_identifier;
            try!(self.write_byte(header));
            self.write_i32(element_count)
        }
    }
}

fn field_type_to_u8(field_type: TType) -> u8 {
    match field_type {
        TType::Stop => 0x00,
        TType::I08 => 0x03, // equivalent to TType::Byte
        TType::I16 => 0x04,
        TType::I32 => 0x05,
        TType::I64 => 0x06,
        TType::Double => 0x07,
        TType::String => 0x08,
        TType::Utf7 => 0x08,
        TType::List => 0x09,
        TType::Set => 0x0A,
        TType::Map => 0x0B,
        TType::Struct => 0x0C,
        _ => panic!(format!("should not have attempted to convert {} to u8", field_type))
    }
}

impl TProtocol for TCompactProtocol {
    fn write_message_begin(&mut self, identifier: &TMessageIdentifier) -> ::Result<()> {
        try!(self.verify_write_state(WriteState::Clear));

        try!(self.write_byte(COMPACT_PROTOCOL_ID));
        try!(self.write_byte((u8::from(identifier.message_type) << 5) | COMPACT_VERSION));
        try!(self.write_i32(identifier.sequence_number));
        try!(self.write_string(&identifier.name));

        self.write_state = WriteState::ValueWrite;

        Ok(())
    }

    fn write_message_end(&mut self) -> ::Result<()> {
        try!(self.verify_write_state(WriteState::ValueWrite));
        self.write_state = WriteState::Clear;
        Ok(())
    }

    fn write_struct_begin(&mut self, _: &TStructIdentifier) -> ::Result<()> {
        try!(self.verify_write_state_in(&[WriteState::ValueWrite, WriteState::ContainerWrite]));

        self.previous_write_states.push((self.write_state, self.last_field_id));

        self.write_state = WriteState::FieldWrite;
        self.last_field_id = 0;

        Ok(())
    }

    fn write_struct_end(&mut self) -> ::Result<()> {
        try!(self.verify_write_state(WriteState::FieldWrite));

        let (previous_write_state, previous_last_field_id) =
            self.previous_write_states.pop().expect("should not have empty previous state stack");

        self.write_state = previous_write_state;
        self.last_field_id = previous_last_field_id;

        Ok(())
    }

    fn write_field_begin(&mut self, identifier: &TFieldIdentifier) -> ::Result<()> {
        try!(self.verify_write_state(WriteState::FieldWrite));

        match identifier.field_type {
            TType::Bool => {
                self.pending_bool_field_identifier = Some(identifier.clone());
                self.write_state = WriteState::BoolWrite;
                Ok(())
            },
            _ => {
                let field_type = field_type_to_u8(identifier.field_type);
                let field_id = identifier.id.expect("non-stop field should have field id");
                self.write_field_header(field_type, field_id)
            }
        }
    }

    fn write_field_end(&mut self) -> ::Result<()> {
        try!(self.verify_write_state_in(&[WriteState::BoolWrite, WriteState::ValueWrite]));
        self.write_state = WriteState::FieldWrite;
        Ok(())
    }

    fn write_field_stop(&mut self) -> ::Result<()> {
        try!(self.verify_write_state(WriteState::ValueWrite));
        self.write_byte(field_type_to_u8(TType::Stop))
    }

    fn write_bool(&mut self, b: bool) -> ::Result<()> {
        match self.write_state {
            WriteState::BoolWrite => {
                let bool_field_id = self.pending_bool_field_identifier
                        .as_ref()
                        .expect("pending identifier must exist in write state BoolWrite")
                        .id.expect("non-stop field should have a field id");
                let field_type_as_u8 = match b {
                    true => 0x01,
                    false => 0x02,
                };

                try!(self.write_field_header(field_type_as_u8, bool_field_id));

                self.pending_bool_field_identifier = None;

                Ok(())
            },
            WriteState::ValueWrite => {
                match b {
                    true => self.write_byte(0x01),
                    false => self.write_byte(0x02),
                }
            }
            _ => panic!("cannot write bool in {:?} write state", self.write_state)
        }
    }

    fn write_bytes(&mut self, b: &[u8]) -> ::Result<()> {
        try!(self.write_i32(b.len() as i32));
        self.transport.borrow_mut().write_all(b).map_err(From::from)
    }

    fn write_i8(&mut self, i: i8) -> ::Result<()> {
        self.write_byte(i as u8)
    }

    fn write_i16(&mut self, i: i16) -> ::Result<()> {
        self.transport.borrow_mut().write_varint(i).map_err(From::from).map(|_| ())
    }

    fn write_i32(&mut self, i: i32) -> ::Result<()> {
        self.transport.borrow_mut().write_varint(i).map_err(From::from).map(|_| ())
    }

    fn write_i64(&mut self, i: i64) -> ::Result<()> {
        self.transport.borrow_mut().write_varint(i).map_err(From::from).map(|_| ())
    }

    fn write_double(&mut self, d: f64) -> ::Result<()> {
        self.transport.borrow_mut().write_f64::<BigEndian>(d).map_err(From::from)
    }

    fn write_string(&mut self, s: &str) -> ::Result<()> {
        self.write_bytes(s.as_bytes())
    }

    fn write_list_begin(&mut self, identifier: &TListIdentifier) -> ::Result<()> {
        self.write_list_set_begin(identifier.element_type, identifier.size)
    }

    fn write_list_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_set_begin(&mut self, identifier: &TSetIdentifier) -> ::Result<()> {
        self.write_list_set_begin(identifier.element_type, identifier.size)
    }

    fn write_set_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_map_begin(&mut self, identifier: &TMapIdentifier) -> ::Result<()> {
        if identifier.size == 0 {
            self.write_byte(0)
        } else {
            try!(self.write_i32(identifier.size));
            let map_type_header = (field_type_to_u8(identifier.key_type) << 4) | field_type_to_u8(identifier.value_type);
            self.write_byte(map_type_header)
        }
    }

    fn write_map_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn flush(&mut self) -> ::Result<()> {
        self.transport.borrow_mut().flush().map_err(From::from)
    }

    fn read_message_begin(&mut self) -> ::Result<TMessageIdentifier> {
        unimplemented!()
    }

    fn read_message_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_struct_begin(&mut self) -> ::Result<Option<TStructIdentifier>> {
        Ok(None)
    }

    fn read_struct_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_field_begin(&mut self) -> ::Result<TFieldIdentifier> {
        unimplemented!()
    }

    fn read_field_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_bool(&mut self) -> ::Result<bool> {
        let b = try!(self.read_byte());
        match b {
            0x01 => Ok(true),
            0x02 => Ok(false),
            unkn => {
                Err(
                    ::Error::Protocol(
                        ::ProtocolError {
                            kind: ::ProtocolErrorKind::InvalidData,
                            message: format!("cannot convert {} into a boolean", unkn)
                        }
                    )
                )
            }
        }
    }

    fn read_bytes(&mut self) -> ::Result<Vec<u8>> {
        let len = try!(self.read_i32());
        let mut buf = vec![0u8; len as usize];
        try!(self.transport.borrow_mut().read_exact(&mut buf));
        Ok(buf)
    }

    fn read_i8(&mut self) -> ::Result<i8> {
        self.read_byte().map(|i| i as i8)
    }

    fn read_i16(&mut self) -> ::Result<i16> {
        self.transport.borrow_mut().read_varint::<i16>().map_err(From::from)
    }

    fn read_i32(&mut self) -> ::Result<i32> {
        self.transport.borrow_mut().read_varint::<i32>().map_err(From::from)
    }

    fn read_i64(&mut self) -> ::Result<i64> {
        self.transport.borrow_mut().read_varint::<i64>().map_err(From::from)
    }

    fn read_double(&mut self) -> ::Result<f64> {
        self.transport.borrow_mut().read_f64::<BigEndian>().map_err(From::from)
    }

    fn read_string(&mut self) -> ::Result<String> {
        let bytes = try!(self.read_bytes());
        String::from_utf8(bytes).map_err(From::from)
    }

    fn read_list_begin(&mut self) -> ::Result<TListIdentifier> {
        unimplemented!()
    }

    fn read_list_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_set_begin(&mut self) -> ::Result<TSetIdentifier> {
        unimplemented!()
    }

    fn read_set_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_map_begin(&mut self) -> ::Result<TMapIdentifier> {
        unimplemented!()
    }

    fn read_map_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    //
    // utility
    //

    fn write_byte(&mut self, b: u8) -> ::Result<()> {
        let bytes: [u8; 1] = [b];
        self.transport.borrow_mut().write(&bytes).map_err(From::from).map(|_| ())
    }

    fn read_byte(&mut self) -> ::Result<u8> {
        let mut buf = vec![0u8; 1];
        self.transport.borrow_mut().read_exact(&mut buf).map_err(From::from).map(|_| buf[0])
    }
}

/// Convenience object that can be used to
/// create an instance of `TCompactProtocol`.
pub struct TCompactProtocolFactory;
impl TProtocolFactory for TCompactProtocolFactory {
    fn build(&self, transport: Rc<RefCell<Box<TTransport>>>) -> Box<TProtocol> {
        Box::new(TCompactProtocol::new(transport)) as Box<TProtocol>
    }
}

#[cfg(test)]
mod tests {

    // use super::*;
    // use ::protocol::TProtocol;
}
