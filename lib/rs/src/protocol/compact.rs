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
use try_from::TryFrom;

use ::transport::TTransport;
use super::{TFieldIdentifier, TListIdentifier, TMapIdentifier, TMessageIdentifier, TMessageType, TProtocol, TProtocolFactory, TSetIdentifier, TStructIdentifier, TType};

const COMPACT_PROTOCOL_ID: u8 = 0x82;
const COMPACT_VERSION: u8 = 0x01;
const COMPACT_VERSION_MASK: u8 = 0x1F;

/// Sends messages over an underlying transport
/// `transport` using the Thrift Compact protocol
/// as described in THRIFT-110.
pub struct TCompactProtocol {
    /// Identifier of the last field deserialized for a struct.
    last_read_field_id: i16,
    /// Identifier of the last field serialized for a struct.
    last_write_field_id: i16,
    /// Stack of the last read field ids (a new entry is added each time a nested struct is read).
    read_field_id_stack: Vec<i16>,
    /// Stack of the last written field ids (a new entry is added each time a nested struct is written).
    write_field_id_stack: Vec<i16>,
    /// Boolean value for a field.
    /// Saved because boolean fields and their value are encoded in a single byte,
    /// and reading the field only occurs after the field id is read.
    pending_read_bool_value: Option<bool>,
    /// Field identifier of the boolean field to be written.
    /// Saved because boolean fields and their value are encoded in a single byte
    pending_write_bool_field_identifier: Option<TFieldIdentifier>,
    /// Underlying transport used for byte-level operations.
    pub transport: Rc<RefCell<Box<TTransport>>>,
}

impl TCompactProtocol {
    pub fn new(transport: Rc<RefCell<Box<TTransport>>>) -> TCompactProtocol {
       TCompactProtocol {
           last_read_field_id: 0,
           last_write_field_id: 0,
           read_field_id_stack: Vec::new(),
           write_field_id_stack: Vec::new(),
           pending_read_bool_value: None,
           pending_write_bool_field_identifier: None,
           transport: transport
       }
    }

    // FIXME: field_type as unconstrained u8 is bad
    fn write_field_header(&mut self, field_type: u8, field_id: i16) -> ::Result<()> {
        let field_delta = self.last_write_field_id - field_id;
        if field_delta > 0 && field_delta < 15 {
            try!(self.write_byte(((field_delta as u8) << 4) | field_type));
        } else {
            try!(self.write_byte(field_type));
            try!(self.write_i16(field_id));
        }
        self.last_write_field_id = field_id;
        Ok(())
    }

    fn write_list_set_begin(&mut self, element_type: TType, element_count: i32) -> ::Result<()> {
        let elem_identifier = type_to_u8(element_type);
        if element_count <= 14 {
            let header = (element_count as u8) << 4 | elem_identifier;
            self.write_byte(header)
        } else {
            let header = 0xF0 | elem_identifier;
            try!(self.write_byte(header));
            self.transport.borrow_mut().write_varint(element_count as u32).map_err(From::from).map(|_| ())
        }
    }

    fn read_list_set_begin(&mut self) -> ::Result<(TType, i32)> {
        let header = try!(self.read_byte());
        let element_type = try!(u8_to_type(header & 0x0F));

        let element_count;
        let possible_element_count = (header & 0xF0) >> 4;
        if possible_element_count != 15 { // high bits set high if count and type encoded separately
            element_count = possible_element_count as i32;
        } else {
            element_count = try!(self.transport.borrow_mut().read_varint::<u32>()) as i32;
        }

        Ok((element_type, element_count))
    }
}

fn type_to_u8(field_type: TType) -> u8 {
    match field_type {
        TType::Stop => 0x00,
        TType::I08 => 0x03, // equivalent to TType::Byte
        TType::I16 => 0x04,
        TType::I32 => 0x05,
        TType::I64 => 0x06,
        TType::Double => 0x07,
        TType::String => 0x08,
        TType::List => 0x09,
        TType::Set => 0x0A,
        TType::Map => 0x0B,
        TType::Struct => 0x0C,
        _ => panic!(format!("should not have attempted to convert {} to u8", field_type))
    }
}

fn u8_to_type(b: u8) -> ::Result<TType> {
    match b {
        0x00 => Ok(TType::Stop),
        0x03 => Ok(TType::I08), // equivalent to TType::Byte
        0x04 => Ok(TType::I16),
        0x05 => Ok(TType::I32),
        0x06 => Ok(TType::I64),
        0x07 => Ok(TType::Double),
        0x08 => Ok(TType::String),
        0x09 => Ok(TType::List),
        0x0A => Ok(TType::Set),
        0x0B => Ok(TType::Map),
        0x0C => Ok(TType::Struct),
        unkn => Err(
            ::Error::Protocol(
                ::ProtocolError {
                    kind: ::ProtocolErrorKind::InvalidData,
                    message: format!("cannot convert {} into TType", unkn),
                }
            )
        )
    }
}

impl TProtocol for TCompactProtocol {
    fn write_message_begin(&mut self, identifier: &TMessageIdentifier) -> ::Result<()> {
        try!(self.write_byte(COMPACT_PROTOCOL_ID));
        try!(self.write_byte((u8::from(identifier.message_type) << 5) | COMPACT_VERSION));
        try!(self.write_i32(identifier.sequence_number));
        try!(self.write_string(&identifier.name));
        Ok(())
    }

    fn write_message_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_struct_begin(&mut self, _: &TStructIdentifier) -> ::Result<()> {
        self.write_field_id_stack.push(self.last_write_field_id);
        self.last_write_field_id = 0;
        Ok(())
    }

    fn write_struct_end(&mut self) -> ::Result<()> {
        self.last_write_field_id = self.write_field_id_stack.pop().expect("should have previous field ids");
        Ok(())
    }

    fn write_field_begin(&mut self, identifier: &TFieldIdentifier) -> ::Result<()> {
        match identifier.field_type {
            TType::Bool => {
                if self.pending_write_bool_field_identifier.is_some() {
                    panic!("should not have a pending bool while writing another bool with id: {:?}", identifier)
                }
                self.pending_write_bool_field_identifier = Some(identifier.clone());
                Ok(())
            },
            _ => {
                let field_type = type_to_u8(identifier.field_type);
                let field_id = identifier.id.expect("non-stop field should have field id");
                self.write_field_header(field_type, field_id)
            }
        }
    }

    fn write_field_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_field_stop(&mut self) -> ::Result<()> {
        self.write_byte(type_to_u8(TType::Stop))
    }

    fn write_bool(&mut self, b: bool) -> ::Result<()> {
        match self.pending_write_bool_field_identifier.take() {
            Some(pending) => {
                let field_id = pending.id.expect("bool field should have a field id");
                let field_type_as_u8 = if b {
                    0x01
                } else {
                    0x02
                };
                self.write_field_header(field_type_as_u8, field_id)
            },
            None => {
                if b {
                    self.write_byte(0x01)
                } else {
                    self.write_byte(0x02)
                }
            }
        }
    }

    fn write_bytes(&mut self, b: &[u8]) -> ::Result<()> {
        try!(self.transport.borrow_mut().write_varint(b.len() as u32));
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
            try!(self.transport.borrow_mut().write_varint(identifier.size as u32));

            let key_type = identifier.key_type.expect("map identifier to write should contain key type");
            let key_type_byte = type_to_u8(key_type) << 4;

            let val_type = identifier.value_type.expect("map identifier to write should contain value type");
            let val_type_byte = type_to_u8(val_type);

            let map_type_header = key_type_byte | val_type_byte;
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
        let compact_id = try!(self.read_byte());
        try!(
            if compact_id != COMPACT_PROTOCOL_ID {
                Err(
                    ::Error::Protocol(
                        ::ProtocolError {
                            kind: ::ProtocolErrorKind::BadVersion,
                            message: format!("invalid compact protocol header {:?}", compact_id)
                        }
                    )
                )
            } else {
                Ok(())
            }
        );

        let type_and_byte = try!(self.read_byte());
        let received_version = type_and_byte & COMPACT_VERSION_MASK;
        try!(
            if received_version != COMPACT_VERSION {
                Err(
                    ::Error::Protocol(
                        ::ProtocolError {
                            kind: ::ProtocolErrorKind::BadVersion,
                            message: format!("cannot process compact protocol version {:?}", received_version)
                        }
                    )
                )
            } else {
                Ok(())
            }
        );

        // NOTE: unsigned right shift will pad with 0s
        let message_type: TMessageType = try!(TMessageType::try_from(type_and_byte >> 5));
        let sequence_number = try!(self.read_i32());
        let service_call_name = try!(self.read_string());

        self.last_read_field_id = 0;

        Ok(TMessageIdentifier::new(service_call_name, message_type, sequence_number))
    }

    fn read_message_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_struct_begin(&mut self) -> ::Result<Option<TStructIdentifier>> {
        self.read_field_id_stack.push(self.last_read_field_id);
        self.last_read_field_id = 0;
        Ok(None)
    }

    fn read_struct_end(&mut self) -> ::Result<()> {
        self.last_read_field_id = self.read_field_id_stack
            .pop()
            .expect("should have previous field ids");
        Ok(())
    }

    fn read_field_begin(&mut self) -> ::Result<TFieldIdentifier> {
        // we can read at least one byte, which is:
        // - the type
        // - the field delta and the type
        let field_type = try!(self.read_byte());
        let field_delta = (field_type & 0xF0) >> 4;
        let field_type = try!(
            match field_type & 0x0F {
                0x01 => {
                    self.pending_read_bool_value = Some(true);
                    Ok(TType::Bool)
                },
                0x02 => {
                    self.pending_read_bool_value = Some(false);
                    Ok(TType::Bool)
                },
                ttu8 => {
                    u8_to_type(ttu8)
                }
            }
        );

        match field_type {
            TType::Stop => {
                Ok(TFieldIdentifier::new::<Option<String>, String, Option<i16>>(None, TType::Stop, None))
            }
            _ => {
                let field_id = if field_delta != 0 {
                    self.last_read_field_id += field_delta as i16;
                    self.last_read_field_id
                } else {
                    try!(self.read_i16())
                };

                Ok(TFieldIdentifier::new::<Option<String>, String, i16>(None, field_type, field_id))
            }
        }
    }

    fn read_field_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_bool(&mut self) -> ::Result<bool> {
        match self.pending_read_bool_value.take() {
            Some(b) => Ok(b),
            None => {
                let b = try!(self.read_byte());
                match b {
                    0x01 => Ok(true),
                    0x02 => Ok(false),
                    unkn => {
                        Err(
                            ::Error::Protocol(
                                ::ProtocolError {
                                    kind: ::ProtocolErrorKind::InvalidData,
                                    message: format!("cannot convert {} into bool", unkn)
                                }
                            )
                        )
                    }
                }
            }
        }
    }

    fn read_bytes(&mut self) -> ::Result<Vec<u8>> {
        let len = try!(self.transport.borrow_mut().read_varint::<u32>());
        let mut buf = vec![0u8; len as usize];
        self.transport.borrow_mut().read_exact(&mut buf).map_err(From::from).map(|_| buf)
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
        let (element_type, element_count) = try!(self.read_list_set_begin());
        Ok(TListIdentifier::new(element_type, element_count))
    }

    fn read_list_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_set_begin(&mut self) -> ::Result<TSetIdentifier> {
        let (element_type, element_count) = try!(self.read_list_set_begin());
        Ok(TSetIdentifier::new(element_type, element_count))
    }

    fn read_set_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_map_begin(&mut self) -> ::Result<TMapIdentifier> {
        let element_count = try!(self.transport.borrow_mut().read_varint::<u32>()) as i32;
        if element_count == 0 {
            Ok(TMapIdentifier::new(None, None, 0))
        } else {
            let type_header = try!(self.read_byte());
            let key_type = try!(u8_to_type((type_header & 0xF0) >> 4));
            let val_type = try!(u8_to_type((type_header & 0x0F)));
            Ok(TMapIdentifier::new(key_type, val_type, element_count))
        }
    }

    fn read_map_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    //
    // utility
    //

    fn write_byte(&mut self, b: u8) -> ::Result<()> {
        self.transport.borrow_mut().write(&[b]).map_err(From::from).map(|_| ())
    }

    fn read_byte(&mut self) -> ::Result<u8> {
        let mut buf = [0u8; 1];
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
