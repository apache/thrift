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

use byteorder::{BigEndian, ByteOrder, ReadBytesExt, WriteBytesExt};
use integer_encoding::{VarIntReader, VarIntWriter};
use std::cell::RefCell;
use std::convert::From;
use std::rc::Rc;
use std::io::{Read, Write};

use ::transport::TTransport;
use super::{TFieldIdentifier, TListIdentifier, TMapIdentifier, TMessageIdentifier, TProtocol, TProtocolFactory, TSetIdentifier, TStructIdentifier, TType};

// Using the following document as a basis
// https://issues.apache.org/jira/secure/attachment/12398366/compact-proto-spec-2.txt

/// Sends messages over an underlying transport
/// `transport` using the Thrift Compact protocol
/// as described in THRIFT-110.
pub struct TCompactProtocol {
    /// Underlying transport used for byte-level operations.
    pub transport: Rc<RefCell<Box<TTransport>>>,
}

impl TProtocol for TCompactProtocol {
    fn write_message_begin(&mut self, _: &TMessageIdentifier) -> ::Result<()> {
        unimplemented!()
    }

    fn write_message_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_struct_begin(&mut self, _: &TStructIdentifier) -> ::Result<()> {
        unimplemented!()
    }

    fn write_struct_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_field_begin(&mut self, _: &TFieldIdentifier) -> ::Result<()> {
        unimplemented!()
    }

    fn write_field_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_field_stop(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn write_bool(&mut self, b: bool) -> ::Result<()> {
        match b {
            true => self.write_byte(0x01),
            false => self.write_byte(0x02),
        }
    }

    fn write_bytes(&mut self, b: &[u8]) -> ::Result<()> {
        let size = b.len() as i32;
        try!(self.transport.borrow_mut().write_varint(size));
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

    fn write_list_begin(&mut self, _: &TListIdentifier) -> ::Result<()> {
        unimplemented!()
    }

    fn write_list_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_set_begin(&mut self, _: &TSetIdentifier) -> ::Result<()> {
        unimplemented!()
    }

    fn write_set_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_map_begin(&mut self, _: &TMapIdentifier) -> ::Result<()> {
        unimplemented!()
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
        unimplemented!()
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
        unimplemented!()
    }

    fn read_bytes(&mut self) -> ::Result<Vec<u8>> {
        unimplemented!()
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
        unimplemented!()
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
        unimplemented!()
    }
}

/// Convenience object that can be used to
/// create an instance of `TCompactProtocol`.
pub struct TCompactProtocolFactory;
impl TProtocolFactory for TCompactProtocolFactory {
    fn build(&self, transport: Rc<RefCell<Box<TTransport>>>) -> Box<TProtocol> {
        Box::new(TCompactProtocol { transport: transport }) as Box<TProtocol>
    }
}

/*
impl From<TType> for u8 {
    fn from(field_type: TType) -> Self {
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
}
*/

#[cfg(test)]
mod tests {

    // use super::*;
    // use ::protocol::TProtocol;
}
