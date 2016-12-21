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
use std::cell::RefCell;
use std::convert::From;
use std::rc::Rc;
use try_from::TryFrom;

use ::{ProtocolError, ProtocolErrorKind};
use ::transport::TTransport;
use super::{TFieldIdentifier, TListIdentifier, TMapIdentifier, TMessageIdentifier, TMessageType, TProtocol, TProtocolFactory, TSetIdentifier, TStructIdentifier, TType};

/// Identifies the serialized message as conforming to Thrift binary protocol version 1.
const BINARY_PROTOCOL_VERSION_1: u32 = 0x80010000;

/// Sends messages over an underlying transport
/// `transport` using a simple binary protocol.
pub struct TBinaryProtocol {
    /// Set to `true` if the strict binary protocol is to be used.
    pub strict: bool,
    /// Underlying transport used for byte-level operations.
    pub transport: Rc<RefCell<Box<TTransport>>>,
}

impl TBinaryProtocol {
    fn write_transport(&mut self, buf: &[u8]) -> ::Result<()> {
        self.transport.borrow_mut().write(buf).map(|_| ()).map_err(From::from)
    }
}

fn field_type_to_u8(field_type: TType) -> u8 {
    match field_type {
        TType::Stop => 0x00,
        TType::Void => 0x01,
        TType::Bool => 0x02,
        TType::I08 => 0x03, // equivalent to TType::Byte
        TType::Double => 0x04,
        TType::I16 => 0x06,
        TType::I32 => 0x08,
        TType::I64 => 0x0A,
        TType::String => 0x0B,
        TType::Utf7 => 0x0B,
        TType::Struct => 0x0C,
        TType::Map => 0x0D,
        TType::Set => 0x0E,
        TType::List => 0x0F,
        TType::Utf8 => 0x10,
        TType::Utf16 => 0x11,
    }
}

fn field_type_from_u8(b: u8) -> ::Result<TType> {
    match b {
        0x00 => Ok(TType::Stop),
        0x01 => Ok(TType::Void),
        0x02 => Ok(TType::Bool),
        0x03 => Ok(TType::I08), // Equivalent to TType::Byte
        0x04 => Ok(TType::Double),
        0x06 => Ok(TType::I16),
        0x08 => Ok(TType::I32),
        0x0A => Ok(TType::I64),
        0x0B => Ok(TType::String), // technically, also a UTF7, but we'll treat it as string
        0x0C => Ok(TType::Struct),
        0x0D => Ok(TType::Map),
        0x0E => Ok(TType::Set),
        0x0F => Ok(TType::List),
        0x10 => Ok(TType::Utf8),
        0x11 => Ok(TType::Utf16),
        unkn => Err(
            ::Error::Protocol(
                ProtocolError {
                    kind: ProtocolErrorKind::InvalidData,
                    message: format!("cannot convert {} to TType", unkn),
                }
            )
        )
    }
}

impl TProtocol for TBinaryProtocol {

    fn write_message_begin(&mut self, identifier: &TMessageIdentifier) -> ::Result<()> {
        if self.strict {
            let message_type: u8 = identifier.message_type.into();
            let header = BINARY_PROTOCOL_VERSION_1 | (message_type as u32);
            try!(self.transport.borrow_mut().write_u32::<BigEndian>(header));
            try!(self.write_string(&identifier.name));
            self.write_i32(identifier.sequence_number)
        } else {
            try!(self.write_string(&identifier.name));
            try!(self.write_byte(identifier.message_type.into()));
            self.write_i32(identifier.sequence_number)
        }
    }

    fn write_message_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_struct_begin(&mut self, _: &TStructIdentifier) -> ::Result<()> {
        Ok(())
    }

    fn write_struct_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_field_begin(&mut self, identifier: &TFieldIdentifier) -> ::Result<()> {
        if identifier.id.is_none() && identifier.field_type != TType::Stop {
            return Err(
                ::Error::Protocol(
                    ProtocolError {
                        kind: ProtocolErrorKind::Unknown,
                        message: format!("cannot write identifier {:?} without sequence number", &identifier),
                    }
                )
            )
        }

        try!(self.write_byte(field_type_to_u8(identifier.field_type)));
        if let Some(id) = identifier.id {
            self.write_i16(id)
        } else {
            Ok(())
        }
    }

    fn write_field_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_field_stop(&mut self) -> ::Result<()> {
        self.write_byte(field_type_to_u8(TType::Stop))
    }

    fn write_bytes(&mut self, b: &[u8]) -> ::Result<()> {
        try!(self.write_i32(b.len() as i32));
        self.write_transport(b)
    }

    fn write_bool(&mut self, b: bool) -> ::Result<()> {
        if b {
            self.write_i8(1)
        } else {
            self.write_i8(0)
        }
    }

    fn write_i8(&mut self, i: i8) -> ::Result<()> {
        self.transport.borrow_mut().write_i8(i).map_err(From::from)
    }

    fn write_i16(&mut self, i: i16) -> ::Result<()> {
        self.transport.borrow_mut().write_i16::<BigEndian>(i).map_err(From::from)
    }

    fn write_i32(&mut self, i: i32) -> ::Result<()> {
        self.transport.borrow_mut().write_i32::<BigEndian>(i).map_err(From::from)
    }

    fn write_i64(&mut self, i: i64) -> ::Result<()> {
        self.transport.borrow_mut().write_i64::<BigEndian>(i).map_err(From::from)
    }

    fn write_double(&mut self, d: f64) -> ::Result<()> {
        self.transport.borrow_mut().write_f64::<BigEndian>(d).map_err(From::from)
    }

    fn write_string(&mut self, s: &str) -> ::Result<()> {
        self.write_bytes(s.as_bytes())
    }

    fn write_list_begin(&mut self, identifier: &TListIdentifier) -> ::Result<()> {
        try!(self.write_byte(field_type_to_u8(identifier.element_type)));
        self.write_i32(identifier.size)
    }

    fn write_list_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_set_begin(&mut self, identifier: &TSetIdentifier) -> ::Result<()> {
        try!(self.write_byte(field_type_to_u8(identifier.element_type)));
        self.write_i32(identifier.size)
    }

    fn write_set_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_map_begin(&mut self, identifier: &TMapIdentifier) -> ::Result<()> {
        try!(self.write_byte(field_type_to_u8(identifier.key_type)));
        try!(self.write_byte(field_type_to_u8(identifier.value_type)));
        self.write_i32(identifier.size)
    }

    fn write_map_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn flush(&mut self) -> ::Result<()> {
        self.transport.borrow_mut().flush().map_err(From::from)
    }

    fn read_message_begin(&mut self) -> ::Result<TMessageIdentifier> {
        let mut first_bytes = vec![0; 4];
        try!(self.transport.borrow_mut().read_exact(&mut first_bytes[..]));

        // the thrift version header is intentionally negative
        // so the first check we'll do is see if the sign bit is set
        // and if so - assume it's the protocol-version header
        if first_bytes[0] >= 8 {
            // apparently we got a protocol-version header - check
            // it, and if it matches, read the rest of the fields
            if first_bytes[0..2] != [0x80, 0x01] {
                Err(
                    ::Error::Protocol(
                        ProtocolError {
                            kind: ProtocolErrorKind::BadVersion,
                            message: format!("received bad version: {:?}", &first_bytes[0..2]),
                        }
                    )
                )
            } else {
                let message_type: TMessageType = try!(TryFrom::try_from(first_bytes[3]));
                let name = try!(self.read_string());
                let sequence_number = try!(self.read_i32());
                Ok(TMessageIdentifier { name: name, message_type: message_type, sequence_number: sequence_number })
            }
        } else {
            // apparently we didn't get a protocol-version header,
            // which happens if the sender is not using the strict protocol
            if self.strict {
                // we're in strict mode however, and that always
                // requires the protocol-version header to be written first
                Err(
                    ::Error::Protocol(
                        ProtocolError {
                            kind: ProtocolErrorKind::BadVersion,
                            message: format!("received bad version: {:?}", &first_bytes[0..2]),
                        }
                    )
                )
            } else {
                // in the non-strict version the first message field
                // is the message name. strings (byte arrays) are length-prefixed,
                // so we've just read the length in the first 4 bytes
                let name_size = BigEndian::read_i32(&first_bytes) as usize;
                let mut name_buf: Vec<u8> = Vec::with_capacity(name_size);
                try!(self.transport.borrow_mut().read_exact(&mut name_buf));
                let name = try!(String::from_utf8(name_buf));

                // read the rest of the fields
                let message_type: TMessageType = try!(self.read_byte().and_then(TryFrom::try_from));
                let sequence_number = try!(self.read_i32());
                Ok(TMessageIdentifier { name: name, message_type: message_type, sequence_number: sequence_number })
            }
        }
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
        let field_type_byte = try!(self.read_byte());
        let field_type = try!(field_type_from_u8(field_type_byte));
        let id = try!(match field_type {
            TType::Stop => Ok(0),
            _ => self.read_i16()
        });
        Ok(TFieldIdentifier { name: None, field_type: field_type, id: Some(id) })
    }

    fn read_field_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_bytes(&mut self) -> ::Result<Vec<u8>> {
        let num_bytes = try!(self.transport.borrow_mut().read_i32::<BigEndian>()) as usize;
        let mut buf = vec![0u8; num_bytes];
        self.transport.borrow_mut().read_exact(&mut buf).map(|_| buf).map_err(From::from)
    }

    fn read_bool(&mut self) -> ::Result<bool> {
        let b = try!(self.read_i8());
        match b {
            0 => Ok(false),
            _ => Ok(true),
        }
    }

    fn read_i8(&mut self) -> ::Result<i8> {
        self.transport.borrow_mut().read_i8().map_err(From::from)
    }

    fn read_i16(&mut self) -> ::Result<i16> {
        self.transport.borrow_mut().read_i16::<BigEndian>().map_err(From::from)
    }

    fn read_i32(&mut self) -> ::Result<i32> {
        self.transport.borrow_mut().read_i32::<BigEndian>().map_err(From::from)
    }

    fn read_i64(&mut self) -> ::Result<i64> {
        self.transport.borrow_mut().read_i64::<BigEndian>().map_err(From::from)
    }

    fn read_double(&mut self) -> ::Result<f64> {
        self.transport.borrow_mut().read_f64::<BigEndian>().map_err(From::from)
    }

    fn read_string(&mut self) -> ::Result<String> {
        let bytes = try!(self.read_bytes());
        String::from_utf8(bytes).map_err(From::from)
    }

    fn read_list_begin(&mut self) -> ::Result<TListIdentifier> {
        let element_type: TType = try!(self.read_byte().and_then(field_type_from_u8));
        let size = try!(self.read_i32());
        let ret = TListIdentifier { element_type: element_type, size: size };
        Ok(ret)
    }

    fn read_list_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_set_begin(&mut self) -> ::Result<TSetIdentifier> {
        let element_type: TType = try!(self.read_byte().and_then(field_type_from_u8));
        let size = try!(self.read_i32());
        let ret = TSetIdentifier { element_type: element_type, size: size };
        Ok(ret)
    }

    fn read_set_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_map_begin(&mut self) -> ::Result<TMapIdentifier> {
        let key_type: TType = try!(self.read_byte().and_then(field_type_from_u8));
        let value_type: TType = try!(self.read_byte().and_then(field_type_from_u8));
        let size = try!(self.read_i32());
        let ret = TMapIdentifier { key_type: key_type, value_type: value_type, size: size };
        Ok(ret)
    }

    fn read_map_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    //
    // utility
    //

    fn write_byte(&mut self, b: u8) -> ::Result<()> {
        self.transport.borrow_mut().write_u8(b).map_err(From::from)
    }

    fn read_byte(&mut self) -> ::Result<u8> {
        self.transport.borrow_mut().read_u8().map_err(From::from)
    }
}

/// Convenience object that can be used to
/// create instances of `TBinaryProtocol`.
pub struct TBinaryProtocolFactory;
impl TProtocolFactory for TBinaryProtocolFactory {
    fn build(&self, transport: Rc<RefCell<Box<TTransport>>>) -> Box<TProtocol> {
        Box::new(TBinaryProtocol { strict: true, transport: transport }) as Box<TProtocol>
    }
}

#[cfg(test)]
mod tests {

    use std::rc::Rc;
    use std::cell::RefCell;

    use super::*;
    use ::protocol::{TMessageIdentifier, TMessageType, TProtocol};
    use ::transport::{TPassThruTransport, TTransport};
    use ::transport::mem::TBufferTransport;

    macro_rules! test_objects {
        () => (
            {
                let mem = Rc::new(RefCell::new(Box::new(TBufferTransport::with_capacity(40, 40))));
                let inner: Box<TTransport> = Box::new(TPassThruTransport { inner: mem.clone() });
                let proto = TBinaryProtocol { strict: true, transport: Rc::new(RefCell::new(inner)) };
                (mem, proto)
            }
        );
    }

    #[test]
    fn must_round_trip_strict_service_call_message_header() {
        let (trans, mut proto) = test_objects!();

        let sent_ident = TMessageIdentifier { name: "test".to_owned(), message_type: TMessageType::Call, sequence_number: 1 };
        assert!(proto.write_message_begin(&sent_ident).is_ok());

        let buf = {
            let m = trans.borrow();
            let written = m.write_buffer();
            let mut b = Vec::with_capacity(written.len());
            b.extend_from_slice(&written);
            b
        };

        let bytes_copied = trans.borrow_mut().set_readable_bytes(&buf);
        assert_eq!(bytes_copied, buf.len());

        let received_ident_result = proto.read_message_begin();
        assert!(received_ident_result.is_ok());
        assert_eq!(received_ident_result.unwrap(), sent_ident);
    }

    #[test]
    fn must_write_message_end() {
        let (trans, mut proto) = test_objects!();
        assert!(proto.write_message_end().is_ok());
        assert_eq!(trans.borrow().write_buffer().len(), 0);
    }
}
