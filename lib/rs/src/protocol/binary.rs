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
use std::io::{Read, Write};
use std::rc::Rc;
use try_from::TryFrom;

use ::{ProtocolError, ProtocolErrorKind};
use ::transport::TTransport;
use super::{TFieldIdentifier, TInputProtocol, TInputProtocolFactory, TListIdentifier,
            TMapIdentifier, TMessageIdentifier, TMessageType};
use super::{TOutputProtocol, TOutputProtocolFactory, TSetIdentifier, TStructIdentifier, TType};

const BINARY_PROTOCOL_VERSION_1: u32 = 0x80010000;

/// Read messages encoded in the Thrift simple binary encoding.
///
/// There are two available modes: `strict` and `non-strict`, where the
/// `non-strict` version does not check for the protocol version in the
/// received message header.
///
/// # Examples
///
/// Create and use a `TBinaryInputProtocol`.
///
/// ```no_run
/// use std::cell::RefCell;
/// use std::rc::Rc;
/// use thrift::protocol::{TBinaryInputProtocol, TInputProtocol};
/// use thrift::transport::{TTcpTransport, TTransport};
///
/// let mut transport = TTcpTransport::new();
/// transport.open("localhost:9090").unwrap();
/// let transport = Rc::new(RefCell::new(Box::new(transport) as Box<TTransport>));
///
/// let mut i_prot = TBinaryInputProtocol::new(transport, true);
///
/// let recvd_bool = i_prot.read_bool().unwrap();
/// let recvd_string = i_prot.read_string().unwrap();
/// ```
pub struct TBinaryInputProtocol {
    strict: bool,
    transport: Rc<RefCell<Box<TTransport>>>,
}

impl TBinaryInputProtocol {
    /// Create a `TBinaryInputProtocol` that reads bytes from `transport`.
    ///
    /// Set `strict` to `true` if all incoming messages contain the protocol
    /// version number in the protocol header.
    pub fn new(transport: Rc<RefCell<Box<TTransport>>>, strict: bool) -> TBinaryInputProtocol {
        TBinaryInputProtocol {
            strict: strict,
            transport: transport,
        }
    }
}

impl TInputProtocol for TBinaryInputProtocol {
    #[cfg_attr(feature = "cargo-clippy", allow(collapsible_if))]
    fn read_message_begin(&mut self) -> ::Result<TMessageIdentifier> {
        let mut first_bytes = vec![0; 4];
        self.transport.borrow_mut().read_exact(&mut first_bytes[..])?;

        // the thrift version header is intentionally negative
        // so the first check we'll do is see if the sign bit is set
        // and if so - assume it's the protocol-version header
        if first_bytes[0] >= 8 {
            // apparently we got a protocol-version header - check
            // it, and if it matches, read the rest of the fields
            if first_bytes[0..2] != [0x80, 0x01] {
                Err(::Error::Protocol(ProtocolError {
                    kind: ProtocolErrorKind::BadVersion,
                    message: format!("received bad version: {:?}", &first_bytes[0..2]),
                }))
            } else {
                let message_type: TMessageType = TryFrom::try_from(first_bytes[3])?;
                let name = self.read_string()?;
                let sequence_number = self.read_i32()?;
                Ok(TMessageIdentifier::new(name, message_type, sequence_number))
            }
        } else {
            // apparently we didn't get a protocol-version header,
            // which happens if the sender is not using the strict protocol
            if self.strict {
                // we're in strict mode however, and that always
                // requires the protocol-version header to be written first
                Err(::Error::Protocol(ProtocolError {
                    kind: ProtocolErrorKind::BadVersion,
                    message: format!("received bad version: {:?}", &first_bytes[0..2]),
                }))
            } else {
                // in the non-strict version the first message field
                // is the message name. strings (byte arrays) are length-prefixed,
                // so we've just read the length in the first 4 bytes
                let name_size = BigEndian::read_i32(&first_bytes) as usize;
                let mut name_buf: Vec<u8> = Vec::with_capacity(name_size);
                self.transport.borrow_mut().read_exact(&mut name_buf)?;
                let name = String::from_utf8(name_buf)?;

                // read the rest of the fields
                let message_type: TMessageType = self.read_byte().and_then(TryFrom::try_from)?;
                let sequence_number = self.read_i32()?;
                Ok(TMessageIdentifier::new(name, message_type, sequence_number))
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
        let field_type_byte = self.read_byte()?;
        let field_type = field_type_from_u8(field_type_byte)?;
        let id = match field_type {
            TType::Stop => Ok(0),
            _ => self.read_i16(),
        }?;
        Ok(TFieldIdentifier::new::<Option<String>, String, i16>(None, field_type, id))
    }

    fn read_field_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_bytes(&mut self) -> ::Result<Vec<u8>> {
        let num_bytes = self.transport.borrow_mut().read_i32::<BigEndian>()? as usize;
        let mut buf = vec![0u8; num_bytes];
        self.transport.borrow_mut().read_exact(&mut buf).map(|_| buf).map_err(From::from)
    }

    fn read_bool(&mut self) -> ::Result<bool> {
        let b = self.read_i8()?;
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
        let bytes = self.read_bytes()?;
        String::from_utf8(bytes).map_err(From::from)
    }

    fn read_list_begin(&mut self) -> ::Result<TListIdentifier> {
        let element_type: TType = self.read_byte().and_then(field_type_from_u8)?;
        let size = self.read_i32()?;
        Ok(TListIdentifier::new(element_type, size))
    }

    fn read_list_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_set_begin(&mut self) -> ::Result<TSetIdentifier> {
        let element_type: TType = self.read_byte().and_then(field_type_from_u8)?;
        let size = self.read_i32()?;
        Ok(TSetIdentifier::new(element_type, size))
    }

    fn read_set_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn read_map_begin(&mut self) -> ::Result<TMapIdentifier> {
        let key_type: TType = self.read_byte().and_then(field_type_from_u8)?;
        let value_type: TType = self.read_byte().and_then(field_type_from_u8)?;
        let size = self.read_i32()?;
        Ok(TMapIdentifier::new(key_type, value_type, size))
    }

    fn read_map_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    // utility
    //

    fn read_byte(&mut self) -> ::Result<u8> {
        self.transport.borrow_mut().read_u8().map_err(From::from)
    }
}

/// Factory for creating instances of `TBinaryInputProtocol`.
#[derive(Default)]
pub struct TBinaryInputProtocolFactory;

impl TBinaryInputProtocolFactory {
    /// Create a `TBinaryInputProtocolFactory`.
    pub fn new() -> TBinaryInputProtocolFactory {
        TBinaryInputProtocolFactory {}
    }
}

impl TInputProtocolFactory for TBinaryInputProtocolFactory {
    fn create(&mut self, transport: Rc<RefCell<Box<TTransport>>>) -> Box<TInputProtocol> {
        Box::new(TBinaryInputProtocol::new(transport, true)) as Box<TInputProtocol>
    }
}

/// Write messages using the Thrift simple binary encoding.
///
/// There are two available modes: `strict` and `non-strict`, where the
/// `strict` version writes the protocol version number in the outgoing message
/// header and the `non-strict` version does not.
///
/// # Examples
///
/// Create and use a `TBinaryOutputProtocol`.
///
/// ```no_run
/// use std::cell::RefCell;
/// use std::rc::Rc;
/// use thrift::protocol::{TBinaryOutputProtocol, TOutputProtocol};
/// use thrift::transport::{TTcpTransport, TTransport};
///
/// let mut transport = TTcpTransport::new();
/// transport.open("localhost:9090").unwrap();
/// let transport = Rc::new(RefCell::new(Box::new(transport) as Box<TTransport>));
///
/// let mut o_prot = TBinaryOutputProtocol::new(transport, true);
///
/// o_prot.write_bool(true).unwrap();
/// o_prot.write_string("test_string").unwrap();
/// ```
pub struct TBinaryOutputProtocol {
    strict: bool,
    transport: Rc<RefCell<Box<TTransport>>>,
}

impl TBinaryOutputProtocol {
    /// Create a `TBinaryOutputProtocol` that writes bytes to `transport`.
    ///
    /// Set `strict` to `true` if all outgoing messages should contain the
    /// protocol version number in the protocol header.
    pub fn new(transport: Rc<RefCell<Box<TTransport>>>, strict: bool) -> TBinaryOutputProtocol {
        TBinaryOutputProtocol {
            strict: strict,
            transport: transport,
        }
    }

    fn write_transport(&mut self, buf: &[u8]) -> ::Result<()> {
        self.transport.borrow_mut().write(buf).map(|_| ()).map_err(From::from)
    }
}

impl TOutputProtocol for TBinaryOutputProtocol {
    fn write_message_begin(&mut self, identifier: &TMessageIdentifier) -> ::Result<()> {
        if self.strict {
            let message_type: u8 = identifier.message_type.into();
            let header = BINARY_PROTOCOL_VERSION_1 | (message_type as u32);
            self.transport.borrow_mut().write_u32::<BigEndian>(header)?;
            self.write_string(&identifier.name)?;
            self.write_i32(identifier.sequence_number)
        } else {
            self.write_string(&identifier.name)?;
            self.write_byte(identifier.message_type.into())?;
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
            return Err(::Error::Protocol(ProtocolError {
                kind: ProtocolErrorKind::Unknown,
                message: format!("cannot write identifier {:?} without sequence number",
                                 &identifier),
            }));
        }

        self.write_byte(field_type_to_u8(identifier.field_type))?;
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
        self.write_i32(b.len() as i32)?;
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
        self.write_byte(field_type_to_u8(identifier.element_type))?;
        self.write_i32(identifier.size)
    }

    fn write_list_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_set_begin(&mut self, identifier: &TSetIdentifier) -> ::Result<()> {
        self.write_byte(field_type_to_u8(identifier.element_type))?;
        self.write_i32(identifier.size)
    }

    fn write_set_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn write_map_begin(&mut self, identifier: &TMapIdentifier) -> ::Result<()> {
        let key_type = identifier.key_type
            .expect("map identifier to write should contain key type");
        self.write_byte(field_type_to_u8(key_type))?;
        let val_type = identifier.value_type
            .expect("map identifier to write should contain value type");
        self.write_byte(field_type_to_u8(val_type))?;
        self.write_i32(identifier.size)
    }

    fn write_map_end(&mut self) -> ::Result<()> {
        Ok(())
    }

    fn flush(&mut self) -> ::Result<()> {
        self.transport.borrow_mut().flush().map_err(From::from)
    }

    // utility
    //

    fn write_byte(&mut self, b: u8) -> ::Result<()> {
        self.transport.borrow_mut().write_u8(b).map_err(From::from)
    }
}

/// Factory for creating instances of `TBinaryOutputProtocol`.
#[derive(Default)]
pub struct TBinaryOutputProtocolFactory;

impl TBinaryOutputProtocolFactory {
    /// Create a `TBinaryOutputProtocolFactory`.
    pub fn new() -> TBinaryOutputProtocolFactory {
        TBinaryOutputProtocolFactory {}
    }
}

impl TOutputProtocolFactory for TBinaryOutputProtocolFactory {
    fn create(&mut self, transport: Rc<RefCell<Box<TTransport>>>) -> Box<TOutputProtocol> {
        Box::new(TBinaryOutputProtocol::new(transport, true)) as Box<TOutputProtocol>
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
        TType::String | TType::Utf7 => 0x0B,
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
        unkn => {
            Err(::Error::Protocol(ProtocolError {
                kind: ProtocolErrorKind::InvalidData,
                message: format!("cannot convert {} to TType", unkn),
            }))
        }
    }
}

#[cfg(test)]
mod tests {

    use std::rc::Rc;
    use std::cell::RefCell;

    use ::protocol::{TFieldIdentifier, TMessageIdentifier, TMessageType, TInputProtocol,
                     TListIdentifier, TMapIdentifier, TOutputProtocol, TSetIdentifier,
                     TStructIdentifier, TType};
    use ::transport::{TPassThruTransport, TTransport};
    use ::transport::mem::TBufferTransport;

    use super::*;

    #[test]
    fn must_write_message_call_begin() {
        let (trans, _, mut o_prot) = test_objects();

        let ident = TMessageIdentifier::new("test", TMessageType::Call, 1);
        assert!(o_prot.write_message_begin(&ident).is_ok());

        let buf = trans.borrow().write_buffer_to_vec();

        let expected: [u8; 16] = [0x80, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x04, 0x74, 0x65,
                                  0x73, 0x74, 0x00, 0x00, 0x00, 0x01];

        assert_eq!(&expected, buf.as_slice());
    }


    #[test]
    fn must_write_message_reply_begin() {
        let (trans, _, mut o_prot) = test_objects();

        let ident = TMessageIdentifier::new("test", TMessageType::Reply, 10);
        assert!(o_prot.write_message_begin(&ident).is_ok());

        let buf = trans.borrow().write_buffer_to_vec();

        let expected: [u8; 16] = [0x80, 0x01, 0x00, 0x02, 0x00, 0x00, 0x00, 0x04, 0x74, 0x65,
                                  0x73, 0x74, 0x00, 0x00, 0x00, 0x0A];

        assert_eq!(&expected, buf.as_slice());
    }

    #[test]
    fn must_round_trip_strict_message_begin() {
        let (trans, mut i_prot, mut o_prot) = test_objects();

        let sent_ident = TMessageIdentifier::new("test", TMessageType::Call, 1);
        assert!(o_prot.write_message_begin(&sent_ident).is_ok());

        trans.borrow_mut().copy_write_buffer_to_read_buffer();

        let received_ident = assert_success!(i_prot.read_message_begin());
        assert_eq!(&received_ident, &sent_ident);
    }

    #[test]
    fn must_write_message_end() {
        assert_no_write(|o| o.write_message_end());
    }

    #[test]
    fn must_write_struct_begin() {
        assert_no_write(|o| o.write_struct_begin(&TStructIdentifier::new("foo")));
    }

    #[test]
    fn must_write_struct_end() {
        assert_no_write(|o| o.write_struct_end());
    }

    #[test]
    fn must_write_field_begin() {
        let (trans, _, mut o_prot) = test_objects();

        assert!(o_prot.write_field_begin(&TFieldIdentifier::new("some_field", TType::String, 22))
            .is_ok());

        let expected: [u8; 3] = [0x0B, 0x00, 0x16];
        let buf = trans.borrow().write_buffer_to_vec();
        assert_eq!(&expected, buf.as_slice());
    }

    #[test]
    fn must_round_trip_field_begin() {
        let (trans, mut i_prot, mut o_prot) = test_objects();

        let sent_field_ident = TFieldIdentifier::new("foo", TType::I64, 20);
        assert!(o_prot.write_field_begin(&sent_field_ident).is_ok());

        trans.borrow_mut().copy_write_buffer_to_read_buffer();

        let expected_ident = TFieldIdentifier {
            name: None,
            field_type: TType::I64,
            id: Some(20),
        }; // no name
        let received_ident = assert_success!(i_prot.read_field_begin());
        assert_eq!(&received_ident, &expected_ident);
    }

    #[test]
    fn must_write_stop_field() {
        let (trans, _, mut o_prot) = test_objects();

        assert!(o_prot.write_field_stop().is_ok());

        let expected: [u8; 1] = [0x00];
        let buf = trans.borrow().write_buffer_to_vec();
        assert_eq!(&expected, buf.as_slice());
    }

    #[test]
    fn must_round_trip_field_stop() {
        let (trans, mut i_prot, mut o_prot) = test_objects();

        assert!(o_prot.write_field_stop().is_ok());

        trans.borrow_mut().copy_write_buffer_to_read_buffer();

        let expected_ident = TFieldIdentifier {
            name: None,
            field_type: TType::Stop,
            id: Some(0),
        }; // we get id 0

        let received_ident = assert_success!(i_prot.read_field_begin());
        assert_eq!(&received_ident, &expected_ident);
    }

    #[test]
    fn must_write_field_end() {
        assert_no_write(|o| o.write_field_end());
    }

    #[test]
    fn must_write_list_begin() {
        let (trans, _, mut o_prot) = test_objects();

        assert!(o_prot.write_list_begin(&TListIdentifier::new(TType::Bool, 5)).is_ok());

        let expected: [u8; 5] = [0x02, 0x00, 0x00, 0x00, 0x05];
        let buf = trans.borrow().write_buffer_to_vec();
        assert_eq!(&expected, buf.as_slice());
    }

    #[test]
    fn must_round_trip_list_begin() {
        let (trans, mut i_prot, mut o_prot) = test_objects();

        let ident = TListIdentifier::new(TType::List, 900);
        assert!(o_prot.write_list_begin(&ident).is_ok());

        trans.borrow_mut().copy_write_buffer_to_read_buffer();

        let received_ident = assert_success!(i_prot.read_list_begin());
        assert_eq!(&received_ident, &ident);
    }

    #[test]
    fn must_write_list_end() {
        assert_no_write(|o| o.write_list_end());
    }

    #[test]
    fn must_write_set_begin() {
        let (trans, _, mut o_prot) = test_objects();

        assert!(o_prot.write_set_begin(&TSetIdentifier::new(TType::I16, 7)).is_ok());

        let expected: [u8; 5] = [0x06, 0x00, 0x00, 0x00, 0x07];
        let buf = trans.borrow().write_buffer_to_vec();
        assert_eq!(&expected, buf.as_slice());
    }

    #[test]
    fn must_round_trip_set_begin() {
        let (trans, mut i_prot, mut o_prot) = test_objects();

        let ident = TSetIdentifier::new(TType::I64, 2000);
        assert!(o_prot.write_set_begin(&ident).is_ok());

        trans.borrow_mut().copy_write_buffer_to_read_buffer();

        let received_ident_result = i_prot.read_set_begin();
        assert!(received_ident_result.is_ok());
        assert_eq!(&received_ident_result.unwrap(), &ident);
    }

    #[test]
    fn must_write_set_end() {
        assert_no_write(|o| o.write_set_end());
    }

    #[test]
    fn must_write_map_begin() {
        let (trans, _, mut o_prot) = test_objects();

        assert!(o_prot.write_map_begin(&TMapIdentifier::new(TType::I64, TType::Struct, 32))
            .is_ok());

        let expected: [u8; 6] = [0x0A, 0x0C, 0x00, 0x00, 0x00, 0x20];
        let buf = trans.borrow().write_buffer_to_vec();
        assert_eq!(&expected, buf.as_slice());
    }

    #[test]
    fn must_round_trip_map_begin() {
        let (trans, mut i_prot, mut o_prot) = test_objects();

        let ident = TMapIdentifier::new(TType::Map, TType::Set, 100);
        assert!(o_prot.write_map_begin(&ident).is_ok());

        trans.borrow_mut().copy_write_buffer_to_read_buffer();

        let received_ident = assert_success!(i_prot.read_map_begin());
        assert_eq!(&received_ident, &ident);
    }

    #[test]
    fn must_write_map_end() {
        assert_no_write(|o| o.write_map_end());
    }

    #[test]
    fn must_write_bool_true() {
        let (trans, _, mut o_prot) = test_objects();

        assert!(o_prot.write_bool(true).is_ok());

        let expected: [u8; 1] = [0x01];
        let buf = trans.borrow().write_buffer_to_vec();
        assert_eq!(&expected, buf.as_slice());
    }

    #[test]
    fn must_write_bool_false() {
        let (trans, _, mut o_prot) = test_objects();

        assert!(o_prot.write_bool(false).is_ok());

        let expected: [u8; 1] = [0x00];
        let buf = trans.borrow().write_buffer_to_vec();
        assert_eq!(&expected, buf.as_slice());
    }

    #[test]
    fn must_read_bool_true() {
        let (trans, mut i_prot, _) = test_objects();

        trans.borrow_mut().set_readable_bytes(&[0x01]);

        let read_bool = assert_success!(i_prot.read_bool());
        assert_eq!(read_bool, true);
    }

    #[test]
    fn must_read_bool_false() {
        let (trans, mut i_prot, _) = test_objects();

        trans.borrow_mut().set_readable_bytes(&[0x00]);

        let read_bool = assert_success!(i_prot.read_bool());
        assert_eq!(read_bool, false);
    }

    #[test]
    fn must_allow_any_non_zero_value_to_be_interpreted_as_bool_true() {
        let (trans, mut i_prot, _) = test_objects();

        trans.borrow_mut().set_readable_bytes(&[0xAC]);

        let read_bool = assert_success!(i_prot.read_bool());
        assert_eq!(read_bool, true);
    }

    #[test]
    fn must_write_bytes() {
        let (trans, _, mut o_prot) = test_objects();

        let bytes: [u8; 10] = [0x0A, 0xCC, 0xD1, 0x84, 0x99, 0x12, 0xAB, 0xBB, 0x45, 0xDF];

        assert!(o_prot.write_bytes(&bytes).is_ok());

        let buf = trans.borrow().write_buffer_to_vec();
        assert_eq!(&buf[0..4], [0x00, 0x00, 0x00, 0x0A]); // length
        assert_eq!(&buf[4..], bytes); // actual bytes
    }

    #[test]
    fn must_round_trip_bytes() {
        let (trans, mut i_prot, mut o_prot) = test_objects();

        let bytes: [u8; 25] = [0x20, 0xFD, 0x18, 0x84, 0x99, 0x12, 0xAB, 0xBB, 0x45, 0xDF, 0x34,
                               0xDC, 0x98, 0xA4, 0x6D, 0xF3, 0x99, 0xB4, 0xB7, 0xD4, 0x9C, 0xA5,
                               0xB3, 0xC9, 0x88];

        assert!(o_prot.write_bytes(&bytes).is_ok());

        trans.borrow_mut().copy_write_buffer_to_read_buffer();

        let received_bytes = assert_success!(i_prot.read_bytes());
        assert_eq!(&received_bytes, &bytes);
    }

    fn test_objects
        ()
        -> (Rc<RefCell<Box<TBufferTransport>>>, TBinaryInputProtocol, TBinaryOutputProtocol)
    {
        let mem = Rc::new(RefCell::new(Box::new(TBufferTransport::with_capacity(40, 40))));

        let inner: Box<TTransport> = Box::new(TPassThruTransport { inner: mem.clone() });
        let inner = Rc::new(RefCell::new(inner));

        let i_prot = TBinaryInputProtocol::new(inner.clone(), true);
        let o_prot = TBinaryOutputProtocol::new(inner.clone(), true);

        (mem, i_prot, o_prot)
    }

    fn assert_no_write<F: FnMut(&mut TBinaryOutputProtocol) -> ::Result<()>>(mut write_fn: F) {
        let (trans, _, mut o_prot) = test_objects();
        assert!(write_fn(&mut o_prot).is_ok());
        assert_eq!(trans.borrow().write_buffer_as_ref().len(), 0);
    }
}
