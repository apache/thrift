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

//! Types used to send and receive primitives between a Thrift client and server.
//!
//! # Examples
//!
//! Create and use a `TInputProtocol`.
//!
//! ```no_run
//! use thrift::protocol::{TBinaryInputProtocol, TInputProtocol};
//! use thrift::transport::TTcpChannel;
//!
//! // create the I/O channel
//! let mut channel = TTcpChannel::new();
//! channel.open("127.0.0.1:9090").unwrap();
//!
//! // create the protocol to decode bytes into types
//! let mut protocol = TBinaryInputProtocol::new(channel, true);
//!
//! // read types from the wire
//! let field_identifier = protocol.read_field_begin().unwrap();
//! let field_contents = protocol.read_string().unwrap();
//! let field_end = protocol.read_field_end().unwrap();
//! ```
//!
//! Create and use a `TOutputProtocol`.
//!
//! ```no_run
//! use thrift::protocol::{TBinaryOutputProtocol, TFieldIdentifier, TOutputProtocol, TType};
//! use thrift::transport::TTcpChannel;
//!
//! // create the I/O channel
//! let mut channel = TTcpChannel::new();
//! channel.open("127.0.0.1:9090").unwrap();
//!
//! // create the protocol to encode types into bytes
//! let mut protocol = TBinaryOutputProtocol::new(channel, true);
//!
//! // write types
//! protocol.write_field_begin(&TFieldIdentifier::new("string_thing", TType::String, 1)).unwrap();
//! protocol.write_string("foo").unwrap();
//! protocol.write_field_end().unwrap();
//! ```

use std::convert::{From, TryFrom};
use std::fmt;
use std::fmt::{Display, Formatter};

use crate::transport::{TReadTransport, TWriteTransport};
use crate::{ProtocolError, ProtocolErrorKind};

#[cfg(test)]
macro_rules! assert_eq_written_bytes {
    ($o_prot:ident, $expected_bytes:ident) => {{
        assert_eq!($o_prot.transport.write_bytes(), &$expected_bytes);
    }};
}

// FIXME: should take both read and write
#[cfg(test)]
macro_rules! copy_write_buffer_to_read_buffer {
    ($o_prot:ident) => {{
        $o_prot.transport.copy_write_buffer_to_read_buffer();
    }};
}

#[cfg(test)]
macro_rules! set_readable_bytes {
    ($i_prot:ident, $bytes:expr) => {
        $i_prot.transport.set_readable_bytes($bytes);
    };
}

mod binary;
mod compact;
mod multiplexed;
mod stored;

pub use self::binary::{
    TBinaryInputProtocol, TBinaryInputProtocolFactory, TBinaryOutputProtocol,
    TBinaryOutputProtocolFactory,
};
pub use self::compact::{
    TCompactInputProtocol, TCompactInputProtocolFactory, TCompactOutputProtocol,
    TCompactOutputProtocolFactory,
};
pub use self::multiplexed::TMultiplexedOutputProtocol;
pub use self::stored::TStoredInputProtocol;

/// Reads and writes the struct to Thrift protocols.
///
/// It is implemented in generated code for Thrift `struct`, `union`, and `enum` types.
pub trait TSerializable: Sized {
    fn read_from_in_protocol(i_prot: &mut dyn TInputProtocol) -> crate::Result<Self>;
    fn write_to_out_protocol(&self, o_prot: &mut dyn TOutputProtocol) -> crate::Result<()>;
}

// Default maximum depth to which `TInputProtocol::skip` will skip a Thrift
// field. A default is necessary because Thrift structs or collections may
// contain nested structs and collections, which could result in indefinite
// recursion.
const MAXIMUM_SKIP_DEPTH: i8 = 64;

/// Converts a stream of bytes into Thrift identifiers, primitives,
/// containers, or structs.
///
/// This trait does not deal with higher-level Thrift concepts like structs or
/// exceptions - only with primitives and message or container boundaries. Once
/// bytes are read they are deserialized and an identifier (for example
/// `TMessageIdentifier`) or a primitive is returned.
///
/// All methods return a `thrift::Result`. If an `Err` is returned the protocol
/// instance and its underlying transport should be terminated.
///
/// # Examples
///
/// Create and use a `TInputProtocol`
///
/// ```no_run
/// use thrift::protocol::{TBinaryInputProtocol, TInputProtocol};
/// use thrift::transport::TTcpChannel;
///
/// let mut channel = TTcpChannel::new();
/// channel.open("127.0.0.1:9090").unwrap();
///
/// let mut protocol = TBinaryInputProtocol::new(channel, true);
///
/// let field_identifier = protocol.read_field_begin().unwrap();
/// let field_contents = protocol.read_string().unwrap();
/// let field_end = protocol.read_field_end().unwrap();
/// ```
pub trait TInputProtocol {
    /// Read the beginning of a Thrift message.
    fn read_message_begin(&mut self) -> crate::Result<TMessageIdentifier>;
    /// Read the end of a Thrift message.
    fn read_message_end(&mut self) -> crate::Result<()>;
    /// Read the beginning of a Thrift struct.
    fn read_struct_begin(&mut self) -> crate::Result<Option<TStructIdentifier>>;
    /// Read the end of a Thrift struct.
    fn read_struct_end(&mut self) -> crate::Result<()>;
    /// Read the beginning of a Thrift struct field.
    fn read_field_begin(&mut self) -> crate::Result<TFieldIdentifier>;
    /// Read the end of a Thrift struct field.
    fn read_field_end(&mut self) -> crate::Result<()>;
    /// Read a bool.
    fn read_bool(&mut self) -> crate::Result<bool>;
    /// Read a fixed-length byte array.
    fn read_bytes(&mut self) -> crate::Result<Vec<u8>>;
    /// Read a word.
    fn read_i8(&mut self) -> crate::Result<i8>;
    /// Read a 16-bit signed integer.
    fn read_i16(&mut self) -> crate::Result<i16>;
    /// Read a 32-bit signed integer.
    fn read_i32(&mut self) -> crate::Result<i32>;
    /// Read a 64-bit signed integer.
    fn read_i64(&mut self) -> crate::Result<i64>;
    /// Read a 64-bit float.
    fn read_double(&mut self) -> crate::Result<f64>;
    /// Read a fixed-length string (not null terminated).
    fn read_string(&mut self) -> crate::Result<String>;
    /// Read the beginning of a list.
    fn read_list_begin(&mut self) -> crate::Result<TListIdentifier>;
    /// Read the end of a list.
    fn read_list_end(&mut self) -> crate::Result<()>;
    /// Read the beginning of a set.
    fn read_set_begin(&mut self) -> crate::Result<TSetIdentifier>;
    /// Read the end of a set.
    fn read_set_end(&mut self) -> crate::Result<()>;
    /// Read the beginning of a map.
    fn read_map_begin(&mut self) -> crate::Result<TMapIdentifier>;
    /// Read the end of a map.
    fn read_map_end(&mut self) -> crate::Result<()>;
    /// Skip a field with type `field_type` recursively until the default
    /// maximum skip depth is reached.
    fn skip(&mut self, field_type: TType) -> crate::Result<()> {
        self.skip_till_depth(field_type, MAXIMUM_SKIP_DEPTH)
    }
    /// Skip a field with type `field_type` recursively up to `depth` levels.
    fn skip_till_depth(&mut self, field_type: TType, depth: i8) -> crate::Result<()> {
        if depth == 0 {
            return Err(crate::Error::Protocol(ProtocolError {
                kind: ProtocolErrorKind::DepthLimit,
                message: format!("cannot parse past {:?}", field_type),
            }));
        }

        match field_type {
            TType::Bool => self.read_bool().map(|_| ()),
            TType::I08 => self.read_i8().map(|_| ()),
            TType::I16 => self.read_i16().map(|_| ()),
            TType::I32 => self.read_i32().map(|_| ()),
            TType::I64 => self.read_i64().map(|_| ()),
            TType::Double => self.read_double().map(|_| ()),
            TType::String => self.read_string().map(|_| ()),
            TType::Struct => {
                self.read_struct_begin()?;
                loop {
                    let field_ident = self.read_field_begin()?;
                    if field_ident.field_type == TType::Stop {
                        break;
                    }
                    self.skip_till_depth(field_ident.field_type, depth - 1)?;
                }
                self.read_struct_end()
            }
            TType::List => {
                let list_ident = self.read_list_begin()?;
                for _ in 0..list_ident.size {
                    self.skip_till_depth(list_ident.element_type, depth - 1)?;
                }
                self.read_list_end()
            }
            TType::Set => {
                let set_ident = self.read_set_begin()?;
                for _ in 0..set_ident.size {
                    self.skip_till_depth(set_ident.element_type, depth - 1)?;
                }
                self.read_set_end()
            }
            TType::Map => {
                let map_ident = self.read_map_begin()?;
                for _ in 0..map_ident.size {
                    let key_type = map_ident
                        .key_type
                        .expect("non-zero sized map should contain key type");
                    let val_type = map_ident
                        .value_type
                        .expect("non-zero sized map should contain value type");
                    self.skip_till_depth(key_type, depth - 1)?;
                    self.skip_till_depth(val_type, depth - 1)?;
                }
                self.read_map_end()
            }
            u => Err(crate::Error::Protocol(ProtocolError {
                kind: ProtocolErrorKind::Unknown,
                message: format!("cannot skip field type {:?}", &u),
            })),
        }
    }

    // utility (DO NOT USE IN GENERATED CODE!!!!)
    //

    /// Read an unsigned byte.
    ///
    /// This method should **never** be used in generated code.
    fn read_byte(&mut self) -> crate::Result<u8>;
}

/// Converts Thrift identifiers, primitives, containers or structs into a
/// stream of bytes.
///
/// This trait does not deal with higher-level Thrift concepts like structs or
/// exceptions - only with primitives and message or container boundaries.
/// Write methods take an identifier (for example, `TMessageIdentifier`) or a
/// primitive. Any or all of the fields in an identifier may be omitted when
/// writing to the transport. Write methods may even be noops. All of this is
/// transparent to the caller; as long as a matching `TInputProtocol`
/// implementation is used, received messages will be decoded correctly.
///
/// All methods return a `thrift::Result`. If an `Err` is returned the protocol
/// instance and its underlying transport should be terminated.
///
/// # Examples
///
/// Create and use a `TOutputProtocol`
///
/// ```no_run
/// use thrift::protocol::{TBinaryOutputProtocol, TFieldIdentifier, TOutputProtocol, TType};
/// use thrift::transport::TTcpChannel;
///
/// let mut channel = TTcpChannel::new();
/// channel.open("127.0.0.1:9090").unwrap();
///
/// let mut protocol = TBinaryOutputProtocol::new(channel, true);
///
/// protocol.write_field_begin(&TFieldIdentifier::new("string_thing", TType::String, 1)).unwrap();
/// protocol.write_string("foo").unwrap();
/// protocol.write_field_end().unwrap();
/// ```
pub trait TOutputProtocol {
    /// Write the beginning of a Thrift message.
    fn write_message_begin(&mut self, identifier: &TMessageIdentifier) -> crate::Result<()>;
    /// Write the end of a Thrift message.
    fn write_message_end(&mut self) -> crate::Result<()>;
    /// Write the beginning of a Thrift struct.
    fn write_struct_begin(&mut self, identifier: &TStructIdentifier) -> crate::Result<()>;
    /// Write the end of a Thrift struct.
    fn write_struct_end(&mut self) -> crate::Result<()>;
    /// Write the beginning of a Thrift field.
    fn write_field_begin(&mut self, identifier: &TFieldIdentifier) -> crate::Result<()>;
    /// Write the end of a Thrift field.
    fn write_field_end(&mut self) -> crate::Result<()>;
    /// Write a STOP field indicating that all the fields in a struct have been
    /// written.
    fn write_field_stop(&mut self) -> crate::Result<()>;
    /// Write a bool.
    fn write_bool(&mut self, b: bool) -> crate::Result<()>;
    /// Write a fixed-length byte array.
    fn write_bytes(&mut self, b: &[u8]) -> crate::Result<()>;
    /// Write an 8-bit signed integer.
    fn write_i8(&mut self, i: i8) -> crate::Result<()>;
    /// Write a 16-bit signed integer.
    fn write_i16(&mut self, i: i16) -> crate::Result<()>;
    /// Write a 32-bit signed integer.
    fn write_i32(&mut self, i: i32) -> crate::Result<()>;
    /// Write a 64-bit signed integer.
    fn write_i64(&mut self, i: i64) -> crate::Result<()>;
    /// Write a 64-bit float.
    fn write_double(&mut self, d: f64) -> crate::Result<()>;
    /// Write a fixed-length string.
    fn write_string(&mut self, s: &str) -> crate::Result<()>;
    /// Write the beginning of a list.
    fn write_list_begin(&mut self, identifier: &TListIdentifier) -> crate::Result<()>;
    /// Write the end of a list.
    fn write_list_end(&mut self) -> crate::Result<()>;
    /// Write the beginning of a set.
    fn write_set_begin(&mut self, identifier: &TSetIdentifier) -> crate::Result<()>;
    /// Write the end of a set.
    fn write_set_end(&mut self) -> crate::Result<()>;
    /// Write the beginning of a map.
    fn write_map_begin(&mut self, identifier: &TMapIdentifier) -> crate::Result<()>;
    /// Write the end of a map.
    fn write_map_end(&mut self) -> crate::Result<()>;
    /// Flush buffered bytes to the underlying transport.
    fn flush(&mut self) -> crate::Result<()>;

    // utility (DO NOT USE IN GENERATED CODE!!!!)
    //

    /// Write an unsigned byte.
    ///
    /// This method should **never** be used in generated code.
    fn write_byte(&mut self, b: u8) -> crate::Result<()>; // FIXME: REMOVE
}

impl<P> TInputProtocol for Box<P>
where
    P: TInputProtocol + ?Sized,
{
    fn read_message_begin(&mut self) -> crate::Result<TMessageIdentifier> {
        (**self).read_message_begin()
    }

    fn read_message_end(&mut self) -> crate::Result<()> {
        (**self).read_message_end()
    }

    fn read_struct_begin(&mut self) -> crate::Result<Option<TStructIdentifier>> {
        (**self).read_struct_begin()
    }

    fn read_struct_end(&mut self) -> crate::Result<()> {
        (**self).read_struct_end()
    }

    fn read_field_begin(&mut self) -> crate::Result<TFieldIdentifier> {
        (**self).read_field_begin()
    }

    fn read_field_end(&mut self) -> crate::Result<()> {
        (**self).read_field_end()
    }

    fn read_bool(&mut self) -> crate::Result<bool> {
        (**self).read_bool()
    }

    fn read_bytes(&mut self) -> crate::Result<Vec<u8>> {
        (**self).read_bytes()
    }

    fn read_i8(&mut self) -> crate::Result<i8> {
        (**self).read_i8()
    }

    fn read_i16(&mut self) -> crate::Result<i16> {
        (**self).read_i16()
    }

    fn read_i32(&mut self) -> crate::Result<i32> {
        (**self).read_i32()
    }

    fn read_i64(&mut self) -> crate::Result<i64> {
        (**self).read_i64()
    }

    fn read_double(&mut self) -> crate::Result<f64> {
        (**self).read_double()
    }

    fn read_string(&mut self) -> crate::Result<String> {
        (**self).read_string()
    }

    fn read_list_begin(&mut self) -> crate::Result<TListIdentifier> {
        (**self).read_list_begin()
    }

    fn read_list_end(&mut self) -> crate::Result<()> {
        (**self).read_list_end()
    }

    fn read_set_begin(&mut self) -> crate::Result<TSetIdentifier> {
        (**self).read_set_begin()
    }

    fn read_set_end(&mut self) -> crate::Result<()> {
        (**self).read_set_end()
    }

    fn read_map_begin(&mut self) -> crate::Result<TMapIdentifier> {
        (**self).read_map_begin()
    }

    fn read_map_end(&mut self) -> crate::Result<()> {
        (**self).read_map_end()
    }

    fn read_byte(&mut self) -> crate::Result<u8> {
        (**self).read_byte()
    }
}

impl<P> TOutputProtocol for Box<P>
where
    P: TOutputProtocol + ?Sized,
{
    fn write_message_begin(&mut self, identifier: &TMessageIdentifier) -> crate::Result<()> {
        (**self).write_message_begin(identifier)
    }

    fn write_message_end(&mut self) -> crate::Result<()> {
        (**self).write_message_end()
    }

    fn write_struct_begin(&mut self, identifier: &TStructIdentifier) -> crate::Result<()> {
        (**self).write_struct_begin(identifier)
    }

    fn write_struct_end(&mut self) -> crate::Result<()> {
        (**self).write_struct_end()
    }

    fn write_field_begin(&mut self, identifier: &TFieldIdentifier) -> crate::Result<()> {
        (**self).write_field_begin(identifier)
    }

    fn write_field_end(&mut self) -> crate::Result<()> {
        (**self).write_field_end()
    }

    fn write_field_stop(&mut self) -> crate::Result<()> {
        (**self).write_field_stop()
    }

    fn write_bool(&mut self, b: bool) -> crate::Result<()> {
        (**self).write_bool(b)
    }

    fn write_bytes(&mut self, b: &[u8]) -> crate::Result<()> {
        (**self).write_bytes(b)
    }

    fn write_i8(&mut self, i: i8) -> crate::Result<()> {
        (**self).write_i8(i)
    }

    fn write_i16(&mut self, i: i16) -> crate::Result<()> {
        (**self).write_i16(i)
    }

    fn write_i32(&mut self, i: i32) -> crate::Result<()> {
        (**self).write_i32(i)
    }

    fn write_i64(&mut self, i: i64) -> crate::Result<()> {
        (**self).write_i64(i)
    }

    fn write_double(&mut self, d: f64) -> crate::Result<()> {
        (**self).write_double(d)
    }

    fn write_string(&mut self, s: &str) -> crate::Result<()> {
        (**self).write_string(s)
    }

    fn write_list_begin(&mut self, identifier: &TListIdentifier) -> crate::Result<()> {
        (**self).write_list_begin(identifier)
    }

    fn write_list_end(&mut self) -> crate::Result<()> {
        (**self).write_list_end()
    }

    fn write_set_begin(&mut self, identifier: &TSetIdentifier) -> crate::Result<()> {
        (**self).write_set_begin(identifier)
    }

    fn write_set_end(&mut self) -> crate::Result<()> {
        (**self).write_set_end()
    }

    fn write_map_begin(&mut self, identifier: &TMapIdentifier) -> crate::Result<()> {
        (**self).write_map_begin(identifier)
    }

    fn write_map_end(&mut self) -> crate::Result<()> {
        (**self).write_map_end()
    }

    fn flush(&mut self) -> crate::Result<()> {
        (**self).flush()
    }

    fn write_byte(&mut self, b: u8) -> crate::Result<()> {
        (**self).write_byte(b)
    }
}

/// Helper type used by servers to create `TInputProtocol` instances for
/// accepted client connections.
///
/// # Examples
///
/// Create a `TInputProtocolFactory` and use it to create a `TInputProtocol`.
///
/// ```no_run
/// use thrift::protocol::{TBinaryInputProtocolFactory, TInputProtocolFactory};
/// use thrift::transport::TTcpChannel;
///
/// let mut channel = TTcpChannel::new();
/// channel.open("127.0.0.1:9090").unwrap();
///
/// let factory = TBinaryInputProtocolFactory::new();
/// let protocol = factory.create(Box::new(channel));
/// ```
pub trait TInputProtocolFactory {
    // Create a `TInputProtocol` that reads bytes from `transport`.
    fn create(&self, transport: Box<dyn TReadTransport + Send>) -> Box<dyn TInputProtocol + Send>;
}

impl<T> TInputProtocolFactory for Box<T>
where
    T: TInputProtocolFactory + ?Sized,
{
    fn create(&self, transport: Box<dyn TReadTransport + Send>) -> Box<dyn TInputProtocol + Send> {
        (**self).create(transport)
    }
}

/// Helper type used by servers to create `TOutputProtocol` instances for
/// accepted client connections.
///
/// # Examples
///
/// Create a `TOutputProtocolFactory` and use it to create a `TOutputProtocol`.
///
/// ```no_run
/// use thrift::protocol::{TBinaryOutputProtocolFactory, TOutputProtocolFactory};
/// use thrift::transport::TTcpChannel;
///
/// let mut channel = TTcpChannel::new();
/// channel.open("127.0.0.1:9090").unwrap();
///
/// let factory = TBinaryOutputProtocolFactory::new();
/// let protocol = factory.create(Box::new(channel));
/// ```
pub trait TOutputProtocolFactory {
    /// Create a `TOutputProtocol` that writes bytes to `transport`.
    fn create(&self, transport: Box<dyn TWriteTransport + Send>)
        -> Box<dyn TOutputProtocol + Send>;
}

impl<T> TOutputProtocolFactory for Box<T>
where
    T: TOutputProtocolFactory + ?Sized,
{
    fn create(
        &self,
        transport: Box<dyn TWriteTransport + Send>,
    ) -> Box<dyn TOutputProtocol + Send> {
        (**self).create(transport)
    }
}

/// Thrift message identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TMessageIdentifier {
    /// Service call the message is associated with.
    pub name: String,
    /// Message type.
    pub message_type: TMessageType,
    /// Ordered sequence number identifying the message.
    pub sequence_number: i32,
}

impl TMessageIdentifier {
    /// Create a `TMessageIdentifier` for a Thrift service-call named `name`
    /// with message type `message_type` and sequence number `sequence_number`.
    pub fn new<S: Into<String>>(
        name: S,
        message_type: TMessageType,
        sequence_number: i32,
    ) -> TMessageIdentifier {
        TMessageIdentifier {
            name: name.into(),
            message_type,
            sequence_number,
        }
    }
}

/// Thrift struct identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TStructIdentifier {
    /// Name of the encoded Thrift struct.
    pub name: String,
}

impl TStructIdentifier {
    /// Create a `TStructIdentifier` for a struct named `name`.
    pub fn new<S: Into<String>>(name: S) -> TStructIdentifier {
        TStructIdentifier { name: name.into() }
    }
}

/// Thrift field identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TFieldIdentifier {
    /// Name of the Thrift field.
    ///
    /// `None` if it's not sent over the wire.
    pub name: Option<String>,
    /// Field type.
    ///
    /// This may be a primitive, container, or a struct.
    pub field_type: TType,
    /// Thrift field id.
    ///
    /// `None` only if `field_type` is `TType::Stop`.
    pub id: Option<i16>,
}

impl TFieldIdentifier {
    /// Create a `TFieldIdentifier` for a field named `name` with type
    /// `field_type` and field id `id`.
    ///
    /// `id` should be `None` if `field_type` is `TType::Stop`.
    pub fn new<N, S, I>(name: N, field_type: TType, id: I) -> TFieldIdentifier
    where
        N: Into<Option<S>>,
        S: Into<String>,
        I: Into<Option<i16>>,
    {
        TFieldIdentifier {
            name: name.into().map(|n| n.into()),
            field_type,
            id: id.into(),
        }
    }
}

/// Thrift list identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TListIdentifier {
    /// Type of the elements in the list.
    pub element_type: TType,
    /// Number of elements in the list.
    pub size: i32,
}

impl TListIdentifier {
    /// Create a `TListIdentifier` for a list with `size` elements of type
    /// `element_type`.
    pub fn new(element_type: TType, size: i32) -> TListIdentifier {
        TListIdentifier { element_type, size }
    }
}

/// Thrift set identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TSetIdentifier {
    /// Type of the elements in the set.
    pub element_type: TType,
    /// Number of elements in the set.
    pub size: i32,
}

impl TSetIdentifier {
    /// Create a `TSetIdentifier` for a set with `size` elements of type
    /// `element_type`.
    pub fn new(element_type: TType, size: i32) -> TSetIdentifier {
        TSetIdentifier { element_type, size }
    }
}

/// Thrift map identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TMapIdentifier {
    /// Map key type.
    pub key_type: Option<TType>,
    /// Map value type.
    pub value_type: Option<TType>,
    /// Number of entries in the map.
    pub size: i32,
}

impl TMapIdentifier {
    /// Create a `TMapIdentifier` for a map with `size` entries of type
    /// `key_type -> value_type`.
    pub fn new<K, V>(key_type: K, value_type: V, size: i32) -> TMapIdentifier
    where
        K: Into<Option<TType>>,
        V: Into<Option<TType>>,
    {
        TMapIdentifier {
            key_type: key_type.into(),
            value_type: value_type.into(),
            size,
        }
    }
}

/// Thrift message types.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TMessageType {
    /// Service-call request.
    Call,
    /// Service-call response.
    Reply,
    /// Unexpected error in the remote service.
    Exception,
    /// One-way service-call request (no response is expected).
    OneWay,
}

impl Display for TMessageType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            TMessageType::Call => write!(f, "Call"),
            TMessageType::Reply => write!(f, "Reply"),
            TMessageType::Exception => write!(f, "Exception"),
            TMessageType::OneWay => write!(f, "OneWay"),
        }
    }
}

impl From<TMessageType> for u8 {
    fn from(message_type: TMessageType) -> Self {
        match message_type {
            TMessageType::Call => 0x01,
            TMessageType::Reply => 0x02,
            TMessageType::Exception => 0x03,
            TMessageType::OneWay => 0x04,
        }
    }
}

impl TryFrom<u8> for TMessageType {
    type Error = crate::Error;
    fn try_from(b: u8) -> Result<Self, Self::Error> {
        match b {
            0x01 => Ok(TMessageType::Call),
            0x02 => Ok(TMessageType::Reply),
            0x03 => Ok(TMessageType::Exception),
            0x04 => Ok(TMessageType::OneWay),
            unkn => Err(crate::Error::Protocol(ProtocolError {
                kind: ProtocolErrorKind::InvalidData,
                message: format!("cannot convert {} to TMessageType", unkn),
            })),
        }
    }
}

/// Thrift struct-field types.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TType {
    /// Indicates that there are no more serialized fields in this Thrift struct.
    Stop,
    /// Void (`()`) field.
    Void,
    /// Boolean.
    Bool,
    /// Signed 8-bit int.
    I08,
    /// Double-precision number.
    Double,
    /// Signed 16-bit int.
    I16,
    /// Signed 32-bit int.
    I32,
    /// Signed 64-bit int.
    I64,
    /// UTF-8 string.
    String,
    /// UTF-7 string. *Unsupported*.
    Utf7,
    /// Thrift struct.
    Struct,
    /// Map.
    Map,
    /// Set.
    Set,
    /// List.
    List,
    /// UTF-8 string.
    Utf8,
    /// UTF-16 string. *Unsupported*.
    Utf16,
}

impl Display for TType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            TType::Stop => write!(f, "STOP"),
            TType::Void => write!(f, "void"),
            TType::Bool => write!(f, "bool"),
            TType::I08 => write!(f, "i08"),
            TType::Double => write!(f, "double"),
            TType::I16 => write!(f, "i16"),
            TType::I32 => write!(f, "i32"),
            TType::I64 => write!(f, "i64"),
            TType::String => write!(f, "string"),
            TType::Utf7 => write!(f, "UTF7"),
            TType::Struct => write!(f, "struct"),
            TType::Map => write!(f, "map"),
            TType::Set => write!(f, "set"),
            TType::List => write!(f, "list"),
            TType::Utf8 => write!(f, "UTF8"),
            TType::Utf16 => write!(f, "UTF16"),
        }
    }
}

/// Compare the expected message sequence number `expected` with the received
/// message sequence number `actual`.
///
/// Return `()` if `actual == expected`, `Err` otherwise.
pub fn verify_expected_sequence_number(expected: i32, actual: i32) -> crate::Result<()> {
    if expected == actual {
        Ok(())
    } else {
        Err(crate::Error::Application(crate::ApplicationError {
            kind: crate::ApplicationErrorKind::BadSequenceId,
            message: format!("expected {} got {}", expected, actual),
        }))
    }
}

/// Compare the expected service-call name `expected` with the received
/// service-call name `actual`.
///
/// Return `()` if `actual == expected`, `Err` otherwise.
pub fn verify_expected_service_call(expected: &str, actual: &str) -> crate::Result<()> {
    if expected == actual {
        Ok(())
    } else {
        Err(crate::Error::Application(crate::ApplicationError {
            kind: crate::ApplicationErrorKind::WrongMethodName,
            message: format!("expected {} got {}", expected, actual),
        }))
    }
}

/// Compare the expected message type `expected` with the received message type
/// `actual`.
///
/// Return `()` if `actual == expected`, `Err` otherwise.
pub fn verify_expected_message_type(
    expected: TMessageType,
    actual: TMessageType,
) -> crate::Result<()> {
    if expected == actual {
        Ok(())
    } else {
        Err(crate::Error::Application(crate::ApplicationError {
            kind: crate::ApplicationErrorKind::InvalidMessageType,
            message: format!("expected {} got {}", expected, actual),
        }))
    }
}

/// Check if a required Thrift struct field exists.
///
/// Return `()` if it does, `Err` otherwise.
pub fn verify_required_field_exists<T>(field_name: &str, field: &Option<T>) -> crate::Result<()> {
    match *field {
        Some(_) => Ok(()),
        None => Err(crate::Error::Protocol(crate::ProtocolError {
            kind: crate::ProtocolErrorKind::Unknown,
            message: format!("missing required field {}", field_name),
        })),
    }
}

/// Extract the field id from a Thrift field identifier.
///
/// `field_ident` must *not* have `TFieldIdentifier.field_type` of type `TType::Stop`.
///
/// Return `TFieldIdentifier.id` if an id exists, `Err` otherwise.
pub fn field_id(field_ident: &TFieldIdentifier) -> crate::Result<i16> {
    field_ident.id.ok_or_else(|| {
        crate::Error::Protocol(crate::ProtocolError {
            kind: crate::ProtocolErrorKind::Unknown,
            message: format!("missing field id in {:?}", field_ident),
        })
    })
}

#[cfg(test)]
mod tests {

    use std::io::Cursor;

    use super::*;
    use crate::transport::{TReadTransport, TWriteTransport};

    #[test]
    fn must_create_usable_input_protocol_from_concrete_input_protocol() {
        let r: Box<dyn TReadTransport> = Box::new(Cursor::new([0, 1, 2]));
        let mut t = TCompactInputProtocol::new(r);
        takes_input_protocol(&mut t)
    }

    #[test]
    fn must_create_usable_input_protocol_from_boxed_input() {
        let r: Box<dyn TReadTransport> = Box::new(Cursor::new([0, 1, 2]));
        let mut t: Box<dyn TInputProtocol> = Box::new(TCompactInputProtocol::new(r));
        takes_input_protocol(&mut t)
    }

    #[test]
    fn must_create_usable_output_protocol_from_concrete_output_protocol() {
        let w: Box<dyn TWriteTransport> = Box::new(vec![0u8; 10]);
        let mut t = TCompactOutputProtocol::new(w);
        takes_output_protocol(&mut t)
    }

    #[test]
    fn must_create_usable_output_protocol_from_boxed_output() {
        let w: Box<dyn TWriteTransport> = Box::new(vec![0u8; 10]);
        let mut t: Box<dyn TOutputProtocol> = Box::new(TCompactOutputProtocol::new(w));
        takes_output_protocol(&mut t)
    }

    fn takes_input_protocol<R>(t: &mut R)
    where
        R: TInputProtocol,
    {
        t.read_byte().unwrap();
    }

    fn takes_output_protocol<W>(t: &mut W)
    where
        W: TOutputProtocol,
    {
        t.flush().unwrap();
    }
}
