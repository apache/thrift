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

//! Types used to send and receive primitives to/from a remote Thrift
//! server or client.
//!
//! Defines the following important high-level types:
//!
//! 1. `TInputProtocol`: Minimum set of operations necessary to read primitives
//!    from their wire representation to their corresponding Rust format.
//! 2. `TOutputProtocol`: Minimum set of operations necessary to write
//!    primitives from their Rust format to their corresponding wire
//!    representation.
//!
//! As well as major implementations:
//!
//! 1. `TBinaryInputProtocol`/`TBinaryOutputProtocol`: Read and write primitives
//!    in the simple Thrift binary protocol from/to the underlying I/O transport.
//! 2. `TCompactInputProtocol`/`TCompactOutputProtocol`: Read and write
//!    primitives in the compact binary protocol from/to the underlying I/O
//!    transport.
//!
//! This module also defines a number of auxiliary types used to support both
//! `TInputProtocol` and `TOutputProtocol`.

use std::cell::RefCell;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::convert::From;
use std::rc::Rc;
use try_from::TryFrom;

use ::{ProtocolError, ProtocolErrorKind};
use ::transport::TTransport;

mod binary;
mod compact;
mod multiplexed;

pub use self::binary::{TBinaryInputProtocol, TBinaryInputProtocolFactory, TBinaryOutputProtocol, TBinaryOutputProtocolFactory};
pub use self::compact::{TCompactInputProtocol, TCompactInputProtocolFactory, TCompactOutputProtocol, TCompactOutputProtocolFactory};
pub use self::multiplexed::{TMultiplexedInputProtocol, TMultiplexedOutputProtocol};

// Default maximum depth to which `TInputProtocol::skip` will skip a Thrift
// field. A default is necessary because Thrift structs or collections may
// contain nested structs and collections, which could result in indefinite
// recursion.
const MAXIMUM_SKIP_DEPTH: i8 = 64;

/// Contains the minimum set of functions necessary to read a Thrift service
/// call, primitive or container from the wire.
///
/// This trait does not deal with higher-level types like structs or exceptions
/// - only with primitives, message or container boundaries. Once read the data
/// is returned either as an identifier (for example `TMessageIdentifier`) or as
/// the primitive itself.
///
/// All methods return a `rift::Result`. If a method returns an `Err` the
/// underlying transport or protocol should be considered suspect, and the
/// channel should be terminated.
pub trait TInputProtocol {
    /// Read the beginning of a Thrift message from the wire.
    fn read_message_begin(&mut self) -> ::Result<TMessageIdentifier>;
    /// Read the end of a Thrift message from the wire.
    fn read_message_end(&mut self) -> ::Result<()>;
    /// Read the beginning of a Thrift struct from the wire.
    fn read_struct_begin(&mut self) -> ::Result<Option<TStructIdentifier>>;
    /// Read the end of a Thrift struct from the wire.
    fn read_struct_end(&mut self) -> ::Result<()>;
    /// Read the beginning of a Thrift struct field from the wire.
    fn read_field_begin(&mut self) -> ::Result<TFieldIdentifier>;
    /// Read the end of a Thrift struct field from the wire.
    fn read_field_end(&mut self) -> ::Result<()>;
    /// Read a bool from the wire.
    fn read_bool(&mut self) -> ::Result<bool>;
    /// Read a fixed-length byte array from the wire.
    fn read_bytes(&mut self) -> ::Result<Vec<u8>>;
    /// Read a word from the wire.
    fn read_i8(&mut self) -> ::Result<i8>;
    /// Read a 16-bit signed integer from the wire.
    fn read_i16(&mut self) -> ::Result<i16>;
    /// Read a 32-bit signed integer from the wire.
    fn read_i32(&mut self) -> ::Result<i32>;
    /// Read a 64-bit signed integer from the wire.
    fn read_i64(&mut self) -> ::Result<i64>;
    /// Read a 64-bit float from the wire.
    fn read_double(&mut self) -> ::Result<f64>;
    /// Read a fixed-length string (not null terminated) from the wire.
    fn read_string(&mut self) -> ::Result<String>;
    /// Read the beginning of a list from the wire.
    fn read_list_begin(&mut self) -> ::Result<TListIdentifier>;
    /// Read the end of a list from the wire.
    fn read_list_end(&mut self) -> ::Result<()>;
    /// Read the beginning of a set from the wire.
    fn read_set_begin(&mut self) -> ::Result<TSetIdentifier>;
    /// Read the end of a set from the wire.
    fn read_set_end(&mut self) -> ::Result<()>;
    /// Read the beginning of a map from the wire.
    fn read_map_begin(&mut self) -> ::Result<TMapIdentifier>;
    /// Read the end of a map from the wire.
    fn read_map_end(&mut self) -> ::Result<()>;
    /// Skip a field of type `field_type` recursively until `MAXIMUM_SKIP_DEPTH`
    /// is reached.
    fn skip(&mut self, field_type: TType) -> ::Result<()> {
        self.skip_till_depth(field_type, MAXIMUM_SKIP_DEPTH)
    }
    /// Skip a field of type `field_type` recursively for `remaining_depth`
    /// levels.
    fn skip_till_depth(&mut self, field_type: TType, remaining_depth: i8) -> ::Result<()> {
        if remaining_depth == 0 {
           return Err(
               ::Error::Protocol(
                   ProtocolError {
                       kind: ProtocolErrorKind::DepthLimit,
                       message: format!("cannot parse past {:?}", field_type),
                   }
               )
           )
        }

        match field_type {
            TType::Bool => {
                self.read_bool().map(|_| ())
            },
            TType::I08 => {
                self.read_i8().map(|_| ())
            },
            TType::I16 => {
                self.read_i16().map(|_| ())
            },
            TType::I32 => {
                self.read_i32().map(|_| ())
            },
            TType::I64 => {
                self.read_i64().map(|_| ())
            },
            TType::Double => {
                self.read_double().map(|_| ())
            },
            TType::String => {
                self.read_string().map(|_| ())
            },
            TType::Struct => {
                try!(self.read_struct_begin());
                loop {
                    let field_ident = try!(self.read_field_begin());
                    if field_ident.field_type == TType::Stop { break; }
                    try!(self.skip_till_depth(field_ident.field_type, remaining_depth - 1));
                }
                self.read_struct_end()
            },
            TType::List => {
                let list_ident = try!(self.read_list_begin());
                for _ in 0..list_ident.size {
                    try!(self.skip_till_depth(list_ident.element_type, remaining_depth - 1));
                }
                self.read_list_end()
            },
            TType::Set => {
                let set_ident = try!(self.read_set_begin());
                for _ in 0..set_ident.size {
                    try!(self.skip_till_depth(set_ident.element_type, remaining_depth - 1));
                }
                self.read_set_end()
            },
            TType::Map => {
                let map_ident = try!(self.read_map_begin());
                for _ in 0..map_ident.size {
                    let key_type = map_ident.key_type.expect("non-zero sized map should contain key type");
                    let val_type = map_ident.value_type.expect("non-zero sized map should contain value type");
                    try!(self.skip_till_depth(key_type, remaining_depth - 1));
                    try!(self.skip_till_depth(val_type, remaining_depth - 1));
                }
                self.read_map_end()
            },
            u => {
                Err(
                    ::Error::Protocol(
                        ProtocolError {
                            kind: ProtocolErrorKind::Unknown,
                            message: format!("cannot skip field type {:?}", &u),
                        }
                    )
                )
            },
        }
    }

    //
    // utility (DO NOT USE IN GENERATED CODE!!!!)
    //

    /// Read an unsigned byte from the wire.
    ///
    /// This method should **never** be used in generated code.
    fn read_byte(&mut self) -> ::Result<u8>;
}

/// Contains the minimum set of functions necessary to write a Thrift service
/// call, primitive or container from the wire.
///
/// This trait does not deal with higher-level types like structs or exceptions
/// - only with primitives, message or container boundaries. The write methods
/// take either an identifier (for example `TMessageIdentifier`) or a primitive.
/// Fields in an identifier may or may not be written to the wire; this depends
/// on the protocol implementation. Moreover, some write methods may be noops -
/// nothing is written to the wire. This is all transparent to the caller: as
/// long as a matching `TInputProtocol` implementation is used there will be no
/// issues.
///
/// All methods return a `rift::Result`. If a method returns an `Err` the
/// underlying transport or protocol should be considered suspect, and the
/// channel should be terminated.
pub trait TOutputProtocol {
    /// Write the beginning of a Thrift message to the wire.
    fn write_message_begin(&mut self, identifier: &TMessageIdentifier) -> ::Result<()>;
    /// Write the end of a Thrift message to the wire.
    fn write_message_end(&mut self) -> ::Result<()>;
    /// Write the beginning of a Thrift struct to the wire.
    fn write_struct_begin(&mut self, identifier: &TStructIdentifier) -> ::Result<()>;
    /// Write the end of a Thrift struct to the wire.
    fn write_struct_end(&mut self) -> ::Result<()>;
    /// Write the beginning of a Thrift field to the wire.
    fn write_field_begin(&mut self, identifier: &TFieldIdentifier) -> ::Result<()>;
    /// Write the end of a Thrift field to the wire.
    fn write_field_end(&mut self) -> ::Result<()>;
    /// Write a marker indicating that all fields in a Thrift struct have been
    /// successfully serialzed to the wire.
    fn write_field_stop(&mut self) -> ::Result<()>;
    /// Write a bool to the wire.
    fn write_bool(&mut self, b: bool) -> ::Result<()>;
    /// Write a fixed-length byte array to the wire.
    fn write_bytes(&mut self, b: &[u8]) -> ::Result<()>;
    /// Write an 8-bit signed integer to the wire.
    fn write_i8(&mut self, i: i8) -> ::Result<()>;
    /// Write a 16-bit signed integer to the wire.
    fn write_i16(&mut self, i: i16) -> ::Result<()>;
    /// Write a 32-bit signed integer to the wire.
    fn write_i32(&mut self, i: i32) -> ::Result<()>;
    /// Write a 64-bit signed integer to the wire.
    fn write_i64(&mut self, i: i64) -> ::Result<()>;
    /// Write a 64-bit float to the wire.
    fn write_double(&mut self, d: f64) -> ::Result<()>;
    /// Write a fixed-length string to the wire.
    fn write_string(&mut self, s: &str) -> ::Result<()>;
    /// Write the beginning of a list to the wire.
    fn write_list_begin(&mut self, identifier: &TListIdentifier) -> ::Result<()>;
    /// Write the end of a list to the wire.
    fn write_list_end(&mut self) -> ::Result<()>;
    /// Write the beginning of a set to the wire.
    fn write_set_begin(&mut self, identifier: &TSetIdentifier) -> ::Result<()>;
    /// Write the end of a set to the wire.
    fn write_set_end(&mut self) -> ::Result<()>;
    /// Write the beginning of a map to the wire.
    fn write_map_begin(&mut self, identifier: &TMapIdentifier) -> ::Result<()>;
    /// Write the end of a map to the wire.
    fn write_map_end(&mut self) -> ::Result<()>;
    /// Flush any intermediately buffered bytes to the underlying transport.
    fn flush(&mut self) -> ::Result<()>;

    //
    // utility (DO NOT USE IN GENERATED CODE!!!!)
    //

    /// Write an unsigned byte to the wire.
    ///
    /// This method should **never** be used in generated code.
    fn write_byte(&mut self, b: u8) -> ::Result<()>; // FIXME: REMOVE
}

pub trait TInputProtocolFactory {
    fn create(&mut self, transport: Rc<RefCell<Box<TTransport>>>) -> Box<TInputProtocol>;
}

pub trait TOutputProtocolFactory {
    fn create(&mut self, transport: Rc<RefCell<Box<TTransport>>>) -> Box<TOutputProtocol>;
}

/// Identifies a Thrift message.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TMessageIdentifier {
    pub name: String,
    pub message_type: TMessageType,
    pub sequence_number: i32,
}

impl TMessageIdentifier {
    /// Convenience constructor to create a new `TMessageIdentifier` instance.
    pub fn new<S: Into<String>>(name: S, message_type: TMessageType, sequence_number: i32) -> TMessageIdentifier {
        TMessageIdentifier {
            name: name.into(),
            message_type: message_type,
            sequence_number: sequence_number
        }
    }
}

/// Identifies a Thrift struct.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TStructIdentifier {
    pub name: String,
}

impl TStructIdentifier {
    /// Convenience constructor to create a new `TStructIdentifier` instance.
    pub fn new<S: Into<String>>(name: S) -> TStructIdentifier {
        TStructIdentifier { name: name.into() }
    }
}

/// Identifies a Thrift field.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TFieldIdentifier {
    pub name: Option<String>,
    pub field_type: TType,
    pub id: Option<i16>,
}

impl TFieldIdentifier {
    /// Convenience constructor to create a new `TFieldIdentifier` instance.
    pub fn new<N, S, I>(name: N, field_type: TType, id: I) -> TFieldIdentifier
        where
            N: Into<Option<S>>,
            S: Into<String>,
            I: Into<Option<i16>>
    {
        TFieldIdentifier {
            name: name.into().map(|n| n.into()),
            field_type: field_type,
            id: id.into()
        }
    }
}

/// Identifies a list.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TListIdentifier {
    pub element_type: TType,
    pub size: i32,
}

impl TListIdentifier {
    /// Convenience constructor to create a new `TListIdentifier` instance.
    pub fn new(element_type: TType, size: i32) -> TListIdentifier {
        TListIdentifier { element_type: element_type, size: size }
    }
}

/// Identifies a set.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TSetIdentifier {
    pub element_type: TType,
    pub size: i32,
}

impl TSetIdentifier {
    /// Convenience constructor to create a new `TSetIdentifier` instance.
    pub fn new(element_type: TType, size: i32) -> TSetIdentifier {
        TSetIdentifier { element_type: element_type, size: size }
    }
}

/// Identifies a map.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TMapIdentifier {
    pub key_type: Option<TType>,
    pub value_type: Option<TType>,
    pub size: i32,
}

impl TMapIdentifier {
    /// Convenience constructor to create a new `TMapIdentifier` instance.
    pub fn new<K, V>(key_type: K, value_type: V, size: i32) -> TMapIdentifier
        where
            K: Into<Option<TType>>,
            V: Into<Option<TType>>
    {
        TMapIdentifier { key_type: key_type.into(), value_type: value_type.into(), size: size }
    }
}

/// List of Thrift message types.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TMessageType {
    /// Thrift service-call request.
    Call,
    /// Thrift service-call response.
    Reply,
    /// Unexpected error returned by remote Thrift service code.
    Exception,
    /// One-way Thrift service-call request (no response is expected).
    OneWay,
}

impl Display for TMessageType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
    type Err = ::Error;
    fn try_from(b: u8) -> ::Result<Self> {
        match b {
            0x01 => Ok(TMessageType::Call),
            0x02 => Ok(TMessageType::Reply),
            0x03 => Ok(TMessageType::Exception),
            0x04 => Ok(TMessageType::OneWay),
            unkn => Err(
                ::Error::Protocol(
                    ProtocolError {
                        kind: ProtocolErrorKind::InvalidData,
                        message: format!("cannot convert {} to TMessageType", unkn),
                    }
                )
            )
        }
    }
}

/// List of Thrift struct-field types.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TType {
    /// Indicates that there are no more serialized fields in this Thrift struct.
    Stop,
    /// Indicates a void (`()`) field.
    Void,
    /// Indicates a boolean field.
    Bool,
    /// Indicates a signed 8-bit int field.
    I08,
    /// Indicates a float field.
    Double,
    /// Indicates a signed 16-bit int field.
    I16,
    /// Indicates a signed 32-bit int field.
    I32,
    /// Indicates a signed 64-bit int field.
    I64,
    /// Indicates a UTF-8 string.
    String,
    /// Indicates a UTF-7 string. *Unsupported*.
    Utf7,
    /// Indicates a Thrift struct.
    Struct,
    /// Indicates a map.
    Map,
    /// Indicates a set.
    Set,
    /// Indicates a list.
    List,
    /// Indicates a UTF-8 string.
    Utf8,
    /// Indicates a UTF-16 string. *Unsupported*.
    Utf16,
}

impl Display for TType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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

pub fn verify_expected_sequence_number(expected: i32, actual: i32) -> ::Result<()> {
    if expected == actual {
        Ok(())
    } else {
        Err(
            ::Error::Application(
                ::ApplicationError {
                    kind: ::ApplicationErrorKind::BadSequenceId,
                    message: format!("expected {} got {}", expected, actual)
                }
            )
        )
    }
}

pub fn verify_expected_service_call(expected: &str, actual: &str) -> ::Result<()> {
    if expected == actual {
        Ok(())
    } else {
        Err(
            ::Error::Application(
                ::ApplicationError {
                    kind: ::ApplicationErrorKind::WrongMethodName,
                    message: format!("expected {} got {}", expected, actual)
                }
            )
        )
    }
}

pub fn verify_expected_message_type(expected: TMessageType, actual: TMessageType) -> ::Result<()> {
    if expected == actual {
        Ok(())
    } else {
        Err(
            ::Error::Application(
                ::ApplicationError {
                    kind: ::ApplicationErrorKind::InvalidMessageType,
                    message: format!("expected {} got {}", expected, actual)
                }
            )
        )
    }
}

pub fn verify_required_field_exists<T>(field_name: &str, field: &Option<T>) -> ::Result<()> {
    match *field {
        Some(_) => Ok(()),
        None => Err(
            ::Error::Protocol(
                ::ProtocolError {
                    kind: ::ProtocolErrorKind::Unknown,
                    message: format!("missing required field {}", field_name)
                }
            )
        ),
    }
}

pub fn field_id(field_ident: &TFieldIdentifier) -> ::Result<i16> {
    field_ident.id.ok_or(
        ::Error::Protocol(
            ::ProtocolError {
                kind: ::ProtocolErrorKind::Unknown,
                message: format!("missing field in in {:?}", field_ident)
            }
        )
    )
}
