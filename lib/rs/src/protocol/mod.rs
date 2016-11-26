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

use std::cell::RefCell;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::convert::From;
use std::rc::Rc;
use try_from::TryFrom;

use ::{ProtocolError, ProtocolErrorKind};
use ::transport::RcTTransport;

pub use self::binary::TBinaryProtocol;

mod binary;
mod compact;
mod multiplexed;

/// Maximum depth to which we will skip a Thrift field.
const MAXIMUM_SKIP_DEPTH: i8 = 64;

// FIXME: consider splitting apart the read and write methods -> makes ownership easier
/// Implemented by Thrift protocols to write/read
/// a Thrift object to/from its serialized representation.
pub trait TProtocol {

    //
    // Methods to write a thrift type into its serialized form.
    //

    fn write_message_begin(&mut self, identifier: &TMessageIdentifier) -> ::Result<()>;
    fn write_message_end(&mut self) -> ::Result<()>;
    fn write_struct_begin(&mut self, identifier: &TStructIdentifier) -> ::Result<()>;
    fn write_struct_end(&mut self) -> ::Result<()>;
    fn write_field_begin(&mut self, identifier: &TFieldIdentifier) -> ::Result<()>;
    fn write_field_end(&mut self) -> ::Result<()>;
    fn write_field_stop(&mut self) -> ::Result<()>; // FIXME: do I actually need this?
    fn write_bool(&mut self, b: bool) -> ::Result<()>;
    fn write_bytes(&mut self, b: &[u8]) -> ::Result<()>;
    fn write_i8(&mut self, i: i8) -> ::Result<()>;
    fn write_i16(&mut self, i: i16) -> ::Result<()>;
    fn write_i32(&mut self, i: i32) -> ::Result<()>;
    fn write_i64(&mut self, i: i64) -> ::Result<()>;
    fn write_double(&mut self, d: f64) -> ::Result<()>;
    fn write_string(&mut self, s: &str) -> ::Result<()>;
    fn write_list_begin(&mut self, identifier: &TListIdentifier) -> ::Result<()>;
    fn write_list_end(&mut self) -> ::Result<()>;
    fn write_set_begin(&mut self, identifier: &TSetIdentifier) -> ::Result<()>;
    fn write_set_end(&mut self) -> ::Result<()>;
    fn write_map_begin(&mut self, identifier: &TMapIdentifier) -> ::Result<()>;
    fn write_map_end(&mut self) -> ::Result<()>;

    fn flush(&mut self) -> ::Result<()>;

    //
    // Methods to read a thrift type from its serialized form.
    //

    fn read_message_begin(&mut self) -> ::Result<TMessageIdentifier>;
    fn read_message_end(&mut self) -> ::Result<()>;
    fn read_struct_begin(&mut self) -> ::Result<Option<TStructIdentifier>>;
    fn read_struct_end(&mut self) -> ::Result<()>;
    fn read_field_begin(&mut self) -> ::Result<TFieldIdentifier>;
    fn read_field_end(&mut self) -> ::Result<()>;
    fn read_bool(&mut self) -> ::Result<bool>;
    fn read_bytes(&mut self) -> ::Result<Vec<u8>>;
    fn read_i8(&mut self) -> ::Result<i8>;
    fn read_i16(&mut self) -> ::Result<i16>;
    fn read_i32(&mut self) -> ::Result<i32>;
    fn read_i64(&mut self) -> ::Result<i64>;
    fn read_double(&mut self) -> ::Result<f64>;
    fn read_string(&mut self) -> ::Result<String>;
    fn read_list_begin(&mut self) -> ::Result<TListIdentifier>;
    fn read_list_end(&mut self) -> ::Result<()>;
    fn read_set_begin(&mut self) -> ::Result<TSetIdentifier>;
    fn read_set_end(&mut self) -> ::Result<()>;
    fn read_map_begin(&mut self) -> ::Result<TMapIdentifier>;
    fn read_map_end(&mut self) -> ::Result<()>;

    fn skip(&mut self, field_type: TType) -> ::Result<()> {
        self.skip_till_depth(field_type, MAXIMUM_SKIP_DEPTH)
    }

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
                    try!(self.skip_till_depth(map_ident.key_type, remaining_depth - 1));
                    try!(self.skip_till_depth(map_ident.value_type, remaining_depth - 1));
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

    fn write_byte(&mut self, b: u8) -> ::Result<()>;
    fn read_byte(&mut self) -> ::Result<u8>;
}

pub type RcTProtocol = Rc<RefCell<Box<TProtocol>>>;

pub trait TProtocolFactory<P: TProtocol> {
    fn new(&self, transport: RcTTransport) -> P;
}

/// Identifies an instance of a Thrift message
/// in its corresponding protocol representation.
#[derive(Debug, Eq, PartialEq)]
pub struct TMessageIdentifier {
    pub name: String, // FIXME: allow usage of &str
    pub message_type: TMessageType,
    pub sequence_number: i32,
}

/// Identifies an instance of a Thrift struct
/// in its corresponding protocol representation.
#[derive(Debug, Eq, PartialEq)]
pub struct TStructIdentifier {
    pub name: String, // FIXME: allow usage of &str
}

/// Identifies an instance of a Thrift field
/// in its corresponding protocol representation.
#[derive(Debug, Eq, PartialEq)]
pub struct TFieldIdentifier {
    pub name: Option<String>, // FIXME: allow usage of &str
    pub field_type: TType,
    pub id: Option<i16>, // FIXME: this sucks that this is an option (only required for Field::Stop)
}

/// Identifies an instance of a list
/// in its protocol representation.
#[derive(Debug, Eq, PartialEq)]
pub struct TListIdentifier {
    pub element_type: TType,
    pub size: i32,
}

/// Identifies an instance of a set
/// in its protocol representation.
#[derive(Debug, Eq, PartialEq)]
pub struct TSetIdentifier {
    pub element_type: TType,
    pub size: i32,
}

/// Identifies an instance of a map
/// in its protocol representation.
#[derive(Debug, Eq, PartialEq)]
pub struct TMapIdentifier {
    pub key_type: TType,
    pub value_type: TType,
    pub size: i32,
}

/// Thrift message type.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TMessageType {
    Call,
    Reply,
    Exception,
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

// Converts a Thrift message-type enum into its
// byte representation for encoding into its serialized form.
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

// Converts the serialized representation of a
// Thrift message type into its enum form.
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

/// Thrift field type.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TType {
    Stop,
    Void,
    Bool,
    I08,
    Double,
    I16,
    I32,
    I64,
    String,
    Utf7,
    Struct,
    Map,
    Set,
    List,
    Utf8,
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

// Converts a Thrift field-type enum into its
// byte representation for encoding into its serialized form.
impl From<TType> for u8 {
    fn from(field_type: TType) -> Self {
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
}

// Converts the serialized representation of a
// Thrift field type into its enum form.
impl TryFrom<u8> for TType {
    type Err = ::Error;
    fn try_from(b: u8) -> ::Result<Self> {
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
}
