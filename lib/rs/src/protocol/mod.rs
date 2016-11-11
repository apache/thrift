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

use std::convert;
use try_from;

use ::{Error, Result};
pub use self::binary::TBinaryProtocol;

mod binary;
mod compact;
mod multiplexed;

/// Implemented by Thrift protocols to write/read
/// a Thrift object to/from its serialized representation.
pub trait TProtocol {

    //
    // Methods to write a thrift type into its serialized form.
    //

    fn write_message_begin(&mut self, identifier: &TMessageIdentifier) -> Result<()>;
    fn write_message_end(&mut self) -> Result<()>;
    fn write_struct_begin(&mut self, identifier: &TStructIdentifier) -> Result<()>;
    fn write_struct_end(&mut self) -> Result<()>;
    fn write_field_begin(&mut self, identifier: &TFieldIdentifier) -> Result<()>;
    fn write_field_end(&mut self) -> ::Result<()>;
    // fn write_map_begin(&mut self) -> Result<()>; // ktype, vtype, size
    // fn write_map_end(&mut self) -> Result<()>;
    // fn write_list_begin(&mut self) -> Result<()>; // etype, size (element_type)
    // fn write_list_end(&mut self) -> Result<()>;
    // fn write_set_begin(&mut self) -> Result<()>; // etype, size
    // fn write_set_end(&mut self) -> Result<()>;
    fn write_bool(&mut self, b: bool) -> Result<()>;
    fn write_byte<I: convert::Into<u8>>(&mut self, b: I) -> Result<()>;
    fn write_bytes(&mut self, b: &[u8]) -> Result<()>;
    fn write_i8(&mut self, i: i8) -> Result<()>;
    fn write_i16(&mut self, i: i16) -> Result<()>;
    fn write_i32(&mut self, i: i32) -> Result<()>;
    fn write_i64(&mut self, i: i64) -> Result<()>;
    fn write_double(&mut self, d: f64) -> Result<()>;
    fn write_string(&mut self, s: &str) -> Result<()>;
    fn flush(&mut self) -> Result<()>;

    //
    // Methods to read a thrift type from its serialized form.
    //

    fn read_message_begin(&mut self) -> Result<TMessageIdentifier>;
    fn read_message_end(&mut self) -> Result<()>;
    fn read_struct_begin(&mut self) -> Result<Option<TStructIdentifier>>; // FIXME: should I make all of them options?
    fn read_struct_end(&mut self) -> Result<()>;
    fn read_field_begin(&mut self) -> Result<TFieldIdentifier>;
    fn read_field_end(&mut self) -> Result<()>;
    // fn read_map_begin(&mut self) -> Result<()>; // k, v, size
    // fn read_map_end(&mut self) -> Result<()>;
    // fn read_list_begin(&mut self) -> Result<()>; // etype, size
    // fn read_list_end(&mut self) -> Result<()>;
    // fn read_set_begin(&mut self) -> Result<()>; // etype, size
    // fn read_set_end(&mut self) -> Result<()>;
    fn read_bool(&mut self) -> Result<bool>;
    fn read_byte(&mut self) -> Result<u8>;
    fn read_bytes(&mut self) -> Result<Vec<u8>>;
    fn read_i8(&mut self) -> Result<i8>;
    fn read_i16(&mut self) -> Result<i16>;
    fn read_i32(&mut self) -> Result<i32>;
    fn read_i64(&mut self) -> Result<i64>;
    fn read_double(&mut self) -> Result<f64>;
    fn read_string(&mut self) -> Result<String>;
}

/// Identifies an instance of a Thrift message
/// in its corresponding protocol representation.
#[derive(Debug)]
pub struct TMessageIdentifier {
    pub name: String, // FIXME: allow usage of &str
    pub message_type: TMessageType,
    pub sequence_number: i32,
}

/// Identifies an instance of a Thrift struct
/// in its corresponding protocol representation.
#[derive(Debug)]
pub struct TStructIdentifier {
    pub name: String, // FIXME: allow usage of &str
}

/// Identifies an instance of a Thrift field
/// in its corresponding protocol representation.
#[derive(Debug)]
pub struct TFieldIdentifier {
    pub name: Option<String>, // FIXME: allow usage of &str
    pub field_type: TType,
    pub id: i16,
}

/// Thrift message type.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TMessageType {
    Call,
    Reply,
    Exception,
    OneWay,
}

// Converts a Thrift message-type enum into its
// byte representation for encoding into its serialized form.
impl convert::From<TMessageType> for u8 {
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
impl try_from::TryFrom<u8> for TMessageType {
    type Err = Error;
    fn try_from(b: u8) -> Result<Self> {
        match b {
            0x01 => Ok(TMessageType::Call),
            0x02 => Ok(TMessageType::Reply),
            0x03 => Ok(TMessageType::Exception),
            0x04 => Ok(TMessageType::OneWay),
            unkn => Err(Error::UnknownThriftMessageType(unkn))
        }
    }
}

/// Thrift field type.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TType {
    Stop,
    Void,
    Bool,
    Byte,
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

// Converts a Thrift field-type enum into its
// byte representation for encoding into its serialized form.
impl convert::From<TType> for u8 {
    fn from(field_type: TType) -> Self {
        match field_type {
            TType::Stop => 0x00,
            TType::Void => 0x01,
            TType::Bool => 0x02,
            TType::Byte => 0x03,
            TType::I08 => 0x04,
            TType::Double => 0x05,
            TType::I16 => 0x06,
            TType::I32 => 0x07,
            TType::I64 => 0x08,
            TType::String => 0x09,
            TType::Utf7 => 0x0A,
            TType::Struct => 0x0B,
            TType::Map => 0x0C,
            TType::Set => 0x0D,
            TType::List => 0x0E,
            TType::Utf8 => 0x0F,
            TType::Utf16 => 0x11,
        }
    }
}

// Converts the serialized representation of a
// Thrift field type into its enum form.
impl try_from::TryFrom<u8> for TType {
    type Err = Error;
    fn try_from(b: u8) -> Result<Self> {
        match b {
            0x00 => Ok(TType::Stop),
            0x01 => Ok(TType::Void),
            0x02 => Ok(TType::Bool),
            0x03 => Ok(TType::Byte),
            0x04 => Ok(TType::I08),
            0x05 => Ok(TType::Double),
            0x06 => Ok(TType::I16),
            0x07 => Ok(TType::I32),
            0x08 => Ok(TType::I64),
            0x09 => Ok(TType::String),
            0x0A => Ok(TType::Utf7),
            0x0B => Ok(TType::Struct),
            0x0C => Ok(TType::Map),
            0x0D => Ok(TType::Set),
            0x0E => Ok(TType::List),
            0x0F => Ok(TType::Utf8),
            0x11 => Ok(TType::Utf16),
            unkn => Err(Error::UnknownThriftFieldType(unkn))
        }
    }
}
