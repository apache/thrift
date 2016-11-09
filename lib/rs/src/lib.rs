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

extern crate byteorder;
extern crate try_from;

use std::{convert, error, fmt, io, string};

pub mod protocol;
pub mod transport;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    Utf8ConversionError(string::FromUtf8Error),
    InvalidThriftMessageHeader,
    UnknownThriftMessageType(u8),
    UnknownThriftFieldType(u8),
    InvalidBooleanValue(i8),
    Unknown(String), // FIXME: make this take &str
    Application(Box<error::Error + Send + Sync>),
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::IoError(ref err) => err.description(),
            Error::Utf8ConversionError(ref err) => err.description(),
            Error::InvalidThriftMessageHeader => "invalid thrift message header",
            Error::UnknownThriftMessageType(_) => "invalid thrift message type",
            Error::UnknownThriftFieldType(_) => "invalid thrift field type",
            Error::InvalidBooleanValue(_) => "invalid boolean value",
            Error::Unknown(ref s) => &s,
            Error::Application(ref err) => err.description(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
       unimplemented!()
    }
}

impl convert::From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::IoError(err)
    }
}

impl convert::From<string::FromUtf8Error> for Error {
    fn from(err: string::FromUtf8Error) -> Self {
        Error::Utf8ConversionError(err)
    }
}
