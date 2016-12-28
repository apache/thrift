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

use std::convert::{From, Into};
use std::error::Error as StdError;
use std::fmt::{Debug, Display, Formatter};
use std::{error, fmt, io, string};
use try_from::TryFrom;

use ::protocol::{TFieldIdentifier, TInputProtocol, TOutputProtocol, TStructIdentifier, TType};

// FIXME: should all my error structs impl error::Error as well?
// FIXME: should all fields in TransportError, ProtocolError and ApplicationError be optional?
pub enum Error {
    Transport(TransportError),
    Protocol(ProtocolError),
    Application(ApplicationError),
    User(Box<error::Error + Sync + Send>),
}

impl Error {
    pub fn read_application_error_from_in_protocol(i: &mut TInputProtocol) -> ::Result<ApplicationError> {
        let mut message = "general remote error".to_owned();
        let mut kind = ApplicationErrorKind::Unknown;

        try!(i.read_struct_begin());

        loop {
            let field_ident = try!(i.read_field_begin());

            if field_ident.field_type == TType::Stop {
                break;
            }

            let id = field_ident.id.expect("sender should always specify id for non-STOP field");

            match id {
                1 => {
                    let remote_message = try!(i.read_string());
                    try!(i.read_field_end());
                    message = remote_message;
                },
                2 => {
                    let remote_type_as_int = try!(i.read_i32());
                    let remote_kind: ApplicationErrorKind = TryFrom::try_from(remote_type_as_int).unwrap_or(ApplicationErrorKind::Unknown);
                    try!(i.read_field_end());
                    kind = remote_kind;
                },
                _ => {
                    try!(i.skip(field_ident.field_type));
                },
            }
        }

        try!(i.read_struct_end());

        Ok(ApplicationError { kind: kind, message: message })
    }

    pub fn write_application_error_to_out_protocol(e: &ApplicationError, o: &mut TOutputProtocol) -> ::Result<()> {
        try!(o.write_struct_begin(&TStructIdentifier { name: "TApplicationException".to_owned() }));

        try!(o.write_field_begin(&TFieldIdentifier { name: Some("message".to_owned()), field_type: TType::String, id: Some(1) }));
        try!(o.write_string(&e.message));
        try!(o.write_field_end());

        try!(o.write_field_begin(&TFieldIdentifier { name: Some("type".to_owned()), field_type: TType::I32, id: Some(2) }));
        try!(o.write_i32(e.kind as i32));
        try!(o.write_field_end());

        try!(o.write_field_stop());
        try!(o.write_struct_end());

        o.flush()
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::Transport(ref e) => TransportError::description(&e),
            Error::Protocol(ref e) => ProtocolError::description(&e),
            Error::Application(ref e) => ApplicationError::description(&e),
            Error::User(ref e) => e.description(),
        }
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Error::Transport(ref e) => Debug::fmt(&e, f),
            Error::Protocol(ref e) => Debug::fmt(&e, f),
            Error::Application(ref e) => Debug::fmt(&e, f),
            Error::User(ref e) => Debug::fmt(&e, f),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Error::Transport(ref e) => Display::fmt(&e, f),
            Error::Protocol(ref e) => Display::fmt(&e, f),
            Error::Application(ref e) => Display::fmt(&e, f),
            Error::User(ref e) => Display::fmt(&e, f),
        }
    }
}

// String is automatically turned into ApplicationError instances.
impl From<String> for Error {
    fn from(s: String) -> Self {
       Error::Application(
           ApplicationError {
               kind: ApplicationErrorKind::Unknown,
               message: s,
           }
       )
    }
}

// &str is automatically turned into ApplicationError instances.
impl<'a> From<&'a str> for Error {
    fn from(s: &'a str) -> Self {
       Error::Application(
           ApplicationError {
               kind: ApplicationErrorKind::Unknown,
               message: String::from(s),
           }
       )
    }
}

#[derive(Debug)]
pub struct TransportError {
    pub kind: TransportErrorKind,
    pub message: String,
}

impl TransportError {
    pub fn new<S: Into<String>>(kind: TransportErrorKind, message: S) -> TransportError {
        TransportError { kind: kind, message: message.into() }
    }
}

#[derive(Clone, Copy, Eq, Debug, PartialEq)]
pub enum TransportErrorKind {
    Unknown      = 0,
    NotOpen      = 1,
    AlreadyOpen  = 2,
    TimedOut     = 3,
    EndOfFile    = 4,
    NegativeSize = 5 ,
    SizeLimit    = 6,
}

impl TransportError {
    fn description(&self) -> &str {
        match self.kind {
            TransportErrorKind::Unknown => "transport error",
            TransportErrorKind::NotOpen => "not open",
            TransportErrorKind::AlreadyOpen => "already open",
            TransportErrorKind::TimedOut => "timed out",
            TransportErrorKind::EndOfFile => "end of file",
            TransportErrorKind::NegativeSize => "negative size message",
            TransportErrorKind::SizeLimit => "message too long",
        }
    }
}

impl Display for TransportError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

impl TryFrom<i32> for TransportErrorKind {
    type Err = Error;
    fn try_from(from: i32) -> Result<Self, Self::Err> {
        match from {
            0 => Ok(TransportErrorKind::Unknown),
            1 => Ok(TransportErrorKind::NotOpen),
            2 => Ok(TransportErrorKind::AlreadyOpen),
            3 => Ok(TransportErrorKind::TimedOut),
            4 => Ok(TransportErrorKind::EndOfFile),
            5 => Ok(TransportErrorKind::NegativeSize),
            6 => Ok(TransportErrorKind::SizeLimit),
            _ => Err(
                Error::Protocol(
                    ProtocolError {
                        kind: ProtocolErrorKind::Unknown,
                        message: format!("cannot convert {} to TransportErrorKind", from)
                    }
                )
            )
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        match err.kind() {
            io::ErrorKind::ConnectionReset | io::ErrorKind::ConnectionRefused | io::ErrorKind::NotConnected => Error::Transport(
                TransportError {
                    kind: TransportErrorKind::NotOpen,
                    message: err.description().to_owned(),
                }
            ),
            io::ErrorKind::AlreadyExists => Error::Transport(
                TransportError {
                    kind: TransportErrorKind::AlreadyOpen,
                    message: err.description().to_owned(),
                }
            ),
            io::ErrorKind::TimedOut => Error::Transport(
                TransportError {
                    kind: TransportErrorKind::TimedOut,
                    message: err.description().to_owned(),
                }
            ),
            io::ErrorKind::UnexpectedEof => Error::Transport(
                TransportError {
                    kind: TransportErrorKind::EndOfFile,
                    message: err.description().to_owned(),
                }
            ),
            _ => Error::Transport(
                TransportError {
                    kind: TransportErrorKind::Unknown,
                    message: err.description().to_owned(), // FIXME: use io error's debug string
                }
            ),
        }
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(err: string::FromUtf8Error) -> Self {
        Error::Protocol(
            ProtocolError {
                kind: ProtocolErrorKind::InvalidData,
                message: err.description().to_owned(), // FIXME: use fmt::Error's debug string
            }
        )
    }
}

#[derive(Debug)]
pub struct ProtocolError {
    pub kind: ProtocolErrorKind,
    pub message: String,
}

impl ProtocolError {
    pub fn new<S: Into<String>>(kind: ProtocolErrorKind, message: S) -> ProtocolError {
        ProtocolError { kind: kind, message: message.into() }
    }
}

#[derive(Clone, Copy, Eq, Debug, PartialEq)]
pub enum ProtocolErrorKind {
    Unknown        = 0,
    InvalidData    = 1,
    NegativeSize   = 2,
    SizeLimit      = 3,
    BadVersion     = 4,
    NotImplemented = 5,
    DepthLimit     = 6,
}

impl ProtocolError {
    fn description(&self) -> &str {
        match self.kind {
            ProtocolErrorKind::Unknown => "protocol error",
            ProtocolErrorKind::InvalidData => "bad data",
            ProtocolErrorKind::NegativeSize => "negative message size",
            ProtocolErrorKind::SizeLimit => "message too long",
            ProtocolErrorKind::BadVersion => "invalid thrift version",
            ProtocolErrorKind::NotImplemented => "protocol method not implemented",
            ProtocolErrorKind::DepthLimit => "maximum skip depth reached",
        }
    }
}

impl Display for ProtocolError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

impl TryFrom<i32> for ProtocolErrorKind {
    type Err = Error;
    fn try_from(from: i32) -> Result<Self, Self::Err> {
        match from {
            0 => Ok(ProtocolErrorKind::Unknown),
            1 => Ok(ProtocolErrorKind::InvalidData),
            2 => Ok(ProtocolErrorKind::NegativeSize),
            3 => Ok(ProtocolErrorKind::SizeLimit),
            4 => Ok(ProtocolErrorKind::BadVersion),
            5 => Ok(ProtocolErrorKind::NotImplemented),
            6 => Ok(ProtocolErrorKind::DepthLimit),
            _ => Err(
                Error::Protocol(
                    ProtocolError {
                        kind: ProtocolErrorKind::Unknown,
                        message: format!("cannot convert {} to ProtocolErrorKind", from)
                    }
                )
            )
        }
    }
}

#[derive(Debug)]
pub struct ApplicationError {
    pub kind: ApplicationErrorKind,
    pub message: String,
}

impl ApplicationError {
    pub fn new<S: Into<String>>(kind: ApplicationErrorKind, message: S) -> ApplicationError {
        ApplicationError { kind: kind, message: message.into() }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ApplicationErrorKind {
    Unknown               = 0,
    UnknownMethod         = 1,
    InvalidMessageType    = 2,
    WrongMethodName       = 3,
    BadSequenceId         = 4,
    MissingResult         = 5,
    InternalError         = 6,
    ProtocolError         = 7,
    InvalidTransform      = 8, // ??
    InvalidProtocol       = 9, // ??
    UnsupportedClientType = 10, // ??
}

impl ApplicationError {
    fn description(&self) -> &str {
        match self.kind {
            ApplicationErrorKind::Unknown => "service error",
            ApplicationErrorKind::UnknownMethod => "unknown service method",
            ApplicationErrorKind::InvalidMessageType => "wrong message type received",
            ApplicationErrorKind::WrongMethodName => "unknown method reply received",
            ApplicationErrorKind::BadSequenceId => "out of order sequence id",
            ApplicationErrorKind::MissingResult => "missing method result",
            ApplicationErrorKind::InternalError => "remote service threw exception",
            ApplicationErrorKind::ProtocolError => "protocol error",
            ApplicationErrorKind::InvalidTransform => "invalid transform",
            ApplicationErrorKind::InvalidProtocol => "invalid protocol requested",
            ApplicationErrorKind::UnsupportedClientType => "unsupported protocol client",
        }
    }
}

impl Display for ApplicationError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

impl TryFrom<i32> for ApplicationErrorKind {
    type Err = Error;
    fn try_from(from: i32) -> Result<Self, Self::Err> {
        match from {
            0 => Ok(ApplicationErrorKind::Unknown),
            1 => Ok(ApplicationErrorKind::UnknownMethod),
            2 => Ok(ApplicationErrorKind::InvalidMessageType),
            3 => Ok(ApplicationErrorKind::WrongMethodName),
            4 => Ok(ApplicationErrorKind::BadSequenceId),
            5 => Ok(ApplicationErrorKind::MissingResult),
            6 => Ok(ApplicationErrorKind::InternalError),
            7 => Ok(ApplicationErrorKind::ProtocolError),
            8 => Ok(ApplicationErrorKind::InvalidTransform),
            9 => Ok(ApplicationErrorKind::InvalidProtocol),
            10 => Ok(ApplicationErrorKind::UnsupportedClientType),
            _ => Err(
                Error::Application(
                    ApplicationError {
                        kind: ApplicationErrorKind::Unknown,
                        message: format!("cannot convert {} to ApplicationErrorKind", from)
                    }
                )
            )
        }
    }
}
