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

//! Types required to send and receive bytes over an I/O channel.
//!
//! The core type exposed here is a `TTransport`, which can by used by a
//! `TProtocol`. While `TProtocol` exposes a typed interface for communicating
//! primitives, `TTransport` only deals with bytes.
//!
//! Specific implementations include:
//! * `TBufferedTransport`: wraps an underlying transport with a buffer,
//!   reducing the number of I/O operations
//! * `TFramedTransport`: wraps an underlying transport and prefixes outgoing
//!   messages with a message-framing header
//! * `TTcpTransport`: sends messages over a TCP socket

use std::cell::RefCell;
use std::io;
use std::rc::Rc;

mod buffered;
mod framed;
mod passthru;
mod socket;

#[cfg(test)]
pub mod mem;

pub use self::buffered::{TBufferedTransport, TBufferedTransportFactory};
pub use self::framed::{TFramedTransport, TFramedTransportFactory};
pub use self::passthru::TPassThruTransport;
pub use self::socket::TTcpTransport;

/// Marker trait identifying a channel that can be used to send/receive bytes.
pub trait TTransport: io::Read + io::Write { }

impl <I: io::Read + io::Write> TTransport for I { }

/// Helper type required by a `TSimpleServer` to create `TTransport`
/// instances with which to communicate with accepted client connections.
pub trait TTransportFactory {
    /// Construct a `TTransport` that wraps an `inner` transport, creating
    /// a transport stack.
    fn create(&self, inner: Rc<RefCell<Box<TTransport>>>) -> Box<TTransport>;
}
