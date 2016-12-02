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

//! Traits and type definitions for sending/receiving
//! bytes over an I/O channel.
//!
//! The core type exposed here is [`TTransport`][ttransport],
//! which can by used by a [`TProtocol`][tprotocol]. While `TProtocol`
//! exposes a typed interface for sending/receiving messages,
//! `TTransport` deals only with bytes.
//!
//! Specific implementations include:
//! * [`TBufferedTransport`][tbuffered]: wraps an underlying transport
//!   with a buffer, reducing the number of I/O operations
//! * [`TFramedTransport`][tframed]: wraps an underlying transport and prefixes
//!   outgoing messages with a message-framing header
//! * [`TTcpTransport`][ttcp]: sends messages over a TCP socket
//!
//! [ttransport] trait.TTransport.html
//! [tprotocol] trait.TProtocol.html
//! [tbuffered] trait.TBufferedTransport.html
//! [tframed] trait.TFramedTransport.html
//! [ttcp] trait.TTcpTransport.html

use std::cell::RefCell;
use std::io;
use std::rc::Rc;

mod buffered;
mod framed;
mod socket;

#[cfg(test)]
pub mod mem;

pub use self::buffered::{TBufferedTransport, TBufferedTransportFactory};
pub use self::framed::{TFramedTransport, TFramedTransportFactory};
pub use self::socket::TTcpTransport;

/// Marker trait identifying a `TTransport` that
/// can be used to send/receive Thrift messages.
pub trait TTransport: io::Read + io::Write { }

/// Blanket implementation of `TTransport` that allows
/// any object `I` that implements both `io::Read` and
/// `io::Write` to be represented as a `TTransport`.
impl <I: io::Read + io::Write> TTransport for I { }

/// A trait for objects that can construct a `TTransport`.
pub trait TTransportFactory {
    /// Construct a `TTransport` that wraps an `inner`
    /// transport, thus creating a transport stack.
    fn build(&self, inner: Rc<RefCell<Box<TTransport>>>) -> Box<TTransport>;
}
