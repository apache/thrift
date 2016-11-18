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

//! Traits and type definitions for performing I/O operations.
//! Each transport implementation controls bytes are transmitted
//! between an sender and a receiver.
//!
//! The core type exposed here is [`TTransport`][ttransport], which
//! can by used by a [`TProtocol`][tprotocol] to send and
//! receive messages to/from a source.
//!
//! [ttransport] trait.TTransport.html
//! [tprotocol] trait.TProtocol.html
//!
//! Specific implementations include:
//! * [`TBufferedTransport`][tbuffered]: wraps an underlying transport
//!   with a buffer, reducing the number of I/O operations
//! * [`TFramedTransport`][tframed]: prefixes outgoing messages with a message header
//! * [`TcpIpSocketTransport`][ttcp]: sends messages over a TCP socket
//!
//! [tbuffered] trait.TBufferedTransport.html
//! [tframed] trait.TFramedTransport.html
//! [ttcp] trait.TTcpIpSocketTransport.html

use std::io;

mod buffered;
mod framed;
mod socket;

#[cfg(test)]
pub mod membuffer;

pub use self::buffered::TBufferedTransport;
pub use self::socket::TTcpTransport;

/// Interface through which a `TProtocol` can perform I/O operations.
pub trait TTransport: io::Read + io::Write {
    /// Open the transport. This *must* be called
    /// before the transport is used for either reads
    /// or writes.
    fn open(&mut self) -> io::Result<()>;
    /// Close the transport. After this point this
    /// transport cannot be used for either reads or
    /// writes.
    fn close(&mut self) -> io::Result<()>;
}
