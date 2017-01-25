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
//! The core type is the `TTransport` trait, through which a `TProtocol` can
//! send and receive primitives over the wire. While `TProtocol` instances deal
//! with primitive types, `TTransport` instances understand only bytes.

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

/// Identifies an I/O channel that can be used to send and receive bytes.
pub trait TTransport: io::Read + io::Write {}
impl<I: io::Read + io::Write> TTransport for I {}

/// Helper type used by servers to create `TTransport` instances for accepted
/// client connections.
pub trait TTransportFactory {
    /// Create a `TTransport` that wraps an `inner` transport, thus creating
    /// a transport stack.
    fn create(&self, inner: Rc<RefCell<Box<TTransport>>>) -> Box<TTransport>;
}
