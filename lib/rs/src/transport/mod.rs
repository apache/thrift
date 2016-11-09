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

//! Defines thrift transport implementations. Each thrift
//! transport implementation controls how thrift-generated types
//! and service calls are transmitted between a caller and
//! a receiver.

use std::io;

use ::Result; // IMPORTANT: absolute path wrt. crate root

mod buffered;
mod framed;
mod socket;

pub use self::buffered::TBufferedTransport;
pub use self::socket::TTcpIpSocket;

/// Marker trait implemented by each transport implementation.
pub trait TTransport: io::Read + io::Write {
    /// Open the transport. This *must* be called
    /// before the transport is used for either reads
    /// or writes.
    fn open(&mut self) -> Result<()>;
    /// Close the transport. After this point this
    /// transport cannot be used for either reads or
    /// writes.
    fn close(&mut self) -> Result<()>;
    /// Current state of the transport.
    fn state(&self) -> TTransportState; // FIXME: make this private to this module
}

/// The valid states a `TTransport` can be in.
#[derive(Clone, Copy, Debug)]
pub enum TTransportState { // FIXME: make this private to this module
    /// The transport was just created. It
    /// cannot be used for reads or writes yet.
    CREATED,
    /// The transport is now open, and can
    /// be used for reads and writes.
    OPEN,
    /// The transport is now closed, and can
    /// no longer be used for reads and writes.
    CLOSED,
}
