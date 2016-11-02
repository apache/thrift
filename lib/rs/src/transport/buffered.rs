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

use std::cmp;
use std::io;

use super::{Error, Result, TTransport, TTransportState};

/// Maximum length of the read buffer.
const DEFAULT_RBUFFER_SIZE: usize = 4096;

/// Maximum length of the write buffer.
const DEFAULT_WBUFFER_SIZE: usize = 4096;

/// A Thrift transport that performs I/O operations
/// to/from an intermediate buffer to avoid hitting
/// the underlying transport unnecessarily.
pub struct TBufferedTransport {
    /// Buffer into which data is read and from which reads are served.
    rbuffer: Vec<u8>,
    /// Buffer into which data is written and from which
    /// writes are served. This buffer is not emptied until
    /// `io::Write::flush()` is called.
    wbuffer: Vec<u8>,
    /// Underlying `TTransport` over which bytes are transferred.
    underlying_transport: Box<TTransport>,
}

impl TBufferedTransport {
    pub fn new(underlying_transport: Box<TTransport>) -> TBufferedTransport {
        TBufferedTransport {
            rbuffer: Vec::with_capacity(DEFAULT_RBUFFER_SIZE),
            wbuffer: Vec::with_capacity(DEFAULT_WBUFFER_SIZE),
            underlying_transport: underlying_transport,
        }
    }

    fn check_transport_open(&self) -> io::Result<()> {
        match self.state() {
            TTransportState::OPEN => Ok(()),
            _ => Err(io::Error::new(io::ErrorKind::NotConnected, "underlying transport unavailable"))

        }
    }
}

impl io::Read for TBufferedTransport {
    fn read(&mut self, _: &mut [u8]) -> io::Result<usize> {
        unimplemented!()
    }
}

impl io::Write for TBufferedTransport {
    fn write(&mut self, b: &[u8]) -> io::Result<usize> { // FIXME: avoid copying
        try!(self.check_transport_open());
        let copy_count = cmp::min(b.len(), self.wbuffer.capacity() - self.wbuffer.len());
        self.wbuffer.extend_from_slice(&b[0..copy_count]);
        assert!(self.wbuffer.len() != self.wbuffer.capacity(), "copy overflowed buffer");
        Ok(copy_count)
    }

    fn flush(&mut self) -> io::Result<()> {
        try!(self.check_transport_open());
        try!(self.underlying_transport.write_all(&self.wbuffer));
        try!(self.underlying_transport.flush());
        self.wbuffer.clear();
        Ok(())
    }
}

impl TTransport for TBufferedTransport {
    fn open(&mut self) -> Result<()> {
        self.underlying_transport.open()
    }

    fn close(&mut self) -> Result<()> {
        self.underlying_transport.close()
    }

    fn state(&self) -> TTransportState {
        self.underlying_transport.state()
    }
}
