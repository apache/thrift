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

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use std::cell::RefCell;
use std::cmp;
use std::io;
use std::io::{ErrorKind, Read, Write};
use std::rc::Rc;

use super::{TTransport, TTransportFactory};

/// Default capacity of the read buffer in bytes.
const WRITE_BUFFER_CAPACITY: usize = 4096;

/// Default capacity of the write buffer in bytes..
const DEFAULT_WBUFFER_CAPACITY: usize = 4096;

/// Transport that communicates with endpoints using framed messages.
///
/// A `TFramedTransport` maintains a fixed-size internal write buffer. All
/// writes are made to this buffer and are sent to the wrapped transport only
/// when `TTransport::flush()` is called. On a flush a fixed-length header with a
/// count of the buffered bytes is written, followed by the bytes themselves.
///
/// A `TFramedTransport` also maintains a fixed-size internal read buffer.
/// On a call to `TTransport::read(...)` one full message - both fixed-length
/// header and bytes - is read from the wrapped transport and buffered.
/// Subsequent read calls are serviced from the internal buffer until it is
/// exhausted, at which point the next full message is read from the wrapped
/// transport.
///
/// # Examples
///
/// Create and use a `TFramedTransport`.
///
/// ```no_run
/// use std::cell::RefCell;
/// use std::rc::Rc;
/// use std::io::{Read, Write};
/// use thrift::transport::{TFramedTransport, TTcpTransport, TTransport};
///
/// let mut t = TTcpTransport::new();
/// t.open("localhost:9090").unwrap();
///
/// let t = Rc::new(RefCell::new(Box::new(t) as Box<TTransport>));
/// let mut t = TFramedTransport::new(t);
///
/// // read
/// t.read(&mut vec![0u8; 1]).unwrap();
///
/// // write
/// t.write(&[0x00]).unwrap();
/// t.flush().unwrap();
/// ```
pub struct TFramedTransport {
    rbuf: Box<[u8]>,
    rpos: usize,
    rcap: usize,
    wbuf: Box<[u8]>,
    wpos: usize,
    inner: Rc<RefCell<Box<TTransport>>>,
}

impl TFramedTransport {
    /// Create a `TFramedTransport` with default-sized internal read and
    /// write buffers that wraps an `inner` `TTransport`.
    pub fn new(inner: Rc<RefCell<Box<TTransport>>>) -> TFramedTransport {
        TFramedTransport::with_capacity(WRITE_BUFFER_CAPACITY, DEFAULT_WBUFFER_CAPACITY, inner)
    }

    /// Create a `TFramedTransport` with an internal read buffer of size
    /// `read_buffer_capacity` and an internal write buffer of size
    /// `write_buffer_capacity` that wraps an `inner` `TTransport`.
    pub fn with_capacity(read_buffer_capacity: usize,
                         write_buffer_capacity: usize,
                         inner: Rc<RefCell<Box<TTransport>>>)
                         -> TFramedTransport {
        TFramedTransport {
            rbuf: vec![0; read_buffer_capacity].into_boxed_slice(),
            rpos: 0,
            rcap: 0,
            wbuf: vec![0; write_buffer_capacity].into_boxed_slice(),
            wpos: 0,
            inner: inner,
        }
    }
}

impl Read for TFramedTransport {
    fn read(&mut self, b: &mut [u8]) -> io::Result<usize> {
        if self.rcap - self.rpos == 0 {
            let message_size = self.inner.borrow_mut().read_i32::<BigEndian>()? as usize;
            if message_size > self.rbuf.len() {
                return Err(io::Error::new(ErrorKind::Other,
                                          format!("bytes to be read ({}) exceeds buffer \
                                                   capacity ({})",
                                                  message_size,
                                                  self.rbuf.len())));
            }
            self.inner.borrow_mut().read_exact(&mut self.rbuf[..message_size])?;
            self.rpos = 0;
            self.rcap = message_size as usize;
        }

        let nread = cmp::min(b.len(), self.rcap - self.rpos);
        b[..nread].clone_from_slice(&self.rbuf[self.rpos..self.rpos + nread]);
        self.rpos += nread;

        Ok(nread)
    }
}

impl Write for TFramedTransport {
    fn write(&mut self, b: &[u8]) -> io::Result<usize> {
        if b.len() > (self.wbuf.len() - self.wpos) {
            return Err(io::Error::new(ErrorKind::Other,
                                      format!("bytes to be written ({}) exceeds buffer \
                                               capacity ({})",
                                              b.len(),
                                              self.wbuf.len() - self.wpos)));
        }

        let nwrite = b.len(); // always less than available write buffer capacity
        self.wbuf[self.wpos..(self.wpos + nwrite)].clone_from_slice(b);
        self.wpos += nwrite;
        Ok(nwrite)
    }

    fn flush(&mut self) -> io::Result<()> {
        let message_size = self.wpos;

        if let 0 = message_size {
            return Ok(());
        } else {
            self.inner.borrow_mut().write_i32::<BigEndian>(message_size as i32)?;
        }

        let mut byte_index = 0;
        while byte_index < self.wpos {
            let nwrite = self.inner.borrow_mut().write(&self.wbuf[byte_index..self.wpos])?;
            byte_index = cmp::min(byte_index + nwrite, self.wpos);
        }

        self.wpos = 0;
        self.inner.borrow_mut().flush()
    }
}

/// Factory for creating instances of `TFramedTransport`.
#[derive(Default)]
pub struct TFramedTransportFactory;

impl TFramedTransportFactory {
    // Create a `TFramedTransportFactory`.
    pub fn new() -> TFramedTransportFactory {
        TFramedTransportFactory {}
    }
}

impl TTransportFactory for TFramedTransportFactory {
    fn create(&self, inner: Rc<RefCell<Box<TTransport>>>) -> Box<TTransport> {
        Box::new(TFramedTransport::new(inner)) as Box<TTransport>
    }
}

#[cfg(test)]
mod tests {
    //    use std::io::{Read, Write};
    //
    //    use super::*;
    //    use ::transport::mem::TBufferTransport;
}
