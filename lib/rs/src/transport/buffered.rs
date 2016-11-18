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

use super::TTransport;

/// Maximum length of the read buffer in bytes.
const DEFAULT_RBUFFER_SIZE: usize = 4096;

/// Maximum length of the write buffer in bytes..
const DEFAULT_WBUFFER_SIZE: usize = 4096;

/// A Thrift transport that performs I/O operations
/// to/from an intermediate buffer to avoid hitting
/// the underlying transport unnecessarily.
pub struct TBufferedTransport<T: TTransport> {
    /// Buffer into which data is read from the underlying
    /// transport, and from which reads are served.
    rbuf: Vec<u8>,
    /// Position to which callers have read bytes from the
    /// read buffer.
    rpos: usize,
    rcap: usize,
    /// Buffer into which data is copied and from which
    /// writes are pushed to the underlying transport. This
    /// buffer is not emptied until `io::Write::flush()`
    /// is called.
    wbuf: Vec<u8>,
    /// Underlying `TTransport` over which bytes are transferred.
    inner: T
}

impl<T: TTransport> TBufferedTransport<T> {
    pub fn new(inner: T) -> TBufferedTransport<T> {
        TBufferedTransport::with_capacity(DEFAULT_RBUFFER_SIZE, DEFAULT_WBUFFER_SIZE, inner)
    }

    // ugh. no function overloading
    pub fn with_capacity(read_buffer_size: usize, write_buffer_size: usize, inner: T) -> TBufferedTransport<T> {
        TBufferedTransport {
            rbuf: Vec::with_capacity(read_buffer_size),
            rpos: 0,
            rcap: 0,
            wbuf: Vec::with_capacity(write_buffer_size),
            inner: inner,
        }
    }

    fn get_bytes(&mut self) -> io::Result<&[u8]> {
        if self.rpos == self.rbuf.len() {
            self.rpos = 0;
            self.rcap = try!(self.inner.read(&mut self.rbuf));
        }

        Ok(&self.rbuf[self.rpos..self.rcap])
    }

    fn consume(&mut self, consumed: usize) {
        self.rpos += cmp::min(self.rcap, self.rpos + consumed);
    }
}

impl<T: TTransport> io::Read for TBufferedTransport<T> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut bytes_read = 0;

        loop {
            let nread = {
                let bytes = try!(self.get_bytes());
                let remaining = buf.len() - bytes_read;
                let nread = cmp::min(remaining, bytes.len());
                buf[..bytes_read].copy_from_slice(&bytes[..nread]);
                nread
            };

            bytes_read += nread;
            self.consume(nread);

            if bytes_read == buf.len() || nread == 0 {
                break
            }
        }

        Ok(bytes_read)
    }
}

impl<T: TTransport> io::Write for TBufferedTransport<T> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let copy_count = cmp::min(buf.len(), self.wbuf.capacity() - self.wbuf.len());
        self.wbuf.extend_from_slice(&buf[..copy_count]);
        assert!(self.wbuf.len() <= self.wbuf.capacity(), "copy overflowed buffer");
        Ok(copy_count)
    }

    fn flush(&mut self) -> io::Result<()> {
        try!(self.inner.write_all(&self.wbuf));
        try!(self.inner.flush());
        self.wbuf.clear();
        Ok(())
    }
}

impl<T: TTransport> TTransport for TBufferedTransport<T> {
    fn open(&mut self) -> io::Result<()> {
        self.inner.open()
    }

    fn close(&mut self) -> io::Result<()> {
        self.inner.close()
    }
}

#[cfg(test)]
mod tests {
    use std::io::{Read, Write};

    use super::*;
    use ::transport::membuffer::TMemBufferTransport;
    use ::transport::TTransport;

    #[test]
    fn return_zero_if_nothing_can_be_read() {
        let mut t = TBufferedTransport::with_capacity(10, 10, TMemBufferTransport::with_capacity(10, 5));
        assert!(t.open().is_ok());

        let mut b = vec![0; 10];
        let r = t.read(&mut b);

        assert!(r.is_ok());
        assert_eq!(r.unwrap(), 0);

        assert!(t.close().is_ok());
    }

    #[test]
    fn return_zero_if_nothing_can_be_written() {
        let mut t = TBufferedTransport::with_capacity(0, 0, TMemBufferTransport::with_capacity(0, 0));
        assert!(t.open().is_ok());

        let mut b = vec![0; 10];
        let r = t.write(&mut b);

        assert!(r.is_ok());
        assert_eq!(r.unwrap(), 0);

        assert!(t.close().is_ok());
    }

    #[test]
    fn only_write_on_flush() {
        let mut t = TBufferedTransport::new(TMemBufferTransport::with_capacity(10, 10));
        assert!(t.open().is_ok());

        let b: [u8; 5] = [0, 1, 2, 3, 4];
        assert!(t.write(&b).is_ok());
        assert_eq!(t.inner.write_buffer().len(), 0);

        assert!(t.flush().is_ok());

        {
            let underlying_buffer = t.inner.write_buffer();
            assert_eq!(b, underlying_buffer);
        }

        assert!(t.close().is_ok());
    }

    #[test]
    fn return_zero_if_read_called_with_zero_capacity_buffer() {}
}
