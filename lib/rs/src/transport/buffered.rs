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

use std::cell::RefCell;
use std::cmp;
use std::io;
use std::io::{Read, Write};
use std::rc::Rc;

use super::TTransport;

/// Default capacity of the read buffer in bytes.
const DEFAULT_RBUFFER_CAPACITY: usize = 4096;

/// Default capacity of the write buffer in bytes..
const DEFAULT_WBUFFER_CAPACITY: usize = 4096;

/// A Thrift transport that performs I/O operations
/// to/from an intermediate buffer to avoid hitting
/// the underlying transport unnecessarily.
pub struct TBufferedTransport<W: TTransport> {
    rbuf: Vec<u8>,
    rpos: usize,
    rcap: usize,
    wbuf: Vec<u8>,
    tran: Rc<RefCell<Box<W>>>,
}

impl<W: TTransport> TBufferedTransport<W> {
    pub fn new(wrapped: Rc<RefCell<Box<W>>>) -> TBufferedTransport<W> {
        TBufferedTransport::with_capacity(DEFAULT_RBUFFER_CAPACITY, DEFAULT_WBUFFER_CAPACITY, wrapped)
    }

    // ugh. no function overloading
    pub fn with_capacity(read_buffer_capacity: usize, write_buffer_capacity: usize, wrapped: Rc<RefCell<Box<W>>>) -> TBufferedTransport<W> {
        TBufferedTransport {
            rbuf: Vec::with_capacity(read_buffer_capacity),
            rpos: 0,
            rcap: 0,
            wbuf: Vec::with_capacity(write_buffer_capacity),
            tran: wrapped,
        }
    }

    fn get_bytes(&mut self) -> io::Result<&[u8]> {
        if self.rpos == self.rbuf.len() {
            self.rpos = 0;
            self.rcap = try!(self.tran.borrow_mut().read(&mut self.rbuf));
        }

        Ok(&self.rbuf[self.rpos..self.rcap])
    }

    fn consume(&mut self, consumed: usize) {
        self.rpos += cmp::min(self.rcap, self.rpos + consumed);
    }
}

impl<W: TTransport> Read for TBufferedTransport<W> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut bytes_read = 0;

        // FIXME: is there even any need for a loop?
        loop {
            let nread = {
                let bytes = try!(self.get_bytes());
                let remaining = buf.len() - bytes_read;
                let nread = cmp::min(remaining, bytes.len());
                buf[bytes_read..(bytes_read + nread)].copy_from_slice(&bytes[..nread]);
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

impl<W: TTransport> Write for TBufferedTransport<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let copy_count = cmp::min(buf.len(), self.wbuf.capacity() - self.wbuf.len());
        self.wbuf.extend_from_slice(&buf[..copy_count]);
        assert!(self.wbuf.len() <= self.wbuf.capacity(), "copy overflowed buffer");
        Ok(copy_count)
    }

    fn flush(&mut self) -> io::Result<()> {
        try!(self.tran.borrow_mut().write_all(&self.wbuf));
        try!(self.tran.borrow_mut().flush());
        self.wbuf.clear();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::io::{Read, Write};
    use std::rc::Rc;

    use super::*;
    use ::transport::mem::TBufferTransport;

    #[test]
    fn return_zero_if_nothing_can_be_read() {
        let i = Rc::new(RefCell::new(Box::new(TBufferTransport::with_capacity(10, 5))));
        let mut t = TBufferedTransport::with_capacity(10, 10, i);

        let mut b = vec![0; 10];
        let r = t.read(&mut b);

        assert!(r.is_ok());
        assert_eq!(r.unwrap(), 0);
    }

    #[test]
    fn return_zero_if_nothing_can_be_written() {
        let i = Rc::new(RefCell::new(Box::new(TBufferTransport::with_capacity(0, 0))));
        let mut t = TBufferedTransport::with_capacity(0, 0, i);

        let mut b = vec![0; 10];
        let r = t.write(&mut b);

        assert!(r.is_ok());
        assert_eq!(r.unwrap(), 0);
    }

    #[test]
    fn only_write_on_flush() {
        let i = Rc::new(RefCell::new(Box::new(TBufferTransport::with_capacity(10, 10))));
        let mut t = TBufferedTransport::new(i);

        let b: [u8; 5] = [0, 1, 2, 3, 4];
        assert!(t.write(&b).is_ok());
        assert_eq!(t.tran.borrow_mut().write_buffer().len(), 0);

        assert!(t.flush().is_ok());

        {
            let inner = t.tran.borrow_mut();
            let underlying_buffer = inner.write_buffer();
            assert_eq!(b, underlying_buffer);
        }
    }

    #[test]
    fn return_zero_if_read_called_with_zero_capacity_buffer() {}
}
