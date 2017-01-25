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

use super::{TTransport, TTransportFactory};

/// Default capacity of the read buffer in bytes.
const DEFAULT_RBUFFER_CAPACITY: usize = 4096;

/// Default capacity of the write buffer in bytes..
const DEFAULT_WBUFFER_CAPACITY: usize = 4096;

/// Transport that communicates with endpoints using a byte stream.
///
/// A `TBufferedTransport` maintains a fixed-size internal write buffer. All
/// writes are made to this buffer and are sent to the wrapped transport only
/// when `TTransport::flush()` is called. On a flush a fixed-length header with a
/// count of the buffered bytes is written, followed by the bytes themselves.
///
/// A `TBufferedTransport` also maintains a fixed-size internal read buffer.
/// On a call to `TTransport::read(...)` one full message - both fixed-length
/// header and bytes - is read from the wrapped transport and buffered.
/// Subsequent read calls are serviced from the internal buffer until it is
/// exhausted, at which point the next full message is read from the wrapped
/// transport.
///
/// # Examples
///
/// Create and use a `TBufferedTransport`.
///
/// ```no_run
/// use std::cell::RefCell;
/// use std::rc::Rc;
/// use std::io::{Read, Write};
/// use thrift::transport::{TBufferedTransport, TTcpTransport, TTransport};
///
/// let mut t = TTcpTransport::new();
/// t.open("localhost:9090").unwrap();
///
/// let t = Rc::new(RefCell::new(Box::new(t) as Box<TTransport>));
/// let mut t = TBufferedTransport::new(t);
///
/// // read
/// t.read(&mut vec![0u8; 1]).unwrap();
///
/// // write
/// t.write(&[0x00]).unwrap();
/// t.flush().unwrap();
/// ```
pub struct TBufferedTransport {
    rbuf: Box<[u8]>,
    rpos: usize,
    rcap: usize,
    wbuf: Vec<u8>,
    inner: Rc<RefCell<Box<TTransport>>>,
}

impl TBufferedTransport {
    /// Create a `TBufferedTransport` with default-sized internal read and
    /// write buffers that wraps an `inner` `TTransport`.
    pub fn new(inner: Rc<RefCell<Box<TTransport>>>) -> TBufferedTransport {
        TBufferedTransport::with_capacity(DEFAULT_RBUFFER_CAPACITY, DEFAULT_WBUFFER_CAPACITY, inner)
    }

    /// Create a `TBufferedTransport` with an internal read buffer of size
    /// `read_buffer_capacity` and an internal write buffer of size
    /// `write_buffer_capacity` that wraps an `inner` `TTransport`.
    pub fn with_capacity(read_buffer_capacity: usize,
                         write_buffer_capacity: usize,
                         inner: Rc<RefCell<Box<TTransport>>>)
                         -> TBufferedTransport {
        TBufferedTransport {
            rbuf: vec![0; read_buffer_capacity].into_boxed_slice(),
            rpos: 0,
            rcap: 0,
            wbuf: Vec::with_capacity(write_buffer_capacity),
            inner: inner,
        }
    }

    fn get_bytes(&mut self) -> io::Result<&[u8]> {
        if self.rcap - self.rpos == 0 {
            self.rpos = 0;
            self.rcap = self.inner.borrow_mut().read(&mut self.rbuf)?;
        }

        Ok(&self.rbuf[self.rpos..self.rcap])
    }

    fn consume(&mut self, consumed: usize) {
        // TODO: was a bug here += <-- test somehow
        self.rpos = cmp::min(self.rcap, self.rpos + consumed);
    }
}

impl Read for TBufferedTransport {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut bytes_read = 0;

        loop {
            let nread = {
                let avail_bytes = self.get_bytes()?;
                let avail_space = buf.len() - bytes_read;
                let nread = cmp::min(avail_space, avail_bytes.len());
                buf[bytes_read..(bytes_read + nread)].copy_from_slice(&avail_bytes[..nread]);
                nread
            };

            self.consume(nread);
            bytes_read += nread;

            if bytes_read == buf.len() || nread == 0 {
                break;
            }
        }

        Ok(bytes_read)
    }
}

impl Write for TBufferedTransport {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let avail_bytes = cmp::min(buf.len(), self.wbuf.capacity() - self.wbuf.len());
        self.wbuf.extend_from_slice(&buf[..avail_bytes]);
        assert!(self.wbuf.len() <= self.wbuf.capacity(),
                "copy overflowed buffer");
        Ok(avail_bytes)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.borrow_mut().write_all(&self.wbuf)?;
        self.inner.borrow_mut().flush()?;
        self.wbuf.clear();
        Ok(())
    }
}

/// Factory for creating instances of `TBufferedTransport`
#[derive(Default)]
pub struct TBufferedTransportFactory;

impl TBufferedTransportFactory {
    /// Create a `TBufferedTransportFactory`.
    pub fn new() -> TBufferedTransportFactory {
        TBufferedTransportFactory {}
    }
}

impl TTransportFactory for TBufferedTransportFactory {
    fn create(&self, inner: Rc<RefCell<Box<TTransport>>>) -> Box<TTransport> {
        Box::new(TBufferedTransport::new(inner)) as Box<TTransport>
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::io::{Read, Write};
    use std::rc::Rc;

    use super::*;
    use ::transport::{TPassThruTransport, TTransport};
    use ::transport::mem::TBufferTransport;

    macro_rules! new_transports {
        ($wbc:expr, $rbc:expr) => (
            {
                let mem = Rc::new(RefCell::new(Box::new(TBufferTransport::with_capacity($wbc, $rbc))));
                let thru: Box<TTransport> = Box::new(TPassThruTransport { inner: mem.clone() });
                let thru = Rc::new(RefCell::new(thru));
                (mem, thru)
            }
        );
    }

    #[test]
    fn must_return_zero_if_read_buffer_is_empty() {
        let (_, thru) = new_transports!(10, 0);
        let mut t = TBufferedTransport::with_capacity(10, 0, thru);

        let mut b = vec![0; 10];
        let read_result = t.read(&mut b);

        assert_eq!(read_result.unwrap(), 0);
    }

    #[test]
    fn must_return_zero_if_caller_reads_into_zero_capacity_buffer() {
        let (_, thru) = new_transports!(10, 0);
        let mut t = TBufferedTransport::with_capacity(10, 0, thru);

        let read_result = t.read(&mut []);

        assert_eq!(read_result.unwrap(), 0);
    }

    #[test]
    fn must_return_zero_if_nothing_more_can_be_read() {
        let (mem, thru) = new_transports!(4, 0);
        let mut t = TBufferedTransport::with_capacity(4, 0, thru);

        mem.borrow_mut().set_readable_bytes(&[0, 1, 2, 3]);

        // read buffer is exactly the same size as bytes available
        let mut buf = vec![0u8; 4];
        let read_result = t.read(&mut buf);

        // we've read exactly 4 bytes
        assert_eq!(read_result.unwrap(), 4);
        assert_eq!(&buf, &[0, 1, 2, 3]);

        // try read again
        let buf_again = vec![0u8; 4];
        let read_result = t.read(&mut buf);

        // this time, 0 bytes and we haven't changed the buffer
        assert_eq!(read_result.unwrap(), 0);
        assert_eq!(&buf_again, &[0, 0, 0, 0])
    }

    #[test]
    fn must_fill_user_buffer_with_only_as_many_bytes_as_available() {
        let (mem, thru) = new_transports!(4, 0);
        let mut t = TBufferedTransport::with_capacity(4, 0, thru);

        mem.borrow_mut().set_readable_bytes(&[0, 1, 2, 3]);

        // read buffer is much larger than the bytes available
        let mut buf = vec![0u8; 8];
        let read_result = t.read(&mut buf);

        // we've read exactly 4 bytes
        assert_eq!(read_result.unwrap(), 4);
        assert_eq!(&buf[..4], &[0, 1, 2, 3]);

        // try read again
        let read_result = t.read(&mut buf[4..]);

        // this time, 0 bytes and we haven't changed the buffer
        assert_eq!(read_result.unwrap(), 0);
        assert_eq!(&buf, &[0, 1, 2, 3, 0, 0, 0, 0])
    }

    #[test]
    fn must_read_successfully() {
        // this test involves a few loops within the buffered transport
        // itself where it has to drain the underlying transport in order
        // to service a read

        // we have a much smaller buffer than the
        // underlying transport has bytes available
        let (mem, thru) = new_transports!(10, 0);
        let mut t = TBufferedTransport::with_capacity(2, 0, thru);

        // fill the underlying transport's byte buffer
        let mut readable_bytes = [0u8; 10];
        for i in 0..10 {
            readable_bytes[i] = i as u8;
        }
        mem.borrow_mut().set_readable_bytes(&readable_bytes);

        // we ask to read into a buffer that's much larger
        // than the one the buffered transport has; as a result
        // it's going to have to keep asking the underlying
        // transport for more bytes
        let mut buf = [0u8; 8];
        let read_result = t.read(&mut buf);

        // we should have read 8 bytes
        assert_eq!(read_result.unwrap(), 8);
        assert_eq!(&buf, &[0, 1, 2, 3, 4, 5, 6, 7]);

        // let's clear out the buffer and try read again
        for i in 0..8 {
            buf[i] = 0;
        }
        let read_result = t.read(&mut buf);

        // this time we were only able to read 2 bytes
        // (all that's remaining from the underlying transport)
        // let's also check that the remaining bytes are untouched
        assert_eq!(read_result.unwrap(), 2);
        assert_eq!(&buf[0..2], &[8, 9]);
        assert_eq!(&buf[2..], &[0, 0, 0, 0, 0, 0]);

        // try read again (we should get 0)
        // and all the existing bytes were untouched
        let read_result = t.read(&mut buf);
        assert_eq!(read_result.unwrap(), 0);
        assert_eq!(&buf[0..2], &[8, 9]);
        assert_eq!(&buf[2..], &[0, 0, 0, 0, 0, 0]);
    }

    #[test]
    fn must_return_zero_if_nothing_can_be_written() {
        let (_, thru) = new_transports!(0, 0);
        let mut t = TBufferedTransport::with_capacity(0, 0, thru);

        let b = vec![0; 10];
        let r = t.write(&b);

        assert_eq!(r.unwrap(), 0);
    }

    #[test]
    fn must_return_zero_if_caller_calls_write_with_empty_buffer() {
        let (mem, thru) = new_transports!(0, 10);
        let mut t = TBufferedTransport::with_capacity(0, 10, thru);

        let r = t.write(&[]);

        assert_eq!(r.unwrap(), 0);
        assert_eq!(mem.borrow_mut().write_buffer_as_ref(), &[]);
    }

    #[test]
    fn must_return_zero_if_write_buffer_full() {
        let (_, thru) = new_transports!(0, 0);
        let mut t = TBufferedTransport::with_capacity(0, 4, thru);

        let b = [0x00, 0x01, 0x02, 0x03];

        // we've now filled the write buffer
        let r = t.write(&b);
        assert_eq!(r.unwrap(), 4);

        // try write the same bytes again - nothing should be writable
        let r = t.write(&b);
        assert_eq!(r.unwrap(), 0);
    }

    #[test]
    fn must_only_write_to_inner_transport_on_flush() {
        let (mem, thru) = new_transports!(10, 10);
        let mut t = TBufferedTransport::new(thru);

        let b: [u8; 5] = [0, 1, 2, 3, 4];
        assert_eq!(t.write(&b).unwrap(), 5);
        assert_eq!(mem.borrow_mut().write_buffer_as_ref().len(), 0);

        assert!(t.flush().is_ok());

        {
            let inner = mem.borrow_mut();
            let underlying_buffer = inner.write_buffer_as_ref();
            assert_eq!(b, underlying_buffer);
        }
    }

    #[test]
    fn must_write_successfully_after_flush() {
        let (mem, thru) = new_transports!(0, 5);
        let mut t = TBufferedTransport::with_capacity(0, 5, thru);

        // write and flush
        let b: [u8; 5] = [0, 1, 2, 3, 4];
        assert_eq!(t.write(&b).unwrap(), 5);
        assert!(t.flush().is_ok());

        // check the flushed bytes
        {
            let inner = mem.borrow_mut();
            let underlying_buffer = inner.write_buffer_as_ref();
            assert_eq!(b, underlying_buffer);
        }

        // reset our underlying transport
        mem.borrow_mut().empty_write_buffer();

        // write and flush again
        assert_eq!(t.write(&b).unwrap(), 5);
        assert!(t.flush().is_ok());

        // check the flushed bytes
        {
            let inner = mem.borrow_mut();
            let underlying_buffer = inner.write_buffer_as_ref();
            assert_eq!(b, underlying_buffer);
        }
    }
}
