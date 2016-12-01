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

use super::TTransport;

/// Default capacity of the read buffer in bytes.
const WRITE_BUFFER_CAPACITY: usize = 4096;

/// Default capacity of the write buffer in bytes..
const DEFAULT_WBUFFER_CAPACITY: usize = 4096;

pub struct TFramedTransport<I: TTransport> {
    rbuf: Box<[u8]>,
    rpos: usize,
    rcap: usize,
    wbuf: Box<[u8]>,
    wpos: usize,
    inner: Rc<RefCell<Box<I>>>,
}

impl <I: TTransport> TFramedTransport<I> {
    pub fn new(inner: Rc<RefCell<Box<I>>>) -> TFramedTransport<I> {
        TFramedTransport::with_capacity(WRITE_BUFFER_CAPACITY, DEFAULT_WBUFFER_CAPACITY, inner)
    }

    pub fn with_capacity(read_buffer_capacity: usize, write_buffer_capacity: usize, inner: Rc<RefCell<Box<I>>>) -> TFramedTransport<I> {
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

impl <I: TTransport> Read for TFramedTransport<I> {
    fn read(&mut self, b: &mut [u8]) -> io::Result<usize> {
        if self.rcap - self.rpos == 0 {
            let message_size = try!(self.inner.borrow_mut().read_i32::<BigEndian>()) as usize;
            if message_size > self.rbuf.len() {
                return Err(
                    io::Error::new(
                        ErrorKind::Other,
                        format!("bytes to be read ({}) exceeds buffer capacity ({})", message_size, self.rbuf.len())
                    )
                );
            }
            try!(self.inner.borrow_mut().read_exact(&mut self.rbuf[..message_size]));
            self.rpos = 0;
            self.rcap = message_size as usize;
        }

        let nread = cmp::min(b.len(), self.rcap - self.rpos);
        b[..nread].clone_from_slice(&self.rbuf[self.rpos..self.rpos + nread]);
        self.rpos += nread;

        Ok(nread)
    }
}

impl <I: TTransport> Write for TFramedTransport<I> {
    fn write(&mut self, b: &[u8]) -> io::Result<usize> {
        if b.len() > (self.wbuf.len() - self.wpos) {
            return Err(
                io::Error::new(
                    ErrorKind::Other,
                    format!("bytes to be written ({}) exceeds buffer capacity ({})", b.len(), self.wbuf.len() - self.wpos)
                )
            );
        }

        let nwrite = b.len(); // always less than available write buffer capacity
        self.wbuf[self.wpos..(self.wpos + nwrite)].clone_from_slice(&b);
        self.wpos += nwrite;
        Ok(nwrite)
    }

    fn flush(&mut self) -> io::Result<()> {
        let message_size = self.wpos;

        if let 0 = message_size {
            return Ok(())
        } else {
            try!(self.inner.borrow_mut().write_i32::<BigEndian>(message_size as i32));
        }

        let mut byte_index = 0;
        while byte_index < self.wpos {
            let nwrite = try!(self.inner.borrow_mut().write(&self.wbuf[byte_index..self.wpos]));
            byte_index = cmp::min(byte_index + nwrite, self.wpos);
        }

        self.wpos = 0;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
//    use std::io::{Read, Write};
//
//    use super::*;
//    use ::transport::mem::TBufferTransport;

    #[test]
    fn foo() {

    }
}
