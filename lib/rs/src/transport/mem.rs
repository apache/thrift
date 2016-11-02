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

/// Simple transport that contains both a fixed-length internal read buffer and
/// a fixed-length internal write buffer.
///
/// On a `write` bytes are written to the internal write buffer. Writes are no
/// longer accepted once this buffer is full. Callers must `empty_write_buffer()`
/// before subsequent writes are accepted.
///
/// You can set readable bytes in the internal read buffer by filling it with
/// `set_readable_bytes(...)`. Callers can then read until the buffer is
/// depleted. No further reads are accepted until the internal read buffer is
/// replenished again.
pub struct TBufferTransport {
    rbuf: Box<[u8]>,
    rpos: usize,
    ridx: usize,
    rcap: usize,
    wbuf: Box<[u8]>,
    wpos: usize,
    wcap: usize,
}

impl TBufferTransport {
    /// Constructs a new, empty `TBufferTransport` with the given
    /// read buffer capacity and write buffer capacity.
    pub fn with_capacity(read_buffer_capacity: usize,
                         write_buffer_capacity: usize)
                         -> TBufferTransport {
        TBufferTransport {
            rbuf: vec![0; read_buffer_capacity].into_boxed_slice(),
            ridx: 0,
            rpos: 0,
            rcap: read_buffer_capacity,
            wbuf: vec![0; write_buffer_capacity].into_boxed_slice(),
            wpos: 0,
            wcap: write_buffer_capacity,
        }
    }

    /// Return a slice containing the bytes held by the internal read buffer.
    /// Returns an empty slice if no readable bytes are present.
    pub fn read_buffer(&self) -> &[u8] {
        &self.rbuf[..self.ridx]
    }

    // FIXME: do I really need this API call?
    // FIXME: should this simply reset to the last set of readable bytes?
    /// Reset the number of readable bytes to zero.
    ///
    /// Subsequent calls to `read` will return nothing.
    pub fn empty_read_buffer(&mut self) {
        self.rpos = 0;
        self.ridx = 0;
    }

    /// Copy bytes from the source buffer `buf` into the internal read buffer,
    /// overwriting any existing bytes. Returns the number of bytes copied,
    /// which is `min(buf.len(), internal_read_buf.len())`.
    pub fn set_readable_bytes(&mut self, buf: &[u8]) -> usize {
        self.empty_read_buffer();
        let max_bytes = cmp::min(self.rcap, buf.len());
        self.rbuf[..max_bytes].clone_from_slice(&buf[..max_bytes]);
        self.ridx = max_bytes;
        max_bytes
    }

    /// Return a slice containing the bytes held by the internal write buffer.
    /// Returns an empty slice if no bytes were written.
    pub fn write_buffer_as_ref(&self) -> &[u8] {
        &self.wbuf[..self.wpos]
    }

    /// Return a vector with a copy of the bytes held by the internal write buffer.
    /// Returns an empty vector if no bytes were written.
    pub fn write_buffer_to_vec(&self) -> Vec<u8> {
        let mut buf = vec![0u8; self.wpos];
        buf.copy_from_slice(&self.wbuf[..self.wpos]);
        buf
    }

    /// Resets the internal write buffer, making it seem like no bytes were
    /// written. Calling `write_buffer` after this returns an empty slice.
    pub fn empty_write_buffer(&mut self) {
        self.wpos = 0;
    }

    /// Overwrites the contents of the read buffer with the contents of the
    /// write buffer. The write buffer is emptied after this operation.
    pub fn copy_write_buffer_to_read_buffer(&mut self) {
        let buf = {
            let b = self.write_buffer_as_ref();
            let mut b_ret = vec![0; b.len()];
            b_ret.copy_from_slice(&b);
            b_ret
        };

        let bytes_copied = self.set_readable_bytes(&buf);
        assert_eq!(bytes_copied, buf.len());

        self.empty_write_buffer();
    }
}

impl io::Read for TBufferTransport {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let nread = cmp::min(buf.len(), self.ridx - self.rpos);
        buf[..nread].clone_from_slice(&self.rbuf[self.rpos..self.rpos + nread]);
        self.rpos += nread;
        Ok(nread)
    }
}

impl io::Write for TBufferTransport {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let nwrite = cmp::min(buf.len(), self.wcap - self.wpos);
        self.wbuf[self.wpos..self.wpos + nwrite].clone_from_slice(&buf[..nwrite]);
        self.wpos += nwrite;
        Ok(nwrite)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(()) // nothing to do on flush
    }
}

#[cfg(test)]
mod tests {
    use std::io::{Read, Write};

    use super::TBufferTransport;

    #[test]
    fn must_empty_write_buffer() {
        let mut t = TBufferTransport::with_capacity(0, 1);

        let bytes_to_write: [u8; 1] = [0x01];
        let result = t.write(&bytes_to_write);
        assert_eq!(result.unwrap(), 1);
        assert_eq!(&t.write_buffer_as_ref(), &bytes_to_write);

        t.empty_write_buffer();
        assert_eq!(t.write_buffer_as_ref().len(), 0);
    }

    #[test]
    fn must_accept_writes_after_buffer_emptied() {
        let mut t = TBufferTransport::with_capacity(0, 2);

        let bytes_to_write: [u8; 2] = [0x01, 0x02];

        // first write (all bytes written)
        let result = t.write(&bytes_to_write);
        assert_eq!(result.unwrap(), 2);
        assert_eq!(&t.write_buffer_as_ref(), &bytes_to_write);

        // try write again (nothing should be written)
        let result = t.write(&bytes_to_write);
        assert_eq!(result.unwrap(), 0);
        assert_eq!(&t.write_buffer_as_ref(), &bytes_to_write); // still the same as before

        // now reset the buffer
        t.empty_write_buffer();
        assert_eq!(t.write_buffer_as_ref().len(), 0);

        // now try write again - the write should succeed
        let result = t.write(&bytes_to_write);
        assert_eq!(result.unwrap(), 2);
        assert_eq!(&t.write_buffer_as_ref(), &bytes_to_write);
    }

    #[test]
    fn must_accept_multiple_writes_until_buffer_is_full() {
        let mut t = TBufferTransport::with_capacity(0, 10);

        // first write (all bytes written)
        let bytes_to_write_0: [u8; 2] = [0x01, 0x41];
        let write_0_result = t.write(&bytes_to_write_0);
        assert_eq!(write_0_result.unwrap(), 2);
        assert_eq!(t.write_buffer_as_ref(), &bytes_to_write_0);

        // second write (all bytes written, starting at index 2)
        let bytes_to_write_1: [u8; 7] = [0x24, 0x41, 0x32, 0x33, 0x11, 0x98, 0xAF];
        let write_1_result = t.write(&bytes_to_write_1);
        assert_eq!(write_1_result.unwrap(), 7);
        assert_eq!(&t.write_buffer_as_ref()[2..], &bytes_to_write_1);

        // third write (only 1 byte written - that's all we have space for)
        let bytes_to_write_2: [u8; 3] = [0xBF, 0xDA, 0x98];
        let write_2_result = t.write(&bytes_to_write_2);
        assert_eq!(write_2_result.unwrap(), 1);
        assert_eq!(&t.write_buffer_as_ref()[9..], &bytes_to_write_2[0..1]); // how does this syntax work?!

        // fourth write (no writes are accepted)
        let bytes_to_write_3: [u8; 3] = [0xBF, 0xAA, 0xFD];
        let write_3_result = t.write(&bytes_to_write_3);
        assert_eq!(write_3_result.unwrap(), 0);

        // check the full write buffer
        let mut expected: Vec<u8> = Vec::with_capacity(10);
        expected.extend_from_slice(&bytes_to_write_0);
        expected.extend_from_slice(&bytes_to_write_1);
        expected.extend_from_slice(&bytes_to_write_2[0..1]);
        assert_eq!(t.write_buffer_as_ref(), &expected[..]);
    }

    #[test]
    fn must_empty_read_buffer() {
        let mut t = TBufferTransport::with_capacity(1, 0);

        let bytes_to_read: [u8; 1] = [0x01];
        let result = t.set_readable_bytes(&bytes_to_read);
        assert_eq!(result, 1);
        assert_eq!(&t.read_buffer(), &bytes_to_read);

        t.empty_read_buffer();
        assert_eq!(t.read_buffer().len(), 0);
    }

    #[test]
    fn must_allow_readable_bytes_to_be_set_after_read_buffer_emptied() {
        let mut t = TBufferTransport::with_capacity(1, 0);

        let bytes_to_read_0: [u8; 1] = [0x01];
        let result = t.set_readable_bytes(&bytes_to_read_0);
        assert_eq!(result, 1);
        assert_eq!(&t.read_buffer(), &bytes_to_read_0);

        t.empty_read_buffer();
        assert_eq!(t.read_buffer().len(), 0);

        let bytes_to_read_1: [u8; 1] = [0x02];
        let result = t.set_readable_bytes(&bytes_to_read_1);
        assert_eq!(result, 1);
        assert_eq!(&t.read_buffer(), &bytes_to_read_1);
    }

    #[test]
    fn must_accept_multiple_reads_until_all_bytes_read() {
        let mut t = TBufferTransport::with_capacity(10, 0);

        let readable_bytes: [u8; 10] = [0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0x00, 0x1A, 0x2B, 0x3C, 0x4D];

        // check that we're able to set the bytes to be read
        let result = t.set_readable_bytes(&readable_bytes);
        assert_eq!(result, 10);
        assert_eq!(&t.read_buffer(), &readable_bytes);

        // first read
        let mut read_buf_0 = vec![0; 5];
        let read_result = t.read(&mut read_buf_0);
        assert_eq!(read_result.unwrap(), 5);
        assert_eq!(read_buf_0.as_slice(), &(readable_bytes[0..5]));

        // second read
        let mut read_buf_1 = vec![0; 4];
        let read_result = t.read(&mut read_buf_1);
        assert_eq!(read_result.unwrap(), 4);
        assert_eq!(read_buf_1.as_slice(), &(readable_bytes[5..9]));

        // third read (only 1 byte remains to be read)
        let mut read_buf_2 = vec![0; 3];
        let read_result = t.read(&mut read_buf_2);
        assert_eq!(read_result.unwrap(), 1);
        read_buf_2.truncate(1); // FIXME: does the caller have to do this?
        assert_eq!(read_buf_2.as_slice(), &(readable_bytes[9..]));

        // fourth read (nothing should be readable)
        let mut read_buf_3 = vec![0; 10];
        let read_result = t.read(&mut read_buf_3);
        assert_eq!(read_result.unwrap(), 0);
        read_buf_3.truncate(0);

        // check that all the bytes we received match the original (again!)
        let mut bytes_read = Vec::with_capacity(10);
        bytes_read.extend_from_slice(&read_buf_0);
        bytes_read.extend_from_slice(&read_buf_1);
        bytes_read.extend_from_slice(&read_buf_2);
        bytes_read.extend_from_slice(&read_buf_3);
        assert_eq!(&bytes_read, &readable_bytes);
    }

    #[test]
    fn must_allow_reads_to_succeed_after_read_buffer_replenished() {
        let mut t = TBufferTransport::with_capacity(3, 0);

        let readable_bytes_0: [u8; 3] = [0x02, 0xAB, 0x33];

        // check that we're able to set the bytes to be read
        let result = t.set_readable_bytes(&readable_bytes_0);
        assert_eq!(result, 3);
        assert_eq!(&t.read_buffer(), &readable_bytes_0);

        let mut read_buf = vec![0; 4];

        // drain the read buffer
        let read_result = t.read(&mut read_buf);
        assert_eq!(read_result.unwrap(), 3);
        assert_eq!(t.read_buffer(), &read_buf[0..3]);

        // check that a subsequent read fails
        let read_result = t.read(&mut read_buf);
        assert_eq!(read_result.unwrap(), 0);

        // we don't modify the read buffer on failure
        let mut expected_bytes = Vec::with_capacity(4);
        expected_bytes.extend_from_slice(&readable_bytes_0);
        expected_bytes.push(0x00);
        assert_eq!(&read_buf, &expected_bytes);

        // replenish the read buffer again
        let readable_bytes_1: [u8; 2] = [0x91, 0xAA];

        // check that we're able to set the bytes to be read
        let result = t.set_readable_bytes(&readable_bytes_1);
        assert_eq!(result, 2);
        assert_eq!(&t.read_buffer(), &readable_bytes_1);

        // read again
        let read_result = t.read(&mut read_buf);
        assert_eq!(read_result.unwrap(), 2);
        assert_eq!(t.read_buffer(), &read_buf[0..2]);
    }
}
