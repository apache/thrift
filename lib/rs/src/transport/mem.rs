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
    pub fn with_capacity(read_buffer_size: usize, write_buffer_size: usize) -> TBufferTransport {
        TBufferTransport {
            rbuf: vec![0; read_buffer_size].into_boxed_slice(),
            ridx: 0,
            rpos: 0,
            rcap: read_buffer_size,
            wbuf: vec![0; write_buffer_size].into_boxed_slice(),
            wpos: 0,
            wcap: write_buffer_size,
        }
    }

    pub fn set_read_buffer(&mut self, buf: &[u8]) -> usize {
        let max_bytes = cmp::min(self.rcap, buf.len());
        self.rbuf[..max_bytes].clone_from_slice(&buf[..max_bytes]);
        self.ridx = max_bytes;
        max_bytes
    }

    pub fn reset_read_buffer(&mut self) {
        self.rpos = 0;
        self.ridx = 0;
    }

    pub fn write_buffer(&self) -> &[u8] {
        &self.wbuf[..self.wpos]
    }

    pub fn reset_write_buffer(&mut self) {
        self.wpos = 0;
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
