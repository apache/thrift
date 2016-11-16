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

use std::io;
use std::io::ErrorKind;
use std::net::{Shutdown, TcpStream};

use super::TTransport;

/// Implementation of [`TTransport`][ttransport] that
/// sends and receives data over TCP.
///
/// [ttransport] traits.TTransport.html
pub struct TTcpTransport {
    remote_address: String,
    stream: Option<TcpStream>,
}

impl TTcpTransport {
    pub fn new(remote_address: &str) -> TTcpTransport {
       TTcpTransport {
           remote_address: remote_address.to_owned(),
           stream: None,
       }
    }

    fn if_set<F, T>(&mut self, mut stream_operation: F) -> io::Result<T>
        where F: FnMut(&mut TcpStream) -> io::Result<T> {

        if let Some(ref mut s) = self.stream {
            stream_operation(s)
        } else {
            Err(io::Error::new(ErrorKind::NotConnected, "tcp endpoint not connected"))
        }
    }
}

impl io::Read for TTcpTransport {
    fn read(&mut self, b: &mut [u8]) -> io::Result<usize> {
        self.if_set(|s| s.read(b))
    }
}

impl io::Write for TTcpTransport {
    fn write(&mut self, b: &[u8]) -> io::Result<usize> {
        self.if_set(|s| s.write(b))
    }

    fn flush(&mut self) -> io::Result<()> {
        self.if_set(|s| s.flush())
    }
}

impl TTransport for TTcpTransport {
    fn open(&mut self) -> io::Result<()> {
        if let Some(_) = self.stream {
            Err(io::Error::new(ErrorKind::AlreadyExists, "transport opened previously"))
        } else {
            // note: I have to do this because rust does not
            // do auto-deref coercion during trait matching
            // see: http://stackoverflow.com/questions/30412011/coercing-string-to-strI
            match TcpStream::connect(&self.remote_address[..]) {
                Ok(s) => {
                    self.stream = Some(s);
                    Ok(())
                }
                Err(e) => Err(e)
            }
        }
    }

    fn close(&mut self) -> io::Result<()> {
        self.if_set(|s| s.shutdown(Shutdown::Both))
    }
}
