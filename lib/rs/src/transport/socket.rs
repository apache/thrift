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

use std::convert::From;
use std::io;
use std::io::{ErrorKind, Read, Write};
use std::net::{Shutdown, TcpStream};
use std::ops::Drop;

use ::{TransportError, TransportErrorKind};

/// Sends and receives bytes over a TCP socket.
pub struct TTcpTransport {
    stream: Option<TcpStream>,
}

impl TTcpTransport {
    /// Creates an uninitialized `TTcpTransport`.
    /// This instance must be opened using `open(...)`
    /// before it can be used.
    pub fn new() -> TTcpTransport {
       TTcpTransport {
           stream: None,
       }
    }

    /// Creates a `TTcpTransport` that wraps an
    /// existing `TcpStream`. The stream is assumed
    /// to have been opened and ready before being
    /// wrapped in a `TTcpTransport` instance.
    pub fn using_stream(stream: TcpStream) -> TTcpTransport {
       TTcpTransport {
           stream: Some(stream),
       }
    }

    pub fn open(&mut self, remote_address: &str) -> ::Result<()> {
        if let Some(_) = self.stream {
            Err(
                ::Error::Transport(
                    TransportError {
                        kind: TransportErrorKind::AlreadyOpen,
                        message: "transport previously opened".to_owned()
                    }
                )
            )
        } else {
            match TcpStream::connect(&remote_address) {
                Ok(s) => {
                    self.stream = Some(s);
                    Ok(())
                }
                Err(e) => Err(From::from(e))
            }
        }
    }

    pub fn close(&mut self) -> ::Result<()> {
        self.if_set(|s| s.shutdown(Shutdown::Both)).map_err(From::from)
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

impl Read for TTcpTransport {
    fn read(&mut self, b: &mut [u8]) -> io::Result<usize> {
        self.if_set(|s| s.read(b))
    }
}

impl Write for TTcpTransport {
    fn write(&mut self, b: &[u8]) -> io::Result<usize> {
        self.if_set(|s| s.write(b))
    }

    fn flush(&mut self) -> io::Result<()> {
        self.if_set(|s| s.flush())
    }
}

// Do I have to implement the Drop trait? TcpStream closes the socket on drop.
impl Drop for TTcpTransport {
    fn drop(&mut self) {
        if let Err(e) = self.close() {
            warn!("error while closing socket transport: {:?}", e)
        }
    }
}
