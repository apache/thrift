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

/// Communicate with a Thrift service over a TCP socket.
///
/// # Examples
///
/// Create a `TTcpTransport`.
///
/// ```no_run
/// use std::io::{Read, Write};
/// use thrift::transport::TTcpTransport;
///
/// let mut t = TTcpTransport::new();
/// t.open("localhost:9090").unwrap();
///
/// let mut buf = vec![0u8; 4];
/// t.read(&mut buf).unwrap();
/// t.write(&vec![0, 1, 2]).unwrap();
/// ```
///
/// Create a `TTcpTransport` by wrapping an existing `TcpStream`.
///
/// ```no_run
/// use std::io::{Read, Write};
/// use std::net::TcpStream;
/// use thrift::transport::TTcpTransport;
///
/// let stream = TcpStream::connect("127.0.0.1:9189").unwrap();
/// let mut t = TTcpTransport::with_stream(stream);
///
/// // no need to call t.open() since we've already connected above
///
/// let mut buf = vec![0u8; 4];
/// t.read(&mut buf).unwrap();
/// t.write(&vec![0, 1, 2]).unwrap();
/// ```
#[derive(Default)]
pub struct TTcpTransport {
    stream: Option<TcpStream>,
}

impl TTcpTransport {
    /// Create an uninitialized `TTcpTransport`.
    ///
    /// The returned instance must be opened using `TTcpTransport::open(...)`
    /// before it can be used.
    pub fn new() -> TTcpTransport {
        TTcpTransport { stream: None }
    }

    /// Create a `TTcpTransport` that wraps an existing `TcpStream`.
    ///
    /// The passed-in stream is assumed to have been opened before being wrapped
    /// by the created `TTcpTransport` instance.
    pub fn with_stream(stream: TcpStream) -> TTcpTransport {
        TTcpTransport { stream: Some(stream) }
    }

    /// Connect to `remote_address`, which should have the form `host:port`.
    pub fn open(&mut self, remote_address: &str) -> ::Result<()> {
        if self.stream.is_some() {
            Err(::Error::Transport(TransportError::new(TransportErrorKind::AlreadyOpen,
                                                       "transport previously opened")))
        } else {
            match TcpStream::connect(&remote_address) {
                Ok(s) => {
                    self.stream = Some(s);
                    Ok(())
                }
                Err(e) => Err(From::from(e)),
            }
        }
    }

    /// Shutdown this transport.
    ///
    /// Both send and receive halves are closed, and this instance can no
    /// longer be used to communicate with another endpoint.
    pub fn close(&mut self) -> ::Result<()> {
        self.if_set(|s| s.shutdown(Shutdown::Both)).map_err(From::from)
    }

    fn if_set<F, T>(&mut self, mut stream_operation: F) -> io::Result<T>
        where F: FnMut(&mut TcpStream) -> io::Result<T>
    {

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
