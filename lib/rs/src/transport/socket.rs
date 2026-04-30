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
use std::net::{Shutdown, TcpStream, ToSocketAddrs};
use std::time::Duration;

#[cfg(unix)]
use std::os::unix::net::UnixStream;

use super::{ReadHalf, TIoChannel, WriteHalf};
use crate::{new_transport_error, TransportErrorKind};

/// Bidirectional TCP/IP channel.
///
/// # Examples
///
/// Create a `TTcpChannel`.
///
/// ```no_run
/// use std::io::{Read, Write};
/// use thrift::transport::TTcpChannel;
///
/// let mut c = TTcpChannel::new();
/// c.open("localhost:9090").unwrap();
///
/// let mut buf = vec![0u8; 4];
/// c.read(&mut buf).unwrap();
/// c.write(&vec![0, 1, 2]).unwrap();
/// ```
///
/// Create a `TTcpChannel` by wrapping an existing `TcpStream`.
///
/// ```no_run
/// use std::io::{Read, Write};
/// use std::net::TcpStream;
/// use thrift::transport::TTcpChannel;
///
/// let stream = TcpStream::connect("127.0.0.1:9189").unwrap();
/// stream.set_nodelay(true).unwrap();
///
/// // no need to call c.open() since we've already connected above
/// let mut c = TTcpChannel::with_stream(stream);
///
/// let mut buf = vec![0u8; 4];
/// c.read(&mut buf).unwrap();
/// c.write(&vec![0, 1, 2]).unwrap();
/// ```
#[derive(Debug, Default)]
pub struct TTcpChannel {
    stream: Option<TcpStream>,
    read_timeout: Option<Duration>,
    write_timeout: Option<Duration>,
}

impl TTcpChannel {
    /// Create an uninitialized `TTcpChannel`.
    ///
    /// The returned instance must be opened using `TTcpChannel::open(...)`
    /// before it can be used.
    pub fn new() -> TTcpChannel {
        TTcpChannel {
            stream: None,
            read_timeout: None,
            write_timeout: None,
        }
    }

    /// Create a `TTcpChannel` that wraps an existing `TcpStream`.
    ///
    /// The passed-in stream is assumed to have been opened before being wrapped
    /// by the created `TTcpChannel` instance.
    pub fn with_stream(stream: TcpStream) -> TTcpChannel {
        let read_timeout = stream.read_timeout().unwrap_or(None);
        let write_timeout = stream.write_timeout().unwrap_or(None);

        TTcpChannel {
            stream: Some(stream),
            read_timeout,
            write_timeout,
        }
    }

    /// Get the read timeout for this channel.
    pub fn read_timeout(&self) -> Option<Duration> {
        self.read_timeout
    }

    /// Get the write timeout for this channel.
    pub fn write_timeout(&self) -> Option<Duration> {
        self.write_timeout
    }

    /// Set the read timeout for this channel.
    pub fn set_read_timeout(&mut self, timeout: Option<Duration>) -> crate::Result<()> {
        if let Some(ref stream) = self.stream {
            stream.set_read_timeout(timeout)?;
        }

        self.read_timeout = timeout;
        Ok(())
    }

    /// Set the write timeout for this channel.
    pub fn set_write_timeout(&mut self, timeout: Option<Duration>) -> crate::Result<()> {
        if let Some(ref stream) = self.stream {
            stream.set_write_timeout(timeout)?;
        }

        self.write_timeout = timeout;
        Ok(())
    }

    /// Set the read and write timeouts for this channel.
    pub fn set_timeouts(
        &mut self,
        read_timeout: Option<Duration>,
        write_timeout: Option<Duration>,
    ) -> crate::Result<()> {
        self.set_read_timeout(read_timeout)?;
        self.set_write_timeout(write_timeout)?;
        Ok(())
    }

    /// Connect to `remote_address`, which should implement `ToSocketAddrs` trait.
    pub fn open<A: ToSocketAddrs>(&mut self, remote_address: A) -> crate::Result<()> {
        if self.stream.is_some() {
            Err(new_transport_error(
                TransportErrorKind::AlreadyOpen,
                "tcp connection previously opened",
            ))
        } else {
            match TcpStream::connect(&remote_address) {
                Ok(s) => {
                    s.set_nodelay(true)?;
                    s.set_read_timeout(self.read_timeout)?;
                    s.set_write_timeout(self.write_timeout)?;
                    self.stream = Some(s);
                    Ok(())
                }
                Err(e) => Err(From::from(e)),
            }
        }
    }

    /// Shut down this channel.
    ///
    /// Both send and receive halves are closed, and this instance can no
    /// longer be used to communicate with another endpoint.
    pub fn close(&mut self) -> crate::Result<()> {
        self.if_set(|s| s.shutdown(Shutdown::Both))
            .map_err(From::from)
    }

    fn if_set<F, T>(&mut self, mut stream_operation: F) -> io::Result<T>
    where
        F: FnMut(&mut TcpStream) -> io::Result<T>,
    {
        if let Some(ref mut s) = self.stream {
            stream_operation(s)
        } else {
            Err(io::Error::new(
                ErrorKind::NotConnected,
                "tcp endpoint not connected",
            ))
        }
    }
}

impl TIoChannel for TTcpChannel {
    fn split(self) -> crate::Result<(ReadHalf<Self>, WriteHalf<Self>)>
    where
        Self: Sized,
    {
        let mut s = self;

        s.stream
            .as_mut()
            .and_then(|s| s.try_clone().ok())
            .map(|cloned| {
                let read_half = ReadHalf::new(TTcpChannel {
                    stream: s.stream.take(),
                    read_timeout: s.read_timeout,
                    write_timeout: s.write_timeout,
                });
                let write_half = WriteHalf::new(TTcpChannel {
                    stream: Some(cloned),
                    read_timeout: s.read_timeout,
                    write_timeout: s.write_timeout,
                });
                (read_half, write_half)
            })
            .ok_or_else(|| {
                new_transport_error(
                    TransportErrorKind::Unknown,
                    "cannot clone underlying tcp stream",
                )
            })
    }
}

impl Read for TTcpChannel {
    fn read(&mut self, b: &mut [u8]) -> io::Result<usize> {
        self.if_set(|s| s.read(b))
    }
}

impl Write for TTcpChannel {
    fn write(&mut self, b: &[u8]) -> io::Result<usize> {
        self.if_set(|s| s.write(b))
    }

    fn flush(&mut self) -> io::Result<()> {
        self.if_set(|s| s.flush())
    }
}

#[cfg(unix)]
impl TIoChannel for UnixStream {
    fn split(self) -> crate::Result<(ReadHalf<Self>, WriteHalf<Self>)>
    where
        Self: Sized,
    {
        let socket_rx = self.try_clone().unwrap();

        Ok((ReadHalf::new(self), WriteHalf::new(socket_rx)))
    }
}

#[cfg(test)]
mod tests {
    use std::net::{SocketAddr, TcpListener, TcpStream};
    use std::thread;
    use std::time::Duration;

    use super::*;

    fn listening_address() -> (TcpListener, SocketAddr) {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let address = listener.local_addr().unwrap();
        (listener, address)
    }

    fn connected_streams() -> (TcpStream, TcpStream) {
        let (listener, address) = listening_address();
        let accept_handle = thread::spawn(move || listener.accept().unwrap().0);
        let client = TcpStream::connect(address).unwrap();
        let server = accept_handle.join().unwrap();
        (client, server)
    }

    fn wrapped_channel() -> (TTcpChannel, TcpStream) {
        let (client, server) = connected_streams();
        (TTcpChannel::with_stream(client), server)
    }

    #[test]
    fn must_store_read_timeout_before_open() {
        let timeout = Some(Duration::from_millis(80));
        let mut channel = TTcpChannel::new();

        channel.set_read_timeout(timeout).unwrap();

        assert_eq!(channel.read_timeout, timeout);
        assert_eq!(channel.read_timeout(), timeout);
        assert!(channel.stream.is_none());
    }

    #[test]
    fn must_apply_timeouts_when_opening_channel() {
        let timeout = Some(Duration::from_millis(80));
        let (listener, address) = listening_address();
        let accept_handle = thread::spawn(move || listener.accept().unwrap().0);
        let mut channel = TTcpChannel::new();

        channel.set_timeouts(timeout, timeout).unwrap();
        channel.open(address).unwrap();

        let stream = channel.stream.as_ref().unwrap();
        assert_eq!(stream.read_timeout().unwrap(), timeout);
        assert_eq!(stream.write_timeout().unwrap(), timeout);
        let _server = accept_handle.join().unwrap();
    }

    #[test]
    fn must_enforce_read_timeout_set_before_open() {
        let timeout = Duration::from_millis(80);
        let (listener, address) = listening_address();
        let server_handle = thread::spawn(move || {
            let stream = listener.accept().unwrap().0;
            thread::sleep(Duration::from_millis(200));
            drop(stream);
        });
        let mut channel = TTcpChannel::new();
        let mut buf = [0; 1];

        channel.set_read_timeout(Some(timeout)).unwrap();
        channel.open(address).unwrap();

        let err = channel.read(&mut buf).unwrap_err();

        assert!(matches!(
            err.kind(),
            ErrorKind::TimedOut | ErrorKind::WouldBlock
        ));
        server_handle.join().unwrap();
    }

    #[test]
    fn must_set_read_timeout_on_wrapped_stream() {
        let timeout = Some(Duration::from_millis(80));
        let (mut channel, _server) = wrapped_channel();

        channel.set_read_timeout(timeout).unwrap();

        assert_eq!(channel.read_timeout, timeout);
        assert_eq!(channel.read_timeout(), timeout);
        assert_eq!(
            channel.stream.as_ref().unwrap().read_timeout().unwrap(),
            timeout
        );
    }

    #[test]
    fn must_set_write_timeout_on_wrapped_stream() {
        let timeout = Some(Duration::from_millis(80));
        let (mut channel, _server) = wrapped_channel();

        channel.set_write_timeout(timeout).unwrap();

        assert_eq!(channel.write_timeout, timeout);
        assert_eq!(channel.write_timeout(), timeout);
        assert_eq!(
            channel.stream.as_ref().unwrap().write_timeout().unwrap(),
            timeout
        );
    }

    #[test]
    fn must_set_both_timeouts_on_wrapped_stream() {
        let read_timeout = Some(Duration::from_millis(80));
        let write_timeout = Some(Duration::from_millis(120));
        let (mut channel, _server) = wrapped_channel();

        channel.set_timeouts(read_timeout, write_timeout).unwrap();

        assert_eq!(channel.read_timeout, read_timeout);
        assert_eq!(channel.write_timeout, write_timeout);
        assert_eq!(channel.read_timeout(), read_timeout);
        assert_eq!(channel.write_timeout(), write_timeout);
        assert_eq!(
            channel.stream.as_ref().unwrap().read_timeout().unwrap(),
            read_timeout
        );
        assert_eq!(
            channel.stream.as_ref().unwrap().write_timeout().unwrap(),
            write_timeout
        );
    }

    #[test]
    fn must_clear_timeouts_on_wrapped_stream() {
        let timeout = Some(Duration::from_millis(80));
        let (mut channel, _server) = wrapped_channel();

        channel.set_timeouts(timeout, timeout).unwrap();
        channel.set_timeouts(None, None).unwrap();

        assert_eq!(channel.read_timeout, None);
        assert_eq!(channel.write_timeout, None);
        assert_eq!(channel.read_timeout(), None);
        assert_eq!(channel.write_timeout(), None);
        assert_eq!(
            channel.stream.as_ref().unwrap().read_timeout().unwrap(),
            None
        );
        assert_eq!(
            channel.stream.as_ref().unwrap().write_timeout().unwrap(),
            None
        );
    }

    #[test]
    fn must_store_timeouts_from_wrapped_stream() {
        let read_timeout = Some(Duration::from_millis(80));
        let write_timeout = Some(Duration::from_millis(120));
        let (client, _server) = connected_streams();

        client.set_read_timeout(read_timeout).unwrap();
        client.set_write_timeout(write_timeout).unwrap();
        let channel = TTcpChannel::with_stream(client);

        assert_eq!(channel.read_timeout, read_timeout);
        assert_eq!(channel.write_timeout, write_timeout);
        assert_eq!(channel.read_timeout(), read_timeout);
        assert_eq!(channel.write_timeout(), write_timeout);
    }
}
