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

use std::io::{self, Read, Write};
use std::net::{Shutdown, TcpStream, ToSocketAddrs};
use std::ops::{Deref, DerefMut};
use std::sync::Arc;
use std::time::Duration;

use rustls::pki_types::ServerName;
use rustls::{
    ClientConfig, ClientConnection, ConnectionCommon, ServerConfig, ServerConnection, SideData,
    StreamOwned,
};

use super::{ReadHalf, TIoChannel, TSharedChannel, WriteHalf};
use crate::{new_transport_error, TransportErrorKind};

/// A blocking rustls client channel.
///
/// The caller supplies a fully configured rustls [`ClientConfig`], including
/// its crypto provider, server-certificate verifier, optional client identity,
/// and protocol policy. The TLS handshake completes according to that
/// configuration before [`connect`](Self::connect) returns.
#[derive(Clone, Debug)]
pub struct TTlsClientChannel {
    inner: TSharedChannel<StreamOwned<ClientConnection, TcpStream>>,
}

impl TTlsClientChannel {
    /// Connect to `remote_address` and complete a TLS handshake for
    /// `server_name`.
    pub fn connect<A: ToSocketAddrs>(
        remote_address: A,
        server_name: ServerName<'static>,
        config: Arc<ClientConfig>,
    ) -> crate::Result<Self> {
        let stream = TcpStream::connect(remote_address)?;
        stream.set_nodelay(true)?;
        Self::with_stream(stream, server_name, config)
    }

    /// Wrap an already-connected TCP stream and complete a TLS handshake.
    pub fn with_stream(
        mut stream: TcpStream,
        server_name: ServerName<'static>,
        config: Arc<ClientConfig>,
    ) -> crate::Result<Self> {
        let mut connection = ClientConnection::new(config, server_name).map_err(|error| {
            new_transport_error(
                TransportErrorKind::Unknown,
                format!("cannot create TLS client connection: {error}"),
            )
        })?;
        connection
            .complete_io(&mut stream)
            .map_err(|error| handshake_io_error("client", error))?;
        if connection.is_handshaking() {
            return Err(incomplete_handshake_error("client"));
        }

        Ok(Self {
            inner: TSharedChannel::new(StreamOwned::new(connection, stream)),
        })
    }

    /// Return the read timeout of the underlying TCP stream.
    pub fn read_timeout(&self) -> crate::Result<Option<Duration>> {
        read_timeout(&self.inner)
    }

    /// Return the write timeout of the underlying TCP stream.
    pub fn write_timeout(&self) -> crate::Result<Option<Duration>> {
        write_timeout(&self.inner)
    }

    /// Set the read timeout of the underlying TCP stream.
    pub fn set_read_timeout(&mut self, timeout: Option<Duration>) -> crate::Result<()> {
        set_read_timeout(&self.inner, timeout)
    }

    /// Set the write timeout of the underlying TCP stream.
    pub fn set_write_timeout(&mut self, timeout: Option<Duration>) -> crate::Result<()> {
        set_write_timeout(&self.inner, timeout)
    }

    /// Set the read and write timeouts of the underlying TCP stream.
    pub fn set_timeouts(
        &mut self,
        read_timeout: Option<Duration>,
        write_timeout: Option<Duration>,
    ) -> crate::Result<()> {
        self.set_read_timeout(read_timeout)?;
        self.set_write_timeout(write_timeout)
    }

    /// Send a TLS close notification and shut down the underlying TCP stream.
    pub fn close(&mut self) -> crate::Result<()> {
        close(&self.inner)
    }
}

impl Read for TTlsClientChannel {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
        self.inner.read(buffer)
    }
}

impl Write for TTlsClientChannel {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
        self.inner.write(buffer)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

impl TIoChannel for TTlsClientChannel {
    fn split(self) -> crate::Result<(ReadHalf<Self>, WriteHalf<Self>)>
    where
        Self: Sized,
    {
        Ok((ReadHalf::new(self.clone()), WriteHalf::new(self)))
    }
}

/// A blocking rustls server channel.
///
/// Construction does not perform socket I/O. The TLS handshake is completed
/// lazily by the first read or write, allowing a server accept loop to enqueue
/// the connection without waiting on an untrusted peer.
#[derive(Clone, Debug)]
pub struct TTlsServerChannel {
    inner: TSharedChannel<StreamOwned<ServerConnection, TcpStream>>,
}

impl TTlsServerChannel {
    /// Wrap an already-connected TCP stream without blocking for a handshake.
    pub fn with_stream(stream: TcpStream, config: Arc<ServerConfig>) -> crate::Result<Self> {
        let connection = ServerConnection::new(config).map_err(|error| {
            new_transport_error(
                TransportErrorKind::Unknown,
                format!("cannot create TLS server connection: {error}"),
            )
        })?;

        Ok(Self {
            inner: TSharedChannel::new(StreamOwned::new(connection, stream)),
        })
    }

    /// Complete the TLS handshake immediately.
    ///
    /// If the handshake is already complete this returns without performing
    /// socket I/O. If the stream cannot make progress, the connection state is
    /// retained so the caller can retry.
    pub fn handshake(&mut self) -> crate::Result<()> {
        let mut stream = self.inner.lock()?;
        let StreamOwned { conn, sock } = &mut *stream;
        if !conn.is_handshaking() {
            return Ok(());
        }
        conn.complete_io(sock)
            .map_err(|error| handshake_io_error("server", error))?;
        if conn.is_handshaking() {
            return Err(incomplete_handshake_error("server"));
        }
        Ok(())
    }

    /// Return the read timeout of the underlying TCP stream.
    pub fn read_timeout(&self) -> crate::Result<Option<Duration>> {
        read_timeout(&self.inner)
    }

    /// Return the write timeout of the underlying TCP stream.
    pub fn write_timeout(&self) -> crate::Result<Option<Duration>> {
        write_timeout(&self.inner)
    }

    /// Set the read timeout of the underlying TCP stream.
    pub fn set_read_timeout(&mut self, timeout: Option<Duration>) -> crate::Result<()> {
        set_read_timeout(&self.inner, timeout)
    }

    /// Set the write timeout of the underlying TCP stream.
    pub fn set_write_timeout(&mut self, timeout: Option<Duration>) -> crate::Result<()> {
        set_write_timeout(&self.inner, timeout)
    }

    /// Set the read and write timeouts of the underlying TCP stream.
    pub fn set_timeouts(
        &mut self,
        read_timeout: Option<Duration>,
        write_timeout: Option<Duration>,
    ) -> crate::Result<()> {
        self.set_read_timeout(read_timeout)?;
        self.set_write_timeout(write_timeout)
    }

    /// Send a TLS close notification and shut down the underlying TCP stream.
    pub fn close(&mut self) -> crate::Result<()> {
        close(&self.inner)
    }
}

impl Read for TTlsServerChannel {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
        self.inner.read(buffer)
    }
}

impl Write for TTlsServerChannel {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
        self.inner.write(buffer)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

impl TIoChannel for TTlsServerChannel {
    fn split(self) -> crate::Result<(ReadHalf<Self>, WriteHalf<Self>)>
    where
        Self: Sized,
    {
        Ok((ReadHalf::new(self.clone()), WriteHalf::new(self)))
    }
}

fn incomplete_handshake_error(role: &str) -> crate::Error {
    io::Error::new(
        io::ErrorKind::WouldBlock,
        format!("TLS {role} handshake did not complete"),
    )
    .into()
}

fn handshake_io_error(role: &str, error: io::Error) -> crate::Error {
    if error.kind() == io::ErrorKind::WouldBlock {
        incomplete_handshake_error(role)
    } else {
        error.into()
    }
}

fn read_timeout<C>(
    channel: &TSharedChannel<StreamOwned<C, TcpStream>>,
) -> crate::Result<Option<Duration>> {
    Ok(channel.lock()?.sock.read_timeout()?)
}

fn write_timeout<C>(
    channel: &TSharedChannel<StreamOwned<C, TcpStream>>,
) -> crate::Result<Option<Duration>> {
    Ok(channel.lock()?.sock.write_timeout()?)
}

fn set_read_timeout<C>(
    channel: &TSharedChannel<StreamOwned<C, TcpStream>>,
    timeout: Option<Duration>,
) -> crate::Result<()> {
    channel.lock()?.sock.set_read_timeout(timeout)?;
    Ok(())
}

fn set_write_timeout<C>(
    channel: &TSharedChannel<StreamOwned<C, TcpStream>>,
    timeout: Option<Duration>,
) -> crate::Result<()> {
    channel.lock()?.sock.set_write_timeout(timeout)?;
    Ok(())
}

fn close<C, S>(channel: &TSharedChannel<StreamOwned<C, TcpStream>>) -> crate::Result<()>
where
    C: Deref<Target = ConnectionCommon<S>> + DerefMut,
    S: SideData,
{
    let mut stream = channel.lock()?;
    stream.conn.send_close_notify();
    stream.flush()?;
    stream.sock.shutdown(Shutdown::Both)?;
    Ok(())
}
