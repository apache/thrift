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
use std::sync::{Arc, Mutex, MutexGuard};

use super::{ReadHalf, TIoChannel, WriteHalf};

/// A cloneable channel that serializes access to an underlying I/O stream.
///
/// This adapter allows a bidirectional stream that cannot be cloned, such as a
/// TLS session, to implement [`TIoChannel`]. Every read, write, and flush holds
/// the same lock for the duration of that operation. It is intended for
/// synchronous request-response traffic, where a caller writes and flushes a
/// complete request before reading its response.
///
/// The shared lock makes access memory-safe across threads, but it does not
/// provide full-duplex progress: a blocking read holds the lock and prevents a
/// concurrent write until that read finishes.
#[derive(Debug)]
pub struct TSharedChannel<C> {
    inner: Arc<Mutex<C>>,
}

impl<C> TSharedChannel<C> {
    /// Wrap `inner` in a shared channel.
    pub fn new(inner: C) -> Self {
        Self {
            inner: Arc::new(Mutex::new(inner)),
        }
    }

    // io::Error::other requires Rust 1.74.
    #[allow(unknown_lints)]
    #[allow(clippy::io_other_error)]
    pub(crate) fn lock(&self) -> io::Result<MutexGuard<'_, C>> {
        self.inner
            .lock()
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "shared channel lock is poisoned"))
    }
}

impl<C> Clone for TSharedChannel<C> {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl<C: Read> Read for TSharedChannel<C> {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
        self.lock()?.read(buffer)
    }
}

impl<C: Write> Write for TSharedChannel<C> {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
        self.lock()?.write(buffer)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.lock()?.flush()
    }
}

impl<C: Read + Write> TIoChannel for TSharedChannel<C> {
    fn split(self) -> crate::Result<(ReadHalf<Self>, WriteHalf<Self>)>
    where
        Self: Sized,
    {
        Ok((ReadHalf::new(self.clone()), WriteHalf::new(self)))
    }
}
