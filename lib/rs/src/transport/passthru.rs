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
use std::rc::Rc;
use std::io;
use std::io::{Read, Write};

use super::TTransport;

/// Proxy that wraps an inner `TTransport` and delegates all calls to it.
///
/// Unlike other `TTransport` wrappers, `TPassThruTransport` is generic with
/// regards to the wrapped transport. This allows callers to use methods
/// specific to the type being wrapped instead of being constrained to methods
/// on the `TTransport` trait.
///
/// # Examples
///
/// Create and use a `TPassThruTransport`.
///
/// ```no_run
/// use std::cell::RefCell;
/// use std::rc::Rc;
/// use thrift::transport::{TPassThruTransport, TTcpTransport};
///
/// let t = TTcpTransport::new();
/// let t = TPassThruTransport::new(Rc::new(RefCell::new(Box::new(t))));
///
/// // since the type parameter is maintained, we are able
/// // to use functions specific to `TTcpTransport`
/// t.inner.borrow_mut().open("localhost:9090").unwrap();
/// ```
pub struct TPassThruTransport<I: TTransport> {
    pub inner: Rc<RefCell<Box<I>>>,
}

impl<I: TTransport> TPassThruTransport<I> {
    /// Create a `TPassThruTransport` that wraps an `inner` TTransport.
    pub fn new(inner: Rc<RefCell<Box<I>>>) -> TPassThruTransport<I> {
        TPassThruTransport { inner: inner }
    }
}

impl<I: TTransport> Read for TPassThruTransport<I> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.inner.borrow_mut().read(buf)
    }
}

impl<I: TTransport> Write for TPassThruTransport<I> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.inner.borrow_mut().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.borrow_mut().flush()
    }
}
