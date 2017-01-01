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

//! Types required to implement the server half of a Thrift service.
//!
//! Provides the following implementations:
//!
//! 1. `TSimpleServer`: single-threaded socket server
//! 2. `TProcessor`: service-call demultiplexer for a single Thrift service
//! 3. `TMultiplexedProcessor`: service-call demultiplexer for multiple Thrift
//!    services

use ::protocol::{TInputProtocol, TOutputProtocol};

mod simple;
mod multiplexed;

pub use self::simple::TSimpleServer;
pub use self::multiplexed::TMultiplexedProcessor;

/// Identifies an auto-generated service-call demultiplexer for a Thrift
/// service. Users need to instantiate the `TProcessor` implementation they
/// want to handle and pass this instance to a `rift::server` implementation.
/// After the server is started the auto-generated processor will demux
/// incoming service calls and automatically execute the user's handler code.
///
/// # Example
///
/// Create and start a server using the auto-generated `TProcessor` for
/// a Thrift service `SimpleService`.
///
/// ```text
/// use generated::{SimpleServiceSyncProcessor, SimpleServiceSyncHandler};
///
/// struct SimpleServiceHandlerImpl;
/// impl SimpleServiceSyncHandler for SimpleServiceHandlerImpl {
///   fn service_call(&mut self, ...) -> Result<()> {
///     // ...
///   }
/// }
///
/// let processor = SimpleServiceSyncProcessor::new(SimpleServiceHandlerImpl {});
/// let server = TSimpleServer::new(..., processor);
/// ```
pub trait TProcessor {
    /// Process a Thrift service call. Reads arguments from `i`, executes the
    /// user-implemented handler and writes the response out to `o`.
    ///
    /// Returns `()` if the service call was processed. Returns an `Err` if
    /// no matching service call could be found, or there was a transport or
    /// protocol error in processing the service call.
    fn process(&mut self, i: &mut TInputProtocol, o: &mut TOutputProtocol) -> ::Result<()>;
}
