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
use std::net::{TcpListener, TcpStream};
use std::rc::Rc;

use ::{ApplicationError, ApplicationErrorKind};
use ::protocol::{TInputProtocolFactory, TOutputProtocolFactory};
use ::transport::{TTcpTransport, TTransport, TTransportFactory};

use super::TProcessor;

/// Single-threaded blocking Thrift socket server.
///
/// A `TSimpleServer` listens on a given address and services accepted
/// connections *synchronously* and *sequentially* - i.e. in a blocking manner,
/// one at a time - on the main thread. Each accepted connection has an input
/// half and an output half, each of which uses a `TTransport` and `TProtocol`
/// to translate messages to and from byes. Any combination of `TProtocol` and
/// `TTransport` may be used.
///
/// # Examples
///
/// Creating and running a `TSimpleServer` using Thrift-compiler-generated
/// service code.
///
/// ```no_run
/// use thrift;
/// use thrift::protocol::{TInputProtocolFactory, TOutputProtocolFactory};
/// use thrift::protocol::{TBinaryInputProtocolFactory, TBinaryOutputProtocolFactory};
/// use thrift::protocol::{TInputProtocol, TOutputProtocol};
/// use thrift::transport::{TBufferedTransportFactory, TTransportFactory};
/// use thrift::server::{TProcessor, TSimpleServer};
///
/// //
/// // auto-generated
/// //
///
/// // processor for `SimpleService`
/// struct SimpleServiceSyncProcessor;
/// impl SimpleServiceSyncProcessor {
///     fn new<H: SimpleServiceSyncHandler>(processor: H) -> SimpleServiceSyncProcessor {
///         unimplemented!();
///     }
/// }
///
/// // `TProcessor` implementation for `SimpleService`
/// impl TProcessor for SimpleServiceSyncProcessor {
///     fn process(&mut self, i: &mut TInputProtocol, o: &mut TOutputProtocol) -> thrift::Result<()> {
///         unimplemented!();
///     }
/// }
///
/// // service functions for SimpleService
/// trait SimpleServiceSyncHandler {
///     fn service_call(&mut self) -> thrift::Result<()>;
/// }
///
/// //
/// // user-code follows
/// //
///
/// // define a handler that will be invoked when `service_call` is received
/// struct SimpleServiceHandlerImpl;
/// impl SimpleServiceSyncHandler for SimpleServiceHandlerImpl {
///     fn service_call(&mut self) -> thrift::Result<()> {
///         unimplemented!();
///     }
/// }
///
/// // instantiate the processor
/// let processor = SimpleServiceSyncProcessor::new(SimpleServiceHandlerImpl {});
///
/// // instantiate the server
/// let i_tr_fact: Box<TTransportFactory> = Box::new(TBufferedTransportFactory::new());
/// let i_pr_fact: Box<TInputProtocolFactory> = Box::new(TBinaryInputProtocolFactory::new());
/// let o_tr_fact: Box<TTransportFactory> = Box::new(TBufferedTransportFactory::new());
/// let o_pr_fact: Box<TOutputProtocolFactory> = Box::new(TBinaryOutputProtocolFactory::new());
///
/// let mut server = TSimpleServer::new(
///     i_tr_fact,
///     i_pr_fact,
///     o_tr_fact,
///     o_pr_fact,
///     processor
/// );
///
/// // start listening for incoming connections
/// match server.listen("127.0.0.1:8080") {
///   Ok(_)  => println!("listen completed"),
///   Err(e) => println!("listen failed with error {:?}", e),
/// }
/// ```
pub struct TSimpleServer<PR: TProcessor> {
    i_trans_factory: Box<TTransportFactory>,
    i_proto_factory: Box<TInputProtocolFactory>,
    o_trans_factory: Box<TTransportFactory>,
    o_proto_factory: Box<TOutputProtocolFactory>,
    processor: PR,
}

impl<PR: TProcessor> TSimpleServer<PR> {
    /// Create a `TSimpleServer`.
    ///
    /// Each accepted connection has an input and output half, each of which
    /// requires a `TTransport` and `TProtocol`. `TSimpleServer` uses
    /// `input_transport_factory` and `input_protocol_factory` to create
    /// implementations for the input, and `output_transport_factory` and
    /// `output_protocol_factory` to create implementations for the output.
    pub fn new(input_transport_factory: Box<TTransportFactory>,
               input_protocol_factory: Box<TInputProtocolFactory>,
               output_transport_factory: Box<TTransportFactory>,
               output_protocol_factory: Box<TOutputProtocolFactory>,
               processor: PR)
               -> TSimpleServer<PR> {
        TSimpleServer {
            i_trans_factory: input_transport_factory,
            i_proto_factory: input_protocol_factory,
            o_trans_factory: output_transport_factory,
            o_proto_factory: output_protocol_factory,
            processor: processor,
        }
    }

    /// Listen for incoming connections on `listen_address`.
    ///
    /// `listen_address` should be in the form `host:port`,
    /// for example: `127.0.0.1:8080`.
    ///
    /// Return `()` if successful.
    ///
    /// Return `Err` when the server cannot bind to `listen_address` or there
    /// is an unrecoverable error.
    pub fn listen(&mut self, listen_address: &str) -> ::Result<()> {
        let listener = TcpListener::bind(listen_address)?;
        for stream in listener.incoming() {
            match stream {
                Ok(s) => self.handle_incoming_connection(s),
                Err(e) => warn!("failed to accept remote connection with error {:?}", e),
            }
        }

        Err(::Error::Application(ApplicationError {
            kind: ApplicationErrorKind::Unknown,
            message: "aborted listen loop".into(),
        }))
    }

    fn handle_incoming_connection(&mut self, stream: TcpStream) {
        // create the shared tcp stream
        let stream = TTcpTransport::with_stream(stream);
        let stream: Box<TTransport> = Box::new(stream);
        let stream = Rc::new(RefCell::new(stream));

        // input protocol and transport
        let i_tran = self.i_trans_factory.create(stream.clone());
        let i_tran = Rc::new(RefCell::new(i_tran));
        let mut i_prot = self.i_proto_factory.create(i_tran);

        // output protocol and transport
        let o_tran = self.o_trans_factory.create(stream.clone());
        let o_tran = Rc::new(RefCell::new(o_tran));
        let mut o_prot = self.o_proto_factory.create(o_tran);

        // process loop
        loop {
            let r = self.processor.process(&mut *i_prot, &mut *o_prot);
            if let Err(e) = r {
                warn!("processor failed with error: {:?}", e);
                break; // FIXME: close here
            }
        }
    }
}
