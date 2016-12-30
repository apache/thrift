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

//! Single-threaded blocking Thrift socket server. This implementation listens
//! on the given host/port pair and accepts a single connection on the main
//! thread. It processes messages from the remote, executes the user's handler
//! code and sends responses back to the remote all on this thread. It allows
//! different `TProtocol` and `TTransport` implementations to be used for the
//! input and output halves of communication.
//!
//! # Examples
//!
//! Creating and running a `TSimpleServer` using Thrift-compiler-generated
//! service code.
//!
//! ```no_run
//! use rift::protocol::{TBinaryInputProtocolFactory, TBinaryOutputProtocolFactory, TInputProtocolFactory, TOutputProtocolFactory};
//! use rift::server::TSimpleServer;
//! use rift::transport::{TBufferedTransportFactory, TTransportFactory};
//! use generated::{ServiceSyncProcessor, ServiceSyncHandler};
//!
//! struct ServiceSyncHandlerImpl;
//! impl ServiceSyncHandler for ServiceSyncHandlerImpl {
//!   fn service_call(&mut self, ...) -> Result<()> {
//!     // ...
//!   }
//! }
//!
//! let i_trans_factory = TBufferedTransportFactory {};
//! let i_proto_factory = TBinaryInputProtocolFactory {};
//! let o_trans_factory = TBufferedTransportFactory {};
//! let o_proto_factory = TBinaryOutputProtocolFactory {};
//!
//! let processor = ServiceSyncProcessor::new(ServiceSyncHandlerImpl {});
//! let server = TSimpleServer::new(
//!   i_trans_factory,
//!   i_proto_factory,
//!   o_trans_factory,
//!   o_proto_factory,
//!   processor
//! );
//!
//! match server.listen("127.0.0.1:8080") {
//!   Ok(_)  => println!("listen completed"),
//!   Err(e) => println!("listen failed with error {:?}", e),
//! }
//! ```

use std::cell::RefCell;
use std::net::{TcpListener, TcpStream};
use std::rc::Rc;

use ::{ApplicationError, ApplicationErrorKind};
use ::protocol::{TInputProtocolFactory, TOutputProtocolFactory};
use ::transport::{TTcpTransport, TTransport, TTransportFactory};

use super::TProcessor;

pub struct TSimpleServer<PR: TProcessor> {
    i_trans_factory: Box<TTransportFactory>,
    i_proto_factory: Box<TInputProtocolFactory>,
    o_trans_factory: Box<TTransportFactory>,
    o_proto_factory: Box<TOutputProtocolFactory>,
    processor: PR,
}

impl <PR: TProcessor> TSimpleServer<PR> {
    pub fn new(
        input_transport_factory: Box<TTransportFactory>,
        input_protocol_factory: Box<TInputProtocolFactory>,
        output_transport_factory: Box<TTransportFactory>,
        output_protocol_factory: Box<TOutputProtocolFactory>,
        processor: PR
    ) -> TSimpleServer<PR> {
        TSimpleServer {
            i_trans_factory: input_transport_factory,
            i_proto_factory: input_protocol_factory,
            o_trans_factory: output_transport_factory,
            o_proto_factory: output_protocol_factory,
            processor: processor
        }
    }

    pub fn listen(&mut self, listening_address: &str) -> ::Result<()> {
        let listener = try!(TcpListener::bind(listening_address));
        for stream in listener.incoming() {
            match stream {
                Ok(s) => {
                    self.handle_incoming_connection(s)
                },
                Err(e) => {
                    warn!("failed to accept remote connection with error {:?}", e)
                }
            }
        }

        Err(
            ::Error::Application(
                ApplicationError {
                    kind: ApplicationErrorKind::Unknown,
                    message: "aborted listen loop".into()
                }
            )
        )
    }

    fn handle_incoming_connection(&mut self, stream: TcpStream) {
        // create the shared tcp stream
        let stream = TTcpTransport::using_stream(stream);
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
            match r {
                Err(e) => {
                    warn!("processor failed with error: {:?}", e);
                    break; // FIXME: close here
                },
                Ok(_) => (),
            }
        }
    }
}
