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
        let i_tran = self.i_trans_factory.build(stream.clone());
        let i_tran = Rc::new(RefCell::new(i_tran));
        let mut i_prot = self.i_proto_factory.create(i_tran);

        // output protocol and transport
        let o_tran = self.o_trans_factory.build(stream.clone());
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
