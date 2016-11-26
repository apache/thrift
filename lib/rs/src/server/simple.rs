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
use std::net::{Shutdown, TcpListener, TcpStream};
use std::rc::Rc;

use ::{ApplicationError, ApplicationErrorKind};
use ::protocol::TProtocolFactory;
use ::transport::{TTransport, TTcpTransport, TTransportFactory};
use super::TProcessor;

pub struct TSimpleServer {}

/*
pub struct TSimpleServer<TFI, TRI, PFI, TFO, TRO, PFO, PRC>
    where
        TFI: TTransportFactory<TRI>, TRI: TTransport,
        PFI: TProtocolFactory,
        TFO: TTransportFactory<TRO>, TRO: TTransport,
        PFO: TProtocolFactory,
        PRC: TProcessor {
    i_tran_factory: TFI,
    i_prot_factory: PFI,
    o_tran_factory: TFO,
    o_prot_factory: PFO,
    processor: PRC,
}

impl <TFI, TRI, PFI, TFO, TRO, PFO, PRC> TSimpleServer<TFI, TRI, PFI, TFO, TRO, PFO, PRC>
    where
        TFI: TTransportFactory<TRI>, TRI: TTransport,
        PFI: TProtocolFactory,
        TFO: TTransportFactory<TFO>, TFO: TTransport,
        PFO: TProtocolFactory,
        PRC: TProcessor {

    pub fn new(
        input_transport_factory: TFI,
        input_protocol_factory: PFI,
        output_transport_factory: TFO,
        output_protocol_factory: PFO,
        processor: PRC
    ) -> TSimpleServer<TFI, TRI, PFI, TFO, TRO, PFO, PRC> {
        TSimpleServer {
            i_tran_factory: input_transport_factory,
            i_prot_factory: input_protocol_factory,
            o_tran_factory: output_transport_factory,
            o_prot_factory: output_protocol_factory,
            processor: processor
        }
    }

    pub fn listener(&mut self, listening_address: &str) -> ::Result<()> {
        let listener = try!(TcpListener::bind(listening_address));
        for stream in listener.incoming() {
            match stream {
                Ok(s) => {
                    self.handle_incoming_connection(s)
                },
                Err(e) => {
                    println!("failed to accept remote connection with error {:?}", e)
                }
            }
        }

        Err(
            ::Error::Application(
                ApplicationError {
                    kind: ApplicationErrorKind::Unknown,
                    message: "aborted listening loop".into()
                }
            )
        )
    }

    fn handle_incoming_connection(&mut self, stream: TcpStream) {
        // create the base transport (both input/output share this underlying stream)
        let stream_transport = TTcpTransport::using_stream(stream);
        let boxed_stream_transport: Box<TTransport> = Box::new(stream_transport);
        let ref_stream_transport = Rc::new(RefCell::new(boxed_stream_transport));

        // input protocol and transport
        let i_tran = Box::new(self.i_tran_factory.new(ref_stream_transport.clone()));
        let ref_i_tran = Rc::new(RefCell::new(i_tran));
        let i_prot = self.i_prot_factory.new(ref_i_tran);

        // output protocol and transport
        let o_tran = self.o_tran_factory.new(ref_stream_transport.clone());
        let ref_o_tran = Rc::new(RefCell::new(Box::new(o_tran)));
        let o_prot = self.o_prot_factory.new(ref_o_tran);

        // process loop
        /*
        loop {
            let r = self.processor.process(&**i_prot.borrow_mut(), &**o_prot.borrow_mut());
            match r {
                Err(e) => {
                    println!("processor failed with error {:?}", e);
                    stream.shutdown(Shutdown::Both);
                    break;
                },
                Ok(_) => {
                    ()
                },
            }
        }*/
    }
}*/
