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
use std::marker::PhantomData;
use std::net::{TcpListener, TcpStream};
use std::rc::Rc;

use ::{ApplicationError, ApplicationErrorKind};
use ::protocol::{TProtocol, TProtocolFactory};
use ::transport::{RcTTransport, TTcpTransport, TTransport, TTransportFactory};
use super::TProcessor;

pub struct TSimpleServer<TI, TFI, PI, PFI, TO, TFO, PO, PFO, PR>
    where TI: TTransport + 'static, TFI: TTransportFactory<TI>, PI: TProtocol + 'static, PFI: TProtocolFactory<PI>,
          TO: TTransport + 'static, TFO: TTransportFactory<TO>, PO: TProtocol + 'static, PFO: TProtocolFactory<PO>,
          PR: TProcessor {
    i_trans_type: PhantomData<TI>,
    i_trans_factory: TFI,
    i_proto_type: PhantomData<PI>,
    i_proto_factory: PFI,
    o_trans_type: PhantomData<TO>,
    o_trans_factory: TFO,
    o_proto_type: PhantomData<PO>,
    o_proto_factory: PFO,
    processor: PR,
}

impl <TI, TFI, PI, PFI, TO, TFO, PO, PFO, PR> TSimpleServer<TI, TFI, PI, PFI, TO, TFO, PO, PFO, PR>

    where TI: TTransport + 'static, TFI: TTransportFactory<TI>, PI: TProtocol + 'static, PFI: TProtocolFactory<PI>,
          TO: TTransport + 'static, TFO: TTransportFactory<TO>, PO: TProtocol + 'static, PFO: TProtocolFactory<PO>,
          PR: TProcessor {

    pub fn new(
        input_transport_factory: TFI,
        input_protocol_factory: PFI,
        output_transport_factory: TFO,
        output_protocol_factory: PFO,
        processor: PR
    ) -> TSimpleServer<TI, TFI, PI, PFI, TO, TFO, PO, PFO, PR> {
        TSimpleServer {
            i_trans_type: PhantomData,
            i_trans_factory: input_transport_factory,
            i_proto_type: PhantomData,
            i_proto_factory: input_protocol_factory,
            o_trans_type: PhantomData,
            o_trans_factory: output_transport_factory,
            o_proto_type: PhantomData,
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
        let stream = TTcpTransport::using_stream(stream);
        let box_stream: Box<TTransport> = Box::new(stream);
        let ref_stream = Rc::new(RefCell::new(box_stream));

        // input protocol and transport
        let i_tran = self.i_trans_factory.new(ref_stream.clone());
        let ref_i_tran: RcTTransport = Rc::new(RefCell::new(Box::new(i_tran)));
        let i_prot = self.i_proto_factory.new(ref_i_tran);

        // output protocol and transport
        let o_tran = self.o_trans_factory.new(ref_stream.clone());
        let ref_o_tran: RcTTransport = Rc::new(RefCell::new(Box::new(o_tran)));
        let o_prot = self.o_proto_factory.new(ref_o_tran);

        // process loop
        loop {
            let r = self.processor.process(&i_prot, &o_prot);
            match r {
                Err(e) => {
                    println!("processor failed with error {:?}", e);
//                    if let Err(e) = ref_stream.borrow_mut().close() {
//                        println!("error {:?} while closing stream", e);
//                    }
                    break;
                },
                Ok(_) => {
                    ()
                },
            }
        }
    }
}
