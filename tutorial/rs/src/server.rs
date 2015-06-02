/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

extern crate thrift;

mod tutorial;
mod shared;

use std::net;
use std::str::FromStr;
use std::collections::HashMap;

use thrift::protocol::binary_protocol::BinaryProtocol;
use thrift::server::SimpleServer;
use thrift::processor::Processor;

use tutorial::{Operation, Work};
use shared::SharedStruct;

#[allow(dead_code)]
struct CalculatorHandler {
    log: HashMap<i32, SharedStruct>
}

#[allow(dead_code)]
impl CalculatorIf for CalculatorHandler {
    fn ping(&self) {
        println!("ping()");
    }

    fn add(&self, n1: i32, n2: i32) -> i32 {
        println!("add({}, {})", n1, n2);
        n1 + n2
    }

    fn calculate(&mut self, log_id: i32, work: &Work) -> i32 {
        println!("calculate({}, {:?})", log_id, work);

        let val = match work.op {
            Operation::ADD => work.num1 + work.num2,
            Operation::SUBTRACT => work.num1 - work.num2,
            Operation::MULTIPLY => work.num1 * work.num2,
            Operation::DIVIDE => {
                if work.num2 == 0 {
                    //InvalidOperation { what: work.op, why: "Cannot divide by 0".to_owned() };
                    // TODO: Throw?
                    unimplemented!()
                }
                work.num1 / work.num2
            }
        };

        let ss = SharedStruct { key: log_id, value: val.to_string() };
        self.log.insert(log_id, ss);

        val
    }
}

fn construct_protocol() -> BinaryProtocol {
    BinaryProtocol
}

pub fn main() {
    let handler = CalculatorHandler { log: HashMap::new() };
    let processor = CalculatorProcessor::new(handler);

    let addr: net::SocketAddr = FromStr::from_str("127.0.0.1:9090").ok()
        .expect("bad server address");
    let server_transport = net::TcpListener::bind(addr).unwrap();
    let mut server = SimpleServer::new(processor, server_transport, construct_protocol);

    println!("Starting the server...");
    server.serve();
    println!("Done.");
}

// TO BE GENERATED

use thrift::TResult;
use thrift::transport::Transport;
use thrift::protocol::{Protocol, ProtocolHelpers, MessageType};
use tutorial::{CalculatorPingArgs, CalculatorPingResult};
use tutorial::{CalculatorAddArgs, CalculatorAddResult};

trait CalculatorIf {
    fn ping(&self);
    fn add(&self, n1: i32, n2: i32) -> i32;
    fn calculate(&mut self, log_id: i32, work: &Work) -> i32;
}

struct CalculatorProcessor<T: CalculatorIf> {
    iface: T
}

impl<I: CalculatorIf, P1: Protocol, P2: Protocol, T: Transport> Processor<P1, P2, T> for CalculatorProcessor<I> {
    fn process(&mut self, in_prot: &mut P1, _: &mut P2, transport: &mut T) -> TResult<()> {
        let (name, ty, id) = try!(in_prot.read_message_begin(transport));
        match &*name {
            "ping" => self.ping(in_prot, transport, ty, id),
            "add" => self.add(in_prot, transport, ty, id),
            _ => unimplemented!()
        }
    }
}

impl<I: CalculatorIf> CalculatorProcessor<I> {
    fn new(iface: I) -> Self {
        CalculatorProcessor { iface: iface }
    }

    #[allow(non_snake_case)]
    fn ping<P1: Protocol, T: Transport>(&mut self, in_prot: &mut P1, transport: &mut T,
                                        ty: MessageType, id: i32) -> TResult<()> {
        let mut args = CalculatorPingArgs;
        try!(ProtocolHelpers::receive_body(in_prot, transport, "ping", &mut args, "ping", ty, id));

        let result = CalculatorPingResult::new();
        self.iface.ping();
        try!(ProtocolHelpers::send(in_prot, transport, "ping", MessageType::MtReply, &result));
        Ok(())
    }

    #[allow(non_snake_case)]
    fn add<P1: Protocol, T: Transport>(&mut self, in_prot: &mut P1, transport: &mut T,
                                       ty: MessageType, id: i32) -> TResult<()> {
        let mut args = CalculatorAddArgs::new();
        try!(ProtocolHelpers::receive_body(in_prot, transport, "add", &mut args, "add", ty, id));

        let mut result = CalculatorAddResult::new();
        result.success = self.iface.add(args.num1, args.num2);
        try!(ProtocolHelpers::send(in_prot, transport, "add", MessageType::MtReply, &result));
        Ok(())
    }
}
