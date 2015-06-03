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
use std::rc::Rc;
use std::cell::RefCell;
use std::str::FromStr;
use std::collections::HashMap;

use thrift::protocol::binary_protocol::BinaryProtocol;
use thrift::server::SimpleServer;

use tutorial::{Operation, Work, CalculatorProcessor, CalculatorIf};
use shared::{SharedStruct, SharedServiceIf};

#[allow(dead_code)]
struct CalculatorHandler {
    log: HashMap<i32, SharedStruct>
}

#[allow(dead_code)]
impl CalculatorIf for CalculatorHandler {
    fn ping(&mut self) {
        println!("ping()");
    }

    fn add(&mut self, n1: i32, n2: i32) -> i32 {
        println!("add({}, {})", n1, n2);
        n1 + n2
    }

    fn calculate(&mut self, log_id: i32, work: Work) -> i32 {
        println!("calculate({}, {:?})", log_id, work);

        let val = match work.op {
            Operation::ADD => work.num1 + work.num2,
            Operation::SUBTRACT => work.num1 - work.num2,
            Operation::MULTIPLY => work.num1 * work.num2,
            Operation::DIVIDE => {
                if work.num2 == 0 {
                    // TODO: Add this back in when exceptions are implemented
                    //InvalidOperation { what: work.op, why: "Cannot divide by 0".to_owned() };
                    unimplemented!()
                }
                work.num1 / work.num2
            }
        };

        let ss = SharedStruct { key: log_id, value: val.to_string() };
        self.log.insert(log_id, ss);

        val
    }

    fn zip(&mut self) {
        println!("zip");
    }
}

impl SharedServiceIf for CalculatorHandler {
    fn getStruct(&mut self, log_id: i32) -> SharedStruct {
        println!("getStruct({})", log_id);
        self.log[&log_id].clone()
    }
}

fn construct_protocol() -> BinaryProtocol {
    BinaryProtocol
}

pub fn main() {
    let handler = CalculatorHandler { log: HashMap::new() };
    let processor = CalculatorProcessor::new(Rc::new(RefCell::new(handler)));

    let addr: net::SocketAddr = FromStr::from_str("127.0.0.1:9090").ok()
        .expect("bad server address");
    let server_transport = net::TcpListener::bind(addr).unwrap();
    let mut server = SimpleServer::new(processor, server_transport, construct_protocol);

    println!("Starting the server...");
    server.serve();
    println!("Done.");
}
