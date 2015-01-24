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

#![crate_name="calculator"]
#![crate_type="bin"]

extern crate thrift;

use std::str::FromStr;
use std::io::net::ip;
use thrift::TResult;
use thrift::ThriftErr;
use thrift::ThriftErr::*;
use thrift::protocol::{MessageType, Type};
use thrift::transport::Transport;
use thrift::protocol::Protocol;
use thrift::protocol::ProtocolHelpers;
use thrift::protocol::{Readable, Writeable};
use thrift::protocol::binary_protocol::BinaryProtocol;

mod tutorial;
mod shared;


struct CalculatorClient<T: Transport, P: Protocol> {
    transport: T,
    protocol: P,
}

impl <T: Transport, P: Protocol> CalculatorClient<T, P> {
    
    fn ping(&mut self) -> TResult<()> {
        try!(ProtocolHelpers::send(&self.protocol, &mut self.transport, "ping", MessageType::MtCall, &tutorial::CalculatorPingArgs));

        let mut result = tutorial::CalculatorPingResult;
        try!(ProtocolHelpers::receive(&self.protocol, &mut self.transport, "ping", &mut result));
        Ok(())
    }
    
    fn add(&mut self, num1: i32, num2: i32) -> TResult<i32> {
        let args = tutorial::CalculatorAddArgs { num1: num1, num2: num2 };
        try!(ProtocolHelpers::send(&self.protocol, &mut self.transport, "add", MessageType::MtCall, &args));

        let mut result = tutorial::CalculatorAddResult  { success: 0 };
        try!(ProtocolHelpers::receive(&self.protocol, &mut self.transport, "add", &mut result));
        Ok(result.success)
    }

    fn calculate(&mut self, logid: i32, w: tutorial::Work) -> TResult<i32> {
        let args = tutorial::CalculatorCalculateArgs { logid: logid, w: w };
        try!(ProtocolHelpers::send(&self.protocol, &mut self.transport, "calculate", MessageType::MtCall, &args));

        let mut result = tutorial::CalculatorCalculateResult { success: 0, ouch: None };
        try!(ProtocolHelpers::receive(&self.protocol, &mut self.transport, "calculate", &mut result));
        Ok(result.success)
    }

    #[allow(non_snake_case)] 
    fn getStruct(&mut self, key: i32) -> TResult<shared::SharedStruct> {
        let args = shared::SharedServiceGetStructArgs { key: key };
        try!(ProtocolHelpers::send(&self.protocol, &mut self.transport, "getStruct", MessageType::MtCall, &args));

        let mut result = shared::SharedServiceGetStructResult { 
            success: shared::SharedStruct { key: -1, value: String::new() } };
        try!(ProtocolHelpers::receive(&self.protocol, &mut self.transport, "getStruct", &mut result));
        Ok(result.success)
    }
}

pub fn main() {
    let server_address = "127.0.0.1:9090";
    let addr: ip::SocketAddr = FromStr::from_str(server_address)
        .expect("bad server address");
    let tcp = std::io::TcpStream::connect(addr).unwrap();

    let mut client = CalculatorClient{ protocol: BinaryProtocol, transport: tcp };

    // Ping
    client.ping().unwrap();
    println!("ping()");

    // Add
    println!("1 + 1 = {}", client.add(1, 1).unwrap());

    // Work: divide
    let work = tutorial::Work { op: tutorial::Operation::DIVIDE, num1: 1, num2: 0, comment: None };

    match client.calculate(1, work) {
      Ok(_) => {
        println!("Whoa? We can divide by zero!");
      }
      Err(_) => {
        // FIXME: use thrift exceptions
        println!("Invalid operation")
      }
    }

    // Work: subtract
    let work = tutorial::Work { op: tutorial::Operation::SUBTRACT, num1: 15, num2: 10, comment: None };
    println!("15 - 10 = {}", client.calculate(2, work).unwrap());

    let ss = client.getStruct(1).unwrap();
    println!("Received log: {:?}", ss);

    println!("PASS");
}

