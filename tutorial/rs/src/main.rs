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

use std::io::net::ip;
use thrift::TResult;
use thrift::ThriftErr;
use thrift::ThriftErr::*;
use thrift::protocol::{MessageType, Type};
use thrift::transport::Transport;
use thrift::protocol::Protocol;
use thrift::protocol::{Readable, Writeable};
use thrift::protocol::binary_protocol::BinaryProtocol;

mod tutorial;

struct CalculatorClient<T: Transport, P: Protocol> {
    transport: T,
    protocol: P,
}

impl <T: Transport, P: Protocol> CalculatorClient<T, P> {
    
    fn ping(& mut self) -> TResult<()> {
        try!(self.send("ping", MessageType::MtCall, &tutorial::CalculatorPingArgs));

        let mut result = tutorial::CalculatorPingResult;
        try!(self.receive("ping", &mut result));
        Ok(())
    }
    
    fn add(& mut self, num1: i32, num2: i32) -> TResult<i32> {
        let args = tutorial::CalculatorAddArgs { num1: num1, num2: num2 };
        try!(self.send("add", MessageType::MtCall, &args));

        let mut result = tutorial::CalculatorAddResult  { success: 0 };
        try!(self.receive("add", &mut result));
        Ok(result.success)
    }

    fn calculate(& mut self, logid: i32, w: tutorial::Work) -> TResult<i32> {
        let args = tutorial::CalculatorCalculateArgs { logid: logid, w: w };
        try!(self.send("calculate", MessageType::MtCall, &args));

        let mut result = tutorial::CalculatorCalculateResult { success: 0, ouch: None };
        try!(self.receive("calculate", &mut result));
        Ok(result.success)
    }

    fn send<W: Writeable>(&mut self, name: &str, _type: MessageType, args: &W) -> TResult<()> {
        let cseqid: i32 = 0;
        self.protocol.write_message_begin(&mut self.transport, name, _type, cseqid);
        
        try!(args.write(&self.protocol, &mut self.transport));
        
        self.protocol.write_message_end(&mut self.transport);

        //self.transport.write_end();
        try!(self.transport.flush());

        Ok(())
    }

    #[allow(unused_variables)]
    fn receive<R: Readable>(& mut self, op: &'static str, result: &mut R) -> TResult<()> {

        match try!(self.protocol.read_message_begin(& mut self.transport)) {
          (_, MessageType::MtException, _) => {
              // TODO
              //let x = ApplicationException;
              //x.read(& mut self protocol)
              //self.protocol.read_message_end();
              //transport.read_end();
              //throw x     
              Err(ThriftErr::Exception)
          }
          (fname, MessageType::MtReply, _) => {
              if fname.as_slice() == op {
                  try!(result.read(&self.protocol, & mut self.transport));
                  try!(self.protocol.read_message_end(& mut self.transport));
                  Ok(())
               }
              else {
                // FIXME: shall we err in this case?
                  try!(self.protocol.skip(& mut self.transport, Type::TStruct));
                  try!(self.protocol.read_message_end(& mut self.transport));
                  Err(ThriftErr::ProtocolError)
              }
          }
          (_, _, _) => {
              try!(self.protocol.skip(& mut self.transport, Type::TStruct));
              try!(self.protocol.read_message_end(& mut self.transport));
              Err(ThriftErr::ProtocolError)
          }
        }
    }
}

pub fn main() {
    let server_address = "127.0.0.1:9090";
    let addr: ip::SocketAddr = std::str::FromStr::from_str(server_address)
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

    println!("PASS");
}

