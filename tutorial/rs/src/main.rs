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
use thrift::protocol::{MessageType, Type};
use thrift::transport::Transport;
use thrift::protocol::Protocol;
use thrift::protocol::binary_protocol::BinaryProtocol;


// TODO: shall be generated
//#[allow(dead_code)]
struct CalculatorPingArgs;

impl CalculatorPingArgs {

    #[allow(unused_variables)]
    fn write(&self, oprot: &Protocol, transport: & mut Transport) {
        oprot.write_struct_begin(transport, "Calculator_ping_args");
        oprot.write_field_stop(transport);
        oprot.write_struct_end(transport);
    }
}

struct CalculatorPingResult;

impl CalculatorPingResult {

    fn read(&self, iprot: &Protocol, transport: & mut Transport) {
        iprot.skip(transport, Type::TStruct);
    }
}

struct CalculatorAddArgs {
    num1: i32,
    num2: i32,
}

impl CalculatorAddArgs {

    #[allow(unused_variables)]
    fn write(&self, oprot: &Protocol, transport: & mut Transport) {
        oprot.write_struct_begin(transport, "Calculator_add_args");

        oprot.write_field_begin(transport, "num1", Type::TI32, 1);
        oprot.write_i32(transport, self.num1);
        oprot.write_field_end(transport);

        oprot.write_field_begin(transport, "num2", Type::TI32, 2);
        oprot.write_i32(transport, self.num2);
        oprot.write_field_end(transport);

        oprot.write_field_stop(transport);
        oprot.write_struct_end(transport);
    }
}

struct CalculatorAddResult {
    success: Option<i32>,
}

impl CalculatorAddResult {

    #[allow(unused_variables)]
    fn read(& mut self, iprot: &Protocol, transport: & mut Transport) {
        iprot.read_struct_begin(transport);

        loop {
            let (fname, ftype, fid) = iprot.read_field_begin(transport);
            if ftype == Type::TStop {
                break;
            }
            match (fid, ftype) {
                (0, Type::TI32) => {
                    self.success = Some(iprot.read_i32(transport));
                }
                _ => {
                    iprot.skip(transport, ftype);
                }
            }
            iprot.read_field_end(transport);
        }
        iprot.read_struct_end(transport);
    }
}

struct CalculatorClient<T: Transport, P: Protocol> {
    transport: T,
    protocol: P,
}

impl <T: Transport, P: Protocol> CalculatorClient<T, P> {
    
    fn ping(& mut self) {
        self.send_ping();
        self.receive_ping();
    }
    
    #[allow(unused_variables)]
    fn send_ping(& mut self) {
        let cseqid: i32 = 0;
        self.protocol.write_message_begin(& mut self.transport, "ping", MessageType::MtCall, cseqid);
        
        let args = CalculatorPingArgs;
        args.write(&self.protocol, & mut self.transport);
        
        self.protocol.write_message_end(& mut self.transport);

        //self.transport.write_end();
        self.transport.flush().unwrap();
    }

    #[allow(unused_variables)]
    fn receive_ping(& mut self) {
      let (fname, mtype, rseqid) = self.protocol.read_message_begin(& mut self.transport);
      match mtype {
        MessageType::MtException => {
            // TODO
            //let x = ApplicationException;
            //x.read(& mut self protocol)
            //self.protocol.read_message_end();
            //transport.read_end();
            //throw x     
        }
        MessageType::MtReply => {
            match fname.as_slice() {
                "ping" => {
                    let result = CalculatorPingResult;
                    result.read(&self.protocol, & mut self.transport);
                }
                _ => {
                    self.protocol.skip(& mut self.transport, Type::TStruct);
                }
            }
            self.protocol.read_message_end(& mut self.transport);
            //self.transport.read_end();
        }
        _ => {
            self.protocol.skip(& mut self.transport, Type::TStruct);
            self.protocol.read_message_end(& mut self.transport);
            //self.transport.read_end();         
        }
      }
    }
    
    fn add(& mut self, num1: i32, num2: i32) -> Option<i32> {
        self.send_add(num1, num2);
        self.receive_add()
    }
    
    fn send_add(& mut self, num1: i32, num2: i32) {
        let cseqid: i32 = 0;
        self.protocol.write_message_begin(& mut self.transport, "add", MessageType::MtCall, cseqid);
        
        let args = CalculatorAddArgs { num1: num1, num2: num2 };
        args.write(&self.protocol, & mut self.transport);
        
        self.protocol.write_message_end(& mut self.transport);

        //self.transport.write_end();
        self.transport.flush().unwrap();
    }

    #[allow(unused_variables)]
    fn receive_add(& mut self) -> Option<i32> {
      let mut result = CalculatorAddResult { success: None };
      let (fname, mtype, rseqid) = self.protocol.read_message_begin(& mut self.transport);
      match mtype {
        MessageType::MtException => {
            // TODO
            //let x = ApplicationException;
            //x.read(& mut self protocol)
            //self.protocol.read_message_end();
            //transport.read_end();
            //throw x  
        }
        MessageType::MtReply => {
            match fname.as_slice() {
                "add" => {
                    result.read(&self.protocol, & mut self.transport);
                }
                _ => {
                    self.protocol.skip(& mut self.transport, Type::TStruct);
                }
            }
            self.protocol.read_message_end(& mut self.transport);
            //self.transport.read_end();
        }
        _ => {
            self.protocol.skip(& mut self.transport, Type::TStruct);
            self.protocol.read_message_end(& mut self.transport);
            //self.transport.read_end(); 
        }
      }
      result.success
    }
}

pub fn main() {
    let server_address = "127.0.0.1:9090";
    let addr: ip::SocketAddr = std::str::FromStr::from_str(server_address)
        .expect("bad server address");
    let tcp = std::io::TcpStream::connect(addr).unwrap();

    let mut client = CalculatorClient{ protocol: BinaryProtocol, transport: tcp };

    client.ping();
    println!("ping()");

    println!("1 + 1 = {}", client.add(1, 1).unwrap());

    println!("PASS");
}
