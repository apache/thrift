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


trait Readable {
    fn read(& mut self, iprot: &Protocol, transport: & mut Transport) -> bool;
}

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

impl Readable for CalculatorAddResult {

    #[allow(unused_variables)]
    fn read(& mut self, iprot: &Protocol, transport: & mut Transport) -> bool {
        let mut have_result = false;

        iprot.read_struct_begin(transport);

        loop {
            let (fname, ftype, fid) = iprot.read_field_begin(transport);
            if ftype == Type::TStop {
                break;
            }
            match (fid, ftype) {
                (0, Type::TI32) => {
                    self.success = Some(iprot.read_i32(transport));
                    have_result = true
                }
                _ => {
                    iprot.skip(transport, ftype);
                }
            }
            iprot.read_field_end(transport);
        }
        iprot.read_struct_end(transport);
        have_result
    }
}


#[derive(Copy)]
#[allow(dead_code)]
enum Operation {
    ADD = 1,
    SUBTRACT = 2,
    MULTIPLY = 3,
    DIVIDE = 4,
}

struct Work {
    num1: i32,
    num2: i32,
    op: Operation,
    comment: Option<String>,
}

impl Work {

    #[allow(unused_variables)]
    fn write(&self, oprot: &Protocol, transport: & mut Transport) {
        oprot.write_struct_begin(transport, "Work");

        oprot.write_field_begin(transport, "num1", Type::TI32, 1);
        oprot.write_i32(transport, self.num1);
        oprot.write_field_end(transport);

        oprot.write_field_begin(transport, "num2", Type::TI32, 2);
        oprot.write_i32(transport, self.num2);
        oprot.write_field_end(transport);

        oprot.write_field_begin(transport, "op", Type::TI32, 3);
        oprot.write_i32(transport, self.op as i32);
        oprot.write_field_end(transport);

        match self.comment {
            Some(ref s) => {
                oprot.write_field_begin(transport, "comment", Type::TString, 4);
                oprot.write_string(transport, s.as_slice());
                oprot.write_field_end(transport);
            }
            _ => {}
        }

        oprot.write_field_stop(transport);
        oprot.write_struct_end(transport);
    }
}

struct CalculatorCalculateArgs {
    logid: i32,
    w: Work,
}

impl CalculatorCalculateArgs {

    #[allow(unused_variables)]
    fn write(&self, oprot: &Protocol, transport: & mut Transport) {
        oprot.write_struct_begin(transport, "Calculator_calculate_args");

        oprot.write_field_begin(transport, "logid", Type::TI32, 1);
        oprot.write_i32(transport, self.logid);
        oprot.write_field_end(transport);

        oprot.write_field_begin(transport, "work", Type::TStruct, 2);
        self.w.write(oprot, transport);
        oprot.write_field_end(transport);

        oprot.write_field_stop(transport);
        oprot.write_struct_end(transport);
    }
}

struct Exception;

#[allow(dead_code)]
struct CalculatorCalculateResult {
    success: Option<i32>,
    ouch: Exception
}

impl Readable for CalculatorCalculateResult {

    #[allow(unused_variables)]
    fn read(& mut self, iprot: &Protocol, transport: & mut Transport) -> bool {
        let mut have_result = false;

        iprot.read_struct_begin(transport);

        loop {
            let (fname, ftype, fid) = iprot.read_field_begin(transport);
            if ftype == Type::TStop {
                break;
            }
            match (fid, ftype) {
                (0, Type::TI32) => {
                    self.success = Some(iprot.read_i32(transport));
                    have_result = true;
                }
                (1, Type::TStruct) => {
                    // FIXME read ouch
                    iprot.skip(transport, ftype);
                }
                _ => {
                    iprot.skip(transport, ftype);
                }
            }
            iprot.read_field_end(transport);
        }
        iprot.read_struct_end(transport);
        have_result
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

    fn receive_add(& mut self) -> Option<i32> {
        let mut result = CalculatorAddResult { success: None };
        if self.receive("add", &mut result) { result.success } else { None }
    }

    #[allow(unused_variables)]
    fn receive<R: Readable>(& mut self, op: &'static str, result: &mut R) -> bool {
        let mut have_result = false;
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
                if fname.as_slice() == op {
                    have_result = result.read(&self.protocol, & mut self.transport);
                }
                else {
                    self.protocol.skip(& mut self.transport, Type::TStruct);
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
        have_result
    }


    fn calculate(& mut self, logid: i32, w: Work) -> Option<i32> {
        self.send_calculate(logid, w);
        self.receive_calculate()
    }

    fn send_calculate(& mut self, logid: i32,  w: Work) {
        let cseqid: i32 = 0;
        self.protocol.write_message_begin(& mut self.transport, "calculate", MessageType::MtCall, cseqid);
        
        let args = CalculatorCalculateArgs { logid: logid, w: w };
        args.write(&self.protocol, & mut self.transport);
        
        self.protocol.write_message_end(& mut self.transport);

        //self.transport.write_end();
        self.transport.flush().unwrap();
    }

    fn receive_calculate(& mut self) -> Option<i32> {
        let mut result = CalculatorCalculateResult { success: None, ouch: Exception };
        // FIXME: handle exception
        if self.receive("calculate", &mut result) { result.success } else { None }
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

    let work = Work { op: Operation::DIVIDE, num1: 1, num2: 0, comment: None };

    match client.calculate(1, work) {
      Some(_) => {
        println!("Whoa? We can divide by zero!");
      }
      None => {
        // FIXME: use thrift exceptions
        println!("Invalid operation")
      }
    }

    let work = Work { op: Operation::SUBTRACT, num1: 15, num2: 10, comment: None };
    println!("15 - 10 = {}", client.calculate(2, work).unwrap());

    println!("PASS");
}
