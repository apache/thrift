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


// TODO: move to TProtocol
// Decided to leave the same naming as in the C++ code; to be discussed
#[allow(dead_code)]
#[allow(non_camel_case_types)]
pub enum MessageType {
    T_CALL = 1,
    T_REPLY = 2,
    T_EXCEPTION = 3,
    T_ONEWAY = 4,
}

#[allow(dead_code)]
#[allow(non_camel_case_types)]
pub enum TThriftType {
  T_STOP       = 0,
  T_VOID       = 1,
  T_BOOL       = 2,
  T_BYTE       = 3,
//  T_I08        = 3, TODO
  T_I16        = 6,
  T_I32        = 8,
  T_U64        = 9,
  T_I64        = 10,
  T_DOUBLE     = 4,
  T_STRING     = 11,
//  T_UTF7       = 11, TODO
  T_STRUCT     = 12,
  T_MAP        = 13,
  T_SET        = 14,
  T_LIST       = 15,
  T_UTF8       = 16,
  T_UTF16      = 17,
}

static BINARY_PROTOCOL_VERSION_1: u32 = 0x80010000;


pub trait Transport {

    fn open(& mut self) -> Option<()> {
        // default is nothing
        Some(())
    }

    fn write_i8(& mut self, value: i8) -> i32;

    fn write_be_i32(& mut self, value: i32) -> i32;

    fn write(& mut self, value: &[u8]) -> i32;

    fn write_end(& mut self) -> i32 {
        // default is nothing
        0
    }

    fn flush(& mut self);

    fn close(& mut self) {
        // default is nothing
    }
}

// Matches corresponding C++ class.
// TODO: move to Transport
pub struct ThriftTransport<T: std::io::Writer> {
    writer: T
}
 
impl <T: std::io::Writer> ThriftTransport<T> {

    fn new(w: T) -> ThriftTransport<T> {
        ThriftTransport { writer: w }
    }
}

impl <T: std::io::Writer> Transport for ThriftTransport<T> {

    fn write_i8(& mut self, value: i8) -> i32 {
        match self.writer.write_i8(value) {
          Ok(_) => 4,
          Err(e) => panic!(e),
        }
    }

    fn write_be_i32(& mut self, value: i32) -> i32 {
        match self.writer.write_be_i32(value) {
          Ok(_) => 4,
          Err(e) => panic!(e),
        }
    }

    fn write(& mut self, value: &[u8]) -> i32 {
        match self.writer.write(value) {
          Ok(_) => 4,
          Err(e) => panic!(e),
        }
    }

    fn flush(& mut self) {
        // Flush the underlying transport.
        self.writer.flush();
    }
}

pub trait Protocol {

//    fn get_transport(&'a self) -> & mut ThriftBufferedTransport {
//        self.transport
//    }

    fn write_message_begin(& mut self, name: &str, message_type: MessageType, seq_id: i32) -> i32 {
        println!("Protocol: message begin");
        
        // TODO: strict write
        let mut wsize: i32 = 0;
        wsize += self.write_string(name);
        wsize += self.write_i8(message_type as i8);
        wsize += self.write_i32(seq_id);
        wsize
    }
    
    fn write_string(& mut self, s: &str) -> i32 {
        println!("Protocol: {}", s);
        s.len() as i32
    }
    
    fn write_i8(& mut self, b: i8) -> i32 {
        println!("Protocol: {}", b);
        1
    }
    
    fn write_i32(& mut self, i: i32) -> i32 {
        println!("Protocol: {}", i);
        4
    }
    
    fn write_binary(& mut self, value: &[u8]) -> i32;

    fn write_field_stop(& mut self) -> i32 {
        self.write_i8(TThriftType::T_STOP as i8)
    }

    fn write_message_end(& mut self) -> i32 {
        println!("Protocol: end");
        0
    }

    fn flush(& mut self);
}


// TODO: move to Protocol
struct ThriftBinaryProtocol<T: Transport> {
    transport: T
}
 

impl <T: Transport> ThriftBinaryProtocol<T> {

    fn new(transport: T) -> ThriftBinaryProtocol<T> {
        ThriftBinaryProtocol { transport: transport }
    }
}

impl <T: Transport> Protocol for ThriftBinaryProtocol<T> {

    // TODO: remove
    fn flush(& mut self) {
        self.transport.flush();
    }

  fn write_message_begin(
    & mut self,
    name: &str,
    message_type: MessageType,
    sequence_id: i32
  ) -> i32 {
    let version = BINARY_PROTOCOL_VERSION_1 as i32 | message_type as i32;
    self.write_i32(version);
    self.write_string(name);
    self.write_i32(sequence_id);
    0
  }
    
  fn write_i8(& mut self, value: i8) -> i32 {
      self.transport.write_i8(value)
  }    

  fn write_i32(& mut self, value: i32) -> i32 {
      self.transport.write_be_i32(value)
  }
  
  fn write_string(& mut self, value: &str) -> i32 {
    self.write_binary(value.as_bytes());
    0
  }

  fn write_binary(& mut self, value: &[u8]) -> i32 {
    self.write_i32(value.len() as i32);
    self.transport.write(value)
  }
}

// TODO: shall be generated
// TODO: shall we add smth like _dummy: () here?
#[allow(dead_code)]
struct CalculatorPingArgs;

impl CalculatorPingArgs {

    #[allow(unused_variables)]
    fn write(&self, oprot: & mut Protocol) {
        println!("CalculatorPingArgs::write");

  //        oprot->incrementRecursionDepth();
  //xfer += oprot->writeStructBegin("Calculator_ping_args");

  //xfer += oprot->writeFieldStop();
        oprot.write_field_stop();
  //xfer += oprot->writeStructEnd();
  //oprot->decrementRecursionDepth();

    }
}

struct CalculatorClient<T: Protocol> {
    protocol: T
}

impl <T: Protocol> CalculatorClient<T> {

    fn new(protocol: T) -> CalculatorClient<T> {
        CalculatorClient { protocol: protocol }
    }
    
    fn ping(& mut self) {
        self.send_ping();
        self.receive_ping();
    }
    
    fn send_ping(& mut self) {
        let cseqid: i32 = 0;
        self.protocol.write_message_begin("ping", MessageType::T_CALL, cseqid);
        
        let args = CalculatorPingArgs;
        args.write(& mut self.protocol);
        
        self.protocol.write_message_end();

        //self.oprotocol.get_transport().write_end();
        //self.oprotocol.get_transport().flush();
        self.protocol.flush();
    }

    fn receive_ping(& mut self) {


    }
}

pub fn main() {
    let server_address = "127.0.0.1:9090";
    let addr : std::io::net::ip::SocketAddr = std::str::FromStr::from_str(server_address).expect("bad server address");
    let tcp = std::io::TcpStream::connect(addr).unwrap();
    let transport = ThriftTransport::new(tcp);
    let protocol = ThriftBinaryProtocol::new(transport);
    let mut client = CalculatorClient::new(protocol);

    client.ping();
    client.ping();
    client.ping();
    client.ping();
    client.ping();

    println!("PASS");
}
