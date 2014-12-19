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

#![crate_name="thrift-client"]
#![crate_type="bin"]

//use std::io::net::ip::SocketAddr;
// TODO: does not work
//use std::from_str::from_str;

// TODO: move to TProtocol
// Decided to leave the same naming as in the C++ code; to be discussed
#[allow(dead_code)]
#[allow(non_camel_case_types)]
enum MessageType {
    T_CALL = 1,
    T_REPLY = 2,
    T_EXCEPTION = 3,
    T_ONEWAY = 4,
}


// TODO: move to Socket
#[allow(dead_code)]
struct TSocket {
    host: String,
    port: i32,
}
 
impl TSocket {

    fn new(h: String, p: i32) -> TSocket {
        TSocket { host: h, port: p }
    }

    // TODO: return type
    fn open(&self) -> Option<()> {
        // TODO
        Some(())
    }

    fn flush(&self) {
        // TODO
    }

    fn close(&self) {
        // TODO
    }
}


// TODO: move to Transport
struct ThriftBufferedTransport<'a> {
    socket: &'a TSocket,
}
 
impl<'a> ThriftBufferedTransport<'a> {

    fn new(s: &'a TSocket) -> ThriftBufferedTransport<'a> {
        ThriftBufferedTransport { socket: s }
    }

    fn open(&self) -> Option<()> {
        self.socket.open()
    }

    fn write_end(&self) -> i32 {
        // default is do nothing
        0
    }

    fn flush(&self) {
        // TODO

        // Flush the underlying transport.
        self.socket.flush();
    }

    fn close(&self) {
        // TODO
        self.socket.close();
    }
}


// TODO: move to Protocol
struct ThriftBinaryProtocol<'a> {
    transport: &'a ThriftBufferedTransport<'a>, // TODO: replace with Transport
}
 

impl<'a> ThriftBinaryProtocol<'a> {

    fn new(t: &'a ThriftBufferedTransport) -> ThriftBinaryProtocol<'a> {
        ThriftBinaryProtocol { transport: t }
    }

    fn get_transport(&self) -> &ThriftBufferedTransport {
        self.transport
    }

    fn write_message_begin(&self, name: &str, message_type: MessageType, seq_id: i32) -> i32 {
        println!("Protocol: message begin");
        
        // TODO: strict write
        let mut wsize: i32 = 0;
        wsize += self.write_string(name);
        wsize += self.write_byte(message_type as i8);
        wsize += self.write_i32(seq_id);
        wsize
    }
    
    fn write_string(&self, s: &str) -> i32 {
        println!("Protocol: {}", s);
        s.len() as i32
    }
    
    fn write_byte(&self, b: i8) -> i32 {
        println!("Protocol: {}", b);
        1
    }
    
    fn write_i32(&self, i: i32) -> i32 {
        println!("Protocol: {}", i);
        4
    }
    
    fn write_message_end(&self) -> i32 {
        println!("Protocol: end");
        0
    }
}

// TODO: shall be generated
#[allow(dead_code)]
struct CalculatorPingArgs;

impl CalculatorPingArgs {

    #[allow(unused_variables)]
    fn write(&self, oprot: &ThriftBinaryProtocol) {
        println!("CalculatorPingArgs::write");
    }
}

#[allow(dead_code)]
struct CalculatorClient<'a> {
    oprotocol: &'a ThriftBinaryProtocol<'a>,
    iprotocol: &'a ThriftBinaryProtocol<'a>,
}

impl<'a> CalculatorClient<'a> {

    fn new(protocol: &'a ThriftBinaryProtocol) -> CalculatorClient<'a> {
        CalculatorClient { oprotocol: protocol, iprotocol: protocol }
    }
    
    fn ping(&self) {
        self.send_ping();
        self.receive_ping();
    }
    
    fn send_ping(&self) {
        let cseqid: i32 = 0;
        self.oprotocol.write_message_begin("ping", MessageType::T_CALL, cseqid);
        
        let args = CalculatorPingArgs;
        args.write(self.oprotocol);
        
        self.oprotocol.write_message_end();

        self.oprotocol.get_transport().write_end();
        self.oprotocol.get_transport().flush();
    }

    fn receive_ping(&self) {
        // TODO
    }
}


pub fn main() {
    // TODO: does not work
    //let addr: SocketAddr =
    //    from_str("127.0.0.1:9090").expect("malformed address");
    let socket = TSocket::new(String::from_str("localhost"), 9090);
    let transport = ThriftBufferedTransport::new(&socket);
    let protocol = ThriftBinaryProtocol::new(&transport);
    let client = CalculatorClient::new(&protocol);
    
    transport.open()
        .expect("failed to connect");
    
    client.ping();

    println!("PASS")

    transport.close();
}
