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

#[allow(dead_code)]
struct ThriftBinaryProtocol {
    x: i32,
}

impl ThriftBinaryProtocol {

    fn new() -> ThriftBinaryProtocol {
        ThriftBinaryProtocol { x: 0 }
    }

    fn write_message_begin(&self, name: &str, message_type: i32, seq_id: i32) -> i32 {
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
    
    fn write_message_end(&self) {
        println!("Protocol: end");
    }
}

#[allow(dead_code)]
struct CalculatorPingArgs {
    dummy: i32  // TODO
}

impl CalculatorPingArgs {

    fn new() -> CalculatorPingArgs {
        CalculatorPingArgs { dummy: 1 }
    }
    
    fn write(&self, oprot: &ThriftBinaryProtocol) {
        println!("CalculatorPingArgs::write");
    }
}

#[allow(dead_code)]
struct CalculatorClient {
    oprotocol: ThriftBinaryProtocol,
    iprotocol: ThriftBinaryProtocol,
}

impl CalculatorClient {

    fn new(protocol: ThriftBinaryProtocol) -> CalculatorClient {
        CalculatorClient { oprotocol: protocol, iprotocol: protocol }
    }
    
    fn ping(&self) {
        self.send_ping();
        self.receive_ping();
    }
    
    fn send_ping(&self) {
        let cseqid: i32 = 0;
        let T_CALL = 0; // TODO
        self.oprotocol.write_message_begin("ping", T_CALL, cseqid);
        
        let args = CalculatorPingArgs::new();
        args.write(&self.oprotocol);
        
        self.oprotocol.write_message_end();
        // TODO:
//        self.oprotocol.get_transport().write_end();
//        self.oprotocol.get_transport().flush();
    }

    fn receive_ping(&self) {
        // TODO
    }
}

pub fn main() {
    //let socket = ThriftSocket::new("localhost", 9090);
    //let transport = ThriftBufferedTransport::new(socket);

    let protocol = ThriftBinaryProtocol::new( /*transport*/);
    let client = CalculatorClient::new(protocol);
    
    //transport.open();
    
    client.ping();

    println!("PASS")
}
