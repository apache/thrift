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

#[macro_use]
extern crate clap;
extern crate rift;
extern crate rift_test; // huh. I have to do this to use my lib

use rift::transport::TTransport;
use rift_test::*;

fn main() {
    // unsupported options:
    // --domain-socket
    // --named-pipe
    // --anon-pipes
    // --ssl
    // --threads
    let matches = clap_app!(rust_test_client =>
        (version: "1.0")
        (author: "Allen George <allen.george@gmail.com>")
        (about: "Rust Thrift test client")
        (@arg host: --host +takes_value "Host on which the Thrift test server is located")
        (@arg port: --port +takes_value "Port on which the Thrift test server is listening")
        (@arg transport: --transport +takes_value "Thrift transport implementation to use")
        (@arg protocol: --protocol +takes_value "Thrift protocol implementation to use")
        (@arg testloops: -n --testloops +takes_value "Number of times to run tests")
    ).get_matches();

    let host = matches.value_of("host").unwrap_or("localhost");
    let port = value_t!(matches, "port", u16).unwrap_or(9090);
    let testloops = value_t!(matches, "testloops", u8).unwrap_or(1);
    let transport = matches.value_of("transport").unwrap_or("buffered");
    let protocol = matches.value_of("protocol").unwrap_or("binary");

    let t = match transport {
        "buffered" => {
            let mut t = rift::transport::TTcpTransport::new(&format!("{}:{}", host, port));
            match t.open() {
                Ok(()) => t,
                Err(e) => { // FIXME: expose "open" through the client interface so I don't have to early open the transport
                    println!("failed to open transport: {:?}", e);
                    std::process::exit(1)
                }
            }
        },
        unmatched => panic!(format!("unsupported transport {}", unmatched)),
    };

    let p = match protocol {
        "binary" => rift::protocol::TBinaryProtocol { strict: true, transport: t},
        unmatched => panic!(format!("unsupported protocol {}", unmatched)),
    };

    println!("connecting to {}:{} with {}//{} stack", host, port, protocol, transport);

    let mut client = TThriftTestSyncClient::new(p);

    for _ in 0..testloops {
        match make_thrift_calls(&mut client) {
            Err(_) => {

                std::process::exit(1)
            },
            Ok(()) => (),
        }
    }
}

fn make_thrift_calls<C: TAbstractThriftTestSyncClient>(client: &mut C) -> Result<(), rift::Error> {
    client.testVoid()
}
