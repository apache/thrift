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

use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

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

    let t = Rc::new(RefCell::new(Box::new(t)));

    let p = match protocol {
        "binary" => rift::protocol::TBinaryProtocol { strict: true, transport: t.clone() },
        unmatched => panic!(format!("unsupported protocol {}", unmatched)),
    };

    println!("connecting to {}:{} with {}+{} stack", host, port, protocol, transport);

    let mut client = TThriftTestSyncClient::new(p);

    for _ in 0..testloops {
        match make_thrift_calls(&mut client) {
            Err(e) => {
                println!("test failed with error {:?}", e);
                std::process::exit(1)
            },
            Ok(()) => (),
        }
    }
}

fn make_thrift_calls<C: TAbstractThriftTestSyncClient>(client: &mut C) -> Result<(), rift::Error> {
    try!(client.testVoid());

    try!(verify_expected_result(client.testString("thing".to_owned()), "thing".to_owned()));
    try!(verify_expected_result(client.testBool(true), true));
    try!(verify_expected_result(client.testBool(false), false));
    try!(verify_expected_result(client.testBool(false), false));
    try!(verify_expected_result(client.testByte(42), 42));
    try!(verify_expected_result(client.testI32(1159348374), 1159348374));
    try!(verify_expected_result(client.testI64(-8651829879438294565), -8651829879438294565));
    try!(verify_expected_result(client.testDouble(42.42), 42.42));

    {
 //       let b:[u8; 10] = [0x77, 0x30, 0x30, 0x74, 0x21, 0x20, 0x52, 0x75, 0x73, 0x74];
//        try!(verify_expected_result(client.testBinary(&b), &b)); // FIXME: BROKEN
    }

    // Xtruct
    {
        let x_snd = Xtruct { string_thing: Some("foo".to_owned()), byte_thing: Some(12), i32_thing: Some(219129), i64_thing: Some(12938492818) };
        let x_cmp = Xtruct { string_thing: Some("foo".to_owned()), byte_thing: Some(12), i32_thing: Some(219129), i64_thing: Some(12938492818) };
        try!(verify_expected_result(client.testStruct(x_snd), x_cmp));
    }

    // FIXME: structs should be taken by reference
    // FIXME: apparently optional values are broken
    // Xtruct again (some things null)
    {
        //let x_snd = Xtruct { string_thing: Some("foo".to_owned()), byte_thing: None, i32_thing: None, i64_thing: Some(12938492818) };
        //let x_cmp = Xtruct { string_thing: Some("foo".to_owned()), byte_thing: None, i32_thing: None, i64_thing: Some(12938492818) };
        //try!(verify_expected_result(client.testStruct(x_snd), x_cmp));
    }

    // Xtruct2
    {

    }

    // Exception
    // multi-exception

    try!(client.testOneway(2));

    // final test to verify that the connection is still writable after the one-way call
    try!(client.testVoid());
    Ok(())
}

fn verify_expected_result<T: Debug + PartialEq + Sized>(actual: Result<T, rift::Error>, expected: T) -> Result<(), rift::Error> {
    match actual {
        Ok(v) => {
            if v == expected {
               Ok(())
            } else {
                // FIXME: find a way to use ApplicationError
                Err(rift::Error::Unknown(format!("expected {:?} got {:?}", expected, v)))
            }
        },
        Err(e) => Err(e)
    }
}
