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
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Debug;
use std::rc::Rc;

use rift::transport::TTransport;
use rift_test::*;

// FIXME: take structs by reference
// FIXME: take &str as well as String
// FIXME: have defaults for structs were all fields are optionals
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

    // primitives
    try!(verify_expected_result(client.testString("thing".to_owned()), "thing".to_owned()));
    try!(verify_expected_result(client.testBool(true), true));
    try!(verify_expected_result(client.testBool(false), false));
    try!(verify_expected_result(client.testBool(false), false));
    try!(verify_expected_result(client.testByte(42), 42));
    try!(verify_expected_result(client.testI32(1159348374), 1159348374));
    try!(verify_expected_result(client.testI64(-8651829879438294565), -8651829879438294565));
    //try!(verify_expected_result(client.testDouble(42.42), 42.42));

    // typedef
    {
        let u_snd: UserId  = 2348;
        let u_cmp: UserId  = 2348;
        try!(verify_expected_result(client.testTypedef(u_snd), u_cmp));
    }

    // enum
    {
        try!(verify_expected_result(client.testEnum(Numberz::TWO), Numberz::TWO));
    }

    {
 //       let b:[u8; 10] = [0x77, 0x30, 0x30, 0x74, 0x21, 0x20, 0x52, 0x75, 0x73, 0x74];
//        try!(verify_expected_result(client.testBinary(&b), &b)); // FIXME: BROKEN
    }

    //
    // case t_base_type::TYPE_STRING:
    /*
    if (((t_base_type*)type)->is_binary() && !inkey) {
    out << "WriteBinary(" << name << ")";
    } else {
    out << "WriteString(string(" << name << "))";
    }*/

    // Xtruct
    {
        let x_snd = Xtruct { string_thing: Some("foo".to_owned()), byte_thing: Some(12), i32_thing: Some(219129), i64_thing: Some(12938492818) };
        let x_cmp = Xtruct { string_thing: Some("foo".to_owned()), byte_thing: Some(12), i32_thing: Some(219129), i64_thing: Some(12938492818) };
        try!(verify_expected_result(client.testStruct(x_snd), x_cmp));
    }

    // Xtruct again, with optional values
    {
        let x_snd = Xtruct { string_thing: Some("foo".to_owned()), byte_thing: None, i32_thing: None, i64_thing: Some(12938492818) };
        let x_cmp = Xtruct { string_thing: Some("foo".to_owned()), byte_thing: Some(0), i32_thing: Some(0), i64_thing: Some(12938492818) }; // the C++ server is responding correctly
        try!(verify_expected_result(client.testStruct(x_snd), x_cmp));
    }

    // Xtruct2 (FIXME: try Xtruct2 with optional values)
    {
        let x_snd = Xtruct2 {
            byte_thing: Some(32),
            struct_thing: Some(
                Xtruct {
                    string_thing: Some("foo".to_owned()),
                    byte_thing: Some(1),
                    i32_thing: Some(324382098),
                    i64_thing: Some(12938492818)
                }
            ),
            i32_thing: Some(293481098),
        };
        let x_cmp = Xtruct2 {
            byte_thing: Some(32),
            struct_thing: Some(
                Xtruct {
                    string_thing: Some("foo".to_owned()),
                    byte_thing: Some(1),
                    i32_thing: Some(324382098),
                    i64_thing: Some(12938492818)
                }
            ),
            i32_thing: Some(293481098),
        };
        try!(verify_expected_result(client.testNest(x_snd), x_cmp));
    }

    // vec<i32>
    {
        let mut v_snd: Vec<i32> = Vec::new();
        v_snd.push(29384);
        v_snd.push(238);
        v_snd.push(32498);

        let mut v_cmp: Vec<i32> = Vec::new();
        v_cmp.push(29384);
        v_cmp.push(238);
        v_cmp.push(32498);

        try!(verify_expected_result(client.testList(v_snd), v_cmp));
    }

    // set<i32>
    {
        let mut s_snd: BTreeSet<i32> = BTreeSet::new();
        s_snd.insert(293481);
        s_snd.insert(23);
        s_snd.insert(3234);

        let mut s_cmp: BTreeSet<i32> = BTreeSet::new();
        s_cmp.insert(293481);
        s_cmp.insert(23);
        s_cmp.insert(3234);

        try!(verify_expected_result(client.testSet(s_snd), s_cmp));
    }

    // map<i32, i32>
    {
        let mut m_snd: BTreeMap<i32, i32> = BTreeMap::new();
        m_snd.insert(2, 4);
        m_snd.insert(4, 6);
        m_snd.insert(8, 7);

        let mut m_cmp: BTreeMap<i32, i32> = BTreeMap::new();
        m_cmp.insert(2, 4);
        m_cmp.insert(4, 6);
        m_cmp.insert(8, 7);

        try!(verify_expected_result(client.testMap(m_snd), m_cmp));
    }

    // map<string, string>
    {
        let mut m_snd: BTreeMap<String, String> = BTreeMap::new();
        m_snd.insert("2".to_owned(), "4_string".to_owned());
        m_snd.insert("4".to_owned(), "6_string".to_owned());
        m_snd.insert("8".to_owned(), "7_string".to_owned());

        let mut m_rcv: BTreeMap<String, String> = BTreeMap::new();
        m_rcv.insert("2".to_owned(), "4_string".to_owned());
        m_rcv.insert("4".to_owned(), "6_string".to_owned());
        m_rcv.insert("8".to_owned(), "7_string".to_owned());

        try!(verify_expected_result(client.testStringMap(m_snd), m_rcv));
    }

    // nested map
    // expect : {-4 => {-4 => -4, -3 => -3, -2 => -2, -1 => -1, }, 4 => {1 => 1, 2 => 2, 3 => 3, 4 => 4, }, }
    {
        let mut m_cmp_nested_0: BTreeMap<i32, i32> = BTreeMap::new();
        for i in (-4 as i32)..0 {
            m_cmp_nested_0.insert(i, i);
        }
        let mut m_cmp_nested_1: BTreeMap<i32, i32> = BTreeMap::new();
        for i in 1..5 {
            m_cmp_nested_1.insert(i, i);
        }

        let mut m_cmp: BTreeMap<i32, BTreeMap<i32, i32>> = BTreeMap::new();
        m_cmp.insert(-4, m_cmp_nested_0);
        m_cmp.insert(4, m_cmp_nested_1);

        try!(verify_expected_result(client.testMapMap(42), m_cmp));
    }

    // multi
    {
        let mut m_snd: BTreeMap<i16, String> = BTreeMap::new();
        m_snd.insert(1298, "fizz".to_owned());
        m_snd.insert(-148, "buzz".to_owned());

        let s_cmp = Xtruct {
            string_thing: Some("Hello2".to_owned()),
            byte_thing: Some(1),
            i32_thing: Some(-123948),
            i64_thing: Some(-19234123981),
        };

        try!(verify_expected_result(client.testMulti(1, -123948, -19234123981, m_snd, Numberz::EIGHT, 81), s_cmp));
    }

    // Insanity
    {

    }

    // Exception (expect an Xception to be thrown)
    {
        let r = client.testException("Xception".to_owned());
        let x = try!(match r {
            Err(rift::Error::User(ref e)) => {
                match e.downcast_ref::<Xception>() {
                    Some(x) => Ok(x),
                    None => Err(rift::Error::User("did not get expected Xception struct".into()))
                }
            },
            _ => Err(rift::Error::User("did not get exception".into())),
        });

        let x_cmp = Xception {
            errorCode: Some(1001),
            message: Some("Xception".to_owned()),
        };

        try!(verify_expected_result(Ok(x), &x_cmp));
    }

    // Exception (expect an Error::Application)
    /*
    {
        let r = client.testException("TException".to_owned());
        try!(match r {
            Err(rift::Error::Application(ref e)) => {
                println!("received a {:?}", e);
                Ok(())
            },
            _ => Err(rift::Error::User("did not get exception".into())),
        });
    }
    */

    // Exception (expect nothing)
    {
        let r = client.testException("foo".to_owned());
        try!(match r {
            Ok(_) => Ok(()),
            _ => Err(rift::Error::User("received an exception".into())),
        });
    }

    // Multi-exception (expect an Xception to be thrown)
    {
        let r = client.testMultiException("Xception".to_owned(), "ignored".to_owned());
        let x = try!(match r {
            Err(rift::Error::User(ref e)) => {
                match e.downcast_ref::<Xception>() {
                    Some(x) => Ok(x),
                    None => Err(rift::Error::User("did not get expected Xception struct".into()))
                }
            },
            _ => Err(rift::Error::User("did not get exception".into())),
        });

        let x_cmp = Xception {
            errorCode: Some(1001),
            message: Some("This is an Xception".to_owned()),
        };

        try!(verify_expected_result(Ok(x), &x_cmp));
    }

    // Multi-exception (expect an Xception to be thrown)
    {
        let r = client.testMultiException("Xception2".to_owned(), "ignored".to_owned());
        let x = try!(match r {
            Err(rift::Error::User(ref e)) => {
                match e.downcast_ref::<Xception2>() {
                    Some(x) => Ok(x),
                    None => Err(rift::Error::User("did not get expected Xception struct".into()))
                }
            },
            _ => Err(rift::Error::User("did not get exception".into())),
        });

        let x_cmp = Xception2 {
            errorCode: Some(2002),
            struct_thing: Some(
                Xtruct {
                    string_thing: Some("This is an Xception2".to_owned()),
                    byte_thing: Some(0), // OPT_IN_REQ_OUT weirdness
                    i32_thing: Some(0), // OPT_IN_REQ_OUT weirdness
                    i64_thing: Some(0), // OPT_IN_REQ_OUT weirdness
                }
            ),
        };

        try!(verify_expected_result(Ok(x), &x_cmp));
    }

    // Multi-exception (expect a response)
    {
        let r = client.testMultiException("haha".to_owned(), "RETURNED".to_owned());
        let x = try!(match r {
            Err(e) => Err(rift::Error::User(format!("received an unexpected exception {:?}", e).into())),
            _ => r,
        });

        let x_cmp = Xtruct {
            string_thing: Some("RETURNED".to_owned()),
            byte_thing: Some(0), // OPT_IN_REQ_OUT weirdness
            i32_thing: Some(0), // OPT_IN_REQ_OUT weirdness
            i64_thing: Some(0), // OPT_IN_REQ_OUT weirdness
        };

        try!(verify_expected_result(Ok(x), x_cmp));
    }

    // one-way (delays for 2 seconds
    {
        try!(client.testOneway(2));
    }

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
                Err(rift::Error::User(format!("expected {:?} but got {:?}", &expected, &v).into()))
            }
        },
        Err(e) => Err(e)
    }
}
