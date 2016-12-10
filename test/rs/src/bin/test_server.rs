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
extern crate rift_test;

use std::collections::{BTreeMap, BTreeSet};
use std::thread;
use std::time::Duration;

use rift::protocol::{TBinaryProtocolFactory, TCompactProtocolFactory, TProtocolFactory};
use rift::server::TSimpleServer;
use rift::transport::{TBufferedTransportFactory, TFramedTransportFactory, TTransportFactory};
use rift_test::*;

fn main() {
    // unsupported options:
    // --domain-socket
    // --named-pipe
    // --ssl
    // --workers
    let matches = clap_app!(rust_test_client =>
        (version: "1.0")
        (author: "Allen George <allen.george@gmail.com>")
        (about: "Rust Thrift test server")
        (@arg port: --port +takes_value "port on which the test server listens")
        (@arg transport: --transport +takes_value "transport implementation to use (\"buffered\", \"framed\")")
        (@arg protocol: --protocol +takes_value "protocol implementation to use (\"binary\", \"compact\")")
        (@arg server_type: --server_type +takes_value "type of server instantiated (\"simple\", \"thread-pool\", \"threaded\", \"non-blocking\")")
    ).get_matches();

    let port = value_t!(matches, "port", u16).unwrap_or(9090);
    let transport = matches.value_of("transport").unwrap_or("buffered");
    let protocol = matches.value_of("protocol").unwrap_or("binary");
    let server_type = matches.value_of("server_type").unwrap_or("simple");
    let listen_address = format!("localhost:{}", port);

    let transport_factory: Box<TTransportFactory> = match &*transport {
        "buffered" => {
            Box::new(TBufferedTransportFactory {})
        },
        "framed" => {
            Box::new(TFramedTransportFactory {})
        },
        unknown => {
            println!("unsupported transport type {}", unknown);
            std::process::exit(1)
        }
    };

    let protocol_factory: Box<TProtocolFactory> = match &*protocol {
        "binary" => {
            Box::new(TBinaryProtocolFactory {})
        },
        "compact" => {
            Box::new(TCompactProtocolFactory {})
        },
        unknown => {
            println!("unsupported transport type {}", unknown);
            std::process::exit(1)
        }
    };

    let processor = TThriftTestProcessor::new(ThriftTestHandler {});

    match &*server_type {
        "simple" => {
            let mut server = TSimpleServer::with_shared(transport_factory, protocol_factory, processor);
            match server.listen(&listen_address) {
                Ok(_) => println!("listen completed successfully"),
                Err(e) => println!("listen failed with error {:?}", e),
            }
        },
        unknown => {
            println!("unsupported server type {}", unknown);
            std::process::exit(1)
        }
    };
}

struct ThriftTestHandler {
    // empty
}

impl TAbstractThriftTestSyncHandler for ThriftTestHandler {
    fn handle_test_void(&mut self) -> rift::Result<()> {
        Ok(())
    }

    fn handle_test_string(&mut self, thing: String) -> rift::Result<String> {
        Ok(thing)
    }

    fn handle_test_bool(&mut self, thing: bool) -> rift::Result<bool> {
        Ok(thing)
    }

    fn handle_test_byte(&mut self, thing: i8) -> rift::Result<i8> {
        Ok(thing)
    }

    fn handle_test_i32(&mut self, thing: i32) -> rift::Result<i32> {
        Ok(thing)
    }

    fn handle_test_i64(&mut self, thing: i64) -> rift::Result<i64> {
        Ok(thing)
    }

    fn handle_test_binary(&mut self, thing: Vec<u8>) -> rift::Result<Vec<u8>> {
        Ok(thing)
    }

    fn handle_test_struct(&mut self, thing: Xtruct) -> rift::Result<Xtruct> {
        Ok(thing)
    }

    fn handle_test_nest(&mut self, thing: Xtruct2) -> rift::Result<Xtruct2> {
        Ok(thing)
    }

    fn handle_test_map(&mut self, thing: BTreeMap<i32, i32>) -> rift::Result<BTreeMap<i32, i32>> {
        Ok(thing)
    }

    fn handle_test_string_map(&mut self, thing: BTreeMap<String, String>) -> rift::Result<BTreeMap<String, String>> {
        Ok(thing)
    }

    fn handle_test_set(&mut self, thing: BTreeSet<i32>) -> rift::Result<BTreeSet<i32>> {
        Ok(thing)
    }

    fn handle_test_list(&mut self, thing: Vec<i32>) -> rift::Result<Vec<i32>> {
        Ok(thing)
    }

    fn handle_test_enum(&mut self, thing: Numberz) -> rift::Result<Numberz> {
        println!("testEnum({:?})", thing);
        Ok(thing)
    }

    fn handle_test_typedef(&mut self, thing: UserId) -> rift::Result<UserId> {
        println!("testTypedef({})", thing);
        Ok(thing)
    }

    /// @return map<i32,map<i32,i32>> - returns a dictionary with these values:
    /// {-4 => {-4 => -4, -3 => -3, -2 => -2, -1 => -1, }, 4 => {1 => 1, 2 => 2, 3 => 3, 4 => 4, }, }
    fn handle_test_map_map(&mut self, hello: i32) -> rift::Result<BTreeMap<i32, BTreeMap<i32, i32>>> {
        println!("testMapMap({})", hello);

        let mut inner_map_0: BTreeMap<i32, i32> = BTreeMap::new();
        for i in -4..(0 as i32) {
           inner_map_0.insert(i, i);
        }

        let mut inner_map_1: BTreeMap<i32, i32> = BTreeMap::new();
        for i in 1..5 {
            inner_map_1.insert(i, i);
        }

        let mut ret_map: BTreeMap<i32, BTreeMap<i32, i32>> = BTreeMap::new();
        ret_map.insert(-4, inner_map_0);
        ret_map.insert(4, inner_map_1);

        Ok(ret_map)
    }

    /// Creates a the returned map with these values and prints it out:
    ///     { 1 => { 2 => argument,
    ///              3 => argument,
    ///            },
    ///       2 => { 6 => <empty Insanity struct>, },
    ///     }
    /// return map<UserId, map<Numberz,Insanity>> - a map with the above values
    fn handle_test_insanity(&mut self, argument: Insanity) -> rift::Result<BTreeMap<UserId, BTreeMap<Numberz, Insanity>>> {
        let mut map_0: BTreeMap<Numberz, Insanity> = BTreeMap::new();
        map_0.insert(Numberz::TWO, argument.clone());
        map_0.insert(Numberz::THREE, argument.clone());

        let mut map_1: BTreeMap<Numberz, Insanity> = BTreeMap::new();
        let insanity = Insanity {
            user_map: None,
            xtructs: None,
        };
        map_1.insert(Numberz::SIX, insanity);

        let mut ret: BTreeMap<UserId, BTreeMap<Numberz, Insanity>> = BTreeMap::new();
        ret.insert(1, map_0);
        ret.insert(2, map_1);

        Ok(ret)
    }

    /// returns an Xtruct with string_thing = "Hello2", byte_thing = arg0, i32_thing = arg1 and i64_thing = arg2
    fn handle_test_multi(&mut self, arg0: i8, arg1: i32, arg2: i64, _: BTreeMap<i16, String>, _: Numberz, _: UserId) -> rift::Result<Xtruct> {
        let x_ret = Xtruct {
            string_thing: Some("Hello2".to_owned()),
            byte_thing: Some(arg0),
            i32_thing: Some(arg1),
            i64_thing: Some(arg2),
        };

        Ok(x_ret)
    }

    /// Print 'testException(%s)' with arg as '%s'
    /// if arg == "Xception" throw Xception with errorCode = 1001 and message = arg
    /// else if arg == "TException" throw TException
    /// else do not throw anything
    fn handle_test_exception(&mut self, arg: String) -> rift::Result<()> {
        println!("testException({})", arg);
        match &*arg {
            "Xception" => Err((Xception { error_code: Some(1001), message: Some(arg) }).into()),
            "TException" => Err("this is a random error".into()),
            _ => Ok(())
        }
    }

    /**
   * Print 'testMultiException(%s, %s)' with arg0 as '%s' and arg1 as '%s'
   * @param string arg - a string indication what type of exception to throw
   * if arg0 == "Xception" throw Xception with errorCode = 1001 and message = "This is an Xception"
   * else if arg0 == "Xception2" throw Xception2 with errorCode = 2002 and struct_thing.string_thing = "This is an Xception2"
   * else do not throw anything
   * @return Xtruct - an Xtruct with string_thing = arg1
   */
    fn handle_test_multi_exception(&mut self, arg0: String, arg1: String) -> rift::Result<Xtruct> {
        unimplemented!()
    }

    fn handle_test_oneway(&mut self, seconds_to_sleep: i32) -> rift::Result<()> {
        thread::sleep(Duration::from_secs(seconds_to_sleep as u64));
        Ok(())
    }
}
