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

#![allow(unused_must_use)]
#![allow(unused_variables)]
#![allow(dead_code)]

#[macro_use]
extern crate clap;

extern crate rift;
extern crate rift_test;

use rift::protocol::{TBinaryInputProtocolFactory, TBinaryOutputProtocolFactory, TCompactInputProtocolFactory, TCompactOutputProtocolFactory, TInputProtocolFactory, TOutputProtocolFactory};
use rift::transport::{TFramedTransportFactory, TTransportFactory};
use rift::server::TSimpleServer;
use rift_test::base_two::{Napkin, Ramen, NapkinServiceSyncHandler, RamenServiceSyncHandler};
use rift_test::midlayer::{Meal, MealServiceSyncHandler, MealServiceSyncProcessor};
use rift_test::ultimate::{FullMeal, FullMealAndDrinks, FullMealServiceSyncHandler};
use rift_test::ultimate::{FullMealAndDrinksServiceSyncHandler};

// IMPORTANT: this code is never meant to be run; it's simply to ensure that service extension works
fn main() {
    let matches = clap_app!(rust_kitchen_sink_server =>
        (version: "1.0")
        (author: "Allen George <allen.george@gmail.com>")
        (about: "Rust Thrift Kitchen Sink test server")
        (@arg port: --port +takes_value "port on which the test server listens")
        (@arg protocol: --protocol +takes_value "Thrift protocol implementation to use (\"binary\", \"compact\")")
        (@arg service: --service +takes_value "Service type to contact (\"part\", \"full\")")
    ).get_matches();

    let port = value_t!(matches, "port", u16).unwrap_or(9090);
    let protocol = matches.value_of("protocol").unwrap_or("compact");
    let service = matches.value_of("service").unwrap_or("part");
    let listen_address = format!("127.0.0.1:{}", port);

    println!("binding to {}", listen_address);

    let (i_transport_factory, o_transport_factory): (Box<TTransportFactory>, Box<TTransportFactory>) =
        (
            Box::new(TFramedTransportFactory {}),
            Box::new(TFramedTransportFactory {})
        );

    let (i_protocol_factory, o_protocol_factory): (Box<TInputProtocolFactory>, Box<TOutputProtocolFactory>) = match &*protocol {
        "binary" => {
            (
                Box::new(TBinaryInputProtocolFactory {}),
                Box::new(TBinaryOutputProtocolFactory {})
            )
        },
        "compact" => {
            (
                Box::new(TCompactInputProtocolFactory {}),
                Box::new(TCompactOutputProtocolFactory {})
            )
        },
        unknown => {
            println!("unsupported transport type {}", unknown);
            std::process::exit(1)
        }
    };

    // FIXME: should processor be boxed as well? [sigh] I hate Rust generics implementation
    let mut server = match &*service {
        "part" => {
            let processor = MealServiceSyncProcessor::new(PartHandler {});
            TSimpleServer::new(
                i_transport_factory,
                i_protocol_factory,
                o_transport_factory,
                o_protocol_factory,
                processor
            )
        },
//        "full" => {
//            let processor = FullMealAndDrinksServiceSyncProcessor::new(FullHandler {});
//            TSimpleServer::new(
//                i_transport_factory,
//                i_protocol_factory,
//                o_transport_factory,
//                o_protocol_factory,
//                processor
//            )
//        },
        unknown => {
            println!("unsupported service type {}", unknown);
            std::process::exit(1)
        }
    };

    match server.listen(&listen_address) {
        Ok(_)  => println!("listen completed successfully"),
        Err(e) => println!("listen failed with error {:?}", e),
    }
}

struct PartHandler;
impl MealServiceSyncHandler for PartHandler {
    fn handle_meal(&mut self) -> rift::Result<Meal> {
        unimplemented!()
    }
}
impl RamenServiceSyncHandler for PartHandler {
    fn handle_ramen(&mut self, requested_noodle_count: i32) -> rift::Result<Ramen> {
        unimplemented!()
    }
}
impl NapkinServiceSyncHandler for PartHandler {
    fn handle_napkin(&mut self) -> rift::Result<Napkin> {
        unimplemented!()
    }
}

//
// full service
//

struct FullHandler;
impl FullMealAndDrinksServiceSyncHandler for FullHandler {
    fn handle_full_meal_and_drinks(&mut self) -> rift::Result<FullMealAndDrinks> {
        unimplemented!()
    }
}
impl FullMealServiceSyncHandler for FullHandler {
    fn handle_full_meal(&mut self) -> rift::Result<FullMeal> {
        unimplemented!()
    }
}
impl MealServiceSyncHandler for FullHandler {
    fn handle_meal(&mut self) -> rift::Result<Meal> {
        unimplemented!()
    }
}
impl RamenServiceSyncHandler for FullHandler {
    fn handle_ramen(&mut self, requested_noodle_count: i32) -> rift::Result<Ramen> {
        unimplemented!()
    }
}
impl NapkinServiceSyncHandler for FullHandler {
    fn handle_napkin(&mut self) -> rift::Result<Napkin> {
        unimplemented!()
    }
}
