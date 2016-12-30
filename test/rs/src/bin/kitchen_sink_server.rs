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

#![allow(unused_imports)]
#![allow(unused_must_use)]
#![allow(unused_variables)]
#![allow(dead_code)]

#[macro_use]
extern crate clap;

extern crate rift;
extern crate rift_test;

use std::cell::RefCell;
use std::rc::Rc;

//use rift::protocol::{TCompactProtocol, TProtocol};
use rift::transport::{TTcpTransport, TTransport};
use rift_test::base_two::{Napkin, Ramen, TNapkinServiceSyncHandler, TRamenServiceSyncClient, TRamenServiceSyncHandler};
use rift_test::midlayer::{Meal, MealServiceSyncClient, TMealServiceSyncClient, TMealServiceSyncHandler};
use rift_test::ultimate::{FullMeal, FullMealAndDrinks, FullMealServiceSyncClient, TFullMealServiceSyncClient, TFullMealServiceSyncHandler};
use rift_test::ultimate::{TFullMealAndDrinksServiceSyncHandler, FullMealAndDrinksServiceSyncProcessor};

// IMPORTANT: this code is never meant to be run; it's simply to ensure that service extension works
fn main() {
    let matches = clap_app!(rust_kitchen_sink_server =>
        (version: "1.0")
        (author: "Allen George <allen.george@gmail.com>")
        (about: "Rust Thrift kitchen sink test server")
        (@arg server_type: --server +takes_value "server type to use (\"minimal\", \"ultimate\")")
    ).get_matches();

    let server_type = matches.value_of("server").unwrap_or("ultimate");

    let processor = FullMealAndDrinksServiceSyncProcessor::new(Handler {});
}

struct Handler;
impl TFullMealAndDrinksServiceSyncHandler for Handler {
    fn handle_full_meal_and_drinks(&mut self) -> rift::Result<FullMealAndDrinks> {
        unimplemented!()
    }
}
impl TFullMealServiceSyncHandler for Handler {
    fn handle_full_meal(&mut self) -> rift::Result<FullMeal> {
        unimplemented!()
    }
}
impl TMealServiceSyncHandler for Handler {
    fn handle_meal(&mut self) -> rift::Result<Meal> {
        unimplemented!()
    }
}
impl TRamenServiceSyncHandler for Handler {
    fn handle_ramen(&mut self, requested_noodle_count: i32) -> rift::Result<Ramen> {
        unimplemented!()
    }
}
impl TNapkinServiceSyncHandler for Handler {
    fn handle_napkin(&mut self) -> rift::Result<Napkin> {
        unimplemented!()
    }
}
