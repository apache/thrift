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

extern crate rift;
extern crate rift_test;

use std::cell::RefCell;
use std::rc::Rc;

use rift::protocol::{TCompactProtocol, TProtocol};
use rift::transport::{TTcpTransport, TTransport};
use rift_test::base_two::TRamenServiceSyncClient;
use rift_test::midlayer::{MealServiceSyncClient, TMealServiceSyncClient};
use rift_test::ultimate::{FullMealServiceSyncClient, TFullMealServiceSyncClient};

// IMPORTANT: this code is never meant to be run; it's simply to ensure that service extension works
fn main() {
    // midlayer: MealService
    {
        let transport: Box<TTransport> = Box::new(TTcpTransport::new());
        let transport = Rc::new(RefCell::new(transport));
        let protocol: Box<TProtocol> = Box::new(TCompactProtocol::new(transport));
        let mut client = MealServiceSyncClient::new(protocol);

        // only the following two calls work
        client.ramen(100);
        client.meal();
        // client.full_meal(); // <-- IMPORTANT: if you uncomment this, compilation *should* fail
        // this is because the MealService struct does not contain the appropriate service marker
    }

    // ultimate: FullMealService
    {
        let transport: Box<TTransport> = Box::new(TTcpTransport::new());
        let transport = Rc::new(RefCell::new(transport));
        let protocol: Box<TProtocol> = Box::new(TCompactProtocol::new(transport));
        let mut client = FullMealServiceSyncClient::new(protocol);

        // all
        client.ramen(100);
        client.meal();
        client.full_meal();
    }
}
