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

use std::cell::RefCell;
use std::rc::Rc;

use rift::transport::{TFramedTransport, TTcpTransport, TTransport};
use rift::protocol::{TBinaryInputProtocol, TBinaryOutputProtocol, TCompactInputProtocol, TCompactOutputProtocol, TInputProtocol, TOutputProtocol};
use rift_test::base_two::TRamenServiceSyncClient;
use rift_test::midlayer::{MealServiceSyncClient, TMealServiceSyncClient};
use rift_test::ultimate::{FullMealServiceSyncClient, TFullMealServiceSyncClient};

// IMPORTANT: this code is never meant to be run; it's simply to ensure that service extension works
fn main() {
    let matches = clap_app!(rust_test_client =>
        (version: "1.0")
        (author: "Allen George <allen.george@gmail.com>")
        (about: "Rust Thrift Kitchen Sink client")
        (@arg host: --host +takes_value "Host on which the Thrift test server is located")
        (@arg port: --port +takes_value "Port on which the Thrift test server is listening")
        (@arg protocol: --protocol +takes_value "Thrift protocol implementation to use (\"binary\", \"compact\")")
        (@arg service: --service +takes_value "Service type to contact (\"part\", \"full\")")
    ).get_matches();

    let host = matches.value_of("host").unwrap_or("127.0.0.1");
    let port = value_t!(matches, "port", u16).unwrap_or(9090);
    let protocol = matches.value_of("protocol").unwrap_or("compact");
    let service = matches.value_of("service").unwrap_or("part");

    let t = open_tcp_transport(host, port);
    let t: Box<TTransport> = Box::new(TFramedTransport::new(t));
    let t = Rc::new(RefCell::new(t));

    let (i_prot, o_prot): (Box<TInputProtocol>, Box<TOutputProtocol>) = match protocol {
        "binary" => {
            (
                Box::new(TBinaryInputProtocol::new(true, t.clone())),
                Box::new(TBinaryOutputProtocol::new(true, t.clone()))
            )
        },
        "compact" => {
            (
                Box::new(TCompactInputProtocol::new(t.clone())),
                Box::new(TCompactOutputProtocol::new(t.clone())),
            )
        }
        unmatched => panic!(format!("unsupported protocol {}", unmatched)),
    };

    match run(service, i_prot, o_prot) {
        Ok(_)  => println!("succeeded"),
        Err(_) => println!("failed"),
    }
}

fn run(service: &str, i_prot: Box<TInputProtocol>, o_prot: Box<TOutputProtocol>) -> rift::Result<()> {
    match service {
        "full" => run_full_meal_service(i_prot, o_prot),
        "part" => run_meal_service(i_prot, o_prot),
        _  => Err(rift::Error::from(format!("unknown service type {}", service)))
    }
}

fn open_tcp_transport(host: &str, port: u16) -> Rc<RefCell<Box<TTransport>>> {
    let mut t = TTcpTransport::new();
    let t = match t.open(&format!("{}:{}", host, port)) {
        Ok(()) => t,
        Err(e) => {
            println!("failed to open transport: {:?}", e);
            std::process::exit(1)
        }
    };
    let t: Box<TTransport> = Box::new(t);
    Rc::new(RefCell::new(t))
}

fn run_meal_service(i_prot: Box<TInputProtocol>, o_prot: Box<TOutputProtocol>) -> rift::Result<()> {
    let mut client = MealServiceSyncClient::new(i_prot, o_prot);

    // client.full_meal(); // <-- IMPORTANT: if you uncomment this, compilation *should* fail
    // this is because the MealService struct does not contain the appropriate service marker

    // only the following two calls work
    try!(client.ramen(100));
    try!(client.meal());

    Ok(())
}

fn run_full_meal_service(i_prot: Box<TInputProtocol>, o_prot: Box<TOutputProtocol>) -> rift::Result<()> {
    let mut client = FullMealServiceSyncClient::new(i_prot, o_prot);

    try!(client.ramen(100));
    try!(client.meal());
    try!(client.full_meal());

    Ok(())
}
