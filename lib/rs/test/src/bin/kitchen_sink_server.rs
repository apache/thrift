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

use clap::{clap_app, value_t};
use log::*;

use thrift::protocol::{
    TBinaryInputProtocolFactory, TBinaryOutputProtocolFactory, TCompactInputProtocolFactory,
    TCompactOutputProtocolFactory, TInputProtocolFactory, TOutputProtocolFactory,
};
use thrift::server::TServer;
use thrift::transport::{
    TFramedReadTransportFactory, TFramedWriteTransportFactory, TReadTransportFactory,
    TWriteTransportFactory,
};

use crate::Socket::{ListenAddress, UnixDomainSocket};
use kitchen_sink::base_one::Noodle;
use kitchen_sink::base_two::{
    BrothType, Napkin, NapkinServiceSyncHandler, Ramen, RamenServiceSyncHandler,
};
use kitchen_sink::midlayer::{
    Dessert, Meal, MealServiceSyncHandler, MealServiceSyncProcessor, Pie,
};
use kitchen_sink::recursive;
use kitchen_sink::ultimate::FullMealAndDrinksServiceSyncHandler;
use kitchen_sink::ultimate::{
    Drink, FullMeal, FullMealAndDrinks, FullMealAndDrinksServiceSyncProcessor,
    FullMealServiceSyncHandler,
};

enum Socket {
    ListenAddress(String),
    UnixDomainSocket(String),
}

fn main() {
    match run() {
        Ok(()) => println!("kitchen sink server completed successfully"),
        Err(e) => {
            println!("kitchen sink server failed with error {:?}", e);
            std::process::exit(1);
        }
    }
}

fn run() -> thrift::Result<()> {
    let matches = clap_app!(rust_kitchen_sink_server =>
        (version: "0.1.0")
        (author: "Apache Thrift Developers <dev@thrift.apache.org>")
        (about: "Thrift Rust kitchen sink test server")
        (@arg port: --port +takes_value "Port on which the Thrift test server listens")
        (@arg domain_socket: --("domain-socket") + takes_value "Unix Domain Socket on which the Thrift test server listens")
        (@arg protocol: --protocol +takes_value "Thrift protocol implementation to use (\"binary\", \"compact\")")
        (@arg service: --service +takes_value "Service type to contact (\"part\", \"full\", \"recursive\")")
    )
            .get_matches();

    let port = value_t!(matches, "port", u16).unwrap_or(9090);
    let domain_socket = matches.value_of("domain_socket");
    let protocol = matches.value_of("protocol").unwrap_or("compact");
    let service = matches.value_of("service").unwrap_or("part");
    let listen_address = format!("127.0.0.1:{}", port);

    let socket = match domain_socket {
        None => {
            info!("Server is binding to {}", listen_address);
            Socket::ListenAddress(listen_address)
        }
        Some(domain_socket) => {
            info!("Server is binding to {} (UDS)", domain_socket);
            Socket::UnixDomainSocket(domain_socket.to_string())
        }
    };

    let r_transport_factory = TFramedReadTransportFactory::new();
    let w_transport_factory = TFramedWriteTransportFactory::new();

    let (i_protocol_factory, o_protocol_factory): (
        Box<dyn TInputProtocolFactory>,
        Box<dyn TOutputProtocolFactory>,
    ) = match &*protocol {
        "binary" => (
            Box::new(TBinaryInputProtocolFactory::new()),
            Box::new(TBinaryOutputProtocolFactory::new()),
        ),
        "compact" => (
            Box::new(TCompactInputProtocolFactory::new()),
            Box::new(TCompactOutputProtocolFactory::new()),
        ),
        unknown => {
            return Err(format!("unsupported transport type {}", unknown).into());
        }
    };

    // FIXME: should processor be boxed as well?
    //
    // [sigh] I hate Rust generics implementation
    //
    // I would have preferred to build a server here, return it, and then do
    // the common listen-and-handle stuff, but since the server doesn't have a
    // common type (because each match arm instantiates a server with a
    // different processor) this isn't possible.
    //
    // Since what I'm doing is uncommon I'm just going to duplicate the code
    match &*service {
        "part" => run_meal_server(
            socket,
            r_transport_factory,
            i_protocol_factory,
            w_transport_factory,
            o_protocol_factory,
        ),
        "full" => run_full_meal_server(
            socket,
            r_transport_factory,
            i_protocol_factory,
            w_transport_factory,
            o_protocol_factory,
        ),
        "recursive" => run_recursive_server(
            socket,
            r_transport_factory,
            i_protocol_factory,
            w_transport_factory,
            o_protocol_factory,
        ),
        unknown => Err(format!("unsupported service type {}", unknown).into()),
    }
}

fn run_meal_server<RTF, IPF, WTF, OPF>(
    socket: Socket,
    r_transport_factory: RTF,
    i_protocol_factory: IPF,
    w_transport_factory: WTF,
    o_protocol_factory: OPF,
) -> thrift::Result<()>
where
    RTF: TReadTransportFactory + 'static,
    IPF: TInputProtocolFactory + 'static,
    WTF: TWriteTransportFactory + 'static,
    OPF: TOutputProtocolFactory + 'static,
{
    let processor = MealServiceSyncProcessor::new(PartHandler {});
    let mut server = TServer::new(
        r_transport_factory,
        i_protocol_factory,
        w_transport_factory,
        o_protocol_factory,
        processor,
        1,
    );

    match socket {
        ListenAddress(listen_address) => server.listen(listen_address),
        UnixDomainSocket(s) => server.listen_uds(s),
    }
}

fn run_full_meal_server<RTF, IPF, WTF, OPF>(
    socket: Socket,
    r_transport_factory: RTF,
    i_protocol_factory: IPF,
    w_transport_factory: WTF,
    o_protocol_factory: OPF,
) -> thrift::Result<()>
where
    RTF: TReadTransportFactory + 'static,
    IPF: TInputProtocolFactory + 'static,
    WTF: TWriteTransportFactory + 'static,
    OPF: TOutputProtocolFactory + 'static,
{
    let processor = FullMealAndDrinksServiceSyncProcessor::new(FullHandler {});
    let mut server = TServer::new(
        r_transport_factory,
        i_protocol_factory,
        w_transport_factory,
        o_protocol_factory,
        processor,
        1,
    );

    match socket {
        ListenAddress(listen_address) => server.listen(listen_address),
        UnixDomainSocket(s) => server.listen_uds(s),
    }
}

struct PartHandler;

impl MealServiceSyncHandler for PartHandler {
    fn handle_meal(&self) -> thrift::Result<Meal> {
        println!("part: handling meal call");
        Ok(meal())
    }
}

impl RamenServiceSyncHandler for PartHandler {
    fn handle_ramen(&self, _: i32) -> thrift::Result<Ramen> {
        println!("part: handling ramen call");
        Ok(ramen())
    }
}

impl NapkinServiceSyncHandler for PartHandler {
    fn handle_napkin(&self) -> thrift::Result<Napkin> {
        println!("part: handling napkin call");
        Ok(napkin())
    }
}

// full service
//

struct FullHandler;

impl FullMealAndDrinksServiceSyncHandler for FullHandler {
    fn handle_full_meal_and_drinks(&self) -> thrift::Result<FullMealAndDrinks> {
        println!("full_meal_and_drinks: handling full meal and drinks call");
        Ok(FullMealAndDrinks::new(full_meal(), Drink::CANADIAN_WHISKY))
    }

    fn handle_best_pie(&self) -> thrift::Result<Pie> {
        println!("full_meal_and_drinks: handling pie call");
        Ok(Pie::MISSISSIPPI_MUD) // I prefer Pie::Pumpkin, but I have to check that casing works
    }
}

impl FullMealServiceSyncHandler for FullHandler {
    fn handle_full_meal(&self) -> thrift::Result<FullMeal> {
        println!("full: handling full meal call");
        Ok(full_meal())
    }
}

impl MealServiceSyncHandler for FullHandler {
    fn handle_meal(&self) -> thrift::Result<Meal> {
        println!("full: handling meal call");
        Ok(meal())
    }
}

impl RamenServiceSyncHandler for FullHandler {
    fn handle_ramen(&self, _: i32) -> thrift::Result<Ramen> {
        println!("full: handling ramen call");
        Ok(ramen())
    }
}

impl NapkinServiceSyncHandler for FullHandler {
    fn handle_napkin(&self) -> thrift::Result<Napkin> {
        println!("full: handling napkin call");
        Ok(napkin())
    }
}

fn full_meal() -> FullMeal {
    FullMeal::new(meal(), Dessert::Port("Graham's Tawny".to_owned()))
}

fn meal() -> Meal {
    Meal::new(noodle(), ramen())
}

fn noodle() -> Noodle {
    Noodle::new("spelt".to_owned(), 100)
}

fn ramen() -> Ramen {
    Ramen::new("Mr Ramen".to_owned(), 72, BrothType::MISO)
}

fn napkin() -> Napkin {
    Napkin {}
}

fn run_recursive_server<RTF, IPF, WTF, OPF>(
    socket: Socket,
    r_transport_factory: RTF,
    i_protocol_factory: IPF,
    w_transport_factory: WTF,
    o_protocol_factory: OPF,
) -> thrift::Result<()>
where
    RTF: TReadTransportFactory + 'static,
    IPF: TInputProtocolFactory + 'static,
    WTF: TWriteTransportFactory + 'static,
    OPF: TOutputProtocolFactory + 'static,
{
    let processor = recursive::TestServiceSyncProcessor::new(RecursiveTestServerHandler {});
    let mut server = TServer::new(
        r_transport_factory,
        i_protocol_factory,
        w_transport_factory,
        o_protocol_factory,
        processor,
        1,
    );

    match socket {
        ListenAddress(listen_address) => server.listen(listen_address),
        UnixDomainSocket(s) => server.listen_uds(s),
    }
}

struct RecursiveTestServerHandler;
impl recursive::TestServiceSyncHandler for RecursiveTestServerHandler {
    fn handle_echo_tree(&self, tree: recursive::RecTree) -> thrift::Result<recursive::RecTree> {
        println!("{:?}", tree);
        Ok(tree)
    }

    fn handle_echo_list(&self, lst: recursive::RecList) -> thrift::Result<recursive::RecList> {
        println!("{:?}", lst);
        Ok(lst)
    }

    fn handle_echo_co_rec(&self, item: recursive::CoRec) -> thrift::Result<recursive::CoRec> {
        println!("{:?}", item);
        Ok(item)
    }
}
