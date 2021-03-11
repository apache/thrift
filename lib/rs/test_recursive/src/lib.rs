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

#![allow(dead_code)]

pub mod transit;
pub mod vehicles;
pub mod maintenance;

mod server {
    use crate::maintenance::maintenance_facility::{
        BigBarnSyncHandler, MultimodalFacilitySyncHandler,
    };
    use crate::transit::buses::{Bus, GarageSyncHandler};
    use crate::transit::buses::{Powertrain, Route as BusRoute};
    use crate::transit::light::streetcars::{
        BarnSyncHandler, Flexity, RollingStock, Route, RouteNumber, Streetcar,
    };
    use crate::transit::services::city_services::TransitImprovements;
    use crate::transit::trains::Locomotive;
    use crate::transit::transporters::{FlatcarConsist, SingleVehicleTransporter};
    use crate::vehicles::Material;
    use thrift::Result;

    //
    // implement a whole bunch of handler methods just to make sure I can, and that everything compiles
    //

    pub struct AllInOneHandler;

    impl BigBarnSyncHandler for AllInOneHandler {
        fn handle_add_streetcar(&self, route: Route) -> Result<Streetcar> {
            if let Some(route_number) = route.id {
                match route_number {
                    RouteNumber::LAKESHORE => Ok(Streetcar {
                        id: Some(4417),
                        stock: Some(RollingStock::Flexity(Flexity {
                            materials: Some(vec![Material::STEEL, Material::ALUMINUM]),
                            locomotive: Some(Locomotive::ELECTRIC_PANTOGRAPH),
                        })),
                        route: Some(Route {
                            id: Some(RouteNumber::LAKESHORE),
                            improvements: None,
                        }),
                    }),
                    _ => Err(thrift::Error::from(format!(
                        "Cannot create streetcar for route number {}",
                        route_number.0
                    ))),
                }
            } else {
                Err(thrift::Error::from("Can't add a streetcar"))
            }
        }
    }

    impl BarnSyncHandler for AllInOneHandler {
        fn handle_upgrade_streetcar(&self, streetcar: Streetcar) -> Result<Streetcar> {
            if let Some(rolling_stock) = streetcar.stock {
                match rolling_stock {
                    RollingStock::Clrv(_) => Ok(Streetcar {
                        stock: Some(RollingStock::Flexity(Flexity {
                            materials: Some(vec![Material::STEEL, Material::ALUMINUM]),
                            locomotive: Some(Locomotive::ELECTRIC_PANTOGRAPH),
                        })),
                        ..streetcar
                    }),
                    RollingStock::Flexity(_) => {
                        Err(thrift::Error::from("Streetcar already upgraded"))
                    }
                }
            } else {
                Err(thrift::Error::from("Can't upgrade streetcar"))
            }
        }
    }

    impl MultimodalFacilitySyncHandler for AllInOneHandler {
        fn handle_build_transporter(
            &self,
            source: String,
            destination: String,
            consist: FlatcarConsist,
        ) -> Result<SingleVehicleTransporter> {
            Ok(SingleVehicleTransporter {
                consist: Some(consist),
                source: Some(source),
                destination: Some(destination),
            })
        }
    }

    impl GarageSyncHandler for AllInOneHandler {
        fn handle_upgrade_bus(&self, bus: Bus) -> Result<Bus> {
            if let Some(p) = bus.powertrain {
                match p {
                    Powertrain::COMPRESSED_NATURAL_GAS => Ok(Bus {
                        powertrain: Some(Powertrain::DIESEL),
                        ..bus
                    }),
                    _ => Err(thrift::Error::from("Cannot upgrade from this powertrain")),
                }
            } else {
                Err(thrift::Error::from("Cannot upgrade bus"))
            }
        }

        fn handle_improvements_for_route(
            &self,
            route: BusRoute,
        ) -> Result<Vec<TransitImprovements>> {
            Ok(route
                .improvements
                .expect("Expecting a list of improvements"))
        }
    }
}

#[cfg(test)]
mod tests {

    //
    // TODO: consider using the generated client/server and doing a round-trip
    //

    use crate::server::AllInOneHandler;
    use crate::transit::buses::{Bus, Powertrain, Route as BusRoute, DEFAULT4WHEELCAPACITY};
    use crate::transit::light::light_rail::Lrt;
    use crate::transit::light::streetcars::{
        BarnSyncHandler, Flexity, RollingStock, Route, RouteNumber, Streetcar, CLRV,
    };
    use crate::transit::services::city_services::TransitImprovements;
    use crate::transit::trains::Locomotive;
    use crate::transit::transporters::{FlatcarConsist, SingleVehicleTransporter};
    use crate::vehicles::{Material, VehicleIdentifier};

    use crate::maintenance::maintenance_facility::{
        BigBarnSyncHandler, MultimodalFacilitySyncHandler,
    };
    use crate::transit::buses::GarageSyncHandler;

    #[test]
    fn handle_add_streetcar_compiles_and_returns_expected_value() {
        let expected = Streetcar {
            id: Some(4417),
            stock: Some(RollingStock::Flexity(Flexity {
                materials: Some(vec![Material::STEEL, Material::ALUMINUM]),
                locomotive: Some(Locomotive::ELECTRIC_PANTOGRAPH),
            })),
            route: Some(Route {
                id: Some(RouteNumber::LAKESHORE),
                improvements: None,
            }),
        };

        let handler = AllInOneHandler {};
        let actual = handler
            .handle_add_streetcar(Route {
                id: Some(RouteNumber::LAKESHORE),
                improvements: None,
            })
            .expect("Expected a result");

        assert_eq!(expected, actual)
    }

    #[test]
    fn handle_upgrade_streetcar_compiles_and_returns_expected_value() {
        let input = Streetcar {
            stock: Some(RollingStock::Clrv(CLRV {
                materials: Some(vec![Material::STEEL, Material::ALUMINUM]),
                locomotive: Some(Locomotive::ELECTRIC_POLE),
            })),
            id: Some(4415),
            route: Some(Route {
                id: Some(RouteNumber::SPADINA),
                improvements: Some(vec![TransitImprovements::DEDICATED_RIGHT_OF_WAY]),
            }),
        };

        let expected = Streetcar {
            stock: Some(RollingStock::Flexity(Flexity {
                materials: Some(vec![Material::STEEL, Material::ALUMINUM]),
                locomotive: Some(Locomotive::ELECTRIC_PANTOGRAPH),
            })),
            id: Some(4415),
            route: Some(Route {
                id: Some(RouteNumber::SPADINA),
                improvements: Some(vec![TransitImprovements::DEDICATED_RIGHT_OF_WAY]),
            }),
        };

        let handler = AllInOneHandler {};
        let actual = handler
            .handle_upgrade_streetcar(input)
            .expect("Expected an upgraded streetcar");

        assert_eq!(expected, actual)
    }

    #[test]
    fn handle_build_transporter_compiles_and_returns_expected_value() {
        let consist = FlatcarConsist::Lrt(Lrt {
            materials: Some(vec![Material::STEEL, Material::ALUMINUM]),
            locomotive: Some(Locomotive::ELECTRIC_PANTOGRAPH),
        });
        let expected = SingleVehicleTransporter {
            consist: Some(consist.clone()),
            source: Some("905".to_owned()),
            destination: Some("416".to_owned()),
        };

        let handler = AllInOneHandler {};
        let actual = handler
            .handle_build_transporter("905".to_owned(), "416".to_owned(), consist)
            .expect("Expected a transporter");

        assert_eq!(expected, actual)
    }

    #[test]
    fn handle_upgrade_bus_compiles_and_returns_expected_value() {
        let bus = Bus {
            identifier: Some(VehicleIdentifier {
                manufacturer: Some("Orion".to_owned()),
                model: Some("Orion 07.501 NG HEV".to_owned()),
                qualifiers: None,
            }),
            capacity: Some(DEFAULT4WHEELCAPACITY),
            powertrain: Some(Powertrain::COMPRESSED_NATURAL_GAS),
            materials: Some(vec![Material::STEEL, Material::ALUMINUM]),
        };

        let expected = Bus {
            powertrain: Some(Powertrain::DIESEL),
            ..(bus.clone())
        };

        let handler = AllInOneHandler {};
        let actual = handler
            .handle_upgrade_bus(bus)
            .expect("Expected improved bus");

        assert_eq!(expected, actual)
    }

    #[test]
    fn handle_improvements_for_route_compiles_and_returns_expected_value() {
        let expected = vec![TransitImprovements::TRANSIT_SIGNAL_PRIORITY];
        let bus_route = BusRoute {
            route_id: Some("320".to_owned()),
            improvements: Some(expected.clone()),
        };

        let handler = AllInOneHandler {};
        let actual = handler
            .handle_improvements_for_route(bus_route)
            .expect("Expected list of transit improvements");

        assert_eq!(expected, actual)
    }
}
