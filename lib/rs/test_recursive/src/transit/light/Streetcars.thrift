/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 * Contains some contributions under the Thrift Software License.
 * Please see doc/old-thrift-license.txt in the Thrift distribution for
 * details.
 */

include "CityServices.thrift"
include "Trains.thrift"
include "Vehicles.thrift"

namespace rs transit.light

struct CLRV {
  1: list<Vehicles.Material> materials
  2: Trains.Locomotive locomotive
}

struct Flexity {
  1: list<Vehicles.Material> materials
  2: Trains.Locomotive locomotive
}

union RollingStock {
  1: CLRV clrv
  2: Flexity flexity
}

enum RouteNumber {
  Queen = 501
  Downtowner = 502
  Kingston = 503
  King = 504
  Dundas = 505
  Carlton = 506
  Lakeshore = 508
  Harbourfront = 509
  Spadina = 510
  Bathurst = 511
  StClair = 512
}

struct Route {
  1: RouteNumber id
  2: list<CityServices.TransitImprovements> improvements = []  // ABSOLUTELY NONE!
}

struct Streetcar {
  1: i16 id
  2: RollingStock stock
  3: Route route
}

service Barn {
    Streetcar upgradeStreetcar(1: Streetcar streetcar)
}