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

struct Lrt {
  1: list<Vehicles.Material> materials
  2: Trains.Locomotive locomotive
}

enum Route {
  EglintonCrosstown = 0
  FinchWest = 1
}

struct Line {
  1: Lrt lrt
  2: Route route
  3: list<CityServices.TransitImprovements> improvements = [] // ABSOLUTELY NONE BY DEFAULT!
}

service Msf {
    Lrt fixLrt(1: Lrt lrt)
}