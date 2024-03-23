#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

# intentionally added a cyclic dependency between '.' and 'light'
SUBDIRS = . light services

THRIFT = $(top_builddir)/compiler/cpp/thrift

stubs: ../Vehicles.thrift Buses.thrift Trains.thrift Transporters.thrift services/CityServices.thrift light/LightRail.thrift light/Streetcars.thrift $(THRIFT)
	$(THRIFT) -I . -I ../ -I ./services -I ./light -out . --gen rs Buses.thrift
	$(THRIFT) -I . -I ../ -I ./services -I ./light -out . --gen rs Trains.thrift
	$(THRIFT) -I . -I ../ -I ./services -I ./light -out . --gen rs Transporters.thrift

check: stubs

clean-local:
	-$(RM) buses.rs
	-$(RM) trains.rs
	-$(RM) transporters.rs

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	mod.rs \
	Buses.thrift \
	Trains.thrift \
	Transporters.thrift
