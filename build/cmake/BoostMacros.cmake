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

set(BOOST_MINREV 1.56)

# we are not ready for the new style link targets introduced in
# boost 1.70.0 and cmake 3.14.2 which showed up on appveyor in
# mingw builds
set(Boost_NO_BOOST_CMAKE ON)

macro(REQUIRE_BOOST_HEADERS)
  find_package(Boost ${BOOST_MINREV} QUIET REQUIRED)
  if (NOT Boost_FOUND)
    string(CONCAT fatal_message
        "Boost ${BOOST_MINREV} or later is required to build sources in ${CMAKE_CURRENT_SOURCE_DIR}")
    message(FATAL_ERROR, ${fatal_message})
  endif()
  if (DEFINED Boost_INCLUDE_DIRS)
    # pre-boost 1.70.0 aware cmake, otherwise it is using targets
    include_directories(SYSTEM "${Boost_INCLUDE_DIRS}")
  endif()
endmacro()

macro(REQUIRE_BOOST_LIBRARIES libs)
  message(STATUS "Locating boost libraries required by sources in ${CMAKE_CURRENT_SOURCE_DIR}")
  find_package(Boost ${BOOST_MINREV} REQUIRED COMPONENTS ${${libs}})
  if (NOT Boost_FOUND)
    string(CONCAT fatal_message
        "Boost ${BOOST_MINREV} or later is required to build sources in ${CMAKE_CURRENT_SOURCE_DIR}, "
	    "or use -DBUILD_TESTING=OFF")
    message(FATAL_ERROR, ${fatal_message})
  endif()
endmacro()
