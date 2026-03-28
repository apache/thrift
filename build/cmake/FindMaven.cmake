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


#  MAVEN_FOUND - system has Maven
#  MAVEN_EXECUTABLE - the Maven executable
#
# It will search the environment variable MAVEN_HOME if it is set

include(FindPackageHandleStandardArgs)

find_program(MAVEN_EXECUTABLE NAMES mvn PATHS $ENV{MAVEN_HOME}/bin NO_CMAKE_FIND_ROOT_PATH)
find_package_handle_standard_args(Maven DEFAULT_MSG MAVEN_EXECUTABLE)
mark_as_advanced(MAVEN_EXECUTABLE)
