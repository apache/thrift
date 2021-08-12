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

include(CMakePackageConfigHelpers)
set(PACKAGE_INCLUDE_INSTALL_DIR "${includedir}/thrift")
set(PACKAGE_CMAKE_INSTALL_DIR "${cmakedir}/thrift")
set(PACKAGE_BIN_INSTALL_DIR "${exec_prefix}")

# In CYGWIN enviroment below commands does not work properly
if (NOT CYGWIN)
		configure_package_config_file("${CMAKE_CURRENT_SOURCE_DIR}/build/cmake/ThriftConfig.cmake.in"
						"${CMAKE_CURRENT_BINARY_DIR}/ThriftConfig.cmake"
						INSTALL_DESTINATION "${CMAKE_INSTALL_DIR}/thrift"
						PATH_VARS
						PACKAGE_INCLUDE_INSTALL_DIR
						PACKAGE_CMAKE_INSTALL_DIR
						PACKAGE_BIN_INSTALL_DIR
						)

		write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/ThriftConfigVersion.cmake"
						VERSION ${thrift_VERSION_MAJOR}.${thrift_VERSION_MINOR}.${thrift_VERSION_PATCH}
						COMPATIBILITY SameMajorVersion
						)

		install(FILES "${CMAKE_CURRENT_BINARY_DIR}/ThriftConfig.cmake"
						"${CMAKE_CURRENT_BINARY_DIR}/ThriftConfigVersion.cmake"
						DESTINATION "${CMAKE_INSTALL_DIR}/thrift")
endif()
