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


if(${WITH_PLUGIN})
    # Find required packages
    set(Boost_USE_STATIC_LIBS ON) # Force the use of static boost test framework
    find_package(Boost 1.53.0 REQUIRED COMPONENTS unit_test_framework)
    include_directories(SYSTEM "${Boost_INCLUDE_DIRS}")

    #Make sure gen-cpp files can be included
    include_directories("${CMAKE_CURRENT_BINARY_DIR}")

    set(plugintest_SOURCES
        plugin/conversion_test.cc
    )
    add_executable(plugintest ${plugintest_SOURCES})
    target_link_libraries(plugintest
        thriftc
        ${Boost_LIBRARIES}
    )
    add_test(NAME PluginTest COMMAND plugintest)

    set(thrift-gen-mycpp_SOURCES
        ../src/thrift/generate/t_cpp_generator.cc
        plugin/cpp_plugin.cc
    )
    add_executable(thrift-gen-mycpp ${thrift-gen-mycpp_SOURCES})
    if(WITH_SHARED_LIB AND NOT MSVC)
        target_link_libraries(thrift-gen-mycpp thriftc)
    else()
        target_link_libraries(thrift-gen-mycpp thriftc_static thrift_static)
    endif()

    add_test(
          NAME cmake_trying_to_be_not_shit
          COMMAND ${CMAKE_COMMAND}
              -DTHRIFT_COMPILER=${THRIFT_COMPILER}
              -DBINDIR=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
              -DCURDIR=${CMAKE_CURRENT_BINARY_DIR}
              -DSRCDIR=${CMAKE_CURRENT_SOURCE_DIR}
              -P ${CMAKE_CURRENT_SOURCE_DIR}/cpp_plugin_test.cmake)
endif()
