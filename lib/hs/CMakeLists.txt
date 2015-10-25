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

# Rebuild when any of these files changes
set(haskell_sources
    src/Thrift.hs
    src/Thrift/Arbitraries.hs
    src/Thrift/Protocol.hs
    src/Thrift/Protocol/Binary.hs
    src/Thrift/Protocol/Compact.hs
    src/Thrift/Protocol/JSON.hs
    src/Thrift/Protocol/JSONUtils.hs
    src/Thrift/Protocol/PrettyJSON.hs
    src/Thrift/Protocol/SimpleJSON.hs
    src/Thrift/Server.hs
    src/Thrift/Transport.hs
    src/Thrift/Transport/Empty.hs
    src/Thrift/Transport/Framed.hs
    src/Thrift/Transport/Handle.hs
    src/Thrift/Transport/HttpClient.hs
    src/Thrift/Transport/IOBuffer.hs
    src/Thrift/Types.hs
    Thrift.cabal
)

set(haskell_artifacts thrift_cabal.stamp)
# Adding *.hi files so that any missing file triggers the build
foreach(SRC ${haskell_sources})
    get_filename_component(EX ${SRC} EXT)
    if(${EX} STREQUAL ".hs")
        file(RELATIVE_PATH REL ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_SOURCE_DIR}/${SRC})
        get_filename_component(DIR ${REL} DIRECTORY)
        get_filename_component(BASE ${REL} NAME_WE)
        list(APPEND haskell_artifacts dist/build/${DIR}/${BASE}.hi)
    endif()
endforeach()

add_custom_command(
    OUTPUT ${haskell_artifacts}
    COMMAND ${CABAL} update
    # Build dependencies first without --builddir, otherwise it fails.
    COMMAND ${CABAL} install --dependencies-only
    COMMAND ${CABAL} install --builddir=${CMAKE_CURRENT_BINARY_DIR}/dist
    COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_BINARY_DIR}/thrift_cabal.stamp
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${haskell_sources}
    COMMENT "Building Haskell library")

add_custom_target(haskell_library ALL
    DEPENDS ${haskell_artifacts})

set(hs_unittest_gen
    Hs_test_Consts.hs
    Hs_test_Types.hs
    IncludeTest_Consts.hs
    IncludeTest_Types.hs
    ThriftTest.hs
    ThriftTest_Client.hs
    ThriftTest_Consts.hs
    ThriftTest_Iface.hs
    ThriftTest_Types.hs
)

set(hs_unittest_gen_sources
    ${PROJECT_SOURCE_DIR}/test/ThriftTest.thrift
    src/tests/if/hs_test.thrift
    src/tests/if/IncludeTest.thrift
    src/tests/if/IncludedTest.thrift
)

set(hs_unittest_sources
    src/tests/BinaryTests.hs
    src/tests/CompactTests.hs
    src/tests/IncludeTest.hs
    src/tests/JSONTests.hs
    src/tests/PrettyJSONTests.hs
    src/tests/SimpleJSONTests.hs
)
if(BUILD_TESTING)
    set(hs_unittest_args
        -Wall
        -XScopedTypeVariables
        -i${CMAKE_CURRENT_SOURCE_DIR}/src/tests
        -i${CMAKE_CURRENT_SOURCE_DIR}/src
        -i${CMAKE_CURRENT_BINARY_DIR}/gen-hs
    )
    add_custom_command(OUTPUT ${hs_unittest_gen}
        COMMAND thrift-compiler --gen hs ${PROJECT_SOURCE_DIR}/test/ThriftTest.thrift
        COMMAND thrift-compiler --gen hs ${CMAKE_CURRENT_SOURCE_DIR}/src/tests/if/hs_test.thrift
        COMMAND thrift-compiler --gen hs ${CMAKE_CURRENT_SOURCE_DIR}/src/tests/if/IncludeTest.thrift
        COMMAND thrift-compiler --gen hs ${CMAKE_CURRENT_SOURCE_DIR}/src/tests/if/IncludedTest.thrift
        DEPENDS thrift-compiler ${hs_unittest_gen_sources}
        COMMENT "Generating Haskell stubs for testing")
    add_custom_target(haskell_unittests ALL DEPENDS ${hs_unittest_gen})
    foreach(SRC ${hs_unittest_sources})
        get_filename_component(BASE ${SRC} NAME_WE)
        add_test(NAME HaskellUnitTests-${BASE}
            COMMAND ${RUN_HASKELL} ${hs_unittest_args} ${SRC}
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
    endforeach()
    add_test(NAME CabalCheck
            COMMAND ${CABAL} check
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endif()
