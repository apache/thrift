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
 */

#include <thrift/c_glib/protocol/thrift_binary_protocol.h>
#include <thrift/c_glib/protocol/thrift_protocol.h>
#include <thrift/c_glib/transport/thrift_memory_buffer.h>
#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/thrift_configuration.h>
#include <stdint.h>
#include "gen-c_glib/fuzz_test_no_uuid_types.h"
#include <stdio.h>

// 10MB message size limit to prevent over-allocation during fuzzing
#define FUZZ_MAX_MESSAGE_SIZE (10 * 1024 * 1024)

int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size) {
  GError* error = NULL;
  
  // Create a GByteArray with the fuzz data
  GByteArray* byte_array = g_byte_array_new();
  g_byte_array_append(byte_array, data, size);

  // Create a ThriftConfiguration with message size limits
  ThriftConfiguration* tconfiguration = g_object_new(THRIFT_TYPE_CONFIGURATION, 
                                                     "max_message_size", FUZZ_MAX_MESSAGE_SIZE,
                                                     "max_frame_size", FUZZ_MAX_MESSAGE_SIZE, 
                                                     NULL);

  // Create a memory buffer transport with the byte array and configuration
  ThriftTransport* transport = THRIFT_TRANSPORT(
      g_object_new(THRIFT_TYPE_MEMORY_BUFFER,
                  "buf", byte_array,
                  "buf_size", size,
                  "configuration", tconfiguration,
                  NULL));

  // Create a binary protocol
  ThriftProtocol* protocol = THRIFT_PROTOCOL(
      g_object_new(THRIFT_TYPE_BINARY_PROTOCOL, 
                  "transport", transport,
                  NULL));

  // Create a FuzzTest struct to read into
  FuzzTest* test_struct = g_object_new(TYPE_FUZZ_TEST, NULL);
  FuzzTestClass* cls = FUZZ_TEST_GET_CLASS(test_struct);

  // Try to read the struct from the fuzz data
  THRIFT_STRUCT_CLASS(cls)->read(THRIFT_STRUCT(test_struct), protocol, &error);
  
  // Clean up
  g_object_unref(test_struct);
  g_object_unref(protocol);
  g_object_unref(transport);
  g_object_unref(tconfiguration);
  if (error) {
    g_error_free(error);
  }

  return 0;
} 