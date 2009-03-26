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

$:.unshift File.dirname(__FILE__)

require 'thrift/core_ext'
require 'thrift/exceptions'
require 'thrift/types'
require 'thrift/processor'
require 'thrift/client'
require 'thrift/struct'
require 'thrift/protocol'
require 'thrift/protocol/binaryprotocol'
require 'thrift/protocol/compact_protocol'
require 'thrift/transport'
require 'thrift/transport/socket'
require 'thrift/server'
require "thrift/thrift_native"