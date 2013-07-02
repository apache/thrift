# encoding: UTF-8
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

require 'spec_helper'
require File.expand_path("#{File.dirname(__FILE__)}/compact_protocol_spec_shared")

describe Thrift::BufferedLayeredCompactProtocol do


  it_should_behave_like 'a compact protocol'

  def protocol_class
    Thrift::BufferedLayeredCompactProtocol
  end
end

describe Thrift::BypassLayeredCompactProtocol do


  it_should_behave_like 'a compact protocol'

  def protocol_class
    Thrift::BypassLayeredCompactProtocol
  end
end