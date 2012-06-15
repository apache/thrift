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

require File.expand_path("#{File.dirname(__FILE__)}/spec_helper")
require 'thrift/server/thin_http_server'

class ThriftHTTPServerSpec < Spec::ExampleGroup
  include Thrift

  describe ThinHTTPServer do

    describe "#initialize" do

      describe "when using the defaults" do

        it "binds to port 80"
        it "listens on 0.0.0.0"
        it "has a path of '/'"
        it "uses the BinaryProtocolFactory"

      end

      describe "when using the options" do

        it 'accepts :ip'
        it 'accepts :port'
        it 'accepts :path'
        it 'accepts :protocol_factory'

      end

      it "creates an instance of the Thin::Server"

    end

    describe ".serve" do

      describe "404 response" do

        it 'receives a non-POST'
        it 'receives a header other than application/x-thrift'

      end

      describe "200 response" do

        it 'creates a Rack::Response'
        it 'creates an IOStreamTransport'
        it 'fetches the right protocol based on the Transport'
        it 'returns the response'

      end

    end

  end
end
