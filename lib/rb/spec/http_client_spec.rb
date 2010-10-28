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

require File.dirname(__FILE__) + '/spec_helper'

class ThriftHTTPClientTransportSpec < Spec::ExampleGroup
  include Thrift

  describe HTTPClientTransport do
    before(:each) do
      @client = HTTPClientTransport.new("http://my.domain.com/path/to/service")
    end

    it "should always be open" do
      @client.should be_open
      @client.close
      @client.should be_open
    end

    it "should post via HTTP and return the results" do
      @client.write "a test"
      @client.write " frame"
      Net::HTTP.should_receive(:new).with("my.domain.com", 80).and_return do
        mock("Net::HTTP").tee do |http|
          http.should_receive(:use_ssl=).with(false)
          http.should_receive(:post).with("/path/to/service", "a test frame", {"Content-Type"=>"application/x-thrift"}).and_return([nil, "data"])
        end
      end
      @client.flush
      @client.read(10).should == "data"
    end
  end
end
