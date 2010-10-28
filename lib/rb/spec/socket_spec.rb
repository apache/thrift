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
require File.dirname(__FILE__) + "/socket_spec_shared"

class ThriftSocketSpec < Spec::ExampleGroup
  include Thrift

  describe Socket do
    before(:each) do
      @socket = Socket.new
      @handle = mock("Handle", :closed? => false)
      @handle.stub!(:close)
      @handle.stub!(:connect_nonblock)
      ::Socket.stub!(:new).and_return(@handle)
    end

    it_should_behave_like "a socket"

    it "should raise a TransportException when it cannot open a socket" do
      ::Socket.should_receive(:new).and_raise(StandardError)
      lambda { @socket.open }.should raise_error(Thrift::TransportException) { |e| e.type.should == Thrift::TransportException::NOT_OPEN }
    end

    it "should open a ::Socket with default args" do
      ::Socket.should_receive(:new).and_return(mock("Handle", :connect_nonblock => true))
      ::Socket.should_receive(:getaddrinfo).with("localhost", 9090).and_return([[]])
      ::Socket.should_receive(:sockaddr_in)
      @socket.open
    end

    it "should accept host/port options" do
      ::Socket.should_receive(:new).and_return(mock("Handle", :connect_nonblock => true))
      ::Socket.should_receive(:getaddrinfo).with("my.domain", 1234).and_return([[]])
      ::Socket.should_receive(:sockaddr_in)
      Socket.new('my.domain', 1234).open
    end

    it "should accept an optional timeout" do
      ::Socket.stub!(:new)
      Socket.new('localhost', 8080, 5).timeout.should == 5
    end
  end
end
