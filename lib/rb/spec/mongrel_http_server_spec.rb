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
require 'thrift/server/mongrel_http_server'

class ThriftHTTPServerSpec < Spec::ExampleGroup
  include Thrift

  Handler = MongrelHTTPServer::Handler

  describe MongrelHTTPServer do
    it "should have appropriate defaults" do
      mock_factory = mock("BinaryProtocolFactory")
      mock_proc = mock("Processor")
      BinaryProtocolFactory.should_receive(:new).and_return(mock_factory)
      Mongrel::HttpServer.should_receive(:new).with("0.0.0.0", 80).and_return do
        mock("Mongrel::HttpServer").tee do |mock|
          handler = mock("Handler")
          Handler.should_receive(:new).with(mock_proc, mock_factory).and_return(handler)
          mock.should_receive(:register).with("/", handler)
        end
      end
      MongrelHTTPServer.new(mock_proc)
    end

    it "should understand :ip, :port, :path, and :protocol_factory" do
      mock_proc = mock("Processor")
      mock_factory = mock("ProtocolFactory")
      Mongrel::HttpServer.should_receive(:new).with("1.2.3.4", 1234).and_return do
        mock("Mongrel::HttpServer").tee do |mock|
          handler = mock("Handler")
          Handler.should_receive(:new).with(mock_proc, mock_factory).and_return(handler)
          mock.should_receive(:register).with("/foo", handler)
        end
      end
      MongrelHTTPServer.new(mock_proc, :ip => "1.2.3.4", :port => 1234, :path => "foo",
                                             :protocol_factory => mock_factory)
    end

    it "should serve using Mongrel::HttpServer" do
      BinaryProtocolFactory.stub!(:new)
      Mongrel::HttpServer.should_receive(:new).and_return do
        mock("Mongrel::HttpServer").tee do |mock|
          Handler.stub!(:new)
          mock.stub!(:register)
          mock.should_receive(:run).and_return do
            mock("Mongrel::HttpServer.run").tee do |runner|
              runner.should_receive(:join)
            end
          end
        end
      end
      MongrelHTTPServer.new(nil).serve
    end
  end

  describe MongrelHTTPServer::Handler do
    before(:each) do
      @processor = mock("Processor")
      @factory = mock("ProtocolFactory")
      @handler = Handler.new(@processor, @factory)
    end

    it "should return 404 for non-POST requests" do
      request = mock("request", :params => {"REQUEST_METHOD" => "GET"})
      response = mock("response")
      response.should_receive(:start).with(404)
      response.should_not_receive(:start).with(200)
      @handler.process(request, response)
    end

    it "should serve using application/x-thrift" do
      request = mock("request", :params => {"REQUEST_METHOD" => "POST"}, :body => nil)
      response = mock("response")
      head = mock("head")
      head.should_receive(:[]=).with("Content-Type", "application/x-thrift")
      IOStreamTransport.stub!(:new)
      @factory.stub!(:get_protocol)
      @processor.stub!(:process)
      response.should_receive(:start).with(200).and_yield(head, nil)
      @handler.process(request, response)
    end

    it "should use the IOStreamTransport" do
      body = mock("body")
      request = mock("request", :params => {"REQUEST_METHOD" => "POST"}, :body => body)
      response = mock("response")
      head = mock("head")
      head.stub!(:[]=)
      out = mock("out")
      protocol = mock("protocol")
      transport = mock("transport")
      IOStreamTransport.should_receive(:new).with(body, out).and_return(transport)
      @factory.should_receive(:get_protocol).with(transport).and_return(protocol)
      @processor.should_receive(:process).with(protocol, protocol)
      response.should_receive(:start).with(200).and_yield(head, out)
      @handler.process(request, response)
    end
  end
end
