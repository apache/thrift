# frozen_string_literal: true
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
require File.expand_path("#{File.dirname(__FILE__)}/socket_spec_shared")

describe 'SSLServerSocket' do
  describe Thrift::SSLServerSocket do
    before(:each) do
      @socket = Thrift::SSLServerSocket.new(1234)
    end

    it "should delegate to_io to the underlying SSL server handle" do
      tcp_server = double("TCPServer")
      ssl_server = double("SSLServer")

      allow(TCPServer).to receive(:new).with(nil, 1234).and_return(tcp_server)
      allow(OpenSSL::SSL::SSLServer).to receive(:new).with(tcp_server, nil).and_return(ssl_server)
      allow(ssl_server).to receive(:to_io).and_return(tcp_server)

      @socket.listen
      expect(@socket.to_io).to eq(tcp_server)
    end

    it "should default accepted sockets to a finite client timeout" do
      expect(@socket.client_timeout).to eq(5)
    end

    it "should preserve the positional ssl context argument" do
      context = double("SSLContext")
      socket = Thrift::SSLServerSocket.new(nil, 1234, context)

      expect(socket.ssl_context).to eq(context)
      expect(socket.client_timeout).to eq(5)
    end

    it "should accept a custom client timeout" do
      context = double("SSLContext")
      socket = Thrift::SSLServerSocket.new(nil, 1234, context, client_timeout: 2.5)

      expect(socket.ssl_context).to eq(context)
      expect(socket.client_timeout).to eq(2.5)
    end

    it "should apply the client timeout to accepted sockets" do
      tcp_server = double("TCPServer")
      ssl_server = double("SSLServer")
      sock = double("SSLSocket")

      allow(TCPServer).to receive(:new).with(nil, 1234).and_return(tcp_server)
      allow(OpenSSL::SSL::SSLServer).to receive(:new).with(tcp_server, nil).and_return(ssl_server)
      expect(ssl_server).to receive(:accept).and_return(sock)
      expect(sock).to receive(:setsockopt).with(Socket::IPPROTO_TCP, Socket::TCP_NODELAY, 1)

      trans = double("Socket")
      expect(Thrift::Socket).to receive(:new).and_return(trans)
      expect(trans).to receive(:timeout=).with(Thrift::BaseServerTransport::DEFAULT_CLIENT_TIMEOUT)
      expect(trans).to receive(:handle=).with(sock)

      @socket.listen
      expect(@socket.accept).to eq(trans)
    end

    it "should provide a reasonable to_s" do
      expect(@socket.to_s).to eq("ssl(socket(:1234))")
    end
  end
end
