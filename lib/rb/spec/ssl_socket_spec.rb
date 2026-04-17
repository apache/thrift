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

describe 'SSLSocket' do
  describe Thrift::SSLSocket do
    before(:each) do
      @context = OpenSSL::SSL::SSLContext.new
      @socket = Thrift::SSLSocket.new
      @addrinfo = double("Addrinfo")
      @simple_socket_handle = double("Handle", :closed? => false)
      allow(@simple_socket_handle).to receive(:close)
      allow(@simple_socket_handle).to receive(:setsockopt)
      allow(@simple_socket_handle).to receive(:wait_readable)
      allow(@simple_socket_handle).to receive(:wait_writable)

      @handle = double("SSLHandle", :closed? => false)
      allow(@handle).to receive(:connect).and_return(true)
      allow(@handle).to receive(:connect_nonblock).and_return(@handle)
      allow(@handle).to receive(:close)
      allow(@handle).to receive(:post_connection_check)
      allow(@handle).to receive(:sync_close=)
      allow(@handle).to receive(:to_io).and_return(@simple_socket_handle)

      allow(@addrinfo).to receive(:connect).and_return(@simple_socket_handle)
      allow(Addrinfo).to receive(:foreach).and_yield(@addrinfo)
      allow(OpenSSL::SSL::SSLSocket).to receive(:new).and_return(@handle)
    end

    it_should_behave_like "a socket"

    it "should raise a TransportException when it cannot open a ssl socket" do
      allow(Addrinfo).to receive(:foreach).and_raise(SocketError.new("lookup failed"))
      expect { @socket.open }.to raise_error(Thrift::TransportException) do |e|
        expect(e.type).to eq(Thrift::TransportException::NOT_OPEN)
        expect(e.message).to eq("Could not connect to localhost:9090")
        expect(e.cause).to be_a(SocketError)
      end
    end

    it "should open a ::Socket with default args" do
      expect(@simple_socket_handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
      expect(OpenSSL::SSL::SSLSocket).to receive(:new).with(@simple_socket_handle, nil).and_return(@handle)
      expect(@handle).to receive(:sync_close=).with(true)
      expect(@handle).to receive(:connect).and_return(true)
      expect(@handle).to receive(:post_connection_check).with('localhost')
      @socket.open
    end

    it "should accept host/port options" do
      handle = double("Handle", :closed? => false)
      allow(handle).to receive(:close)
      expect(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(100.0, 100.0)
      expect(handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
      expect(Addrinfo).to receive(:foreach).with("my.domain", 1234, nil, :STREAM).and_yield(@addrinfo)
      expect(@addrinfo).to receive(:connect).with(timeout: 6000).and_return(handle)
      expect(OpenSSL::SSL::SSLSocket).to receive(:new).with(handle, nil).and_return(@handle)
      expect(@handle).to receive(:sync_close=).with(true)
      expect(@handle).to receive(:connect_nonblock).with(exception: false).and_return(@handle)
      expect(@handle).to receive(:post_connection_check).with('my.domain')
      Thrift::SSLSocket.new('my.domain', 1234, 6000, nil).open
    end

    it "should accept an optional timeout" do
      expect(Thrift::SSLSocket.new('localhost', 8080, 5).timeout).to eq(5)
    end

    it "should accept an optional context" do
      expect(Thrift::SSLSocket.new('localhost', 8080, 5, @context).ssl_context).to eq(@context)
    end

    it "should treat zero timeout as blocking for open and handshake" do
      @socket.timeout = 0

      expect(@addrinfo).to receive(:connect).with(no_args).and_return(@simple_socket_handle)
      expect(@simple_socket_handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
      expect(@handle).to receive(:sync_close=).with(true)
      expect(@handle).to receive(:connect).and_return(true)
      expect(@handle).not_to receive(:connect_nonblock)
      expect(@handle).to receive(:post_connection_check).with('localhost')

      expect(@socket.open).to eq(@handle)
    end

    it "should use the remaining timeout across ssl wait states" do
      @socket.timeout = 5

      expect(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(100.0, 101.0, 102.0, 104.0)
      expect(@addrinfo).to receive(:connect).with(timeout: 4.0).and_return(@simple_socket_handle)
      expect(@simple_socket_handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
      expect(@handle).to receive(:sync_close=).with(true)
      expect(@handle).to receive(:connect_nonblock).with(exception: false).ordered.and_return(:wait_readable)
      expect(@simple_socket_handle).to receive(:wait_readable).with(3.0).ordered.and_return(true)
      expect(@handle).to receive(:connect_nonblock).with(exception: false).ordered.and_return(:wait_writable)
      expect(@simple_socket_handle).to receive(:wait_writable).with(1.0).ordered.and_return(true)
      expect(@handle).to receive(:connect_nonblock).with(exception: false).ordered.and_return(@handle)
      expect(@handle).to receive(:post_connection_check).with('localhost')

      expect(@socket.open).to eq(@handle)
    end

    it "should share one timeout budget across tcp fallback and the ssl handshake" do
      @socket.timeout = 5
      second_addrinfo = double("Addrinfo")

      expect(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(100.0, 101.0, 102.0, 103.0)
      expect(Addrinfo).to receive(:foreach).with("localhost", 9090, nil, :STREAM).and_yield(@addrinfo).and_yield(second_addrinfo)
      expect(@addrinfo).to receive(:connect).with(timeout: 4.0).and_raise(Errno::ECONNREFUSED)
      expect(second_addrinfo).to receive(:connect).with(timeout: 3.0).and_return(@simple_socket_handle)
      expect(@simple_socket_handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
      expect(@handle).to receive(:sync_close=).with(true)
      expect(@handle).to receive(:connect_nonblock).with(exception: false).ordered.and_return(:wait_readable)
      expect(@simple_socket_handle).to receive(:wait_readable).with(2.0).ordered.and_return(true)
      expect(@handle).to receive(:connect_nonblock).with(exception: false).ordered.and_return(@handle)
      expect(@handle).to receive(:post_connection_check).with('localhost')

      expect(@socket.open).to eq(@handle)
    end

    it "should continue to the next address after a tcp connect timeout during ssl open" do
      @socket.timeout = 5
      second_addrinfo = double("Addrinfo")

      expect(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(100.0, 101.0, 103.0)
      expect(Addrinfo).to receive(:foreach).with("localhost", 9090, nil, :STREAM).and_yield(@addrinfo).and_yield(second_addrinfo)
      expect(@addrinfo).to receive(:connect).with(timeout: 4.0).and_raise(Errno::ETIMEDOUT)
      expect(second_addrinfo).to receive(:connect).with(timeout: 2.0).and_return(@simple_socket_handle)
      expect(@simple_socket_handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
      expect(@handle).to receive(:sync_close=).with(true)
      expect(@handle).to receive(:connect_nonblock).with(exception: false).and_return(@handle)
      expect(@handle).to receive(:post_connection_check).with('localhost')

      expect(@socket.open).to eq(@handle)
    end

    it "should raise TIMED_OUT when ssl handshake wait times out" do
      @socket.timeout = 5

      expect(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(100.0, 101.0, 102.0)
      expect(@addrinfo).to receive(:connect).with(timeout: 4.0).and_return(@simple_socket_handle)
      expect(@simple_socket_handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
      expect(@handle).to receive(:sync_close=).with(true)
      expect(@handle).to receive(:connect_nonblock).with(exception: false).and_return(:wait_readable)
      expect(@simple_socket_handle).to receive(:wait_readable).with(3.0).and_return(nil)
      expect(@handle).to receive(:close)

      expect { @socket.open }.to raise_error(Thrift::TransportException) do |e|
        expect(e.type).to eq(Thrift::TransportException::TIMED_OUT)
        expect(e.message).to eq("SSL socket: Timed out establishing session with localhost:9090")
      end
    end

    it "should surface tcp open timeout before starting ssl" do
      @socket.timeout = 5

      expect(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(100.0, 101.0)
      expect(@addrinfo).to receive(:connect).with(timeout: 4.0).and_raise(Errno::ETIMEDOUT)
      expect(OpenSSL::SSL::SSLSocket).not_to receive(:new)

      expect { @socket.open }.to raise_error(Thrift::TransportException) do |e|
        expect(e.type).to eq(Thrift::TransportException::TIMED_OUT)
        expect(e.message).to eq("Socket: Timed out opening connection to localhost:9090")
      end
    end

    it "should close the raw socket if ssl wrapper creation fails" do
      @socket.timeout = 5

      expect(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(100.0, 101.0)
      expect(@addrinfo).to receive(:connect).with(timeout: 4.0).and_return(@simple_socket_handle)
      expect(@simple_socket_handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
      expect(OpenSSL::SSL::SSLSocket).to receive(:new).with(@simple_socket_handle, nil).and_raise(StandardError.new("ssl init failed"))
      expect(@simple_socket_handle).to receive(:close)

      expect { @socket.open }.to raise_error(Thrift::TransportException) do |e|
        expect(e.type).to eq(Thrift::TransportException::NOT_OPEN)
        expect(e.message).to eq("Could not connect to localhost:9090")
        expect(e.cause.message).to eq("ssl init failed")
      end
    end

    it "should close the ssl socket when post connection check fails" do
      @socket.timeout = 5

      expect(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(100.0, 101.0)
      expect(@addrinfo).to receive(:connect).with(timeout: 4.0).and_return(@simple_socket_handle)
      expect(@simple_socket_handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
      expect(@handle).to receive(:sync_close=).with(true)
      expect(@handle).to receive(:connect_nonblock).with(exception: false).and_return(@handle)
      expect(@handle).to receive(:post_connection_check).with('localhost').and_raise(StandardError.new("hostname mismatch"))
      expect(@handle).to receive(:close)

      expect { @socket.open }.to raise_error(Thrift::TransportException) do |e|
        expect(e.type).to eq(Thrift::TransportException::NOT_OPEN)
        expect(e.message).to eq("Could not connect to localhost:9090")
        expect(e.cause.message).to eq("hostname mismatch")
      end
    end

    it "should close the ssl socket on an unexpected nonblocking handshake result" do
      @socket.timeout = 5

      expect(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(100.0, 101.0)
      expect(@addrinfo).to receive(:connect).with(timeout: 4.0).and_return(@simple_socket_handle)
      expect(@simple_socket_handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
      expect(@handle).to receive(:sync_close=).with(true)
      expect(@handle).to receive(:connect_nonblock).with(exception: false).and_return(:unexpected)
      expect(@handle).to receive(:close)

      expect { @socket.open }.to raise_error(Thrift::TransportException) do |e|
        expect(e.type).to eq(Thrift::TransportException::NOT_OPEN)
        expect(e.message).to include("unexpected SSL connect result")
      end
    end

    it "should delegate to_io to the underlying SSL socket handle" do
      @socket.open
      expect(@socket.to_io).to eq(@simple_socket_handle)
    end

    it "should raise IOError when to_io is called on a closed stream" do
      expect { @socket.to_io }.to raise_error(IOError, 'closed stream')
    end

    it "should provide a reasonable to_s" do
      expect(Thrift::SSLSocket.new('myhost', 8090).to_s).to eq("ssl(socket(myhost:8090))")
    end
  end
end
