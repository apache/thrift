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

describe 'Socket' do
  describe Thrift::Socket do
    before(:each) do
      @socket = Thrift::Socket.new
      @addrinfo = double("Addrinfo")
      @handle = double("Handle", :closed? => false)
      allow(@handle).to receive(:close)
      allow(@handle).to receive(:setsockopt)
      allow(@addrinfo).to receive(:connect).and_return(@handle)
      allow(Addrinfo).to receive(:foreach).and_yield(@addrinfo)
    end

    it_should_behave_like "a socket"

    it "should raise a TransportException when it cannot open a socket" do
      allow(Addrinfo).to receive(:foreach).and_raise(SocketError.new("lookup failed"))
      expect { @socket.open }.to raise_error(Thrift::TransportException) do |e|
        expect(e.type).to eq(Thrift::TransportException::NOT_OPEN)
        expect(e.message).to eq("Could not connect to localhost:9090")
        expect(e.cause).to be_a(SocketError)
      end
    end

    it "should open a ::Socket with default args" do
      expect(Addrinfo).to receive(:foreach).with("localhost", 9090, nil, :STREAM).and_yield(@addrinfo)
      expect(@addrinfo).to receive(:connect).with(no_args).and_return(@handle)
      expect(@handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
      @socket.to_s == "socket(localhost:9090)"
      @socket.open
    end

    it "should accept host/port options" do
      handle = double("Handle", :closed? => false)
      allow(handle).to receive(:close)
      expect(handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)
      expect(Addrinfo).to receive(:foreach).with("my.domain", 1234, nil, :STREAM).and_yield(@addrinfo)
      expect(@addrinfo).to receive(:connect).with(no_args).and_return(handle)
      @socket = Thrift::Socket.new('my.domain', 1234).open
      @socket.to_s == "socket(my.domain:1234)"
    end

    it "should accept an optional timeout" do
      expect(Thrift::Socket.new('localhost', 8080, 5).timeout).to eq(5)
    end

    it "should provide a reasonable to_s" do
      expect(Thrift::Socket.new('myhost', 8090).to_s).to eq("socket(myhost:8090)")
    end

    it "should pass the remaining timeout to each address attempt" do
      @socket.timeout = 5
      second_addrinfo = double("Addrinfo")

      expect(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(100.0, 101.0, 103.0)
      expect(Addrinfo).to receive(:foreach).with("localhost", 9090, nil, :STREAM).and_yield(@addrinfo).and_yield(second_addrinfo)
      expect(@addrinfo).to receive(:connect).with(timeout: 4.0).and_raise(Errno::ECONNREFUSED)
      expect(second_addrinfo).to receive(:connect).with(timeout: 2.0).and_return(@handle)
      expect(@handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)

      expect(@socket.open).to eq(@handle)
    end

    it "should continue to the next address after a connect timeout while time remains" do
      @socket.timeout = 5
      second_addrinfo = double("Addrinfo")

      expect(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(100.0, 101.0, 103.0)
      expect(Addrinfo).to receive(:foreach).with("localhost", 9090, nil, :STREAM).and_yield(@addrinfo).and_yield(second_addrinfo)
      expect(@addrinfo).to receive(:connect).with(timeout: 4.0).and_raise(Errno::ETIMEDOUT)
      expect(second_addrinfo).to receive(:connect).with(timeout: 2.0).and_return(@handle)
      expect(@handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)

      expect(@socket.open).to eq(@handle)
    end

    it "should raise TIMED_OUT when an address attempt times out" do
      @socket.timeout = 5

      expect(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(100.0, 101.0)
      expect(@addrinfo).to receive(:connect).with(timeout: 4.0).and_raise(Errno::ETIMEDOUT)

      expect { @socket.open }.to raise_error(Thrift::TransportException) do |e|
        expect(e.type).to eq(Thrift::TransportException::TIMED_OUT)
        expect(e.message).to eq("Socket: Timed out opening connection to localhost:9090")
      end
    end

    it "should raise TIMED_OUT when the deadline expires before the next address attempt" do
      @socket.timeout = 5
      second_addrinfo = double("Addrinfo")

      expect(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(100.0, 101.0, 105.0)
      expect(Addrinfo).to receive(:foreach).with("localhost", 9090, nil, :STREAM).and_yield(@addrinfo).and_yield(second_addrinfo)
      expect(@addrinfo).to receive(:connect).with(timeout: 4.0).and_raise(Errno::ECONNREFUSED)
      expect(second_addrinfo).not_to receive(:connect)

      expect { @socket.open }.to raise_error(Thrift::TransportException) do |e|
        expect(e.type).to eq(Thrift::TransportException::TIMED_OUT)
        expect(e.message).to eq("Socket: Timed out opening connection to localhost:9090")
      end
    end

    it "should treat zero timeout as blocking for open" do
      @socket.timeout = 0

      expect(@addrinfo).to receive(:connect).with(no_args).and_return(@handle)
      expect(@handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)

      expect(@socket.open).to eq(@handle)
    end

    it "should report the last connection error when all addresses fail" do
      second_addrinfo = double("Addrinfo")

      expect(Addrinfo).to receive(:foreach).with("localhost", 9090, nil, :STREAM).and_yield(@addrinfo).and_yield(second_addrinfo)
      expect(@addrinfo).to receive(:connect).and_raise(Errno::ECONNREFUSED)
      expect(second_addrinfo).to receive(:connect).and_raise(Errno::EHOSTUNREACH)

      expect { @socket.open }.to raise_error(Thrift::TransportException) do |e|
        expect(e.type).to eq(Thrift::TransportException::NOT_OPEN)
        expect(e.message).to eq("Could not connect to localhost:9090")
        expect(e.cause).to be_a(Errno::EHOSTUNREACH)
      end
    end

    it "should close a connected candidate before falling back when socket setup fails" do
      first_addrinfo = @addrinfo
      second_addrinfo = double("Addrinfo")
      first_handle = double("Handle", :closed? => false)
      allow(first_handle).to receive(:close)

      expect(Addrinfo).to receive(:foreach).with("localhost", 9090, nil, :STREAM).and_yield(first_addrinfo).and_yield(second_addrinfo)
      expect(first_addrinfo).to receive(:connect).with(no_args).and_return(first_handle)
      expect(first_handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1).and_raise(StandardError.new("setsockopt failed"))
      expect(first_handle).to receive(:close)
      expect(second_addrinfo).to receive(:connect).with(no_args).and_return(@handle)
      expect(@handle).to receive(:setsockopt).with(::Socket::IPPROTO_TCP, ::Socket::TCP_NODELAY, 1)

      expect(@socket.open).to eq(@handle)
    end

    it "should avoid a blank error when no addresses are returned" do
      allow(Addrinfo).to receive(:foreach)

      expect { @socket.open }.to raise_error(Thrift::TransportException) do |e|
        expect(e.type).to eq(Thrift::TransportException::NOT_OPEN)
        expect(e.message).to eq("Could not connect to localhost:9090")
      end
    end
  end
end
