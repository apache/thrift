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

    it "should delegate to_io to the underlying TCP server handle" do
      tcp_server = double("TCPServer")
      ssl_server = double("SSLServer")

      allow(TCPServer).to receive(:new).with(nil, 1234).and_return(tcp_server)
      expect(OpenSSL::SSL::SSLServer).to receive(:new).with(tcp_server, nil).and_return(ssl_server)
      expect(ssl_server).to receive(:start_immediately=).with(false)
      allow(ssl_server).to receive(:to_io).and_return(tcp_server)

      expect(@socket.listen).to eq(ssl_server)
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
      sock = double("TCPSocket")
      ssl_sock = double("SSLSocket")

      allow(TCPServer).to receive(:new).with(nil, 1234).and_return(tcp_server)
      expect(OpenSSL::SSL::SSLServer).to receive(:new).with(tcp_server, nil).and_return(ssl_server)
      expect(ssl_server).to receive(:start_immediately=).with(false)
      expect(ssl_server).to receive(:accept).and_return(ssl_sock)
      allow(Process).to receive(:clock_gettime).with(Process::CLOCK_MONOTONIC).and_return(10)
      expect(sock).to receive(:setsockopt).with(Socket::IPPROTO_TCP, Socket::TCP_NODELAY, 1)
      expect(ssl_sock).to receive(:to_io).and_return(sock)
      expect(ssl_sock).to receive(:accept_nonblock).with(exception: false).and_return(ssl_sock)

      trans = double("Socket")
      expect(Thrift::Socket).to receive(:new).and_return(trans)
      expect(trans).to receive(:timeout=).with(Thrift::BaseServerTransport::DEFAULT_CLIENT_TIMEOUT)
      expect(trans).to receive(:handle=).with(ssl_sock)

      @socket.listen
      expect(@socket.accept).to eq(trans)
    end

    it "should provide a reasonable to_s" do
      expect(@socket.to_s).to eq("ssl(socket(:1234))")
    end

    it "times out and closes a TCP client that sends no TLS handshake" do
      server = build_server(client_timeout: 0.05)
      client = TCPSocket.new('127.0.0.1', server.to_io.local_address.ip_port)
      accept_thread = capture_accept(server)

      expect(accept_thread.join(1)).not_to be_nil
      status, error = accept_thread.value
      expect(status).to eq(:error)
      expect(error).to be_a(OpenSSL::SSL::SSLError)
      expect(error.message).to eq("SSL server socket: Timed out accepting TLS connection")
      expect { client.read_nonblock(1) }.to raise_error(EOFError)
    ensure
      client&.close
      server&.close
      accept_thread&.kill
      accept_thread&.join
    end

    it "uses the same timeout for a stalled partial TLS handshake" do
      server = build_server(client_timeout: 0.05)
      client = TCPSocket.new('127.0.0.1', server.to_io.local_address.ip_port)
      client.write("\x16\x03\x01\x00".b)
      accept_thread = capture_accept(server)

      expect(accept_thread.join(1)).not_to be_nil
      status, error = accept_thread.value
      expect(status).to eq(:error)
      expect(error).to be_a(OpenSSL::SSL::SSLError)
      expect(error.message).to eq("SSL server socket: Timed out accepting TLS connection")
      expect { client.read_nonblock(1) }.to raise_error(EOFError)
    ensure
      client&.close
      server&.close
      accept_thread&.kill
      accept_thread&.join
    end

    it "accepts a completed TLS handshake and preserves the client timeout" do
      server = build_server(client_timeout: 1)
      accept_thread = capture_accept(server)
      tcp_client = TCPSocket.new('127.0.0.1', server.to_io.local_address.ip_port)
      ssl_client = OpenSSL::SSL::SSLSocket.new(tcp_client, OpenSSL::SSL::SSLContext.new)
      ssl_client.sync_close = true

      ssl_client.connect
      expect(accept_thread.join(1)).not_to be_nil
      status, transport = accept_thread.value
      expect(status).to eq(:ok)
      expect(transport).to be_open
      expect(transport.timeout).to eq(1)
    ensure
      transport&.close
      ssl_client&.close
      tcp_client&.close
      server&.close
      accept_thread&.kill
      accept_thread&.join
    end

    it "supports resumed client-authenticated TLS sessions" do
      context = server_context
      context.verify_mode = OpenSSL::SSL::VERIFY_PEER | OpenSSL::SSL::VERIFY_FAIL_IF_NO_PEER_CERT
      # The legacy client certificate fixture is self-issued; this test exercises
      # authenticated session resumption rather than certificate-chain validation.
      context.verify_callback = proc { true }
      context.min_version = OpenSSL::SSL::TLS1_2_VERSION
      context.max_version = OpenSSL::SSL::TLS1_2_VERSION
      context.session_cache_mode = OpenSSL::SSL::SSLContext::SESSION_CACHE_SERVER
      server = build_server(client_timeout: 1, context: context)
      results = Queue.new
      accept_thread = Thread.new do
        2.times do
          results << [:ok, server.accept]
        rescue => error
          results << [:error, error]
          break
        end
      end

      client_context = OpenSSL::SSL::SSLContext.new
      client_context.verify_mode = OpenSSL::SSL::VERIFY_NONE
      client_context.cert = OpenSSL::X509::Certificate.new(File.read(File.join(ssl_keys_dir, 'client.crt')))
      client_context.key = OpenSSL::PKey::RSA.new(File.read(File.join(ssl_keys_dir, 'client.key')))
      client_context.min_version = OpenSSL::SSL::TLS1_2_VERSION
      client_context.max_version = OpenSSL::SSL::TLS1_2_VERSION
      client_context.session_cache_mode = OpenSSL::SSL::SSLContext::SESSION_CACHE_CLIENT

      first_client = build_ssl_client(server, client_context)
      first_client.connect
      status, first_transport = results.pop
      expect(status).to eq(:ok)
      session = first_client.session
      first_client.close
      first_transport.close

      second_client = build_ssl_client(server, client_context, session)
      client_error = begin
        second_client.connect
        nil
      rescue => error
        error
      end
      status, second_transport = results.pop

      expect(status).to eq(:ok), "server accept failed: #{second_transport.inspect}"
      expect(client_error).to be_nil
      expect(second_client.session_reused?).to be(true)
    ensure
      first_client&.close
      first_transport&.close
      second_client&.close
      second_transport&.close if status == :ok
      server&.close
      accept_thread&.kill
      accept_thread&.join
    end

    def build_server(client_timeout:, context: server_context)
      Thrift::SSLServerSocket.new('127.0.0.1', 0, context, client_timeout: client_timeout).tap(&:listen)
    end

    def server_context
      context = OpenSSL::SSL::SSLContext.new
      context.cert = OpenSSL::X509::Certificate.new(File.read(File.join(ssl_keys_dir, 'server.crt')))
      context.key = OpenSSL::PKey::RSA.new(File.read(File.join(ssl_keys_dir, 'server.key')))
      context
    end

    def build_ssl_client(server, context, session = nil)
      tcp_client = TCPSocket.new('127.0.0.1', server.to_io.local_address.ip_port)
      OpenSSL::SSL::SSLSocket.new(tcp_client, context).tap do |ssl_client|
        ssl_client.sync_close = true
        ssl_client.session = session unless session.nil?
      end
    end

    def capture_accept(server)
      Thread.new do
        [:ok, server.accept]
      rescue => error
        [:error, error]
      end
    end

    def ssl_keys_dir
      File.expand_path('../../../test/keys', __dir__)
    end
  end
end
