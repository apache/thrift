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
require 'timeout'

describe 'NonblockingServer' do

  class Handler
    def initialize
      @queue = Queue.new
    end

    attr_accessor :server

    def greeting(english)
      if english
        SpecNamespace::Hello.new
      else
        SpecNamespace::Hello.new(:greeting => "Aloha!")
      end
    end

    def block
      @queue.pop
    end

    def unblock(n)
      n.times { @queue.push true }
    end

    def sleep(time)
      Kernel.sleep time
    end

    def shutdown
      @server.shutdown(0, false)
    end
  end

  class SpecTransport < Thrift::BaseTransport
    def initialize(transport, queue)
      @transport = transport
      @queue = queue
      @flushed = false
    end

    def open?
      @transport.open?
    end

    def open
      @transport.open
    end

    def close
      @transport.close
    end

    def read(sz)
      @transport.read(sz)
    end

    def write(buf,sz=nil)
      @transport.write(buf, sz)
    end

    def flush
      @queue.push :flushed unless @flushed or @queue.nil?
      @flushed = true
      @transport.flush
    end
  end

  class SpecServerSocket < Thrift::ServerSocket
    def initialize(host, port, queue)
      super(host, port)
      @queue = queue
    end

    def listen
      super
      @queue.push :listen
    end
  end

  describe Thrift::NonblockingServer do
    before(:each) do
      @port = 43251
      handler = Handler.new
      processor = SpecNamespace::NonblockingService::Processor.new(handler)
      queue = Queue.new
      @transport = SpecServerSocket.new('localhost', @port, queue)
      transport_factory = Thrift::FramedTransportFactory.new
      logger = Logger.new(STDERR)
      logger.level = Logger::WARN
      @server = Thrift::NonblockingServer.new(processor, @transport, transport_factory, nil, 5, logger)
      handler.server = @server
      @server_thread = Thread.new(Thread.current) do |master_thread|
        begin
          @server.serve
        rescue => e
          p e
          puts e.backtrace * "\n"
          master_thread.raise e
        end
      end
      queue.pop

      @clients = []
      @catch_exceptions = false
    end

    after(:each) do
      @clients.each { |client, trans| trans.close }
      # @server.shutdown(1)
      @server_thread.kill
      @transport.close
    end

    def setup_client(queue = nil)
      transport = SpecTransport.new(Thrift::FramedTransport.new(Thrift::Socket.new('localhost', @port)), queue)
      protocol = Thrift::BinaryProtocol.new(transport)
      client = SpecNamespace::NonblockingService::Client.new(protocol)
      transport.open
      @clients << [client, transport]
      client
    end

    def setup_client_thread(result)
      queue = Queue.new
      Thread.new do
        begin
          client = setup_client
          while (cmd = queue.pop)
            msg, *args = cmd
            case msg
            when :block
              result << client.block
            when :unblock
              client.unblock(args.first)
            when :hello
              result << client.greeting(true) # ignore result
            when :sleep
              client.sleep(args[0] || 0.5)
              result << :slept
            when :shutdown
              client.shutdown
            when :exit
              result << :done
              break
            end
          end
          @clients.each { |c,t| t.close and break if c == client } #close the transport
        rescue => e
          raise e unless @catch_exceptions
        end
      end
      queue
    end

    it "should handle basic message passing" do
      client = setup_client
      expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)
      expect(client.greeting(false)).to eq(SpecNamespace::Hello.new(:greeting => 'Aloha!'))
      @server.shutdown
    end

    it "should handle concurrent clients" do
      queue = Queue.new
      trans_queue = Queue.new
      4.times do
        Thread.new(Thread.current) do |main_thread|
          begin
            queue.push setup_client(trans_queue).block
          rescue => e
            main_thread.raise e
          end
        end
      end
      4.times { trans_queue.pop }
      setup_client.unblock(4)
      4.times { expect(queue.pop).to be_truthy }
      @server.shutdown
    end

    it "should handle messages from more than 5 long-lived connections" do
      queues = []
      result = Queue.new
      7.times do |i|
        queues << setup_client_thread(result)
        Thread.pass if i == 4 # give the server time to accept connections
      end
      client = setup_client
      # block 4 connections
      4.times { |i| queues[i] << :block }
      queues[4] << :hello
      queues[5] << :hello
      queues[6] << :hello
      3.times { expect(result.pop).to eq(SpecNamespace::Hello.new) }
      expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)
      queues[5] << [:unblock, 4]
      4.times { expect(result.pop).to be_truthy }
      queues[2] << :hello
      expect(result.pop).to eq(SpecNamespace::Hello.new)
      expect(client.greeting(false)).to eq(SpecNamespace::Hello.new(:greeting => 'Aloha!'))
      7.times { queues.shift << :exit }
      expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)
      @server.shutdown
    end

    it "should shut down when asked" do
      # connect first to ensure it's running
      client = setup_client
      client.greeting(false) # force a message pass
      @server.shutdown
      expect(@server_thread.join(2)).to be_an_instance_of(Thread)
    end

    it "should continue processing active messages when shutting down" do
      result = Queue.new
      client = setup_client_thread(result)
      client << :sleep
      sleep 0.1 # give the server time to start processing the client's message
      @server.shutdown
      expect(@server_thread.join(2)).to be_an_instance_of(Thread)
      expect(result.pop).to eq(:slept)
    end

    it "should kill active messages when they don't expire while shutting down" do
      result = Queue.new
      client = setup_client_thread(result)
      client << [:sleep, 10.0]
      sleep 0.1 # start processing the client's message
      @server.shutdown(1)
      @catch_exceptions = true
      expect(@server_thread.join(3)).not_to be_nil
      expect(result).to be_empty
    end

    it "should allow shutting down in response to a message" do
      client = setup_client
      expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)
      client.shutdown
      expect(@server_thread.join(2)).not_to be_nil
    end
  end

  describe "#{Thrift::NonblockingServer} with TLS transport" do
    before(:each) do
      @port = available_port
      handler = Handler.new
      processor = SpecNamespace::NonblockingService::Processor.new(handler)
      @transport = Thrift::SSLServerSocket.new('localhost', @port, create_server_ssl_context)
      transport_factory = Thrift::FramedTransportFactory.new
      logger = Logger.new(STDERR)
      logger.level = Logger::WARN
      @server = Thrift::NonblockingServer.new(processor, @transport, transport_factory, nil, 5, logger)
      handler.server = @server

      @server_thread = Thread.new(Thread.current) do |master_thread|
        begin
          @server.serve
        rescue => e
          master_thread.raise e
        end
      end

      @clients = []
      wait_until_listening
    end

    after(:each) do
      @clients.each(&:close)
      @server.shutdown if @server
      @server_thread.join(2) if @server_thread
      @transport.close if @transport
    end

    it "should handle requests over TLS" do
      expect(@server_thread).to be_alive

      client = setup_tls_client
      expect(client.greeting(true)).to eq(SpecNamespace::Hello.new)

      @server.shutdown
      expect(@server_thread.join(2)).to be_an_instance_of(Thread)
    end

    def setup_tls_client
      transport = Thrift::FramedTransport.new(
        Thrift::SSLSocket.new('localhost', @port, nil, create_client_ssl_context)
      )
      protocol = Thrift::BinaryProtocol.new(transport)
      client = SpecNamespace::NonblockingService::Client.new(protocol)
      transport.open
      @clients << transport
      client
    end

    def wait_until_listening
      Timeout.timeout(2) do
        until @transport.handle
          raise "Server thread exited unexpectedly" unless @server_thread.alive?
          sleep 0.01
        end
      end
    end

    def available_port
      TCPServer.open('localhost', 0) { |server| server.addr[1] }
    end

    def ssl_keys_dir
      File.expand_path('../../../test/keys', __dir__)
    end

    def create_server_ssl_context
      OpenSSL::SSL::SSLContext.new.tap do |ctx|
        ctx.verify_mode = OpenSSL::SSL::VERIFY_PEER
        if ctx.respond_to?(:min_version=) && OpenSSL::SSL.const_defined?(:TLS1_2_VERSION)
          ctx.min_version = OpenSSL::SSL::TLS1_2_VERSION
        end
        ctx.ca_file = File.join(ssl_keys_dir, 'CA.pem')
        ctx.cert = OpenSSL::X509::Certificate.new(File.read(File.join(ssl_keys_dir, 'server.crt')))
        ctx.cert_store = OpenSSL::X509::Store.new
        ctx.cert_store.add_file(File.join(ssl_keys_dir, 'client.pem'))
        ctx.key = OpenSSL::PKey::RSA.new(File.read(File.join(ssl_keys_dir, 'server.key')))
      end
    end

    def create_client_ssl_context
      OpenSSL::SSL::SSLContext.new.tap do |ctx|
        ctx.verify_mode = OpenSSL::SSL::VERIFY_PEER
        if ctx.respond_to?(:min_version=) && OpenSSL::SSL.const_defined?(:TLS1_2_VERSION)
          ctx.min_version = OpenSSL::SSL::TLS1_2_VERSION
        end
        ctx.ca_file = File.join(ssl_keys_dir, 'CA.pem')
        ctx.cert = OpenSSL::X509::Certificate.new(File.read(File.join(ssl_keys_dir, 'client.crt')))
        ctx.cert_store = OpenSSL::X509::Store.new
        ctx.cert_store.add_file(File.join(ssl_keys_dir, 'server.pem'))
        ctx.key = OpenSSL::PKey::RSA.new(File.read(File.join(ssl_keys_dir, 'client.key')))
      end
    end
  end
end
