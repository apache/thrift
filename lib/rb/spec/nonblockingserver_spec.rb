require File.dirname(__FILE__) + '/spec_helper'
require 'thrift/server/nonblockingserver'
$:.unshift File.dirname(__FILE__) + '/gen-rb'
require 'NonblockingService'

class ThriftNonblockingServerSpec < Spec::ExampleGroup
  include Thrift
  include SpecNamespace

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

    def unblock
      @queue.num_waiting.times { @queue.push true }
    end

    def sleep(time)
      Kernel.sleep time
    end

    def shutdown
      @server.shutdown
    end
  end

  before(:each) do
    @port = 43251
    handler = Handler.new
    processor = NonblockingService::Processor.new(handler)
    @transport = ServerSocket.new('localhost', @port)
    transportFactory = FramedTransportFactory.new
    @server = NonblockingServer.new(processor, @transport, transportFactory, nil, 5)
    handler.server = @server
    @server_thread = Thread.new do
      begin
        @server.serve
      rescue => e
        p e
        puts e.backtrace * "\n"
        raise e
      end
    end
    Thread.pass

    @clients = []
  end

  after(:each) do
    @clients.each { |client, trans| trans.close }
    @server_thread.kill
    @transport.close
  end

  def setup_client
    transport = FramedTransport.new(Socket.new('localhost', @port))
    protocol = BinaryProtocol.new(transport)
    client = NonblockingService::Client.new(protocol)
    transport.open
    @clients << [client, transport]
    client
  end

  def setup_client_thread(result)
    queue = Queue.new
    Thread.new do
      client = setup_client
      while (msg = queue.pop)
        case msg
        when :block
          result << client.block
        when :unblock
          client.unblock
        when :hello
          result << client.greeting(true) # ignore result
        when :sleep
          client.sleep(0.5)
          result << :slept
        when :shutdown
          client.shutdown
        when :exit
          result << :done
          break
        end
      end
      @clients.each { |c,t| t.close and break if c == client } #close the transport
    end
    queue
  end

  it "should handle basic message passing" do
    client = setup_client
    client.greeting(true).should == Hello.new
    client.greeting(false).should == Hello.new(:greeting => 'Aloha!')
  end

  it "should handle concurrent clients" do
    queue = Queue.new
    4.times { Thread.new { queue.push setup_client.block } }
    setup_client.unblock
    4.times { queue.pop.should be_true }
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
    3.times { result.pop.should == Hello.new }
    client.greeting(true).should == Hello.new
    queues[5] << :unblock
    4.times { result.pop.should be_true }
    queues[2] << :hello
    result.pop.should == Hello.new
    client.greeting(false).should == Hello.new(:greeting => 'Aloha!')
    7.times { queues.shift << :exit }
    client.greeting(true).should == Hello.new
  end

  it "should shut down when asked" do
    @server.shutdown
    @server_thread.join(2).should be_an_instance_of(Thread)
  end

  it "should continue processing active messages when shutting down" do
    result = Queue.new
    client = setup_client_thread(result)
    client << :sleep
    sleep 0.1 # give the server time to start processing the client's message
    @server.shutdown
    @server_thread.join(2).should be_an_instance_of(Thread)
    result.pop.should == :slept
  end

  it "should kill active messages when they don't expire while shutting down" do
    result = Queue.new
    client = setup_client_thread(result)
    client << :block
    sleep 0.1 # start processing the client's message
    @server.shutdown(1, true)
    @server_thread.join(3).should_not be_nil
  end
end
