# Copyright (c) 2006- Facebook
# Distributed under the Apache Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/
#
# Author: Mark Slee <mcslee@facebook.com>
#
require 'thrift/protocol'
require 'thrift/protocol/binaryprotocol'
require 'thrift/transport'

module Thrift
  class Server
    def initialize(processor, serverTransport, transportFactory=nil, protocolFactory=nil)
      @processor = processor
      @serverTransport = serverTransport
      @transportFactory = transportFactory ? transportFactory : Thrift::TransportFactory.new
      @protocolFactory = protocolFactory ? protocolFactory : Thrift::BinaryProtocolFactory.new
    end

    def serve; nil; end
  end
  deprecate_class! :TServer => Server

  class SimpleServer < Server
    def serve
      begin
        @serverTransport.listen
        loop do
          client = @serverTransport.accept
          trans = @transportFactory.get_transport(client)
          prot = @protocolFactory.get_protocol(trans)
          begin
            loop do
              @processor.process(prot, prot)
            end
          rescue Thrift::TransportException, Thrift::ProtocolException
          ensure
            trans.close
          end
        end
      ensure
        @serverTransport.close
      end
    end
  end
  deprecate_class! :TSimpleServer => SimpleServer
end

# do *not* use fastthread
# it has a bug that triggers on NonblockingServer
require 'thread'

module Thrift
  class ThreadedServer < Server
    def serve
      begin
        @serverTransport.listen
        loop do
          client = @serverTransport.accept
          trans = @transportFactory.get_transport(client)
          prot = @protocolFactory.get_protocol(trans)
          Thread.new(prot, trans) do |p, t|
            begin
              loop do
                @processor.process(p, p)
              end
            rescue Thrift::TransportException, Thrift::ProtocolException
            ensure
              t.close
            end
          end
        end
      ensure
        @serverTransport.close
      end
    end
  end
  deprecate_class! :TThreadedServer => ThreadedServer

  class ThreadPoolServer < Server
    def initialize(processor, serverTransport, transportFactory=nil, protocolFactory=nil, num=20)
      super(processor, serverTransport, transportFactory, protocolFactory)
      @thread_q = SizedQueue.new(num)
      @exception_q = Queue.new
      @running = false
    end

    ## exceptions that happen in worker threads will be relayed here and
    ## must be caught. 'retry' can be used to continue. (threads will
    ## continue to run while the exception is being handled.)
    def rescuable_serve
      Thread.new { serve } unless @running
      @running = true
      raise @exception_q.pop
    end

    ## exceptions that happen in worker threads simply cause that thread
    ## to die and another to be spawned in its place.
    def serve
      @serverTransport.listen

      begin
        loop do
          @thread_q.push(:token)
          Thread.new do
            begin
              loop do
                client = @serverTransport.accept
                trans = @transportFactory.get_transport(client)
                prot = @protocolFactory.get_protocol(trans)
                begin
                  loop do
                    @processor.process(prot, prot)
                  end
                rescue Thrift::TransportException, Thrift::ProtocolException => e
                ensure
                  trans.close
                end
              end
            rescue => e
              @exception_q.push(e)
            ensure
              @thread_q.pop # thread died!
            end
          end
        end
      ensure
        @serverTransport.close
      end
    end
  end
  deprecate_class! :TThreadPoolServer => ThreadPoolServer
end
