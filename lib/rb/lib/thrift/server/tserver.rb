#!/usr/bin/env ruby
#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/
#
# Author: Mark Slee <mcslee@facebook.com>
#
require('thrift/protocol/tprotocol')
require('thrift/protocol/tbinaryprotocol')
require('thrift/transport/ttransport')

class TServer

  def initialize(processor, serverTransport, transportFactory=nil, protocolFactory=nil)
    @processor = processor
    @serverTransport = serverTransport
    @transportFactory = transportFactory ? transportFactory : TTransportFactory.new()
    @protocolFactory = protocolFactory ? protocolFactory : TBinaryProtocolFactory.new()
  end

  def serve(); nil; end

end

class TSimpleServer < TServer

  def initialize(processor, serverTransport, transportFactory=nil, protocolFactory=nil)
    super(processor, serverTransport, transportFactory, protocolFactory)
  end

  def serve()
    begin
      @serverTransport.listen()
      while (true)
        client = @serverTransport.accept()
        trans = @transportFactory.getTransport(client)
        prot = @protocolFactory.getProtocol(trans)
        begin
          while (true)
            @processor.process(prot, prot)
          end
        rescue TTransportException, Thrift::ProtocolException => ttx
          #print ttx,"\n"
        ensure
          trans.close()
        end
      end
    ensure
      @serverTransport.close()
    end
  end
end

begin
  require 'fastthread'
rescue LoadError
  require 'thread'
end

class TThreadedServer < TServer
  def serve()
    begin
      @serverTransport.listen()
      while (true)
        client = @serverTransport.accept()
        trans = @transportFactory.getTransport(client)
        prot = @protocolFactory.getProtocol(trans)
        Thread.new(prot, trans) do |p, t|
          begin
            while (true)
              @processor.process(p, p)
            end
          rescue TTransportException, Thrift::ProtocolException => e
          ensure
            t.close()
          end
        end
      end
    ensure
      @serverTransport.close()
    end
  end
end

class TThreadPoolServer < TServer
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
    raise @exception_q.pop
  end

  ## exceptions that happen in worker threads simply cause that thread
  ## to die and another to be spawned in its place.
  def serve
    @serverTransport.listen()

    begin
      while (true)
        @thread_q.push(:token)
        Thread.new do
          begin
            while (true)
              client = @serverTransport.accept()
              trans = @transportFactory.getTransport(client)
              prot = @protocolFactory.getProtocol(trans)
              begin
                while (true)
                  @processor.process(prot, prot)
                end
              rescue TTransportException, Thrift::ProtocolException => e
              ensure
                trans.close()
              end
            end
          rescue Exception => e
            @exception_q.push(e)
          ensure
            @thread_q.pop() # thread died!
          end
        end
      end
    ensure
      @serverTransport.close()
    end
  end
end

