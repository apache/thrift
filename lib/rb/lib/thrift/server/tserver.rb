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
    @serverTransport.listen()
    while (true)
      client = @serverTransport.accept()
      trans = @transportFactory.getTransport(client)
      prot = @protocolFactory.getProtocol(trans)
      begin
        while (true)
          @processor.process(prot, prot)
        end
      rescue TTransportException, TProtocolException => ttx
        #print ttx,"\n"
      ensure
        trans.close()
      end
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
          rescue TTransportException, TProtocolException => e
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
    @q = SizedQueue.new(num)
  end

  def serve()
    @serverTransport.listen()

    begin
      while (true)
        @q.push(:token)
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
              rescue TTransportException, TProtocolException => e
              ensure
                trans.close()
              end
            end
          ensure
            @q.pop() # thread died!
          end
        end
      end
    ensure
      @serverTransport.close()
    end
  end
end

