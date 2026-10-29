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

require "thread"

module Thrift
  class ThreadPoolServer < BaseServer
    def initialize(processor, server_transport, transport_factory = nil, protocol_factory = nil, num = 20, logger: nil)
      super(processor, server_transport, transport_factory, protocol_factory, logger: logger)
      @thread_q = SizedQueue.new(num)
      @exception_q = Queue.new
      @running = false
    end

    ## exceptions that happen in worker threads will be relayed here and
    ## must be caught. 'retry' can be used to continue. (threads will
    ## continue to run while the exception is being handled.)
    def rescuable_serve
      unless @running
        # Set before the server starts, so that its workers relay every exception.
        @running = true
        Thread.new do
          Thread.current.name = "thrift.server"
          serve
        end
      end
      raise @exception_q.pop
    end

    ## an error while serving one client is logged and closes only that
    ## client's connection. any other exception in a worker thread is logged
    ## and causes that thread to die and another to be spawned in its place.
    ## (under rescuable_serve, such exceptions are relayed instead of logged.)
    def serve
      @server_transport.listen
      next_worker_id = 0

      begin
        loop do
          @thread_q.push(:token)
          Thread.new(next_worker_id) do |worker_id|
            Thread.current.name = "thrift.pool.#{worker_id}"
            loop do
              begin
                client = @server_transport.accept
              rescue => e
                next if defined?(OpenSSL::SSL::SSLError) && e.is_a?(OpenSSL::SSL::SSLError)
                raise
              end
              trans = @transport_factory.get_transport(client)
              prot = @protocol_factory.get_protocol(trans)
              begin
                loop do
                  @processor.process(prot, prot)
                end
              rescue Thrift::TransportException, Thrift::ProtocolException
              rescue => e
                # rescuable_serve relays the exception to its caller.
                raise if @running

                # Under serve, as in SimpleServer: log the error, close this
                # connection, and keep accepting clients.
                @logger.error("Error while serving a client connection: #{e.inspect}")
              ensure
                trans.close
              end
            end
          rescue => e
            # Only rescuable_serve reads the queue; under serve it would only grow.
            if @running
              @exception_q.push(e)
            else
              @logger.error("Error in thread pool worker: #{e.inspect}")
            end
          ensure
            @thread_q.pop # thread died!
          end
          next_worker_id += 1
        end
      ensure
        @server_transport.close
      end
    end

    def to_s
      "threadpool(#{super})"
    end
  end
end
