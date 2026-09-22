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

module Thrift
  class SimpleServer < BaseServer
    def serve
      @server_transport.listen
      loop do
        begin
          client = @server_transport.accept
        rescue Errno::ECONNRESET, Errno::EPIPE
          next
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
          # The client hung up or sent a message this connection could not
          # read. Close it below and keep accepting other clients.
        rescue => e
          # Any other error raised while serving this one client must not end
          # the accept loop. Log it, close the connection, and keep serving.
          @logger.error("Error while serving a client connection: #{e.inspect}")
        ensure
          trans.close
        end
      end
    ensure
      @server_transport.close
    end

    def to_s
      "simple(#{super})"
    end
  end
end
