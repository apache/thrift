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

$:.unshift File.dirname(__FILE__) + '/../lib'
$:.unshift File.dirname(__FILE__) + '/../ext'
require 'thrift'
require 'openssl'
$:.unshift File.dirname(__FILE__) + "/gen-rb"
require 'benchmark_service'

class Client
  def initialize(host, port, clients_per_process, calls_per_client, log_exceptions, tls, protocol_type)
    @host = host
    @port = port
    @clients_per_process = clients_per_process
    @calls_per_client = calls_per_client
    @log_exceptions = log_exceptions
    @tls = tls
    @protocol_type = protocol_type || 'binary'
  end

  def create_protocol(socket)
    case @protocol_type
    when 'binary'
      transport = Thrift::FramedTransport.new(socket)
      Thrift::BinaryProtocol.new(transport)
    when 'compact'
      transport = Thrift::FramedTransport.new(socket)
      Thrift::CompactProtocol.new(transport)
    when 'header'
      Thrift::HeaderProtocol.new(socket)
    when 'header-compact'
      Thrift::HeaderProtocol.new(socket, nil, Thrift::HeaderSubprotocolID::COMPACT)
    when 'header-zlib'
      protocol = Thrift::HeaderProtocol.new(socket)
      protocol.add_transform(Thrift::HeaderTransformID::ZLIB)
      protocol
    else
      transport = Thrift::FramedTransport.new(socket)
      Thrift::BinaryProtocol.new(transport)
    end
  end

  def run
    @clients_per_process.times do
      socket = if @tls
        ssl_context = OpenSSL::SSL::SSLContext.new.tap do |ctx|
          ctx.verify_mode = OpenSSL::SSL::VERIFY_PEER
          ctx.min_version = OpenSSL::SSL::TLS1_2_VERSION

          keys_dir = File.expand_path("../../../test/keys", __dir__)
          ctx.ca_file = File.join(keys_dir, "CA.pem")
          ctx.cert = OpenSSL::X509::Certificate.new(File.open(File.join(keys_dir, "client.crt")))
          ctx.cert_store = OpenSSL::X509::Store.new
          ctx.cert_store.add_file(File.join(keys_dir, 'server.pem'))
          ctx.key = OpenSSL::PKey::RSA.new(File.open(File.join(keys_dir, "client.key")))
        end

        Thrift::SSLSocket.new(@host, @port, 5, ssl_context)
      else
        Thrift::Socket.new(@host, @port, 5)
      end
      protocol = create_protocol(socket)
      transport = protocol.trans
      client = ThriftBenchmark::BenchmarkService::Client.new(protocol)
      begin
        start = Time.now
        transport.open
        Marshal.dump [:start, start], STDOUT
      rescue => e
        Marshal.dump [:connection_failure, Time.now], STDOUT
        print_exception e if @log_exceptions
      else
        begin
          @calls_per_client.times do
            Marshal.dump [:call_start, Time.now], STDOUT
            client.fibonacci(15)
            Marshal.dump [:call_end, Time.now], STDOUT
          end
          transport.close
          Marshal.dump [:end, Time.now], STDOUT
        rescue Thrift::TransportException => e
          Marshal.dump [:connection_error, Time.now], STDOUT
          print_exception e if @log_exceptions
        end
      end
    end
  end

  def print_exception(e)
    STDERR.puts "ERROR: #{e.message}"
    STDERR.puts "\t#{e.backtrace * "\n\t"}"
  end
end

log_exceptions = true if ARGV[0] == '-log-exceptions' and ARGV.shift
tls = true if ARGV[0] == '-tls' and ARGV.shift

host, port, clients_per_process, calls_per_client, protocol_type = ARGV

Client.new(host, port.to_i, clients_per_process.to_i, calls_per_client.to_i, log_exceptions, tls, protocol_type).run
