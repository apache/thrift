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

module Server
  include Thrift

  class BenchmarkHandler
    # 1-based index into the fibonacci sequence
    def fibonacci(n)
      seq = [1, 1]
      3.upto(n) do
        seq << seq[-1] + seq[-2]
      end
      seq[n-1] # n is 1-based
    end
  end

  def self.start_server(host, port, serverClass, tls)
    handler = BenchmarkHandler.new
    processor = ThriftBenchmark::BenchmarkService::Processor.new(handler)
    transport = if tls
      ssl_context = OpenSSL::SSL::SSLContext.new.tap do |ctx|
        ctx.verify_mode = OpenSSL::SSL::VERIFY_PEER
        ctx.min_version = OpenSSL::SSL::TLS1_2_VERSION

        keys_dir = File.expand_path("../../../test/keys", __dir__)
        ctx.ca_file = File.join(keys_dir, "CA.pem")
        ctx.cert = OpenSSL::X509::Certificate.new(File.open(File.join(keys_dir, "server.crt")))
        ctx.cert_store = OpenSSL::X509::Store.new
        ctx.cert_store.add_file(File.join(keys_dir, 'client.pem'))
        ctx.key = OpenSSL::PKey::RSA.new(File.open(File.join(keys_dir, "server.key")))
      end

      Thrift::SSLServerSocket.new(host, port, ssl_context)
    else
      ServerSocket.new(host, port)
    end
    transport_factory = FramedTransportFactory.new
    args = [processor, transport, transport_factory, nil, 20]
    if serverClass == NonblockingServer
      logger = Logger.new(STDERR)
      logger.level = Logger::WARN
      args << logger
    end
    server = serverClass.new(*args)
    @server_thread = Thread.new do
      server.serve
    end
    @server = server
  end

  def self.shutdown
    return if @server.nil?
    if @server.respond_to? :shutdown
      @server.shutdown
    else
      @server_thread.kill
    end
  end
end

def resolve_const(const)
  const and const.split('::').inject(Object) { |k,c| k.const_get(c) }
end

tls = true if ARGV[0] == '-tls' and ARGV.shift

host, port, serverklass = ARGV

Server.start_server(host, port.to_i, resolve_const(serverklass), tls)

# let our host know that the interpreter has started
# ideally we'd wait until the server was serving, but we don't have a hook for that
Marshal.dump(:started, STDOUT)
STDOUT.flush

Marshal.load(STDIN) # wait until we're instructed to shut down

Server.shutdown
