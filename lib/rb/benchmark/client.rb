$:.unshift File.dirname(__FILE__) + '/../lib'
require 'thrift'
require 'thrift/server/nonblockingserver'
$:.unshift File.dirname(__FILE__) + "/gen-rb"
require 'BenchmarkService'

class Client
  def initialize(host, port, clients_per_process, calls_per_client)
    @host = host
    @port = port
    @clients_per_process = clients_per_process
    @calls_per_client = calls_per_client
  end

  def run
    @clients_per_process.times do
      socket = Thrift::Socket.new(@host, @port)
      transport = Thrift::FramedTransport.new(socket)
      protocol = Thrift::BinaryProtocol.new(transport)
      client = ThriftBenchmark::BenchmarkService::Client.new(protocol)
      begin
        transport.open
      rescue
        Marshal.dump [:connection_failure, Time.now], STDOUT
      else
        Marshal.dump [:start, Time.now], STDOUT
        @calls_per_client.times do
          Marshal.dump [:call_start, Time.now], STDOUT
          client.fibonacci(15)
          Marshal.dump [:call_end, Time.now], STDOUT
        end
        transport.close
        Marshal.dump [:end, Time.now], STDOUT
      end
    end
  end
end

host, port, clients_per_process, calls_per_client = ARGV

Client.new(host, port.to_i, clients_per_process.to_i, calls_per_client.to_i).run
