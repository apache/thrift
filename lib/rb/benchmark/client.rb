$:.unshift File.dirname(__FILE__) + '/../lib'
require 'thrift'
require 'thrift/server/nonblockingserver'
$:.unshift File.dirname(__FILE__) + "/gen-rb"
require 'BenchmarkService'

class Client
  def initialize(host, port, clients_per_process, calls_per_client, log_exceptions)
    @host = host
    @port = port
    @clients_per_process = clients_per_process
    @calls_per_client = calls_per_client
    @log_exceptions = log_exceptions
  end

  def run
    @clients_per_process.times do
      socket = Thrift::Socket.new(@host, @port)
      transport = Thrift::FramedTransport.new(socket)
      protocol = Thrift::BinaryProtocol.new(transport)
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

host, port, clients_per_process, calls_per_client = ARGV

Client.new(host, port.to_i, clients_per_process.to_i, calls_per_client.to_i, log_exceptions).run
