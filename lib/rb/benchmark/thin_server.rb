$:.unshift File.dirname(__FILE__) + '/../lib'
require 'thrift'
require 'thrift/server/nonblockingserver'
$:.unshift File.dirname(__FILE__) + "/gen-rb"
require 'BenchmarkService'
HOST = 'localhost'
PORT = 42587

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

handler = BenchmarkHandler.new
processor = ThriftBenchmark::BenchmarkService::Processor.new(handler)
transport = Thrift::ServerSocket.new(HOST, PORT)
transportFactory = Thrift::FramedTransportFactory.new
logger = Logger.new(STDERR)
logger.level = Logger::WARN
Thrift::NonblockingServer.new(processor, transport, transportFactory, nil, 20, logger).serve
