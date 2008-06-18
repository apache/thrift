$:.unshift File.dirname(__FILE__) + '/../lib'
require 'thrift'
require 'thrift/server/nonblockingserver'
$:.unshift File.dirname(__FILE__) + "/gen-rb"
require 'BenchmarkService'

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

  def self.start_server(host, port, serverClass)
    handler = BenchmarkHandler.new
    processor = ThriftBenchmark::BenchmarkService::Processor.new(handler)
    transport = ServerSocket.new(host, port)
    transportFactory = FramedTransportFactory.new
    args = [processor, transport, transportFactory, nil, 20]
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

host, port, serverklass = ARGV

Server.start_server(host, port.to_i, resolve_const(serverklass))

# let our host know that the interpreter has started
# ideally we'd wait until the server was serving, but we don't have a hook for that
Marshal.dump(:started, STDOUT)
STDOUT.flush

Marshal.load(STDIN) # wait until we're instructed to shut down

Server.shutdown
