require 'rubygems'
$:.unshift File.dirname(__FILE__) + '/../lib'
require 'thrift'
require 'thrift/server/nonblockingserver'
$:.unshift File.dirname(__FILE__) + "/gen-rb"
require 'BenchmarkService'
require 'thread'
require 'stringio'
HOST = 'localhost'
PORT = 42587

Thread.abort_on_exception = true

###############
## Server
###############

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

  def self.start_server(serverClass)
    handler = BenchmarkHandler.new
    processor = ThriftBenchmark::BenchmarkService::Processor.new(handler)
    transport = ServerSocket.new(HOST, PORT)
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
    if @server.respond_to? :shutdown
      @server.shutdown
    else
      @server_thread.kill
    end
  end

  def self.class
    @server and @server.class
  end
end

module Client
  include Thrift

  def self.start_client(&block)
    transport = FramedTransport.new(Socket.new(HOST, PORT))
    protocol = BinaryProtocol.new(transport)
    client = ThriftBenchmark::BenchmarkService::Client.new(protocol)
    # transport.open
    Thread.new do
      block.call(client, transport)
    end
  end
end

class BenchmarkManager
  def initialize(opts)
    @host = opts.fetch(:host, 'localhost')
    @port = opts.fetch(:port)
    @num_processes = opts.fetch(:num_processes, 40)
    @clients_per_process = opts.fetch(:clients_per_process, 10)
    @calls_per_client = opts.fetch(:calls_per_client, 50)
  end

  def run
    @pool = []
    @benchmark_start = Time.now
    puts "Spawning benchmark processes..."
    @num_processes.times do
      spawn
      sleep 0.05 # space out spawns
    end
    collect_output
    @benchmark_end = Time.now # we know the procs are done here
    translate_output
    analyze_output
    report_output
  end

  def spawn
    rd, wr = IO.pipe
    pid = fork do
      STDIN.close
      rd.close
      @clients_per_process.times do
        transport = Thrift::FramedTransport.new(Thrift::Socket.new(@host, @port))
        protocol = Thrift::BinaryProtocol.new(transport)
        client = ThriftBenchmark::BenchmarkService::Client.new(protocol)
        begin
          transport.open
        rescue
          Marshal.dump [:connection_failure, Time.now], wr
        else
          Marshal.dump [:start, Time.now], wr
          @calls_per_client.times do
            Marshal.dump [:call_start, Time.now], wr
            client.fibonacci(15)
            Marshal.dump [:call_end, Time.now], wr
          end
          transport.close
          Marshal.dump [:end, Time.now], wr
        end
      end
    end
    wr.close
    @pool << rd
    pid
  end

  def collect_output
    puts "Collecting output..."
    # read from @pool until all sockets are closed
    @buffers = Hash.new { |h,k| h[k] = '' }
    until @pool.empty?
      rd, = select(@pool)
      next if rd.nil?
      rd.each do |fd|
        begin
          @buffers[fd] << fd.read_nonblock(4096)
        rescue EOFError
          @pool.delete fd
        end
      end
    end
  end

  def translate_output
    puts "Translating output..."
    @output = []
    @buffers.each do |fd, buffer|
      strio = StringIO.new(buffer)
      logs = []
      begin
        loop do
          logs << Marshal.load(strio)
        end
      rescue EOFError
        @output << logs
      end
    end
  end

  def analyze_output
    puts "Analyzing output..."
    call_times = []
    client_times = []
    connection_failures = []
    longest_call = 0
    longest_client = 0
    @output.each do |logs|
      cur_call, cur_client = nil
      logs.each do |tok, time|
        case tok
        when :start
          cur_client = time
        when :call_start
          cur_call = time
        when :call_end
          delta = time - cur_call
          call_times << delta
          longest_call = delta unless longest_call > delta
          cur_call = nil
        when :end
          delta = time - cur_client
          client_times << delta
          longest_client = delta unless longest_client > delta
          cur_client = nil
        when :connection_failure
          connection_failures << time
        end
      end
    end
    @report = {}
    @report[:total_calls] = call_times.inject(0.0) { |a,t| a += t }
    @report[:avg_calls] = @report[:total_calls] / call_times.size
    @report[:total_clients] = client_times.inject(0.0) { |a,t| a += t }
    @report[:avg_clients] = @report[:total_clients] / client_times.size
    @report[:connection_failures] = connection_failures.size
    @report[:longest_call] = longest_call
    @report[:longest_client] = longest_client
    @report[:total_benchmark_time] = @benchmark_end - @benchmark_start
    @report[:fastthread] = $".include?('fastthread.bundle')
  end

  def report_output
    fmt = "%.4f seconds"
    puts
    tabulate "%d",
             [["Server class", "%s"], Server.class],
             ["Number of processes", @num_processes],
             ["Clients per process", @clients_per_process],
             ["Calls per client", @calls_per_client],
             [["Using fastthread", "%s"], @report[:fastthread] ? "yes" : "no"]
    puts
    tabulate fmt,
             [["Connection failures", "%d"], @report[:connection_failures]],
             ["Average time per call", @report[:avg_calls]],
             ["Average time per client (%d calls)" % @calls_per_client, @report[:avg_clients]],
             ["Total time for all calls", @report[:total_calls]],
             ["Real time for benchmarking", @report[:total_benchmark_time]],
             ["Longest call time", @report[:longest_call]],
             ["Longest client time (%d calls)" % @calls_per_client, @report[:longest_client]]
  end

  def tabulate(fmt, *labels_and_values)
    labels = labels_and_values.map { |(l,)| Array === l ? l.first : l }
    label_width = labels.inject(0) { |w,l| l.size > w ? l.size : w }
    labels_and_values.each do |(l,v)|
      f = fmt
      l, f = l if Array === l
      puts "%-#{label_width+1}s #{f}" % [l+":", v]
    end
  end
end

def resolve_const(const)
  const and const.split('::').inject(Object) { |k,c| k.const_get(c) }
end

puts "Starting server..."
serverklass = resolve_const(ENV['THRIFT_SERVER']) || Thrift::NonblockingServer
Server.start_server(serverklass)

sleep 0.2 # give the server time to start

BenchmarkManager.new(:host => HOST, :port => PORT, :num_processes => 40, :clients_per_process => 5).run

Server.shutdown
