#!/usr/bin/env ruby

$:.push('../gen-rb')
$:.unshift '../../lib/rb/lib'

require 'thrift'
require 'thrift/protocol/binaryprotocol'
require 'thrift/server/tserver'

require 'Calculator'
require 'shared_types'

class CalculatorHandler
  def initialize()
    @log = {}
  end

  def ping()
    puts "ping()"
  end

  def add(n1, n2)
    print "add(", n1, ",", n2, ")\n"
    return n1 + n2
  end

  def calculate(logid, work)
    print "calculate(", logid, ", {", work.op, ",", work.num1, ",", work.num2,"})\n"
    if work.op == Operation::ADD
      val = work.num1 + work.num2
    elsif work.op == Operation::SUBTRACT
      val = work.num1 - work.num2
    elsif work.op == Operation::MULTIPLY
      val = work.num1 * work.num2
    elsif work.op == Operation::DIVIDE
      if work.num2 == 0
        x = InvalidOperation.new()
        x.what = work.op
        x.why = "Cannot divide by 0"
        raise x
      end
      val = work.num1 / work.num2
    else
      x = InvalidOperation.new()
      x.what = work.op
      x.why = "Invalid operation"
      raise x
    end

    entry = SharedStruct.new()
    entry.key = logid
    entry.value = "#{val}"
    @log[logid] = entry

    return val

  end

  def getStruct(key)
    print "getStruct(", key, ")\n"
    return @log[key]
  end

  def zip()
    print "zip\n"
  end

end

handler = CalculatorHandler.new()
processor = Calculator::Processor.new(handler)
transport = Thrift::ServerSocket.new(9090)
transportFactory = Thrift::BufferedTransportFactory.new()
server = Thrift::SimpleServer.new(processor, transport, transportFactory)

puts "Starting the server..."
server.serve()
puts "done."
