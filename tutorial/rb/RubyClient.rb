#!/usr/bin/env ruby

$:.push('../gen-rb')

require 'thrift/transport/tsocket.rb'
require 'thrift/protocol/tbinaryprotocol.rb'

require 'Calculator'

begin
  port = ARGV[0] || 9090
  
  transport = TBufferedTransport.new(TSocket.new('localhost', port))
  protocol = TBinaryProtocol.new(transport)
  client = Calculator::Client.new(protocol)

  transport.open()

  client.ping()
  print "ping()\n"

  sum = client.add(1,1)
  print "1+1=", sum, "\n"

  sum = client.add(1,4)
  print "1+4=", sum, "\n"

  work = Work.new()

  work.op = Operation::SUBTRACT
  work.num1 = 15
  work.num2 = 10
  diff = client.calculate(1, work)
  print "15-10=", diff, "\n"

  log = client.getStruct(1)
  print "Log: ", log.value, "\n"

  begin
    work.op = Operation::DIVIDE
    work.num1 = 1
    work.num2 = 0
    quot = client.calculate(1, work)
    puts "Whoa, we can divide by 0 now?"
  rescue InvalidOperation => io
    print "InvalidOperation: ", io.why, "\n"
  end

  client.zip()
  print "zip\n"

  transport.close()

rescue TException => tx
  print 'TException: ', tx.message, "\n"
end
