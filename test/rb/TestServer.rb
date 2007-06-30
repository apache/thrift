#!/usr/bin/env ruby

$:.push('gen-rb')
$:.push('../../lib/rb/lib')

require 'thrift/transport/tsocket'
require 'thrift/protocol/tbinaryprotocol'
require 'thrift/server/tserver'
require 'ThriftTest'

class TestHandler
  include ThriftTest::Iface
  
  def testVoid()
    print "testVoid()\n"
  end

  def testString(thing)
    print "testString(#{thing})\n"
    return thing
  end

  def testByte(thing)
    print "testByte(#{thing})\n"
    return thing
  end

  def testI32(thing)
    print "testI32(#{thing})\n"
    return thing
  end

  def testI64(thing)
    print "testI64(#{thing})\n"
    return thing
  end

  def testDouble(thing)
    print "testDouble(#{thing})\n"
    return thing
  end

  def testStruct(thing)
    print "testStruct(#{thing})\n"
    return thing
  end

  def testMap(thing)
    print "testMap(#{thing})\n"
    return thing
  end
    
  def testSet(thing)
    print "testSet(#{thing})\n"
    return thing
  end

  def testList(thing)
    print "testList(#{thing})\n"
    return thing
  end

  def testNest(thing)
    print "testNest(#{thing})\n"
    puts "  i32_thing: #{thing.i32_thing}"
    puts "  with struct: "
    %w{ string_thing byte_thing i32_thing }.each do |t|
      puts "    #{t}: #{thing.struct_thing.send(t)}"
    end
    return thing
  end

end

handler = TestHandler.new()
processor = ThriftTest::Processor.new(handler)
transport = TServerSocket.new(9090)
server = TSimpleServer.new(processor, transport)
server.serve()
