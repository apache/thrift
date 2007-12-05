#!/usr/bin/env ruby

$:.push('gen-rb')
$:.push('../../lib/rb/lib')

require 'thrift/transport/tsocket'
require 'thrift/protocol/tbinaryprotocol'
require 'ThriftTest'

t = TFramedTransport.new(TSocket.new('localhost', 9090))
p = TBinaryProtocol.new(t)
c = Thrift::Test::ThriftTest::Client.new(p)

t.open()

puts c.testString('string')
puts c.testByte(8)
puts c.testByte(-8)
puts c.testI32(32)
puts c.testI32(-32)
puts c.testI64(64)
puts c.testI64(-64)
puts c.testDouble(3.14)
puts c.testDouble(-3.14)
puts c.testMap({1 => 1, 2 => 2, 3 => 3})
puts c.testList([1,2,3,4,5])
puts c.testSet({1 => true, 2 => true, 3 => true})
struct = Thrift::Test::Xtruct.new({'string_thing' => 'hi!', 'i32_thing' => 4 })
puts c.testStruct(struct)
puts c.testNest(Thrift::Test::Xtruct2.new({'struct_thing' => struct, 'i32_thing' => 10}))
insane = Thrift::Test::Insanity.new({'userMap' => { Thrift::Test::Numberz::ONE => 44 }, 'xtructs' => [struct, Thrift::Test::Xtruct.new({'string_thing' => 'hi again', 'i32_thing' => 12})]})
puts c.testInsanity(insane)
puts c.testMapMap(4).inspect

t.close()
