#!/usr/bin/env ruby

$:.push('gen-rb')
$:.push('../../lib/rb/lib')

require 'ThriftTest'

class TestHandler
  def testVoid
  end

  def testString(thing)
    return thing
  end

  def testByte(thing)
    return thing
  end

  def testI32(thing)
    return thing
  end

  def testI64(thing)
    return thing
  end

  def testDouble(thing)
    return thing
  end

  def testStruct(thing)
    return thing
  end

  def testMap(thing)
    return thing
  end

  def testSet(thing)
    return thing
  end

  def testList(thing)
    return thing
  end

  def testNest(thing)
    return thing
  end

  def testInsanity(thing)
    num, uid = thing.userMap.find { true }
    return {uid => {num => thing}}
  end

  def testMapMap(thing)
    return {thing => {thing => thing}}
  end

  def testEnum(thing)
    return thing
  end

  def testTypedef(thing)
    return thing
  end

  def testException(thing)
    raise Thrift::Test::Xception, 'error'
  end

end
