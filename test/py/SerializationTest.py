#!/usr/bin/env python

#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

import sys, glob
sys.path.insert(0, './gen-py')
sys.path.insert(0, glob.glob('../../lib/py/build/lib.*')[0])

from ThriftTest.ttypes import *
from DebugProtoTest.ttypes import CompactProtoTestStruct, Empty
from thrift.transport import TTransport
from thrift.transport import TSocket
from thrift.protocol import TBinaryProtocol, TCompactProtocol
from thrift.TSerialization import serialize, deserialize
import unittest
import time

class AbstractTest(unittest.TestCase):

  def setUp(self):
      self.v1obj = VersioningTestV1(
          begin_in_both=12345,
          old_string='aaa',
          end_in_both=54321,
          )

      self.v2obj = VersioningTestV2(
          begin_in_both=12345,
          newint=1,
          newbyte=2,
          newshort=3,
          newlong=4,
          newdouble=5.0,
          newstruct=Bonk(message="Hello!", type=123),
          newlist=[7,8,9],
          newset=set([42,1,8]),
          newmap={1:2,2:3},
          newstring="Hola!",
          end_in_both=54321,
          )

      self.bools = Bools(im_true=True, im_false=False)
      self.bools_flipped = Bools(im_true=False, im_false=True)

      self.large_deltas = LargeDeltas (
          b1=self.bools,
          b10=self.bools_flipped,
          b100=self.bools,
          check_true=True,
          b1000=self.bools_flipped,
          check_false=False,
          vertwo2000=VersioningTestV2(newstruct=Bonk(message='World!', type=314)),
          a_set2500=set(['lazy', 'brown', 'cow']),
          vertwo3000=VersioningTestV2(newset=set([2, 3, 5, 7, 11])),
          big_numbers=[2**8, 2**16, 2**31-1, -(2**31-1)]
          )

      self.compact_struct = CompactProtoTestStruct(
          a_byte = 127,
          a_i16=32000,
          a_i32=1000000000,
          a_i64=0xffffffffff,
          a_double=5.6789,
          a_string="my string",
          true_field=True,
          false_field=False,
          empty_struct_field=Empty(),
          byte_list=[-127, -1, 0, 1, 127],
          i16_list=[-1, 0, 1, 0x7fff],
          i32_list= [-1, 0, 0xff, 0xffff, 0xffffff, 0x7fffffff],
          i64_list=[-1, 0, 0xff, 0xffff, 0xffffff, 0xffffffff, 0xffffffffff, 0xffffffffffff, 0xffffffffffffff, 0x7fffffffffffffff],
          double_list=[0.1, 0.2, 0.3],
          string_list=["first", "second", "third"],
          boolean_list=[True, True, True, False, False, False],
          struct_list=[Empty(), Empty()],
          byte_set=set([-127, -1, 0, 1, 127]),
          i16_set=set([-1, 0, 1, 0x7fff]),
          i32_set=set([1, 2, 3]),
          i64_set=set([-1, 0, 0xff, 0xffff, 0xffffff, 0xffffffff, 0xffffffffff, 0xffffffffffff, 0xffffffffffffff, 0x7fffffffffffffff]),
          double_set=set([0.1, 0.2, 0.3]),
          string_set=set(["first", "second", "third"]),
          boolean_set=set([True, False]),
          #struct_set=set([Empty()]), # unhashable instance
          byte_byte_map={1 : 2},
          i16_byte_map={1 : 1, -1 : 1, 0x7fff : 1},
          i32_byte_map={1 : 1, -1 : 1, 0x7fffffff : 1},
          i64_byte_map={0 : 1,  1 : 1, -1 : 1, 0x7fffffffffffffff : 1},
          double_byte_map={-1.1 : 1, 1.1 : 1},
          string_byte_map={"first" : 1, "second" : 2, "third" : 3, "" : 0},
          boolean_byte_map={True : 1, False: 0},
          byte_i16_map={1 : 1, 2 : -1, 3 : 0x7fff},
          byte_i32_map={1 : 1, 2 : -1, 3 : 0x7fffffff},
          byte_i64_map={1 : 1, 2 : -1, 3 : 0x7fffffffffffffff},
          byte_double_map={1 : 0.1, 2 : -0.1, 3 : 1000000.1},
          byte_string_map={1 : "", 2 : "blah", 3 : "loooooooooooooong string"},
          byte_boolean_map={1 : True, 2 : False},
          #list_byte_map # unhashable
          #set_byte_map={set([1, 2, 3]) : 1, set([0, 1]) : 2, set([]) : 0}, # unhashable
          #map_byte_map # unhashable
          byte_map_map={0 : {}, 1 : {1 : 1}, 2 : {1 : 1, 2 : 2}},
          byte_set_map={0 : set([]), 1 : set([1]), 2 : set([1, 2])},
          byte_list_map={0 : [], 1 : [1], 2 : [1, 2]},
          )


  def _serialize(self, obj):
      trans = TTransport.TMemoryBuffer()
      prot = self.protocol_factory.getProtocol(trans)
      obj.write(prot)
      return trans.getvalue()

  def _deserialize(self, objtype, data):
      prot = self.protocol_factory.getProtocol(TTransport.TMemoryBuffer(data))
      ret = objtype()
      ret.read(prot)
      return ret

  def testForwards(self):
      obj = self._deserialize(VersioningTestV2, self._serialize(self.v1obj))
      self.assertEquals(obj.begin_in_both, self.v1obj.begin_in_both)
      self.assertEquals(obj.end_in_both, self.v1obj.end_in_both)

  def testBackwards(self):
      obj = self._deserialize(VersioningTestV1, self._serialize(self.v2obj))
      self.assertEquals(obj.begin_in_both, self.v2obj.begin_in_both)
      self.assertEquals(obj.end_in_both, self.v2obj.end_in_both)

  def testSerializeV1(self):
    obj = self._deserialize(VersioningTestV1, self._serialize(self.v1obj))
    self.assertEquals(obj, self.v1obj)

  def testSerializeV2(self):
    obj = self._deserialize(VersioningTestV2, self._serialize(self.v2obj))
    self.assertEquals(obj, self.v2obj)

  def testBools(self):
    self.assertNotEquals(self.bools, self.bools_flipped)
    obj = self._deserialize(Bools, self._serialize(self.bools))
    self.assertEquals(obj, self.bools)
    obj = self._deserialize(Bools, self._serialize(self.bools_flipped))
    self.assertEquals(obj, self.bools_flipped)

  def testLargeDeltas(self):
    # test large field deltas (meaningful in CompactProto only)
    obj = self._deserialize(LargeDeltas, self._serialize(self.large_deltas))
    self.assertEquals(obj, self.large_deltas)

  def testCompactStruct(self):
    # test large field deltas (meaningful in CompactProto only)
    obj = self._deserialize(CompactProtoTestStruct, self._serialize(self.compact_struct))
    self.assertEquals(obj, self.compact_struct)

class NormalBinaryTest(AbstractTest):
  protocol_factory = TBinaryProtocol.TBinaryProtocolFactory()

class AcceleratedBinaryTest(AbstractTest):
  protocol_factory = TBinaryProtocol.TBinaryProtocolAcceleratedFactory()

class CompactProtocolTest(AbstractTest):
  protocol_factory = TCompactProtocol.TCompactProtocolFactory()

class AcceleratedFramedTest(unittest.TestCase):
  def testSplit(self):
    """Test FramedTransport and BinaryProtocolAccelerated

    Tests that TBinaryProtocolAccelerated and TFramedTransport
    play nicely together when a read spans a frame"""

    protocol_factory = TBinaryProtocol.TBinaryProtocolAcceleratedFactory()
    bigstring = "".join(chr(byte) for byte in range(ord("a"), ord("z")+1))

    databuf = TTransport.TMemoryBuffer()
    prot = protocol_factory.getProtocol(databuf)
    prot.writeI32(42)
    prot.writeString(bigstring)
    prot.writeI16(24)
    data = databuf.getvalue()
    cutpoint = len(data)/2
    parts = [ data[:cutpoint], data[cutpoint:] ]

    framed_buffer = TTransport.TMemoryBuffer()
    framed_writer = TTransport.TFramedTransport(framed_buffer)
    for part in parts:
      framed_writer.write(part)
      framed_writer.flush()
    self.assertEquals(len(framed_buffer.getvalue()), len(data) + 8)

    # Recreate framed_buffer so we can read from it.
    framed_buffer = TTransport.TMemoryBuffer(framed_buffer.getvalue())
    framed_reader = TTransport.TFramedTransport(framed_buffer)
    prot = protocol_factory.getProtocol(framed_reader)
    self.assertEqual(prot.readI32(), 42)
    self.assertEqual(prot.readString(), bigstring)
    self.assertEqual(prot.readI16(), 24)

class SerializersTest(unittest.TestCase):

  def testSerializeThenDeserialize(self):
    obj = Xtruct2(i32_thing=1,
                  struct_thing=Xtruct(string_thing="foo"))

    s1 = serialize(obj)
    for i in range(10):
      self.assertEquals(s1, serialize(obj))
      objcopy = Xtruct2()
      deserialize(objcopy, serialize(obj))
      self.assertEquals(obj, objcopy)

    obj = Xtruct(string_thing="bar")
    objcopy = Xtruct()
    deserialize(objcopy, serialize(obj))
    self.assertEquals(obj, objcopy)

    # test booleans
    obj = Bools(im_true=True, im_false=False)
    objcopy = Bools()
    deserialize(objcopy, serialize(obj))
    self.assertEquals(obj, objcopy)
    
    # test enums
    for num, name in Numberz._VALUES_TO_NAMES.iteritems():
      obj = Bonk(message='enum Numberz value %d is string %s' % (num, name), type=num)
      objcopy = Bonk()
      deserialize(objcopy, serialize(obj))
      self.assertEquals(obj, objcopy)
  

def suite():
  suite = unittest.TestSuite()
  loader = unittest.TestLoader()

  suite.addTest(loader.loadTestsFromTestCase(NormalBinaryTest))
  suite.addTest(loader.loadTestsFromTestCase(AcceleratedBinaryTest))
  suite.addTest(loader.loadTestsFromTestCase(CompactProtocolTest))
  suite.addTest(loader.loadTestsFromTestCase(AcceleratedFramedTest))
  suite.addTest(loader.loadTestsFromTestCase(SerializersTest))
  return suite

if __name__ == "__main__":
  unittest.main(defaultTest="suite", testRunner=unittest.TextTestRunner(verbosity=2))
