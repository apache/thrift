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
from thrift.transport import TTransport
from thrift.transport import TSocket
from thrift.protocol import TBinaryProtocol
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
          newset=[42,1,8],
          newmap={1:2,2:3},
          newstring="Hola!",
          end_in_both=54321,
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


class NormalBinaryTest(AbstractTest):
  protocol_factory = TBinaryProtocol.TBinaryProtocolFactory()

class AcceleratedBinaryTest(AbstractTest):
  protocol_factory = TBinaryProtocol.TBinaryProtocolAcceleratedFactory()


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



def suite():
  suite = unittest.TestSuite()
  loader = unittest.TestLoader()

  suite.addTest(loader.loadTestsFromTestCase(NormalBinaryTest))
  suite.addTest(loader.loadTestsFromTestCase(AcceleratedBinaryTest))
  suite.addTest(loader.loadTestsFromTestCase(AcceleratedFramedTest))
  return suite

if __name__ == "__main__":
  unittest.main(defaultTest="suite", testRunner=unittest.TextTestRunner(verbosity=2))
