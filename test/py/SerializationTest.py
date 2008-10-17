#!/usr/bin/env python

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
      self.v1obj = VersioningTestV1(d=dict(
          begin_in_both=12345,
          end_in_both=54321,
          ))

      self.v2obj = VersioningTestV2(d=dict(
          begin_in_both=12345,
          newint=1,
          newbyte=2,
          newshort=3,
          newlong=4,
          newdouble=5.0,
          newstruct=Bonk(d=dict(message="Hello!", type=123)),
          newlist=[7,8,9],
          newset=[42,1,8],
          newmap={1:2,2:3},
          newstring="Hola!",
          end_in_both=54321,
          ))

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


def suite():
  suite = unittest.TestSuite()
  loader = unittest.TestLoader()

  suite.addTest(loader.loadTestsFromTestCase(NormalBinaryTest))
  suite.addTest(loader.loadTestsFromTestCase(AcceleratedBinaryTest))
  return suite

if __name__ == "__main__":
  unittest.main(defaultTest="suite", testRunner=unittest.TextTestRunner(verbosity=2))
