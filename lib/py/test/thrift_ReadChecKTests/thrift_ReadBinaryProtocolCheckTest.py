from thrift.TConfiguration import TConfiguration
from thrift.protocol.TBinaryProtocol import TBinaryProtocol
from thrift.protocol import TCompactProtocol
from thrift.protocol.TJSONProtocol import TJSONProtocol
from thrift.protocol.TProtocol import TType
from thrift.protocol import TMap, TSet, TList
from thrift.transport.TEndpointTransport import TEndpointTransport
from thrift.transport.TSocket import TSocket, TServerSocket
from thrift.transport.TTransport import TTransportException
from thrift.transport import TTransport,TMemoryBuffer, TBufferedTransport
from thrift.transport import TZlibTransport
import unittest
import traceback
import os
import string
import random


class TestBinaryProtocolReadCheck(unittest.TestCase):
   
    config = TConfiguration()
    config.setMaxMessageSize(20)

    def test_TMap(self):
        buf = TMemoryBuffer.TMemoryBuffer()
        transport = TBufferedTransport.TBufferedTransportFactory().getTransport(buf, self.config)
        protocol= TBinaryProtocol(transport)
        tmap = TMap.TMap(TType.BYTE,TType.BYTE,TType.I32)
        protocol.writeMapBegin(tmap.keyType, tmap.valueType, tmap.size)
        protocol.writeMapEnd()

        transport.flush()
        data_r = buf.getvalue()
        buf = TMemoryBuffer.TMemoryBuffer(data_r)
        transport = TBufferedTransport.TBufferedTransportFactory().getTransport(buf, self.config)
        protocol= TBinaryProtocol(transport)
        acc = protocol.readMapBegin()
        self.assertEqual(acc, (TType.BYTE,TType.BYTE,TType.I32))

        
    def test_TList(self):
        buf = TMemoryBuffer.TMemoryBuffer()
        transport = TBufferedTransport.TBufferedTransportFactory().getTransport(buf, self.config)
        protocol= TBinaryProtocol(transport)
        tlist = TList.TList(TType.BYTE,TType.I32)
        protocol.writeListBegin(tlist.elemType, tlist.size)
        protocol.writeListEnd()

        transport.flush()
        data_r = buf.getvalue()
        buf = TMemoryBuffer.TMemoryBuffer(data_r)
        transport = TBufferedTransport.TBufferedTransportFactory().getTransport(buf, self.config)
        protocol= TBinaryProtocol(transport)
        acc = protocol.readListBegin()
        self.assertEqual(acc, (TType.BYTE,TType.I32))

    def test_TSet(self):
        buf = TMemoryBuffer.TMemoryBuffer()
        transport = TBufferedTransport.TBufferedTransportFactory().getTransport(buf, self.config)
        protocol= TBinaryProtocol(transport)
        tset = TSet.TSet(TType.BYTE,TType.I32)
        protocol.writeSetBegin(tset.elemType, tset.size)
        protocol.writeSetEnd()

        transport.flush()
        data_r = buf.getvalue()
        buf = TMemoryBuffer.TMemoryBuffer(data_r)
        transport = TBufferedTransport.TBufferedTransportFactory().getTransport(buf, self.config)
        protocol= TBinaryProtocol(transport)
        acc = protocol.readSetBegin()
        self.assertEqual(acc, (TType.BYTE,TType.I32))


if __name__ == '__main__':
    unittest.main()
