from thrift.transport import TBufferedTransport, TTransport
from thrift.TConfiguration import TConfiguration
import unittest

class TestTBufferedTransportReadCheck(unittest.TestCase):
    data = '{"1":[1,"hello"], "a":{"A":"abc"}, "bool":true, "num":12345}'
    config = TConfiguration()
    config.setMaxMessageSize(200000)
    def test_bufferedtransport_readcheck(self):
        buf = TTransport.TMemoryBuffer()
        trans = TBufferedTransport.TBufferedTransportFactory().getTransport(buf,self.config)
        buffer_trans =TBufferedTransport.TBufferedTransport(trans, self.config)
        buffer_trans.write(self.data.encode('utf-8'))
        buffer_trans.flush()
        value = buf.getvalue()
        buffer_trans.close()

        buf = TTransport.TMemoryBuffer(value)
        trans = TBufferedTransport.TBufferedTransportFactory().getTransport(buf,self.config)
        buffer_trans = TBufferedTransport.TBufferedTransport(trans, self.config)
        acc = buffer_trans.read(len(self.data)).decode('utf-8')
        buffer_trans.close()
        self.assertEqual(self.data, acc)

if __name__ =='__main__':
    unittest.main()

