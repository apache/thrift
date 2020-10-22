from thrift.transport import TFramedTransport, TTransport
from thrift.TConfiguration import TConfiguration
import unittest

class TestTFrameTransportReadCheck(unittest.TestCase):
    data = '{"1":[1,"hello"], "a":{"A":"abc"}, "bool":true, "num":12345}'
    config = TConfiguration()
    config.setMaxMessageSize(20000)
    def test_frametransport_readcheck(self):
        buf = TTransport.TMemoryBuffer()
        trans = TFramedTransport.TFramedTransportFactory().getTransport(buf,self.config)
        frame_trans =TFramedTransport.TFramedTransport(trans, self.config)
        frame_trans.write(self.data.encode('utf-8'))
        frame_trans.flush()
        value = buf.getvalue()
        frame_trans.close()

        buf = TTransport.TMemoryBuffer(value)
        trans = TFramedTransport.TFramedTransportFactory().getTransport(buf,self.config)
        frame_trans = TFramedTransport.TFramedTransport(trans, self.config)
        acc = frame_trans.read(len(self.data)).decode('utf-8')
        frame_trans.close()
        self.assertEqual(self.data, acc)

if __name__ =='__main__':
    unittest.main()

