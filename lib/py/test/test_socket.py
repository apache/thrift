from thrift.transport import TSocket
import unittest
import time
import socket
import random
class TimeoutTest(unittest.TestCase):
    def SetUp(self):
        sr=socket.socket(socket.AF_UNIX,socket.SOCK_STREAM)
        sr.bind(host,self.post)
        sr.listen(5)
    def testConnectTimeout(self):
        starttime=time.time()
        try:
            leaky=[]
            for i in range(100):
                socket=socket.socket(socket.AF_UNIX,socket.SOCK_STREAM)
                socket.settimeout(20)
                socket.open()
                leaky.append(socket)
        except Exception:
            self.assert_(time.time()-starttime<10.0)
    def testReadTimeout(self):
        try:
            leaky=[]
            starttime=time.time()
            socket=socket.socket(socket.AF_UNIX,socket.SOCK_STREAM)
            socket.settimeout(20)
            socket.open()
            content=read(socket)
            leaky.append(content)
            self.assertEqual(len(leaky),0)
        except Exception:
            self.assertEqual(len(leaky),0)

if __name__=='__main__':
    from thrift.transport.TSocket import TSocketBase,TSocket,TServerSocket
    from thrift.transport.TTransport import TTransportException
    unittest.main()




