import sys
sys.path.append('/usr/lib/python2.6/site-packages/')

from thrift.transport import TSocket
import socket, ssl

class TSSLSocket(TSocket.TSocket):
    def open(self):
        try:
          res0 = self._resolveAddr()
          for res in res0:
            plain_sock = socket.socket(res[0], res[1])
            #TODO verify server cert
            self.handle = ssl.wrap_socket(plain_sock, ssl_version=ssl.PROTOCOL_TLSv1) 
            self.handle.settimeout(self._timeout)
            try:
              self.handle.connect(res[4])
            except socket.error, e:
              if res is not res0[-1]:
                continue
              else:
                raise e
            break
        except socket.error, e:
          if self._unix_socket:
            message = 'Could not connect to secure socket %s' % self._unix_socket
          else:
            message = 'Could not connect to %s:%d' % (self.host, self.port)
          raise TTransportException(type=TTransportException.NOT_OPEN, message=message)

class TSSLServerSocket(TSocket.TServerSocket):
    def accept(self):
        plain_client, addr = self.handle.accept()
        result = TSocket.TSocket()
        #TODO take certfile/keyfile as a parameter at setup
        client = ssl.wrap_socket(plain_client, certfile='cert.pem', server_side=True)
        result.setHandle(client)
        return result
