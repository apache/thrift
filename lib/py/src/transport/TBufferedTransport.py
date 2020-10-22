from struct import pack,unpack
from thrift.Thrift import TException
from thrift.compat import BufferIO
from thrift.TConfiguration import TConfiguration
from thrift.transport.TTransport import TTransportBase, CReadableTransport, TTransportException


class TBufferedTransport(TTransportBase, CReadableTransport):
    """Class that wraps another transport and buffers its I/O.

    The implementation uses a (configurable) fixed-size read buffer
    but buffers all writes until a flush is performed.
    """
    
    remainingMessageSize = 0
    knownMessageSize = 0
    DEFAULT_BUFFER = 4096

    def __init__(self, trans, config, rbuf_size=DEFAULT_BUFFER):
        
        self.__trans = trans
        self.config = config
        self.__wbuf = BufferIO()
        # Pass string argument to initialize read buffer as cStringIO.InputType
        self.__rbuf = BufferIO(b'')
        self.__rbuf_size = rbuf_size
        self.resetConsumedMessageSize(-1)

    def getMessageSize(self):
        getMessageSize = self.getConfiguration().getMaxMessageSize()
        return getMessageSize

    def getConfiguration(self):
        if self.config == None:
            cfg = TConfiguration()
        else:
            cfg = self.config
        return cfg

    def resetConsumedMessageSize(self, newSize):
        if newSize < 0:
            self.knownMessageSize = self.getMessageSize()
            self.remainingMessageSize = self.getMessageSize()
            return
        if newSize > self.knownMessageSize:
            raise Exception(TTransportException.END_OF_FILE, "MaxMessageSize reached")
            self.knownMessageSize = newSize
            self.remainingMessageSize = newSize

    def updateKnownMessageSize(self, size):
        consumed = self.knownMessageSize - self.remainingMessageSize 
        if size == 0:
            self.resetConSumedMessageSize(-1)
        else:
            self.resetConSumedMessageSize(size)
        self.countConsumedMessageBytes(consumed)

    def checkReadBytesAvailable(self, numBytes):
        if self.remainingMessageSize < numBytes:
#            print(self.remainingMessageSize, numBytes)
            raise Exception(TTransportException.END_OF_FILE, "MaxMessageSize reached")

    def countConsumedMessageBytes(self, numBytes):
        if self.remainingMessageSize >= numBytes:
            self.remainingMessageSize -= numBytes
        else:
            self.remainingMessageSize = 0
            raise Exception(TTransportException.END_OF_FILE, "MaxMessageSize reached")

    def isOpen(self):
        return self.__trans.isOpen()

    def open(self):
        return self.__trans.open()

    def close(self):
        return self.__trans.close()

    def read(self, sz):
        self.checkReadBytesAvailable(sz)
        ret = self.__rbuf.read(sz)
        if len(ret) != 0:
            return ret
        self.__rbuf = BufferIO(self.__trans.read(max(sz, self.__rbuf_size)))
        return self.__rbuf.read(sz)

    def write(self, buf):
        try:
            self.__wbuf.write(buf)
        except Exception as e:
            # on exception reset wbuf so it doesn't contain a partial function call
            self.__wbuf = BufferIO()
            raise e
     
    def flush(self):
        self.resetConsumedMessageSize(-1)
        out = self.__wbuf.getvalue()
        # reset wbuf before write/flush to preserve state on underlying failure
        self.__wbuf = BufferIO()
        self.__trans.write(out)
        self.__trans.flush()

class TBufferedTransportFactory(object):
    """Factory transport that builds buffered transports"""

    def getTransport(self, trans, config):
        buffered = TBufferedTransport(trans,config)
        return buffered
