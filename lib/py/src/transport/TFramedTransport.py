from struct import pack, unpack
from thrift.Thrift import TException
from thrift.compat import BufferIO

from thrift.TConfiguration import TConfiguration
from thrift.transport.TTransport import CReadableTransport, TTransportBase, TTransportException


class TFramedTransport(TTransportBase, CReadableTransport):
    """Class that wraps another transport and frames its I/O when writing."""
    remainingMessageSize = 0
    knownMessageSize = 0

    def __init__(self, trans, config):
        self.__trans = trans
        self.config = config
        self.resetConsumedMessageSize(-1)
        self.__rbuf = BufferIO(b'')
        self.__wbuf = BufferIO()

    
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

        self.readFrame()
        return self.__rbuf.read(sz)

    def readFrame(self):
        buff = self.__trans.readAll(4)
        sz, = unpack('!i', buff)
        self.__rbuf = BufferIO(self.__trans.readAll(sz))

    def write(self, buf):
        self.__wbuf.write(buf)

    def flush(self):
        self.resetConsumedMessageSize(-1)
        wout = self.__wbuf.getvalue()
        wsz = len(wout)
        # reset wbuf before write/flush to preserve state on underlying failure
        self.__wbuf = BufferIO()
        # N.B.: Doing this string concatenation is WAY cheaper than making
        # two separate calls to the underlying socket object. Socket writes in
        # Python turn out to be REALLY expensive, but it seems to do a pretty
        # good job of managing string buffer operations without excessive copies
        buf = pack("!i", wsz) + wout
        self.__trans.write(buf)
        self.__trans.flush()

    # Implement the CReadableTransport interface.
    @property
    def cstringio_buf(self):
        return self.__rbuf

    def cstringio_refill(self, prefix, reqlen):
        # self.__rbuf will already be empty here because fastbinary doesn't
        # ask for a refill until the previous buffer is empty.  Therefore,
        # we can start reading new frames immediately.
        while len(prefix) < reqlen:
            self.readFrame()
            prefix += self.__rbuf.getvalue()
        self.__rbuf = BufferIO(prefix)
        return self.__rbuf

class TFramedTransportFactory(object):
    """Factory transport that builds framed transports"""

    def getTransport(self, trans, config):
        framed = TFramedTransport(trans, config)
        return framed
