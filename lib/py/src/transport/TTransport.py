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

from struct import pack, unpack
from thrift.Thrift import TException
from thrift.compat import BufferIO
from thrift.TConfiguration import TConfiguration

class TTransportException(TException):
    """Custom Transport Exception class"""

    UNKNOWN = 0
    NOT_OPEN = 1
    ALREADY_OPEN = 2
    TIMED_OUT = 3
    END_OF_FILE = 4
    NEGATIVE_SIZE = 5
    SIZE_LIMIT = 6
    INVALID_CLIENT_TYPE = 7

    def __init__(self, type=UNKNOWN, message=None, inner=None):
        TException.__init__(self, message)
        self.type = type
        self.inner = inner


class TTransportBase(object):
    """Base class for Thrift transport layer."""

    def isOpen(self):
        pass

    def open(self):
        pass

    def close(self):
        pass

    def read(self, sz):
        pass

    def readAll(self, sz):
        buff = b''
        have = 0
        while (have < sz):
            chunk = self.read(sz - have)
            chunkLen = len(chunk)
            have += chunkLen
            buff += chunk

            if chunkLen == 0:
                raise EOFError()

        return buff

    def write(self, buf):
        pass

    def flush(self):
        pass

    def getConfiguration(self):
        pass

    def updateKnownMessageSize(self, size):
        pass

    def checkReadBytesAvailable(self, numBytes):
        pass


# This class should be thought of as an interface.
class CReadableTransport(object):
    """base class for transports that are readable from C"""

    # TODO(dreiss): Think about changing this interface to allow us to use
    #               a (Python, not c) StringIO instead, because it allows
    #               you to write after reading.

    # NOTE: This is a classic class, so properties will NOT work
    #       correctly for setting.
    @property
    def cstringio_buf(self):
        """A cStringIO buffer that contains the current chunk we are reading."""
        pass

    def cstringio_refill(self, partialread, reqlen):
        """Refills cstringio_buf.

        Returns the currently used buffer (which can but need not be the same as
        the old cstringio_buf). partialread is what the C code has read from the
        buffer, and should be inserted into the buffer before any more reads.  The
        return value must be a new, not borrowed reference.  Something along the
        lines of self._buf should be fine.

        If reqlen bytes can't be read, throw EOFError.
        """
        pass


class TServerTransportBase(object):
    """Base class for Thrift server transports."""

    def listen(self):
        pass

    def accept(self):
        pass

    def close(self):
        pass

    def getConfiguration(self):
        pass

    def updateKnownMessageSize(self, size):
        pass

    def checkReadBytesAvailable(self, numBytes):
        pass

class TTransportFactoryBase(object):
    """Base class for a Transport Factory"""

    def getTransport(self, trans):
        return trans


class TMemoryBuffer(TTransportBase, CReadableTransport):
    """Wraps a cBytesIO object as a TTransport.

    NOTE: Unlike the C++ version of this class, you cannot write to it
          then immediately read from it.  If you want to read from a
          TMemoryBuffer, you must either pass a string to the constructor.
    TODO(dreiss): Make this work like the C++ version.
    """

    def __init__(self, value=None, offset=0):
        """value -- a value to read from for stringio

        If value is set, this will be a transport for reading,
        otherwise, it is for writing"""

        if value is not None:
            self._buffer = BufferIO(value)
        else:
            self._buffer = BufferIO()
        if offset:
            self._buffer.seek(offset)


    def isOpen(self):
        return not self._buffer.closed

    def open(self):
        pass

    def close(self):
        self._buffer.close()

    def read(self, sz):
        return self._buffer.read(sz)

    def write(self, buf):
        self._buffer.write(buf)

    def flush(self):
        pass

    def getvalue(self):
        return self._buffer.getvalue()

    # Implement the CReadableTransport interface.
    @property
    def cstringio_buf(self):
        return self._buffer

    def cstringio_refill(self, partialread, reqlen):
        # only one shot at reading...
        raise EOFError()

class TFileObjectTransport(TTransportBase):
    """Wraps a file-like object to make it work as a Thrift transport."""
     
    remainingMessageSize = 0
    knownMessageSize = 0

    def __init__(self, fileobj, config):
        self.fileobj = fileobj
        self.config = config
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
            raise Exception(TTransportException.END_OF_FILE, "MaxMessageSize reached")

    def countConsumedMessageBytes(self, numBytes):
        if self.remainingMessageSize >= numBytes:
            self.remainingMessageSize -= numBytes
        else:
            self.remainingMessageSize = 0
            raise Exception(TTransportException.END_OF_FILE, "MaxMessageSize reached")

    def isOpen(self):
        return True

    def close(self):
        self.fileobj.close()

    def read(self, sz):
        self.checkReadBytesAvailable(sz)
        return self.fileobj.read(sz)

    def write(self, buf):
        self.fileobj.write(buf)

    def flush(self):
        self.resetConsumedMessageSize(-1)
        self.fileobj.flush()

class TFileObjectTransportFactory(object):
    def getTransport(self, fileobj, config):
        fileObject = TFileObjectTransport(fileobj, config)
        return fileObject


class TSaslClientTransport(TTransportBase, CReadableTransport):
    """
    SASL transport
    """

    START = 1
    OK = 2
    BAD = 3
    ERROR = 4
    COMPLETE = 5
    remainingMessageSize = 0
    knownMessageSize =0

    def __init__(self, transport, host, service, config, mechanism='GSSAPI',
                 **sasl_kwargs):
        """
        transport: an underlying transport to use, typically just a TSocket
        host: the name of the server, from a SASL perspective
        service: the name of the server's service, from a SASL perspective
        mechanism: the name of the preferred mechanism to use

        All other kwargs will be passed to the puresasl.client.SASLClient
        constructor.
        """

        from puresasl.client import SASLClient
        self.config = config
        self.transport = transport
        self.sasl = SASLClient(host, service, mechanism, **sasl_kwargs)

        self.__wbuf = BufferIO()
        self.__rbuf = BufferIO(b'')


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

    def open(self):
        if not self.transport.isOpen():
            self.transport.open()

        self.send_sasl_msg(self.START, bytes(self.sasl.mechanism, 'ascii'))
        self.send_sasl_msg(self.OK, self.sasl.process())

        while True:
            status, challenge = self.recv_sasl_msg()
            if status == self.OK:
                self.send_sasl_msg(self.OK, self.sasl.process(challenge))
            elif status == self.COMPLETE:
                if not self.sasl.complete:
                    raise TTransportException(
                        TTransportException.NOT_OPEN,
                        "The server erroneously indicated "
                        "that SASL negotiation was complete")
                else:
                    break
            else:
                raise TTransportException(
                    TTransportException.NOT_OPEN,
                    "Bad SASL negotiation status: %d (%s)"
                    % (status, challenge))

    def send_sasl_msg(self, status, body):
        header = pack(">BI", status, len(body))
        self.transport.write(header + body)
        self.transport.flush()

    def recv_sasl_msg(self):
        header = self.transport.readAll(5)
        status, length = unpack(">BI", header)
        if length > 0:
            payload = self.transport.readAll(length)
        else:
            payload = ""
        return status, payload

    def write(self, data):
        self.__wbuf.write(data)

    def flush(self):
        self.resetConsumedMessageSize(-1)
        data = self.__wbuf.getvalue()
        encoded = self.sasl.wrap(data)
        self.transport.write(pack("!i", len(encoded)) + encoded)
        self.transport.flush()
        self.__wbuf = BufferIO()

    def read(self, sz):
        self.checkReadBytesAvailable(sz)
        ret = self.__rbuf.read(sz)
        if len(ret) != 0:
            return ret

        self._read_frame()
        return self.__rbuf.read(sz)

    def _read_frame(self):
        header = self.transport.readAll(4)
        length, = unpack('!i', header)
        encoded = self.transport.readAll(length)
        self.__rbuf = BufferIO(self.sasl.unwrap(encoded))

    def close(self):
        self.sasl.dispose()
        self.transport.close()

    # based on TFramedTransport
    @property
    def cstringio_buf(self):
        return self.__rbuf

    def cstringio_refill(self, prefix, reqlen):
        # self.__rbuf will already be empty here because fastbinary doesn't
        # ask for a refill until the previous buffer is empty.  Therefore,
        # we can start reading new frames immediately.
        while len(prefix) < reqlen:
            self._read_frame()
            prefix += self.__rbuf.getvalue()
        self.__rbuf = BufferIO(prefix)
        return self.__rbuf
