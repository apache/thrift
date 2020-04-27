
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

from thrift.protocol import TCompactProtocol
from thrift.transport import TTransport
import unittest

CLEAR=0
FIELD_WRITE=1
VALUE_WRITE=2
CONTAINER_WRITE=3
BOOL_WRITE=4
FIELD_READ=5
CONTAINER_READ=6
VALUE_READ=7
BOOL_READ=8
class data():
    TEST_BOOL=True
    TEST_BYTE=123
    TEST_I16=12345
    TEST_I32=1234567890
    TEST_I64=123456789012345
    TEST_DOUBLE=1234567890.123
    TEST_STRING="this is a test string 1234567890!@#$%^&*()"
    TEST_PORT=51199
class TestcompactNaked(unittest.TestCase):
    def __init__(self,*args,**kwargs):
        unittest.TestCase.__init__(self,*args,**kwargs)
    def test_write(self):
        buf=TTransport.TMemoryBuffer()
        transport=TTransport.TBufferedTransportFactory().getTransport(buf)
        protocol=TCompactProtocol.TCompactProtocol(transport)
        protocol.state=CONTAINER_WRITE
        protocol.writeBool(data.TEST_BOOL)
        transport.flush()
        acc=buf.getvalue()
        print(acc)
        self.assertEqual(data.TEST_BOOL,bool(acc))
    def test_read(self):
        buf=TTransport.TMemoryBuffer(bytes(data.TEST_BOOL))
        transport=TTransport.TBufferedTransportFactory().getTransport(buf)
        protocol=TCompactProtocol.TCompactProtocol(transport)
        protocol.state=CONTAINER_READ
        acc=protocol.readBool()
        self.assertNotEqual(bool(bytes(data.TEST_BOOL)),acc)
class TestcompactField(unittest.TestCase):
    def test_write(self):
        buf=TTransport.TMemoryBuffer()
        transport=TTransport.TBufferedTransportFactory().getTransport(buf)
        protocol=TCompactProtocol.TCompactProtocol(transport)
        protocol.writeStructBegin("test_struct")
        protocol.writeFieldBegin("test_field",6,15)
        protocol.writeI16(data.TEST_I16)
        protocol.writeFieldEnd()
        protocol.writeStructEnd()
        transport.flush()
        acc=buf.getvalue()
        print(acc)
        self.assertEqual(acc,b'\xf4\xf2\xc0\x01')
    def test_read(self):
        buf=TTransport.TMemoryBuffer(bytes(data.TEST_BYTE))
        transport=TTransport.TBufferedTransportFactory().getTransport(buf)
        protocol=TCompactProtocol.TCompactProtocol(transport)
        protocol.readStructBegin()
        protocol.readFieldBegin()
        acc=protocol.writeI16
        protocol.state=FIELD_READ
        protocol.readStructEnd()
        protocol.state=VALUE_READ
        protocol.readFieldEnd()
if __name__=='__main__':
    unittest.main()


