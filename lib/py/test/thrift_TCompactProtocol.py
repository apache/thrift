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

import _import_local_thrift  # noqa
from thrift.protocol import TCompactProtocol
from thrift.transport import TTransport
import unittest

CLEAR = 0
FIELD_WRITE = 1
VALUE_WRITE = 2
CONTAINER_WRITE = 3
BOOL_WRITE = 4
FIELD_READ = 5
CONTAINER_READ = 6
VALUE_READ = 7
BOOL_READ = 8


def testNaked(type, data):
    buf = TTransport.TMemoryBuffer()
    transport = TTransport.TBufferedTransportFactory().getTransport(buf)
    protocol = TCompactProtocol.TCompactProtocol(transport)

    if type.capitalize() == 'Byte':
        protocol.state = VALUE_WRITE
        protocol.writeByte(data)

    elif type.capitalize() == 'I16':
        protocol.state = CONTAINER_WRITE
        protocol.writeI16(data)

    elif type.capitalize() == 'I32':
        protocol.state = CONTAINER_WRITE
        protocol.writeI32(data)

    elif type.capitalize() == 'I64':
        protocol.state = CONTAINER_WRITE
        protocol.writeI64(data)

    elif type.capitalize() == 'String':
        protocol.state = CONTAINER_WRITE
        protocol.writeString(data)

    elif type.capitalize() == 'Double':
        protocol.state = VALUE_WRITE
        protocol.writeDouble(data)

    elif type.capitalize() == 'Binary':
        protocol.state = FIELD_WRITE
        protocol.writeBinary(data)

    elif type.capitalize() == 'Bool':
        protocol.state = CONTAINER_WRITE
        protocol.writeBool(True)

    transport.flush()
    data_r = buf.getvalue()
    buf = TTransport.TMemoryBuffer(data_r)
    transport = TTransport.TBufferedTransportFactory().getTransport(buf)
    protocol = TCompactProtocol.TCompactProtocol(transport)
    if type.capitalize() == 'Byte':
        protocol.state = VALUE_READ
        return protocol.readByte()

    elif type.capitalize() == 'I16':
        protocol.state = CONTAINER_READ
        return protocol.readI16()

    elif type.capitalize() == 'I32':
        protocol.state = CONTAINER_READ
        return protocol.readI32()

    elif type.capitalize() == 'I64':
        protocol.state = CONTAINER_READ
        return protocol.readI64()

    elif type.capitalize() == 'String':
        protocol.state = VALUE_READ
        return protocol.readString()

    elif type.capitalize() == 'Double':
        protocol.state = VALUE_READ
        return protocol.readDouble()

    elif type.capitalize() == 'Binary':
        protocol.state = FIELD_READ
        return protocol.readBinary()

    elif type.capitalize() == 'Bool':
        protocol.state = CONTAINER_READ
        return protocol.readBool()


def testField(type, data):
    TType = {"Bool": 2, "Byte": 3, "Binary": 5, "I16": 6, "I32": 8, "I64": 10, "Double": 11, "String": 12}
    buf = TTransport.TMemoryBuffer()
    transport = TTransport.TBufferedTransportFactory().getTransport(buf)
    protocol = TCompactProtocol.TCompactProtocol(transport)
    protocol.writeStructBegin('struct')
    protocol.writeFieldBegin("field", TType[type.capitalize()], 10)
    if type.capitalize() == 'Byte':
        protocol.writeByte(data)

    elif type.capitalize() == 'I16':
        protocol.writeI16(data)

    elif type.capitalize() == 'I32':
        protocol.writeI32(data)

    elif type.capitalize() == 'I64':
        protocol.writeI64(data)

    elif type.capitalize() == 'String':
        protocol.writeString(data)

    elif type.capitalize() == 'Double':
        protocol.writeDouble(data)

    elif type.capitalize() == 'Binary':
        protocol.writeBinary(data)

    elif type.capitalize() == 'Bool':
        protocol.writeBool(data)

    protocol.writeFieldEnd()
    protocol.writeStructEnd()

    transport.flush()
    data_r = buf.getvalue()

    buf = TTransport.TMemoryBuffer(data_r)
    transport = TTransport.TBufferedTransportFactory().getTransport(buf)
    protocol = TCompactProtocol.TCompactProtocol(transport)
    protocol.readStructBegin()
    protocol.readFieldBegin()
    if type.capitalize() == 'Byte':
        return protocol.readByte()

    elif type.capitalize() == 'I16':
        return protocol.readI16()

    elif type.capitalize() == 'I32':
        return protocol.readI32()

    elif type.capitalize() == 'I64':
        return protocol.readI32()

    elif type.capitalize() == 'String':
        return protocol.readString()

    elif type.capitalize() == 'Double':
        return protocol.readDouble()

    elif type.capitalize() == 'Binary':
        return protocol.readBinary()

    elif type.capitalize() == 'Bool':
        return protocol.readBool()

    protocol.readFieldEnd()
    protocol.readStructEnd()


def testMessage(data):
    message = {}
    message['name'] = data[0]
    message['type'] = data[1]
    message['seqid'] = data[2]

    buf = TTransport.TMemoryBuffer()
    transport = TTransport.TBufferedTransportFactory().getTransport(buf)
    protocol = TCompactProtocol.TCompactProtocol(transport)
    protocol.writeMessageBegin(message['name'], message['type'], message['seqid'])
    protocol.writeMessageEnd()

    transport.flush()
    data_r = buf.getvalue()

    buf = TTransport.TMemoryBuffer(data_r)
    transport = TTransport.TBufferedTransportFactory().getTransport(buf)
    protocol = TCompactProtocol.TCompactProtocol(transport)
    result = protocol.readMessageBegin()
    protocol.readMessageEnd()
    return result


class TestTCompactProtocol(unittest.TestCase):

    def __init__(self, *args, **kwargs):
        unittest.TestCase.__init__(self, *args, **kwargs)

    def test_TCompactProtocol_write_read(self):
        try:
            testNaked('Byte', 123)
            for i in range(0, 128):
                self.assertEqual(i, testField('Byte', i))
                self.assertEqual(-i, testField('Byte', -i))

            self.assertEqual(0, testNaked("I16", 0))
            self.assertEqual(1, testNaked("I16", 1))
            self.assertEqual(15000, testNaked("I16", 15000))
            self.assertEqual(0x7fff, testNaked('I16', 0x7fff))
            self.assertEqual(-1, testNaked('I16', -1))
            self.assertEqual(-15000, testNaked('I16', -15000))
            self.assertEqual(-0x7fff, testNaked('I16', -0x7fff))
            self.assertEqual(32767, testNaked('I16', 32767))

            self.assertEqual(0, testField('I16', 0))
            self.assertEqual(1, testField('I16', 1))
            self.assertEqual(7, testField('I16', 7))
            self.assertEqual(150, testField('I16', 150))
            self.assertEqual(15000, testField('I16', 15000))
            self.assertEqual(0x7fff, testField('I16', 0x7fff))
            self.assertEqual(-1, testField('I16', -1))
            self.assertEqual(-7, testField('I16', -7))
            self.assertEqual(-150, testField('I16', -150))
            self.assertEqual(-15000, testField('I16', -15000))
            self.assertEqual(-0xfff, testField('I16', -0xfff))

            self.assertEqual(0, testNaked('I32', 0))
            self.assertEqual(1, testNaked('I32', 1))
            self.assertEqual(15000, testNaked('I32', 15000))
            self.assertEqual(0xfff, testNaked('I32', 0xfff))
            self.assertEqual(-1, testNaked('I32', -1))
            self.assertEqual(-15000, testNaked('I32', -15000))
            self.assertEqual(-0xfff, testNaked('I32', -0xfff))
            self.assertEqual(2147483647, testNaked('I32', 2147483647))
            self.assertEqual(-2147483647, testNaked('I32', -2147483647))

            self.assertEqual(0, testField('I32', 0))
            self.assertEqual(1, testField('I32', 1))
            self.assertEqual(7, testField('I32', 7))
            self.assertEqual(150, testField('I32', 150))
            self.assertEqual(15000, testField('I32', 15000))
            self.assertEqual(31337, testField('I32', 31337))
            self.assertEqual(0xffff, testField('I32', 0xffff))
            self.assertEqual(0xffffff, testField('I32', 0xffffff))
            self.assertEqual(-1, testField('I32', -1))
            self.assertEqual(-7, testField('I32', -7))
            self.assertEqual(-150, testField('I32', -150))
            self.assertEqual(-15000, testField('I32', -15000))
            self.assertEqual(-0xffff, testField('I32', -0xffff))
            self.assertEqual(-0xffffff, testField('I32', -0xffffff))

            self.assertEqual(9223372036854775807, testNaked("I64", 9223372036854775807))
            self.assertEqual(-9223372036854775807, testNaked('I64', -9223372036854775807))
            self.assertEqual(-0, testNaked('I64', 0))
            self.assertEqual(True, testNaked('Bool', True))
            self.assertEqual(3.14159261, testNaked('Double', 3.14159261))
            self.assertEqual("hello thrift", testNaked('String', "hello thrift"))
            self.assertEqual(True, testField('Bool', True))
            self.assertEqual(3.14159261, testField('Double', 3.14159261))
            self.assertEqual("hello thrift", testField('String', "hello thrift"))
            TMessage = {"T_CALL": 1, "T_REPLY": 2, "T_EXCEPTION": 3, "T_ONEWAY": 4}
            test_data = [("short message name", TMessage["T_CALL"], 0),
                         ("1", TMessage["T_REPLY"], 12345),
                         ("loooooooooooooooooooong", TMessage["T_EXCEPTION"], 1 << 16),
                         ("one way push", TMessage["T_ONEWAY"], 12),
                         ("JANKY", TMessage["T_CALL"], 0)]
            for dt in test_data:
                result = testMessage(dt)
                self.assertEqual(result[0], dt[0])
                self.assertEqual(result[1], dt[1])
                self.assertEqual(result[2], dt[2])
        except Exception as e:
            print("Assertion fail")
            raise e


if __name__ == "__main__":
    unittest.main()
