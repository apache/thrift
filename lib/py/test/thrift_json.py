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

import sys
import unittest

import _import_local_thrift  # noqa
from thrift.protocol.TJSONProtocol import TJSONProtocol
from thrift.transport import TTransport

#
# In order to run the test under Windows. We need to create symbolic link
# name 'thrift' to '../src' folder by using:
#
# mklink /D thrift ..\src
#


class TestJSONString(unittest.TestCase):

    def test_escaped_unicode_string(self):
        unicode_json = b'"hello \\u0e01\\u0e02\\u0e03\\ud835\\udcab\\udb40\\udc70 unicode"'
        unicode_text = u'hello \u0e01\u0e02\u0e03\U0001D4AB\U000E0070 unicode'

        buf = TTransport.TMemoryBuffer(unicode_json)
        transport = TTransport.TBufferedTransportFactory().getTransport(buf)
        protocol = TJSONProtocol(transport)

        if sys.version_info[0] == 2:
            unicode_text = unicode_text.encode('utf8')
        self.assertEqual(protocol.readString(), unicode_text)

    def test_TJSONProtocol_write(self):
        write_data = '{"software":"thrift","1":[23,1.2010000000000001,32767,2147483647,9223372036854775807],"base64":"aGVsbG8gdGhyaWZ0","bool":0}'

        buff = TTransport.TMemoryBuffer()
        transport = TTransport.TBufferedTransportFactory().getTransport(buff)
        protocol = TJSONProtocol(transport)
        protocol.writeJSONObjectStart()
        protocol.writeJSONString("software")
        protocol.writeJSONString("thrift")
        protocol.writeJSONString("1")
        protocol.writeJSONArrayStart()
        protocol.writeJSONNumber(23)
        protocol.writeDouble(1.201)
        protocol.writeI16(32767)
        protocol.writeI32(2147483647)
        protocol.writeI64(9223372036854775807)
        protocol.writeJSONArrayEnd()
        protocol.writeJSONString("base64")
        protocol.writeJSONBase64("hello thrift".encode('utf-8'))
        protocol.writeJSONString("bool")
        protocol.writeBool(0)
        protocol.writeJSONObjectEnd()

        transport.flush()
        value = buff.getvalue()

        self.assertEqual(write_data, value.decode('utf-8'))

    def test_TJSONProtol_read(self):
        expected = "{'software':'thrift','1':[23,1.2010000000000001,32767,2147483647,9223372036854775807],'base64':'hello thrift','bool':False}"
        read_data = '{"software":"thrift","1":[23,1.2010000000000001,32767,2147483647,9223372036854775807],"base64":"aGVsbG8gdGhyaWZ0","bool":0}'

        buff = TTransport.TMemoryBuffer(read_data.encode('utf-8'))
        transport = TTransport.TBufferedTransportFactory().getTransport(buff)
        protocol = TJSONProtocol(transport)
        protocol.readJSONObjectStart()
        u_1 = protocol.readString()
        u_2 = protocol.readString()
        u_3 = protocol.readString()
        protocol.readJSONArrayStart()
        u_4 = protocol.readNumber()
        u_5 = protocol.readDouble()
        u_6 = protocol.readI16()
        u_7 = protocol.readI32()
        u_8 = protocol.readI64()
        protocol.readJSONArrayEnd()
        u_9 = protocol.readString()
        u_10 = protocol.readJSONBase64()
        u_11 = protocol.readString()
        u_12 = protocol.readBool()
        protocol.writeJSONObjectEnd()

        result_read = {}
        result_read[u_1] = u_2
        result_read[u_3] = []
        result_read[u_3].append(u_4)
        result_read[u_3].append(u_5)
        result_read[u_3].append(u_6)
        result_read[u_3].append(u_7)
        result_read[u_3].append(u_8)
        result_read[u_9] = u_10.decode('utf-8')
        result_read[u_11] = u_12

        self.assertEqual(eval(expected), result_read)


if __name__ == '__main__':
    unittest.main()
