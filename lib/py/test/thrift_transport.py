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

import unittest
import os

import _import_local_thrift  # noqa
from thrift.transport import TTransport


class TestTFileObjectTransport(unittest.TestCase):

    def test_TFileObjectTransport(self):
        test_dir = os.path.dirname(os.path.abspath(__file__))
        datatxt_path = os.path.join(test_dir, 'data.txt')
        buffer = '{"soft":"thrift","version":0.13,"1":true}'
        with open(datatxt_path, "w+") as f:
            buf = TTransport.TFileObjectTransport(f)
            buf.write(buffer)
            buf.flush()
            buf.close()

        with open(datatxt_path, "rb") as f:
            buf = TTransport.TFileObjectTransport(f)
            value = buf.read(len(buffer)).decode('utf-8')
            self.assertEqual(buffer, value)
            buf.close()
        os.remove(datatxt_path)


class TestMemoryBuffer(unittest.TestCase):

    def test_memorybuffer_write(self):
        data = '{"1":[1,"hello"],"a":{"A":"abc"},"bool":true,"num":12345}'

        buffer_w = TTransport.TMemoryBuffer()
        buffer_w.write(data.encode('utf-8'))
        value = buffer_w.getvalue()
        self.assertEqual(value.decode('utf-8'), data)
        buffer_w.close()

    def test_memorybuffer_read(self):
        data = '{"1":[1, "hello"],"a":{"A":"abc"},"bool":true,"num":12345}'

        buffer_r = TTransport.TMemoryBuffer(data.encode('utf-8'))
        value_r = buffer_r.read(len(data))
        value = buffer_r.getvalue()
        self.assertEqual(value.decode('utf-8'), data)
        self.assertEqual(value_r.decode('utf-8'), data)
        buffer_r.close()


if __name__ == '__main__':
    unittest.main()
