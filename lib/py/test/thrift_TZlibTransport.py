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
import random
import string

import _import_local_thrift  # noqa
from thrift.transport import TTransport
from thrift.transport import TZlibTransport


def generate_random_buff():
    data = []
    buf_len = 1024 * 32
    index = 0

    while index < buf_len:
        run_len = random.randint(1, 64)
        if index + run_len > buf_len:
            run_len = buf_len - index
        for i in range(run_len):
            data.extend(random.sample(string.printable, 1))
        index += 1

    new_data = ''.join(data)
    return new_data


class TestTZlibTransport(unittest.TestCase):

    def test_write_then_read(self):
        buff = TTransport.TMemoryBuffer()
        trans = TTransport.TBufferedTransportFactory().getTransport(buff)
        zlib_trans = TZlibTransport.TZlibTransport(trans)
        data_w = generate_random_buff()
        zlib_trans.write(data_w.encode('utf-8'))
        zlib_trans.flush()

        value = buff.getvalue()
        zlib_trans.close()

        buff = TTransport.TMemoryBuffer(value)
        trans = TTransport.TBufferedTransportFactory().getTransport(buff)
        zlib_trans = TZlibTransport.TZlibTransport(trans)
        data_r = zlib_trans.read(len(data_w))
        zlib_trans.close()

        try:
            self.assertEqual(data_w, data_r.decode('utf-8'))
            self.assertEqual(len(data_w), len(data_r.decode('utf-8')))
        except AssertionError:
            raise

    def test_after_flushd_write_then_read(self):
        buff = TTransport.TMemoryBuffer()
        trans = TTransport.TBufferedTransportFactory().getTransport(buff)
        zlib_trans = TZlibTransport.TZlibTransport(trans)
        data_w_1 = "hello thrift !@#" * 50
        zlib_trans.write(data_w_1.encode('utf-8'))
        zlib_trans.flush()
        data_w_2 = "{'name': 'thrift', 1: ['abcd' , 233, ('a','c')]}" * 20
        zlib_trans.write(data_w_2.encode('utf-8'))
        zlib_trans.flush()

        value = buff.getvalue()
        zlib_trans.close()

        buff = TTransport.TMemoryBuffer(value)
        trans = TTransport.TBufferedTransportFactory().getTransport(buff)
        zlib_trans = TZlibTransport.TZlibTransport(trans)
        data_r = zlib_trans.read(len(data_w_1) + len(data_w_2))
        zlib_trans.close()

        try:
            self.assertEqual(data_w_1 + data_w_2, data_r.decode('utf-8'))
            self.assertEqual(len(data_w_1) + len(data_w_2), len(data_r.decode('utf-8')))
        except AssertionError:
            raise


if __name__ == '__main__':
    unittest.main()
