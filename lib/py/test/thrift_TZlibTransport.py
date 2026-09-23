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
from thrift.transport.TTransport import TTransportException


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


class ChunkedTransport(TTransport.TTransportBase):
    """Keeps every write as a chunk of its own and hands the chunks back one
    read at a time, the way a socket returns what the peer has sent so far.
    """

    def __init__(self, chunks=()):
        self.chunks = list(chunks)

    def isOpen(self):
        return True

    def write(self, buf):
        self.chunks.append(bytes(buf))

    def flush(self):
        pass

    def read(self, sz):
        if not self.chunks:
            raise TTransportException(TTransportException.END_OF_FILE, "no more chunks")
        chunk = self.chunks.pop(0)
        if len(chunk) > sz:
            self.chunks.insert(0, chunk[sz:])
            chunk = chunk[:sz]
        return chunk


def zlib_chunks(*payloads):
    """Compress the payloads with one TZlibTransport, flushing after each."""
    trans = ChunkedTransport()
    writer = TZlibTransport.TZlibTransport(trans)
    for payload in payloads:
        writer.write(payload)
        writer.flush()
    return trans.chunks


def read_outcome(trans, sz):
    """Return the number of bytes a read gives, or the error it raises."""
    try:
        return len(trans.read(sz))
    except TTransportException as e:
        return ('TTransportException', e.type)
    except Exception as e:
        return (type(e).__name__, str(e))


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

    def test_decompressed_size_limit_exceeded(self):
        data = b'a' * 4096  # highly compressible
        buff_w = TTransport.TMemoryBuffer()
        writer = TZlibTransport.TZlibTransport(TTransport.TBufferedTransportFactory().getTransport(buff_w))
        writer.write(data)
        writer.flush()

        compressed = buff_w.getvalue()
        buff_r = TTransport.TMemoryBuffer(compressed)
        reader = TZlibTransport.TZlibTransport(
            TTransport.TBufferedTransportFactory().getTransport(buff_r),
            max_decompressed_size=1024,
        )
        with self.assertRaises(TTransportException) as ctx:
            reader.read(4096)
        self.assertEqual(ctx.exception.type, TTransportException.SIZE_LIMIT)

    def test_decompressed_size_limit_not_exceeded(self):
        data = b'hello thrift ' * 10  # 130 bytes
        buff_w = TTransport.TMemoryBuffer()
        writer = TZlibTransport.TZlibTransport(TTransport.TBufferedTransportFactory().getTransport(buff_w))
        writer.write(data)
        writer.flush()

        compressed = buff_w.getvalue()
        buff_r = TTransport.TMemoryBuffer(compressed)
        reader = TZlibTransport.TZlibTransport(
            TTransport.TBufferedTransportFactory().getTransport(buff_r),
            max_decompressed_size=1024,
        )
        result = reader.read(len(data))
        self.assertEqual(result, data)

    def test_whole_decompressed_size_limit_can_be_read_in_parts(self):
        limit = 4096
        parts = [b'a' * 1000, b'b' * 1000, b'c' * 2096]
        reader = TZlibTransport.TZlibTransport(
            ChunkedTransport(zlib_chunks(*parts)),
            max_decompressed_size=limit,
        )
        for part in parts:
            self.assertEqual(reader.readAll(len(part)), part)
        self.assertEqual(reader._bytes_decompressed, limit)

    def test_no_data_is_read_once_the_decompressed_size_limit_is_used_up(self):
        limit = 4096
        more = b'\0' * (1024 * 1024)
        reader = TZlibTransport.TZlibTransport(
            ChunkedTransport(zlib_chunks(b'x' * limit, more, b'y' * 16)),
            max_decompressed_size=limit,
        )
        self.assertEqual(len(reader.readAll(limit)), limit)
        self.assertEqual(reader._bytes_decompressed, limit)

        size_limit = ('TTransportException', TTransportException.SIZE_LIMIT)
        outcomes = [read_outcome(reader, len(more)), read_outcome(reader, 16)]
        self.assertEqual(outcomes, [size_limit, size_limit])
        self.assertEqual(reader._bytes_decompressed, limit)

    def test_max_decompressed_size_is_validated(self):
        for size in (0, -1):
            with self.assertRaises(ValueError):
                TZlibTransport.TZlibTransport(
                    TTransport.TMemoryBuffer(), max_decompressed_size=size)
            with self.assertRaises(ValueError):
                TZlibTransport.TZlibTransportFactory().getTransport(
                    TTransport.TMemoryBuffer(), max_decompressed_size=size)
        # the limit covers a whole session, so it may be larger than any one frame
        for size in (1, 1 << 40):
            TZlibTransport.TZlibTransport(TTransport.TMemoryBuffer(), max_decompressed_size=size)


if __name__ == '__main__':
    unittest.main()
