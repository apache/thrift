#!/usr/bin/env python3

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

import asyncio
import sys
import glob
sys.path.append('gen-py.asyncio')
sys.path.insert(0, glob.glob('../../lib/py/build/lib*')[0])

from tutorial import Calculator
from tutorial.ttypes import InvalidOperation, Operation, Work

from thrift import Thrift
from thrift.TAsyncio import TAsyncioBufferedTransport, TAsyncioBinaryProtocol


@asyncio.coroutine
def main():
    # Initialize transport with buffered asyncio reader and writer
    transport = yield from TAsyncioBufferedTransport.connect('localhost', 9090)

    # Wrap in a protocol
    protocol = TAsyncioBinaryProtocol(transport)

    # Create a client to use the protocol encoder
    client = Calculator.Client(protocol)

    yield from client.ping()
    print('ping()')

    sum_ = yield from client.add(1, 1)
    print('1+1=%d' % sum_)

    work = Work()

    work.op = Operation.DIVIDE
    work.num1 = 1
    work.num2 = 0

    try:
        quotient = yield from client.calculate(1, work)
        print('Whoa? You know how to divide by zero?')
        print('FYI the answer is %d' % quotient)
    except InvalidOperation as e:
        print('InvalidOperation: %r' % e)

    work.op = Operation.SUBTRACT
    work.num1 = 15
    work.num2 = 10

    diff = yield from client.calculate(1, work)
    print('15-10=%d' % diff)

    log = yield from client.getStruct(1)
    print('Check log: %s' % log.value)

    # Close! - sync op
    transport.close()

if __name__ == '__main__':
    try:
        asyncio.get_event_loop().run_until_complete(main())
    except Thrift.TException as tx:
        print('%s' % tx.message)
