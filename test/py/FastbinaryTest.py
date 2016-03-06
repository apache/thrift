#!/usr/bin/env python

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

r"""
PYTHONPATH=./gen-py:../../lib/py/build/lib... ./FastbinaryTest.py
"""

# TODO(dreiss): Test error cases.  Check for memory leaks.

import math
import os
import sys
import timeit

from copy import deepcopy
from pprint import pprint

from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

from DebugProtoTest import Srv
from DebugProtoTest.ttypes import Backwards, Bonk, Empty, HolyMoley, OneOfEach, RandomStuff, Wrapper


class TDevNullTransport(TTransport.TTransportBase):
  def __init__(self):
    pass

  def isOpen(self):
    return True

ooe1 = OneOfEach()
ooe1.im_true = True
ooe1.im_false = False
ooe1.a_bite = 0xd6
ooe1.integer16 = 27000
ooe1.integer32 = 1 << 24
ooe1.integer64 = 6000 * 1000 * 1000
ooe1.double_precision = math.pi
ooe1.some_characters = "Debug THIS!"
ooe1.zomg_unicode = u"\xd7\n\a\t"

ooe2 = OneOfEach()
ooe2.integer16 = 16
ooe2.integer32 = 32
ooe2.integer64 = 64
ooe2.double_precision = (math.sqrt(5) + 1) / 2
ooe2.some_characters = ":R (me going \"rrrr\")"
ooe2.zomg_unicode = u"\xd3\x80\xe2\x85\xae\xce\x9d\x20"\
                    u"\xd0\x9d\xce\xbf\xe2\x85\xbf\xd0\xbe"\
                    u"\xc9\xa1\xd0\xb3\xd0\xb0\xcf\x81\xe2\x84\x8e"\
                    u"\x20\xce\x91\x74\x74\xce\xb1\xe2\x85\xbd\xce\xba"\
                    u"\xc7\x83\xe2\x80\xbc"

if sys.version_info[0] == 2 and os.environ.get('THRIFT_TEST_PY_NO_UTF8STRINGS'):
  ooe1.zomg_unicode = ooe1.zomg_unicode.encode('utf8')
  ooe2.zomg_unicode = ooe2.zomg_unicode.encode('utf8')

hm = HolyMoley(**{"big": [], "contain": set(), "bonks": {}})
hm.big.append(ooe1)
hm.big.append(ooe2)
hm.big[0].a_bite = 0x22
hm.big[1].a_bite = 0x22

hm.contain.add(("and a one", "and a two"))
hm.contain.add(("then a one, two", "three!", "FOUR!"))
hm.contain.add(())

hm.bonks["nothing"] = []
hm.bonks["something"] = [
  Bonk(**{"type": 1, "message": "Wait."}),
  Bonk(**{"type": 2, "message": "What?"}),
]
hm.bonks["poe"] = [
  Bonk(**{"type": 3, "message": "quoth"}),
  Bonk(**{"type": 4, "message": "the raven"}),
  Bonk(**{"type": 5, "message": "nevermore"}),
]

rs = RandomStuff()
rs.a = 1
rs.b = 2
rs.c = 3
rs.myintlist = range(20)
rs.maps = {1: Wrapper(**{"foo": Empty()}), 2: Wrapper(**{"foo": Empty()})}
rs.bigint = 124523452435
rs.triple = 3.14

# make sure this splits two buffers in a buffered protocol
rshuge = RandomStuff()
rshuge.myintlist = range(10000)

my_zero = Srv.Janky_result(**{"success": 5})


def check_write(o):
  trans_fast = TTransport.TMemoryBuffer()
  trans_slow = TTransport.TMemoryBuffer()
  prot_fast = TBinaryProtocol.TBinaryProtocolAccelerated(trans_fast)
  prot_slow = TBinaryProtocol.TBinaryProtocol(trans_slow)

  o.write(prot_fast)
  o.write(prot_slow)
  ORIG = trans_slow.getvalue()
  MINE = trans_fast.getvalue()
  if ORIG != MINE:
    print("mine: %s\norig: %s" % (repr(MINE), repr(ORIG)))


def check_read(o):
  prot = TBinaryProtocol.TBinaryProtocol(TTransport.TMemoryBuffer())
  o.write(prot)

  slow_version_binary = prot.trans.getvalue()

  prot = TBinaryProtocol.TBinaryProtocolAccelerated(
      TTransport.TMemoryBuffer(slow_version_binary))
  c = o.__class__()
  c.read(prot)
  if c != o:
    print("copy: ")
    pprint(eval(repr(c)))
    print("orig: ")
    pprint(eval(repr(o)))

  prot = TBinaryProtocol.TBinaryProtocolAccelerated(
      TTransport.TBufferedTransport(
        TTransport.TMemoryBuffer(slow_version_binary)))
  c = o.__class__()
  c.read(prot)
  if c != o:
    print("copy: ")
    pprint(eval(repr(c)))
    print("orig: ")
    pprint(eval(repr(o)))


def do_test():
  check_write(hm)
  check_read(HolyMoley())
  no_set = deepcopy(hm)
  no_set.contain = set()
  check_read(no_set)
  check_write(rs)
  check_read(rs)
  check_write(rshuge)
  check_read(rshuge)
  check_write(my_zero)
  check_read(my_zero)
  check_read(Backwards(**{"first_tag2": 4, "second_tag1": 2}))

  # One case where the serialized form changes, but only superficially.
  o = Backwards(**{"first_tag2": 4, "second_tag1": 2})
  trans_fast = TTransport.TMemoryBuffer()
  trans_slow = TTransport.TMemoryBuffer()
  prot_fast = TBinaryProtocol.TBinaryProtocolAccelerated(trans_fast)
  prot_slow = TBinaryProtocol.TBinaryProtocol(trans_slow)

  o.write(prot_fast)
  o.write(prot_slow)
  ORIG = trans_slow.getvalue()
  MINE = trans_fast.getvalue()
  assert id(ORIG) != id(MINE)

  prot = TBinaryProtocol.TBinaryProtocolAccelerated(TTransport.TMemoryBuffer())
  o.write(prot)
  prot = TBinaryProtocol.TBinaryProtocol(
      TTransport.TMemoryBuffer(prot.trans.getvalue()))
  c = o.__class__()
  c.read(prot)
  if c != o:
    print("copy: ")
    pprint(eval(repr(c)))
    print("orig: ")
    pprint(eval(repr(o)))


def do_benchmark(iters=5000):
  setup = """
from __main__ import hm, rs, TDevNullTransport
from thrift.protocol import TBinaryProtocol
trans = TDevNullTransport()
prot = TBinaryProtocol.TBinaryProtocol%s(trans)
"""

  setup_fast = setup % "Accelerated"
  setup_slow = setup % ""

  print("Starting Benchmarks")

  print("HolyMoley Standard = %f" %
        timeit.Timer('hm.write(prot)', setup_slow).timeit(number=iters))
  print("HolyMoley Acceler. = %f" %
        timeit.Timer('hm.write(prot)', setup_fast).timeit(number=iters))

  print("FastStruct Standard = %f" %
        timeit.Timer('rs.write(prot)', setup_slow).timeit(number=iters))
  print("FastStruct Acceler. = %f" %
        timeit.Timer('rs.write(prot)', setup_fast).timeit(number=iters))

if __name__ == '__main__':
  do_test()
  do_benchmark()
