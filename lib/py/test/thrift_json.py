# -*- coding: utf-8 -*-
from thrift import Thrift
from thrift.protocol.TJSONProtocol import TJSONProtocol
from thrift.transport import TTransport

import sys
import unittest

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

    self.assertEqual(protocol.readString(), unicode_text)

if __name__ == '__main__':
  unittest.main()

