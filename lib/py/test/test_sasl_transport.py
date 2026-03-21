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

import os
import sys
import types
import unittest
from unittest.mock import MagicMock, PropertyMock, call

# Register 'thrift' as a package alias for the src directory so that
# tests can run without a build step.  This mirrors setup.py's
# package_dir={'thrift': 'src'} configuration.
_src_dir = os.path.realpath(os.path.join(os.path.dirname(__file__), '..', 'src'))
if 'thrift' not in sys.modules:
    _thrift_pkg = types.ModuleType('thrift')
    _thrift_pkg.__path__ = [_src_dir]
    _thrift_pkg.__package__ = 'thrift'
    sys.modules['thrift'] = _thrift_pkg

# Stub puresasl so TSaslClientTransport can be imported without it installed.
sys.modules.setdefault('puresasl', types.ModuleType('puresasl'))
sys.modules.setdefault('puresasl.client', types.ModuleType('puresasl.client'))

from thrift.transport.TTransport import TSaslClientTransport
from thrift.transport.TTransport import TTransportException


class TSaslClientTransportTest(unittest.TestCase):

    def _make_transport(self, process_side_effect, recv_messages, complete_value=True):
        transport = object.__new__(TSaslClientTransport)

        mock_inner = MagicMock()
        mock_inner.isOpen.return_value = True
        transport.transport = mock_inner

        mock_sasl = MagicMock()
        mock_sasl.mechanism = 'DIGEST-MD5'
        mock_sasl.process.side_effect = process_side_effect
        type(mock_sasl).complete = PropertyMock(return_value=complete_value)
        transport.sasl = mock_sasl

        transport.send_sasl_msg = MagicMock()
        transport.recv_sasl_msg = MagicMock(side_effect=recv_messages)

        return transport, mock_sasl

    def test_open_with_none_initial_response(self):
        transport, mock_sasl = self._make_transport(
            process_side_effect=[None, b'response'],
            recv_messages=[
                (TSaslClientTransport.OK, b'server-challenge'),
                (TSaslClientTransport.COMPLETE, b''),
            ],
        )

        transport.open()

        transport.send_sasl_msg.assert_any_call(
            TSaslClientTransport.START, b'DIGEST-MD5'
        )
        transport.send_sasl_msg.assert_any_call(TSaslClientTransport.OK, b'')
        mock_sasl.process.assert_any_call(b'server-challenge')

    def test_open_with_bytes_initial_response(self):
        transport, mock_sasl = self._make_transport(
            process_side_effect=[b'initial-token'],
            recv_messages=[
                (TSaslClientTransport.COMPLETE, b''),
            ],
        )

        transport.open()

        transport.send_sasl_msg.assert_any_call(
            TSaslClientTransport.OK, b'initial-token'
        )

    def test_open_complete_with_challenge(self):
        transport, mock_sasl = self._make_transport(
            process_side_effect=[b'initial', b'response', None],
            recv_messages=[
                (TSaslClientTransport.OK, b'challenge1'),
                (TSaslClientTransport.COMPLETE, b'rspauth=abc123'),
            ],
        )

        transport.open()

        mock_sasl.process.assert_any_call(b'rspauth=abc123')

    def test_open_complete_without_challenge(self):
        transport, mock_sasl = self._make_transport(
            process_side_effect=[b'initial'],
            recv_messages=[
                (TSaslClientTransport.COMPLETE, b''),
            ],
        )

        transport.open()

        process_calls = mock_sasl.process.call_args_list
        self.assertNotIn(call(b''), process_calls)

    def test_open_bad_status_raises(self):
        transport, mock_sasl = self._make_transport(
            process_side_effect=[b'initial'],
            recv_messages=[
                (0xFF, b'error message'),
            ],
        )

        with self.assertRaises(TTransportException) as ctx:
            transport.open()
        self.assertIn('Bad SASL negotiation status', str(ctx.exception))

    def test_open_incomplete_after_complete_status_raises(self):
        transport, mock_sasl = self._make_transport(
            process_side_effect=[b'initial'],
            recv_messages=[
                (TSaslClientTransport.COMPLETE, b''),
            ],
            complete_value=False,
        )

        with self.assertRaises(TTransportException) as ctx:
            transport.open()
        self.assertIn('erroneously indicated', str(ctx.exception))

    def test_open_process_raises_during_complete(self):
        transport, mock_sasl = self._make_transport(
            process_side_effect=[b'initial', Exception('rspauth verification failed')],
            recv_messages=[
                (TSaslClientTransport.COMPLETE, b'rspauth=bad'),
            ],
        )

        with self.assertRaises(Exception) as ctx:
            transport.open()
        self.assertIn('rspauth verification failed', str(ctx.exception))


if __name__ == '__main__':
    unittest.main()
