import errno
import unittest

from test_sslsocket import ServerAcceptor

import _import_local_thrift  # noqa

from thrift.transport.TSocket import TServerSocket
from thrift.transport.TSocket import TSocket
from thrift.transport.TTransport import TTransportException


class TSocketTest(unittest.TestCase):
    def test_socket_readtimeout_exception(self):
        acc = ServerAcceptor(TServerSocket(port=0))
        acc.start()

        sock = TSocket(host="localhost", port=acc.port)
        sock.open()
        sock.setTimeout(1)
        sock.write(b"sleep")

        with self.assertRaises(TTransportException) as ctx:
            sock.read(5)
        exc = ctx.exception
        self.assertEqual(exc.message, "read timeout")

        acc.client.close() # this also blocks until the other thread is done
        acc.close()
        sock.close()

    def test_isOpen_checks_for_readability(self):
        # https://docs.python.org/3/library/socket.html#notes-on-socket-timeouts
        # https://docs.python.org/3/library/socket.html#socket.socket.settimeout
        timeouts = [
            None,  # blocking mode
            0,  # non-blocking mode
            1.0,  # timeout mode
        ]

        for timeout in timeouts:
            acc = ServerAcceptor(TServerSocket(port=0))
            acc.start()

            sock = TSocket(host="localhost", port=acc.port)
            self.assertFalse(sock.isOpen())
            sock.open()
            sock.setTimeout(timeout)

            # the socket shows as open immediately after connecting
            self.assertTrue(sock.isOpen())

            # and remains open during usage
            sock.write(b"hello")
            self.assertTrue(sock.isOpen())
            while True:
                try:
                    sock.read(5)
                except TTransportException as exc:
                    if exc.inner.errno == errno.EAGAIN:
                        # try again when we're in non-blocking mode
                        continue
                    raise
                break
            self.assertTrue(sock.isOpen())

            # once the server side closes, it no longer shows open
            acc.client.close()  # this also blocks until the other thread is done
            acc.close()

            self.assertIsNotNone(sock.handle)
            self.assertFalse(sock.isOpen())
            # after isOpen() returned False the socket should be closed (THRIFT-5813)
            self.assertIsNone(sock.handle)


if __name__ == "__main__":
    unittest.main()
