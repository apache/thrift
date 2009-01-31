#!/usr/bin/env python

import time
import subprocess
import sys
import os
import signal

def relfile(fname):
    return os.path.join(os.path.dirname(__file__), fname)

FRAMED = ["TNonblockingServer"]

def runTest(server_class):
    print "Testing ", server_class
    serverproc = subprocess.Popen([sys.executable, relfile("TestServer.py"), server_class])
    time.sleep(0.25)
    try:
        argv = [sys.executable, relfile("TestClient.py")]
        if server_class in FRAMED:
            argv.append('--framed')
        if server_class == 'THttpServer':
            argv.append('--http=/')
        ret = subprocess.call(argv)
        if ret != 0:
            raise Exception("subprocess failed")
    finally:
        # fixme: should check that server didn't die
        os.kill(serverproc.pid, signal.SIGKILL)

    # wait for shutdown
    time.sleep(1)

map(runTest, [
  "TSimpleServer",
  "TThreadedServer",
  "TThreadPoolServer",
  "TForkingServer",
  "TNonblockingServer",
  "THttpServer",
  ])
