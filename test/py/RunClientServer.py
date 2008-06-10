#!/usr/bin/env python

import time
import subprocess
import sys
import os
import signal

def relfile(fname):
    return os.path.join(os.path.dirname(__file__), fname)

def runTest(server_class):
    print "Testing ", server_class
    serverproc = subprocess.Popen([sys.executable, relfile("TestServer.py"), server_class])
    try:

        ret = subprocess.call([sys.executable, relfile("TestClient.py")])
        if ret != 0:
            raise Exception("subprocess failed")
    finally:
        # fixme: should check that server didn't die
        os.kill(serverproc.pid, signal.SIGKILL)

    # wait for shutdown
    time.sleep(5)

map(runTest, ["TForkingServer", "TThreadPoolServer",
              "TThreadedServer", "TSimpleServer"])
