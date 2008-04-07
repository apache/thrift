#!/usr/bin/env python

import subprocess
import sys
import os
import signal

def relfile(fname):
    return os.path.join(os.path.dirname(__file__), fname)

serverproc = subprocess.Popen([sys.executable, relfile("TestServer.py")])
try:

    ret = subprocess.call([sys.executable, relfile("TestClient.py")])
    if ret != 0:
        raise Exception("subprocess failed")
finally:
    # fixme: should check that server didn't die
    os.kill(serverproc.pid, signal.SIGKILL)
