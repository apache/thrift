#!/usr/bin/env python

import subprocess
import sys
import os
import signal

serverproc = subprocess.Popen([sys.executable, "TestServer.py"])
try:
    
    ret = subprocess.call([sys.executable, "TestClient.py"])
    if ret != 0:
        raise Exception("subprocess failed")
finally:
    # fixme: should check that server didn't die
    os.kill(serverproc.pid, signal.SIGKILL)
