import os
import sys


SCRIPT_DIR = os.path.realpath(os.path.dirname(__file__))
ROOT_DIR = os.path.dirname(os.path.dirname(os.path.dirname(SCRIPT_DIR)))

if sys.version_info[0] == 2:
  import glob
  libdir = glob.glob(os.path.join(ROOT_DIR, 'lib', 'py', 'build', 'lib.*'))[0]
  sys.path.insert(0, libdir)
else:
  sys.path.insert(0, os.path.join(ROOT_DIR, 'lib', 'py', 'build', 'lib'))
