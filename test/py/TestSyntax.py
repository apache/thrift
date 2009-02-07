#!/usr/bin/env python

import sys, glob
sys.path.insert(0, './gen-py')
sys.path.insert(0, glob.glob('../../lib/py/build/lib.*')[0])

# Just import these generated files to make sure they are syntactically valid
from DebugProtoTest import EmptyService
from DebugProtoTest import Inherited
