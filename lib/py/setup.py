#!/usr/bin/env python
#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/

from distutils.core import setup

setup(name = 'Thrift',
      version = '1.0',
      description = 'Thrift Python Libraries',
      author = ['Mark Slee'],
      author_email = ['mcslee@facebook.com'],
      url = 'http://code.facebook.com/thrift',
      packages = ['thrift', 'thrift.protocol', 'thrift.transport', 'thrift.server'],
      package_dir = {'thrift' : 'src'},
      )

