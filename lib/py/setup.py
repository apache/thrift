#!/usr/bin/env python

#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#	http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

import platform
import sys
try:
    from setuptools import setup, Extension
except:
    from distutils.core import setup, Extension, Command

from distutils.command.build_ext import build_ext
from distutils.errors import CCompilerError, DistutilsExecError, DistutilsPlatformError

# Fix to build sdist under vagrant
import os
if 'vagrant' in str(os.environ):
    del os.link

include_dirs = []
if sys.platform == 'win32':
    include_dirs.append('compat/win32')
    ext_errors = (CCompilerError, DistutilsExecError, DistutilsPlatformError, IOError)
else:
    ext_errors = (CCompilerError, DistutilsExecError, DistutilsPlatformError)

class BuildFailed(Exception):
    pass

class ve_build_ext(build_ext):
    def run(self):
        try:
            build_ext.run(self)
        except DistutilsPlatformError as x:
            raise BuildFailed()

    def build_extension(self, ext):
        try:
            build_ext.build_extension(self, ext)
        except ext_errors as x:
            raise BuildFailed()

def run_setup(with_binary):
    if with_binary:
        extensions = dict(
            ext_modules = [
                 Extension('thrift.protocol.fastbinary',
                       sources = ['src/protocol/fastbinary.c'],
                    include_dirs = include_dirs,
                )
            ],
            cmdclass=dict(build_ext=ve_build_ext)
        )
    else:
        extensions = dict()

    setup(name = 'thrift',
        version = '1.0.0-dev',
        description = 'Python bindings for the Apache Thrift RPC system',
        author = 'Thrift Developers',
        author_email = 'dev@thrift.apache.org',
        url = 'http://thrift.apache.org',
        license = 'Apache License 2.0',
        install_requires=['six>=1.7.2'],
        packages = [
            'thrift',
            'thrift.protocol',
            'thrift.transport',
            'thrift.server',
        ],
        package_dir = {'thrift' : 'src'},
        classifiers = [
            'Development Status :: 5 - Production/Stable',
            'Environment :: Console',
            'Intended Audience :: Developers',
            'Programming Language :: Python',
            'Programming Language :: Python :: 2',
            'Programming Language :: Python :: 3',
            'Topic :: Software Development :: Libraries',
            'Topic :: System :: Networking'
        ],
        **extensions
    )

try:
    with_binary = False
    # Don't even try to build the C module unless we're on CPython 2.x.
    # TODO: fix it for CPython 3.x
    if platform.python_implementation() == 'CPython' and sys.version_info < (3,):
        with_binary = True
    run_setup(with_binary)
except BuildFailed:
    print()
    print('*' * 80)
    print("An error occurred while trying to compile with the C extension enabled")
    print("Attempting to build without the extension now")
    print('*' * 80)
    print()

    run_setup(False)
