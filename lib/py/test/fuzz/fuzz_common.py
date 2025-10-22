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

import glob
import sys
import os
import atheris

def setup_thrift_imports():
    """Set up the Python path to include Thrift libraries and generated code."""

    # For oss-fuzz, we need to package it using pyinstaller and set up paths properly
    if getattr(sys, 'frozen', False) and hasattr(sys, '_MEIPASS'):
        print('running in a PyInstaller bundle')
        sys.path.insert(0, "thrift_lib")
        sys.path.insert(0, "gen-py")
    else:
        print('running in a normal Python process')
        SCRIPT_DIR = os.path.realpath(os.path.dirname(__file__))
        ROOT_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(SCRIPT_DIR))))

        for libpath in glob.glob(os.path.join(ROOT_DIR, 'lib', 'py', 'build', 'lib.*')):
            for pattern in ('-%d.%d', '-%d%d'):
                postfix = pattern % (sys.version_info[0], sys.version_info[1])
                if libpath.endswith(postfix):
                    sys.path.insert(0, libpath)
                    break

        gen_path = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "..", "gen-py"
        )
        sys.path.append(gen_path)
    print(sys.path)

setup_thrift_imports()

from thrift.transport import TTransport
from thrift.TSerialization import serialize, deserialize
from fuzz.ttypes import FuzzTest

def create_parser_fuzzer(protocol_factory_class):
    """
    Create a parser fuzzer function for a specific protocol.
    
    Args:
        protocol_factory_class: The Thrift protocol factory class to use
    
    Returns:
        A function that can be used with atheris.Setup()
    """
    def TestOneInput(data):
        if len(data) < 2:
            return

        try:
            # Create a memory buffer with the fuzzed data
            buf = TTransport.TMemoryBuffer(data)
            transport = TTransport.TBufferedTransportFactory().getTransport(buf)
            factory = protocol_factory_class(string_length_limit=1000, container_length_limit=1000)

            # Try to deserialize the fuzzed data into the test class
            test_instance = deserialize(FuzzTest(), data, factory)

        except Exception as e:
            # We expect various exceptions during fuzzing
            pass

    return TestOneInput

def create_roundtrip_fuzzer(protocol_factory_class):
    """
    Create a roundtrip fuzzer function for a specific protocol.
    
    Args:
        protocol_factory_class: The Thrift protocol factory class to use
    
    Returns:
        A function that can be used with atheris.Setup()
    """
    def TestOneInput(data):
        if len(data) < 2:
            return

        try:
            # Create a memory buffer with the fuzzed data
            buf = TTransport.TMemoryBuffer(data)
            transport = TTransport.TBufferedTransportFactory().getTransport(buf)
            factory = protocol_factory_class(string_length_limit=1000, container_length_limit=1000)

            # Try to deserialize the fuzzed data into the test class
            test_instance = deserialize(FuzzTest(), data, factory)
            # If deserialization succeeds, try to serialize it back
            serialized = serialize(test_instance, factory)
            # Deserialize again
            deserialized = deserialize(FuzzTest(), serialized, factory)
            # Verify the objects are equal after a second deserialization
            assert test_instance == deserialized

        except AssertionError as e:
            raise e
        except Exception as e:
            # We expect various exceptions during fuzzing
            pass

    return TestOneInput

def _run_fuzzer(fuzzer_function):
    """
    Set up and run the fuzzer for a specific protocol.
    
    Args:
        fuzzer_function: The fuzzer function to use
    """
    setup_thrift_imports()
    atheris.instrument_all()
    atheris.Setup(sys.argv, fuzzer_function, enable_python_coverage=True)
    atheris.Fuzz()


def run_roundtrip_fuzzer(protocol_factory_class):
    """
    Set up and run the fuzzer for a specific protocol.
    
    Args:
        protocol_factory_class: The Thrift protocol factory class to use
    """
    _run_fuzzer(create_roundtrip_fuzzer(protocol_factory_class))


def run_parser_fuzzer(protocol_factory_class):
    """
    Set up and run the fuzzer for a specific protocol.
    
    Args:
        protocol_factory_class: The Thrift protocol factory class to use
    """
    _run_fuzzer(create_parser_fuzzer(protocol_factory_class))