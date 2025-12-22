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

from __future__ import annotations

from typing import Any, TYPE_CHECKING, TypeVar

from .protocol import TBinaryProtocol
from .transport import TTransport

if TYPE_CHECKING:
    from thrift.protocol.TProtocol import TProtocolFactory
    from thrift.protocol.TBase import TBase

T = TypeVar('T')


def serialize(
    thrift_object: TBase,
    protocol_factory: TProtocolFactory = TBinaryProtocol.TBinaryProtocolFactory(),
) -> bytes:
    transport = TTransport.TMemoryBuffer()
    protocol = protocol_factory.getProtocol(transport)
    thrift_object.write(protocol)
    return transport.getvalue()


def deserialize(
    base: T,
    buf: bytes,
    protocol_factory: TProtocolFactory = TBinaryProtocol.TBinaryProtocolFactory(),
) -> T:
    transport = TTransport.TMemoryBuffer(buf)
    protocol = protocol_factory.getProtocol(transport)
    base.read(protocol)  # type: ignore[union-attr]
    return base
