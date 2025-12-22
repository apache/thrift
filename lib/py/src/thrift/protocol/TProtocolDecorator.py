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

from typing import Any

from thrift.protocol.TProtocol import TProtocolBase


class TProtocolDecorator(TProtocolBase):
    """Protocol decorator base class that wraps another protocol."""

    def __new__(cls, protocol: TProtocolBase, *args: Any, **kwargs: Any) -> TProtocolDecorator:
        decorated_cls = type(''.join(['Decorated', protocol.__class__.__name__]),
                             (cls, protocol.__class__),
                             protocol.__dict__)
        return object.__new__(decorated_cls)
