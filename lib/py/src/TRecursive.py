
# MODIFIED June 20, 2017, Eric Conner
#    - 
#
# Original source copyright 2014-present Facebook, Inc.
#
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

from .Thrift import TType

def fix_spec(all_structs):
    for s in all_structs:
        spec = s.thrift_spec

        # Format of thrift_spec is:
        # (<field_key>, <field_type>, <field_name>, <field_spec_args>, <default_value>)
        # The spec can be recursive via spec_args. For example:
        # (4, TType.LIST, 'struct_list', (TType.STRUCT, (RandomStuff, None), False), None, ),  # 4
        if spec is None:
            continue
        list_spec = list(spec)

        for idx, cur_type in enumerate(list_spec):
            if cur_type is None:
                continue

            list_type = list(cur_type)

            if isinstance(list_type[3], tuple):
                list_type[3] = list(list_type[3])

            if list_type[1] == TType.STRUCT:
                list_type[3][1] = list_type[3][0].thrift_spec
            elif list_type[1] in (TType.LIST, TType.SET):
                _fix_list_or_set(list_type[3])
            elif list_type[1] == TType.MAP:
                _fix_map(list_type[3])

            if isinstance(list_type[3], list):
                list_type[3] = tuple(list_type[3])

            list_type = tuple(list_type)
            list_spec[idx] = list_type

        s.thrift_spec = tuple(list_spec)


def _fix_list_or_set(element_type):
    if isinstance(element_type[1], tuple):
        element_type[1] = list(element_type[1])

    if element_type[0] == TType.STRUCT:
        element_type[1][1] = element_type[1][0].thrift_spec
    elif element_type[0] in (TType.LIST, TType.SET):
        _fix_list_or_set(element_type[1])
    elif element_type[0] == TType.MAP:
        _fix_map(element_type[1])

    if isinstance(element_type[1], list):
        element_type[1] = tuple(element_type[1])

def _fix_map(element_type):
    if isinstance(element_type[1], tuple):
        element_type[1] = list(element_type[1])

    if element_type[0] == TType.STRUCT:
        element_type[1][1] = element_type[1][0].thrift_spec
    elif element_type[0] in (TType.LIST, TType.SET):
        _fix_list_or_set(element_type[1])
    elif element_type[0] == TType.MAP:
        _fix_map(element_type[1])

    if isinstance(element_type[1], list):
        element_type[1] = tuple(element_type[1])

    if isinstance(element_type[3], tuple):
        element_type[3] = list(element_type[3])

    if element_type[2] == TType.STRUCT:
        element_type[3][1] = element_type[3][0].thrift_spec
    elif element_type[2] in (TType.LIST, TType.SET):
        _fix_list_or_set(element_type[3])
    elif element_type[2] == TType.MAP:
        _fix_map(element_type[3])

    if isinstance(element_type[3], list):
        element_type[3] = tuple(element_type[3])
