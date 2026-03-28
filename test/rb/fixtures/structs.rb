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

require 'thrift'

module Fixtures
  module Structs
    class OneBool
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::BOOL, :name => 'bool'}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    class OneByte
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::BYTE, :name => 'byte'}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    class OneI16
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::I16, :name => 'i16'}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    class OneI32
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::I32, :name => 'i32'}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    class OneI64
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::I64, :name => 'i64'}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    class OneDouble
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::DOUBLE, :name => 'double'}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    class OneString
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::STRING, :name => 'string'}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    class OneMap
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::MAP, :name => 'map', :key => {:type => Thrift::Types::STRING}, :value => {:type => Thrift::Types::STRING}}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    class NestedMap
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        0 => {:type => Thrift::Types::MAP, :name => 'map', :key => {:type => Thrift::Types::I32}, :value => {:type => Thrift::Types::MAP, :key => {:type => Thrift::Types::I32}, :value => {:type => Thrift::Types::I32}}}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    class OneList
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::LIST, :name => 'list', :element => {:type => Thrift::Types::STRING}}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    class NestedList
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        0 => {:type => Thrift::Types::LIST, :name => 'list', :element => {:type => Thrift::Types::LIST, :element => { :type => Thrift::Types::I32 } } }
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    class OneSet
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::SET, :name => 'set', :element => {:type => Thrift::Types::STRING}}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    class NestedSet
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::SET, :name => 'set', :element => {:type => Thrift::Types::SET, :element => { :type => Thrift::Types::STRING } }}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    # struct OneOfEach {
    #   1: bool im_true,
    #   2: bool im_false,
    #   3: byte a_bite,
    #   4: i16 integer16,
    #   5: i32 integer32,
    #   6: i64 integer64,
    #   7: double double_precision,
    #   8: string some_characters,
    #   9: string zomg_unicode,
    #   10: bool what_who,
    #   11: binary base64,
    # }
    class OneOfEach
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::BOOL, :name => 'im_true'},
        2 => {:type => Thrift::Types::BOOL, :name => 'im_false'},
        3 => {:type => Thrift::Types::BYTE, :name => 'a_bite'},
        4 => {:type => Thrift::Types::I16, :name => 'integer16'},
        5 => {:type => Thrift::Types::I32, :name => 'integer32'},
        6 => {:type => Thrift::Types::I64, :name => 'integer64'},
        7 => {:type => Thrift::Types::DOUBLE, :name => 'double_precision'},
        8 => {:type => Thrift::Types::STRING, :name => 'some_characters'},
        9 => {:type => Thrift::Types::STRING, :name => 'zomg_unicode'},
        10 => {:type => Thrift::Types::BOOL, :name => 'what_who'},
        11 => {:type => Thrift::Types::STRING, :name => 'base64', :binary => true}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    # struct Nested1 {
    #   1: list<OneOfEach> a_list
    #   2: map<i32, OneOfEach> i32_map
    #   3: map<i64, OneOfEach> i64_map
    #   4: map<double, OneOfEach> dbl_map
    #   5: map<string, OneOfEach> str_map
    # }
    class Nested1
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::LIST, :name => 'a_list', :element => {:type => Thrift::Types::STRUCT, :class => OneOfEach}},
        2 => {:type => Thrift::Types::MAP, :name => 'i32_map', :key => {:type => Thrift::Types::I32}, :value => {:type => Thrift::Types::STRUCT, :class => OneOfEach}},
        3 => {:type => Thrift::Types::MAP, :name => 'i64_map', :key => {:type => Thrift::Types::I64}, :value => {:type => Thrift::Types::STRUCT, :class => OneOfEach}},
        4 => {:type => Thrift::Types::MAP, :name => 'dbl_map', :key => {:type => Thrift::Types::DOUBLE}, :value => {:type => Thrift::Types::STRUCT, :class => OneOfEach}},
        5 => {:type => Thrift::Types::MAP, :name => 'str_map', :key => {:type => Thrift::Types::STRING}, :value => {:type => Thrift::Types::STRUCT, :class => OneOfEach}}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    # struct Nested2 {
    #   1: list<Nested1> a_list
    #   2: map<i32, Nested1> i32_map
    #   3: map<i64, Nested1> i64_map
    #   4: map<double, Nested1> dbl_map
    #   5: map<string, Nested1> str_map
    # }
    class Nested2
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::LIST, :name => 'a_list', :element => {:type => Thrift::Types::STRUCT, :class => Nested1}},
        2 => {:type => Thrift::Types::MAP, :name => 'i32_map', :key => {:type => Thrift::Types::I32}, :value => {:type => Thrift::Types::STRUCT, :class => Nested1}},
        3 => {:type => Thrift::Types::MAP, :name => 'i64_map', :key => {:type => Thrift::Types::I64}, :value => {:type => Thrift::Types::STRUCT, :class => Nested1}},
        4 => {:type => Thrift::Types::MAP, :name => 'dbl_map', :key => {:type => Thrift::Types::DOUBLE}, :value => {:type => Thrift::Types::STRUCT, :class => Nested1}},
        5 => {:type => Thrift::Types::MAP, :name => 'str_map', :key => {:type => Thrift::Types::STRING}, :value => {:type => Thrift::Types::STRUCT, :class => Nested1}}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    # struct Nested3 {
    #   1: list<Nested2> a_list
    #   2: map<i32, Nested2> i32_map
    #   3: map<i64, Nested2> i64_map
    #   4: map<double, Nested2> dbl_map
    #   5: map<string, Nested2> str_map
    # }
    class Nested3
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::LIST, :name => 'a_list', :element => {:type => Thrift::Types::STRUCT, :class => Nested2}},
        2 => {:type => Thrift::Types::MAP, :name => 'i32_map', :key => {:type => Thrift::Types::I32}, :value => {:type => Thrift::Types::STRUCT, :class => Nested2}},
        3 => {:type => Thrift::Types::MAP, :name => 'i64_map', :key => {:type => Thrift::Types::I64}, :value => {:type => Thrift::Types::STRUCT, :class => Nested2}},
        4 => {:type => Thrift::Types::MAP, :name => 'dbl_map', :key => {:type => Thrift::Types::DOUBLE}, :value => {:type => Thrift::Types::STRUCT, :class => Nested2}},
        5 => {:type => Thrift::Types::MAP, :name => 'str_map', :key => {:type => Thrift::Types::STRING}, :value => {:type => Thrift::Types::STRUCT, :class => Nested2}}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end

    # struct Nested4 {
    #   1: list<Nested3> a_list
    #   2: map<i32, Nested3> i32_map
    #   3: map<i64, Nested3> i64_map
    #   4: map<double, Nested3> dbl_map
    #   5: map<string, Nested3> str_map
    # }
    class Nested4
      include Thrift::Struct, Thrift::Struct_Union

      FIELDS = {
        1 => {:type => Thrift::Types::LIST, :name => 'a_list', :element => {:type => Thrift::Types::STRUCT, :class => Nested3}},
        2 => {:type => Thrift::Types::MAP, :name => 'i32_map', :key => {:type => Thrift::Types::I32}, :value => {:type => Thrift::Types::STRUCT, :class => Nested3}},
        3 => {:type => Thrift::Types::MAP, :name => 'i64_map', :key => {:type => Thrift::Types::I64}, :value => {:type => Thrift::Types::STRUCT, :class => Nested3}},
        4 => {:type => Thrift::Types::MAP, :name => 'dbl_map', :key => {:type => Thrift::Types::DOUBLE}, :value => {:type => Thrift::Types::STRUCT, :class => Nested3}},
        5 => {:type => Thrift::Types::MAP, :name => 'str_map', :key => {:type => Thrift::Types::STRING}, :value => {:type => Thrift::Types::STRUCT, :class => Nested3}}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      Thrift::Struct.generate_accessors self
    end
  end
end
