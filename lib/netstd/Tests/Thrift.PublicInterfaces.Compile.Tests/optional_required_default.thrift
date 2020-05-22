# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

// Testcase for THRIFT-5216 generate DeepCopy methods 

namespace netstd OptReqDefTest
 
enum Distance
{ 
    foo = 0, 
    bar = 1,
    baz = 2
} 

struct RaceDetails 
{ 
	// this is really the max field index used here, intentionally placed at the beginning
    666: required Distance           triplesix
    
    // without default values
    
    1: optional Distance           opt_one
    2: optional double             opt_two
    3: optional i16                opt_three
    4: optional string             opt_four
    5: optional binary             opt_five
    6: optional list<i32>          opt_six
    7: optional set<i64>           opt_seven
    8: optional map<i8,i16>        opt_eight

    11: required Distance          req_one
    12: required double            req_two
    13: required i16               req_three
    14: required string            req_four
    15: required binary            req_five
    16: required list<i32>         req_six
    17: required set<i64>          req_seven
    18: required map<i8,i16>       req_eight
    
    21:          Distance          def_one
    22:          double            def_two
    23:          i16               def_three
    24:          string            def_four
    25:          binary            def_five
    26:          list<i32>         def_six
    27:          set<i64>          def_seven
    28:          map<i8,i16>       def_eight

    // having default values
    
    31: optional Distance          opt_one_with_value   = Distance.bar
    32: optional double            opt_two_with_value   = 2.22
    33: optional i16               opt_three_with_value = 3
    34: optional string            opt_four_with_value  = "four"
    35: optional binary            opt_five_with_value  = "five\t"
    36: optional list<i32>         opt_six_with_value   = [6]
    37: optional set<i64>          opt_seven_with_value = [7]
    38: optional map<i8,i16>       opt_eight_with_value = { 8 : 8 }

    41: required Distance          req_one_with_value     = Distance.bar
    42: required double            req_two_with_value     = 2.22
    43: required i16               req_three_with_value = 3
    44: required string            req_four_with_value     = "four"
    45: required binary            req_five_with_value     = "five"
    46: required list<i32>         req_six_with_value     = [6]
    47: required set<i64>          req_seven_with_value = [7]
    48: required map<i8,i16>       req_eight_with_value = { 8 : 8 }
    
    51:          Distance          def_one_with_value     = Distance.bar
    52:          double            def_two_with_value     = 2.22
    53:          i16               def_three_with_value = 3
    54:          string            def_four_with_value     = "four"
    55:          binary            def_five_with_value     = "five"
    56:          list<i32>         def_six_with_value     = [6]
    57:          set<i64>          def_seven_with_value = [7]
    58:          map<i8,i16>       def_eight_with_value = { 8 : 8 }
    
    90: optional bool              last_of_the_mohicans

	// some more complicated ones, including recursion
	
    300: required list<Distance>            far_list
    301: optional set<Distance>             far_set
    302:          map<Distance,Distance>    far_map

    310: required set<list<Distance>>            far_set_list
    311: optional list<map<i8,set<Distance>>>    far_list_map_set
    312:          map<Distance,RDs>              far_map_dist_to_rds
    
    320: required RaceDetails      req_nested
    321: optional RaceDetails      opt_nested
    322:          RaceDetails      def_nested

    330: required jack      	   req_union
    331: optional jack      	   opt_union
    332:          jack      	   def_union
} 

union jack {
    1: list<RaceDetails2>     stars    
    2: list<RDs>             stripes    

    310: set<list<Distance>>            far_set_list
    311: list<map<i8,set<Distance>>>    far_list_map_set
    312: map<Distance,RDs>              far_map_dist_to_rds
    
    320: jack      					nested_union
    321: RaceDetails      			nested_struct

    401: optional Distance           opt_one
    402: optional double             opt_two
    403: optional i16                opt_three
    404: optional string             opt_four
    405: optional binary             opt_five
    406: optional list<i32>          opt_six
    407: optional set<i64>           opt_seven
    408: optional map<i8,i16>        opt_eight
}

typedef RaceDetails  RaceDetails2
typedef list<RaceDetails>  RDs

exception CrashBoomBang {
    1 : i32 MyErrorCode
}

service foobar {
    set<set<set<Distance>>> DoItNow( 1 : list<list<list<RaceDetails>>> rd, 2: i32 mitDefault = 42) throws (1: CrashBoomBang cbb)
}

