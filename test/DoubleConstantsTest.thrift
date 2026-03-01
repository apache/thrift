/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

namespace java thrift.test
namespace cpp thrift.test

// more tests on double constants (precision and type checks)
const double DOUBLE_ASSIGNED_TO_INT_CONSTANT_TEST = 1
const double DOUBLE_ASSIGNED_TO_NEGATIVE_INT_CONSTANT_TEST = -100
const double DOUBLE_ASSIGNED_TO_LARGEST_INT_CONSTANT_TEST = 9223372036854775807
const double DOUBLE_ASSIGNED_TO_SMALLEST_INT_CONSTANT_TEST = -9223372036854775807
const double DOUBLE_ASSIGNED_TO_DOUBLE_WITH_MANY_DECIMALS_TEST = 3.14159265359
const double DOUBLE_ASSIGNED_TO_FRACTIONAL_DOUBLE_TEST = 1000000.1
const double DOUBLE_ASSIGNED_TO_NEGATIVE_FRACTIONAL_DOUBLE_TEST = -1000000.1
const double DOUBLE_ASSIGNED_TO_LARGE_DOUBLE_TEST = 1.7e+308
const double DOUBLE_ASSIGNED_TO_LARGE_FRACTIONAL_DOUBLE_TEST = 9223372036854775816.43
const double DOUBLE_ASSIGNED_TO_SMALL_DOUBLE_TEST = -1.7e+308
const double DOUBLE_ASSIGNED_TO_NEGATIVE_BUT_LARGE_FRACTIONAL_DOUBLE_TEST = -9223372036854775816.43

const list<double> DOUBLE_LIST_TEST = [1,-100,100,9223372036854775807,-9223372036854775807,3.14159265359,1000000.1,-1000000.1,1.7e+308,-1.7e+308,9223372036854775816.43,-9223372036854775816.43]
