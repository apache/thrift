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
 *
 * Contains some contributions under the Thrift Software License.
 * Please see doc/old-thrift-license.txt in the Thrift distribution for
 * details.
 */

namespace js Int64Test

const i64 SMALL_INT64 = 42
const i64 MAX_JS_SAFE_INT64 = 9007199254740991
const i64 MIN_JS_SAFE_INT64 = -9007199254740991
const i64 MAX_JS_SAFE_PLUS_ONE_INT64 = 9007199254740992
const i64 MIN_JS_SAFE_MINUS_ONE_INT64 = -9007199254740992
const i64 MAX_SIGNED_INT64 = 9223372036854775807
const i64 MIN_SIGNED_INT64 = -9223372036854775808

const list<i64> INT64_LIST = [SMALL_INT64, MAX_JS_SAFE_INT64, MIN_JS_SAFE_INT64, MAX_JS_SAFE_PLUS_ONE_INT64, MIN_JS_SAFE_MINUS_ONE_INT64, MAX_SIGNED_INT64, MIN_SIGNED_INT64]

const map<i64, i64> INT64_2_INT64_MAP = {
    SMALL_INT64: SMALL_INT64,
    MAX_JS_SAFE_INT64: MAX_JS_SAFE_INT64,
    MIN_JS_SAFE_INT64: MIN_JS_SAFE_INT64,
    MAX_JS_SAFE_PLUS_ONE_INT64: MAX_JS_SAFE_PLUS_ONE_INT64,
    MIN_JS_SAFE_MINUS_ONE_INT64: MIN_JS_SAFE_MINUS_ONE_INT64,
    MAX_SIGNED_INT64: MAX_SIGNED_INT64,
    MIN_SIGNED_INT64: MIN_SIGNED_INT64
    }
