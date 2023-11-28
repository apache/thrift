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

# We are only testing that generated code compiles, no correctness checking is done

struct ExampleRequest {
    1: required string example_field
}

struct ExampleResponse {
    1: required string example_field
}

struct ExampleOptionalRequest {
    1: optional string example_field
}

struct ExampleOptionalResponse {
    1: optional string example_field
}

struct ExampleNested {
    1: required string example_field
}

struct ExampleOptionalNestedRequest {
    1: optional ExampleNested example_field
}

struct ExampleOptionalNestedResponse {
    1: optional ExampleNested example_field
}

typedef ExampleRequest TypedefExampleRequest
typedef ExampleResponse TypedefExampleResponse

typedef string PrimativeTypedefExampleRequest
typedef string PrimativeTypedefExampleResponse

service TypeDefService {
    ExampleResponse exampleMethod(1: ExampleRequest request)
    TypedefExampleResponse typedefExampleMethod(1: TypedefExampleRequest request)
    string primativeExampleMethod(1: string request)
    PrimativeTypedefExampleResponse primativeTypedefExampleMethod(1: PrimativeTypedefExampleRequest request)
    ExampleOptionalResponse exampleOptionalMethod(1: ExampleOptionalRequest request)
    ExampleOptionalNestedResponse exampleOptionalNestedMethod(1: ExampleOptionalNestedRequest request)
}
