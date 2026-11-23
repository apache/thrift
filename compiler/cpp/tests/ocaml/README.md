<!--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements. See the NOTICE file
distributed with this work for additional information
regarding copyright ownership. The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied. See the License for the
specific language governing permissions and limitations
under the License.
-->

## Testing approach

1. Programmatically construct parsed instances of Thrift IDLs using internal
   types
2. Generate the OCaml output using the OCaml generator
3. Capture the generated output in `ostringstream`
4. Query and compare the outputs in the strings to stored snapshots in the
   `snapshot_*.cc` files

Run tests in `../tests` directory:

      # Only on changing build definition:
      cmake -DCMAKE_PREFIX_PATH=/usr/local/opt/bison -DCMAKE_CXX_STANDARD=11 .

      # On each iteration:
      rm -rf gen-ocaml; cmake --build . && ctest --output-on-failure
