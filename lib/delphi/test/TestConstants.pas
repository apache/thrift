(*
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
 *)

unit TestConstants;

interface

type
  TKnownProtocol = ( prot_Binary,  // default binary protocol
                     prot_JSON     // JSON protocol
                   );
const
  KNOWN_PROTOCOLS : array[TKnownProtocol] of string
                  = ('binary', 'JSON');

  // defaults are: read=false, write=true
  BINARY_STRICT_READ  = FALSE;
  BINARY_STRICT_WRITE = FALSE;

  HUGE_TEST_STRING = 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. ';

implementation

// nothing

end.
