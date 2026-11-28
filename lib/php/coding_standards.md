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

## PHP Coding Standards

Please follow:
 * [Thrift General Coding Standards](/doc/coding_standards.md)
 * [PSR-12](https://www.php-fig.org/psr/psr-12/)

### Naming

In addition to PSR-12, Apache Thrift's PHP code does not use leading or trailing
underscores on identifiers (properties, methods, constants). Earlier versions of
the library carried a Google-C++-style trailing underscore convention on
private/protected members; this is no longer accepted. Use plain camelCase for
properties and methods, with PHP visibility keywords (`private`, `protected`,
`public`) to convey intent.

The phpcs configuration in `phpcs.xml.dist` enforces PSR-12 across
`lib/php/lib/` and `lib/php/test/`.
