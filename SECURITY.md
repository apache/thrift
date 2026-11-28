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

# Security Policy

## Reporting a Vulnerability

Apache Thrift follows the
[Apache Software Foundation vulnerability handling process](https://www.apache.org/security/).

**Do not report security vulnerabilities through public GitHub issues, pull requests, or
discussion threads.**

Send a report to **[security@apache.org](mailto:security@apache.org)** with:

- Apache Thrift version(s) affected
- Language binding(s) affected
- A clear description of the issue and its potential impact
- Reproduction steps or a minimal proof-of-concept (where safe to include)

The Apache Security Team will acknowledge receipt within a few days and will work with the
project's security team to assess and remediate the issue before coordinating public
disclosure.

## Threat Model

The project maintains a threat model document at
[`doc/thrift-threat-model.md`](doc/thrift-threat-model.md).
It describes the attack surface, trust boundaries, transport-level security properties,
and known design trade-offs for all supported language bindings.

## Past Advisories

Past security advisories are published on the
[Apache Thrift security page](https://thrift.apache.org/security) and on the
[ASF security advisories page](https://www.apache.org/security/projects.html).
