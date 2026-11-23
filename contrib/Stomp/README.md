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

Sample code for STOMP-based Thrift clients and/or servers.

Although the sample Thrift STOMP Transport is written in 
Delphi/Pascal, it can easily serve as a starting point for 
similar implementations in other languages.

STOMP is a protocol widely supported by many messaging systems,
such as Apache ActiveMQ, RabbitMQ and many others. In particular,
it can be used to communicate with Service-Bus products like Rebus
or NServiceBus, when running against a STOMP-capable MQ system.

A prerequisite for this sample is the Delphi STOMP Adapter written
by Daniele Teti (http://www.danieleteti.it/stomp-client), currently
hosted at Google Code (http://code.google.com/p/delphistompclient).

At the time of writing, the STOMP adapter does not fully support 
binary data. Please check whether this has been fixed, otherwise 
you have to use the JSON protocol (or to fix it on your own).
