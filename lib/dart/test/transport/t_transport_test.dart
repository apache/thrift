// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

library thrift.test.transport.t_socket_transport_test;

import 'package:test/test.dart';
import 'package:thrift/thrift.dart';

/// Common transport tests
void main() {
  group('TTransportFactory', () {
    test('transport is returned from base factory', () async {
      TTransport? result;
      TTransport? transport;

      var factory = TTransportFactory();

      // Test with null transport
      try {
        result = await factory.getTransport(transport!);
        // If getTransport doesn't throw, we assert that result should be null.
        expect(result, isNull,
            reason: 'Expected result to be null when transport is null');
      } catch (e) {
        // If getTransport throws, we catch the exception and fail the test, or handle it accordingly.
        expect(e, isA<TypeError>(),
            reason: 'Expected NoSuchMethodError when transport is null');
      }

      // Test with non-null transport
      transport = TBufferedTransport()..open();
      result = await factory.getTransport(transport);
      expect(result, transport,
          reason:
              'Expected result to be the transport instance when transport is non-null');
    });
  });
}
