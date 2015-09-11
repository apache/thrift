library thrift.test.t_application_error_test;

import 'package:test/test.dart';
import 'package:thrift/thrift.dart';

void main() {
  TProtocol protocol;

  setUp(() {
    protocol = new TBinaryProtocol(new TBufferedTransport());
  });

  test('Write and read an application error', () {
    var expectedType = TApplicationErrorType.INTERNAL_ERROR;
    var expectedMessage = 'test error message';

    TApplicationError error = new TApplicationError(expectedType, expectedMessage);
    error.write(protocol);

    protocol.transport.flush();

    TApplicationError subject = TApplicationError.read(protocol);

    expect(subject, isNotNull);
    expect(subject.type, expectedType);
    expect(subject.message, expectedMessage);
  });

}
