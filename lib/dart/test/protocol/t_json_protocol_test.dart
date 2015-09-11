library thrift.test.transport.t_json_protocol_test;

import 'package:test/test.dart';
import 'package:thrift/thrift.dart';

void main() {

  TJsonProtocol protocol;
  TMessage message;

  setUp(() {
    protocol = new TJsonProtocol(new TBufferedTransport());

    message = new TMessage('my message', TMessageType.ONEWAY, 123);
    protocol.writeMessageBegin(message);
  });

  test('Test message', () async {
    protocol.writeMessageEnd();

    await protocol.transport.flush();

    var subject = protocol.readMessageBegin();

    expect(subject.name, message.name);
    expect(subject.type, message.type);
    expect(subject.seqid, message.seqid);
  });

  test('Test struct', () async {
    var struct = new TStruct();

    protocol.writeStructBegin(struct);
    protocol.writeStructEnd();
    protocol.writeMessageEnd();

    await protocol.transport.flush();

    protocol.readMessageBegin();
    var subject = protocol.readStructBegin();

    // struct name is not serialized, see C# version for reference
    expect(subject, isNotNull);
  });

  test('Test field', () async {
    var field = new TField('my field', TType.MAP, 123);

    protocol.writeFieldBegin(field);
    protocol.writeFieldEnd();
    protocol.writeMessageEnd();

    await protocol.transport.flush();

    protocol.readMessageBegin();
    var subject = protocol.readFieldBegin();

    // struct name is not serialized, see C# version for reference
    expect(subject.type, field.type);
    expect(subject.id, field.id);
  });

  test('Test map', () async {
    var map = new TMap(TType.STRING, TType.STRUCT, 123);

    protocol.writeMapBegin(map);
    protocol.writeMapEnd();
    protocol.writeMessageEnd();

    await protocol.transport.flush();

    protocol.readMessageBegin();
    var subject = protocol.readMapBegin();

    // struct name is not serialized, see C# version for reference
    expect(subject.keyType, map.keyType);
    expect(subject.valueType, map.valueType);
    expect(subject.length, map.length);
  });



}
