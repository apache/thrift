/// Licensed to the Apache Software Foundation (ASF) under one
/// or more contributor license agreements. See the NOTICE file
/// distributed with this work for additional information
/// regarding copyright ownership. The ASF licenses this file
/// to you under the Apache License, Version 2.0 (the
/// "License"); you may not use this file except in compliance
/// with the License. You may obtain a copy of the License at
///
/// http://www.apache.org/licenses/LICENSE-2.0
///
/// Unless required by applicable law or agreed to in writing,
/// software distributed under the License is distributed on an
/// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
/// KIND, either express or implied. See the License for the
/// specific language governing permissions and limitations
/// under the License.

part of thrift;

/// Framed [TTransport].
///
/// Adapted from the Java Framed transport.
class TFramedTransport extends TBufferedTransport {
  static const uint32ByteCount = 4;

  final TTransport _transport;

  final Uint8List headerBytes = new Uint8List(uint32ByteCount);

  TFramedTransport(TTransport transport) : _transport = transport {
    if (transport == null) {
      throw new ArgumentError.notNull("transport");
    }
  }

  bool get isOpen => _transport.isOpen;

  void open() {
    super.open();
    _transport.open();
  }

  void close() {
    super.close();
    _transport.close();
  }

  int read(List<int> buffer, int offset, int length) {
    if (hasReadData) {
      int got = super.read(buffer, offset, length);
      if (got > 0) return got;
    }

    _readFrame();

    return super.read(buffer, offset, length);
  }

  void _readFrame() {
    _transport.readAll(headerBytes, 0, uint32ByteCount);
    int size = headerBytes.buffer.asByteData().getUint32(0);

    if (size < 0) {
      throw new TTransportError(
          TTransportErrorType.UNKNOWN, "Read a negative frame size: $size");
    }

    List<int> buffer = new List(size);
    _transport.readAll(buffer, 0, size);
    _setReadBuffer(buffer);
  }

  Future flush() {
    List<int> buffer = _consumeWriteBuffer();
    int length = buffer.length;

    headerBytes.buffer.asByteData().setUint32(0, length);
    _transport.write(headerBytes, 0, uint32ByteCount);
    _transport.write(buffer, 0, length);

    return _transport.flush();
  }
}
