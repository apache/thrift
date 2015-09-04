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

/// Socket implementation of [TTransport].
///
/// Adapted from the JS WebSocket transport.
abstract class TAsyncTransport extends TTransport {

  final List<int> _sendBuffer = [];
  Iterator<int> _dataIterator;

  int read(List<int> buffer, int offset, int length) {
    if (buffer == null) {
      throw new ArgumentError.notNull("buffer");
    }

    if (offset + length > buffer.length) {
      throw new ArgumentError("The range exceeds the buffer length");
    }

    if (_dataIterator == null || length <= 0) {
      return 0;
    }

    int i = 0;
    while (i < length && _dataIterator.moveNext()) {
      buffer[offset + i] = _dataIterator.current;
      i++;
    }

    // cleanup iterator when we've reached the end
    if (_dataIterator.current == null) {
      _dataIterator = null;
    }

    return i;
  }

  void write(List<int> buffer, int offset, int length) {
    if (buffer == null) {
      throw new ArgumentError.notNull("buffer");
    }

    if (offset + length > buffer.length) {
      throw new ArgumentError("The range exceeds the buffer length");
    }

    _sendBuffer.addAll(buffer.sublist(offset, offset + length));
  }

}
