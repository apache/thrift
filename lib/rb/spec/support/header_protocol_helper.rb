#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

module HeaderProtocolHelper
  def varint32(n)
    bytes = []
    loop do
      if (n & ~0x7f) == 0
        bytes << n
        break
      else
        bytes << ((n & 0x7f) | 0x80)
        n >>= 7
      end
    end
    bytes.pack('C*')
  end

  def build_header_frame(header_data, payload = Thrift::Bytes.empty_byte_buffer, header_words: nil)
    header_data = Thrift::Bytes.force_binary_encoding(header_data)
    if header_words.nil?
      padding = (4 - (header_data.bytesize % 4)) % 4
      header_data += "\x00" * padding
      header_words = header_data.bytesize / 4
    end

    frame_size = 2 + 2 + 4 + 2 + header_data.bytesize + payload.bytesize
    frame = Thrift::Bytes.empty_byte_buffer
    frame << [frame_size].pack('N')
    frame << [Thrift::HeaderTransport::HEADER_MAGIC].pack('n')
    frame << [0].pack('n')
    frame << [0].pack('N')
    frame << [header_words].pack('n')
    frame << header_data
    frame << payload
    frame
  end
end
