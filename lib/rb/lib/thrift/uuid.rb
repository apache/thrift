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

require 'thrift/protocol/base_protocol'

module Thrift
  module UUID
    UUID_REGEX = /\A[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\z/.freeze

    def self.validate_uuid!(uuid)
      unless uuid.is_a?(String)
        raise ProtocolException.new(ProtocolException::INVALID_DATA, 'UUID must be a string')
      end

      unless uuid =~ UUID_REGEX
        raise ProtocolException.new(ProtocolException::INVALID_DATA, 'Invalid UUID format')
      end
    end

    def self.uuid_bytes(uuid)
      [uuid.delete('-')].pack('H*')
    end

    def self.uuid_from_bytes(bytes)
      unless bytes.bytesize == 16
        raise ProtocolException.new(ProtocolException::INVALID_DATA, 'Invalid UUID data length')
      end

      hex = bytes.unpack('H*').first
      "#{hex[0, 8]}-#{hex[8, 4]}-#{hex[12, 4]}-#{hex[16, 4]}-#{hex[20, 12]}"
    end
  end
end
