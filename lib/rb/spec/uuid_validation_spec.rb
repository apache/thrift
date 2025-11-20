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

require 'spec_helper'

describe 'UUID Validation' do
  protocols = [
    ['BinaryProtocol', Thrift::BinaryProtocol],
  ]

  if defined?(Thrift::BinaryProtocolAccelerated)
    protocols << ['BinaryProtocolAccelerated', Thrift::BinaryProtocolAccelerated]
  end

  protocols << ['CompactProtocol', Thrift::CompactProtocol]
  protocols << ['JsonProtocol', Thrift::JsonProtocol]

  protocols.each do |protocol_name, protocol_class|
    describe protocol_name do
      before(:each) do
        @trans = Thrift::MemoryBufferTransport.new
        @prot = protocol_class.new(@trans)
      end

      context 'valid UUIDs' do
        it 'should accept lowercase UUIDs' do
          uuid = '550e8400-e29b-41d4-a716-446655440000'
          expect { @prot.write_uuid(uuid) }.not_to raise_error
          result = @prot.read_uuid
          expect(result).to eq(uuid)
        end

        it 'should accept uppercase UUIDs' do
          uuid = '550E8400-E29B-41D4-A716-446655440000'
          expect { @prot.write_uuid(uuid) }.not_to raise_error
          result = @prot.read_uuid
          # Result should be lowercase
          expect(result).to eq('550e8400-e29b-41d4-a716-446655440000')
        end

        it 'should accept mixed case UUIDs' do
          uuid = '550e8400-E29B-41d4-A716-446655440000'
          expect { @prot.write_uuid(uuid) }.not_to raise_error
          result = @prot.read_uuid
          expect(result).to eq('550e8400-e29b-41d4-a716-446655440000')
        end

        it 'should accept all zeros' do
          uuid = '00000000-0000-0000-0000-000000000000'
          expect { @prot.write_uuid(uuid) }.not_to raise_error
          result = @prot.read_uuid
          expect(result).to eq(uuid)
        end

        it 'should accept all fs' do
          uuid = 'ffffffff-ffff-ffff-ffff-ffffffffffff'
          expect { @prot.write_uuid(uuid) }.not_to raise_error
          result = @prot.read_uuid
          expect(result).to eq(uuid)
        end
      end

      context 'invalid UUIDs' do
        def expect_invalid_uuid(value, message)
          expect { @prot.write_uuid(value) }.to raise_error(Thrift::ProtocolException) do |error|
            expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
            expect(error.message).to eq(message)
          end
        end

        it 'should reject nil' do
          expect_invalid_uuid(nil, 'UUID must be a string')
        end

        it 'should reject non-string' do
          expect_invalid_uuid(12345, 'UUID must be a string')
        end

        it 'should reject wrong length' do
          expect_invalid_uuid('550e8400-e29b-41d4-a716', 'Invalid UUID format')
        end

        it 'should reject missing hyphens' do
          expect_invalid_uuid('550e8400e29b41d4a716446655440000', 'Invalid UUID format')
        end

        it 'should reject hyphens in wrong positions' do
          expect_invalid_uuid('550e840-0e29b-41d4-a716-446655440000', 'Invalid UUID format')
        end

        it 'should reject invalid hex characters (g)' do
          expect_invalid_uuid('550e8400-e29b-41d4-a716-44665544000g', 'Invalid UUID format')
        end

        it 'should reject invalid hex characters (z)' do
          expect_invalid_uuid('z50e8400-e29b-41d4-a716-446655440000', 'Invalid UUID format')
        end

        it 'should reject invalid hex characters (space)' do
          expect_invalid_uuid('550e8400-e29b-41d4-a716-44665544000 ', 'Invalid UUID format')
        end

        it 'should reject empty string' do
          expect_invalid_uuid('', 'Invalid UUID format')
        end

        it 'should reject UUID with extra characters' do
          expect_invalid_uuid('550e8400-e29b-41d4-a716-446655440000x', 'Invalid UUID format')
        end

        it 'should reject trailing hyphen' do
          expect_invalid_uuid('550e8400-e29b-41d4-a716-44665544000-', 'Invalid UUID format')
        end

        it 'should reject hyphen inside hex pair' do
          expect_invalid_uuid('550e8400-e29b-41d4-a716-4466-5544000', 'Invalid UUID format')
        end
      end

      context 'malformed binary data on read' do
        it 'should raise error on truncated data' do
          @trans = Thrift::MemoryBufferTransport.new
          @prot = protocol_class.new(@trans)

          # Write only 10 bytes instead of 16
          if protocol_class == Thrift::JsonProtocol
            @trans.write('"00000000-0000-0000-0000"')
          else
            @trans.write("\x00" * 10)
          end

          expect { @prot.read_uuid }.to raise_error(EOFError)
        end

        it 'should raise error on 15 bytes (one byte short)' do
          @trans = Thrift::MemoryBufferTransport.new
          @prot = protocol_class.new(@trans)

          if protocol_class == Thrift::JsonProtocol
            @trans.write('"00000000-0000-0000-0000-000000000"')
          else
            @trans.write("\x00" * 15)
          end

          expect { @prot.read_uuid }.to raise_error(EOFError)
        end

        it 'should raise error on empty buffer' do
          @trans = Thrift::MemoryBufferTransport.new
          @prot = protocol_class.new(@trans)

          expect { @prot.read_uuid }.to raise_error(EOFError)
        end
      end

      context 'multiple UUIDs in sequence' do
        it 'should handle 10 UUIDs in sequence' do
          uuids = 10.times.map { |i| sprintf('%08x-0000-0000-0000-000000000000', i) }

          @trans = Thrift::MemoryBufferTransport.new
          @prot = protocol_class.new(@trans)

          uuids.each { |uuid| @prot.write_uuid(uuid) }

          results = 10.times.map { @prot.read_uuid }
          expect(results).to eq(uuids)
        end

        it 'should handle UUIDs interleaved with other types' do
          @trans = Thrift::MemoryBufferTransport.new
          @prot = protocol_class.new(@trans)

          @prot.write_message_begin('testMessage',  Thrift::MessageTypes::CALL, 0)
          @prot.write_i32(42)
          @prot.write_uuid('550e8400-e29b-41d4-a716-446655440000')
          @prot.write_string('test')
          @prot.write_uuid('6ba7b810-9dad-11d1-80b4-00c04fd430c8')
          @prot.write_i64(123456789)
          @prot.write_message_end

          @prot.read_message_begin
          expect(@prot.read_i32).to eq(42)
          expect(@prot.read_uuid).to eq('550e8400-e29b-41d4-a716-446655440000')
          expect(@prot.read_string).to eq('test')
          expect(@prot.read_uuid).to eq('6ba7b810-9dad-11d1-80b4-00c04fd430c8')
          expect(@prot.read_i64).to eq(123456789)
          @prot.read_message_end
        end

        it 'should handle UUIDs in struct fields context' do
          @trans = Thrift::MemoryBufferTransport.new
          @prot = protocol_class.new(@trans)

          # Simulate struct field headers
          @prot.write_struct_begin('test')
          @prot.write_field_begin('uuid1', Thrift::Types::UUID, 1)
          @prot.write_uuid('550e8400-e29b-41d4-a716-446655440000')
          @prot.write_field_end
          @prot.write_field_begin('uuid2', Thrift::Types::UUID, 2)
          @prot.write_uuid('6ba7b810-9dad-11d1-80b4-00c04fd430c8')
          @prot.write_field_end
          @prot.write_field_stop
          @prot.write_struct_end

          @prot.read_struct_begin
          name, type, id = @prot.read_field_begin
          expect(type).to eq(Thrift::Types::UUID)
          expect(@prot.read_uuid).to eq('550e8400-e29b-41d4-a716-446655440000')
          @prot.read_field_end

          name, type, id = @prot.read_field_begin
          expect(type).to eq(Thrift::Types::UUID)
          expect(@prot.read_uuid).to eq('6ba7b810-9dad-11d1-80b4-00c04fd430c8')
          @prot.read_field_end

          name, type, id = @prot.read_field_begin
          expect(type).to eq(Thrift::Types::STOP)
        end
      end
    end
  end
end
