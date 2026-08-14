# frozen_string_literal: true
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

require "spec_helper"

describe "UUID Validation" do
  fixed_width_protocols = [
    ["BinaryProtocol", Thrift::BinaryProtocol],
  ]

  if defined?(Thrift::BinaryProtocolAccelerated)
    fixed_width_protocols << ["BinaryProtocolAccelerated", Thrift::BinaryProtocolAccelerated]
  end

  fixed_width_protocols << ["CompactProtocol", Thrift::CompactProtocol]
  protocols = fixed_width_protocols.dup
  protocols << ["JsonProtocol", Thrift::JsonProtocol]

  protocols.each do |protocol_name, protocol_class|
    describe protocol_name do
      before(:each) do
        @trans = Thrift::MemoryBufferTransport.new
        @prot = protocol_class.new(@trans)
      end

      context "valid UUIDs" do
        it "should accept lowercase UUIDs" do
          uuid = "550e8400-e29b-41d4-a716-446655440000"
          expect { @prot.write_uuid(uuid) }.not_to raise_error
          result = @prot.read_uuid
          expect(result).to eq(uuid)
        end

        it "should accept uppercase UUIDs" do
          uuid = "550E8400-E29B-41D4-A716-446655440000"
          expect { @prot.write_uuid(uuid) }.not_to raise_error
          result = @prot.read_uuid
          # Result should be lowercase
          expect(result).to eq("550e8400-e29b-41d4-a716-446655440000")
        end

        it "should accept mixed case UUIDs" do
          uuid = "550e8400-E29B-41d4-A716-446655440000"
          expect { @prot.write_uuid(uuid) }.not_to raise_error
          result = @prot.read_uuid
          expect(result).to eq("550e8400-e29b-41d4-a716-446655440000")
        end

        it "should accept all zeros" do
          uuid = "00000000-0000-0000-0000-000000000000"
          expect { @prot.write_uuid(uuid) }.not_to raise_error
          result = @prot.read_uuid
          expect(result).to eq(uuid)
        end

        it "should accept all fs" do
          uuid = "ffffffff-ffff-ffff-ffff-ffffffffffff"
          expect { @prot.write_uuid(uuid) }.not_to raise_error
          result = @prot.read_uuid
          expect(result).to eq(uuid)
        end
      end

      context "invalid UUIDs" do
        def expect_invalid_uuid(value, message)
          expect { @prot.write_uuid(value) }.to raise_error(Thrift::ProtocolException) do |error|
            expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
            expect(error.message).to eq(message)
          end
        end

        it "should reject nil" do
          expect_invalid_uuid(nil, "UUID must be a string")
        end

        it "should reject non-string" do
          expect_invalid_uuid(12345, "UUID must be a string")
        end

        it "should reject wrong length" do
          expect_invalid_uuid("550e8400-e29b-41d4-a716", "Invalid UUID format")
        end

        it "should reject missing hyphens" do
          expect_invalid_uuid("550e8400e29b41d4a716446655440000", "Invalid UUID format")
        end

        it "should reject hyphens in wrong positions" do
          expect_invalid_uuid("550e840-0e29b-41d4-a716-446655440000", "Invalid UUID format")
        end

        it "should reject invalid hex characters (g)" do
          expect_invalid_uuid("550e8400-e29b-41d4-a716-44665544000g", "Invalid UUID format")
        end

        it "should reject invalid hex characters (z)" do
          expect_invalid_uuid("z50e8400-e29b-41d4-a716-446655440000", "Invalid UUID format")
        end

        it "should reject invalid hex characters (space)" do
          expect_invalid_uuid("550e8400-e29b-41d4-a716-44665544000 ", "Invalid UUID format")
        end

        it "should reject empty string" do
          expect_invalid_uuid("", "Invalid UUID format")
        end

        it "should reject UUID with extra characters" do
          expect_invalid_uuid("550e8400-e29b-41d4-a716-446655440000x", "Invalid UUID format")
        end

        it "should reject trailing hyphen" do
          expect_invalid_uuid("550e8400-e29b-41d4-a716-44665544000-", "Invalid UUID format")
        end

        it "should reject hyphen inside hex pair" do
          expect_invalid_uuid("550e8400-e29b-41d4-a716-4466-5544000", "Invalid UUID format")
        end
      end

      context "multiple UUIDs in sequence" do
        it "should handle 10 UUIDs in sequence" do
          uuids = Array.new(10) { |i| sprintf("%08x-0000-0000-0000-000000000000", i) }

          @trans = Thrift::MemoryBufferTransport.new
          @prot = protocol_class.new(@trans)

          uuids.each { |uuid| @prot.write_uuid(uuid) }

          results = Array.new(10) { @prot.read_uuid }
          expect(results).to eq(uuids)
        end

        it "should handle UUIDs interleaved with other types" do
          @trans = Thrift::MemoryBufferTransport.new
          @prot = protocol_class.new(@trans)

          @prot.write_message_begin("testMessage", Thrift::MessageTypes::CALL, 0)
          @prot.write_i32(42)
          @prot.write_uuid("550e8400-e29b-41d4-a716-446655440000")
          @prot.write_string("test")
          @prot.write_uuid("6ba7b810-9dad-11d1-80b4-00c04fd430c8")
          @prot.write_i64(123456789)
          @prot.write_message_end

          @prot.read_message_begin
          expect(@prot.read_i32).to eq(42)
          expect(@prot.read_uuid).to eq("550e8400-e29b-41d4-a716-446655440000")
          expect(@prot.read_string).to eq("test")
          expect(@prot.read_uuid).to eq("6ba7b810-9dad-11d1-80b4-00c04fd430c8")
          expect(@prot.read_i64).to eq(123456789)
          @prot.read_message_end
        end

        it "should handle UUIDs in struct fields context" do
          @trans = Thrift::MemoryBufferTransport.new
          @prot = protocol_class.new(@trans)

          # Simulate struct field headers
          @prot.write_struct_begin("test")
          @prot.write_field_begin("uuid1", Thrift::Types::UUID, 1)
          @prot.write_uuid("550e8400-e29b-41d4-a716-446655440000")
          @prot.write_field_end
          @prot.write_field_begin("uuid2", Thrift::Types::UUID, 2)
          @prot.write_uuid("6ba7b810-9dad-11d1-80b4-00c04fd430c8")
          @prot.write_field_end
          @prot.write_field_stop
          @prot.write_struct_end

          @prot.read_struct_begin
          name, type, id = @prot.read_field_begin
          expect(type).to eq(Thrift::Types::UUID)
          expect(@prot.read_uuid).to eq("550e8400-e29b-41d4-a716-446655440000")
          @prot.read_field_end

          name, type, id = @prot.read_field_begin
          expect(type).to eq(Thrift::Types::UUID)
          expect(@prot.read_uuid).to eq("6ba7b810-9dad-11d1-80b4-00c04fd430c8")
          @prot.read_field_end

          name, type, id = @prot.read_field_begin
          expect(type).to eq(Thrift::Types::STOP)
        end
      end
    end
  end

  describe "fixed-width UUID protocols" do
    fixed_width_protocols.each do |protocol_name, protocol_class|
      describe protocol_name do
        [10, 15].each do |available_bytes|
          it "raises EOFError when only #{available_bytes} of 16 UUID bytes are available" do
            trans = Thrift::MemoryBufferTransport.new("\x00" * available_bytes)
            prot = protocol_class.new(trans)

            expect { prot.read_uuid }.to raise_error(EOFError)
          end
        end

        it "raises EOFError when no UUID bytes are available" do
          trans = Thrift::MemoryBufferTransport.new
          prot = protocol_class.new(trans)

          expect { prot.read_uuid }.to raise_error(EOFError)
        end
      end
    end
  end

  describe Thrift::JsonProtocol do
    it "raises EOFError when the JSON string is missing its closing quote" do
      uuid = "00000000-0000-0000-0000-000000000000"
      json_without_closing_quote = '"' + uuid
      trans = Thrift::MemoryBufferTransport.new(json_without_closing_quote)
      prot = described_class.new(trans)

      expect { prot.read_uuid }.to raise_error(EOFError)
    end

    it "raises EOFError when no JSON UUID data is available" do
      trans = Thrift::MemoryBufferTransport.new
      prot = described_class.new(trans)

      expect { prot.read_uuid }.to raise_error(EOFError)
    end

    context "with a complete malformed UUID string" do
      [
        "00000000-0000-0000-0000",
        "00000000-0000-0000-0000-000000000",
      ].each do |uuid|
        it "rejects a #{uuid.length}-character UUID" do
          trans = Thrift::MemoryBufferTransport.new("\"#{uuid}\"")
          prot = described_class.new(trans)

          expect { prot.read_uuid }.to raise_error(
            Thrift::ProtocolException,
            "Invalid UUID format",
          ) do |error|
            expect(error.type).to eq(Thrift::ProtocolException::INVALID_DATA)
          end
        end
      end
    end
  end
end
