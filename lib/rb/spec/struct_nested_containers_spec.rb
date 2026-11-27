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

describe "StructNestedContainers" do
  def with_type_checking
    saved_type_checking, Thrift.type_checking = Thrift.type_checking, true
    begin
      yield
    ensure
      Thrift.type_checking = saved_type_checking
    end
  end

  describe Thrift::Struct do
    # Nested container tests, see THRIFT-369.
    it "should support nested lists inside lists" do
      with_type_checking do
        a, b = SpecNamespace::NestedListInList.new, SpecNamespace::NestedListInList.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = [ [1, 2, 3], [2, 3, 4] ]
          thrift_struct.validate
        end
        expect(a).to eq(b)
        b.value.push [3, 4, 5]
        expect(a).not_to eq(b)
      end
    end

    it "should support nested lists inside sets" do
      with_type_checking do
        a, b = SpecNamespace::NestedListInSet.new, SpecNamespace::NestedListInSet.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = [ [1, 2, 3], [2, 3, 4] ].to_set
          thrift_struct.validate
        end
        expect(a).to eq(b)
        b.value.add [3, 4, 5]
        expect(a).not_to eq(b)
      end
    end

    [Thrift::BinaryProtocol, Thrift::CompactProtocol, Thrift::JsonProtocol].each do |protocol_class|
      describe "sets of lists with #{protocol_class}" do
        let(:lists) { [[], [1], [2, 3]] }

        {
          "Set" => ->(items) { Set.new(items) },
          "Set subclass" => ->(items) { Class.new(Set).new(items) },
          "Array" => ->(items) { items },
          "Hash" => ->(items) { items.zip([true, false, nil]).to_h },
        }.each do |representation, build_set|
          it "preserves list elements in the pure-Ruby writer using #{representation}" do
            struct = SpecNamespace::NestedListInSet.new
            field = SpecNamespace::NestedListInSet::FIELDS[1]
            transport = Thrift::MemoryBufferTransport.new
            protocol = protocol_class.new(transport)

            # Call the Ruby container writer even when the native struct writer is loaded.
            struct.write_container(protocol, build_set.call(lists), field)

            expect(struct.read_field(protocol_class.new(transport), field)).to eq(Set.new(lists))
            expect(transport.available).to eq(0)
          end
        end

        it "round trips a generated struct containing a set of lists" do
          original = SpecNamespace::NestedListInSet.new(value: Set.new(lists))
          transport = Thrift::MemoryBufferTransport.new
          original.write(protocol_class.new(transport))
          restored = SpecNamespace::NestedListInSet.new
          restored.read(protocol_class.new(transport))

          expect(restored).to eq(original)
          expect(transport.available).to eq(0)
        end
      end
    end

    it "should support nested lists in map keys" do
      with_type_checking do
        a, b = SpecNamespace::NestedListInMapKey.new, SpecNamespace::NestedListInMapKey.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = { [1, 2, 3] => 1, [2, 3, 4] => 2 }
          thrift_struct.validate
        end
        expect(a).to eq(b)
        b.value[[3, 4, 5]] = 3
        expect(a).not_to eq(b)
      end
    end

    it "should support nested lists in map values" do
      with_type_checking do
        a, b = SpecNamespace::NestedListInMapValue.new, SpecNamespace::NestedListInMapValue.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = { 1 => [1, 2, 3], 2 => [2, 3, 4] }
          thrift_struct.validate
        end
        expect(a).to eq(b)
        b.value[3] = [3, 4, 5]
        expect(a).not_to eq(b)
      end
    end

    it "should support nested sets inside lists" do
      with_type_checking do
        a, b = SpecNamespace::NestedSetInList.new, SpecNamespace::NestedSetInList.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = [ [1, 2, 3].to_set, [2, 3, 4].to_set ]
          thrift_struct.validate
        end
        expect(a).to eq(b)
        b.value.push([3, 4, 5].to_set)
        expect(a).not_to eq(b)
      end
    end

    it "should support nested sets inside sets" do
      with_type_checking do
        a, b = SpecNamespace::NestedSetInSet.new, SpecNamespace::NestedSetInSet.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = [ [1, 2, 3].to_set, [2, 3, 4].to_set ].to_set
          thrift_struct.validate
        end
        expect(a).to eq(b)
        b.value.add([3, 4, 5].to_set)
        expect(a).not_to eq(b)
      end
    end

    it "should support nested sets in map keys" do
      with_type_checking do
        a, b = SpecNamespace::NestedSetInMapKey.new, SpecNamespace::NestedSetInMapKey.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = { [1, 2, 3].to_set => 1, [2, 3, 4].to_set => 2 }
          thrift_struct.validate
        end
        expect(a).to eq(b)
        b.value[[3, 4, 5].to_set] = 3
        expect(a).not_to eq(b)
      end
    end

    it "should support nested sets in map values" do
      with_type_checking do
        a, b = SpecNamespace::NestedSetInMapValue.new, SpecNamespace::NestedSetInMapValue.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = { 1 => [1, 2, 3].to_set, 2 => [2, 3, 4].to_set }
          thrift_struct.validate
        end
        expect(a).to eq(b)
        b.value[3] = [3, 4, 5].to_set
        expect(a).not_to eq(b)
      end
    end

    it "should support nested maps inside lists" do
      with_type_checking do
        a, b = SpecNamespace::NestedMapInList.new, SpecNamespace::NestedMapInList.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = [ {1 => 2, 3 => 4}, {2 => 3, 4 => 5} ]
          thrift_struct.validate
        end
        expect(a).to eq(b)
        b.value.push({ 3 => 4, 5 => 6 })
        expect(a).not_to eq(b)
      end
    end

    it "should support nested maps inside sets" do
      with_type_checking do
        a, b = SpecNamespace::NestedMapInSet.new, SpecNamespace::NestedMapInSet.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = [ {1 => 2, 3 => 4}, {2 => 3, 4 => 5} ].to_set
          thrift_struct.validate
        end
        expect(a).to eq(b)
        b.value.add({ 3 => 4, 5 => 6 })
        expect(a).not_to eq(b)
      end
    end

    it "should support nested maps in map keys" do
      with_type_checking do
        a, b = SpecNamespace::NestedMapInMapKey.new, SpecNamespace::NestedMapInMapKey.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = { { 1 => 2, 3 => 4} => 1, {2 => 3, 4 => 5} => 2 }
          thrift_struct.validate
        end
        expect(a).to eq(b)
        b.value[{3 => 4, 5 => 6}] = 3
        expect(a).not_to eq(b)
      end
    end

    it "should support nested maps in map values" do
      with_type_checking do
        a, b = SpecNamespace::NestedMapInMapValue.new, SpecNamespace::NestedMapInMapValue.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = { 1 => { 1 => 2, 3 => 4}, 2 => {2 => 3, 4 => 5} }
          thrift_struct.validate
        end
        expect(a).to eq(b)
        b.value[3] = { 3 => 4, 5 => 6 }
        expect(a).not_to eq(b)
      end
    end

    protocols = [Thrift::BinaryProtocol, Thrift::CompactProtocol, Thrift::JsonProtocol]
    protocols << Thrift::BinaryProtocolAccelerated if defined?(Thrift::BinaryProtocolAccelerated)
    protocols.each do |protocol_class|
      describe "double set elements and map keys with #{protocol_class}" do
        # Separate NaN objects, with and without a sign bit or payload; a Hash keeps every one of them.
        let(:nans) { [Float::NAN, -Float::NAN, [0x7ff8000000000001].pack("Q>").unpack1("G")].map { |nan| nan * 1.0 } }
        let(:ordinary) { [1.5, -0.0, Float::INFINITY, -Float::INFINITY] }
        let(:ordinary_weights) { ordinary.each_with_index.to_h { |value, i| [value, 10 + i] } }
        let(:transport) do
          values = Set.new(nans + ordinary)
          weights = nans.each_with_index.to_h { |nan, i| [nan, i + 1] }.merge(ordinary_weights)
          expect([values.size, weights.size]).to eq([nans.size + ordinary.size] * 2)
          Thrift::MemoryBufferTransport.new.tap do |buffer|
            SpecNamespace::DoubleKeys.new(values: values, weights: weights).write(protocol_class.new(buffer))
          end
        end

        def expect_one_nan(values, weights)
          expect(values.size).to eq(ordinary.size + 1)
          expect(values.select(&:nan?)).to eq([Float::NAN])
          expect(values.reject(&:nan?)).to match_array(ordinary)
          expect(values.find(&:zero?).to_s).to eq("-0.0")

          # The last NaN key read keeps its value.
          expect(weights.size).to eq(ordinary.size + 1)
          expect(weights.select { |key, _| key.nan? }.to_a).to eq([[Float::NAN, nans.size]])
          expect(weights.reject { |key, _| key.nan? }).to eq(ordinary_weights)
          expect(transport.available).to eq(0)
        end

        it "keeps one NaN when a generated struct reads them" do
          restored = SpecNamespace::DoubleKeys.new
          restored.read(protocol_class.new(transport))

          expect_one_nan(restored.values, restored.weights)
        end

        it "keeps every NaN in a list of doubles" do
          list_transport = Thrift::MemoryBufferTransport.new
          SpecNamespace::SimpleList.new(doubles: nans + [1.5] + nans).write(protocol_class.new(list_transport))
          restored = SpecNamespace::SimpleList.new
          restored.read(protocol_class.new(list_transport))

          expect(restored.doubles.size).to eq((2 * nans.size) + 1)
          expect(restored.doubles.map(&:nan?)).to eq(([true] * nans.size) + [false] + ([true] * nans.size))
          expect(restored.doubles[nans.size]).to eq(1.5)
          expect(list_transport.available).to eq(0)
        end

        it "keeps one NaN when the pure-Ruby reader reads them" do
          protocol = protocol_class.new(transport)
          struct = SpecNamespace::DoubleKeys.new
          read = {}
          # Call the Ruby container reader even when the native struct reader is loaded.
          protocol.read_struct_begin
          loop do
            _, type, id = protocol.read_field_begin
            break if type == Thrift::Types::STOP
            read[id] = struct.read_field(protocol, SpecNamespace::DoubleKeys::FIELDS[id])
            protocol.read_field_end
          end
          protocol.read_struct_end

          expect_one_nan(read[1], read[2])
        end
      end
    end
  end
end
