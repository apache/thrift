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

describe "recursion depth limit" do
  MAX_DEPTH = Thrift::DEFAULT_RECURSION_DEPTH
  OVER_LIMIT = MAX_DEPTH + 1

  def binary_protocol
    Thrift::BinaryProtocol.new(Thrift::MemoryBufferTransport.new)
  end

  it "accepts an optional recursion depth" do
    tree = SpecNamespace::RecTree.new(item: 1, children: [])
    protocol = binary_protocol

    tree.write(protocol, 1)
    expect { SpecNamespace::RecTree.new.read(protocol, 1) }.not_to raise_error

    expect { tree.read(binary_protocol, 0) }.to raise_error(Thrift::ProtocolException) { |error|
      expect(error.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
    }
    expect { tree.write(binary_protocol, 0) }.to raise_error(Thrift::ProtocolException) { |error|
      expect(error.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
    }
  end

  describe "structs nested in containers" do
    it "round-trips a chain at the limit" do
      tree = SpecNamespace::RecTree.new(item: 1, children: [])
      2.upto(MAX_DEPTH) do |depth|
        tree = SpecNamespace::RecTree.new(item: depth, children: [tree])
      end

      protocol = binary_protocol
      tree.write(protocol)
      result = SpecNamespace::RecTree.new
      result.read(protocol)

      depth = 0
      until result.nil?
        depth += 1
        result = result.children.first
      end
      expect(depth).to eq(MAX_DEPTH)
    end

    it "rejects writing a chain past the limit" do
      tree = SpecNamespace::RecTree.new(item: 1, children: [])
      2.upto(OVER_LIMIT) do |depth|
        tree = SpecNamespace::RecTree.new(item: depth, children: [tree])
      end

      expect { tree.write(binary_protocol) }.to raise_error(Thrift::ProtocolException) { |error|
        expect(error.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
    end

    it "rejects reading a payload past the limit" do
      tree = SpecNamespace::RecTree.new(item: 1, children: [])
      2.upto(OVER_LIMIT) do |depth|
        tree = SpecNamespace::RecTree.new(item: depth, children: [tree])
      end

      protocol = binary_protocol
      tree.write(protocol, OVER_LIMIT)

      expect { SpecNamespace::RecTree.new.read(protocol) }.to raise_error(Thrift::ProtocolException) { |error|
        expect(error.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
    end

    it "round-trips a wide shallow tree" do
      width = MAX_DEPTH * 3
      root = SpecNamespace::RecTree.new(
        item: 0,
        children: (1..width).map { |item| SpecNamespace::RecTree.new(item: item, children: []) }
      )

      protocol = binary_protocol
      root.write(protocol)
      result = SpecNamespace::RecTree.new
      result.read(protocol)

      expect(result.children.size).to eq(width)
    end

    it "rejects cyclic values on write" do
      root = SpecNamespace::RecTree.new(item: 1, children: [])
      root.children = [root]

      expect { root.write(binary_protocol) }.to raise_error(Thrift::ProtocolException) { |error|
        expect(error.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
    end
  end

  describe "direct struct fields" do
    it "round-trips a chain at the limit" do
      tree = SpecNamespace::RecStruct.new
      (MAX_DEPTH - 1).times { tree = SpecNamespace::RecStruct.new(child: tree) }

      protocol = binary_protocol
      tree.write(protocol)

      expect { SpecNamespace::RecStruct.new.read(protocol) }.not_to raise_error
    end

    it "rejects writing a chain past the limit" do
      tree = SpecNamespace::RecStruct.new
      (OVER_LIMIT - 1).times { tree = SpecNamespace::RecStruct.new(child: tree) }

      expect { tree.write(binary_protocol) }.to raise_error(Thrift::ProtocolException) { |error|
        expect(error.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
    end

    it "preserves the remaining depth through the public field helper" do
      parent = SpecNamespace::RecStruct.new(child: SpecNamespace::RecStruct.new)
      parent.define_singleton_method(:write) do |oprot, remaining_depth = MAX_DEPTH|
        oprot.write_struct_begin(self.class.name)
        oprot.write_field(self.class::FIELDS[1], 1, child, remaining_depth)
        oprot.write_field_stop
        oprot.write_struct_end
      end

      expect { parent.write(binary_protocol, 1) }.to raise_error(Thrift::ProtocolException) { |error|
        expect(error.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
    end

    it "rejects reading a payload past the limit" do
      tree = SpecNamespace::RecStruct.new
      (OVER_LIMIT - 1).times { tree = SpecNamespace::RecStruct.new(child: tree) }

      protocol = binary_protocol
      tree.write(protocol, OVER_LIMIT)

      expect { SpecNamespace::RecStruct.new.read(protocol) }.to raise_error(Thrift::ProtocolException) { |error|
        expect(error.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
    end

    it "uses an independent depth limit for skipped structs" do
      write_payload = lambda do |struct_depth|
        protocol = binary_protocol
        write_struct = lambda do |depth|
          protocol.write_struct_begin("UnknownStruct")
          if depth > 1
            protocol.write_field_begin("child", Thrift::Types::STRUCT, 1)
            write_struct.call(depth - 1)
            protocol.write_field_end
          end
          protocol.write_field_stop
          protocol.write_struct_end
        end
        protocol.write_struct_begin("RecStruct")
        protocol.write_field_begin("unknown", Thrift::Types::STRUCT, 2)
        write_struct.call(struct_depth)
        protocol.write_field_end
        protocol.write_field_stop
        protocol.write_struct_end
        protocol
      end

      within_limit = write_payload.call(MAX_DEPTH)
      expect { SpecNamespace::RecStruct.new.read(within_limit, 1) }.not_to raise_error

      over_limit = write_payload.call(OVER_LIMIT)
      expect { SpecNamespace::RecStruct.new.read(over_limit, 1) }.to raise_error(Thrift::ProtocolException) { |error|
        expect(error.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
    end

    it "uses an independent depth limit for skipped containers" do
      protocol = binary_protocol
      write_list = lambda do |depth|
        element_type = depth > 1 ? Thrift::Types::LIST : Thrift::Types::I32
        protocol.write_list_begin(element_type, 1)
        if depth > 1
          write_list.call(depth - 1)
        else
          protocol.write_i32(1)
        end
        protocol.write_list_end
      end
      protocol.write_struct_begin("RecStruct")
      protocol.write_field_begin("unknown", Thrift::Types::LIST, 2)
      write_list.call(3)
      protocol.write_field_end
      protocol.write_field_stop
      protocol.write_struct_end

      expect { SpecNamespace::RecStruct.new.read(protocol, 1) }.not_to raise_error
    end

    unless defined? Thrift::BinaryProtocolAccelerated
      it "uses the public field hook for direct structs" do
        child = SpecNamespace::RecStruct.new
        parent = SpecNamespace::RecStruct.new(child: child)
        protocol = binary_protocol
        expect(protocol).to receive(:write_field).with(parent.class::FIELDS[1], 1, child, MAX_DEPTH).and_call_original

        parent.write(protocol)
      end

      it "uses the public write hook for nested structs" do
        child = SpecNamespace::RecStruct.new
        protocol = binary_protocol
        expect(child).to receive(:write).with(protocol, MAX_DEPTH - 1).and_call_original

        SpecNamespace::RecStruct.new(child: child).write(protocol)
      end
    end
  end

  describe "unions" do
    it "round-trips a chain at the limit" do
      union = SpecNamespace::RecUnion.new(leaf: 0)
      (MAX_DEPTH - 1).times { union = SpecNamespace::RecUnion.new(children: [union]) }

      protocol = binary_protocol
      union.write(protocol)

      expect { SpecNamespace::RecUnion.new.read(protocol) }.not_to raise_error
    end

    it "rejects writing a chain past the limit" do
      union = SpecNamespace::RecUnion.new(leaf: 0)
      (OVER_LIMIT - 1).times { union = SpecNamespace::RecUnion.new(children: [union]) }

      expect { union.write(binary_protocol) }.to raise_error(Thrift::ProtocolException) { |error|
        expect(error.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
    end

    it "rejects reading a payload past the limit" do
      union = SpecNamespace::RecUnion.new(leaf: 0)
      (OVER_LIMIT - 1).times { union = SpecNamespace::RecUnion.new(children: [union]) }

      protocol = binary_protocol
      union.write(protocol, OVER_LIMIT)

      expect { SpecNamespace::RecUnion.new.read(protocol) }.to raise_error(Thrift::ProtocolException) { |error|
        expect(error.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
    end
  end

  describe "exceptions" do
    it "round-trips a chain at the limit" do
      error = SpecNamespace::RecError.new(leaf: 1, children: [])
      2.upto(MAX_DEPTH) do |depth|
        error = SpecNamespace::RecError.new(leaf: depth, children: [error])
      end

      protocol = binary_protocol
      error.write(protocol)
      result = SpecNamespace::RecError.new
      result.read(protocol)

      depth = 0
      until result.nil?
        depth += 1
        result = result.children.first
      end
      expect(depth).to eq(MAX_DEPTH)
    end

    it "rejects writing a chain past the limit" do
      error = SpecNamespace::RecError.new(leaf: 1, children: [])
      2.upto(OVER_LIMIT) do |depth|
        error = SpecNamespace::RecError.new(leaf: depth, children: [error])
      end

      expect { error.write(binary_protocol) }.to raise_error(Thrift::ProtocolException) { |exception|
        expect(exception.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
    end

    it "rejects reading a payload past the limit" do
      recursive_error = SpecNamespace::RecError.new(leaf: 1, children: [])
      2.upto(OVER_LIMIT) do |depth|
        recursive_error = SpecNamespace::RecError.new(leaf: depth, children: [recursive_error])
      end

      protocol = binary_protocol
      recursive_error.write(protocol, OVER_LIMIT)

      expect { SpecNamespace::RecError.new.read(protocol) }.to raise_error(Thrift::ProtocolException) { |error|
        expect(error.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
    end
  end

  describe "application exceptions" do
    it "accepts and propagates optional recursion depths" do
      error = Thrift::ApplicationException.new(Thrift::ApplicationException::UNKNOWN, "message")
      protocol = binary_protocol

      protocol.write_type({type: Thrift::Types::STRUCT}, error, 2)
      expect {
        SpecNamespace::RecStruct.new.read_field(
          protocol,
          {type: Thrift::Types::STRUCT, class: Thrift::ApplicationException},
          2
        )
      }.not_to raise_error

      expect { error.write(binary_protocol, 0) }.to raise_error(Thrift::ProtocolException) { |exception|
        expect(exception.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
      expect { error.read(binary_protocol, 0) }.to raise_error(Thrift::ProtocolException) { |exception|
        expect(exception.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
    end

    it "uses an independent depth limit for skipped unknown fields" do
      write_payload = lambda do |unknown_depth|
        protocol = binary_protocol
        write_unknown_struct = lambda do |depth|
          protocol.write_struct_begin("UnknownStruct")
          if depth > 1
            protocol.write_field_begin("child", Thrift::Types::STRUCT, 1)
            write_unknown_struct.call(depth - 1)
            protocol.write_field_end
          end
          protocol.write_field_stop
          protocol.write_struct_end
        end
        protocol.write_struct_begin("ApplicationException")
        protocol.write_field_begin("unknown", Thrift::Types::STRUCT, 3)
        write_unknown_struct.call(unknown_depth)
        protocol.write_field_end
        protocol.write_field_stop
        protocol.write_struct_end
        protocol
      end

      within_limit = write_payload.call(MAX_DEPTH)
      expect { Thrift::ApplicationException.new.read(within_limit, 1) }.not_to raise_error

      over_limit = write_payload.call(OVER_LIMIT)
      expect { Thrift::ApplicationException.new.read(over_limit, 1) }.to raise_error(Thrift::ProtocolException) { |error|
        expect(error.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
      }
    end
  end
end
