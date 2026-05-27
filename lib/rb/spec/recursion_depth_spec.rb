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

require 'spec_helper'

# Round-trip test for struct/union/exception recursion depth, driving the
# *generated* read/write path (Thrift::Struct#read/#write,
# Thrift::Union#read/#write) over RecTree / RecUnion / RecError from
# ThriftSpec.thrift; a linear chain is one struct level deeper per node, so a
# chain of N nodes reaches depth N.
#
# NOTE: the Ruby library does not enforce a recursion-depth limit yet
# (THRIFT-6045). The round-trip / within-limit examples are active; the
# limit-enforcement (over-limit) examples are `pending` and will start passing
# once the limit is implemented, at which point RSpec flags them to be enabled.
describe 'recursion depth limit' do
  # The intended struct/union nesting limit. The Ruby library does not enforce a
  # recursion-depth limit yet (THRIFT-6045); the limit-enforcement examples below
  # are therefore `pending` until it is implemented. 64 matches the limit other
  # Thrift libraries use.
  RECURSION_LIMIT = 64

  # Attached to the pending (over-limit) examples.
  PENDING_REASON = 'recursion-depth limit not implemented in the Ruby library yet (THRIFT-6045)'

  def binary_protocol
    Thrift::BinaryProtocol.new(Thrift::MemoryBufferTransport.new)
  end

  # A linearly nested RecTree that is `depth` struct levels deep.
  def struct_chain(depth)
    node = SpecNamespace::RecTree.new(item: depth, children: [])
    node.children = [struct_chain(depth - 1)] if depth > 1
    node
  end

  def tree_depth(node)
    n = 0
    until node.nil?
      n += 1
      break if node.children.nil? || node.children.empty?
      node = node.children.first
    end
    n
  end

  # A linearly nested RecUnion that is `depth` levels deep (each union holds the
  # next; the innermost holds a scalar leaf).
  def union_chain(depth)
    if depth > 1
      SpecNamespace::RecUnion.new(children: [union_chain(depth - 1)])
    else
      SpecNamespace::RecUnion.new(leaf: 0)
    end
  end

  # A linearly nested RecError exception that is `depth` struct levels deep.
  # Exceptions read/write through the same generated path as structs, so
  # tree_depth applies to the decoded chain too.
  def error_chain(depth)
    node = SpecNamespace::RecError.new(leaf: depth, children: [])
    node.children = [error_chain(depth - 1)] if depth > 1
    node
  end

  # Emit, via raw protocol calls (which carry no depth guard), the wire image of
  # a RecTree chain `depth` levels deep. This lets the read tests feed an
  # over-limit payload that the guarded writer would itself refuse to produce.
  def write_raw_tree(oprot, depth)
    oprot.write_struct_begin('RecTree')
    oprot.write_field_begin('children', Thrift::Types::LIST, 1)
    oprot.write_list_begin(Thrift::Types::STRUCT, depth > 1 ? 1 : 0)
    write_raw_tree(oprot, depth - 1) if depth > 1
    oprot.write_list_end
    oprot.write_field_end
    oprot.write_field_begin('item', Thrift::Types::I16, 2)
    oprot.write_i16(depth)
    oprot.write_field_end
    oprot.write_field_stop
    oprot.write_struct_end
  end

  # Same as write_raw_tree but for the RecError exception (leaf is i32 here).
  def write_raw_error(oprot, depth)
    oprot.write_struct_begin('RecError')
    oprot.write_field_begin('children', Thrift::Types::LIST, 1)
    oprot.write_list_begin(Thrift::Types::STRUCT, depth > 1 ? 1 : 0)
    write_raw_error(oprot, depth - 1) if depth > 1
    oprot.write_list_end
    oprot.write_field_end
    oprot.write_field_begin('leaf', Thrift::Types::I32, 2)
    oprot.write_i32(depth)
    oprot.write_field_end
    oprot.write_field_stop
    oprot.write_struct_end
  end

  def expect_depth_limit
    expect { yield }.to raise_error(Thrift::ProtocolException) { |e|
      expect(e.type).to eq(Thrift::ProtocolException::DEPTH_LIMIT)
    }
  end

  describe 'structs' do
    it 'round-trips a chain at the limit' do
      prot = binary_protocol
      struct_chain(RECURSION_LIMIT).write(prot)
      result = SpecNamespace::RecTree.new
      result.read(prot)
      expect(tree_depth(result)).to eq(RECURSION_LIMIT)
    end

    it 'rejects writing a chain past the limit' do
      pending PENDING_REASON
      expect_depth_limit { struct_chain(RECURSION_LIMIT + 1).write(binary_protocol) }
    end

    it 'rejects reading a payload past the limit' do
      pending PENDING_REASON
      prot = binary_protocol
      write_raw_tree(prot, RECURSION_LIMIT + 1)
      expect_depth_limit { SpecNamespace::RecTree.new.read(prot) }
    end

    it 'round-trips a wide shallow tree (counter unwinds per sibling)' do
      width = RECURSION_LIMIT * 3
      prot = binary_protocol
      root = SpecNamespace::RecTree.new(
        item: 0,
        children: (1..width).map { |i| SpecNamespace::RecTree.new(item: i, children: []) }
      )
      root.write(prot)
      result = SpecNamespace::RecTree.new
      result.read(prot)
      expect(result.children.size).to eq(width)
    end
  end

  describe 'unions' do
    it 'round-trips a chain at the limit' do
      prot = binary_protocol
      union_chain(RECURSION_LIMIT).write(prot)
      expect { SpecNamespace::RecUnion.new.read(prot) }.not_to raise_error
    end

    it 'rejects writing a chain past the limit' do
      pending PENDING_REASON
      expect_depth_limit { union_chain(RECURSION_LIMIT + 1).write(binary_protocol) }
    end
  end

  describe 'exceptions' do
    it 'round-trips a chain at the limit' do
      prot = binary_protocol
      error_chain(RECURSION_LIMIT).write(prot)
      result = SpecNamespace::RecError.new
      result.read(prot)
      expect(tree_depth(result)).to eq(RECURSION_LIMIT)
    end

    it 'rejects writing a chain past the limit' do
      pending PENDING_REASON
      expect_depth_limit { error_chain(RECURSION_LIMIT + 1).write(binary_protocol) }
    end

    it 'rejects reading a payload past the limit' do
      pending PENDING_REASON
      prot = binary_protocol
      write_raw_error(prot, RECURSION_LIMIT + 1)
      expect_depth_limit { SpecNamespace::RecError.new.read(prot) }
    end
  end

  describe 'protocol decorators' do
    # A struct written through a decorator must still be bounded and must not
    # crash: decorators (MultiplexedProtocol via ProtocolDecorator) do not chain
    # BaseProtocol#initialize, so the depth counter starts unset on that object.
    it 'round-trips a struct through a MultiplexedProtocol' do
      mprot = Thrift::MultiplexedProtocol.new(binary_protocol, 'svc')
      struct_chain(3).write(mprot)
      result = SpecNamespace::RecTree.new
      result.read(mprot)
      expect(tree_depth(result)).to eq(3)
    end
  end

  # Only present when the native (thrift_native) extension is loaded, which
  # also makes Thrift::Struct#read/#write the native implementations -- the path
  # that must enforce the limit in C, not just in pure Ruby.
  if defined? Thrift::BinaryProtocolAccelerated
    describe 'accelerated binary protocol' do
      it 'rejects writing a chain past the limit' do
        pending PENDING_REASON
        prot = Thrift::BinaryProtocolAccelerated.new(Thrift::MemoryBufferTransport.new)
        expect_depth_limit { struct_chain(RECURSION_LIMIT + 1).write(prot) }
      end

      it 'rejects reading a payload past the limit' do
        pending PENDING_REASON
        prot = Thrift::BinaryProtocolAccelerated.new(Thrift::MemoryBufferTransport.new)
        write_raw_tree(prot, RECURSION_LIMIT + 1)
        expect_depth_limit { SpecNamespace::RecTree.new.read(prot) }
      end
    end
  end
end
