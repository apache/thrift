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

# The binary and compact protocols read a string's length off the wire and ask
# the transport for that many bytes. These specs hold them to a maximum string
# size, and check that a refused length is never asked of the transport.
describe "The maximum string size" do
  class StringLimitRecordingTransport < Thrift::MemoryBufferTransport
    attr_reader :requested

    def initialize(*args)
      super
      @requested = []
    end

    def read_all(size)
      @requested << size
      super
    end
  end

  let(:limit) { 16_384_000 }
  let(:huge) { 0x7fffffff }

  def transport_with(bytes)
    trans = StringLimitRecordingTransport.new
    trans.write(bytes)
    trans
  end

  def expect_refused_unread(trans, declared)
    expect { yield }.to raise_error(Thrift::ProtocolException) { |e|
      expect(e.type).to eq(Thrift::ProtocolException::SIZE_LIMIT)
    }
    expect(trans.requested.select { |size| size >= declared }).to be_empty
  end

  # The transport holds only the length, so a read the protocol lets through
  # runs out of data; what matters is the size the transport was asked for.
  def read_ignoring_missing_data
    yield
  rescue EOFError, Thrift::TransportException
    nil
  end

  it "is exported as BaseProtocol::DEFAULT_MAX_STRING_SIZE" do
    expect(Thrift::BaseProtocol.const_defined?(:DEFAULT_MAX_STRING_SIZE) && Thrift::BaseProtocol::DEFAULT_MAX_STRING_SIZE).to eq(limit)
  end

  protocols = {
    "BinaryProtocol" => [Thrift::BinaryProtocol, Thrift::BinaryProtocolFactory, :binary],
    "CompactProtocol" => [Thrift::CompactProtocol, Thrift::CompactProtocolFactory, :compact],
  }
  if defined?(Thrift::BinaryProtocolAccelerated)
    protocols["BinaryProtocolAccelerated"] = [Thrift::BinaryProtocolAccelerated, Thrift::BinaryProtocolAcceleratedFactory, :binary]
  end

  protocols.each do |name, (protocol_class, factory_class, encoding)|
    describe name do
      define_method(:length_bytes) do |size|
        if encoding == :binary
          [size].pack("N")
        else
          out = []
          loop do
            byte = size & 0x7f
            size >>= 7
            out << (size.zero? ? byte : (byte | 0x80))
            break if size.zero?
          end
          out.pack("C*")
        end
      end

      define_method(:protocol) do |trans, **options|
        if encoding == :binary
          protocol_class.new(trans, true, true, **options)
        else
          protocol_class.new(trans, **options)
        end
      end

      it "defaults max_string_size to the frame size limit" do
        expect(protocol(transport_with("")).max_string_size).to eq(limit)
      end

      it "reads a string within the default maximum" do
        expect(protocol(transport_with(length_bytes(5) + "hello")).read_binary).to eq("hello")
      end

      it "refuses a string over the default maximum without asking for it" do
        trans = transport_with(length_bytes(huge))
        prot = protocol(trans)
        expect_refused_unread(trans, huge) { prot.read_binary }
      end

      it "refuses one byte over the maximum and asks for a string at it" do
        trans = transport_with(length_bytes(limit + 1))
        prot = protocol(trans)
        expect_refused_unread(trans, limit + 1) { prot.read_binary }
        trans = transport_with(length_bytes(limit))
        read_ignoring_missing_data { protocol(trans).read_binary }
        expect(trans.requested).to include(limit)
      end

      it "honours a caller-set maximum" do
        trans = transport_with(length_bytes(101) + ("x" * 101))
        prot = protocol(trans, max_string_size: 100)
        expect_refused_unread(trans, 101) { prot.read_binary }
        trans = transport_with(length_bytes(100) + ("x" * 100))
        expect(protocol(trans, max_string_size: 100).read_binary).to eq("x" * 100)
      end

      it "reads any length when the maximum is nil" do
        trans = transport_with(length_bytes(limit + 1))
        read_ignoring_missing_data { protocol(trans, max_string_size: nil).read_binary }
        expect(trans.requested).to include(limit + 1)
      end

      it "rejects a maximum that is not a positive Integer" do
        expect { protocol(transport_with(""), max_string_size: 0) }.to raise_error(ArgumentError, /must be nil or a positive Integer/)
      end

      it "passes a factory's maximum to the protocol" do
        expect(factory_class.new(max_string_size: 100).get_protocol(transport_with("")).max_string_size).to eq(100)
      end

      if encoding == :binary
        it "bounds the method name of an old-style message header" do
          trans = transport_with(length_bytes(huge))
          prot = protocol_class.new(trans, false, true)
          expect_refused_unread(trans, huge) { prot.read_message_begin }
          trans = transport_with(length_bytes(101) + ("x" * 101) + [1].pack("c") + [7].pack("N"))
          prot = protocol_class.new(trans, false, true, max_string_size: 100)
          expect_refused_unread(trans, 101) { prot.read_message_begin }
        end
      end
    end
  end
end
