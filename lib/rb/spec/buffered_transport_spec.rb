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

describe Thrift::BufferedTransport do
  let(:sink) { double("Transport") }
  let(:transport) { described_class.new(sink, write_buffer_size: 4) }

  it "keeps unlimited buffering by default and with an explicit nil size" do
    expect(sink).not_to receive(:write)
    [described_class.new(sink), described_class.new(sink, write_buffer_size: nil)].each do |buffered|
      payload = "x" * (1024 * 1024)
      buffered.write(payload)
      expect(buffered.instance_variable_get(:@wbuf)).to eq(payload)
    end
  end

  it "requires a positive integer write buffer size" do
    [0, -1, false, 1.5, "4"].each do |size|
      expect { described_class.new(sink, write_buffer_size: size) }.to raise_error(ArgumentError, "write_buffer_size must be a positive Integer or nil")
    end
  end

  it "coalesces small writes and drains a full buffer without flushing downstream" do
    transport.write("a")
    transport.write("b")
    transport.write("cd")
    expect(sink).to receive(:write).with("abcd").ordered
    transport.write("e")
    expect(sink).to receive(:write).with("e").ordered
    expect(sink).to receive(:flush).ordered
    transport.flush
  end

  it "drains a partial buffer before bypassing it for a large write" do
    transport.write("ab")
    expect(sink).to receive(:write).with("ab").ordered
    expect(sink).to receive(:write).with("cdefgh").ordered
    transport.write("cdefgh")
    expect(sink).to receive(:flush).ordered
    transport.flush
  end

  it "bypasses the local buffer for a write equal to its size" do
    expect(sink).to receive(:write).with("abcd")
    transport.write("abcd")
    expect(transport.instance_variable_get(:@wbuf)).to be_empty
  end

  it "bounds local staging across many small, overflowing and large writes" do
    output = +""
    allow(sink).to receive(:write) { |buf| output << buf }
    allow(sink).to receive(:flush)
    chunks = ["a", "bc", "def", "ghij", "klmnop", ""] * 100
    chunks.each do |chunk|
      transport.write(chunk)
      expect(transport.instance_variable_get(:@wbuf).bytesize).to be <= 4
    end
    transport.flush
    expect(output).to eq(chunks.join)
  end

  it "does not drain a full buffer for an empty write" do
    transport.write("ab")
    transport.write("cd")
    transport.write("")
    expect(sink).to receive(:write).with("abcd")
    expect(sink).to receive(:flush)
    transport.flush
  end

  it "gives downstream its own mutable binary string for bypassed input" do
    ["éé", +"éé", "abcd".b.freeze, +"abcd".b].each do |input|
      original = input.dup
      expect(sink).to receive(:write) do |buf|
        expect(buf).not_to equal(input)
        expect(buf.encoding).to eq(Encoding::BINARY)
        buf.replace("changed")
      end
      transport.write(input)
      expect(input.bytes).to eq(original.bytes)
    end
  end

  it "does not reuse a drained string retained by a custom transport" do
    writes = []
    allow(sink).to receive(:write) { |buf| writes << buf }
    allow(sink).to receive(:flush)
    transport.write("ab")
    transport.write("cde")
    transport.flush
    expect(writes).to eq(["ab", "cde"])
  end

  it "normalizes a String subclass without invoking its copy hook" do
    string_class = Class.new(String) do
      def initialize_copy(other)
        super
        replace("copy hook")
      end
    end
    input = string_class.new("abcdef").force_encoding(Encoding::BINARY)
    expect(sink).to receive(:write) do |buf|
      expect(buf).to be_instance_of(String)
      expect(buf).to eq("abcdef")
    end
    transport.write(input)
    expect(input).to eq("abcdef")
  end

  it "retains a failed drain and accepts no bytes from the next write" do
    transport.write("abc")
    expect(sink).to receive(:write).with("abc").ordered.and_raise(IOError, "write failed")
    expect { transport.write("de") }.to raise_error(IOError, "write failed")
    expect(sink).to receive(:write).with("abc").ordered
    transport.write("de")
    expect(sink).to receive(:write).with("de").ordered
    expect(sink).to receive(:flush).ordered
    transport.flush
  end

  it "leaves a failed bypassed write to the caller to retry" do
    expect(sink).to receive(:write).with("abcdef").ordered.and_raise(IOError, "write failed")
    expect { transport.write("abcdef") }.to raise_error(IOError, "write failed")
    expect(transport.instance_variable_get(:@wbuf)).to be_empty
    expect(sink).to receive(:write).with("abcdef").ordered
    transport.write("abcdef")
  end

  it "does not automatically replay a partially accepted direct write" do
    accepted = +""
    expect(sink).to receive(:write).with("abcdef") do |buf|
      accepted << buf.byteslice(0, 2)
      raise IOError, "partial write"
    end
    expect { transport.write("abcdef") }.to raise_error(IOError, "partial write")
    expect(sink).to receive(:flush)
    transport.flush
    expect(accepted).to eq("ab")
  end

  it "retains an unsuccessful final write for a later flush" do
    transport.write("abc")
    expect(sink).to receive(:write).with("abc").ordered.and_raise(IOError)
    expect { transport.flush }.to raise_error(IOError)
    expect(sink).to receive(:write).with("abc").ordered
    expect(sink).to receive(:flush).ordered
    transport.flush
  end

  it "does not resend successful writes after a downstream flush failure" do
    transport.write("abc")
    expect(sink).to receive(:write).with("abc").once.ordered
    expect(sink).to receive(:flush).ordered.and_raise(IOError)
    expect { transport.flush }.to raise_error(IOError)
    expect(sink).to receive(:flush).ordered
    transport.flush
  end

  it "preserves one frame when wrapping a framed transport" do
    framed = Thrift::FramedTransport.new(sink)
    buffered = described_class.new(framed, write_buffer_size: 4)
    buffered.write("abcdef")
    buffered.write("gh")
    expect(sink).to receive(:write).with([8].pack("N") + "abcdefgh").ordered
    expect(sink).to receive(:flush).ordered
    buffered.flush
  end

  it "preserves one frame when a framed transport wraps it" do
    framed = Thrift::FramedTransport.new(transport)
    framed.write("abcdef")
    framed.write("gh")
    expect(sink).to receive(:write).with([8].pack("N") + "abcdefgh").ordered
    expect(sink).to receive(:flush).ordered
    framed.flush
  end

  it "only flushes nested buffered transports on explicit flush" do
    inner = described_class.new(sink, write_buffer_size: 2)
    outer = described_class.new(inner, write_buffer_size: 4)
    expect(sink).to receive(:write).with("abcdef").ordered
    outer.write("abcdef")
    outer.write("g")
    expect(sink).to receive(:write).with("g").ordered
    expect(sink).to receive(:flush).ordered
    outer.flush
  end

  it "only sends an HTTP request on explicit flush" do
    http_transport = Thrift::HTTPClientTransport.new("http://localhost/service")
    buffered = described_class.new(http_transport, write_buffer_size: 4)
    buffered.write("abcdef")
    buffered.write("gh")
    http = double("HTTP", :use_ssl= => nil)
    expect(Net::HTTP).to receive(:new).with("localhost", 80).and_return(http)
    expect(http).to receive(:post).with("/service", "abcdefgh", anything).and_return(double("Response", code: "200", body: ""))
    buffered.flush
  end

  it "uses write hooks in subclasses without invoking flush while draining" do
    subclass = Class.new(described_class) do
      attr_reader :write_calls

      def write(buf)
        @write_calls = (@write_calls || 0) + 1
        super
      end

      def flush
        raise "explicit flush hook"
      end
    end
    buffered = subclass.new(sink, write_buffer_size: 4)
    expect(sink).to receive(:write).with("abcdef")
    buffered.write("abcdef")
    expect(buffered.write_calls).to eq(1)
    expect { buffered.flush }.to raise_error("explicit flush hook")
  end

  it "drains remaining data and flushes downstream before close" do
    transport.write("a")
    expect(sink).to receive(:write).with("a").ordered
    expect(sink).to receive(:flush).ordered
    expect(sink).to receive(:close).ordered
    transport.close
  end
end

describe Thrift::BufferedTransportFactory do
  it "passes the configured write buffer size to each transport" do
    sink = double("Transport")
    factory = described_class.new(write_buffer_size: 4)
    2.times do
      buffered = factory.get_transport(sink)
      expect(sink).to receive(:write).with("abcd")
      buffered.write("abcd")
    end
  end

  it "rejects an invalid write buffer size before accepting transports" do
    expect { described_class.new(write_buffer_size: 0) }.to raise_error(ArgumentError)
  end
end
