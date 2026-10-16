# encoding: ascii-8bit
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

module Thrift
  class MemoryBufferTransport < BaseTransport
    GARBAGE_BUFFER_SIZE = 4 * (2**10) # 4kB
    HEX_BYTES = Array.new(256) { |byte| byte.to_s(16).freeze }.freeze
    private_constant :HEX_BYTES

    # The transport copies the input buffer and keeps its own mutable storage.
    def initialize(buffer = nil)
      @buf = buffer ? Bytes.force_binary_encoding(buffer.dup) : Bytes.empty_byte_buffer
      @index = 0
      @compact_at = 0
    end

    def open?
      true
    end

    def open
    end

    def close
    end

    def peek
      @index < @buf.size
    end

    # this method does not use the passed object directly but copies it
    def reset_buffer(new_buf = "")
      @buf.replace Bytes.force_binary_encoding(new_buf)
      @index = 0
      @compact_at = 0
    end

    def available
      @buf.length - @index
    end

    def read(len)
      raise TransportException.new(TransportException::NEGATIVE_SIZE, "Negative size") unless len >= 0

      data = @buf.slice(@index, len)
      @index += len
      @index = @buf.size if @index > @buf.size
      compact_buffer if @index >= @compact_at
      if data.size < len
        raise EOFError, "Not enough bytes remain in buffer"
      end
      data
    end

    def read_all(size)
      raise TransportException.new(TransportException::NEGATIVE_SIZE, "Negative size") unless size >= 0

      read(size)
    end

    def read_byte
      raise EOFError.new("Not enough bytes remain in buffer") if @index >= @buf.size
      val = Bytes.get_string_byte(@buf, @index)
      @index += 1
      compact_buffer if @index >= @compact_at
      val
    end

    def read_into_buffer(buffer, size)
      i = 0
      while i < size
        raise EOFError.new("Not enough bytes remain in buffer") if @index >= @buf.size

        # The read buffer has some data now, so copy bytes over to the output buffer.
        byte = Bytes.get_string_byte(@buf, @index)
        Bytes.set_string_byte(buffer, i, byte)
        @index += 1
        i += 1
      end
      i
    ensure
      compact_buffer if @index >= @compact_at
    end

    def write(wbuf)
      @buf << Bytes.force_binary_encoding(wbuf)
    end

    def flush
    end

    def inspect_buffer
      out = +""
      idx = 0
      size = @buf.size
      while idx < size
        out << " " unless idx.zero?
        out << "> " if idx == @index
        out << HEX_BYTES[@buf.getbyte(idx)]
        idx += 1
      end
      # Array#join used the first token's encoding: binary only for a leading cursor.
      out.force_encoding(Encoding::US_ASCII) unless @index == 0 && !out.empty?
      out
    end

    def to_s
      "memory"
    end

    private

    def compact_buffer
      remaining = @buf.bytesize - @index
      if remaining == 0
        # Replace rather than clear: even a small suffix can share a large input.
        @buf = Bytes.empty_byte_buffer
        @index = 0
      elsif @index >= GARBAGE_BUFFER_SIZE && @index >= remaining
        # Copy at most as many bytes as have been consumed since the last copy.
        # Appending to fresh storage also detaches a shared suffix immediately.
        @buf = Bytes.empty_byte_buffer << @buf.slice(@index, remaining)
        @index = 0
      end
      # Cache the next boundary for pure Ruby reads. Appends can only move it
      # forward, so an earlier cached boundary stays safe until rechecked.
      size = @buf.bytesize
      half = (size + 1) / 2
      @compact_at = size < GARBAGE_BUFFER_SIZE ? size : (half < GARBAGE_BUFFER_SIZE ? GARBAGE_BUFFER_SIZE : half)
    end
  end
end
