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
  module Client
    MIN_SEQUENCE_ID = -(2**31)
    MAX_SEQUENCE_ID = (2**31) - 1

    def initialize(iprot, oprot = nil)
      @iprot = iprot
      @oprot = oprot || iprot
      @seqid = 0
      @pending_seqids = []
    end

    def send_message(name, args_class, args = {})
      seqid = next_seqid!
      @oprot.write_message_begin(name, MessageTypes::CALL, seqid)
      send_message_args(args_class, args)
      @pending_seqids << seqid
    end

    def send_oneway_message(name, args_class, args = {})
      @oprot.write_message_begin(name, MessageTypes::ONEWAY, next_seqid!)
      send_message_args(args_class, args)
    end

    def send_message_args(args_class, args)
      data = args_class.new
      args.each do |k, v|
        data.send("#{k.to_s}=", v)
      end
      begin
        data.write(@oprot)
      rescue StandardError => e
        @oprot.trans.close
        raise e
      end
      @oprot.write_message_end
      @oprot.trans.flush
    end

    def receive_message_begin()
      fname, mtype, rseqid = @iprot.read_message_begin
      [fname, mtype, rseqid]
    end

    def reply_seqid(rseqid)
      expected_seqid = dequeue_pending_seqid
      !expected_seqid.nil? && rseqid == expected_seqid
    end

    def validate_message_begin(fname, mtype, rseqid, expected_name)
      expected_seqid = dequeue_pending_seqid

      if mtype == MessageTypes::EXCEPTION
        raise_application_exception
      end

      if mtype != MessageTypes::REPLY
        raise ApplicationException.new(
          ApplicationException::INVALID_MESSAGE_TYPE,
          "#{expected_name} failed: invalid message type"
        )
      end

      if fname != expected_name
        raise ApplicationException.new(
          ApplicationException::WRONG_METHOD_NAME,
          "#{expected_name} failed: wrong method name"
        )
      end

      return if !expected_seqid.nil? && rseqid == expected_seqid

      raise ApplicationException.new(
        ApplicationException::BAD_SEQUENCE_ID,
        "#{expected_name} failed: out of sequence response"
      )
    end

    def receive_message(result_klass)
      result = result_klass.new
      result.read(@iprot)
      @iprot.read_message_end
      result
    end

    def handle_exception(mtype)
      if mtype == MessageTypes::EXCEPTION
        dequeue_pending_seqid
        raise_application_exception
      end
    end

    private

    def next_seqid!
      seqid = @seqid
      @seqid = (seqid == MAX_SEQUENCE_ID) ? MIN_SEQUENCE_ID : seqid + 1
      seqid
    end

    def dequeue_pending_seqid
      @pending_seqids.shift
    end

    def raise_application_exception
      x = ApplicationException.new
      x.read(@iprot)
      @iprot.read_message_end
      raise x
    end
  end
end
