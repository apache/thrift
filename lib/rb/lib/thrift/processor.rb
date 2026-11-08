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

require "logger"
require "thrift/protocol/base_protocol"
require "thrift/transport/base_transport"

module Thrift
  module Processor
    class ArgumentProtocolException < ProtocolException
    end
    private_constant :ArgumentProtocolException

    class ArgumentTransportException < TransportException
    end
    private_constant :ArgumentTransportException

    class ResponseError < StandardError
    end
    private_constant :ResponseError

    def initialize(handler, logger = nil)
      @handler = handler
      if logger.nil?
        @logger = Logger.new(STDERR)
        @logger.level = Logger::WARN
      else
        @logger = logger
      end
    end

    # The optional block receives an error and its source:
    # :malformed_request, :unexpected, or :response_error. It may raise to
    # prevent a fallback reply. With a block, unexpected ONEWAY failures are
    # reported without writing the automatic INTERNAL_ERROR reply.
    def process(iprot, oprot, &on_error)
      name, type, seqid = decode_request(on_error) { iprot.read_message_begin }
      unless type == MessageTypes::CALL || type == MessageTypes::ONEWAY
        on_error.call(Thrift::Exception.new("Invalid message type #{type}"), :malformed_request) if on_error
        iprot.skip(Types::STRUCT)
        iprot.read_message_end
        x = ApplicationException.new(
          ApplicationException::INVALID_MESSAGE_TYPE,
          "Invalid message type #{type} for function #{name}",
        )
        write_error(x, oprot, name, seqid)
        return false
      end

      # A name that is not valid UTF-8 names no method, and respond_to? would
      # raise on it, so it is answered like any other unknown method.
      unless name.valid_encoding? && respond_to?("process_#{name}")
        decode_request(on_error) do
          iprot.skip(Types::STRUCT)
          iprot.read_message_end
        end
        x = ApplicationException.new(ApplicationException::UNKNOWN_METHOD, "Unknown function " + name)
        write_error(x, oprot, name, seqid)
        return false
      end

      begin
        send("process_#{name}", seqid, iprot, oprot)
      rescue ArgumentProtocolException => e
        on_error.call(e, :malformed_request) if on_error
        x = ApplicationException.new(ApplicationException::PROTOCOL_ERROR, e.message)
        write_error(x, oprot, name, seqid)
      rescue ArgumentTransportException => e
        on_error.call(e, :malformed_request) if on_error
        log_error(e.cause)
        x = ApplicationException.new(ApplicationException::INTERNAL_ERROR, "Internal error")
        write_error(x, oprot, name, seqid)
      rescue ResponseError => e
        on_error.call(e.cause, :response_error) if on_error
        log_error(e.cause)
        x = ApplicationException.new(ApplicationException::INTERNAL_ERROR, "Internal error")
        write_error(x, oprot, name, seqid)
      rescue => e
        on_error.call(e, :unexpected) if on_error
        log_error(e)
        return true if on_error && type == MessageTypes::ONEWAY

        x = ApplicationException.new(ApplicationException::INTERNAL_ERROR, "Internal error")
        write_error(x, oprot, name, seqid)
      end
      true
    end

    def read_args(iprot, args_class)
      args = args_class.new
      args.read(iprot)
      iprot.read_message_end
      args
    rescue ProtocolException => e
      raise ArgumentProtocolException.new(e.type, e.message)
    rescue TransportException => e
      raise ArgumentTransportException.new(e.type, e.message)
    end

    def decode_request(on_error)
      yield
    rescue ProtocolException, TransportException => e
      on_error.call(e, :malformed_request) if on_error
      raise
    end
    private :decode_request

    def log_error(error)
      @logger.debug "Internal error : #{error.message}\n#{error.backtrace.join("\n")}"
    end
    private :log_error

    def write_result(result, oprot, name, seqid)
      oprot.write_message_begin(name, MessageTypes::REPLY, seqid)
      result.write(oprot)
      oprot.write_message_end
      oprot.trans.flush
    rescue => e
      # Even a failure before the first byte belongs to response encoding.
      raise ResponseError.new(e.message)
    end

    def write_error(err, oprot, name, seqid)
      oprot.write_message_begin(name, MessageTypes::EXCEPTION, seqid)
      err.write(oprot)
      oprot.write_message_end
      oprot.trans.flush
    end
  end
end
