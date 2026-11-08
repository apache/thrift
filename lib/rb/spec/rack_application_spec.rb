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
require "rack/test"
require "thrift/server/rack_application"
require "reverse_order_service"

describe Thrift::RackApplication do
  include Rack::Test::Methods

  let(:processor) { double("processor") }
  let(:protocol_factory) { double("protocol factory") }
  let(:protocol) { double("protocol") }

  subject(:app) { Thrift::RackApplication.new(processor, protocol_factory) }

  def request_body(name, args, type = Thrift::MessageTypes::CALL)
    transport = Thrift::MemoryBufferTransport.new
    protocol = protocol_factory.get_protocol(transport)
    protocol.write_message_begin(name, type, 7)
    args.write(protocol)
    protocol.write_message_end
    protocol.trans.flush
    transport.read(transport.available)
  end

  def expect_internal_error(name)
    expect(last_response.status).to eq 200
    expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
    transport = Thrift::MemoryBufferTransport.new(last_response.body)
    protocol = protocol_factory.get_protocol(transport)
    expect(protocol.read_message_begin).to eq [name, Thrift::MessageTypes::EXCEPTION, 7]
    error = Thrift::ApplicationException.new
    error.read(protocol)
    protocol.read_message_end
    expect(error.type).to eq Thrift::ApplicationException::INTERNAL_ERROR
    expect(error.message).to eq "Internal error"
    expect(transport.available).to eq 0
  end

  context "404 response" do
    it "receives a non-POST" do
      header("Content-Type", "application/x-thrift")
      get "/"
      expect(last_response.status).to eq 404
      expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
    end

    it "receives a header other than application/x-thrift" do
      header("Content-Type", "application/json")
      post "/"
      expect(last_response.status).to eq 404
      expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
    end
  end

  context "200 response" do
    before do
      allow(protocol_factory).to receive(:get_protocol).and_return(protocol)
      allow(processor).to receive(:process)
    end

    it "creates an IOStreamTransport" do
      header("Content-Type", "application/x-thrift")
      expect(Thrift::IOStreamTransport).to receive(:new).with(an_object_responding_to(:read), an_instance_of(Rack::Response))
      post "/"
    end

    it "fetches the right protocol based on the Transport" do
      header("Content-Type", "application/x-thrift")
      expect(protocol_factory).to receive(:get_protocol).with(an_instance_of(Thrift::IOStreamTransport))
      post "/"
    end

    it "uses the same protocol for input and output" do
      header("Content-Type", "application/x-thrift")
      expect(processor).to receive(:process).with(protocol, protocol)
      post "/"
    end

    it "status code 200" do
      header("Content-Type", "application/x-thrift")
      post "/"
      expect(last_response.ok?).to be true
    end

    it "accepts content type parameters" do
      header("Content-Type", "application/x-thrift; charset=binary")
      post "/"
      expect(last_response.ok?).to be true
    end
  end

  context "with HeaderProtocol" do
    let(:protocol_factory) { Thrift::HeaderProtocolFactory.new }

    before do
      allow(processor).to receive(:process) do |input_protocol, output_protocol|
        expect(output_protocol).to equal(input_protocol)

        name, _type, seqid = input_protocol.read_message_begin
        input_protocol.read_struct_begin
        _field_name, field_type, _field_id = input_protocol.read_field_begin
        expect(field_type).to eq Thrift::Types::STOP
        input_protocol.read_struct_end
        input_protocol.read_message_end

        output_protocol.write_message_begin(name, Thrift::MessageTypes::REPLY, seqid)
        output_protocol.write_struct_begin("result")
        output_protocol.write_field_stop
        output_protocol.write_struct_end
        output_protocol.write_message_end
        output_protocol.trans.flush
      end
    end

    it "preserves legacy framed binary transport in the response" do
      request_transport = Thrift::MemoryBufferTransport.new
      request_protocol = Thrift::BinaryProtocol.new(request_transport)
      request_protocol.write_message_begin("legacy_framed", Thrift::MessageTypes::CALL, 7)
      request_protocol.write_struct_begin("args")
      request_protocol.write_field_stop
      request_protocol.write_struct_end
      request_protocol.write_message_end
      payload = request_transport.read(request_transport.available)

      header("Content-Type", "application/x-thrift")
      post "/", [payload.bytesize].pack("N") + payload

      frame_size = last_response.body.unpack1("N")
      response_transport = Thrift::MemoryBufferTransport.new(last_response.body.byteslice(4, frame_size))
      response_protocol = Thrift::BinaryProtocol.new(response_transport)
      expect(response_protocol.read_message_begin).to eq ["legacy_framed", Thrift::MessageTypes::REPLY, 7]
    end
  end

  context "HTTP response boundary" do
    let(:handler) { double("handler") }
    let(:processor) { SpecNamespace::NonblockingService::Processor.new(handler) }
    let(:protocol_factory) { Thrift::BinaryProtocolFactory.new }

    before { header("Content-Type", "application/x-thrift") }

    it "returns HTTP 400 without a Thrift body for a malformed message header" do
      post "/", "\xff\xff\xff\xff".b

      expect(last_response.status).to eq 400
      expect(last_response.body).to be_empty
      expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
      expect(last_request.env["rack.exception"]).to be_nil
    end

    context "with JSON" do
      let(:protocol_factory) { Thrift::JsonProtocolFactory.new }
      let(:processor) { Thrift::Test::ReverseOrderService::Processor.new(handler) }

      it "returns an unreported 400 for truncated headers and string arguments" do
        body = request_body("myMethod", Thrift::Test::ReverseOrderService::MyMethod_args.new(first: "hello" * 5))
        payloads = ['[1,"s'] + (0...body.bytesize).map { |size| body.byteslice(0, size) }
        expect(handler).not_to receive(:myMethod)

        payloads.each do |payload|
          post "/", payload

          expect(last_response.status).to eq(400), "Unexpected status for #{payload.inspect}"
          expect(last_response.body).to be_empty
          expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
          expect(last_request.env["rack.exception"]).to be_nil
        end
      end
    end

    it "returns HTTP 400 for an invalid request message type" do
      request_transport = Thrift::MemoryBufferTransport.new
      request_protocol = Thrift::BinaryProtocol.new(request_transport)
      request_protocol.write_message_begin("sleep", Thrift::MessageTypes::REPLY, 7)
      SpecNamespace::NonblockingService::Sleep_args.new(seconds: 3.0).write(request_protocol)
      request_protocol.write_message_end

      expect(handler).not_to receive(:sleep)
      post "/", request_transport.read(request_transport.available)

      expect(last_response.status).to eq 400
      expect(last_response.body).to be_empty
      expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
    end

    it "returns HTTP 400 without a Thrift body for truncated oneway arguments" do
      request_transport = Thrift::MemoryBufferTransport.new
      request_protocol = Thrift::BinaryProtocol.new(request_transport)
      request_protocol.write_message_begin("unblock", Thrift::MessageTypes::ONEWAY, 7)

      expect(handler).not_to receive(:unblock)
      post "/", request_transport.read(request_transport.available)

      expect(last_response.status).to eq 400
      expect(last_response.body).to be_empty
      expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
      expect(last_request.env["rack.exception"]).to be_nil
    end

    it "returns HTTP 400 without a Thrift body for truncated call arguments" do
      request_transport = Thrift::MemoryBufferTransport.new
      request_protocol = Thrift::BinaryProtocol.new(request_transport)
      request_protocol.write_message_begin("sleep", Thrift::MessageTypes::CALL, 7)

      expect(handler).not_to receive(:sleep)
      post "/", request_transport.read(request_transport.available)

      expect(last_response.status).to eq 400
      expect(last_response.body).to be_empty
      expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
    end

    it "returns HTTP 200 without a Thrift body for a valid oneway call" do
      request_transport = Thrift::MemoryBufferTransport.new
      request_protocol = Thrift::BinaryProtocol.new(request_transport)
      request_protocol.write_message_begin("unblock", Thrift::MessageTypes::ONEWAY, 7)
      SpecNamespace::NonblockingService::Unblock_args.new(n: 1).write(request_protocol)
      request_protocol.write_message_end

      expect(handler).to receive(:unblock).with(1)
      post "/", request_transport.read(request_transport.available)

      expect(last_response.status).to eq 200
      expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
      expect(last_response.body).to be_empty
    end

    it "lets outer reporting middleware observe a handled error without replacing the reply" do
      error = RuntimeError.new("handler failed")
      captured_errors = []
      thrift_app = app
      reporting_app = lambda do |env|
        response = thrift_app.call(env)
        captured_errors << env["rack.exception"] if env["rack.exception"]
        response
      end
      expect(handler).to receive(:sleep).with(3.0).and_raise(error)
      response = Rack::Test::Session.new(reporting_app).post(
        "/", request_body("sleep", SpecNamespace::NonblockingService::Sleep_args.new(seconds: 3.0)),
        {"CONTENT_TYPE" => "application/x-thrift"},
      )
      expect(response.status).to eq 200
      protocol = Thrift::BinaryProtocol.new(Thrift::MemoryBufferTransport.new(response.body))
      expect(protocol.read_message_begin).to eq ["sleep", Thrift::MessageTypes::EXCEPTION, 7]
      exception = Thrift::ApplicationException.new
      exception.read(protocol)
      expect(exception.type).to eq Thrift::ApplicationException::INTERNAL_ERROR
      expect(captured_errors.size).to eq 1
      expect(captured_errors.first).to equal error
    end

    [
      Thrift::ProtocolException.new(Thrift::ProtocolException::INVALID_DATA, "handler failed"),
      Thrift::TransportException.new(Thrift::TransportException::END_OF_FILE, "handler EOF"),
      Thrift::TransportException.new(Thrift::TransportException::UNKNOWN, "handler transport failure"),
    ].each do |error|
      it "does not treat a handler-raised #{error.class} as malformed input" do
        expect(handler).to receive(:sleep).with(3.0).and_raise(error)
        post "/", request_body("sleep", SpecNamespace::NonblockingService::Sleep_args.new(seconds: 3.0))

        expect_internal_error("sleep")
        expect(last_request.env["rack.exception"]).to equal error
      end
    end

    [Thrift::MessageTypes::CALL, Thrift::MessageTypes::ONEWAY].each do |type|
      {
        "unblock" => SpecNamespace::NonblockingService::Unblock_args.new(n: 1),
        "sleep" => SpecNamespace::NonblockingService::Sleep_args.new(seconds: 3.0),
      }.each do |name, args|
        it "uses the #{type} envelope for failed #{name} fallback suppression" do
          error = RuntimeError.new("handler failed")
          expect(handler).to receive(name).and_raise(error)
          post "/", request_body(name, args, type)

          if type == Thrift::MessageTypes::CALL
            expect_internal_error(name)
          else
            expect(last_response.status).to eq 200
            expect(last_response.body).to be_empty
          end
          expect(last_request.env["rack.exception"]).to equal error
        end
      end
    end
  end

  [
    Thrift::BinaryProtocolFactory, Thrift::CompactProtocolFactory,
    Thrift::JsonProtocolFactory, Thrift::HeaderProtocolFactory,
  ].each do |factory_class|
    context "generated service with #{factory_class}" do
      let(:handler) { double("handler") }
      let(:processor) { Thrift::Test::Srv::Processor.new(handler) }
      let(:protocol_factory) { factory_class.new }

      before { header("Content-Type", "application/x-thrift") }

      it "preserves a valid result and an existing environment exception" do
        previous = RuntimeError.new("outer error")
        env "rack.exception", previous
        expect(handler).to receive(:primitiveMethod).and_return(23)
        post "/", request_body("primitiveMethod", Thrift::Test::Srv::PrimitiveMethod_args.new)

        expect(last_response.status).to eq 200
        protocol = protocol_factory.get_protocol(Thrift::MemoryBufferTransport.new(last_response.body))
        expect(protocol.read_message_begin).to eq ["primitiveMethod", Thrift::MessageTypes::REPLY, 7]
        result = Thrift::Test::Srv::PrimitiveMethod_result.new
        result.read(protocol)
        protocol.read_message_end
        expect(result.success).to eq 23
        expect(last_request.env["rack.exception"]).to equal previous
      end

      it "returns a complete generic error and retains the original handler exception" do
        error = RuntimeError.new("private handler detail")
        expect(handler).to receive(:primitiveMethod).and_raise(error)
        post "/", request_body("primitiveMethod", Thrift::Test::Srv::PrimitiveMethod_args.new)

        expect_internal_error("primitiveMethod")
        expect(last_request.env["rack.exception"]).to equal error
      end

      it "does not report a declared IDL exception as unexpected" do
        error = Thrift::Test::ExceptionWithAMap.new(blah: "declared", map_field: {"key" => "value"})
        expect(handler).to receive(:declaredExceptionMethod).with(true).and_raise(error)
        post "/", request_body("declaredExceptionMethod", Thrift::Test::Srv::DeclaredExceptionMethod_args.new(shouldThrow: true))

        expect(last_response.status).to eq 200
        protocol = protocol_factory.get_protocol(Thrift::MemoryBufferTransport.new(last_response.body))
        expect(protocol.read_message_begin).to eq ["declaredExceptionMethod", Thrift::MessageTypes::REPLY, 7]
        result = Thrift::Test::Srv::DeclaredExceptionMethod_result.new
        result.read(protocol)
        protocol.read_message_end
        expect(result.xwamap).to eq error
        expect(last_request.env["rack.exception"]).to be_nil
      end

      it "discards a completed reply if its flush fails, even for an ONEWAY envelope" do
        body = request_body("primitiveMethod", Thrift::Test::Srv::PrimitiveMethod_args.new, Thrift::MessageTypes::ONEWAY)
        error = IOError.new("flush failed")
        expect(handler).to receive(:primitiveMethod).and_return(23)
        allow(protocol_factory).to receive(:get_protocol).and_wrap_original do |original, transport|
          protocol = original.call(transport)
          allow(protocol.trans).to receive(:flush).and_raise(error)
          protocol
        end
        post "/", body

        expect(last_response.status).to eq 500
        expect(last_response.body).to be_empty
        expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
        expect(last_request.env["rack.exception"]).to equal error
      end
    end
  end

  context "response encoding boundary" do
    let(:handler) { double("handler") }
    let(:processor) { Thrift::Test::Srv::Processor.new(handler) }
    let(:protocol_factory) { Thrift::BinaryProtocolFactory.new }

    before { header("Content-Type", "application/x-thrift") }

    [true, false].each do |type_checking|
      it "classifies an invalid i32 return at the runtime boundary with type checking #{type_checking}" do
        Thrift.type_checking = type_checking
        expect(handler).to receive(:primitiveMethod).and_return("not an integer")
        post "/", request_body("primitiveMethod", Thrift::Test::Srv::PrimitiveMethod_args.new)

        if type_checking
          expect_internal_error("primitiveMethod")
          expect(last_request.env["rack.exception"]).to be_a Thrift::TypeError
        else
          expect(last_response.status).to eq 500
          expect(last_response.body).to be_empty
          expect(last_request.env["rack.exception"]).to be_a TypeError
        end
      end
    end

    [:write_message_begin, :write_message_end].each do |stage|
      it "returns a fresh 500 for #{stage} failure rather than classifying by exception type or byte count" do
        body = request_body("primitiveMethod", Thrift::Test::Srv::PrimitiveMethod_args.new)
        error = Thrift::ProtocolException.new(Thrift::ProtocolException::INVALID_DATA, "writer failed")
        expect(handler).to receive(:primitiveMethod).and_return(23)
        allow(protocol_factory).to receive(:get_protocol).and_wrap_original do |original, transport|
          protocol = original.call(transport)
          failed = false
          allow(protocol).to receive(stage).and_wrap_original do |write, *args|
            unless failed
              failed = true
              raise error
            end
            write.call(*args)
          end
          protocol
        end
        post "/", body

        expect(last_response.status).to eq 500
        expect(last_response.body).to be_empty
        expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
        expect(last_request.env["rack.exception"]).to equal error
      end
    end

    it "reports the failed error writer and preserves the original handler cause without another fallback" do
      body = request_body("primitiveMethod", Thrift::Test::Srv::PrimitiveMethod_args.new)
      handler_error = RuntimeError.new("handler failed")
      writer_error = IOError.new("error writer failed")
      expect(handler).to receive(:primitiveMethod).and_raise(handler_error)
      allow(protocol_factory).to receive(:get_protocol).and_wrap_original do |original, transport|
        protocol = original.call(transport)
        allow(protocol).to receive(:write_message_end).and_raise(writer_error)
        protocol
      end
      post "/", body

      expect(last_response.status).to eq 500
      expect(last_response.body).to be_empty
      expect(last_request.env["rack.exception"]).to equal writer_error
      expect(writer_error.cause).to equal handler_error
    end
  end

  context "HeaderProtocol response boundary" do
    let(:handler) { double("handler") }
    let(:processor) { SpecNamespace::NonblockingService::Processor.new(handler) }
    let(:protocol_factory) { Thrift::HeaderProtocolFactory.new }

    before { header("Content-Type", "application/x-thrift") }

    {
      "oversized frame" => "\xff\xff\xff\xff".b,
      "undersized frame" => [2].pack("N") + "\x00\x00".b,
      "unknown transport" => [8, 0x12345678, 0].pack("N*"),
    }.each do |description, payload|
      it "returns an unreported HTTP 400 for an #{description}" do
        post "/", payload

        expect(last_response.status).to eq 400
        expect(last_response.body).to be_empty
        expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
        expect(last_request.env["rack.exception"]).to be_nil
      end
    end

    [Thrift::BinaryProtocol, Thrift::CompactProtocol].each do |protocol_class|
      {
        "sleep" => Thrift::MessageTypes::CALL,
        "unblock" => Thrift::MessageTypes::ONEWAY,
      }.each do |name, type|
        it "returns an unreported HTTP 400 for truncated unframed #{protocol_class} #{name} arguments" do
          transport = Thrift::MemoryBufferTransport.new
          protocol_class.new(transport).write_message_begin(name, type, 7)

          expect(handler).not_to receive(name)
          post "/", transport.read(transport.available)

          expect(last_response.status).to eq 400
          expect(last_response.body).to be_empty
          expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
          expect(last_request.env["rack.exception"]).to be_nil
        end
      end
    end

    it "returns an unreported HTTP 400 for an invalid frame while reading arguments" do
      transport = Thrift::MemoryBufferTransport.new
      Thrift::BinaryProtocol.new(transport).write_message_begin("sleep", Thrift::MessageTypes::CALL, 7)
      payload = transport.read(transport.available)

      expect(handler).not_to receive(:sleep)
      post "/", [payload.bytesize].pack("N") + payload + [2].pack("N") + "\x00\x00".b

      expect(last_response.status).to eq 400
      expect(last_response.body).to be_empty
      expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
      expect(last_request.env["rack.exception"]).to be_nil
    end

    it "returns HTTP 400 for truncated framed oneway arguments" do
      request_transport = Thrift::MemoryBufferTransport.new
      request_protocol = Thrift::BinaryProtocol.new(request_transport)
      request_protocol.write_message_begin("unblock", Thrift::MessageTypes::ONEWAY, 7)
      payload = request_transport.read(request_transport.available)

      header("Content-Type", "application/x-thrift")
      expect(handler).not_to receive(:unblock)
      post "/", [payload.bytesize].pack("N") + payload

      expect(last_response.status).to eq 400
      expect(last_response.body).to be_empty
    end
  end

  context "mapped application" do
    subject(:app) { Thrift::RackApplication.mapped("/", processor, protocol_factory) }

    before do
      allow(protocol_factory).to receive(:get_protocol).and_return(protocol)
      allow(processor).to receive(:process)
    end

    it "sets the content length for an empty response" do
      header("Content-Type", "application/x-thrift")
      post "/"
      expect(last_response.headers["Content-Length"]).to eq "0"
    end

    it "returns a generic empty 500 with zero content length when a custom processor fails" do
      error = RuntimeError.new("processor failed")
      allow(processor).to receive(:process).and_raise(error)

      header("Content-Type", "application/x-thrift")
      post "/"
      expect(last_response.status).to eq 500
      expect(last_response.body).to be_empty
      expect(last_response.headers["Content-Type"]).to eq "application/x-thrift"
      expect(last_response.headers["Content-Length"]).to eq "0"
      expect(last_request.env["rack.exception"]).to equal error
    end

    it "returns zero content length for a failed ONEWAY request" do
      handler = double("handler")
      error = RuntimeError.new("oneway failed")
      real_processor = SpecNamespace::NonblockingService::Processor.new(handler)
      real_factory = Thrift::BinaryProtocolFactory.new
      transport = Thrift::MemoryBufferTransport.new
      input = real_factory.get_protocol(transport)
      input.write_message_begin("shutdown", Thrift::MessageTypes::ONEWAY, 7)
      SpecNamespace::NonblockingService::Shutdown_args.new.write(input)
      input.write_message_end
      expect(handler).to receive(:shutdown).and_raise(error)
      response = Rack::MockRequest.new(Thrift::RackApplication.mapped("/", real_processor, real_factory)).post(
        "/", "CONTENT_TYPE" => "application/x-thrift", input: transport.read(transport.available),
      )

      expect(response.status).to eq 200
      expect(response.body).to be_empty
      expect(response.headers["Content-Length"]).to eq "0"
    end
  end
end
