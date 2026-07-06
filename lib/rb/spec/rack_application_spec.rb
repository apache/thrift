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
require 'rack/test'
require 'thrift/server/rack_application'

describe Thrift::RackApplication do
  include Rack::Test::Methods

  let(:processor) { double('processor') }
  let(:protocol_factory) { double('protocol factory') }
  let(:protocol) { double('protocol') }

  subject(:app) { Thrift::RackApplication.new(processor, protocol_factory) }

  context "404 response" do
    it 'receives a non-POST' do
      header('Content-Type', "application/x-thrift")
      get "/"
      expect(last_response.status).to eq 404
    end

    it 'receives a header other than application/x-thrift' do
      header('Content-Type', "application/json")
      post "/"
      expect(last_response.status).to eq 404
    end
  end

  context "200 response" do
    before do
      allow(protocol_factory).to receive(:get_protocol).and_return(protocol)
      allow(processor).to receive(:process)
    end

    it 'creates an IOStreamTransport' do
      header('Content-Type', "application/x-thrift")
      expect(Thrift::IOStreamTransport).to receive(:new).with(an_object_responding_to(:read), an_instance_of(Rack::Response))
      post "/"
    end

    it 'fetches the right protocol based on the Transport' do
      header('Content-Type', "application/x-thrift")
      expect(protocol_factory).to receive(:get_protocol).with(an_instance_of(Thrift::IOStreamTransport))
      post "/"
    end

    it 'uses the same protocol for input and output' do
      header('Content-Type', "application/x-thrift")
      expect(processor).to receive(:process).with(protocol, protocol)
      post "/"
    end

    it 'status code 200' do
      header('Content-Type', "application/x-thrift")
      post "/"
      expect(last_response.ok?).to be true
    end

    it 'accepts content type parameters' do
      header('Content-Type', "application/x-thrift; charset=binary")
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
        output_protocol.write_struct_begin('result')
        output_protocol.write_field_stop
        output_protocol.write_struct_end
        output_protocol.write_message_end
        output_protocol.trans.flush
      end
    end

    it 'preserves legacy framed binary transport in the response' do
      request_transport = Thrift::MemoryBufferTransport.new
      request_protocol = Thrift::BinaryProtocol.new(request_transport)
      request_protocol.write_message_begin('legacy_framed', Thrift::MessageTypes::CALL, 7)
      request_protocol.write_struct_begin('args')
      request_protocol.write_field_stop
      request_protocol.write_struct_end
      request_protocol.write_message_end
      payload = request_transport.read(request_transport.available)

      header('Content-Type', "application/x-thrift")
      post "/", [payload.bytesize].pack('N') + payload

      frame_size = last_response.body.unpack1('N')
      response_transport = Thrift::MemoryBufferTransport.new(last_response.body.byteslice(4, frame_size))
      response_protocol = Thrift::BinaryProtocol.new(response_transport)
      expect(response_protocol.read_message_begin).to eq ['legacy_framed', Thrift::MessageTypes::REPLY, 7]
    end
  end

  context "mapped application" do
    subject(:app) { Thrift::RackApplication.mapped('/', processor, protocol_factory) }

    before do
      allow(protocol_factory).to receive(:get_protocol).and_return(protocol)
      allow(processor).to receive(:process)
    end

    it 'sets the content length for an empty response' do
      header('Content-Type', "application/x-thrift")
      post "/"
      expect(last_response.headers['Content-Length']).to eq '0'
    end

    it 'leaves exception handling to the host application' do
      allow(processor).to receive(:process).and_raise("processor failed")

      header('Content-Type', "application/x-thrift")
      expect { post "/" }.to raise_error(RuntimeError, "processor failed")
    end
  end
end
