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

describe 'Thrift::HTTPClientTransport' do
  describe Thrift::HTTPClientTransport do
    before(:each) do
      @client = Thrift::HTTPClientTransport.new("http://my.domain.com/path/to/service?param=value")
    end

    it "should provide a reasonable to_s" do
      expect(@client.to_s).to eq("http(my.domain.com:80/path/to/service)")
    end

    it "should include the root path in its endpoint label" do
      client = Thrift::HTTPClientTransport.new("http://my.domain.com")

      expect(client.to_s).to eq("http(my.domain.com:80/)")
    end

    it "should distinguish an IPv6 host from its port" do
      client = Thrift::HTTPClientTransport.new("https://[2001:db8::1]:8443/path/to/service")

      expect(client.to_s).to eq("https([2001:db8::1]:8443/path/to/service)")
    end

    it "should not include userinfo in its endpoint label" do
      client = Thrift::HTTPClientTransport.new("https://user:secret@my.domain.com/path/to/service?token=secret")

      expect(client.to_s).to eq("https(my.domain.com:443/path/to/service)")
      expect(client.to_s).not_to include("user", "secret", "token")
    end

    it "should always be open" do
      expect(@client).to be_open
      @client.close
      expect(@client).to be_open
    end

    it "should post via HTTP and return the results" do
      @client.write "a test"
      @client.write " frame"
      expect(Net::HTTP).to receive(:new).with("my.domain.com", 80) do
        double("Net::HTTP").tap do |http|
          expect(http).to receive(:use_ssl=).with(false)
          expect(http).to receive(:post).with("/path/to/service?param=value", "a test frame", {"Content-Type"=>"application/x-thrift"}) do
            double("Net::HTTPOK").tap do |response|
              expect(response).to receive(:body).and_return "data"
              expect(response).to receive(:code).and_return "200"
            end
          end
        end
      end
      @client.flush
      expect(@client.read(10)).to eq("data")
    end

    it "should report successful HTTP responses without a Thrift payload when read" do
      {'204' => nil, '200' => ''}.each do |status, body|
        client = Thrift::HTTPClientTransport.new("http://my.domain.com/service")
        client.write("request")
        http = instance_double(Net::HTTP)
        response = instance_double(Net::HTTPResponse, :code => status, :body => body)

        expect(Net::HTTP).to receive(:new).with("my.domain.com", 80).and_return(http)
        expect(http).to receive(:use_ssl=).with(false)
        expect(http).to receive(:post).with("/service", "request", {"Content-Type" => "application/x-thrift"}).and_return(response)

        expect { client.flush }.not_to raise_error
        expect { client.read_all(1) }.to raise_error(Thrift::TransportException) do |error|
          expect(error.type).to eq(Thrift::TransportException::END_OF_FILE)
          expect(error.message).to eq("Thrift::HTTPClientTransport reached EOF reading response from http(my.domain.com:80/service), HTTP status code #{status}")
        end
      end
    end

    it "should allow oneway calls with successful empty HTTP responses" do
      {'200' => '', '204' => nil}.each do |status, body|
        transport = Thrift::HTTPClientTransport.new("http://my.domain.com/service")
        client = SpecNamespace::NonblockingService::Client.new(Thrift::BinaryProtocol.new(transport))
        http = instance_double(Net::HTTP)
        response = instance_double(Net::HTTPResponse, :code => status, :body => body)

        expect(Net::HTTP).to receive(:new).with("my.domain.com", 80).and_return(http)
        expect(http).to receive(:use_ssl=).with(false)
        expect(http).to receive(:post).with("/service", kind_of(String), {"Content-Type" => "application/x-thrift"}).and_return(response)

        expect { client.shutdown }.not_to raise_error
      end
    end

    it "should include a safe endpoint and status in empty response errors" do
      client = Thrift::HTTPClientTransport.new("https://user:secret@my.domain.com/service?token=secret")
      client.write("request")
      http = instance_double(Net::HTTP)
      response = instance_double(Net::HTTPNoContent, :code => "204", :body => nil)

      expect(Net::HTTP).to receive(:new).with("my.domain.com", 443).and_return(http)
      expect(http).to receive(:use_ssl=).with(true)
      expect(http).to receive(:verify_mode=).with(OpenSSL::SSL::VERIFY_PEER)
      expect(http).to receive(:post).with("/service?token=secret", "request", {"Content-Type" => "application/x-thrift"}).and_return(response)

      expect { client.flush }.not_to raise_error
      expect { client.read_all(1) }.to raise_error(Thrift::TransportException) do |error|
        expect(error.message).to eq("Thrift::HTTPClientTransport reached EOF reading response from https(my.domain.com:443/service), HTTP status code 204")
      end
    end

    it "should send custom headers if defined" do
      @client.write "test"
      custom_headers = {"Cookie" => "Foo"}
      headers = {"Content-Type"=>"application/x-thrift"}.merge(custom_headers)

      @client.add_headers(custom_headers)
      expect(Net::HTTP).to receive(:new).with("my.domain.com", 80) do
        double("Net::HTTP").tap do |http|
          expect(http).to receive(:use_ssl=).with(false)
          expect(http).to receive(:post).with("/path/to/service?param=value", "test", headers) do
            double("Net::HTTPOK").tap do |response|
              expect(response).to receive(:body).and_return "data"
              expect(response).to receive(:code).and_return "200"
            end
          end
        end
      end
      @client.flush
    end

    it 'should reset the outbuf on HTTP failures' do
      @client.write "test"

      expect(Net::HTTP).to receive(:new).with("my.domain.com", 80) do
        double("Net::HTTP").tap do |http|
          expect(http).to receive(:use_ssl=).with(false)
          expect(http).to receive(:post).with("/path/to/service?param=value", "test", {"Content-Type"=>"application/x-thrift"}) { raise Net::ReadTimeout }
        end
      end

      @client.flush rescue
      expect(@client.instance_variable_get(:@outbuf)).to eq(Thrift::Bytes.empty_byte_buffer)
    end

    it 'should raise TransportError on HTTP failures' do
      @client.write "test"

      expect(Net::HTTP).to receive(:new).with("my.domain.com", 80) do
        double("Net::HTTP").tap do |http|
          expect(http).to receive(:use_ssl=).with(false)
          expect(http).to receive(:post).with("/path/to/service?param=value", "test", {"Content-Type"=>"application/x-thrift"}) do
            double("Net::HTTPOK").tap do |response|
              expect(response).not_to receive(:body)
              expect(response).to receive(:code).at_least(:once).and_return "503"
            end
          end
        end
      end

      expect { @client.flush }.to raise_error(Thrift::TransportException)
    end

    it 'should omit URL secrets from HTTP failure messages' do
      client = Thrift::HTTPClientTransport.new("https://user:secret@my.domain.com/path/to/service?token=secret")
      client.write "test"
      http = double("Net::HTTP")
      response = double("HTTP response", :code => "503")

      expect(Net::HTTP).to receive(:new).with("my.domain.com", 443).and_return(http)
      expect(http).to receive(:use_ssl=).with(true)
      expect(http).to receive(:verify_mode=).with(OpenSSL::SSL::VERIFY_PEER)
      expect(http).to receive(:post).with("/path/to/service?token=secret", "test", {"Content-Type" => "application/x-thrift"}).and_return(response)

      expect { client.flush }.to raise_error(Thrift::TransportException) do |error|
        expect(error.message).to include("https(my.domain.com:443/path/to/service)")
        expect(error.message).not_to include("user", "secret", "token")
      end
    end
  end

  describe 'ssl enabled' do
    before(:each) do
      @service_path = "/path/to/service?param=value"
      @server_uri = "https://my.domain.com"
    end

    it "should use SSL for https" do
      client = Thrift::HTTPClientTransport.new("#{@server_uri}#{@service_path}")

      client.write "test"

      expect(Net::HTTP).to receive(:new).with("my.domain.com", 443) do
        double("Net::HTTP").tap do |http|
          expect(http).to receive(:use_ssl=).with(true)
          expect(http).to receive(:verify_mode=).with(OpenSSL::SSL::VERIFY_PEER)
          expect(http).to receive(:post).with(@service_path, "test",
              {"Content-Type" => "application/x-thrift"}) do
            double("Net::HTTPOK").tap do |response|
              expect(response).to receive(:body).and_return "data"
              expect(response).to receive(:code).and_return "200"
            end
          end
        end
      end
      client.flush
      expect(client.read(4)).to eq("data")
    end

    it "should set SSL verify mode when specified" do
      client = Thrift::HTTPClientTransport.new("#{@server_uri}#{@service_path}",
          :ssl_verify_mode => OpenSSL::SSL::VERIFY_NONE)

      client.write "test"
      expect(Net::HTTP).to receive(:new).with("my.domain.com", 443) do
        double("Net::HTTP").tap do |http|
          expect(http).to receive(:use_ssl=).with(true)
          expect(http).to receive(:verify_mode=).with(OpenSSL::SSL::VERIFY_NONE)
          expect(http).to receive(:post).with(@service_path, "test",
              {"Content-Type" => "application/x-thrift"}) do
            double("Net::HTTPOK").tap do |response|
              expect(response).to receive(:body).and_return "data"
              expect(response).to receive(:code).and_return "200"
            end
          end
        end
      end
      client.flush
      expect(client.read(4)).to eq("data")
    end

    it "should set the SSL CA file when specified" do
      client = Thrift::HTTPClientTransport.new("#{@server_uri}#{@service_path}",
          :ssl_ca_file => "/path/to/ca.pem")

      client.write "test"
      expect(Net::HTTP).to receive(:new).with("my.domain.com", 443) do
        double("Net::HTTP").tap do |http|
          expect(http).to receive(:use_ssl=).with(true)
          expect(http).to receive(:verify_mode=).with(OpenSSL::SSL::VERIFY_PEER)
          expect(http).to receive(:ca_file=).with("/path/to/ca.pem")
          expect(http).to receive(:post).with(@service_path, "test",
              {"Content-Type" => "application/x-thrift"}) do
            double("Net::HTTPOK").tap do |response|
              expect(response).to receive(:body).and_return "data"
              expect(response).to receive(:code).and_return "200"
            end
          end
        end
      end
      client.flush
      expect(client.read(4)).to eq("data")
    end
  end
end
