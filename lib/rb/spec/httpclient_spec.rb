require File.dirname(__FILE__) + '/spec_helper'
require 'thrift/transport/httpclient'

class ThriftHTTPClientSpec < Spec::ExampleGroup
  include Thrift

  describe HTTPClient do
    before(:each) do
      @client = HTTPClient.new("http://my.domain.com/path/to/service")
    end

    it "should always be open" do
      @client.should be_open
      @client.close
      @client.should be_open
    end

    it "should post via HTTP and return the results" do
      @client.write "a test"
      @client.write " frame"
      Net::HTTP.should_receive(:new).with("my.domain.com", 80).and_return do
        mock("Net::HTTP").tee do |http|
          http.should_receive(:use_ssl=).with(false)
          http.should_receive(:post).with("/path/to/service", "a test frame", {"Content-Type"=>"application/x-thrift"}).and_return([nil, "data"])
        end
      end
      @client.flush
      @client.read(10).should == "data"
    end
  end
end
