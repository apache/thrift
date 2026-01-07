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
require File.expand_path("#{File.dirname(__FILE__)}/socket_spec_shared")

describe 'SSLServerSocket' do

  describe Thrift::SSLServerSocket do
    before(:each) do
      @socket = Thrift::SSLServerSocket.new(1234)
    end

    it "should set linger on the underlying server socket" do
      tcp = double("TCPServer")
      expect(TCPServer).to receive(:new).with(nil, 1234).and_return(tcp)
      expect(tcp).to receive(:setsockopt).with(Socket::SOL_SOCKET, Socket::SO_LINGER, [0, 0].pack('ii'))
      expect(OpenSSL::SSL::SSLServer).to receive(:new).with(tcp, nil)
      @socket.listen
    end

    it "should provide a reasonable to_s" do
      expect(@socket.to_s).to eq("ssl(socket(:1234))")
    end
  end
end
