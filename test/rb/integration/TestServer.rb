#!/usr/bin/env ruby

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

$:.push File.dirname(__FILE__) + '/..'

require 'test_helper'
require 'thrift'
require 'thrift_test'
require 'thrift_test_types'
require 'logger'
require 'optparse'

class SimpleHandler
  [:testVoid, :testString, :testBool, :testByte, :testI32, :testI64, :testDouble, :testBinary,
   :testStruct, :testMap, :testStringMap, :testSet, :testList, :testNest, :testEnum, :testTypedef,
   :testEnum, :testTypedef, :testMultiException, :testUuid].each do |meth|

    define_method(meth) do |thing|
      p meth
      p thing
      thing
    end

  end

  def testVoid()
  end

  def testInsanity(thing)
    return {
      1 => {
        2 => thing,
        3 => thing
      },
      2 => {
        6 => Thrift::Test::Insanity::new()
      }
    }
  end

  def testMapMap(thing)
    return {
      -4 => {
        -4 => -4,
        -3 => -3,
        -2 => -2,
        -1 => -1,
      },
      4 => {
        4 => 4,
        3 => 3,
        2 => 2,
        1 => 1,
      }
    }
  end

  def testMulti(arg0, arg1, arg2, arg3, arg4, arg5)
    return Thrift::Test::Xtruct.new({
      'string_thing' => 'Hello2',
      'byte_thing' => arg0,
      'i32_thing' => arg1,
      'i64_thing' => arg2,
    })
  end

  def testException(thing)
    if thing == "Xception"
      raise Thrift::Test::Xception, :errorCode => 1001, :message => thing
    elsif thing == "TException"
      raise Thrift::Exception, :message => thing
    else
      # no-op
    end
  end

  def testMultiException(arg0, arg1)
    if arg0 == "Xception2"
      raise Thrift::Test::Xception2, :errorCode => 2002, :struct_thing => ::Thrift::Test::Xtruct.new({ :string_thing => 'This is an Xception2' })
    elsif arg0 == "Xception"
      raise Thrift::Test::Xception, :errorCode => 1001, :message => 'This is an Xception'
    else
      return ::Thrift::Test::Xtruct.new({'string_thing' => arg1})
    end
  end

  def testOneway(arg0)
    sleep(arg0)
  end

end

options = {
  domain_socket: nil,
  port: 9090,
  protocol: 'binary',
  ssl: false,
  transport: 'buffered',
  server_type: 'threaded',
  workers: nil,
}

server_type_map = {
  'simple' => 'simple',
  'threaded' => 'threaded',
  'thread-pool' => 'thread-pool',
  'thread_pool' => 'thread-pool',
  'threadpool' => 'thread-pool',
  'nonblocking' => 'nonblocking',
  'tsimpleserver' => 'simple',
  'tthreadedserver' => 'threaded',
  'tthreadpoolserver' => 'thread-pool',
  'tnonblockingserver' => 'nonblocking',
}

parser = OptionParser.new do |opts|
  opts.banner = "Allowed options:"
  opts.on('-h', '--help', 'produce help message') do
    puts opts
    exit 0
  end
  opts.on('--domain-socket=PATH', String, 'Unix domain socket path') { |v| options[:domain_socket] = v }
  opts.on('--port=PORT', Integer, 'Port number to listen (not valid with domain-socket)') { |v| options[:port] = v }
  opts.on('--protocol=PROTO', String, 'protocol: accel, binary, compact, json, header') { |v| options[:protocol] = v }
  opts.on('--ssl', 'use ssl (not valid with domain-socket)') { options[:ssl] = true }
  opts.on('--transport=TRANSPORT', String, 'transport: buffered, framed, header') { |v| options[:transport] = v }
  opts.on('--server-type=TYPE', String, 'type of server: simple, thread-pool, threaded, nonblocking') { |v| options[:server_type] = v }
  opts.on('-n', '--workers=N', Integer, 'Number of workers (thread-pool/nonblocking)') { |v| options[:workers] = v }
end

begin
  parser.parse!(ARGV)
  if ARGV.length > 1
    raise OptionParser::InvalidOption, "Only one positional server type may be specified"
  end
rescue OptionParser::ParseError => e
  warn e.message
  warn parser
  exit 1
end

# Accept Python-style server type positional arg for compatibility.
options[:server_type] = ARGV.first unless ARGV.empty?
normalized_server_type = server_type_map[options[:server_type].to_s.downcase]
if normalized_server_type.nil?
  warn "Unknown server type '#{options[:server_type]}'"
  warn parser
  exit 1
end

if options[:ssl] && !options[:domain_socket].to_s.strip.empty?
  warn '--ssl is not valid with --domain-socket'
  exit 1
end

protocol_factory = case options[:protocol].to_s.strip
when '', 'binary'
  Thrift::BinaryProtocolFactory.new
when 'compact'
  Thrift::CompactProtocolFactory.new
when 'json'
  Thrift::JsonProtocolFactory.new
when 'accel'
  Thrift::BinaryProtocolAcceleratedFactory.new
when 'header'
  Thrift::HeaderProtocolFactory.new
else
  raise "Unknown protocol type '#{options[:protocol]}'"
end

transport_factory = case options[:transport].to_s.strip
when '', 'buffered'
  Thrift::BufferedTransportFactory.new
when 'framed'
  Thrift::FramedTransportFactory.new
when 'header'
  Thrift::HeaderTransportFactory.new
else
  raise "Unknown transport type '#{options[:transport]}'"
end

if normalized_server_type == 'nonblocking' && options[:transport] != 'framed'
  raise 'server-type nonblocking requires transport of framed'
end

handler = SimpleHandler.new
processor = Thrift::Test::ThriftTest::Processor.new(handler)

transport = nil
if options[:domain_socket].to_s.strip.empty?
  if options[:ssl]
    # the working directory for ruby crosstest is test/rb/gen-rb
    keysDir = File.join(File.dirname(File.dirname(Dir.pwd)), "keys")
    ctx = OpenSSL::SSL::SSLContext.new
    ctx.ca_file = File.join(keysDir, "CA.pem")
    ctx.cert = OpenSSL::X509::Certificate.new(File.binread(File.join(keysDir, "server.crt")))
    ctx.cert_store = OpenSSL::X509::Store.new
    ctx.cert_store.add_file(File.join(keysDir, 'client.pem'))
    ctx.key = OpenSSL::PKey::RSA.new(File.binread(File.join(keysDir, "server.key")))
    ctx.min_version = :TLS1_2
    ctx.verify_mode = OpenSSL::SSL::VERIFY_PEER
    transport = Thrift::SSLServerSocket.new(nil, options[:port], ctx)
  else
    transport = Thrift::ServerSocket.new(options[:port])
  end
else
  transport = Thrift::UNIXServerSocket.new(options[:domain_socket])
end

workers = options[:workers] || 20
server = case normalized_server_type
when 'simple'
  Thrift::SimpleServer.new(processor, transport, transport_factory, protocol_factory)
when 'threaded'
  Thrift::ThreadedServer.new(processor, transport, transport_factory, protocol_factory)
when 'thread-pool'
  Thrift::ThreadPoolServer.new(processor, transport, transport_factory, protocol_factory, workers)
when 'nonblocking'
  logger = Logger.new(STDERR)
  logger.level = Logger::WARN
  Thrift::NonblockingServer.new(processor, transport, transport_factory, protocol_factory, workers, logger)
end

puts "Starting TestServer #{server.to_s}"
server.serve
puts "done."
