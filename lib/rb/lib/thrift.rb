#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/
#
# Author: Mark Slee <mcslee@facebook.com>
#

$:.unshift File.dirname(__FILE__)

require 'thrift/exceptions'
TException = Thrift::Exception
TApplicationException = Thrift::ApplicationException

require 'thrift/types'
TType = Thrift::Types
TMessageType = Thrift::MessageTypes

require 'thrift/processor'
TProcessor = Thrift::Processor

require 'thrift/client'
ThriftClient = Thrift::Client

require 'thrift/struct'
ThriftStruct = Thrift::Struct

require 'thrift/protocol/tprotocol'

require 'thrift/transport/tsocket'
require 'thrift/transport/ttransport'

require 'thrift/server/tserver'




