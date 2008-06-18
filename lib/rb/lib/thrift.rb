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

module Thrift
  # prevent the deprecation layer from being loaded if you require 'thrift'
  DEPRECATION = false unless const_defined? :DEPRECATION
end

require 'thrift/deprecation'
require 'thrift/exceptions'
require 'thrift/types'
require 'thrift/processor'
require 'thrift/client'
require 'thrift/struct'
require 'thrift/protocol'
require 'thrift/transport/socket'
require 'thrift/transport'
require 'thrift/server'
