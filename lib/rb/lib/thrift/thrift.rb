# This file kept for backwards compatability
# require File.join(File.dirname(__FILE__), '../thrift')
$:.unshift File.dirname(File.dirname(__FILE__))
require 'thrift/deprecation'
require 'thrift/types'
require 'thrift/processor'
require 'thrift/exceptions'
require 'thrift/client'
require 'thrift/struct'
