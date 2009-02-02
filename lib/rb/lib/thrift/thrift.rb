# This file kept for backwards compatability
# require File.join(File.dirname(__FILE__), '../thrift')
$:.unshift File.dirname(File.dirname(__FILE__))
require 'thrift/deprecation'
require 'thrift/types'
require 'thrift/processor'
require 'thrift/exceptions'
require 'thrift/client'
require 'thrift/struct'
begin
  require "thrift_native"
rescue
  puts "Could not load thrift_native libraries. Using pure ruby version."
end