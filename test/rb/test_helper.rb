$:.unshift File.dirname(__FILE__) + '/gen-rb'
$:.unshift File.join(File.dirname(__FILE__), '../../lib/rb/lib')
$:.unshift File.join(File.dirname(__FILE__), '../../lib/rb/ext')

require 'test/unit'

module Thrift
  module Struct
    def ==(other)
      return false unless other.is_a? self.class
      self.class.const_get(:FIELDS).collect {|fid, data| data[:name] }.all? do |field|
        send(field) == other.send(field)
      end
    end
  end
end
