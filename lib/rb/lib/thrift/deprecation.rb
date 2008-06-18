# provide a backwards-compatible wrapper API and deprecate it

unless Thrift.const_defined?(:DEPRECATION)
  Thrift::DEPRECATION = true
end

class Module
  # Wraps the given methods to print a warning and call the real method
  # Example:
  #   deprecate! :readAll => :read_all
  def deprecate!(methods)
    methods.each_pair do |old, new|
      module_eval <<-EOF
        def #{old}(*args, &block)
          old, new = #{[old,new].inspect}
          STDERR.puts "Warning: calling deprecated method \#{self.is_a?(Module) ? "\#{self}." : "\#{self.class}#"}\#{old}"
          target = (self.is_a?(Module) ? (class << self;self;end) : self.class)
          target.send :define_method, old, target.instance_method(new) # unwrap
          target.instance_method(new).bind(self).call(*args, &block)
        end
      EOF
    end
  end
end

module Kernel
  # Provides an alternate name for the class for deprecation purposes
  # Example:
  #   deprecate_class! :TBinaryProtocol => Thrift::BinaryProtocol
  #--
  # at the moment this only works for creating top-level constants
  # if necessary, this can be extended to take something like :'Thrift::TBinaryProtocol'
  # alternately, Module can be extended with a similar method
  #
  # another idea is to not make the old name a pointer to the new, but rather
  # a pointer to a proxy class that logs deprecation warnings and forwards methods
  def deprecate_class!(klasses)
    klasses.each_pair do |old, new|
      Object.const_set old, new
    end
  end
end

# TProtocol = Thrift::Protocol
# TProtocolException = Thrift::ProtocolException
# ThriftStruct = Thrift::Struct
# ThriftClient = Thrift::Client
# TProcessor = Thrift::Processor
# TType = Thrift::Types
# TMessageType = Thrift::MessageTypes
# TException = Thrift::Exception
# TApplicationException = Thrift::ApplicationException
# TBinaryProtocol = Thrift::BinaryProtocol
