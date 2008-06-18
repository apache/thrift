# provide a backwards-compatible wrapper API and deprecate it

module Thrift
  unless const_defined?(:DEPRECATION)
    DEPRECATION = true
  end
end

class Module
  # Wraps the given methods to print a warning and call the real method
  # Example:
  #   deprecate! :readAll => :read_all
  #--
  # Yeah, this is ugly, passing a string to module_eval, but unfortunately
  # using a block removes the ability to pass blocks to the defined method
  # and breaks spec
  def deprecate!(methods)
    return unless Thrift::DEPRECATION
    methods.each_pair do |old, new|
      module_eval <<-EOF
        def #{old}(*args, &block)
          old, new = #{[old,new].inspect}
          STDERR.puts "Warning: calling deprecated method \#{self.is_a?(Module) ? "\#{self}." : "\#{self.class}#"}\#{old}"
          STDERR.puts "  from \#{caller.first}"
          target = (self.is_a?(Module) ? (class << self;self;end) : self.class)
          target.send :define_method, old, target.instance_method(new) # unwrap
          target.instance_method(new).bind(self).call(*args, &block)
        end
      EOF
    end
  end
end

module Thrift::DeprecationProxy
  def self.new_class(obj, name)
    Class.new(obj) do
      klass = self
      @@self = klass
      @@obj = obj
      instance_methods.sort.reject { |x| [:__id__,:__send__].include? x.to_sym }.each do |sym|
        undef_method sym
      end
      def method_missing(sym, *args, &block)
        @@obj.instance_method(sym).bind(self).call(*args, &block)
      end
      (class << self;self;end).class_eval do
        @@self = klass
        @@obj = obj
        @@name = name
        @@warned = false
        instance_methods.sort.reject { |x| [:__id__,:__send__].include? x.to_sym }.each do |sym|
          undef_method sym
        end
        def method_missing(sym, *args, &block)
          unless @@warned
            STDERR.puts "Warning: class #{@@name} is deprecated"
            STDERR.puts "  from #{caller.first}"
            @@warned = true
          end
          if @@self.__id__ == self.__id__
            @@obj.send sym, *args, &block
          else
            @@obj.method(sym).unbind.bind(self).call(*args, &block)
          end
        end
      end
    end
  end
  def self.new_module(obj, name)
    Module.new do
      @@obj = obj
      @@warned = false
      @@name = name
      include obj
      instance_methods.sort.reject { |x| [:__id__,:__send__].include? x.to_sym }.each do |sym|
        undef_method sym
      end
      def method_missing(sym, *args, &block)
        unless @@warned
          STDERR.puts "Warning: module #{@@name} is deprecated"
          STDERR.puts "  from #{caller.first}"
          @@warned = true
        end
        @@obj.instance_method(sym).bind(self).call(*args, &block)
      end
      (class << self;self;end).class_eval do
        @@obj = obj
        @@warned = false
        instance_methods.sort.reject { |x| [:__id__,:__send__].include? x.to_sym }.each do |sym|
          undef_method sym
        end
        def method_missing(sym, *args, &block)
          unless @@warned
            STDERR.puts "Warning: module #{@@name} is deprecated"
            STDERR.puts "  from #{caller.first}"
            @@warned = true
          end
          @@obj.send sym, *args, &block
        end
      end
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
  def deprecate_class!(klasses)
    return unless Thrift::DEPRECATION
    klasses.each_pair do |old, new|
      raise "deprecate_class! expected Class, called with #{new}" unless new.is_a? Class
      Object.const_set old, Thrift::DeprecationProxy.new_class(new, old)
    end
  end

  # like deprecate_class! but for Modules
  def deprecate_module!(modules)
    return unless Thrift::DEPRECATION
    modules.each_pair do |old, new|
      Object.const_set old, Thrift::DeprecationProxy.new_module(new, old)
    end
  end
end
