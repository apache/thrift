class Module
   def deprecate!(*method_names)
     method_names.each do |method_name|
       module_eval <<-END
         alias_method :deprecated_#{method_name}, :#{method_name}
         def #{method_name}(*args, &block)
           $stderr.puts "Warning: calling deprecated method: #{self}.#{method_name}"
           return deprecated_#{method_name}(*args, &block)
         end
       END
      end
   end
end

require 'thrift/transport/ttransport'

class TTransport
  deprecate! :isOpen, :readAll
end