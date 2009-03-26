begin
  require "thrift_native"
rescue LoadError
  puts "Unable to load thrift_native extension. Defaulting to pure Ruby libraries."
end