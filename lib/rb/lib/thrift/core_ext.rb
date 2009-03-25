Dir[File.dirname(__FILE__) + "/core_ext/*.rb"].each do |file|
  name = File.basename(file, '.rb')
  require "thrift/core_ext/#{name}"
end
