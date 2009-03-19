require "spec/spec_helper"
require "lib/thrift/serializer"

path, factory_class = ARGV

factory = eval(factory_class).new

ser = Thrift::Serializer.new(factory)

File.open(path, "w") do |file|
  file.write(ser.serialize(Fixtures::COMPACT_PROTOCOL_TEST_STRUCT))
end