require "spec/spec_helper"
require "lib/thrift/serializer"

path, factory_class = ARGV

factory = eval(factory_class).new

deser = Thrift::Deserializer.new(factory)

cpts = CompactProtoTestStruct.new
CompactProtoTestStruct.constants.each do |const|
  cpts.instance_variable_set("@#{const}", nil)
end

data = File.read(path)

deser.deserialize(cpts, data)

if cpts == Fixtures::COMPACT_PROTOCOL_TEST_STRUCT
  puts "Object verified successfully!"
else
  puts "Object failed verification! Expected #{Fixtures::COMPACT_PROTOCOL_TEST_STRUCT.inspect} but got #{cpts.inspect}"
  
  puts cpts.differences(Fixtures::COMPACT_PROTOCOL_TEST_STRUCT)
end
