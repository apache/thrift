# frozen_string_literal: true
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

require "spec_helper"

module StructEqualityFixtures
  class Narrow
    include Thrift::Struct, Thrift::Struct_Union

    FIELDS = {1 => {type: Thrift::Types::STRING, name: "shared"}}

    def struct_fields; FIELDS; end
    def validate; end

    Thrift::Struct.generate_accessors self
  end

  class Wide
    include Thrift::Struct, Thrift::Struct_Union

    FIELDS = {
      1 => {type: Thrift::Types::STRING, name: "shared"},
      2 => {type: Thrift::Types::STRING, name: "extra"},
    }

    def struct_fields; FIELDS; end
    def validate; end

    Thrift::Struct.generate_accessors self
  end
end

describe "Struct" do
  describe Thrift::Struct do
    it "should iterate over all fields properly" do
      fields = {}
      SpecNamespace::Foo.new.each_field { |fid, field_info| fields[fid] = field_info }
      expect(fields).to eq(SpecNamespace::Foo::FIELDS)
    end

    it "should initialize all fields to defaults" do
      validate_default_arguments(SpecNamespace::Foo.new)
    end

    it "should initialize all fields to defaults and accept a block argument" do
      SpecNamespace::Foo.new do |f|
        validate_default_arguments(f)
      end
    end

    def validate_default_arguments(object)
      expect(object.simple).to eq(53)
      expect(object.words).to eq("words")
      expect(object.hello).to eq(SpecNamespace::Hello.new(greeting: "hello, world!"))
      expect(object.ints).to eq([1, 2, 2, 3])
      expect(object.complex).to be_nil
      expect(object.shorts).to eq(Set.new([5, 17, 239]))
    end

    it "should not share default values between instances" do
      struct = SpecNamespace::Foo.new
      struct.ints << 17
      expect(SpecNamespace::Foo.new.ints).to eq([1, 2, 2, 3])
    ensure
      # ensure no leakage to other tests
      SpecNamespace::Foo::FIELDS[4][:default] = [1, 2, 2, 3]
    end

    it "isolates nested generated defaults between instances" do
      first = SpecNamespace::NestedDefaults.new
      second = SpecNamespace::NestedDefaults.new

      first.nested_list.first << 2
      first.nested_map["key"] << "changed"
      first.nested_set.first << 2
      first.nested_set.reset
      first.child.greeting << " changed"
      first.text << " changed"
      first.data << " changed"
      first.token.replace("ffeeddcc-bbaa-9988-7766-554433221100")

      expect(first.nested_set.include?([1, 2])).to be(true)
      expect(second.nested_list).to eq([[1]])
      expect(second.nested_map).to eq({"key" => ["value"]})
      expect(second.nested_set).to eq(Set.new([[1]]))
      expect(second.child.greeting).to eq("child")
      expect(second.text).to eq("fixed")
      expect(second.data).to eq("binary")
      expect(second.token).to eq("00112233-4455-6677-8899-aabbccddeeff")
      expect(first.scalar).to equal(second.scalar)
    end

    it "copies frozen containers and strings into mutable values" do
      defaults = {
        "list" => [[1].freeze].freeze,
        "text" => "fixed",
      }

      copy = Thrift::Struct.copy_default_value(defaults)
      copy["list"].first << 2
      copy["text"] << " changed"

      expect(copy).to eq({"list" => [[1, 2]], "text" => "fixed changed"})
      expect(defaults).to eq({"list" => [[1]], "text" => "fixed"})
    end

    it "rejects a frozen defaulted target before consuming truncated input" do
      buffer = Thrift::MemoryBufferTransport.new
      protocol = Thrift::BinaryProtocol.new(buffer)
      protocol.write_struct_begin("BoolStruct")
      protocol.write_field_begin("yesno", Thrift::Types::BOOL, 1)
      payload = buffer.read(buffer.available)
      input = Thrift::MemoryBufferTransport.new(payload)
      target = SpecNamespace::BoolStruct.new.freeze

      expect { target.read(Thrift::BinaryProtocol.new(input)) }.to raise_error(FrozenError)

      expect(input.available).to eq(payload.bytesize)
      expect(target).to eq(SpecNamespace::BoolStruct.new)
    end

    [Thrift::BinaryProtocol, Thrift::CompactProtocol, Thrift::JsonProtocol].each do |protocol_class|
      describe "reading nested defaults with #{protocol_class}" do
        def empty_nested_defaults(protocol_class)
          transport = Thrift::MemoryBufferTransport.new
          protocol = protocol_class.new(transport)
          protocol.write_struct_begin("NestedDefaultsList")
          protocol.write_field_begin("values", Thrift::Types::LIST, 1)
          protocol.write_list_begin(Thrift::Types::STRUCT, 2)
          2.times do
            protocol.write_struct_begin("NestedDefaults")
            protocol.write_field_stop
            protocol.write_struct_end
          end
          protocol.write_list_end
          protocol.write_field_end
          protocol.write_field_stop
          protocol.write_struct_end
          protocol
        end

        it "isolates omitted defaults when reading nested values" do
          protocol = empty_nested_defaults(protocol_class)
          default = SpecNamespace::NestedDefaults::FIELDS[1][:default]

          result = SpecNamespace::NestedDefaultsList.new
          result.read(protocol)

          first, second = result.values
          first.nested_list.first << 2
          expect(second.nested_list).to eq([[1]])
          expect(default).to eq([[1]])
        end

        [[], [:nested_map, :child]].each do |omitted|
          it "does not recopy defaults for received fields when #{omitted.inspect} are omitted" do
            source = SpecNamespace::NestedDefaults.new
            omitted.each { |name| source.public_send("#{name}=", nil) }
            buffer = Thrift::MemoryBufferTransport.new
            source.write(protocol_class.new(buffer))
            target = SpecNamespace::NestedDefaults.new
            list_default = SpecNamespace::NestedDefaults::FIELDS[1][:default]
            map_default = SpecNamespace::NestedDefaults::FIELDS[2][:default]
            copies = {list: 0, map: 0}
            allow(Thrift::Struct).to receive(:copy_default_value).and_wrap_original do |original, value|
              copies[:list] += 1 if value.equal?(list_default)
              copies[:map] += 1 if value.equal?(map_default)
              original.call(value)
            end

            target.read(protocol_class.new(buffer))

            expect(copies).to eq({list: 0, map: omitted.include?(:nested_map) ? 1 : 0})
            expect(target.nested_list).to eq([[1]])
            expect(target.nested_map).to eq({"key" => ["value"]})
            expect(target.child.greeting).to eq("child")
          end
        end

        it "does not restore a default over a received container with mismatched elements" do
          buffer = Thrift::MemoryBufferTransport.new
          protocol = protocol_class.new(buffer)
          protocol.write_struct_begin("NestedDefaults")
          protocol.write_field_begin("nested_list", Thrift::Types::LIST, 1)
          protocol.write_list_begin(Thrift::Types::STRING, 1)
          protocol.write_string("ignored")
          protocol.write_list_end
          protocol.write_field_end
          protocol.write_field_stop
          protocol.write_struct_end
          target = SpecNamespace::NestedDefaults.new

          target.read(protocol_class.new(buffer))

          expect(target.nested_list).to be_nil
          expect(target.nested_map).to eq({"key" => ["value"]})
        end

        it "resets unread fields after failure without discarding completed fields" do
          buffer = Thrift::MemoryBufferTransport.new
          protocol = protocol_class.new(buffer)
          protocol.write_struct_begin("NestedDefaults")
          protocol.write_field_begin("text", Thrift::Types::STRING, 5)
          protocol.write_string("received")
          protocol.write_field_end
          protocol.write_field_begin("scalar", Thrift::Types::I32, 6)
          target = SpecNamespace::NestedDefaults.new(scalar: 99)
          old_list = target.nested_list
          old_list.first << 99

          expect { target.read(protocol_class.new(buffer)) }.to raise_error(EOFError)

          expect(target.text).to eq("received")
          expect(target.scalar).to eq(7)
          expect(target.nested_list).to eq([[1]])
          expect(target.nested_list).not_to equal(old_list)
        end

        it "still resets fields changed by a custom initializer" do
          custom_class = Class.new(SpecNamespace::NestedDefaults) do
            def initialize
              super
              @nested_list.first << 99
              @text = "custom"
              @custom_initialized = true
            end
          end
          field = SpecNamespace::NestedDefaultsList::FIELDS.fetch(1)
          stub_const("SpecNamespace::NestedDefaultsList::FIELDS", {
            1 => field.merge(element: field[:element].merge(class: custom_class)),
          })

          result = SpecNamespace::NestedDefaultsList.new
          result.read(empty_nested_defaults(protocol_class))

          result.values.each do |value|
            expect(value.nested_list).to eq([[1]])
            expect(value.text).to eq("fixed")
            expect(value.instance_variable_get(:@custom_initialized)).to be(true)
          end
        end

        it "preserves custom readers in the Ruby field reader" do
          arguments = nil
          custom_class = Class.new(SpecNamespace::NestedDefaults) do
            define_method(:read) do |*args|
              arguments = args
              super(*args)
            end
          end
          protocol = empty_nested_defaults(protocol_class)
          protocol.read_struct_begin
          protocol.read_field_begin
          protocol.read_list_begin

          value = SpecNamespace::Foo.new.read_field(protocol, {type: Thrift::Types::STRUCT, class: custom_class})

          expect(arguments).to eq([protocol, Thrift::DEFAULT_RECURSION_DEPTH - 1])
          expect(value.nested_list).to eq([[1]])
        end

        it "still resets pooled objects returned by custom constructors" do
          custom_class = Class.new(SpecNamespace::NestedDefaults)
          existing = custom_class.new
          existing.nested_list.first << 99
          allow(custom_class).to receive(:new).and_return(existing)
          protocol = empty_nested_defaults(protocol_class)
          protocol.read_struct_begin
          protocol.read_field_begin
          protocol.read_list_begin

          value = SpecNamespace::Foo.new.read_field(protocol, {type: Thrift::Types::STRUCT, class: custom_class})

          expect(value).to equal(existing)
          expect(value.nested_list).to eq([[1]])
        end
      end
    end

    it "isolates generated union defaults between instances" do
      first = SpecNamespace::StructWithUnionDefault.new
      second = SpecNamespace::StructWithUnionDefault.new

      first.union_field.string_field << " changed"
      expect(second.union_field.string_field).to eq("default union value")
      expect(SpecNamespace::StructWithUnionDefault::FIELDS[1][:default].get_value).to eq("default union value")
    end

    it "does not confuse generated field accessors with default-copy helpers" do
      value = SpecNamespace::DefaultCopyNames.new

      expect(value.copy_default_value).to eq("literal")
      expect(value.copy_struct_default.greeting).to eq("child")
    end

    it "does not rerun nested struct constructors when copying defaults" do
      expect(SpecNamespace::Hello).not_to receive(:new)

      expect(SpecNamespace::Foo.new.hello.greeting).to eq("hello, world!")
    end

    it "keeps explicitly supplied mutable values owned by the caller" do
      value = [[7]]
      struct = SpecNamespace::NestedDefaults.new(nested_list: value)

      expect(struct.nested_list).to equal(value)
    end

    [Thrift::BinaryProtocol, Thrift::CompactProtocol, Thrift::JsonProtocol].each do |protocol_class|
      it "isolates omitted defaults on repeated #{protocol_class} reads" do
        buffer = Thrift::MemoryBufferTransport.new
        protocol = protocol_class.new(buffer)
        protocol.write_struct_begin("NestedDefaults")
        protocol.write_field_stop
        protocol.write_struct_end
        bytes = buffer.read(buffer.available)

        first = SpecNamespace::NestedDefaults.new
        second = SpecNamespace::NestedDefaults.new
        2.times do
          [first, second].each do |value|
            value.read(protocol_class.new(Thrift::MemoryBufferTransport.new(bytes)))
          end
          expect(first.nested_list.first).not_to equal(second.nested_list.first)
          expect(first.nested_map["key"]).not_to equal(second.nested_map["key"])
          expect(first.nested_set.first).not_to equal(second.nested_set.first)
          expect(first.child.greeting).not_to equal(second.child.greeting)
          expect(first.nested_list).to eq([[1]])

          [:text, :data, :token].each do |name|
            default = first.fields_with_default_values.fetch(name.to_s)
            first_value = first.public_send(name)
            second_value = second.public_send(name)

            expect(first_value).to eq(default)
            expect(first_value.encoding).to eq(default.encoding)
            expect(first_value).not_to equal(default)
            expect(first_value).not_to equal(second_value)
            first_value << " changed"
            expect(second_value).to eq(default)
          end
        end
      end
    end

    it "should properly initialize boolean values" do
      struct = SpecNamespace::BoolStruct.new(yesno: false)
      expect(struct.yesno).to be_falsey
    end

    it "should have proper == semantics" do
      expect(SpecNamespace::Foo.new).not_to eq(SpecNamespace::Hello.new)
      expect(SpecNamespace::Foo.new).to eq(SpecNamespace::Foo.new)
      expect(SpecNamespace::Foo.new(simple: 52)).not_to eq(SpecNamespace::Foo.new)
    end

    it "compares only structs of the same generated class" do
      narrow = StructEqualityFixtures::Narrow.new(shared: "value")
      wide = StructEqualityFixtures::Wide.new(shared: "value", extra: "different")
      same_narrow = StructEqualityFixtures::Narrow.new(shared: "value")

      expect(narrow == wide).to be(false)
      expect(wide == narrow).to be(false)
      expect(narrow).to eq(same_narrow)
      expect(narrow).to eql(same_narrow)
      expect(narrow.hash).to eq(same_narrow.hash)

      expect({narrow => :value}[wide]).to be_nil
      expect(Set.new([narrow])).not_to include(wide)
    end

    it "should print enum value names in inspect" do
      expect(SpecNamespace::StructWithSomeEnum.new(some_enum: SpecNamespace::SomeEnum::ONE).inspect).to eq("<SpecNamespace::StructWithSomeEnum some_enum:ONE (0)>")

      expect(SpecNamespace::StructWithEnumMap.new(my_map: {SpecNamespace::SomeEnum::ONE => [SpecNamespace::SomeEnum::TWO]}).inspect).to eq("<SpecNamespace::StructWithEnumMap my_map:{ONE (0): [TWO (1)]}>")
    end

    it "should pretty print binary fields" do
      expect(SpecNamespace::Foo2.new(my_binary: "\001\002\003").inspect).to eq("<SpecNamespace::Foo2 my_binary:010203>")
    end

    it "should offer field? methods" do
      expect(SpecNamespace::Foo.new.opt_string?).to be_falsey
      expect(SpecNamespace::Foo.new(simple: 52).simple?).to be_truthy
      expect(SpecNamespace::Foo.new(my_bool: false).my_bool?).to be_truthy
      expect(SpecNamespace::Foo.new(my_bool: true).my_bool?).to be_truthy
    end

    it "should be comparable" do
      s1 = SpecNamespace::StructWithSomeEnum.new(some_enum: SpecNamespace::SomeEnum::ONE)
      s2 = SpecNamespace::StructWithSomeEnum.new(some_enum: SpecNamespace::SomeEnum::TWO)

      expect(s1 <=> s2).to eq(-1)
      expect(s2 <=> s1).to eq(1)
      expect(s1 <=> s1).to eq(0)
      expect(s1 <=> SpecNamespace::StructWithSomeEnum.new).to eq(-1)
    end

    it "should read itself off the wire" do
      struct = SpecNamespace::Foo.new
      prot = Thrift::BaseProtocol.new(double("transport"))
      expect(prot).to receive(:read_struct_begin).twice
      expect(prot).to receive(:read_struct_end).twice
      expect(prot).to receive(:read_field_begin).and_return(
        ["complex", Thrift::Types::MAP, 5], # Foo/complex
        ["words", Thrift::Types::STRING, 2], # Foo/words
        ["hello", Thrift::Types::STRUCT, 3], # Foo/hello
        ["greeting", Thrift::Types::STRING, 1], # Foo/hello/greeting
        [nil, Thrift::Types::STOP, 0], # Foo/hello
        ["simple", Thrift::Types::I32, 1], # Foo/simple
        ["ints", Thrift::Types::LIST, 4], # Foo/ints
        ["shorts", Thrift::Types::SET, 6], # Foo/shorts
        [nil, Thrift::Types::STOP, 0], # Foo
      )
      expect(prot).to receive(:read_field_end).exactly(7).times
      expect(prot).to receive(:read_map_begin).and_return(
        [Thrift::Types::I32, Thrift::Types::MAP, 2], # Foo/complex
        [Thrift::Types::STRING, Thrift::Types::DOUBLE, 2], # Foo/complex/1/value
        [Thrift::Types::STRING, Thrift::Types::DOUBLE, 1], # Foo/complex/2/value
      )
      expect(prot).to receive(:read_map_end).exactly(3).times
      expect(prot).to receive(:read_list_begin).and_return([Thrift::Types::I32, 4])
      expect(prot).to receive(:read_list_end)
      expect(prot).to receive(:read_set_begin).and_return([Thrift::Types::I16, 2])
      expect(prot).to receive(:read_set_end)
      expect(prot).to receive(:read_i32).and_return(
        1, 14,        # Foo/complex/keys
        42,           # Foo/simple
        4, 23, 4, 29, # Foo/ints
      )
      expect(prot).to receive(:read_string).and_return("pi", "e", "feigenbaum", "apple banana", "what's up?")
      expect(prot).to receive(:read_double).and_return(Math::PI, Math::E, 4.669201609)
      expect(prot).to receive(:read_i16).and_return(2, 3)
      expect(prot).not_to receive(:skip)
      struct.read(prot)

      expect(struct.simple).to eq(42)
      expect(struct.complex).to eq({1 => {"pi" => Math::PI, "e" => Math::E}, 14 => {"feigenbaum" => 4.669201609}})
      expect(struct.hello).to eq(SpecNamespace::Hello.new(greeting: "what's up?"))
      expect(struct.words).to eq("apple banana")
      expect(struct.ints).to eq([4, 23, 4, 29])
      expect(struct.shorts).to eq(Set.new([3, 2]))
    end

    it "rejects negative container sizes while reading" do
      struct = SpecNamespace::Foo.new
      prot = Thrift::BaseProtocol.new(double("transport"))

      expect(prot).to receive(:read_list_begin).and_return([Thrift::Types::I32, -1])

      expect {
        struct.send(:read_field, prot, SpecNamespace::Foo::FIELDS[4])
      }.to raise_error(Thrift::ProtocolException, "Negative size") { |error|
        expect(error.type).to eq(Thrift::ProtocolException::NEGATIVE_SIZE)
      }
    end

    it "does not preallocate arrays from declared list sizes" do
      struct = SpecNamespace::Foo.new
      prot = Thrift::BaseProtocol.new(double("transport"))
      declared_size = 1 << 30
      sentinel = RuntimeError.new("stop after first element")

      expect(prot).to receive(:read_list_begin).and_return([Thrift::Types::I32, declared_size])
      expect(prot).to receive(:read_i32).and_raise(sentinel)
      expect(Array).not_to receive(:new).with(declared_size)

      expect {
        struct.send(:read_field, prot, SpecNamespace::Foo::FIELDS[4])
      }.to raise_error(sentinel)
    end

    [
      ["map", Thrift::Types::MAP, 5, :read_map_begin, [Thrift::Types::I32, Thrift::Types::MAP]],
      ["list", Thrift::Types::LIST, 4, :read_list_begin, [Thrift::Types::I32]],
      ["set", Thrift::Types::SET, 6, :read_set_begin, [Thrift::Types::I16]],
    ].each do |name, field_type, field_id, begin_method, header|
      it "rejects #{name} sizes above the signed 32-bit range" do
        struct = SpecNamespace::Foo.new
        prot = Thrift::BaseProtocol.new(double("transport"))
        declared_size = 1 << 31

        expect(prot).to receive(:read_struct_begin)
        expect(prot).to receive(:read_field_begin).and_return([name, field_type, field_id])
        expect(prot).to receive(begin_method).and_return([*header, declared_size])

        expect { struct.read(prot) }.to raise_error(Thrift::ProtocolException, "Container size limit exceeded") do |error|
          expect(error.type).to eq(Thrift::ProtocolException::SIZE_LIMIT)
        end
      end
    end

    it "should serialize false boolean fields correctly" do
      b = SpecNamespace::BoolStruct.new(yesno: false)
      prot = Thrift::BinaryProtocol.new(Thrift::MemoryBufferTransport.new)
      expect(prot).to receive(:write_bool).with(false)
      b.write(prot)
    end

    it "should skip unexpected fields in structs and use default values" do
      struct = SpecNamespace::Foo.new
      prot = Thrift::BaseProtocol.new(double("transport"))
      expect(prot).to receive(:read_struct_begin)
      expect(prot).to receive(:read_struct_end)
      expect(prot).to receive(:read_field_begin).and_return(
        ["simple", Thrift::Types::I32, 1],
        ["complex", Thrift::Types::STRUCT, 5],
        ["thinz", Thrift::Types::MAP, 7],
        ["foobar", Thrift::Types::I32, 3],
        ["words", Thrift::Types::STRING, 2],
        [nil, Thrift::Types::STOP, 0],
      )
      expect(prot).to receive(:read_field_end).exactly(5).times
      expect(prot).to receive(:read_i32).and_return(42)
      expect(prot).to receive(:read_string).and_return("foobar")
      expect(prot).to receive(:skip).with(Thrift::Types::STRUCT)
      expect(prot).to receive(:skip).with(Thrift::Types::MAP)
      # prot.should_receive(:read_map_begin).and_return([Thrift::Types::I32, Thrift::Types::I32, 0])
      # prot.should_receive(:read_map_end)
      expect(prot).to receive(:skip).with(Thrift::Types::I32)
      struct.read(prot)

      expect(struct.simple).to eq(42)
      expect(struct.complex).to be_nil
      expect(struct.words).to eq("foobar")
      expect(struct.hello).to eq(SpecNamespace::Hello.new(greeting: "hello, world!"))
      expect(struct.ints).to eq([1, 2, 2, 3])
      expect(struct.shorts).to eq(Set.new([5, 17, 239]))
    end

    it "should write itself to the wire" do
      prot = Thrift::BaseProtocol.new(double("transport")) # mock("Protocol")
      expect(prot).to receive(:write_struct_begin).with("SpecNamespace::Foo")
      expect(prot).to receive(:write_struct_begin).with("SpecNamespace::Hello")
      expect(prot).to receive(:write_struct_end).twice
      expect(prot).to receive(:write_field_begin).with("ints", Thrift::Types::LIST, 4)
      expect(prot).to receive(:write_i32).with(1)
      expect(prot).to receive(:write_i32).with(2).twice
      expect(prot).to receive(:write_i32).with(3)
      expect(prot).to receive(:write_field_begin).with("complex", Thrift::Types::MAP, 5)
      expect(prot).to receive(:write_i32).with(5)
      expect(prot).to receive(:write_string).with("foo")
      expect(prot).to receive(:write_double).with(1.23)
      expect(prot).to receive(:write_field_begin).with("shorts", Thrift::Types::SET, 6)
      expect(prot).to receive(:write_i16).with(5)
      expect(prot).to receive(:write_i16).with(17)
      expect(prot).to receive(:write_i16).with(239)
      expect(prot).to receive(:write_field_stop).twice
      expect(prot).to receive(:write_field_end).exactly(6).times
      expect(prot).to receive(:write_field_begin).with("simple", Thrift::Types::I32, 1)
      expect(prot).to receive(:write_i32).with(53)
      expect(prot).to receive(:write_field_begin).with("hello", Thrift::Types::STRUCT, 3)
      expect(prot).to receive(:write_field_begin).with("greeting", Thrift::Types::STRING, 1)
      expect(prot).to receive(:write_string).with("hello, world!")
      expect(prot).to receive(:write_map_begin).with(Thrift::Types::I32, Thrift::Types::MAP, 1)
      expect(prot).to receive(:write_map_begin).with(Thrift::Types::STRING, Thrift::Types::DOUBLE, 1)
      expect(prot).to receive(:write_map_end).twice
      expect(prot).to receive(:write_list_begin).with(Thrift::Types::I32, 4)
      expect(prot).to receive(:write_list_end)
      expect(prot).to receive(:write_set_begin).with(Thrift::Types::I16, 3)
      expect(prot).to receive(:write_set_end)

      struct = SpecNamespace::Foo.new
      struct.words = nil
      struct.complex = {5 => {"foo" => 1.23}}
      struct.write(prot)
    end

    it "should serialize subclasses of Set like Set" do
      set_subclass = Class.new(Set)
      regular = SpecNamespace::Foo.new(shorts: Set.new([5, 17, 239]))
      subclassed = SpecNamespace::Foo.new(shorts: set_subclass.new([5, 17, 239]))
      serializer = Thrift::Serializer.new(Thrift::BinaryProtocolFactory.new)

      expect(serializer.serialize(subclassed)).to eq(serializer.serialize(regular))
    end

    it "should raise an exception if presented with an unknown container" do
      # yeah this is silly, but I'm going for code coverage here
      struct = SpecNamespace::Foo.new
      expect { struct.send :write_container, nil, nil, {type: "foo"} }.to raise_error(StandardError, "Not a container type: foo")
    end

    it "should support optional type-checking in Thrift::Struct.new" do
      Thrift.type_checking = true
      begin
        expect { SpecNamespace::Hello.new(greeting: 3) }.to raise_error(Thrift::TypeError, "Expected Types::STRING, received Integer for field greeting")
      ensure
        Thrift.type_checking = false
      end
      expect { SpecNamespace::Hello.new(greeting: 3) }.not_to raise_error
    end

    it "should support optional type-checking in field accessors" do
      Thrift.type_checking = true
      begin
        hello = SpecNamespace::Hello.new
        expect { hello.greeting = 3 }.to raise_error(Thrift::TypeError, "Expected Types::STRING, received Integer for field greeting")
      ensure
        Thrift.type_checking = false
      end
      expect { hello.greeting = 3 }.not_to raise_error
    end

    it "should raise an exception when unknown types are given to Thrift::Struct.new" do
      expect { SpecNamespace::Hello.new(fish: "salmon") }.to raise_error(Exception, "Unknown key given to SpecNamespace::Hello.new: fish")
    end

    it "should support `raise Xception, 'message'` for Exception structs" do
      raise SpecNamespace::Xception, "something happened"
    rescue Thrift::Exception => e
      expect(e.message).to eq("something happened")
      expect(e.code).to eq(1)
      # ensure it gets serialized properly, this is the really important part
      prot = Thrift::BaseProtocol.new(double("trans"))
      expect(prot).to receive(:write_struct_begin).with("SpecNamespace::Xception")
      expect(prot).to receive(:write_struct_end)
      expect(prot).to receive(:write_field_begin).with("message", Thrift::Types::STRING, 1)
      expect(prot).to receive(:write_string).with("something happened")
      expect(prot).to receive(:write_field_begin).with("code", Thrift::Types::I32, 2)
      expect(prot).to receive(:write_i32).with(1)
      expect(prot).to receive(:write_field_stop)
      expect(prot).to receive(:write_field_end).twice

      e.write(prot)
    end

    it "should support the regular initializer for exception structs" do
      raise SpecNamespace::Xception, {message: "something happened", code: 5}
    rescue Thrift::Exception => e
      expect(e.message).to eq("something happened")
      expect(e.code).to eq(5)
      prot = Thrift::BaseProtocol.new(double("trans"))
      expect(prot).to receive(:write_struct_begin).with("SpecNamespace::Xception")
      expect(prot).to receive(:write_struct_end)
      expect(prot).to receive(:write_field_begin).with("message", Thrift::Types::STRING, 1)
      expect(prot).to receive(:write_string).with("something happened")
      expect(prot).to receive(:write_field_begin).with("code", Thrift::Types::I32, 2)
      expect(prot).to receive(:write_i32).with(5)
      expect(prot).to receive(:write_field_stop)
      expect(prot).to receive(:write_field_end).twice

      e.write(prot)
    end

    it "should handle UUID fields in structs" do
      struct = SpecNamespace::Foo.new(
        simple: 42,
        words: "test",
        opt_uuid: "550e8400-e29b-41d4-a716-446655440000",
      )

      trans = Thrift::MemoryBufferTransport.new
      prot = Thrift::BinaryProtocol.new(trans)

      struct.write(prot)

      result = SpecNamespace::Foo.new
      result.read(prot)

      expect(result.simple).to eq(42)
      expect(result.words).to eq("test")
      expect(result.opt_uuid).to eq("550e8400-e29b-41d4-a716-446655440000")
    end

    it "should handle optional UUID fields when unset" do
      struct = SpecNamespace::Foo.new(simple: 42, words: "test")
      expect(struct.opt_uuid).to be_nil
      expect(struct.opt_uuid?).to be_falsey
    end

    it "should handle list of UUIDs in SimpleList" do
      uuids = ["550e8400-e29b-41d4-a716-446655440000", "6ba7b810-9dad-11d1-80b4-00c04fd430c8"]
      struct = SpecNamespace::SimpleList.new(uuids: uuids)

      trans = Thrift::MemoryBufferTransport.new
      prot = Thrift::CompactProtocol.new(trans)

      struct.write(prot)

      result = SpecNamespace::SimpleList.new
      result.read(prot)

      expect(result.uuids).to eq(uuids)
    end

    it "should normalize UUID case to lowercase" do
      struct = SpecNamespace::Foo.new(opt_uuid: "550E8400-E29B-41D4-A716-446655440000")

      trans = Thrift::MemoryBufferTransport.new
      prot = Thrift::BinaryProtocol.new(trans)

      struct.write(prot)

      result = SpecNamespace::Foo.new
      result.read(prot)

      expect(result.opt_uuid).to eq("550e8400-e29b-41d4-a716-446655440000")
    end

    it "should handle UUID alongside other types in SimpleList" do
      struct = SpecNamespace::SimpleList.new(
        bools: [true, false],
        i32s: [1, 2, 3],
        strings: ["hello", "world"],
        uuids: ["550e8400-e29b-41d4-a716-446655440000", "00000000-0000-0000-0000-000000000000"],
      )

      trans = Thrift::MemoryBufferTransport.new
      prot = Thrift::BinaryProtocol.new(trans)

      struct.write(prot)

      result = SpecNamespace::SimpleList.new
      result.read(prot)

      expect(result.bools).to eq([true, false])
      expect(result.i32s).to eq([1, 2, 3])
      expect(result.strings).to eq(["hello", "world"])
      expect(result.uuids).to eq(["550e8400-e29b-41d4-a716-446655440000", "00000000-0000-0000-0000-000000000000"])
    end
  end
end
