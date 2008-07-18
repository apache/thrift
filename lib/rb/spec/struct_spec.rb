require File.dirname(__FILE__) + '/spec_helper'
require File.dirname(__FILE__) + '/gen-rb/ThriftSpec_types'

class ThriftStructSpec < Spec::ExampleGroup
  include Thrift
  include SpecNamespace

  class OldStruct
    include Thrift::Struct
    attr_accessor :set
    FIELDS = {
      1 => {:type => Thrift::Types::SET, :name => 'val', :default => {:foo => true, :bar => true}}
    }
  end

  describe Struct do
    it "should iterate over all fields properly" do
      fields = {}
      Foo.new.each_field { |fid,type,name,default| fields[fid] = [type,name,default] }
      fields.should == {
        1 => [Types::I32, 'simple', 53],
        2 => [Types::STRING, 'words', "words"],
        3 => [Types::STRUCT, 'hello', Hello.new(:greeting => 'hello, world!')],
        4 => [Types::LIST, 'ints', [1, 2, 2, 3]],
        5 => [Types::MAP, 'complex', nil],
        6 => [Types::SET, 'shorts', Set.new([5, 17, 239])]
      }
    end

    it "should initialize all fields to defaults" do
      struct = Foo.new
      struct.simple.should == 53
      struct.words.should == "words"
      struct.hello.should == Hello.new(:greeting => 'hello, world!')
      struct.ints.should == [1, 2, 2, 3]
      struct.complex.should be_nil
      struct.shorts.should == Set.new([5, 17, 239])
    end

    it "should not share default values between instances" do
      begin
        struct = Foo.new
        struct.ints << 17
        Foo.new.ints.should == [1,2,2,3]
      ensure
        # ensure no leakage to other tests
        Foo::FIELDS[4][:default] = [1,2,2,3]
      end
    end

    it "should properly initialize boolean values" do
      struct = BoolStruct.new(:yesno => false)
      struct.yesno.should be_false
    end

    it "should have proper == semantics" do
      Foo.new.should_not == Hello.new
      Foo.new.should == Foo.new
      Foo.new(:simple => 52).should_not == Foo.new
    end

    it "should read itself off the wire" do
      struct = Foo.new
      prot = mock("Protocol")
      prot.should_receive(:read_struct_begin).twice
      prot.should_receive(:read_struct_end).twice
      prot.should_receive(:read_field_begin).and_return(
        ['complex', Types::MAP, 5], # Foo
        ['words', Types::STRING, 2], # Foo
        ['hello', Types::STRUCT, 3], # Foo
          ['greeting', Types::STRING, 1], # Hello
          [nil, Types::STOP, 0], # Hello
        ['simple', Types::I32, 1], # Foo
        ['ints', Types::LIST, 4], # Foo
        ['shorts', Types::SET, 6], # Foo
        [nil, Types::STOP, 0] # Hello
      )
      prot.should_receive(:read_field_end).exactly(7).times
      prot.should_receive(:read_map_begin).and_return(
        [Types::I32, Types::MAP, 2], # complex
          [Types::STRING, Types::DOUBLE, 2], # complex/1/value
          [Types::STRING, Types::DOUBLE, 1] # complex/2/value
      )
      prot.should_receive(:read_map_end).exactly(3).times
      prot.should_receive(:read_list_begin).and_return([Types::I32, 4])
      prot.should_receive(:read_list_end)
      prot.should_receive(:read_set_begin).and_return([Types::I16, 2])
      prot.should_receive(:read_set_end)
      prot.should_receive(:read_type).with(Types::I32).and_return(
        1, 14,        # complex keys
        42,           # simple
        4, 23, 4, 29  # ints
      )
      prot.should_receive(:read_type).with(Types::STRING).and_return("pi", "e", "feigenbaum", "apple banana", "what's up?")
      prot.should_receive(:read_type).with(Types::DOUBLE).and_return(Math::PI, Math::E, 4.669201609)
      prot.should_receive(:read_type).with(Types::I16).and_return(2, 3)
      prot.should_not_receive(:skip)
      struct.read(prot)

      struct.simple.should == 42
      struct.complex.should == {1 => {"pi" => Math::PI, "e" => Math::E}, 14 => {"feigenbaum" => 4.669201609}}
      struct.hello.should == Hello.new(:greeting => "what's up?")
      struct.words.should == "apple banana"
      struct.ints.should == [4, 23, 4, 29]
      struct.shorts.should == Set.new([3, 2])
    end

    it "should skip unexpected fields in structs and use default values" do
      struct = Foo.new
      prot = mock("Protocol")
      prot.should_receive(:read_struct_begin)
      prot.should_receive(:read_struct_end)
      prot.should_receive(:read_field_begin).and_return(
        ['simple', Types::I32, 1],
        ['complex', Types::STRUCT, 5],
        ['thinz', Types::MAP, 7],
        ['foobar', Types::I32, 3],
        ['words', Types::STRING, 2],
        [nil, Types::STOP, 0]
      )
      prot.should_receive(:read_field_end).exactly(5).times
      prot.should_receive(:read_type).with(Types::I32).and_return(42)
      prot.should_receive(:read_type).with(Types::STRING).and_return("foobar")
      prot.should_receive(:skip).with(Types::STRUCT)
      prot.should_receive(:skip).with(Types::MAP)
      prot.should_receive(:skip).with(Types::I32)
      struct.read(prot)

      struct.simple.should == 42
      struct.complex.should be_nil
      struct.words.should == "foobar"
      struct.hello.should == Hello.new(:greeting => 'hello, world!')
      struct.ints.should == [1, 2, 2, 3]
      struct.shorts.should == Set.new([5, 17, 239])
    end

    it "should write itself to the wire" do
      prot = mock("Protocol")
      prot.should_receive(:write_struct_begin).with("SpecNamespace::Foo")
      prot.should_receive(:write_struct_end)
      prot.should_receive(:write_field_begin).with('ints', Types::LIST, 4)
      prot.should_receive(:write_field_begin).with('complex', Types::MAP, 5)
      prot.should_receive(:write_field_begin).with('shorts', Types::SET, 6)
      prot.should_receive(:write_field_stop)
      prot.should_receive(:write_field_end).exactly(3).times
      prot.should_receive(:write_field).with('simple', Types::I32, 1, 53)
      prot.should_receive(:write_field).with('hello', Types::STRUCT, 3, Hello.new(:greeting => 'hello, world!'))
      prot.should_receive(:write_map_begin).with(Types::I32, Types::MAP, 1)
      prot.should_receive(:write_map_begin).with(Types::STRING, Types::DOUBLE, 1)
      prot.should_receive(:write_type).with(Types::I32, 5) # complex/1/key
      prot.should_receive(:write_type).with(Types::STRING, "foo") # complex/1/value/1/key
      prot.should_receive(:write_type).with(Types::DOUBLE, 1.23) # complex/1/value/1/value
      prot.should_receive(:write_map_end).twice
      prot.should_receive(:write_list_begin).with(Types::I32, 4)
      prot.should_receive(:write_type).with(Types::I32, 1)
      prot.should_receive(:write_type).with(Types::I32, 2).twice
      prot.should_receive(:write_type).with(Types::I32, 3)
      prot.should_receive(:write_list_end)
      prot.should_receive(:write_set_begin).with(Types::I16, 3)
      prot.should_receive(:write_type).with(Types::I16, 5)
      prot.should_receive(:write_type).with(Types::I16, 17)
      prot.should_receive(:write_type).with(Types::I16, 239)
      prot.should_receive(:write_set_end)

      struct = Foo.new
      struct.words = nil
      struct.complex = {5 => {"foo" => 1.23}}
      struct.write(prot)
    end

    it "should raise an exception if presented with an unknown container" do
      # yeah this is silly, but I'm going for code coverage here
      struct = Foo.new
      lambda { struct.send :write_container, nil, nil, {:type => "foo"} }.should raise_error(StandardError, "Not a container type: foo")
    end

    it "should support optional type-checking in Thrift::Struct.new" do
      Thrift.type_checking = true
      begin
        lambda { Hello.new(:greeting => 3) }.should raise_error(TypeError, "Expected Types::STRING, received Fixnum for field greeting")
      ensure
        Thrift.type_checking = false
      end
      lambda { Hello.new(:greeting => 3) }.should_not raise_error(TypeError)
    end

    it "should support optional type-checking in field accessors" do
      Thrift.type_checking = true
      begin
        hello = Hello.new
        lambda { hello.greeting = 3 }.should raise_error(TypeError, "Expected Types::STRING, received Fixnum for field greeting")
      ensure
        Thrift.type_checking = false
      end
      lambda { hello.greeting = 3 }.should_not raise_error(TypeError)
    end

    it "should raise an exception when unknown types are given to Thrift::Struct.new" do
      lambda { Hello.new(:fish => 'salmon') }.should raise_error(Exception, "Unknown keys given to SpecNamespace::Hello.new: fish")
      lambda { Hello.new(:foo => 'bar', :baz => 'blah', :greeting => 'Good day') }.should raise_error(Exception, /^Unknown keys given to SpecNamespace::Hello.new: (foo, baz|baz, foo)$/)
    end
  end
end
