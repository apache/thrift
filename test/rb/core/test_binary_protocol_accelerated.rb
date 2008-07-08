require File.join(File.dirname(__FILE__), '../test_helper')
require File.join(File.dirname(__FILE__), '../fixtures/structs')

require 'thrift'
require 'thrift/transport'
require 'thrift/protocol/binaryprotocol'
require 'thrift/protocol/binaryprotocolaccelerated'

class BinaryProtocolAcceleratedTest < Test::Unit::TestCase
  I8_MIN = -128
  I8_MAX = 127
  I16_MIN = -32768
  I16_MAX = 32767
  I32_MIN = -2147483648
  I32_MAX = 2147483647
  I64_MIN = -9223372036854775808
  I64_MAX = 9223372036854775807
  DBL_MIN = Float::MIN
  DBL_MAX = Float::MAX

  # booleans might be read back differently, so we supply a list [write_value, read_value]
  BOOL_VALUES = [[0,true], [14,true], [-14,true], [true,true], [false,false], ["",true]]
  BYTE_VALUES = [14, -14, I8_MIN, I8_MAX]
  I16_VALUES = [400, 0, -234, I16_MIN, I16_MAX]
  I32_VALUES = [325, 0, -1, -1073741825, -278, -4352388, I32_MIN, I32_MAX]
  I64_VALUES = [15, 0, -33, I64_MIN, I64_MAX]
  DBL_VALUES = [DBL_MIN, -33.8755, 0, 3658.1279, DBL_MAX]
  STR_VALUES = ["", "welcome to my test"]
  
  def setup
    @trans = Thrift::MemoryBuffer.new
    @fast_proto = Thrift::BinaryProtocolAccelerated.new(@trans)
    @slow_proto = Thrift::BinaryProtocol.new(@trans)
  end
  
  def assert_encodes_struct(obj)
    obj.write(@slow_proto)    
    expected = @trans.read(@trans.available) # read it all baby
    assert_equal expected, @fast_proto.encode_binary(obj)
  end
  
  # Assumes encode works
  def assert_decodes_struct(obj, eql_obj = nil)
    data = @fast_proto.encode_binary(obj)
    @trans.write data
    assert_equal (eql_obj || obj), @fast_proto.decode_binary(obj.class.new, @trans)
  end
  
  def test_encodes_and_decodes_bools
    BOOL_VALUES.each do |(write_val, read_val)|
      obj = Fixtures::Structs::OneBool.new(:bool => write_val)
      assert_encodes_struct obj
      assert_decodes_struct obj, Fixtures::Structs::OneBool.new(:bool => read_val)
    end
  end
  
  def test_encodes_and_decodes_bytes
    BYTE_VALUES.each do |val|
      obj = Fixtures::Structs::OneByte.new(:byte => val)
      assert_encodes_struct obj
      assert_decodes_struct obj
    end
  end
  
  def test_encodes_and_decodes_i16
    I16_VALUES.each do |val|
      obj = Fixtures::Structs::OneI16.new(:i16 => val)
      assert_encodes_struct obj
      assert_decodes_struct obj
    end
  end
  
  def test_encodes_and_decodes_i32
    I32_VALUES.each do |val|
      obj = Fixtures::Structs::OneI32.new(:i32 => val)
      assert_encodes_struct obj
      assert_decodes_struct obj
    end
  end
  
  def test_encodes_and_decodes_i64
    I64_VALUES.each do |val|
      obj = Fixtures::Structs::OneI64.new(:i64 => val)
      assert_encodes_struct obj
      assert_decodes_struct obj
    end
  end
  
  def test_encodes_and_decodes_double
    DBL_VALUES.each do |val|
      obj = Fixtures::Structs::OneDouble.new(:double => val)
      assert_encodes_struct obj
      assert_decodes_struct obj
    end
  end
  
  def test_encodes_strings
    STR_VALUES.each do |val|
      obj = Fixtures::Structs::OneString.new(:string => val)
      assert_encodes_struct obj
      assert_decodes_struct obj
    end
  end
  
  def test_encodes_maps
    obj = Fixtures::Structs::OneMap.new(:map => {"a" => "b", "c" => "d"})
    assert_encodes_struct obj
    assert_decodes_struct obj
  end
  
  def test_encodes_lists
    obj = Fixtures::Structs::OneList.new(:list => ["a", "b", "c", "d"])
    assert_encodes_struct obj
    assert_decodes_struct obj
  end
  
  def test_encodes_sets
    expected_return = Fixtures::Structs::OneSet.new(:set => Set.new(["a", "b", "c"]))
    # support hashes
    obj = Fixtures::Structs::OneSet.new(:set => {"a" => true, "b" => true, "c" => true})
    assert_encodes_struct obj
    assert_decodes_struct obj, expected_return
    
    # We also support arrays as compatability bug from TBinaryProtocol....
    obj = Fixtures::Structs::OneSet.new(:set => ["a", "b", "c"])
    assert_encodes_struct obj
    assert_decodes_struct obj, expected_return

    # We also support sets
    obj = Fixtures::Structs::OneSet.new(:set => Set.new(["a", "b", "c"]))
    assert_encodes_struct obj
    assert_decodes_struct obj, expected_return
  end
  
  def test_encodes_maps_of_maps
    obj = Fixtures::Structs::NestedMap.new(:map => { 1 => { 2 => 3 }})
    assert_encodes_struct obj
    assert_decodes_struct obj
  end
  
  def test_encodes_list_of_lists
    obj = Fixtures::Structs::NestedList.new(:list => [[1,2,3], [4,5,6]])
    assert_encodes_struct obj
    assert_decodes_struct obj
  end

  def test_encodes_set_of_sets
    obj = Fixtures::Structs::NestedSet.new(:set => Set.new([Set.new('a')]))
    assert_encodes_struct obj

    # Nested hashes won't work with ==, so we do it by hand
    data = @fast_proto.encode_binary(obj)
    @trans.write data
    decoded = @fast_proto.decode_binary(obj.class.new, @trans)
    assert_equal decoded.set.entries, Set.new([Set.new('a')]).entries
  end
  
  if ENV['MEM_TEST']
    def test_for_memory_leaks_on_exceptions
      ooe = Fixtures::Structs::OneOfEach.new
      ooe.im_true   = true
      ooe.im_false  = false
      ooe.a_bite    = -42
      ooe.integer16 = 27000
      ooe.integer32 = 1<<24
      ooe.integer64 = 6000 * 1000 * 1000
      ooe.double_precision = Math::PI
      ooe.some_characters  = "Debug THIS!"
      ooe.zomg_unicode     = "\xd7\n\a\t"
    
      10_000.times do
        data = @fast_proto.encode_binary(ooe)
        bytes = []
        data.each_byte do |b|
          bytes << b
          transport = TMemoryBuffer.new
          transport.write(bytes.pack("c#{bytes.length}"))
      
          begin
            @fast_proto.decode_binary(Fixtures::Structs::OneOfEach.new, transport)
          rescue Exception
          end
        end
      end
    
    end
  end
  
  unless ENV['FAST_TEST']
    def test_encodes_and_decodes_struct_of_structs
      ooe = Fixtures::Structs::OneOfEach.new
      ooe.im_true   = true
      ooe.im_false  = false
      ooe.a_bite    = -42
      ooe.integer16 = 27000
      ooe.integer32 = 1<<24
      ooe.integer64 = 6000 * 1000 * 1000
      ooe.double_precision = Math::PI
      ooe.some_characters  = "Debug THIS!"
      ooe.zomg_unicode     = "\xd7\n\a\t"
  
      n1 = Fixtures::Structs::Nested1.new
      n1.a_list = []
      n1.a_list << ooe << ooe << ooe << ooe
      n1.i32_map = {}
      n1.i32_map[1234] = ooe
      n1.i32_map[46345] = ooe
      n1.i32_map[-34264] = ooe
      n1.i64_map = {}
      n1.i64_map[43534986783945] = ooe
      n1.i64_map[-32434639875122] = ooe
      n1.dbl_map = {}
      n1.dbl_map[324.65469834] = ooe
      n1.dbl_map[-9458672340.4986798345112] = ooe
      n1.str_map = {}
      n1.str_map['sdoperuix'] = ooe
      n1.str_map['pwoerxclmn'] = ooe
    
      n2 = Fixtures::Structs::Nested2.new
      n2.a_list = []
      n2.a_list << n1 << n1 << n1 << n1 << n1
      n2.i32_map = {}
      n2.i32_map[398345] = n1
      n2.i32_map[-2345] = n1
      n2.i32_map[12312] = n1
      n2.i64_map = {}
      n2.i64_map[2349843765934] = n1
      n2.i64_map[-123234985495] = n1
      n2.i64_map[0] = n1
      n2.dbl_map = {}
      n2.dbl_map[23345345.38927834] = n1
      n2.dbl_map[-1232349.5489345] = n1
      n2.dbl_map[-234984574.23498725] = n1
      n2.str_map = {}
      n2.str_map[''] = n1
      n2.str_map['sdflkertpioux'] = n1
      n2.str_map['sdfwepwdcjpoi'] = n1
  
      n3 = Fixtures::Structs::Nested3.new
      n3.a_list = []
      n3.a_list << n2 << n2 << n2 << n2 << n2
      n3.i32_map = {}
      n3.i32_map[398345] = n2
      n3.i32_map[-2345] = n2
      n3.i32_map[12312] = n2
      n3.i64_map = {}
      n3.i64_map[2349843765934] = n2
      n3.i64_map[-123234985495] = n2
      n3.i64_map[0] = n2
      n3.dbl_map = {}
      n3.dbl_map[23345345.38927834] = n2
      n3.dbl_map[-1232349.5489345] = n2
      n3.dbl_map[-234984574.23498725] = n2
      n3.str_map = {}
      n3.str_map[''] = n2
      n3.str_map['sdflkertpioux'] = n2
      n3.str_map['sdfwepwdcjpoi'] = n2
  
      n4 = Fixtures::Structs::Nested4.new
      n4.a_list = []
      n4.a_list << n3
      n4.i32_map = {}
      n4.i32_map[-2345] = n3
      n4.i64_map = {}
      n4.i64_map[2349843765934] = n3
      n4.dbl_map = {}
      n4.dbl_map[-1232349.5489345] = n3
      n4.str_map = {}
      n4.str_map[''] = n3
    
      assert_encodes_struct n4
      assert_decodes_struct n4
    end
  end
end
