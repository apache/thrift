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

require File.expand_path(File.join(File.dirname(__FILE__), 'spec_helper'))

class StructNestedContainersSpec < Spec::ExampleGroup
  include Thrift
  include SpecNamespace

  def with_type_checking
    saved_type_checking, Thrift.type_checking = Thrift.type_checking, true
    begin
      yield
    ensure
      Thrift.type_checking = saved_type_checking
    end
  end

  describe Struct do
    # Nested container tests, see THRIFT-369.
    it "should support nested lists inside lists" do
      with_type_checking do
        a, b = NestedListInList.new, NestedListInList.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = [ [1, 2, 3], [2, 3, 4] ]
          thrift_struct.validate
        end
        a.should == b
        b.value.push [3, 4, 5]
        a.should_not == b
      end
    end

    it "should support nested lists inside sets" do
      with_type_checking do
        a, b = NestedListInSet.new, NestedListInSet.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = [ [1, 2, 3], [2, 3, 4] ].to_set
          thrift_struct.validate
        end
        a.should == b
        b.value.add [3, 4, 5]
        a.should_not == b
      end
    end

    it "should support nested lists in map keys" do
      with_type_checking do
        a, b = NestedListInMapKey.new, NestedListInMapKey.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = { [1, 2, 3] => 1, [2, 3, 4] => 2 }
          thrift_struct.validate
        end
        a.should == b
        b.value[[3, 4, 5]] = 3
        a.should_not == b
      end
    end

    it "should support nested lists in map values" do
      with_type_checking do
        a, b = NestedListInMapValue.new, NestedListInMapValue.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = { 1 => [1, 2, 3], 2 => [2, 3, 4] }
          thrift_struct.validate
        end
        a.should == b
        b.value[3] = [3, 4, 5]
        a.should_not == b
      end
    end

    it "should support nested sets inside lists" do
      with_type_checking do
        a, b = NestedSetInList.new, NestedSetInList.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = [ [1, 2, 3].to_set, [2, 3, 4].to_set ]
          thrift_struct.validate
        end
        a.should == b
        b.value.push([3, 4, 5].to_set)
        a.should_not == b
      end
    end

    it "should support nested sets inside sets" do
      with_type_checking do
        a, b = NestedSetInSet.new, NestedSetInSet.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = [ [1, 2, 3].to_set, [2, 3, 4].to_set ].to_set
          thrift_struct.validate
        end
        a.should == b
        b.value.add([3, 4, 5].to_set)
        a.should_not == b
      end
    end

    it "should support nested sets in map keys" do
      with_type_checking do
        a, b = NestedSetInMapKey.new, NestedSetInMapKey.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = { [1, 2, 3].to_set => 1, [2, 3, 4].to_set => 2 }
          thrift_struct.validate
        end
        a.should == b
        b.value[[3, 4, 5].to_set] = 3
        a.should_not == b
      end
    end

    it "should support nested sets in map values" do
      with_type_checking do
        a, b = NestedSetInMapValue.new, NestedSetInMapValue.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = { 1 => [1, 2, 3].to_set, 2 => [2, 3, 4].to_set }
          thrift_struct.validate
        end
        a.should == b
        b.value[3] = [3, 4, 5].to_set
        a.should_not == b
      end
    end

    it "should support nested maps inside lists" do
      with_type_checking do
        a, b = NestedMapInList.new, NestedMapInList.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = [ {1 => 2, 3 => 4}, {2 => 3, 4 => 5} ]
          thrift_struct.validate
        end
        a.should == b
        b.value.push({ 3 => 4, 5 => 6 })
        a.should_not == b
      end
    end

    it "should support nested maps inside sets" do
      with_type_checking do
        a, b = NestedMapInSet.new, NestedMapInSet.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = [ {1 => 2, 3 => 4}, {2 => 3, 4 => 5} ].to_set
          thrift_struct.validate
        end
        a.should == b
        b.value.add({ 3 => 4, 5 => 6 })
        a.should_not == b
      end
    end

    it "should support nested maps in map keys" do
      with_type_checking do
        a, b = NestedMapInMapKey.new, NestedMapInMapKey.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = { { 1 => 2, 3 => 4} => 1, {2 => 3, 4 => 5}  => 2 }
          thrift_struct.validate
        end
        a.should == b
        b.value[{3 => 4, 5 => 6}] = 3
        a.should_not == b
      end
    end

    it "should support nested maps in map values" do
      with_type_checking do
        a, b = NestedMapInMapValue.new, NestedMapInMapValue.new
        [a, b].each do |thrift_struct|
          thrift_struct.value = { 1 => { 1 => 2, 3 => 4}, 2 => {2 => 3, 4 => 5} }
          thrift_struct.validate
        end
        a.should == b
        b.value[3] = { 3 => 4, 5 => 6 }
        a.should_not == b
      end
    end
  end
end
