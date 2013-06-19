require 'spec_helper'

shared_examples_for 'a memory buffer' do
  before(:each) do
    @trans = Thrift::MemoryBufferTransport.new
    @prot = protocol_class.new(@trans)
  end


  it 'should accpet bytes' do
  	1.should(12);
  end
 end
