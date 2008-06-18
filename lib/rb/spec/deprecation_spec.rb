require File.dirname(__FILE__) + '/spec_helper'

describe 'deprecate!' do
  def stub_stderr(callstr)
    STDERR.should_receive(:puts).with("Warning: calling deprecated method #{callstr}")
  end

  it "should work for Module methods" do
    mod = Module.new do
      class << self
        def new
          "new"
        end
        deprecate! :old => :new
      end
    end
    stub_stderr "#{mod.inspect}.old"
    mod.old.should == "new"
  end

  it "should work with Modules that extend themselves" do
    mod = Module.new do
      extend self
      def new
        "new"
      end
      deprecate! :old => :new
    end
    stub_stderr "#{mod.inspect}.old"
    mod.old.should == "new"
  end

  it "should work wtih Class methods" do
    klass = Class.new do
      class << self
        def new
          "new"
        end
        deprecate! :old => :new
      end
    end
    stub_stderr "#{klass.inspect}.old"
    klass.old.should == "new"
  end

  it "should work with Classes that include Modules" do
    mod = Module.new do
      def new
        "new"
      end
      deprecate! :old => :new
    end
    klass = Class.new do
      include mod
    end
    stub_stderr "#{klass.inspect}#old"
    klass.new.old.should == "new"
  end

  it "should work with instance methods" do
    klass = Class.new do
      def new
        "new"
      end
      deprecate! :old => :new
    end
    stub_stderr "#{klass.inspect}#old"
    klass.new.old.should == "new"
  end

  it "should work with multiple method names" do
    klass = Class.new do
      def new1
        "new 1"
      end
      def new2
        "new 2"
      end
      deprecate! :old1 => :new1, :old2 => :new2
    end
    stub_stderr("#{klass.inspect}#old1").ordered
    stub_stderr("#{klass.inspect}#old2").ordered
    inst = klass.new
    inst.old1.should == "new 1"
    inst.old2.should == "new 2"
  end

  it "should only log a message once, even across multiple instances" do
    klass = Class.new do
      def new
        "new"
      end
      deprecate! :old => :new
    end
    stub_stderr("#{klass.inspect}#old").once
    klass.new.old.should == "new"
    klass.new.old.should == "new"
  end

  it "should pass arguments" do
    klass = Class.new do
      def new(a, b)
        "new: #{a + b}"
      end
      deprecate! :old => :new
    end
    stub_stderr("#{klass.inspect}#old")
    klass.new.old(3, 5).should == "new: 8"
  end

  it "should pass blocks" do
    klass = Class.new do
      def new
        "new #{yield}"
      end
      deprecate! :old => :new
    end
    stub_stderr("#{klass.inspect}#old")
    klass.new.old { "block" }.should == "new block"
  end

  it "should not freeze the definition of the new method" do
    klass = Class.new do
      def new
        "new"
      end
      deprecate! :old => :new
    end
    klass.send :define_method, :new do
      "new 2"
    end
    stub_stderr("#{klass.inspect}#old")
    klass.new.old.should == "new 2"
  end
end

describe "deprecate_class!" do
  it "should create a new global constant that points to the old one" do
    begin
      klass = Class.new do
        def foo
          "foo"
        end
      end
      deprecate_class! :DeprecationSpecOldClass => klass
      DeprecationSpecOldClass.should eql(klass)
      DeprecationSpecOldClass.new.foo.should == "foo"
    ensure
      Object.send :remove_const, :DeprecationSpecOldClass
    end
  end
end
