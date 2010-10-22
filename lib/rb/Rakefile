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

require 'rubygems'
require 'rake'
require 'spec/rake/spectask'

THRIFT = '../../compiler/cpp/thrift'

task :default => [:spec]

task :spec => [:'gen-rb', :realspec]

Spec::Rake::SpecTask.new(:realspec) do |t|
  t.spec_files = FileList['spec/**/*_spec.rb']
  t.spec_opts = ['--color']
end

Spec::Rake::SpecTask.new(:'spec:rcov') do |t|
  t.spec_files = FileList['spec/**/*_spec.rb']
  t.spec_opts = ['--color']
  t.rcov = true
  t.rcov_opts = ['--exclude', '^spec,/gems/']
end

desc 'Run the compiler tests (requires full thrift checkout)'
task :test do
  # ensure this is a full thrift checkout and not a tarball of the ruby libs
  cmd = 'head -1 ../../README 2>/dev/null | grep Thrift >/dev/null 2>/dev/null'
  system(cmd) or fail "rake test requires a full thrift checkout"
  sh 'make', '-C', File.dirname(__FILE__) + "/../../test/rb", "check"
end

desc 'Compile the .thrift files for the specs'
task :'gen-rb' => [:'gen-rb:spec', :'gen-rb:benchmark', :'gen-rb:debug_proto']

namespace :'gen-rb' do
  task :'spec' do
    dir = File.dirname(__FILE__) + '/spec'
    sh THRIFT, '--gen', 'rb', '-o', dir, "#{dir}/ThriftSpec.thrift"
  end

  task :'benchmark' do
    dir = File.dirname(__FILE__) + '/benchmark'
    sh THRIFT, '--gen', 'rb', '-o', dir, "#{dir}/Benchmark.thrift"
  end
  
  task :'debug_proto' do
    sh "mkdir", "-p", "debug_proto_test"
    sh THRIFT, '--gen', 'rb', "-o", "debug_proto_test", "../../test/DebugProtoTest.thrift"
  end
end

desc 'Run benchmarking of NonblockingServer'
task :benchmark do
  ruby 'benchmark/benchmark.rb'
end


begin
  require 'echoe'

  Echoe.new('thrift') do |p|
    p.author = ['Thrift Developers']
    p.email = ['thrift-dev@incubator.apache.org']
    p.summary = "Ruby bindings for the Apache Thrift RPC system"
    p.url = "http://incubator.apache.org/thrift/"
    p.include_rakefile = true
    p.version = "0.6.0-dev"
    p.rubygems_version = ">= 1.2.0"
  end

  task :install => [:check_site_lib]

  require 'rbconfig'
  task :check_site_lib do
    if File.exist?(File.join(Config::CONFIG['sitelibdir'], 'thrift.rb'))
      fail "thrift is already installed in site_ruby"
    end
  end
rescue LoadError
  [:install, :package].each do |t|
    desc "Stub for #{t}"
    task t do
      fail "The Echoe gem is required for this task"
    end
  end
end
