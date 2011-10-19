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
require 'rake/clean'
require 'spec/rake/spectask'

THRIFT = '../../compiler/cpp/thrift'

task :default => [:gem]
task :spec => [:'gen-rb', :build_ext, :realspec]

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
    sh "mkdir", "-p", "test/debug_proto"
    sh THRIFT, '--gen', 'rb', "-o", "test/debug_proto", "../../test/DebugProtoTest.thrift"
  end
end

desc "Build the native library"
task :build_ext => :'gen-rb' do
   Dir::chdir(File::dirname('ext/extconf.rb')) do
      unless sh "ruby #{File::basename('ext/extconf.rb')}"
        $stderr.puts "Failed to run extconf"
          break
      end
      unless sh "make"
        $stderr.puts "make failed"
        break
      end
    end
end

desc 'Run the compiler tests (requires full thrift checkout)'
task :test do
  # ensure this is a full thrift checkout and not a tarball of the ruby libs
  cmd = 'head -1 ../../README 2>/dev/null | grep Thrift >/dev/null 2>/dev/null'
  system(cmd) or fail "rake test requires a full thrift checkout"
  sh 'make', '-C', File.dirname(__FILE__) + "/../../test/rb", "check"
end

desc 'Run benchmarking of NonblockingServer'
task :benchmark do
  ruby 'benchmark/benchmark.rb'
end

desc 'Builds the thrift gem'
task :gem => [:spec, :build_ext] do
  unless sh 'gem', 'build', 'thrift.gemspec'
    $stderr.puts "Failed to build thrift gem"
    break
  end
end

desc 'Install the thrift gem'
task :install => [:gem] do
  unless sh 'gem', 'install', 'thrift-*.gem'
    $stderr.puts "Failed to install thrift gem"
    break
  end
end

CLEAN.include [ 'ext/*.{o,bundle,so,dll}', 'mkmf.log', 'ext/mkmf.log', 'ext/Makefile', 
  'Gemfile.lock', '.bundle', 
  'spec/gen-rb', 'test', 'benchmark/gen-rb',
  'pkg', 'thrift-*.gem'
]
