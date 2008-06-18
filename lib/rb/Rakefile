require 'rubygems'
require 'rake'
require 'spec/rake/spectask'

task :default => [:spec, :test]

Spec::Rake::SpecTask.new("spec") do |t|
  t.spec_files = FileList['spec/**/*_spec.rb']
  t.spec_opts = ['--color']
end

Spec::Rake::SpecTask.new("rcov_spec") do |t|
  t.spec_files = FileList['spec/**/*_spec.rb']
  t.spec_opts = ['--color']
  t.rcov = true
  t.rcov_opts = ['--exclude', '^spec,/gems/']
end

task :test do
  sh 'make', '-C', File.dirname(__FILE__) + "/../../test/rb"
end

desc 'Compile the .thrift files for the specs'
task :'gen-rb' => [:'gen-rb-spec', :'gen-rb-benchmark']

THRIFT = '../../compiler/cpp/thrift'

task :'gen-rb-spec' do
  dir = File.dirname(__FILE__) + '/spec'
  sh THRIFT, '--gen', 'rb', '-o', dir, "#{dir}/ThriftSpec.thrift"
end

task :'gen-rb-benchmark' do
  dir = File.dirname(__FILE__) + '/benchmark'
  sh THRIFT, '--gen', 'rb', '-o', dir, "#{dir}/Benchmark.thrift"
end

desc 'Run benchmarking of NonblockingServer'
task :benchmark do
  ruby 'benchmark/benchmark.rb'
end
