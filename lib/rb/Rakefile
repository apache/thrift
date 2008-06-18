require 'rubygems'
require 'rake'
require 'spec/rake/spectask'

task :default => :spec

Spec::Rake::SpecTask.new("spec") do |t|
  t.spec_files = FileList['spec/**/*_spec.rb']
  t.spec_opts = ['--color']
end
