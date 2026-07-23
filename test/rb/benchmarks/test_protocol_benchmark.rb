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

require File.expand_path("../test_helper", __dir__)
require "open3"
require_relative "protocol_benchmark"

class ProtocolBenchmarkTest < Test::Unit::TestCase
  def test_defaulted_struct_read_scenarios
    ids = ProtocolBenchmark::ALL_SCENARIO_IDS.grep(/-read-defaults-/)
    ids -= ProtocolBenchmark::NATIVE_SCENARIO_IDS unless ProtocolBenchmark.native_available?
    scenarios = ProtocolBenchmark.build_scenarios(large_runs: 1, small_runs: 2, scenario_ids: ids)
    expected = TestNamespace::Hello.new(complexer: {1 => {2 => 3}})

    assert_equal ids.sort, scenarios.map { |entry| entry[:id] }.sort
    scenarios.each do |entry|
      value = entry[:job].call
      second = entry[:job].call
      assert_equal expected, value, entry[:id]
      assert_equal expected, second, entry[:id]
      assert_not_same TestNamespace::Hello::FIELDS[2][:default], value.complex, entry[:id]
      assert_not_same TestNamespace::Hello::FIELDS[5][:default], value.thinz, entry[:id]
      assert_not_same second.complex, value.complex, entry[:id]
      assert_not_same second.thinz, value.thinz, entry[:id]
      value.complex.clear
      value.thinz.val = -1
      assert_equal expected, second, entry[:id]
      assert_equal expected.complex, TestNamespace::Hello::FIELDS[2][:default], entry[:id]
      assert_equal expected.thinz, TestNamespace::Hello::FIELDS[5][:default], entry[:id]
    end
  end

  def test_default_run_skips_missing_default_fixtures
    stdout, stderr, status = without_default_fixtures(nil)

    assert_predicate status, :success?, stderr
    ids = JSON.parse(stdout)
    assert_include ids, "rb-bin-read-small"
    assert_empty ids.grep(/-read-defaults-/)
    assert_equal "Skipping defaulted struct benchmarks: run `make stubs` in test/rb first.\n", stderr
  end

  def test_explicit_default_scenario_explains_missing_fixtures
    stdout, stderr, status = without_default_fixtures(["rb-bin-read-defaults-full"])

    assert_predicate status, :success?, stderr
    assert_equal ["ArgumentError", "defaulted struct benchmarks require generated fixtures; run `make stubs` in test/rb first."], JSON.parse(stdout)
    assert_empty stderr
  end

  def test_other_scenarios_do_not_need_default_fixtures
    stdout, stderr, status = without_default_fixtures(["rb-bin-read-small"])

    assert_predicate status, :success?, stderr
    assert_equal ["rb-bin-read-small"], JSON.parse(stdout)
    assert_empty stderr
  end

  private

  def without_default_fixtures(ids)
    # Isolate the missing-file simulation; do not move generated files in the checkout.
    script = <<~'RUBY'
      require "json"
      if ENV["THRIFT_BENCHMARK_SKIP_NATIVE"] == "1"
        $LOADED_FEATURES << File.expand_path("../../../lib/rb/lib/thrift/thrift_native.rb", ARGV[0])
      end
      fixture = File.expand_path("../gen-rb/small_test_types.rb", ARGV[0])
      File.singleton_class.prepend(Module.new do
        define_method(:file?) { |path| path == fixture ? false : super(path) }
      end)
      Kernel.prepend(Module.new do
        define_method(:require) do |path|
          raise LoadError, "cannot load such file -- #{path}" if path == fixture || "#{path}.rb" == fixture
          super(path)
        end
      end)
      require File.join(ARGV[0], "protocol_benchmark")
      begin
        scenarios = ProtocolBenchmark.build_scenarios(large_runs: 1, small_runs: 1, scenario_ids: JSON.parse(ARGV[1]))
        puts JSON.generate(scenarios.map { |entry| entry[:id] })
      rescue ArgumentError => error
        puts JSON.generate([error.class.name, error.message])
      end
    RUBY
    Open3.capture3(RbConfig.ruby, "-e", script, __dir__, JSON.generate(ids))
  end
end
