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

THRIFT_BENCHMARK_SKIP_NATIVE = ENV.fetch('THRIFT_BENCHMARK_SKIP_NATIVE', '').match?(/\A(?:1|true|yes|on)\z/i)

lib_path = File.expand_path('../../../lib/rb/lib', __dir__)
ext_path = File.expand_path('../../../lib/rb/ext', __dir__)

$LOAD_PATH.unshift lib_path unless $LOAD_PATH.include?(lib_path)

if THRIFT_BENCHMARK_SKIP_NATIVE
  $LOAD_PATH.delete(ext_path)
else
  $LOAD_PATH.unshift ext_path unless $LOAD_PATH.include?(ext_path)
end

if THRIFT_BENCHMARK_SKIP_NATIVE
  File.open(File::NULL, 'w') do |null_stdout|
    original_stdout = $stdout
    $stdout = null_stdout
    begin
      require 'thrift'
    ensure
      $stdout = original_stdout
    end
  end
else
  require 'thrift'
end

require 'benchmark'
require 'json'
require 'optparse'

# require 'ruby-debug'
# require 'ruby-prof'

require File.expand_path('../fixtures/structs', __dir__)

module ProtocolBenchmark
  DEFAULT_LARGE_RUNS = 1
  DEFAULT_SMALL_RUNS = 10_000
  ALL_SCENARIO_IDS = %w[
    rb-bin-write-large rb-bin-read-large c-bin-write-large c-bin-read-large
    rb-cmp-write-large rb-cmp-read-large rb-json-write-large rb-json-read-large
    rb-bin-write-small rb-bin-read-small c-bin-write-small c-bin-read-small
    rb-cmp-write-small rb-cmp-read-small rb-json-write-small rb-json-read-small
    hdr-bin-write-small hdr-bin-read-small hdr-cmp-write-small hdr-cmp-read-small
    hdr-zlib-write-small hdr-zlib-read-small
  ].freeze
  NATIVE_SCENARIO_IDS = %w[
    c-bin-write-large c-bin-read-large c-bin-write-small c-bin-read-small
  ].freeze

  module_function

  def parse_run_options(argv = ARGV, env: ENV)
    options = {
      large_runs: env.fetch('THRIFT_BENCHMARK_LARGE_RUNS', DEFAULT_LARGE_RUNS),
      small_runs: env.fetch('THRIFT_BENCHMARK_SMALL_RUNS', DEFAULT_SMALL_RUNS),
      scenarios: env['THRIFT_BENCHMARK_SCENARIOS'],
      json: false
    }

    OptionParser.new do |parser|
      parser.on('--large-runs N', Integer) { |value| options[:large_runs] = value }
      parser.on('--small-runs N', Integer) { |value| options[:small_runs] = value }
      parser.on('--scenarios IDS', String) { |value| options[:scenarios] = value }
      parser.on('--json') { options[:json] = true }
    end.parse!(argv.dup)

    {
      large_runs: normalize_run_count(options[:large_runs], 'large runs'),
      small_runs: normalize_run_count(options[:small_runs], 'small runs'),
      scenarios: normalize_scenarios(options[:scenarios]),
      json: options[:json]
    }
  end

  def normalize_run_count(value, name)
    count = value.is_a?(String) ? Integer(value, 10) : Integer(value)
    raise ArgumentError, "#{name} must be >= 1" if count < 1

    count
  end

  def large_run_label(count)
    count == 1 ? 'once' : "#{count} times"
  end

  def normalize_scenarios(value)
    return nil if value.nil?

    scenario_ids = value.split(/[\s,]+/).filter_map do |scenario_id|
      normalized = scenario_id.strip
      normalized unless normalized.empty?
    end

    scenario_ids.empty? ? nil : scenario_ids.uniq
  end

  def binary_protocol_builder(accelerated: false)
    protocol_class =
      if accelerated && native_available?
        Thrift::BinaryProtocolAccelerated
      else
        Thrift::BinaryProtocol
      end

    lambda do |buffer = nil|
      transport = Thrift::MemoryBufferTransport.new(buffer)
      [transport, protocol_class.new(transport)]
    end
  end

  def compact_protocol_builder
    lambda do |buffer = nil|
      transport = Thrift::MemoryBufferTransport.new(buffer)
      [transport, Thrift::CompactProtocol.new(transport)]
    end
  end

  def json_protocol_builder
    lambda do |buffer = nil|
      transport = Thrift::MemoryBufferTransport.new(buffer)
      [transport, Thrift::JsonProtocol.new(transport)]
    end
  end

  def header_protocol_builder(default_protocol:, zlib: false)
    lambda do |buffer = nil|
      transport = Thrift::MemoryBufferTransport.new(buffer)
      protocol = Thrift::HeaderProtocol.new(transport, nil, default_protocol)
      protocol.add_transform(Thrift::HeaderTransformID::ZLIB) if zlib
      [transport, protocol]
    end
  end

  def serialize(builder, value, count: 1)
    transport, protocol = builder.call

    count.times do
      value.write(protocol)
      flush(protocol)
    end

    transport.read(transport.available)
  end

  def deserialize(builder, struct_class, payload, count: 1)
    _transport, protocol = builder.call(payload.dup)
    value = nil

    count.times do
      value = struct_class.new
      value.read(protocol)
    end

    value
  end

  def write(builder, value, count: 1)
    _transport, protocol = builder.call

    count.times do
      value.write(protocol)
      flush(protocol)
    end
  end

  def flush(protocol)
    protocol.trans.flush if protocol.trans.is_a?(Thrift::HeaderTransport)
  end

  def build_sample_structs
    ooe = Fixtures::Structs::OneOfEach.new
    ooe.im_true = true
    ooe.im_false = false
    ooe.a_bite = -42
    ooe.integer16 = 27_000
    ooe.integer32 = 1 << 24
    ooe.integer64 = 6000 * 1000 * 1000
    ooe.double_precision = Math::PI
    ooe.some_characters = 'Debug THIS!'
    ooe.zomg_unicode = "\u00D7\n\a\t"

    n1 = Fixtures::Structs::Nested1.new
    n1.a_list = [ooe, ooe, ooe, ooe]
    n1.i32_map = {1234 => ooe, 46_345 => ooe, -34_264 => ooe}
    n1.i64_map = {43_534_986_783_945 => ooe, -32_434_639_875_122 => ooe}
    n1.dbl_map = {324.65469834 => ooe, -9_458_672_340.49868 => ooe}
    n1.str_map = {'sdoperuix' => ooe, 'pwoerxclmn' => ooe}

    n2 = Fixtures::Structs::Nested2.new
    n2.a_list = [n1, n1, n1, n1, n1]
    n2.i32_map = {398_345 => n1, -2345 => n1, 12_312 => n1}
    n2.i64_map = {2_349_843_765_934 => n1, -123_234_985_495 => n1, 0 => n1}
    n2.dbl_map = {23_345_345.38927834 => n1, -1_232_349.5489345 => n1, -234_984_574.23498726 => n1}
    n2.str_map = {'' => n1, 'sdflkertpioux' => n1, 'sdfwepwdcjpoi' => n1}

    n3 = Fixtures::Structs::Nested3.new
    n3.a_list = [n2, n2, n2, n2, n2]
    n3.i32_map = {398_345 => n2, -2345 => n2, 12_312 => n2}
    n3.i64_map = {2_349_843_765_934 => n2, -123_234_985_495 => n2, 0 => n2}
    n3.dbl_map = {23_345_345.38927834 => n2, -1_232_349.5489345 => n2, -234_984_574.23498726 => n2}
    n3.str_map = {'' => n2, 'sdflkertpioux' => n2, 'sdfwepwdcjpoi' => n2}

    n4 = Fixtures::Structs::Nested4.new
    n4.a_list = [n3]
    n4.i32_map = {-2345 => n3}
    n4.i64_map = {2_349_843_765_934 => n3}
    n4.dbl_map = {-1_232_349.5489345 => n3}
    n4.str_map = {'' => n3}

    [ooe, n4]
  end

  def scenario(id, label, &job)
    {id: id, label: label, job: job}
  end

  def native_available?
    Thrift.const_defined?(:BinaryProtocolAccelerated, false)
  end

  def with_scenario_selected(requested_ids, *ids)
    selected = requested_ids.nil? || ids.any? { |id| requested_ids.include?(id) }
    return false unless selected
    return true unless block_given?

    yield
  end

  def select_scenarios(scenarios, requested_ids, native_available:)
    return scenarios if requested_ids.nil?

    unknown_ids = requested_ids - ALL_SCENARIO_IDS
    raise ArgumentError, "unknown scenarios: #{unknown_ids.join(', ')}" if unknown_ids.any?

    unavailable_native_ids = requested_ids & NATIVE_SCENARIO_IDS unless native_available
    if unavailable_native_ids&.any?
      raise ArgumentError, "native-only scenarios unavailable without thrift_native: #{unavailable_native_ids.join(', ')}"
    end

    scenarios.select { |entry| requested_ids.include?(entry[:id]) }
  end

  def build_scenarios(large_runs:, small_runs:, scenario_ids: nil)
    unknown_ids = scenario_ids ? scenario_ids - ALL_SCENARIO_IDS : []
    raise ArgumentError, "unknown scenarios: #{unknown_ids.join(', ')}" if unknown_ids.any?

    one_of_each, nested4 = build_sample_structs

    ruby_binary = binary_protocol_builder
    ruby_compact = compact_protocol_builder
    ruby_json = json_protocol_builder
    accelerated_binary = binary_protocol_builder(accelerated: true)
    header_binary = header_protocol_builder(default_protocol: Thrift::HeaderSubprotocolID::BINARY)
    header_compact = header_protocol_builder(default_protocol: Thrift::HeaderSubprotocolID::COMPACT)
    header_zlib = header_protocol_builder(default_protocol: Thrift::HeaderSubprotocolID::BINARY, zlib: true)

    native_available = native_available?
    unavailable_native_ids = native_available ? [] : (scenario_ids || []) & NATIVE_SCENARIO_IDS
    if unavailable_native_ids.any?
      raise ArgumentError, "native-only scenarios unavailable without thrift_native: #{unavailable_native_ids.join(', ')}"
    end

    native_scenarios = []

    ruby_large_payload = with_scenario_selected(scenario_ids, 'rb-bin-read-large') { serialize(ruby_binary, nested4, count: large_runs) }
    ruby_small_payload = with_scenario_selected(scenario_ids, 'rb-bin-read-small') { serialize(ruby_binary, one_of_each, count: small_runs) }
    compact_large_payload = with_scenario_selected(scenario_ids, 'rb-cmp-read-large') { serialize(ruby_compact, nested4, count: large_runs) }
    compact_small_payload = with_scenario_selected(scenario_ids, 'rb-cmp-read-small') { serialize(ruby_compact, one_of_each, count: small_runs) }
    json_large_payload = with_scenario_selected(scenario_ids, 'rb-json-read-large') { serialize(ruby_json, nested4, count: large_runs) }
    json_small_payload = with_scenario_selected(scenario_ids, 'rb-json-read-small') { serialize(ruby_json, one_of_each, count: small_runs) }
    header_binary_payload = with_scenario_selected(scenario_ids, 'hdr-bin-read-small') { serialize(header_binary, one_of_each, count: small_runs) }
    header_compact_payload = with_scenario_selected(scenario_ids, 'hdr-cmp-read-small') { serialize(header_compact, one_of_each, count: small_runs) }
    header_zlib_payload = with_scenario_selected(scenario_ids, 'hdr-zlib-read-small') { serialize(header_zlib, one_of_each, count: small_runs) }

    if native_available
      accelerated_large_payload = with_scenario_selected(scenario_ids, 'c-bin-read-large') { serialize(accelerated_binary, nested4, count: large_runs) }
      accelerated_small_payload = with_scenario_selected(scenario_ids, 'c-bin-read-small') { serialize(accelerated_binary, one_of_each, count: small_runs) }

      native_scenarios = [
        scenario('c-bin-write-large', "c binary write large (1MB) structure #{large_run_label(large_runs)}") { write(accelerated_binary, nested4, count: large_runs) },
        scenario('c-bin-read-large', "c binary read large (1MB) structure #{large_run_label(large_runs)}") { deserialize(accelerated_binary, Fixtures::Structs::Nested4, accelerated_large_payload, count: large_runs) },
        scenario('c-bin-write-small', "c binary write #{small_runs} small structures") { write(accelerated_binary, one_of_each, count: small_runs) },
        scenario('c-bin-read-small', "c binary read #{small_runs} small structures") { deserialize(accelerated_binary, Fixtures::Structs::OneOfEach, accelerated_small_payload, count: small_runs) }
      ]
    elsif !THRIFT_BENCHMARK_SKIP_NATIVE && with_scenario_selected(scenario_ids, *NATIVE_SCENARIO_IDS)
      warn 'Skipping accelerated binary protocol benchmarks: thrift_native extension is unavailable.'
    end

    scenario_list = [
      scenario('rb-bin-write-large', "ruby binary write large (1MB) structure #{large_run_label(large_runs)}") { write(ruby_binary, nested4, count: large_runs) },
      scenario('rb-bin-read-large', "ruby binary read large (1MB) structure #{large_run_label(large_runs)}") { deserialize(ruby_binary, Fixtures::Structs::Nested4, ruby_large_payload, count: large_runs) },
      *native_scenarios.first(2),
      scenario('rb-cmp-write-large', "ruby compact write large (1MB) structure #{large_run_label(large_runs)}") { write(ruby_compact, nested4, count: large_runs) },
      scenario('rb-cmp-read-large', "ruby compact read large (1MB) structure #{large_run_label(large_runs)}") { deserialize(ruby_compact, Fixtures::Structs::Nested4, compact_large_payload, count: large_runs) },
      scenario('rb-json-write-large', "ruby json write large (1MB) structure #{large_run_label(large_runs)}") { write(ruby_json, nested4, count: large_runs) },
      scenario('rb-json-read-large', "ruby json read large (1MB) structure #{large_run_label(large_runs)}") { deserialize(ruby_json, Fixtures::Structs::Nested4, json_large_payload, count: large_runs) },
      scenario('rb-bin-write-small', "ruby binary write #{small_runs} small structures") { write(ruby_binary, one_of_each, count: small_runs) },
      scenario('rb-bin-read-small', "ruby binary read #{small_runs} small structures") { deserialize(ruby_binary, Fixtures::Structs::OneOfEach, ruby_small_payload, count: small_runs) },
      *native_scenarios.drop(2),
      scenario('rb-cmp-write-small', "ruby compact write #{small_runs} small structures") { write(ruby_compact, one_of_each, count: small_runs) },
      scenario('rb-cmp-read-small', "ruby compact read #{small_runs} small structures") { deserialize(ruby_compact, Fixtures::Structs::OneOfEach, compact_small_payload, count: small_runs) },
      scenario('rb-json-write-small', "ruby json write #{small_runs} small structures") { write(ruby_json, one_of_each, count: small_runs) },
      scenario('rb-json-read-small', "ruby json read #{small_runs} small structures") { deserialize(ruby_json, Fixtures::Structs::OneOfEach, json_small_payload, count: small_runs) },
      scenario('hdr-bin-write-small', "header binary write #{small_runs} small structures") { write(header_binary, one_of_each, count: small_runs) },
      scenario('hdr-bin-read-small', "header binary read #{small_runs} small structures") { deserialize(header_binary, Fixtures::Structs::OneOfEach, header_binary_payload, count: small_runs) },
      scenario('hdr-cmp-write-small', "header compact write #{small_runs} small structures") { write(header_compact, one_of_each, count: small_runs) },
      scenario('hdr-cmp-read-small', "header compact read #{small_runs} small structures") { deserialize(header_compact, Fixtures::Structs::OneOfEach, header_compact_payload, count: small_runs) },
      scenario('hdr-zlib-write-small', "header zlib write #{small_runs} small structures") { write(header_zlib, one_of_each, count: small_runs) },
      scenario('hdr-zlib-read-small', "header zlib read #{small_runs} small structures") { deserialize(header_zlib, Fixtures::Structs::OneOfEach, header_zlib_payload, count: small_runs) }
    ]

    select_scenarios(scenario_list, scenario_ids, native_available: native_available)
  end

  def measure_job(job, label: '')
    result = Benchmark.measure(label, &job)
    {
      user: result.utime,
      system: result.stime,
      total: result.total,
      real: result.real
    }
  end

  def warm_up_scenarios(scenarios)
    scenarios.each { |entry| measure_job(entry[:job]) }
  end

  def benchmark_scenarios(scenarios)
    scenarios.map do |entry|
      GC.start
      {
        id: entry[:id],
        label: entry[:label],
        benchmark: measure_job(entry[:job], label: entry[:label])
      }
    end
  end

  def run(large_runs: DEFAULT_LARGE_RUNS, small_runs: DEFAULT_SMALL_RUNS, scenarios: nil, json: false)
    scenario_list = build_scenarios(large_runs: large_runs, small_runs: small_runs, scenario_ids: scenarios)

    if json
      warm_up_scenarios(scenario_list)

      puts JSON.generate(
        config: {
          large_runs: large_runs,
          small_runs: small_runs,
          scenarios: scenario_list.map { |entry| entry[:id] },
          skip_native: THRIFT_BENCHMARK_SKIP_NATIVE,
          native_available: native_available?
        },
        results: benchmark_scenarios(scenario_list)
      )
      return
    end

    Benchmark.bmbm do |x|
      scenario_list.each do |entry|
        x.report(entry[:label], &entry[:job])
      end
    end
  end
end

if $PROGRAM_NAME == __FILE__
  begin
    ProtocolBenchmark.run(**ProtocolBenchmark.parse_run_options)
  rescue OptionParser::ParseError, ArgumentError => e
    warn e.message
    exit 1
  end
end
