# encoding: UTF-8
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

require 'spec_helper'

$:.unshift File.join(File.dirname(__FILE__), *%w[gen-rb/constants_demo])
require 'constants_demo_constants'

describe 'ConstantsDemo' do
  it 'should have correct integer constants' do
    expect(ConstantsDemo::MyInt).to eq(3)
    expect(ConstantsDemo::Hex_const).to eq(0x0001F)
    expect(ConstantsDemo::Negative_hex_constant).to eq(-0x0001F)
    expect(ConstantsDemo::GEN_ME).to eq(-3523553)
  end

  it 'should have correct double constants' do
    expect(ConstantsDemo::GEn_DUB).to eq(325.532)
    expect(ConstantsDemo::GEn_DU).to eq(85.2355)
    expect(ConstantsDemo::E10).to eq(1e+10)
    expect(ConstantsDemo::E11).to eq(-1e+10)
  end

  it 'should have correct string constants' do
    expect(ConstantsDemo::GEN_STRING).to eq("asldkjasfd")
  end

  it 'should have correct uuid constants' do
    expect(ConstantsDemo::GEN_UUID).to eq("00000000-4444-CCCC-ffff-0123456789ab")
    expect(ConstantsDemo::GEN_GUID).to eq("00112233-4455-6677-8899-aaBBccDDeeFF")
    expect(ConstantsDemo::MY_UUID).to eq("00000000-4444-CCCC-ffff-0123456789ab")
    expect(ConstantsDemo::MY_GUID).to eq("00112233-4455-6677-8899-aaBBccDDeeFF")
  end

  it 'should have correct list constants' do
    expect(ConstantsDemo::GEN_LIST).to be_a(Array)
    expect(ConstantsDemo::GEN_LIST).to eq([235235, 23598352, 3253523])
  end

  it 'should have correct map constants' do
    expect(ConstantsDemo::GEN_MAP).to be_a(Hash)
    expect(ConstantsDemo::GEN_MAP[35532]).to eq(233)
    expect(ConstantsDemo::GEN_MAP[43523]).to eq(853)

    expect(ConstantsDemo::GEN_MAP2).to be_a(Hash)
    expect(ConstantsDemo::GEN_MAP2["hello"]).to eq(233)
    expect(ConstantsDemo::GEN_MAP2["lkj98d"]).to eq(853)
    expect(ConstantsDemo::GEN_MAP2['lkjsdf']).to eq(98325)

    expect(ConstantsDemo::GEN_MAPMAP).to be_a(Hash)
    expect(ConstantsDemo::GEN_MAPMAP[235]).to be_a(Hash)
    expect(ConstantsDemo::GEN_MAPMAP[235][532]).to eq(53255)
    expect(ConstantsDemo::GEN_MAPMAP[235][235]).to eq(235)
  end

  it 'should have correct set constants' do
    expect(ConstantsDemo::GEN_SET).to be_a(Set)
    expect(ConstantsDemo::GEN_SET.size).to eq(2)
    expect(ConstantsDemo::GEN_SET.include?(235)).to be true  # added twice, but this is a set
    expect(ConstantsDemo::GEN_SET.include?(53235)).to be true

    expect(ConstantsDemo::GUID_SET).to be_a(Set)
    expect(ConstantsDemo::GUID_SET.size).to eq(2)
    expect(ConstantsDemo::GUID_SET.include?("00112233-4455-6677-8899-aaBBccDDeeFF")).to be true
    expect(ConstantsDemo::GUID_SET.include?("00000000-4444-CCCC-ffff-0123456789ab")).to be true
  end

  it 'should have correct struct constants' do
    expect(ConstantsDemo::GEN_THING).to be_a(Thrift::Struct)
    expect(ConstantsDemo::GEN_THING.hello).to eq(325)
    expect(ConstantsDemo::GEN_THING.goodbye).to eq(325352)
    expect(ConstantsDemo::GEN_THING.id).to eq("00112233-4455-6677-8899-aaBBccDDeeFF")
    expect(ConstantsDemo::GEN_THING.my_id).to eq("00000000-4444-CCCC-ffff-0123456789ab")
    expect(ConstantsDemo::GEN_THING.my_optional_id).to eq("00000000-4444-CCCC-ffff-0123456789ab")

    expect(ConstantsDemo::GEN_WHAT).to be_a(Hash)
    expect(ConstantsDemo::GEN_WHAT[35]).to be_a(Thrift::Struct)
    expect(ConstantsDemo::GEN_WHAT[35].hello).to eq(325)
    expect(ConstantsDemo::GEN_WHAT[35].goodbye).to eq(325352)
    expect(ConstantsDemo::GEN_WHAT[35].id).to eq("00000000-4444-CCCC-ffff-0123456789ab")
    expect(ConstantsDemo::GEN_WHAT[35].my_id).to eq("00000000-4444-CCCC-ffff-0123456789ab")
    expect(ConstantsDemo::GEN_WHAT[35].my_optional_id).to eq("00000000-4444-CCCC-ffff-0123456789ab")
  end
end
