// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

#![no_main]

use thrift::protocol::{
    TBinaryInputProtocol, TBinaryOutputProtocol, TOutputProtocol, TSerializable,
};
use thrift::transport::TBufferChannel;
use thrift_fuzz::fuzz_test::FuzzTest;

const BUFFER_CAPACITY: usize = 65536;

fn run(input: FuzzTest) -> thrift::Result<()> {
    // TODO: Figure out a way to do this without hardcoding the buffer size
    // Serialize
    let mut mem = TBufferChannel::with_capacity(BUFFER_CAPACITY, BUFFER_CAPACITY);
    let mut out_protocol = TBinaryOutputProtocol::new(&mut mem, true);
    input.write_to_out_protocol(&mut out_protocol)?;
    out_protocol.flush()?;

    // Get the serialized bytes
    let serialized = mem.write_bytes();

    // Deserialize
    let mut mem = TBufferChannel::with_capacity(serialized.len(), serialized.len());
    mem.set_readable_bytes(&serialized);
    let mut in_protocol = TBinaryInputProtocol::new(mem, true);
    let obj = FuzzTest::read_from_in_protocol(&mut in_protocol)?;

    assert_eq!(input, obj);

    Ok(())
}

use libfuzzer_sys::fuzz_target;

fuzz_target!(|input: FuzzTest| {
    let _ = run(input);
});
