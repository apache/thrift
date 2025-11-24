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
use thrift::TConfiguration;
use thrift_fuzz::fuzz_test::FuzzTest;

fn run(data: &[u8]) -> thrift::Result<()> {
    // Add some limits to optimize fuzzing
    let config = TConfiguration::builder()
        .max_message_size(Some(data.len()))
        .max_frame_size(Some(data.len()))
        .max_container_size(Some(1024))
        .max_string_size(Some(data.len()))
        .build()
        .unwrap();
    // First try to deserialize the raw input bytes
    let mut mem = TBufferChannel::with_capacity(data.len(), data.len());
    mem.set_readable_bytes(data);
    let mut protocol =
        TBinaryInputProtocol::with_config(mem, true /* strict */, config.clone());
    let input = FuzzTest::read_from_in_protocol(&mut protocol)?;

    // Now do the roundtrip test with the successfully deserialized object
    let mut mem = TBufferChannel::with_capacity(data.len(), data.len());
    let mut out_protocol = TBinaryOutputProtocol::new(&mut mem, true);
    input.write_to_out_protocol(&mut out_protocol)?;
    out_protocol.flush()?;

    // Get the serialized bytes
    let serialized = mem.write_bytes();

    // Deserialize again
    let mut mem = TBufferChannel::with_capacity(serialized.len(), serialized.len());
    mem.set_readable_bytes(&serialized);
    let mut in_protocol = TBinaryInputProtocol::with_config(mem, true, config);
    let obj = FuzzTest::read_from_in_protocol(&mut in_protocol)?;

    assert_eq!(input, obj);

    Ok(())
}

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let _ = run(data);
});
