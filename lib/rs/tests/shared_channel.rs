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

use std::collections::VecDeque;
use std::io::{self, Read, Write};
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::sync::{Arc, Mutex};

use thrift::transport::{TIoChannel, TSharedChannel};

struct TestIo {
    readable: VecDeque<u8>,
    written: Arc<Mutex<Vec<u8>>>,
}

impl Read for TestIo {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
        let count = buffer.len().min(self.readable.len());
        for target in &mut buffer[..count] {
            *target = self.readable.pop_front().unwrap();
        }
        Ok(count)
    }
}

impl Write for TestIo {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
        self.written.lock().unwrap().extend_from_slice(buffer);
        Ok(buffer.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[test]
fn split_supports_a_non_cloneable_inner_channel() {
    let written = Arc::new(Mutex::new(Vec::new()));
    let channel = TSharedChannel::new(TestIo {
        readable: VecDeque::from(b"read".to_vec()),
        written: Arc::clone(&written),
    });

    let (mut reader, mut writer) = channel.split().unwrap();
    let mut input = [0; 4];
    reader.read_exact(&mut input).unwrap();
    writer.write_all(b"write").unwrap();

    assert_eq!(&input, b"read");
    assert_eq!(&*written.lock().unwrap(), b"write");
}

struct PanickingIo;

impl Read for PanickingIo {
    fn read(&mut self, _buffer: &mut [u8]) -> io::Result<usize> {
        panic!("poison the shared channel lock")
    }
}

impl Write for PanickingIo {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
        Ok(buffer.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[test]
fn a_poisoned_channel_returns_an_io_error_instead_of_panicking_again() {
    let mut channel = TSharedChannel::new(PanickingIo);
    let mut reader = channel.clone();
    let panic = catch_unwind(AssertUnwindSafe(|| {
        let _ = reader.read(&mut [0]);
    }));
    assert!(panic.is_err());

    let error = channel.write(b"still poisoned").unwrap_err();
    assert_eq!(error.kind(), io::ErrorKind::Other);
    assert_eq!(error.to_string(), "shared channel lock is poisoned");
}
