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

use std::io;

use super::{TTransport, TTransportState};

pub struct TTcpIpSocket {

}

impl TTcpIpSocket {

}

impl io::Read for TTcpIpSocket {
    fn read(&mut self, _: &mut [u8]) -> io::Result<usize> {
        unimplemented!()
    }
}

impl io::Write for TTcpIpSocket {
    fn write(&mut self, _: &[u8]) -> io::Result<usize> {
        unimplemented!()
    }

    fn flush(&mut self) -> io::Result<()> {
        unimplemented!()
    }
}

impl TTransport for TTcpIpSocket {
    fn open(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn close(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn state(&self) -> TTransportState {
        unimplemented!()
    }
}
