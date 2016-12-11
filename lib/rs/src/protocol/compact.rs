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

use std::cell::RefCell;
use std::rc::Rc;

use ::transport::TTransport;
use super::{TFieldIdentifier, TListIdentifier, TMapIdentifier, TMessageIdentifier, TProtocol, TProtocolFactory, TSetIdentifier, TStructIdentifier};

pub struct TCompactProtocol {
    /// Underlying transport used for byte-level operations.
    pub transport: Rc<RefCell<Box<TTransport>>>,
}

impl TProtocol for TCompactProtocol {
    fn write_message_begin(&mut self, _: &TMessageIdentifier) -> ::Result<()> {
        unimplemented!()
    }

    fn write_message_end(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn write_struct_begin(&mut self, _: &TStructIdentifier) -> ::Result<()> {
        unimplemented!()
    }

    fn write_struct_end(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn write_field_begin(&mut self, _: &TFieldIdentifier) -> ::Result<()> {
        unimplemented!()
    }

    fn write_field_end(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn write_field_stop(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn write_bool(&mut self, _: bool) -> ::Result<()> {
        unimplemented!()
    }

    fn write_bytes(&mut self, _: &[u8]) -> ::Result<()> {
        unimplemented!()
    }

    fn write_i8(&mut self, _: i8) -> ::Result<()> {
        unimplemented!()
    }

    fn write_i16(&mut self, _: i16) -> ::Result<()> {
        unimplemented!()
    }

    fn write_i32(&mut self, _: i32) -> ::Result<()> {
        unimplemented!()
    }

    fn write_i64(&mut self, _: i64) -> ::Result<()> {
        unimplemented!()
    }

    fn write_double(&mut self, _: f64) -> ::Result<()> {
        unimplemented!()
    }

    fn write_string(&mut self, _: &str) -> ::Result<()> {
        unimplemented!()
    }

    fn write_list_begin(&mut self, _: &TListIdentifier) -> ::Result<()> {
        unimplemented!()
    }

    fn write_list_end(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn write_set_begin(&mut self, _: &TSetIdentifier) -> ::Result<()> {
        unimplemented!()
    }

    fn write_set_end(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn write_map_begin(&mut self, _: &TMapIdentifier) -> ::Result<()> {
        unimplemented!()
    }

    fn write_map_end(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn flush(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn read_message_begin(&mut self) -> ::Result<TMessageIdentifier> {
        unimplemented!()
    }

    fn read_message_end(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn read_struct_begin(&mut self) -> ::Result<Option<TStructIdentifier>> {
        unimplemented!()
    }

    fn read_struct_end(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn read_field_begin(&mut self) -> ::Result<TFieldIdentifier> {
        unimplemented!()
    }

    fn read_field_end(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn read_bool(&mut self) -> ::Result<bool> {
        unimplemented!()
    }

    fn read_bytes(&mut self) -> ::Result<Vec<u8>> {
        unimplemented!()
    }

    fn read_i8(&mut self) -> ::Result<i8> {
        unimplemented!()
    }

    fn read_i16(&mut self) -> ::Result<i16> {
        unimplemented!()
    }

    fn read_i32(&mut self) -> ::Result<i32> {
        unimplemented!()
    }

    fn read_i64(&mut self) -> ::Result<i64> {
        unimplemented!()
    }

    fn read_double(&mut self) -> ::Result<f64> {
        unimplemented!()
    }

    fn read_string(&mut self) -> ::Result<String> {
        unimplemented!()
    }

    fn read_list_begin(&mut self) -> ::Result<TListIdentifier> {
        unimplemented!()
    }

    fn read_list_end(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn read_set_begin(&mut self) -> ::Result<TSetIdentifier> {
        unimplemented!()
    }

    fn read_set_end(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn read_map_begin(&mut self) -> ::Result<TMapIdentifier> {
        unimplemented!()
    }

    fn read_map_end(&mut self) -> ::Result<()> {
        unimplemented!()
    }

    fn write_byte(&mut self, _: u8) -> ::Result<()> {
        unimplemented!()
    }

    fn read_byte(&mut self) -> ::Result<u8> {
        unimplemented!()
    }
}

/// Convenience object that can be used to create an instance of `TCompactProtocol`.
pub struct TCompactProtocolFactory;
impl TProtocolFactory for TCompactProtocolFactory {
    fn build(&self, transport: Rc<RefCell<Box<TTransport>>>) -> Box<TProtocol> {
        Box::new(TCompactProtocol { transport: transport }) as Box<TProtocol>
    }
}

#[cfg(test)]
mod tests {

    // use super::*;
    // use ::protocol::TProtocol;
}
