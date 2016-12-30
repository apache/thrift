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

use std::collections::HashMap;
use std::convert::Into;

use ::protocol::{TInputProtocol, TOutputProtocol};

use super::TProcessor;

pub struct TMultiplexedProcessor {
    processors: HashMap<String, Box<TProcessor>>
}

impl TMultiplexedProcessor {
    pub fn register_processor<S: Into<String>>(&mut self, service_name: S, processor: Box<TProcessor>) -> bool {
        let name = service_name.into();
        if self.processors.contains_key(&name) {
            false
        } else {
            self.processors.insert(name, processor);
            true
        }
    }
}

impl TProcessor for TMultiplexedProcessor {
    fn process(&mut self, _: &mut TInputProtocol, _: &mut TOutputProtocol) -> ::Result<()> {
        unimplemented!()
    }
}
