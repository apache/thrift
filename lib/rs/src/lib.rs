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

extern crate byteorder;
extern crate try_from;

use std::{convert, error, fmt, io, string};

pub mod protocol;
pub mod transport;

pub type Result<T> = std::result::Result<T, Error>;

pub enum Error {
    PlaceHolder,
}

impl error::Error for Error {
    fn description(&self) -> &str {
        unimplemented!()
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

impl fmt::Display for Error {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

impl convert::From<io::Error> for Error {
    fn from(_: io::Error) -> Self {
        unimplemented!()
    }
}

impl convert::From<string::FromUtf8Error> for Error {
    fn from(_: string::FromUtf8Error) -> Self {
        unimplemented!()
    }
}

