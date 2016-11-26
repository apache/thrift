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

//! Rust Thrift implementation layer.
//!
//! This crate provides the layers on which you can build a Thrift implementation.

#![crate_type = "lib"]

extern crate byteorder;
extern crate try_from;

pub mod protocol;
pub mod server;
pub mod transport;

mod errors;
pub use errors::*;

/// Result type returned by all rift functions.
/// As is convention, this is a typedef of `std::result::Result`
/// with `E` defined as the rift `Error` type.
pub type Result<T> = std::result::Result<T, self::Error>;
