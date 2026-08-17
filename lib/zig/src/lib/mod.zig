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

pub const result = @import("result.zig");
pub const application_exception = @import("application_exception.zig");
pub const types = @import("types.zig");
pub const configuration = @import("configuration.zig");

pub const ServiceCallResult = result.ServiceCallResult;
pub const TApplicationException = application_exception.TApplicationException;
pub const TConfiguration = configuration.TConfiguration;
pub const DEFAULT_MAX_MESSAGE_SIZE = configuration.DEFAULT_MAX_MESSAGE_SIZE;
pub const DEFAULT_MAX_FRAME_SIZE = configuration.DEFAULT_MAX_FRAME_SIZE;
pub const DEFAULT_RECURSION_DEPTH = configuration.DEFAULT_RECURSION_DEPTH;
