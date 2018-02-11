/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
module thrift.base;

import std.experimental.logger;

/**
 * Common base class for all Thrift exceptions.
 */
class TException : Exception {
  ///
  this(string msg = "", string file = __FILE__, size_t line = __LINE__,
    Throwable next = null)
  {
    super(msg, file, line, next);
  }
}

/**
 * An operation failed because one or more sub-tasks failed.
 */
class TCompoundOperationException : TException {
  ///
  this(string msg, Exception[] exceptions, string file = __FILE__,
    size_t line = __LINE__, Throwable next = null)
  {
    super(msg, file, line, next);
    this.exceptions = exceptions;
  }

  /// The exceptions thrown by the children of the operation. If applicable,
  /// the list is ordered in the same way the exceptions occurred.
  Exception[] exceptions;
}

/// The Thrift version string, used for informative purposes.
// Note: This is currently hardcoded, but will likely be filled in by the build
// system in future versions.
enum VERSION = "1.0.0 dev";

/**
 * Functions used for logging inside Thrift.
 *
 * By default, the formatted messages are written to stdout/stderr, but this
 * behavior can be overwritten by providing custom g_{Info, Error}LogSink
 * handlers.
 *
 * Examples:
 * ---
 * logInfo("An informative message.");
 * logError("Some error occurred: %s", e);
 * ---
 */
alias logInfo = infof;
alias logError = errorf;
