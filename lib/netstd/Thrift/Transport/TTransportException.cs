// Licensed to the Apache Software Foundation(ASF) under one
// or more contributor license agreements.See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

using System;

namespace Thrift.Transport
{
    // ReSharper disable once InconsistentNaming
    public class TTransportException : TException
    {
        public enum ExceptionType
        {
            Unknown,
            NotOpen,
            AlreadyOpen,
            TimedOut,
            EndOfFile,
            Interrupted
        }

        public ExceptionType ExType { get; private set; }

        public TTransportException()
        {
        }

        public TTransportException(ExceptionType exType, Exception inner = null)
            : base(string.Empty, inner)
        {
            ExType = exType;
        }

        public TTransportException(ExceptionType exType, string message, Exception inner = null)
            : base(message, inner)
        {
            ExType = exType;
        }

        public TTransportException(string message, Exception inner = null)
            : base(message, inner)
        {
        }

        public ExceptionType Type => ExType;
    }
}