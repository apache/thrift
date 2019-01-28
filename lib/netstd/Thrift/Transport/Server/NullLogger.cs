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

using Microsoft.Extensions.Logging;
using System;


namespace Thrift.Transport.Server
{
    // sometimes we just don't want to log anything
    internal class NullLogger<T> : IDisposable, ILogger, ILogger<T>
    {
        internal class NullScope : IDisposable
        {
            public void Dispose()
            {
                // nothing to do
            }
        }

        public IDisposable BeginScope<TState>(TState state)
        {
            return new NullScope();
        }

        public void Dispose()
        {
            // nothing to do
        }

        public bool IsEnabled(LogLevel logLevel)
        {
            return false;  // no
        }

        public void Log<TState>(LogLevel logLevel, EventId eventId, TState state, Exception exception, Func<TState, Exception, string> formatter)
        {
            // do nothing
        }
    }
}

