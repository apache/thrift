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
using System.IO;
using SharpFuzz;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.Tests.Protocols.Fuzzers
{
    /// <summary>
    /// Base class for protocol fuzzers that handles the common fuzzing logic.
    /// </summary>
    /// <typeparam name="FuzzProtocol">The type of protocol to use for deserialization.</typeparam>
    public abstract class ProtocolFuzzerBase<FuzzProtocol> where FuzzProtocol : TProtocol
    {
        /// <summary>
        /// Environment variable that controls whether to use in-process fuzzing for AFL.
        /// When set to "1", uses Fuzzer.Run instead of Fuzzer.OutOfProcess.Run.
        /// </summary>
        protected const string UseInProcessFuzzingEnvVar = "THRIFT_AFL_IN_PROCESS";

        /// <summary>
        /// 10MB message size limit to prevent over-allocation during fuzzing
        /// </summary>
        protected const int FUZZ_MAX_MESSAGE_SIZE = 10 * 1024 * 1024;

        /// <summary>
        /// Creates a new instance of the protocol for the given transport.
        /// </summary>
        protected abstract FuzzProtocol CreateProtocol(TTransport transport);

        /// <summary>
        /// Helper method that contains the core fuzzing logic.
        /// </summary>
        private void ProcessFuzzStream(Stream stream)
        {
            try
            {
                var config = new TConfiguration();
                config.MaxMessageSize = FUZZ_MAX_MESSAGE_SIZE;
                var transport = new TStreamTransport(stream, null, config);
                var protocol = CreateProtocol(transport);

                var obj = new FuzzTest();
                obj.ReadAsync(protocol, default).GetAwaiter().GetResult();
            }
            catch (TException) { /* Expected for malformed input */ }
            catch (Exception) { /* Expected for malformed input */ }
        }

        /// <summary>
        /// The core fuzzing logic that processes a single input.
        /// </summary>
        protected void ProcessFuzzInput(ReadOnlySpan<byte> span)
        {
            using var stream = new MemoryStream(span.ToArray());
            ProcessFuzzStream(stream);
        }

        /// <summary>
        /// Runs the fuzzer with LibFuzzer.
        /// </summary>
        protected void RunLibFuzzer()
        {
            Fuzzer.LibFuzzer.Run(ProcessFuzzInput);
        }

        /// <summary>
        /// Runs the fuzzer with AFL.
        /// </summary>
        protected void RunAFL()
        {
            var useInProcess = Environment.GetEnvironmentVariable(UseInProcessFuzzingEnvVar) == "1";
            if (useInProcess)
            {
                Fuzzer.Run(ProcessFuzzStream);
            }
            else
            {
                Fuzzer.OutOfProcess.Run(ProcessFuzzStream);
            }
        }
    }
} 