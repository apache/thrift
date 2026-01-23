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
    /// Base class for protocol round-trip fuzzers that handles the common fuzzing logic.
    /// </summary>
    /// <typeparam name="FuzzProtocol">The type of protocol to use for serialization/deserialization.</typeparam>
    public abstract class ProtocolRoundtripFuzzerBase<FuzzProtocol> where FuzzProtocol : TProtocol
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
                // First deserialize the input
                var config = new TConfiguration();
                config.MaxMessageSize = FUZZ_MAX_MESSAGE_SIZE;
                var inputTransport = new TStreamTransport(stream, null, config);
                var inputProtocol = CreateProtocol(inputTransport);

                var inputObj = new FuzzTest();
                inputObj.ReadAsync(inputProtocol, default).GetAwaiter().GetResult();

                // Now serialize it back
                using var outputStream = new MemoryStream();
                var outputTransport = new TStreamTransport(null, outputStream, config);
                var outputProtocol = CreateProtocol(outputTransport);
                inputObj.WriteAsync(outputProtocol, default).GetAwaiter().GetResult();
                outputTransport.FlushAsync(default).GetAwaiter().GetResult();

                // Get the serialized bytes and deserialize again
                var serialized = outputStream.ToArray();
                using var reStream = new MemoryStream(serialized);
                var reTransport = new TStreamTransport(reStream, null, config);
                var reProtocol = CreateProtocol(reTransport);

                var outputObj = new FuzzTest();
                outputObj.ReadAsync(reProtocol, default).GetAwaiter().GetResult();

                // Compare the objects
                if (!inputObj.Equals(outputObj))
                {
                    throw new Exception("Round-trip objects are not equal");
                }
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