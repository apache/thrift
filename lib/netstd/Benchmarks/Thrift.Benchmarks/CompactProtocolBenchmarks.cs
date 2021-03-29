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

using System.IO;
using System.Threading.Tasks;

using BenchmarkDotNet.Attributes;

using Thrift.Protocol;
using Thrift.Transport.Client;

namespace Thrift.Benchmarks
{
    [MemoryDiagnoser]
    public class CompactProtocolBenchmarks
    {
        private MemoryStream _Stream;
        private TProtocol _Protocol;

        [Params(10000)]
        public int NumberOfOperationsPerIteration { get; set; }

        [GlobalSetup]
        public void GlobalSetup()
        {
            _Stream = new MemoryStream();
            var transport = new TStreamTransport(_Stream, _Stream, null);
            _Protocol = new TCompactProtocol(transport);
        }

        [GlobalCleanup]
        public void GlobalCleanup()
        {
            _Protocol.Dispose();
        }

        [Benchmark]
        public async Task WriteString()
        {
            for (int i = 0; i < NumberOfOperationsPerIteration; i++)
            {
                await _Protocol.WriteStringAsync("Thrift String Benchmark");

                _Stream.Seek(0, SeekOrigin.Begin);
            }
        }

        [Benchmark]
        public async Task ReadString()
        {
            await _Protocol.WriteStringAsync("Thrift String Benchmark");

            for (int i = 0; i < NumberOfOperationsPerIteration; i++)
            {
                _Stream.Seek(0, SeekOrigin.Begin);

                await _Protocol.ReadStringAsync();
            }
        }
    }
}
