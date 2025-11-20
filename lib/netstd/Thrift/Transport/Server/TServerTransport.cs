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

using System.Threading;
using System.Threading.Tasks;

#pragma warning disable IDE0079 // net20 - unneeded suppression
#pragma warning disable IDE0290 // net8 - primary CTOR

namespace Thrift.Transport
{
    // ReSharper disable once InconsistentNaming
    public abstract class TServerTransport
    {
        public readonly TConfiguration Configuration;

        public TServerTransport(TConfiguration config)
        {
            Configuration = config ?? new TConfiguration();
        }

        public abstract bool IsOpen();

        public abstract void Listen();
        public abstract void Close();
        public abstract bool IsClientPending();

        protected abstract ValueTask<TTransport> AcceptImplementationAsync(CancellationToken cancellationToken = default);

        public async ValueTask<TTransport> AcceptAsync(CancellationToken cancellationToken = default)
        {
            return await AcceptImplementationAsync(cancellationToken)
                ?? throw new TTransportException($"{nameof(AcceptImplementationAsync)} should not return null");
        }
    }
}
