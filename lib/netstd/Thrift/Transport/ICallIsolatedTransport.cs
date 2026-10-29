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
using System.Threading;
using System.Threading.Tasks;

namespace Thrift.Transport
{
    /// <summary>
    /// A shared transport that can hand out an independent transport view for each logical call.
    /// </summary>
    /// <remarks>
    /// Each view owns its request and response state. The caller of <see cref="CreateCallTransport"/> is
    /// responsible for disposing the returned view; disposing a view never releases the shared transport or any
    /// sibling view. The shared transport must outlive the calls made through it. Stream-based transports share
    /// ordered input and therefore do not implement this interface.
    /// </remarks>
    public interface ICallIsolatedTransport : IDisposable
    {
        /// <summary>
        /// Creates a transport view for exactly one call. The caller owns and must dispose the view.
        /// </summary>
        /// <exception cref="ObjectDisposedException">The shared transport has been disposed.</exception>
        TTransport CreateCallTransport();
    }

    /// <summary>
    /// A call-isolated transport whose call views need asynchronous setup (for example acquiring a connection).
    /// Transports without such setup implement only <see cref="ICallIsolatedTransport"/>.
    /// </summary>
    public interface IAsyncCallIsolatedTransport : ICallIsolatedTransport
    {
        /// <summary>
        /// Creates a transport view for exactly one call. The caller owns and must dispose the view. When the token
        /// is cancelled before creation completes, no view is left undisposed.
        /// </summary>
        Task<TTransport> CreateCallTransportAsync(CancellationToken cancellationToken);
    }

    /// <summary>
    /// A call-isolated transport that declares the maximum duration of one call. Transports that do not implement
    /// this interface are unbounded.
    /// </summary>
    public interface IPerCallTimeoutTransport
    {
        /// <summary>The maximum duration of one call, or <see cref="Timeout.InfiniteTimeSpan"/> when unbounded.</summary>
        TimeSpan PerCallTimeout { get; }
    }
}
