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
    /// Base class for layers that preserve call isolation: every call view wraps a distinct inner call view.
    /// </summary>
    /// <remarks>
    /// The constructor only accepts an <see cref="ICallIsolatedTransport"/>, so composing a layer over a transport
    /// without per-call isolation is rejected at compile time. Disposing the layer disposes the shared inner transport.
    /// </remarks>
    public abstract class TIsolatedLayeredTransport : IAsyncCallIsolatedTransport, IPerCallTimeoutTransport
    {
        protected readonly ICallIsolatedTransport InnerTransport;
        private bool _isDisposed;

        protected TIsolatedLayeredTransport(ICallIsolatedTransport transport)
        {
            InnerTransport = transport ?? throw new ArgumentNullException(nameof(transport));
        }

        /// <summary>Wraps a freshly created inner call view in this layer's per-call transport.</summary>
        protected abstract TTransport CreateLayer(TTransport callTransport);

        public TTransport CreateCallTransport()
        {
            if (_isDisposed)
            {
                throw new ObjectDisposedException(GetType().Name);
            }

            var inner = RequireView(InnerTransport.CreateCallTransport());
            try
            {
                return CreateLayer(inner);
            }
            catch
            {
                inner.Dispose();
                throw;
            }
        }

        public async Task<TTransport> CreateCallTransportAsync(CancellationToken cancellationToken)
        {
            if (_isDisposed)
            {
                throw new ObjectDisposedException(GetType().Name);
            }

            cancellationToken.ThrowIfCancellationRequested();
            var inner = RequireView(InnerTransport is IAsyncCallIsolatedTransport asyncInner
                ? await asyncInner.CreateCallTransportAsync(cancellationToken)
                : InnerTransport.CreateCallTransport());
            try
            {
                return CreateLayer(inner);
            }
            catch
            {
                inner.Dispose();
                throw;
            }
        }

        // an inner transport that returns no view is a bug of that transport; report it as such, and do not try to
        // clean up a view that does not exist, which would hide the real cause
        private static TTransport RequireView(TTransport view)
        {
            return view ?? throw new InvalidOperationException("The inner call-isolated transport returned no transport view.");
        }

        /// <summary>The timeout of the inner transport; layers add none of their own.</summary>
        public TimeSpan PerCallTimeout => (InnerTransport as IPerCallTimeoutTransport)?.PerCallTimeout ?? Timeout.InfiniteTimeSpan;

        public void Dispose()
        {
            if (!_isDisposed)
            {
                _isDisposed = true;
                InnerTransport.Dispose();
            }
        }
    }

    // ReSharper disable once InconsistentNaming
    public class TIsolatedBufferedTransport : TIsolatedLayeredTransport
    {
        private readonly int _bufSize;

        public TIsolatedBufferedTransport(ICallIsolatedTransport transport, int bufSize = 1024)
            : base(transport)
        {
            if (bufSize <= 0)
            {
                throw new ArgumentOutOfRangeException(nameof(bufSize), "Buffer size must be a positive number.");
            }
            _bufSize = bufSize;
        }

        protected override TTransport CreateLayer(TTransport callTransport) => new TBufferedTransport(callTransport, _bufSize);
    }

    // ReSharper disable once InconsistentNaming
    public class TIsolatedFramedTransport : TIsolatedLayeredTransport
    {
        public TIsolatedFramedTransport(ICallIsolatedTransport transport)
            : base(transport)
        {
        }

        protected override TTransport CreateLayer(TTransport callTransport) => new TFramedTransport(callTransport);
    }
}
