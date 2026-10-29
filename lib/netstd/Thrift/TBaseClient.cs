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
using Thrift.Protocol;
using Thrift.Transport;

namespace Thrift
{
    // ReSharper disable once InconsistentNaming
    /// <summary>
    ///     TBaseClient.
    ///     Base client for generated clients.
    ///     Do not change this class without checking generated code (namings, etc.)
    /// </summary>
    public abstract class TBaseClient
    {
        private readonly TProtocol _inputProtocol;
        private readonly TProtocol _outputProtocol;
        private readonly ICallIsolatedTransport _callTransport;
        private readonly TProtocolFactory _protocolFactory;
        private bool _isDisposed;
        private int _seqId;
        public readonly Guid ClientId = Guid.NewGuid();

        protected TBaseClient(TProtocol inputProtocol, TProtocol outputProtocol)
        {
            _inputProtocol = inputProtocol ?? throw new ArgumentNullException(nameof(inputProtocol));
            _outputProtocol = outputProtocol ?? throw new ArgumentNullException(nameof(outputProtocol));
        }

        /// <summary>
        /// Creates an isolated client: every call gets its own transport view and protocol through
        /// <see cref="CreateCallContext(CancellationToken)"/>. The client owns <paramref name="transport"/>
        /// and must outlive its calls.
        /// </summary>
        protected TBaseClient(ICallIsolatedTransport transport, TProtocolFactory protocolFactory)
        {
            _callTransport = transport ?? throw new ArgumentNullException(nameof(transport));
            _protocolFactory = protocolFactory ?? throw new ArgumentNullException(nameof(protocolFactory));
        }

        public TProtocol InputProtocol => _inputProtocol ?? throw NotSupportedInIsolatedMode();

        public TProtocol OutputProtocol => _outputProtocol ?? throw NotSupportedInIsolatedMode();

        public int SeqId
        {
            get { return Interlocked.Increment(ref _seqId); }
        }

        public virtual async Task OpenTransportAsync()
        {
            await OpenTransportAsync(CancellationToken.None);
        }

        public virtual async Task OpenTransportAsync(CancellationToken cancellationToken)
        {
            if (_callTransport != null)
            {
                cancellationToken.ThrowIfCancellationRequested();
                return; // call views are created on demand and need no shared open
            }

            if (!_inputProtocol.Transport.IsOpen)
            {
                await _inputProtocol.Transport.OpenAsync(cancellationToken);
            }

            if (!_outputProtocol.Transport.IsOpen)
            {
                await _outputProtocol.Transport.OpenAsync(cancellationToken);
            }
        }

        /// <summary>True when this client was created with a call-isolated transport and uses a context per call.</summary>
        public bool IsCallIsolated => _callTransport != null;

        /// <summary>
        /// Starts a new call: creates a transport view and a protocol for it and assigns a unique sequence id.
        /// The caller owns the returned context and must dispose it when the call has completed.
        /// </summary>
        /// <param name="cancellationToken">Cancels the call; combined with the timeouts into the context's token.</param>
        public TCallContext CreateCallContext(CancellationToken cancellationToken = default)
        {
            return CreateCallContextCore(null, cancellationToken);
        }

        /// <summary>
        /// Starts a new call with a timeout of its own. The timeout can only tighten the timeout declared by the
        /// transport, never loosen it; the shortest bound wins.
        /// </summary>
        public TCallContext CreateCallContext(TimeSpan timeout, CancellationToken cancellationToken = default)
        {
            return CreateCallContextCore(timeout, cancellationToken);
        }

        /// <summary>
        /// Starts a new call like <see cref="CreateCallContext(CancellationToken)"/>, creating the transport view
        /// asynchronously when the transport supports it (<see cref="IAsyncCallIsolatedTransport"/>) and
        /// synchronously otherwise.
        /// </summary>
        public Task<TCallContext> CreateCallContextAsync(CancellationToken cancellationToken = default)
        {
            return CreateCallContextCoreAsync(null, cancellationToken);
        }

        /// <summary>Starts a new call with a timeout of its own, see <see cref="CreateCallContext(TimeSpan, CancellationToken)"/>.</summary>
        public Task<TCallContext> CreateCallContextAsync(TimeSpan timeout, CancellationToken cancellationToken = default)
        {
            return CreateCallContextCoreAsync(timeout, cancellationToken);
        }

        private TCallContext CreateCallContextCore(TimeSpan? timeout, CancellationToken cancellationToken)
        {
            var callTransport = CheckCanStartCall();
            var view = callTransport.CreateCallTransport();
            return CreateContext(view, timeout, cancellationToken);
        }

        private async Task<TCallContext> CreateCallContextCoreAsync(TimeSpan? timeout, CancellationToken cancellationToken)
        {
            var callTransport = CheckCanStartCall();
            cancellationToken.ThrowIfCancellationRequested();

            var view = callTransport is IAsyncCallIsolatedTransport asyncTransport
                ? await asyncTransport.CreateCallTransportAsync(cancellationToken)
                : callTransport.CreateCallTransport();
            return CreateContext(view, timeout, cancellationToken);
        }

        private ICallIsolatedTransport CheckCanStartCall()
        {
            if (_callTransport == null)
            {
                throw NotSupportedInLegacyMode();
            }

            if (_isDisposed)
            {
                throw new ObjectDisposedException(GetType().Name);
            }

            return _callTransport;
        }

        private TCallContext CreateContext(TTransport view, TimeSpan? timeout, CancellationToken cancellationToken)
        {
            if (view == null)
            {
                throw new InvalidOperationException("The call-isolated transport returned no transport view.");
            }

            try
            {
                var protocol = _protocolFactory.GetProtocol(view);
                return new TCallContext(protocol, SeqId, cancellationToken, EffectiveTimeout(timeout));
            }
            catch
            {
                view.Dispose();
                throw;
            }
        }

        // the shortest of the transport's declared timeout and the call's own timeout wins
        private TimeSpan? EffectiveTimeout(TimeSpan? callTimeout)
        {
            var transportTimeout = (_callTransport as IPerCallTimeoutTransport)?.PerCallTimeout ?? Timeout.InfiniteTimeSpan;
            if (callTimeout == null || callTimeout.Value == Timeout.InfiniteTimeSpan)
            {
                return transportTimeout;
            }

            if (transportTimeout == Timeout.InfiniteTimeSpan)
            {
                return callTimeout;
            }

            return callTimeout.Value < transportTimeout ? callTimeout : transportTimeout;
        }

        private static InvalidOperationException NotSupportedInIsolatedMode() =>
            new InvalidOperationException("This client uses per-call contexts; use CreateCallContext().");

        private static InvalidOperationException NotSupportedInLegacyMode() =>
            new InvalidOperationException("This client was not created with a call-isolated transport.");

        public void Dispose()
        {
            Dispose(true);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (!_isDisposed)
            {
                if (disposing)
                {
                    _inputProtocol?.Dispose();
                    _outputProtocol?.Dispose();
                    _callTransport?.Dispose();
                }
            }

            _isDisposed = true;
        }
    }
}
