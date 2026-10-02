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
        private readonly ITPerCallTransportProvider _perCallTransportProvider;
        private readonly TProtocolFactory _inputProtocolFactory;
        private readonly TProtocolFactory _outputProtocolFactory;
        private readonly AsyncLocal<PerCallProtocols> _perCallProtocols = new AsyncLocal<PerCallProtocols>();
        private bool _isDisposed;
        private int _seqId;
        public readonly Guid ClientId = Guid.NewGuid();

        protected TBaseClient(TProtocol inputProtocol, TProtocol outputProtocol)
        {
            _inputProtocol = inputProtocol ?? throw new ArgumentNullException(nameof(inputProtocol));
            _outputProtocol = outputProtocol ?? throw new ArgumentNullException(nameof(outputProtocol));
        }

        /// <summary>
        /// Initializes a client with protocols created from the supplied transport and protocol factories.
        /// </summary>
        /// <param name="transport">
        /// The shared transport. When it implements <see cref="ITPerCallTransportProvider"/>,
        /// each client call uses protocols created for a new per-call transport.
        /// </param>
        /// <param name="inputProtocolFactory">The factory used to create input protocols.</param>
        /// <param name="outputProtocolFactory">The factory used to create output protocols.</param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="transport"/>, <paramref name="inputProtocolFactory"/>,
        /// or <paramref name="outputProtocolFactory"/> is <see langword="null"/>.
        /// </exception>
        /// <exception cref="InvalidOperationException">
        /// Thrown when a protocol factory returns <see langword="null"/>.
        /// </exception>
        protected TBaseClient(
            TTransport transport,
            TProtocolFactory inputProtocolFactory,
            TProtocolFactory outputProtocolFactory)
        {
            if (transport == null) throw new ArgumentNullException(nameof(transport));
            if (inputProtocolFactory == null) throw new ArgumentNullException(nameof(inputProtocolFactory));
            if (outputProtocolFactory == null) throw new ArgumentNullException(nameof(outputProtocolFactory));

            var protocols = CreateProtocols(transport, inputProtocolFactory, outputProtocolFactory);
            _inputProtocol = protocols.InputProtocol;
            _outputProtocol = protocols.OutputProtocol;
            _perCallTransportProvider = transport as ITPerCallTransportProvider;
            if (_perCallTransportProvider != null)
            {
                _inputProtocolFactory = inputProtocolFactory;
                _outputProtocolFactory = outputProtocolFactory;
            }
        }

        private static ProtocolPair CreateProtocols(
            TTransport transport,
            TProtocolFactory inputProtocolFactory,
            TProtocolFactory outputProtocolFactory)
        {
            TProtocol inputProtocol = null;
            TProtocol outputProtocol = null;
            try
            {
                inputProtocol = inputProtocolFactory.GetProtocol(transport)
                    ?? throw new InvalidOperationException("The input protocol factory returned null.");
                outputProtocol = outputProtocolFactory.GetProtocol(transport)
                    ?? throw new InvalidOperationException("The output protocol factory returned null.");
                return new ProtocolPair(inputProtocol, outputProtocol);
            }
            catch
            {
                outputProtocol?.Dispose();
                if (inputProtocol != null && !ReferenceEquals(inputProtocol, outputProtocol))
                {
                    inputProtocol.Dispose();
                }
                if (inputProtocol == null && outputProtocol == null)
                {
                    transport.Dispose();
                }
                throw;
            }
        }

        private sealed class ProtocolPair
        {
            public ProtocolPair(TProtocol inputProtocol, TProtocol outputProtocol)
            {
                InputProtocol = inputProtocol;
                OutputProtocol = outputProtocol;
            }

            public TProtocol InputProtocol { get; }
            public TProtocol OutputProtocol { get; }
        }

        /// <summary>
        /// Gets the input protocol for the current call, or the shared input protocol when no
        /// per-call transport is active.
        /// </summary>
        public TProtocol InputProtocol => _perCallProtocols.Value?.InputProtocol ?? _inputProtocol;

        /// <summary>
        /// Gets the output protocol for the current call, or the shared output protocol when no
        /// per-call transport is active.
        /// </summary>
        public TProtocol OutputProtocol => _perCallProtocols.Value?.OutputProtocol ?? _outputProtocol;

        /// <summary>
        /// Gets the next client sequence identifier.
        /// </summary>
        public int SeqId
        {
            get { return Interlocked.Increment(ref _seqId); }
        }

        /// <summary>
        /// Executes an operation using protocols scoped to a new per-call transport when the
        /// shared transport supports per-call transports.
        /// </summary>
        /// <param name="operation">The operation to execute.</param>
        /// <param name="cancellationToken">The token used to cancel per-call transport creation.</param>
        /// <returns>A task that represents the asynchronous operation.</returns>
        protected async Task ExecutePerCallAsync(Func<Task> operation, CancellationToken cancellationToken)
        {
            await ExecutePerCallCoreAsync(async () =>
            {
                await operation();
                return true;
            }, cancellationToken);
        }

        /// <summary>
        /// Executes an operation using protocols scoped to a new per-call transport when the
        /// shared transport supports per-call transports.
        /// </summary>
        /// <typeparam name="TResult">The type of result returned by the operation.</typeparam>
        /// <param name="operation">The operation to execute.</param>
        /// <param name="cancellationToken">The token used to cancel per-call transport creation.</param>
        /// <returns>A task that contains the operation result.</returns>
        protected Task<TResult> ExecutePerCallAsync<TResult>(
            Func<Task<TResult>> operation,
            CancellationToken cancellationToken)
        {
            return ExecutePerCallCoreAsync(operation, cancellationToken);
        }

        private async Task<TResult> ExecutePerCallCoreAsync<TResult>(
            Func<Task<TResult>> operation,
            CancellationToken cancellationToken)
        {
            if (_perCallTransportProvider == null)
                return await operation();

            var transport = await _perCallTransportProvider.CreatePerCallTransportAsync(cancellationToken)
                ?? throw new InvalidOperationException("The per-call transport provider returned null.");
            TProtocol inputProtocol = null;
            TProtocol outputProtocol = null;
            try
            {
                inputProtocol = _inputProtocolFactory.GetProtocol(transport)
                    ?? throw new InvalidOperationException("The input protocol factory returned null.");
                outputProtocol = _outputProtocolFactory.GetProtocol(transport)
                    ?? throw new InvalidOperationException("The output protocol factory returned null.");
            }
            catch
            {
                if (outputProtocol != null)
                {
                    outputProtocol.Dispose();
                }
                if (inputProtocol != null && !ReferenceEquals(inputProtocol, outputProtocol))
                {
                    inputProtocol.Dispose();
                }
                if (inputProtocol == null && outputProtocol == null)
                {
                    transport.Dispose();
                }
                throw;
            }

            var protocols = new PerCallProtocols(inputProtocol, outputProtocol);
            var previousProtocols = _perCallProtocols.Value;
            _perCallProtocols.Value = protocols;
            try
            {
                return await operation();
            }
            finally
            {
                _perCallProtocols.Value = previousProtocols;
                protocols.Dispose();
            }
        }

        private sealed class PerCallProtocols : IDisposable
        {
            public PerCallProtocols(TProtocol inputProtocol, TProtocol outputProtocol)
            {
                InputProtocol = inputProtocol;
                OutputProtocol = outputProtocol;
            }

            public TProtocol InputProtocol { get; }
            public TProtocol OutputProtocol { get; }

            public void Dispose()
            {
                try
                {
                    OutputProtocol.Dispose();
                }
                finally
                {
                    if (!ReferenceEquals(InputProtocol, OutputProtocol))
                    {
                        InputProtocol.Dispose();
                    }
                }
            }
        }

        public virtual async Task OpenTransportAsync()
        {
            await OpenTransportAsync(CancellationToken.None);
        }

        public virtual async Task OpenTransportAsync(CancellationToken cancellationToken)
        {
            if (!_inputProtocol.Transport.IsOpen)
            {
                await _inputProtocol.Transport.OpenAsync(cancellationToken);
            }

            if (!_outputProtocol.Transport.IsOpen)
            {
                await _outputProtocol.Transport.OpenAsync(cancellationToken);
            }
        }

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
                }
            }

            _isDisposed = true;
        }
    }
}
