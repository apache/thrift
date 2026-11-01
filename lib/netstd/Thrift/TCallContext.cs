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
using Thrift.Protocol;
using Thrift.Protocol.Entities;
using Thrift.Transport;

namespace Thrift
{
    /// <summary>
    /// State of one logical call of an isolated client: its own protocol over its own transport view, its sequence
    /// identifier and its cancellation token. The token combines the caller's token with an optional timeout, and the
    /// context owns the resources of that combination. Disposing the context disposes the protocol and thereby the
    /// per-call transport view; the shared transport is not affected.
    /// </summary>
    public sealed class TCallContext : IDisposable
    {
        private readonly CancellationToken _callerToken;
        private readonly CancellationTokenSource _timeoutSource;
        private readonly CancellationTokenSource _linkedSource;
        private TProtocol _protocol;

        /// <param name="protocol">The protocol over this call's own transport view; the context takes ownership.</param>
        /// <param name="seqId">The sequence id assigned to this call.</param>
        /// <param name="cancellationToken">Cancellation token of the caller.</param>
        /// <param name="timeout">Maximum duration of the call, measured from creation of the context until it is
        /// disposed; <c>null</c> or <see cref="Timeout.InfiniteTimeSpan"/> for no bound.</param>
        public TCallContext(TProtocol protocol, int seqId, CancellationToken cancellationToken = default, TimeSpan? timeout = null)
        {
            _protocol = protocol ?? throw new ArgumentNullException(nameof(protocol));
            SeqId = seqId;
            _callerToken = cancellationToken;

            if (timeout.HasValue && timeout.Value != Timeout.InfiniteTimeSpan)
            {
                if (timeout.Value < TimeSpan.Zero)
                {
                    throw new ArgumentOutOfRangeException(nameof(timeout), "The timeout must be non-negative or infinite.");
                }

                // the caller's token and the timeout are linked; the timeout source tells the two reasons apart
                _timeoutSource = new CancellationTokenSource(timeout.Value);
                _linkedSource = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, _timeoutSource.Token);
                CancellationToken = _linkedSource.Token;
            }
            else
            {
                CancellationToken = cancellationToken; // nothing to combine, nothing to allocate
            }
        }

        public TProtocol Protocol => _protocol ?? throw new ObjectDisposedException(nameof(TCallContext));

        public int SeqId { get; }

        /// <summary>Cancelled when the caller cancels or the call's timeout elapses.</summary>
        public CancellationToken CancellationToken { get; }

        /// <summary>True when the call's timeout elapsed and the caller had not cancelled.</summary>
        public bool IsTimedOut => _timeoutSource != null && _timeoutSource.IsCancellationRequested && !_callerToken.IsCancellationRequested;

        /// <summary>True when <paramref name="exception"/> is the effect of this call's timeout.</summary>
        public bool IsTimeoutFailure(Exception exception)
        {
            if (!IsTimedOut)
            {
                return false;
            }

            return exception is OperationCanceledException
                || (exception is TTransportException transportException
                    && transportException.Type == TTransportException.ExceptionType.Interrupted);
        }

        /// <summary>Creates the exception reported for a call that exceeded its timeout.</summary>
        public TTransportException CreateTimeoutException(Exception inner)
        {
            return new TTransportException(TTransportException.ExceptionType.TimedOut, "The call timed out.", inner);
        }

        /// <summary>Throws a bad-sequence-id application exception when a reply does not belong to this call.</summary>
        public void ValidateReply(TMessage message)
        {
            if (message.SeqID != SeqId)
            {
                throw new TApplicationException(TApplicationException.ExceptionType.BadSequenceId,
                    $"{message.Name} failed: out of sequence response (expected {SeqId}, got {message.SeqID})");
            }
        }

        public void Dispose()
        {
            var protocol = Interlocked.Exchange(ref _protocol, null);
            protocol?.Dispose();
            _linkedSource?.Dispose();
            _timeoutSource?.Dispose();
        }
    }
}
