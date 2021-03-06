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
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Thrift.Transport
{

    abstract public class TEndpointTransport : TTransport
    {
        protected long MaxMessageSize { get => Configuration.MaxMessageSize; }
        protected long KnownMessageSize { get; private set; }
        protected long RemainingMessageSize { get; private set; }

        private readonly TConfiguration _configuration;
        public override TConfiguration Configuration { get => _configuration; }

        public TEndpointTransport( TConfiguration config)
        {
            _configuration = config ?? new TConfiguration();
            Debug.Assert(Configuration != null);

            ResetConsumedMessageSize();
        }

        /// <summary>
        /// Resets RemainingMessageSize to the configured maximum 
        /// </summary>
        protected void ResetConsumedMessageSize(long newSize = -1)
        {
            // full reset 
            if (newSize < 0)
            {
                KnownMessageSize = MaxMessageSize;
                RemainingMessageSize = MaxMessageSize;
                return;
            }

            // update only: message size can shrink, but not grow
            Debug.Assert(KnownMessageSize <= MaxMessageSize);
            if (newSize > KnownMessageSize)
                throw new TTransportException(TTransportException.ExceptionType.EndOfFile, "MaxMessageSize reached");

            KnownMessageSize = newSize;
            RemainingMessageSize = newSize;
        }

        /// <summary>
        /// Updates RemainingMessageSize to reflect then known real message size (e.g. framed transport).
        /// Will throw if we already consumed too many bytes or if the new size is larger than allowed.
        /// </summary>
        /// <param name="size"></param>
        public override void UpdateKnownMessageSize(long size)
        {
            var consumed = KnownMessageSize - RemainingMessageSize;
            ResetConsumedMessageSize(size);
            CountConsumedMessageBytes(consumed);
        }

        /// <summary>
        /// Throws if there are not enough bytes in the input stream to satisfy a read of numBytes bytes of data
        /// </summary>
        /// <param name="numBytes"></param>
        public override void CheckReadBytesAvailable(long numBytes)
        {
            if (RemainingMessageSize < numBytes)
                throw new TTransportException(TTransportException.ExceptionType.EndOfFile, "MaxMessageSize reached");
        }

        /// <summary>
        /// Consumes numBytes from the RemainingMessageSize.
        /// </summary>
        /// <param name="numBytes"></param>
        protected void CountConsumedMessageBytes(long numBytes)
        {
            if (RemainingMessageSize >= numBytes)
            {
                RemainingMessageSize -= numBytes;
            }
            else
            {
                RemainingMessageSize = 0;
                throw new TTransportException(TTransportException.ExceptionType.EndOfFile, "MaxMessageSize reached");
            }
        }
    }
}
