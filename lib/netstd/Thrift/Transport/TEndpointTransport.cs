using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Thrift.Transport
{

    abstract public class TEndpointTransport : TTransport
    {
        protected long MaxMessageSize { get => Configuration.MaxMessageSize; }
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
        protected void ResetConsumedMessageSize(long knownSize = -1)
        {
            if(knownSize >= 0)
                RemainingMessageSize = Math.Min( MaxMessageSize, knownSize);
            else
                RemainingMessageSize = MaxMessageSize;
        }

        /// <summary>
        /// Updates RemainingMessageSize to reflect then known real message size (e.g. framed transport).
        /// Will throw if we already consumed too many bytes.
        /// </summary>
        /// <param name="size"></param>
        public override void UpdateKnownMessageSize(long size)
        {
            var consumed = MaxMessageSize - RemainingMessageSize;
            ResetConsumedMessageSize(size);
            CountConsumedMessageBytes(consumed);
        }

        /// <summary>
        /// Throws if there are not enough bytes in the input stream to satisfy a read of numBytes bytes of data
        /// </summary>
        /// <param name="numBytes"></param>
        protected void CheckReadBytesAvailable(long numBytes)
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
