using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Thrift.Protocols;

namespace Thrift
{
    /// <summary>
    /// TBaseClient.
    /// Base client for generated clients.
    /// Do not change this class without checking generated code (namings, etc.)
    /// </summary>
    public abstract class TBaseClient
    {
        private readonly TProtocol _inputProtocol;
        private readonly TProtocol _outputProtocol;
        private int _seqId;
        private bool _isDisposed;

        protected TBaseClient(TProtocol inputProtocol, TProtocol outputProtocol)
        {
            if (inputProtocol == null)
            {
                throw new ArgumentNullException(nameof(inputProtocol));
            }

            if (outputProtocol == null)
            {
                throw new ArgumentNullException(nameof(outputProtocol));
            }

            _inputProtocol = inputProtocol;
            _outputProtocol = outputProtocol;
        }

        public TProtocol InputProtocol => _inputProtocol;

        public TProtocol OutputProtocol => _outputProtocol;

        public int SeqId => _seqId;

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

            if (!_inputProtocol.Transport.IsOpen)
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
