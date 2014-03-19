using System;
using System.Collections.Generic;
using System.IO.Pipes;

namespace Thrift.Transport
{
	public class TNamedPipeServerTransport : TServerTransport
	{
		/// <summary>
		/// This is the address of the Pipe on the localhost.
		/// </summary>
		private readonly string pipeAddress;
		NamedPipeServerStream stream = null;

		public TNamedPipeServerTransport(string pipeAddress)
		{
			this.pipeAddress = pipeAddress;
		}

		public override void Listen()
		{
			// nothing to do here
		}

		public override void Close()
		{
			if (stream != null)
			{
				try
				{
					stream.Close();
					stream.Dispose();
				}
				finally
				{
					stream = null;
				}
			}
		}

		private void EnsurePipeInstance()
		{
			if( stream == null)
				stream = new NamedPipeServerStream(
					pipeAddress, PipeDirection.InOut, 254,
					PipeTransmissionMode.Byte,
					PipeOptions.None, 4096, 4096 /*TODO: security*/);
		}

		protected override TTransport AcceptImpl()
		{
			try
			{
				EnsurePipeInstance();
				stream.WaitForConnection();
				var trans = new ServerTransport(stream);
				stream = null;  // pass ownership to ServerTransport
				return trans;
			}
			catch (Exception e)
			{
				Close();
				throw new TTransportException(TTransportException.ExceptionType.NotOpen, e.Message);
			}
		}

		private class ServerTransport : TTransport
		{
			private NamedPipeServerStream server;
			public ServerTransport(NamedPipeServerStream server)
			{
				this.server = server;
			}

			public override bool IsOpen
			{
				get { return server != null && server.IsConnected; }
			}

			public override void Open()
			{
			}

			public override void Close()
			{
				if (server != null) server.Close();
			}

			public override int Read(byte[] buf, int off, int len)
			{
				if (server == null)
				{
					throw new TTransportException(TTransportException.ExceptionType.NotOpen);
				}
				return server.Read(buf, off, len);
			}

			public override void Write(byte[] buf, int off, int len)
			{
				if (server == null)
				{
					throw new TTransportException(TTransportException.ExceptionType.NotOpen);
				}
				server.Write(buf, off, len);
			}

			protected override void Dispose(bool disposing)
			{
				server.Dispose();
			}
		}
	}
}