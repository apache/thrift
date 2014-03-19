using System.IO.Pipes;

namespace Thrift.Transport
{
	public class TNamedPipeClientTransport : TTransport
	{
		private NamedPipeClientStream client;
		private string ServerName;
		private string PipeName;

		public TNamedPipeClientTransport(string pipe)
		{
			ServerName = ".";
			PipeName = pipe;
		}

		public TNamedPipeClientTransport(string server, string pipe)
		{
			ServerName = (server != "") ? server : ".";
			PipeName = pipe;
		}

		public override bool IsOpen
		{
			get { return client != null && client.IsConnected; }
		}

		public override void Open()
		{
			if (IsOpen)
			{
				throw new TTransportException(TTransportException.ExceptionType.AlreadyOpen);
			}
			client = new NamedPipeClientStream(ServerName, PipeName, PipeDirection.InOut, PipeOptions.None);
			client.Connect();
		}

		public override void Close()
		{
			if (client != null)
			{
				client.Close();
				client = null;
			}
		}

		public override int Read(byte[] buf, int off, int len)
		{
			if (client == null)
			{
				throw new TTransportException(TTransportException.ExceptionType.NotOpen);
			}

			return client.Read(buf, off, len);
		}

		public override void Write(byte[] buf, int off, int len)
		{
			if (client == null)
			{
				throw new TTransportException(TTransportException.ExceptionType.NotOpen);
			}

			client.Write(buf, off, len);
		}

		protected override void Dispose(bool disposing)
		{
			client.Dispose();
		}
	}
}