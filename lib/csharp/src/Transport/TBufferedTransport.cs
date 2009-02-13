//
//  TBufferedTransport.cs
//
//  Begin:  May 22, 2008
//  Authors:
//		Will Palmeri <wpalmeri@imeem.com>
//

using System;
using System.IO;

namespace Thrift.Transport
{
	public class TBufferedTransport : TTransport
	{
		private BufferedStream inputBuffer;
		private BufferedStream outputBuffer;
		private int bufSize;
		private TStreamTransport transport;

		public TBufferedTransport(TStreamTransport transport)
			:this(transport, 1024)
		{

		}

		public TBufferedTransport(TStreamTransport transport, int bufSize)
		{
			this.bufSize = bufSize;
			this.transport = transport;
			InitBuffers();
		}

		private void InitBuffers()
		{
			if (transport.InputStream != null)
			{
				inputBuffer = new BufferedStream(transport.InputStream, bufSize);
			}
			if (transport.OutputStream != null)
			{
				outputBuffer = new BufferedStream(transport.OutputStream, bufSize);
			}
		}

		public TTransport UnderlyingTransport
		{
			get { return transport; }
		}

		public override bool IsOpen
		{
			get { return transport.IsOpen; }
		}

		public override void Open()
		{
			transport.Open();
			InitBuffers();
		}

		public override void Close()
		{
			if (inputBuffer != null && inputBuffer.CanRead)
			{
				inputBuffer.Close();
			}
			if (outputBuffer != null && outputBuffer.CanWrite)
			{
				outputBuffer.Close();
			}
		}

		public override int Read(byte[] buf, int off, int len)
		{
			return inputBuffer.Read(buf, off, len);
		}

		public override void Write(byte[] buf, int off, int len)
		{
			outputBuffer.Write(buf, off, len);
		}

		public override void Flush()
		{
			outputBuffer.Flush();
		}
	}
}
