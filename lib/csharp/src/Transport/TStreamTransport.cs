//
//  TStreamTransport.cs
//
//  Begin:  Aug 19, 2007
//  Authors: 
//		Todd Berman <tberman@imeem.com>
//
//  Copyright (C) 2007 imeem, inc. <http://www.imeem.com>
//  All rights reserved.
//

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace Thrift.Transport
{
	public class TStreamTransport : TTransport
	{
		protected Stream inputStream;
		protected Stream outputStream;

		public TStreamTransport()
		{
		}

		public TStreamTransport(Stream inputStream, Stream outputStream)
		{
			this.inputStream = inputStream;
			this.outputStream = outputStream;
		}

		public override bool IsOpen
		{
			get { return true; }
		}

		public override void Open()
		{
		}

		public override void Close()
		{
			if (inputStream != null)
			{
				inputStream.Close();
				inputStream = null;
			}
			if (outputStream != null)
			{
				outputStream.Close();
				outputStream = null;
			}
		}

		public override int Read(byte[] buf, int off, int len)
		{
			if (inputStream == null)
			{
				throw new TTransportException(TTransportException.ExceptionType.NotOpen, "Cannot read from null inputstream");
			}

			return inputStream.Read(buf, off, len);
		}

		public override void Write(byte[] buf, int off, int len)
		{
			if (outputStream == null)
			{
				throw new TTransportException(TTransportException.ExceptionType.NotOpen, "Cannot write to null outputstream");
			}

			outputStream.Write(buf, off, len);
		}

		public override void Flush()
		{
			if (outputStream == null)
			{
				throw new TTransportException(TTransportException.ExceptionType.NotOpen, "Cannot flush null outputstream");
			}

			outputStream.Flush();
		}
	}
}
