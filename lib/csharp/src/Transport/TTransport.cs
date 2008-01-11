//
//  TTransport.cs
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

namespace Thrift.Transport
{
	public abstract class TTransport
	{
		public abstract bool IsOpen
		{
			get;
		}

		public bool Peek()
		{
			return IsOpen;
		}

		public abstract void Open();

		public abstract void Close();

		public abstract int Read(byte[] buf, int off, int len);

		public int ReadAll(byte[] buf, int off, int len)
		{
			int got = 0;
			int ret = 0;

			while (got < len)
			{
				ret = Read(buf, off + got, len - got);
				if (ret <= 0)
				{
					throw new TTransportException("Cannot read, Remote side has closed");
				}
				got += ret;
			}

			return got;
		}

		public abstract void Write(byte[] buf, int off, int len);

		public virtual void Flush()
		{
		}
	}
}
