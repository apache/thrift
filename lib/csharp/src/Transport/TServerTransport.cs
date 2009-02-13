//
//  TServerTransport.cs
//
//  Begin:  Dec 3, 2007
//  Authors:
//		Will Palmeri <wpalmeri@imeem.com>
//
//  Copyright (C) 2007 imeem, inc. <http://www.imeem.com>
//  All rights reserved.
using System;

namespace Thrift.Transport
{
	public abstract class TServerTransport
	{
		public abstract void Listen();
		public abstract void Close();
		protected abstract TTransport AcceptImpl();

		public TTransport Accept()
		{
			TTransport transport = AcceptImpl();
			if (transport == null) {
			  throw new TTransportException("accept() may not return NULL");
			}
			return transport;
		 }
	}
}
