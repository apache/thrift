//
//  TTransportFactory.cs
//
//  Begin:  Dec 3, 2007
//  Authors: 
//		Will Palmeri <wpalmeri@imeem.com>
//
//  Copyright (C) 2007 imeem, inc. <http://www.imeem.com>
//  All rights reserved.
//
using System;
using System.Collections.Generic;
using System.Text;


namespace Thrift.Transport
{
	/// <summary>
	/// From Mark Slee & Aditya Agarwal of Facebook:
	/// Factory class used to create wrapped instance of Transports.
	/// This is used primarily in servers, which get Transports from
	/// a ServerTransport and then may want to mutate them (i.e. create
	/// a BufferedTransport from the underlying base transport)
	/// </summary>
	public class TTransportFactory
	{
		public TTransport GetTransport(TTransport trans)
		{
			return trans;
		}
	}
}
