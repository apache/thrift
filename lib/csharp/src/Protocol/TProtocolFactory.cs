//
//  TProtocolFactory.cs
//
//  Begin:  Dec 3, 2007
//  Authors:
//		Will Palmeri <wpalmeri@imeem.com>
//
//  Distributed under the Thrift Software License
//
//  See accompanying file LICENSE or visit the Thrift site at:
//  http://developers.facebook.com/thrift/using
using System;
using System.Collections.Generic;
using System.Text;
using Thrift.Transport;

namespace Thrift.Protocol
{
	public interface TProtocolFactory
	{
		TProtocol GetProtocol(TTransport trans);
	}
}
