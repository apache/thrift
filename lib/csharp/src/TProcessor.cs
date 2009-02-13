//
//  TProcessor.cs
//
//  Begin:  Aug 19, 2007
//  Authors:
//		Todd Berman <tberman@imeem.com>
//
//  Distributed under the Thrift Software License
//
//  See accompanying file LICENSE or visit the Thrift site at:
//  http://developers.facebook.com/thrift/using

using System;
using Thrift.Protocol;

namespace Thrift
{
	public interface TProcessor
	{
		bool Process(TProtocol iprot, TProtocol oprot);
	}
}
