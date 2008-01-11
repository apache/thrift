//
//  TType.cs
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
using System.Collections.Generic;
using System.Text;

namespace Thrift.Protocol
{
	public enum TType : byte
	{
		Stop = 0,
		Void = 1,
		Bool = 2,
		Byte = 3,
		Double = 4,
		I16 = 6,
		I32 = 8,
		I64 = 10,
		String = 11,
		Struct = 12,
		Map = 13,
		Set = 14,
		List = 15
	}
}
