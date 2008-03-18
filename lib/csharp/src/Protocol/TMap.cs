//
//  TMap.cs
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
	public struct TMap
	{
		public TMap(TType keyType, TType valueType, int count)
			:this()
		{
			KeyType = keyType;
			ValueType = valueType;
			Count = count;
		}

		public TType KeyType
		{
			get;
			set;
		}

		public TType ValueType
		{
			get;
			set;
		}

		public int Count
		{
			get;
			set;
		}
	}
}
