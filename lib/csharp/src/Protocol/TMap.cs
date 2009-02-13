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
		private TType keyType;
		private TType valueType;
		private int count;

		public TMap(TType keyType, TType valueType, int count)
			:this()
		{
			this.keyType = keyType;
			this.valueType = valueType;
			this.count = count;
		}

		public TType KeyType
		{
			get { return keyType; }
			set { keyType = value; }
		}

		public TType ValueType
		{
			get { return valueType; }
			set { valueType = value; }
		}

		public int Count
		{
			get { return count; }
			set { count = value; }
		}
	}
}
