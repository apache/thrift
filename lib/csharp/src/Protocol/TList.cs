//
//  TList.cs
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
	public struct TList
	{
		private TType elementType;
		private int count;

		public TList(TType elementType, int count)
			:this()
		{
			this.elementType = elementType;
			this.count = count;
		}

		public TType ElementType
		{
			get { return elementType; }
			set { elementType = value; }
		}

		public int Count
		{
			get { return count; }
			set { count = value; }
		}
	}
}
