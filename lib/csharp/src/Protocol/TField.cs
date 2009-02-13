//
//  TField.cs
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
	public struct TField
	{
		private string name;
		private TType type;
		private short id;

		public TField(string name, TType type, short id)
			:this()
		{
			this.name = name;
			this.type = type;
			this.id = id;
		}

		public string Name
		{
			get { return name; }
			set { name = value; }
		}

		public TType Type
		{
			get { return type; }
			set { type = value; }
		}

		public short ID
		{
			get { return id; }
			set { id = value; }
		}
	}
}
