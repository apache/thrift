//
//  TMessage.cs
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
	public struct TMessage
	{
		private string name;
		private TMessageType type;
		private int seqID;

		public TMessage(string name, TMessageType type, int seqid)
			:this()
		{
			this.name = name;
			this.type = type;
			this.seqID = seqid;
		}

		public string Name
		{
			get { return name; }
			set { name = value; }
		}

		public TMessageType Type
		{
			get { return type; }
			set { type = value; }
		}

		public int SeqID
		{
			get { return seqID; }
			set { seqID = value; }
		}
	}
}
