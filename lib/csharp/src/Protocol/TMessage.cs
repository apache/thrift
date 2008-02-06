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
		public TMessage(string name, TMessageType type, int seqid)
		{
			Name = name;
			Type = type;
			SeqID = seqid;
		}

		public string Name
		{
			get;
			set;
		}

		public TMessageType Type
		{
			get;
			set;
		}

		public int SeqID
		{
			get;
			set;
		}
	}
}
