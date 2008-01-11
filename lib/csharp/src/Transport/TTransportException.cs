//
//  TTransportException.cs
//
//  Begin:  Aug 19, 2007
//  Authors: 
//		Todd Berman <tberman@imeem.com>
//
//  Copyright (C) 2007 imeem, inc. <http://www.imeem.com>
//  All rights reserved.
//

using System;
using System.Collections.Generic;
using System.Text;

namespace Thrift.Transport
{
	public class TTransportException : Exception
	{
		protected ExceptionType type;

		public TTransportException()
			: base()
		{
		}

		public TTransportException(ExceptionType type)
			: this()
		{
			this.type = type;
		}

		public TTransportException(ExceptionType type, string message)
			: base(message)
		{
			this.type = type;
		}

		public TTransportException(string message)
			: base(message)
		{
		}

		public ExceptionType Type
		{
			get { return type; }
		}

		public enum ExceptionType
		{
			Unknown,
			NotOpen,
			AlreadyOpen,
			TimedOut,
			EndOfFile
		}
	}
}
