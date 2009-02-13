//
//  TApplicationException.cs
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
	public class TApplicationException : Exception
	{
		protected ExceptionType type;

		public TApplicationException()
		{
		}

		public TApplicationException(ExceptionType type)
		{
			this.type = type;
		}

		public TApplicationException(ExceptionType type, string message)
			: base(message)
		{
			this.type = type;
		}

		public static TApplicationException Read(TProtocol iprot)
		{
			TField field;

			string message = null;
			ExceptionType type = ExceptionType.Unknown;

			while (true)
			{
				field = iprot.ReadFieldBegin();
				if (field.Type == TType.Stop)
				{
					break;
				}

				switch (field.ID)
				{
					case 1:
						if (field.Type == TType.String)
						{
							message = iprot.ReadString();
						}
						else
						{
							TProtocolUtil.Skip(iprot, field.Type);
						}
						break;
					case 2:
						if (field.Type == TType.I32)
						{
							type = (ExceptionType)iprot.ReadI32();
						}
						else
						{
							TProtocolUtil.Skip(iprot, field.Type);
						}
						break;
					default:
						TProtocolUtil.Skip(iprot, field.Type);
						break;
				}

				iprot.ReadFieldEnd();
			}

			iprot.ReadStructEnd();

			return new TApplicationException(type, message);
		}

		public void Write(TProtocol oprot)
		{
			TStruct struc = new TStruct("TApplicationException");
			TField field = new TField();

			oprot.WriteStructBegin(struc);

			if (!String.IsNullOrEmpty(Message))
			{
				field.Name = "message";
				field.Type = TType.String;
				field.ID = 1;
				oprot.WriteFieldBegin(field);
				oprot.WriteString(Message);
				oprot.WriteFieldEnd();
			}

			field.Name = "type";
			field.Type = TType.I32;
			field.ID = 2;
			oprot.WriteFieldBegin(field);
			oprot.WriteI32((int)type);
			oprot.WriteFieldEnd();
			oprot.WriteFieldStop();
			oprot.WriteStructEnd();
		}

		public enum ExceptionType
		{
			Unknown,
			UnknownMethod,
			InvalidMessageType,
			WrongMethodName,
			BadSequenceID,
			MissingResult
		}
	}
}
