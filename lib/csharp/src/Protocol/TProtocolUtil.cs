//
//  TProtocolUtil.cs
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

namespace Thrift.Protocol
{
	public static class TProtocolUtil
	{
		public static void Skip(TProtocol prot, TType type)
		{
			switch (type)
			{
				case TType.Bool:
					prot.ReadBool();
					break;
				case TType.Byte:
					prot.ReadByte();
					break;
				case TType.I16:
					prot.ReadI16();
					break;
				case TType.I32:
					prot.ReadI32();
					break;
				case TType.I64:
					prot.ReadI64();
					break;
				case TType.Double:
					prot.ReadDouble();
					break;
				case TType.String:
					// Don't try to decode the string, just skip it.
					prot.ReadBinary();
					break;
				case TType.Struct:
					prot.ReadStructBegin();
					while (true)
					{
						TField field = prot.ReadFieldBegin();
						if (field.Type == TType.Stop)
						{
							break;
						}
						Skip(prot, field.Type);
						prot.ReadFieldEnd();
					}
					prot.ReadStructEnd();
					break;
				case TType.Map:
					TMap map = prot.ReadMapBegin();
					for (int i = 0; i < map.Count; i++)
					{
						Skip(prot, map.KeyType);
						Skip(prot, map.ValueType);
					}
					prot.ReadMapEnd();
					break;
				case TType.Set:
					TSet set = prot.ReadSetBegin();
					for (int i = 0; i < set.Count; i++)
					{
						Skip(prot, set.ElementType);
					}
					prot.ReadSetEnd();
					break;
				case TType.List:
					TList list = prot.ReadListBegin();
					for (int i = 0; i < list.Count; i++)
					{
						Skip(prot, list.ElementType);
					}
					prot.ReadListEnd();
					break;
			}
		}
	}
}
