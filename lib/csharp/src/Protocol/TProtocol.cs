//
//  TProtocol.cs
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
using Thrift.Transport;

namespace Thrift.Protocol
{
	public abstract class TProtocol
	{
		protected TTransport trans;

		protected TProtocol(TTransport trans)
		{
			this.trans = trans;
		}

		public TTransport Transport
		{
			get { return trans; }
		}

		public abstract void WriteMessageBegin(TMessage message);
		public abstract void WriteMessageEnd();
		public abstract void WriteStructBegin(TStruct struc);
		public abstract void WriteStructEnd();
		public abstract void WriteFieldBegin(TField field);
		public abstract void WriteFieldEnd();
		public abstract void WriteFieldStop();
		public abstract void WriteMapBegin(TMap map);
		public abstract void WriteMapEnd();
		public abstract void WriteListBegin(TList list);
		public abstract void WriteListEnd();
		public abstract void WriteSetBegin(TSet set);
		public abstract void WriteSetEnd();
		public abstract void WriteBool(bool b);
		public abstract void WriteByte(byte b);
		public abstract void WriteI16(short i16);
		public abstract void WriteI32(int i32);
		public abstract void WriteI64(long i64);
		public abstract void WriteDouble(double d);
		public void WriteString(string s) {
			WriteBinary(Encoding.UTF8.GetBytes(s));
		}
		public abstract void WriteBinary(byte[] b);

		public abstract TMessage ReadMessageBegin();
		public abstract void ReadMessageEnd();
		public abstract TStruct ReadStructBegin();
		public abstract void ReadStructEnd();
		public abstract TField ReadFieldBegin();
		public abstract void ReadFieldEnd();
		public abstract TMap ReadMapBegin();
		public abstract void ReadMapEnd();
		public abstract TList ReadListBegin();
		public abstract void ReadListEnd();
		public abstract TSet ReadSetBegin();
		public abstract void ReadSetEnd();
		public abstract bool ReadBool();
		public abstract byte ReadByte();
		public abstract short ReadI16();
		public abstract int ReadI32();
		public abstract long ReadI64();
		public abstract double ReadDouble();
		public string ReadString() {
		       return Encoding.UTF8.GetString(ReadBinary());
		}
		public abstract byte[] ReadBinary();
	}
}
