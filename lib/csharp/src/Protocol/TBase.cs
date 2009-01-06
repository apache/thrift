//
//  TBase.cs
//
//  Distributed under the Thrift Software License
//
//  See accompanying file LICENSE or visit the Thrift site at:
//  http://developers.facebook.com/thrift/using

namespace Thrift.Protocol
{
	public interface TBase
	{
		///
		/// Reads the TObject from the given input protocol.
		///
		void Read(TProtocol tProtocol);

		///
		/// Writes the objects out to the protocol
		///
		void Write(TProtocol tProtocol);
	}
}
