using System;

namespace Thrift.Protocol
{
	class TProtocolException : Exception
	{
		public const int UNKNOWN = 0;
		public const int INVALID_DATA = 1;
		public const int NEGATIVE_SIZE = 2;
		public const int SIZE_LIMIT = 3;
		public const int BAD_VERSION = 4;

		protected int type_ = UNKNOWN;

		public TProtocolException()
			: base()
		{
		}

		public TProtocolException(int type)
			: base()
		{
			type_ = type;
		}

		public TProtocolException(int type, String message)
			: base(message)
		{
			type_ = type;
		}

		public TProtocolException(String message)
			: base(message)
		{
		}

		public int getType()
		{
			return type_;
		}
	}
}
