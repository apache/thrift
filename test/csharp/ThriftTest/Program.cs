// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

using System;
using Thrift.Transport;
using Thrift.Protocol;
using Thrift.Test; //generated code

namespace Test
{
	class Program
	{
		static void Main(string[] args)
		{
			if (args.Length == 0)
			{
				Console.WriteLine("must provide 'server' or 'client' arg");
				return;
			}

			string[] subArgs = new string[args.Length - 1];
			for(int i = 1; i < args.Length; i++)
			{
				subArgs[i-1] = args[i];
			}
			if (args[0] == "client")
			{
				TestClient.Execute(subArgs);
			}
			else if (args[0] == "server")
			{
				TestServer.Execute(subArgs);
			}
			else
			{
				Console.WriteLine("first argument must be 'server' or 'client'");
			}
		}
	}
}
