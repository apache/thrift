using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Thrift.Transport;
using Thrift.Server;
using Thrift.Protocol;

namespace Thrift.Test
{
    class Program
    {
        private static TProtocolFactory protocolFactory = new TBinaryProtocol.Factory();

        static void Main(string[] args)
        {
            try
            {
                for (int i = 0; i < args.Length; i++)
                {
                    if (args[i] == "--binary")
                    {
                        protocolFactory = new TBinaryProtocol.Factory();
                    }
                    if (args[i] == "--json")
                    {
                        protocolFactory = new TJSONProtocol.Factory();
                    }
                    if (args[i] == "--compact")
                    {
                        protocolFactory = new TCompactProtocol.Factory();
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.StackTrace);
            }
            try
            {
                TestHandler handler = new TestHandler();
                ThriftTest.Processor processor = new ThriftTest.Processor(handler);

                TStreamTransport transport = new TStreamTransport(Console.OpenStandardInput(), Console.OpenStandardOutput());

                TStreamServer server = new TStreamServer(processor, transport, new TTransportFactory(), protocolFactory);
                server.Serve();
            }
            catch (Exception x)
            {
                Console.WriteLine(x.StackTrace);
            }   
        }
    }
}
