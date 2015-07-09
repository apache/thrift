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
        static void Main(string[] args)
        {
            try
            {
                TestHandler handler = new TestHandler();
                ThriftTest.Processor processor = new ThriftTest.Processor(handler);

                TStreamTransport transport = new TStreamTransport(Console.OpenStandardInput(), Console.OpenStandardOutput());

                TStreamServer server = new TStreamServer(processor, transport, new TTransportFactory(), new TJSONProtocol.Factory());
                server.Serve();
            }
            catch (Exception x)
            {
                Console.WriteLine(x.StackTrace);
            }   
        }
    }
}
