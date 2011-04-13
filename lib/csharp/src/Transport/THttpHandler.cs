//
//  THttpHandler.cs
//
//  Authors:
//		Fredrik Hedberg <fhedberg@availo.com>
//
//  Distributed under the Apache Public License
//

using System;
using System.Web;

using Thrift.Protocol;

namespace Thrift.Transport
{
    public class THttpHandler : IHttpHandler
    {
        protected TProcessor processor;

        protected TProtocolFactory inputProtocolFactory;
        protected TProtocolFactory outputProtocolFactory;

        public THttpHandler(TProcessor processor)
            : this(processor, new TBinaryProtocol.Factory())
        {

        }
        
        public THttpHandler(TProcessor processor, TProtocolFactory protocolFactory)
            : this(processor, protocolFactory, protocolFactory)
        {

        }

        public THttpHandler(TProcessor processor, TProtocolFactory inputProtocolFactory, TProtocolFactory outputProtocolFactory)
        {
            this.processor = processor;
            this.inputProtocolFactory = inputProtocolFactory;
            this.outputProtocolFactory = outputProtocolFactory;
        }

        public void ProcessRequest(HttpContext context)
        {
            context.Response.ContentType = "application/x-thrift";
            context.Response.ContentEncoding = System.Text.Encoding.UTF8;

            TTransport transport = new TStreamTransport(context.Request.InputStream, context.Response.OutputStream);

            TProtocol inputProtocol = null;
            TProtocol outputProtocol = null;

            try
            {
                inputProtocol = inputProtocolFactory.GetProtocol(transport);
                outputProtocol = outputProtocolFactory.GetProtocol(transport);

                while (processor.Process(inputProtocol, outputProtocol)) { }
            }
            catch (TTransportException)
            {
                // Client died, just move on
            }
            catch (TApplicationException tx)
            {
                Console.Error.Write(tx);
            }
            catch (Exception x)
            {
                Console.Error.Write(x);
            }

            transport.Close();
        }

        public bool IsReusable
        {
            get { return true; }
        }
    }
}
