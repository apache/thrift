using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Thrift.Transport;
using Thrift.Protocol;

namespace Thrift.Server
{
    public class TStreamServer
    {
        protected TProcessor processor;
        private TStreamTransport transport;        
        protected TTransportFactory inputTransportFactory;
        protected TTransportFactory outputTransportFactory;
        protected TProtocolFactory inputProtocolFactory;
        protected TProtocolFactory outputProtocolFactory;

        public TStreamServer(TProcessor processor, TStreamTransport transport)
            : this(processor, transport, new TTransportFactory(), new TBinaryProtocol.Factory())
        {
            
        }

        public TStreamServer(TProcessor processor, TStreamTransport transport, TTransportFactory transportFactory, TProtocolFactory protocolFactory)
        {
            this.processor = processor;
            this.transport = transport;
            this.inputProtocolFactory = protocolFactory;
            this.outputProtocolFactory = protocolFactory;
            this.inputTransportFactory = transportFactory;
            this.outputTransportFactory = transportFactory;
        }

        public void Serve()
        {
            while (true)
            {
                TTransport client = null;
                TTransport inputTransport = null;
                TTransport outputTransport = null;
                TProtocol inputProtocol = null;
                TProtocol outputProtocol = null;
                client = transport;

                if (client != null)
                {
                    inputTransport = inputTransportFactory.GetTransport(client);//EMMA: This is a new TFramedTransport with the transport set to the client (which is the transport).

                    outputTransport = outputTransportFactory.GetTransport(client);// EMMA: same as above

                    inputProtocol = inputProtocolFactory.GetProtocol(inputTransport);//EMMA: these each return a TProtocol. Which gets created using the transport.
                    outputProtocol = outputProtocolFactory.GetProtocol(outputTransport);

                    //Process client requests until client disconnects
                    while (true)
                    {
                        if (!inputTransport.Peek())
                            break;

                        //Process client request (blocks until transport is readable)
                        if (!processor.Process(inputProtocol, outputProtocol))
                            break;
                    }
                }
            }
        }
    }
}
