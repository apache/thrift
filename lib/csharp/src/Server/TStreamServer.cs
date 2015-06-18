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
        private TProcessor processor;
        private TStreamTransport transport;
        private TProtocolFactory inputProtocolFactory;
        private TProtocolFactory outputProtocolFactory;
        private TTransportFactory inputTransportFactory;
        private TTransportFactory outputTransportFactory;

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
               //Object connectionContext = null;
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

                        //if (inputTransport.Peek())
                        //    Console.WriteLine("DEBUG: peek was true");

                        //Process client request (blocks until transport is readable)
                        if (!processor.Process(inputProtocol, outputProtocol))
                            break;

                        //if (processor.Process(inputProtocol, outputProtocol))
                        //    Console.WriteLine("DEBUG: Emma is surprised.");

                    }
                }
            }
        }

        //This crashed because the stream got closed before it was read. Maybe these are the using blocks? though they are the same as in SimpleServer...
        /*
        public void Serve()
        {
            while (true)
            {
                TTransport client = null;
                TTransport inputTransport = null;
                TTransport outputTransport = null;
                TProtocol inputProtocol = null;
                TProtocol outputProtocol = null;
               //Object connectionContext = null;
                using (client = transport)
                {
                    if (client != null)
                    {
                        using (inputTransport = inputTransportFactory.GetTransport(client))//EMMA: This is a new TFramedTransport with the transport set to the client.
                        {
                            using (outputTransport = outputTransportFactory.GetTransport(client))// EMMA: same as above
                            {
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
        }*/

        //I abandoned this in favor of trying to imitate simpleserver as closely as possible
        /*public void Serve()
        {
            TStreamTransport client = null;
            TProtocol inputProtocol = null;
            TProtocol outputProtocol = null;
            TTransport inputTransport = null;
            TTransport outputTransport = null;
            //Console.WriteLine("DEBUG: transport is null? " + (transport==null).ToString());
            using (client = transport)
            {
                if (client != null)
                {
                    inputTransport = inputTransportFactory.GetTransport(client);
                    outputTransport = outputTransportFactory.GetTransport(client);
                    inputProtocol = inputProtocolFactory.GetProtocol(inputTransport);
                    outputProtocol = outputProtocolFactory.GetProtocol(outputTransport);
                    //Console.WriteLine("DEBUG: " + client.Peek().ToString());
                    //Console.WriteLine("DEBUG: transport is open: " + transport.IsOpen.ToString());
                    while (true)
                    {
                        while (true)
                        {
                            if (inputTransport.Peek())//EMMA: YOU ARE HERE AND YOU NEED TO PROBABLY PUT THIS INSIDE OF AN OUTER LOOP THAT ACTUALLY RUNS. RIGHT NOW WHEN THERE ARE NO BYTES, PEEK RETURNS FALSE AND
                            //NOTHING INTERESTING HAPPENS.
                            {
                                Console.WriteLine("DEBUG: We peeked.");
                                if (!processor.Process(inputProtocol, outputProtocol))
                                    break;
                            }
                        }
                    }
                }
            }
        }*/
    }
}
