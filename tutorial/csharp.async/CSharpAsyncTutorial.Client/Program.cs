using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Thrift;
using Thrift.Protocol;
using Thrift.Transport;

namespace CSharpAsyncTutorial.Client
{
    class Program
    {
        static void Main(string[] args)
        {
            Task.Run(async () => {
                await Run();
            }).Wait();
        }

        static async Task Run()
        {
            try
            {
                string host = "localhost";
                //string host = "thrift.service.consul";
                int port = 9090;

                var transport = new TSocket(host, port, 100000);
                TBufferedTransport transportBuff = new TBufferedTransport(transport, 2048);
                TProtocol protocol = new TBinaryProtocol(transportBuff);
                transport.Open();
                Calculator.Client client = new Calculator.Client(protocol);

                Console.WriteLine($"run Client host:{transport.Host},port:{transport.Port}");
                await client.pingAsync();

                int testCount = 100000;
                var sw = Stopwatch.StartNew();
                foreach (var m in Enumerable.Range(0, testCount))
                {
                    int sum = client.add(1, 1);
                }
                sw.Stop();
                Console.WriteLine($"TBufferedTransport Execute client.add(1, 1) do:{testCount} ms:{sw.ElapsedMilliseconds}");

                sw.Restart();
                foreach (var m in Enumerable.Range(0, testCount))
                {
                    int sum = await client.addAsync(1, 1);
                }
                sw.Stop();
                Console.WriteLine($"TBufferedTransport Execute client.addAsync(1, 1) do:{testCount} ms:{sw.ElapsedMilliseconds}");


                //await client.calculateAsync(1,new Work() { Op=Operation.ADD,Comment="add Comment"});//InvalidOperation

                transport.Close();
            }
            catch (InvalidOperation ex)
            {
                Console.WriteLine($"InvalidOperation why:{ex.Why} op:{ex.WhatOp} ex:{ex.GetType()} message:{ex.Message} {ex.StackTrace}");
            }
            catch (TApplicationException x)
            {
                Console.WriteLine($"TApplicationException ex:{x.Type} message:{x.Message} {x.StackTrace}");
            }
        }
    }
}
