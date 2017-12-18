using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NSubstitute;
using Thrift.Protocols;
using Thrift.Protocols.Entities;
using Thrift.Transports;
using Thrift.Transports.Client;

namespace Thrift.Tests.Protocols
{
    // ReSharper disable once InconsistentNaming
    [TestClass]
    public class TJSONProtocolTests
    {
        [TestMethod]
        public void TJSONProtocol_Can_Create_Instance_Test()
        {
            var httpClientTransport = Substitute.For<THttpClientTransport>(new Uri("http://localhost"), null);

            var result = new TJSONProtocolWrapper(httpClientTransport);

            Assert.IsNotNull(result);
            Assert.IsNotNull(result.WrappedContext);
            Assert.IsNotNull(result.WrappedReader);
            Assert.IsNotNull(result.Transport);
            Assert.IsTrue(result.WrappedRecursionDepth == 0);
            Assert.IsTrue(result.WrappedRecursionLimit == TProtocol.DefaultRecursionDepth);

            Assert.IsTrue(result.Transport.Equals(httpClientTransport));
            Assert.IsTrue(result.WrappedContext.GetType().Name.Equals("JSONBaseContext", StringComparison.OrdinalIgnoreCase));
            Assert.IsTrue(result.WrappedReader.GetType().Name.Equals("LookaheadReader", StringComparison.OrdinalIgnoreCase));
        }

        private class TJSONProtocolWrapper : TJSONProtocol
        {
            public TJSONProtocolWrapper(TClientTransport trans) : base(trans)
            {
            }

            public object WrappedContext => Context;
            public object WrappedReader => Reader;
            public int WrappedRecursionDepth => RecursionDepth;
            public int WrappedRecursionLimit => RecursionLimit;
        }
    }
}
