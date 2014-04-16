namespace Thrift.Tests.Transport
{
    using System;
    using Moq;
    using NUnit.Framework;
    using Protocol;
    using Thrift.Transport;

    [TestFixture]
    public class THttpHandlerTests
    {
        private Mock<TProcessor> processor;
        private Mock<TProtocolFactory> inputFactory;
        private Mock<TProtocolFactory> outputFactory;

        #region Exception Handling
        // It's important that the handler doesn't swallow exceptions from the processor,
        // because otherwise it can make it difficult to debug issues with the processor
        // when the application is not running in the console.

        [Test]
        public void ProcessRequest_ThrowsProcessorExceptions()
        {
            // Arrange
            var handler = CreateHandler();

            processor.Setup(p => p.Process(It.IsAny<TProtocol>(), It.IsAny<TProtocol>())).Throws<Exception>();

            // Act / Assert
            Assert.Throws<Exception>(() => handler.ProcessRequest(null, null));
        }

        [Test]
        public void ProcessRequest_ThrowsTApplicationExceptions()
        {
            // Arrange
            var handler = CreateHandler();

            processor.Setup(p => p.Process(It.IsAny<TProtocol>(), It.IsAny<TProtocol>()))
                .Throws<TApplicationException>();

            // Act / Assert
            Assert.Throws<TApplicationException>(() => handler.ProcessRequest(null, null));
        }

        [Test]
        public void ProcessRequest_SwallowsTTransportExceptions()
        {
            // Arrange
            var handler = CreateHandler();

            processor.Setup(p => p.Process(It.IsAny<TProtocol>(), It.IsAny<TProtocol>()))
                .Throws<TTransportException>();

            // Act / Assert
            Assert.DoesNotThrow(() => handler.ProcessRequest(null, null));
        }
        #endregion

        private THttpHandler CreateHandler()
        {
            processor = new Mock<TProcessor>();
            inputFactory = new Mock<TProtocolFactory>();
            outputFactory = new Mock<TProtocolFactory>();

            return new THttpHandler(processor.Object, inputFactory.Object, outputFactory.Object);
        }
    }
}
