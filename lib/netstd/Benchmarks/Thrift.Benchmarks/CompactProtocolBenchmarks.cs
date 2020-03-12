using System.IO;
using System.Threading.Tasks;

using BenchmarkDotNet.Attributes;

using Thrift.Protocol;
using Thrift.Transport.Client;

namespace Thrift.Benchmarks
{
    [MemoryDiagnoser]
    public class CompactProtocolBenchmarks
    {
        private MemoryStream _Stream;
        private TCompactProtocol _Protocol;

        [Params(10000)]
        public int NumberOfOperationsPerIteration { get; set; }

        [GlobalSetup]
        public void GlobalSetup()
        {
            _Stream = new MemoryStream();
            var transport = new TStreamTransport(_Stream, _Stream, null);
            _Protocol = new TCompactProtocol(transport);
        }

        [GlobalCleanup]
        public void GlobalCleanup()
        {
            _Protocol.Dispose();
        }

        [Benchmark]
        public async Task WriteString()
        {
            for (int i = 0; i < NumberOfOperationsPerIteration; i++)
            {
                await _Protocol.WriteStringAsync("Thrift String Benchmark");

                _Stream.Seek(0, SeekOrigin.Begin);
            }
        }

        [Benchmark]
        public async Task ReadString()
        {
            await _Protocol.WriteStringAsync("Thrift String Benchmark");

            for (int i = 0; i < NumberOfOperationsPerIteration; i++)
            {
                _Stream.Seek(0, SeekOrigin.Begin);

                await _Protocol.ReadStringAsync();
            }
        }
    }
}
