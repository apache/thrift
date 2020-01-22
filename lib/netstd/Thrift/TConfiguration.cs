using System;
using System.Collections.Generic;
using System.Text;

namespace Thrift
{
    public class TConfiguration
    {
        public const int DEFAULT_MAX_MESSAGE_SIZE = 100 * 1024 * 1024;
        public const int DEFAULT_MAX_FRAME_SIZE = 16384000;      // this value is used consistently across all Thrift libraries
        public const int DEFAULT_RECURSION_DEPTH = 64;

        public int MaxMessageSize { get; set; } = DEFAULT_MAX_MESSAGE_SIZE;
        public int MaxFrameSize { get; set; } = DEFAULT_MAX_FRAME_SIZE;
        public int RecursionLimit { get; set; } = DEFAULT_RECURSION_DEPTH;

        // TODO(JensG): add connection and i/o timeouts
    }
}
