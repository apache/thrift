/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

using System;
using System.Diagnostics;
using System.IO;
using System.IO.Compression;

namespace Thrift.Transport
{
    public class TZLibTransport : TBufferedTransport
    {
        // Writes smaller than this are buffered up.
        // Larger (or equal) writes are dumped straight to zlib.
        const int MIN_DIRECT_DEFLATE_SIZE = 32;
        const int MAX_UNCOMPRESSED_BUFFER_SIZE = 1024;
        const int DEFAULT_WRITE_BUFFER_SIZE = 1024;

        private readonly MemoryStream SendBuffer = new MemoryStream(0);
        private readonly MemoryStream ReceiveBuffer = new MemoryStream(0);

        public class Factory : TTransportFactory
        {
            public override TTransport GetTransport(TTransport trans)
            {
                return new TZLibTransport(trans);
            }
        }

        public TZLibTransport(TTransport transport)
            : base(transport, DEFAULT_WRITE_BUFFER_SIZE)
        {
        }

        private void CompressBuffer()
        {
            using (var output = new MemoryStream(0))
            {
                using (var gzip = new GZipStream(output, CompressionMode.Compress, true))
                {
                    SendBuffer.Position = 0;
                    SendBuffer.CopyTo(gzip);
                }

                Debug.Assert(output.Length < int.MaxValue);
                base.Write(output.GetBuffer(), 0, (int)output.Length);
                SendBuffer.SetLength(0);
            }
        }


        private bool DecompressIntoBuffer()
        {
            // whenever this gets called we should have read all remaining bytes off this buffer
            Debug.Assert(ReceiveBuffer.Length == ReceiveBuffer.Position);
            ReceiveBuffer.SetLength(0);

            const int HEADER_SIZE = 10;
            const int FOOTER_SIZE = 8;

            // This whole construct could be made much simpler (and faster) if the gzip chunks in the original C++ TZLibTransport
            // were prefixed by their data length, unfortunately they are not. So the only other option would be to not use GZipStream
            // and instead implement the whole deflater from scratch to get access to its internals ... any volunteers?
            // See also https://github.com/Microsoft/referencesource/blob/master/System/sys/system/IO/compression/GZipStream.cs
            using (var input = new MemoryStream(0))
            {
                // gzip header
                var bytes = new byte[1024];  
                var count = 0;
                while (count < HEADER_SIZE)
                {
                    count += base.Read(bytes, count, HEADER_SIZE - count);
                    if (count == 0)  // only exit if we get not a single byte
                        return false;
                }
                input.Write(bytes, 0, count);

                // read extra headers according to the flags
                // according to https://tools.ietf.org/html/rfc1952
                var flags = bytes[3];
                var bFTEXT = (flags & 1) != 0;
                var bFHCRC = (flags & 2) != 0;
                var bFEXTRA = (flags & 4) != 0;
                var bFNAME = (flags & 8) != 0;
                var bFCOMMENT = (flags & 16) != 0;

                // XLEN bytes of "extra field"
                if (bFEXTRA)
                {
                    ushort xlen = 0;
                    count = 0;
                    while (count < sizeof(ushort))
                        count += base.Read(bytes, count, sizeof(ushort) - count);
                    input.Write(bytes, 0, count);

                    xlen = (ushort)(bytes[0] + ((int)bytes[1] << 8));
                    count = 0;
                    while (count < xlen)
                    {
                        var chunk = base.Read(bytes, 0, Math.Min(xlen, bytes.Length) - count);
                        count += chunk;
                        input.Write(bytes, 0, chunk);
                    }
                }

                // ...original file name, zero-terminated
                while (bFNAME)
                {
                    if (base.Read(bytes, 0, 1) > 0)
                    {
                        input.Write(bytes, 0, 1);
                        if (bytes[0] == 0)
                            break;
                    }
                }

                // ...file comment, zero-terminated
                while (bFCOMMENT)
                {
                    if (base.Read(bytes, 0, 1) > 0)
                    {
                        input.Write(bytes, 0, 1);
                        if (bytes[0] == 0)
                            break;
                    }
                }

                // preceding CRC16
                // we don't check the value here, simply read it
                if (bFHCRC)
                {
                    count = 0;
                    while (count < sizeof(ushort))
                        count += base.Read(bytes, count, sizeof(ushort) - count);
                    input.Write(bytes, 0, count);
                }

                // read (potentially multiple) compressed blocks
                // the stash holds any temporary data that were already read from the underlying transport, but not yet processed
                using (var stash = new MemoryStream(0))
                { 
                    while (true)
                    {
                        // read a single byte
                        if (stash.Position < stash.Length)
                            stash.Read(bytes, 0, 1);
                        else if (base.Read(bytes, 0, 1) == 0)
                            continue;  // wait for more

                        // add it to the buffered data and try decompressing
                        input.Seek(0, SeekOrigin.End);
                        input.Write(bytes, 0, 1);
                        input.Seek(0, SeekOrigin.Begin);
                        using (var gzip = new GZipStream(input, CompressionMode.Decompress, true))
                        {
                            ReceiveBuffer.SetLength(0);
                            gzip.CopyTo(ReceiveBuffer);
                        }

                        // if we got nothing, we need more data
                        if (ReceiveBuffer.Length > 0)
                        {
                            // we also need to read the gzip footer off the wire, otherwise any
                            // subsequent reads will run into issues like "invalid magic byte" etc.
                            count = 0;
                            if (stash.Position < stash.Length)
                                count += stash.Read(bytes, count, FOOTER_SIZE);
                            while (count < FOOTER_SIZE)
                                count += base.Read(bytes, count, FOOTER_SIZE - count);

                            // Check CRC and/or ISIZE fields to make sure we really consumed *all* compressed blocks.
                            // Unfortunately, GZipStream does not provide much assistance with this, as it *always* eats the whole input
                            uint crc32 = (uint)bytes[0] + ((uint)bytes[1] << 8) + ((uint)bytes[2] << 16) + ((uint)bytes[3] << 24);
                            uint isize = (uint)bytes[4] + ((uint)bytes[5] << 8) + ((uint)bytes[6] << 16) + ((uint)bytes[7] << 24);
                            if (isize == (ReceiveBuffer.Length % 0x100000000))
                            {
                                // TODO: check crc32
                                ReceiveBuffer.Position = 0;
                                return true;
                            }

                            // otherwise put the data into the stash and try again
                            stash.Seek(0, SeekOrigin.Begin);
                            stash.Write(bytes, 0, count);
                            stash.Seek(0, SeekOrigin.Begin);
                        }
                    }
                }
            }
        }


        public override int Read(byte[] buf, int off, int len)
        {
            CheckNotDisposed();
            ValidateBufferArgs(buf, off, len);
            if (!IsOpen)
                throw new TTransportException(TTransportException.ExceptionType.NotOpen);

            int read = 0;
            while (read < len)
            {
                var buffered = ReceiveBuffer.Length - ReceiveBuffer.Position;
                if (buffered > 0)
                {
                    var count = Math.Min(len-read, (int)Math.Min(int.MaxValue, buffered));
                    ReceiveBuffer.Read(buf, off, count);
                    off += count;
                    read += count;
                }
                else if( !DecompressIntoBuffer())
                {
                    break;
                }
            }

            return read;
        }

        // WRITING STRATEGY
        //
        // We buffer up small writes before sending them to zlib, so our logic is:
        // - Is the write big?
        //   - Send the buffer to zlib.
        //   - Send this data to zlib.
        // - Is the write small?
        //   - Is there insufficient space in the buffer for it?
        //     - Send the buffer to zlib.
        //   - Copy the data to the buffer.
        //
        // We have two buffers for writing also: the uncompressed buffer (mentioned
        // above) and the compressed buffer.  When sending data to zlib we loop over
        // the following until the source (uncompressed buffer or big write) is empty:
        // - Is there no more space in the compressed buffer?
        //   - Write the compressed buffer to the underlying transport.
        // - Deflate from the source into the compressed buffer.

        public override void Write(byte[] buf, int off, int len)
        {
            CheckNotDisposed();
            ValidateBufferArgs(buf, off, len);
            if (!IsOpen)
                throw new TTransportException(TTransportException.ExceptionType.NotOpen);

            if (len > MIN_DIRECT_DEFLATE_SIZE)
            {
                if (SendBuffer.Length > 0)
                    CompressBuffer();
                SendBuffer.Write(buf, off, len);
                CompressBuffer();
            }
            else
            {
                if ((SendBuffer.Length + len) > MAX_UNCOMPRESSED_BUFFER_SIZE)
                    CompressBuffer();
                SendBuffer.Write(buf, off, len);
            }
        }

        public override void Flush()
        {
            CheckNotDisposed();
            if (!IsOpen)
                throw new TTransportException(TTransportException.ExceptionType.NotOpen);

            if (SendBuffer.Length > 0)
                CompressBuffer();

            base.Flush();
        }

        #region " IDisposable Support "

        // IDisposable
        protected override void Dispose(bool disposing)
        {
            if (!_IsDisposed)
            {
                if (disposing)
                {
                    if (SendBuffer != null)
                        SendBuffer.Dispose();
                    if (ReceiveBuffer != null)
                        ReceiveBuffer.Dispose();
                }
            }
            base.Dispose(disposing);
        }
        #endregion
    }
}
