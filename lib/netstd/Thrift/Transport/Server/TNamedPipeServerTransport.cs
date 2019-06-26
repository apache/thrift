// Licensed to the Apache Software Foundation(ASF) under one
// or more contributor license agreements.See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

using Microsoft.Win32.SafeHandles;
using System;
using System.IO.Pipes;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;
using System.ComponentModel;
using System.Security.AccessControl;
using System.Security.Principal;

namespace Thrift.Transport.Server
{
    // ReSharper disable once InconsistentNaming
    public class TNamedPipeServerTransport : TServerTransport
    {
        /// <summary>
        ///     This is the address of the Pipe on the localhost.
        /// </summary>
        private readonly string _pipeAddress;
        private bool _asyncMode = true;
        private volatile bool _isPending = true;
        private NamedPipeServerStream _stream = null;

        public TNamedPipeServerTransport(string pipeAddress)
        {
            _pipeAddress = pipeAddress;
        }

        public override void Listen()
        {
            // nothing to do here
        }

        public override void Close()
        {
            if (_stream != null)
            {
                try
                {
                    if (_stream.IsConnected)
                        _stream.Disconnect();
                    _stream.Dispose();
                }
                finally
                {
                    _stream = null;
                    _isPending = false;
                }
            }
        }

        public override bool IsClientPending()
        {
            return _isPending;
        }

        private void EnsurePipeInstance()
        {
            if (_stream == null)
            {
                const PipeDirection direction = PipeDirection.InOut;
                const int maxconn = NamedPipeServerStream.MaxAllowedServerInstances;
                const PipeTransmissionMode mode = PipeTransmissionMode.Byte;
                const int inbuf = 4096;
                const int outbuf = 4096;
                var options = _asyncMode ? PipeOptions.Asynchronous : PipeOptions.None;


                // TODO: "CreatePipeNative" ist only a workaround, and there are have basically two possible outcomes:
                // - once NamedPipeServerStream() gets a CTOR that supports pipesec, remove CreatePipeNative()
                // - if 31190 gets resolved before, use _stream.SetAccessControl(pipesec) instead of CreatePipeNative()
                // EITHER WAY,
                // - if CreatePipeNative() finally gets removed, also remove "allow unsafe code" from the project settings

                try
                {
                    var handle = CreatePipeNative(_pipeAddress, inbuf, outbuf);
                    if( (handle != null) && (!handle.IsInvalid))
                        _stream = new NamedPipeServerStream(PipeDirection.InOut, _asyncMode, false, handle);
                    else
                        _stream = new NamedPipeServerStream(_pipeAddress, direction, maxconn, mode, options, inbuf, outbuf/*, pipesec*/);
                }
                catch (NotImplementedException) // Mono still does not support async, fallback to sync
                {
                    if (_asyncMode)
                    {
                        options &= (~PipeOptions.Asynchronous);
                        _stream = new NamedPipeServerStream(_pipeAddress, direction, maxconn, mode, options, inbuf, outbuf);
                        _asyncMode = false;
                    }
                    else
                    {
                        throw;
                    }
                }
            }
        }


        #region CreatePipeNative workaround


        [StructLayout(LayoutKind.Sequential)]
        internal class SECURITY_ATTRIBUTES
        {
            internal int nLength = 0;
            internal IntPtr lpSecurityDescriptor = IntPtr.Zero;
            internal int bInheritHandle = 0;
        }


        private const string Kernel32 = "kernel32.dll";

        [DllImport(Kernel32, SetLastError = true)]
        internal static extern IntPtr CreateNamedPipe(
            string lpName, uint dwOpenMode, uint dwPipeMode,
            uint nMaxInstances, uint nOutBufferSize, uint nInBufferSize, uint nDefaultTimeOut,
            SECURITY_ATTRIBUTES pipeSecurityDescriptor
            );



        // Workaround: create the pipe via API call
        // we have to do it this way, since NamedPipeServerStream() for netstd still lacks a few CTORs
        // and _stream.SetAccessControl(pipesec); only keeps throwing ACCESS_DENIED errors at us
        // References:
        // - https://github.com/dotnet/corefx/issues/30170 (closed, continued in 31190)
        // - https://github.com/dotnet/corefx/issues/31190 System.IO.Pipes.AccessControl package does not work
        // - https://github.com/dotnet/corefx/issues/24040 NamedPipeServerStream: Provide support for WRITE_DAC
        // - https://github.com/dotnet/corefx/issues/34400 Have a mechanism for lower privileged user to connect to a privileged user's pipe
        private SafePipeHandle CreatePipeNative(string name, int inbuf, int outbuf)
        {
            if (Environment.OSVersion.Platform != PlatformID.Win32NT)
                return null; // Windows only

            var pinningHandle = new GCHandle();
            try
            {
                // owner gets full access, everyone else read/write
                var pipesec = new PipeSecurity();
                using (var currentIdentity = WindowsIdentity.GetCurrent())
                {
                    var sidOwner = currentIdentity.Owner;
                    var sidWorld = new SecurityIdentifier(WellKnownSidType.WorldSid, null);

                    pipesec.SetOwner(sidOwner);
                    pipesec.AddAccessRule(new PipeAccessRule(sidOwner, PipeAccessRights.FullControl, AccessControlType.Allow));
                    pipesec.AddAccessRule(new PipeAccessRule(sidWorld, PipeAccessRights.ReadWrite, AccessControlType.Allow));
                }

                // create a security descriptor and assign it to the security attribs
                var secAttrs = new SECURITY_ATTRIBUTES();
                byte[] sdBytes = pipesec.GetSecurityDescriptorBinaryForm();
                pinningHandle = GCHandle.Alloc(sdBytes, GCHandleType.Pinned);
                unsafe {
                    fixed (byte* pSD = sdBytes) {
                        secAttrs.lpSecurityDescriptor = (IntPtr)pSD;
                    }
                }

                // a bunch of constants we will need shortly
                const int PIPE_ACCESS_DUPLEX = 0x00000003;
                const int FILE_FLAG_OVERLAPPED = 0x40000000;
                const int WRITE_DAC = 0x00040000;
                const int PIPE_TYPE_BYTE = 0x00000000;
                const int PIPE_READMODE_BYTE = 0x00000000;
                const int PIPE_UNLIMITED_INSTANCES = 255;

                // create the pipe via API call
                var rawHandle = CreateNamedPipe(
                    @"\\.\pipe\" + name,
                    PIPE_ACCESS_DUPLEX | FILE_FLAG_OVERLAPPED | WRITE_DAC,
                    PIPE_TYPE_BYTE | PIPE_READMODE_BYTE,
                    PIPE_UNLIMITED_INSTANCES, (uint)inbuf, (uint)outbuf,
                    5 * 1000,
                    secAttrs
                    );

                // make a SafePipeHandle() from it
                var handle = new SafePipeHandle(rawHandle, true);
                if (handle.IsInvalid)
                    throw new Win32Exception(Marshal.GetLastWin32Error());

                // return it (to be packaged)
                return handle;
            }
            finally
            {
                if (pinningHandle.IsAllocated)
                    pinningHandle.Free();
            }
        }

        #endregion

        protected override async ValueTask<TTransport> AcceptImplementationAsync(CancellationToken cancellationToken)
        {
            try
            {
                EnsurePipeInstance();

                await _stream.WaitForConnectionAsync(cancellationToken);

                var trans = new ServerTransport(_stream);
                _stream = null; // pass ownership to ServerTransport

                //_isPending = false;

                return trans;
            }
            catch (TTransportException)
            {
                Close();
                throw;
            }
            catch (Exception e)
            {
                Close();
                throw new TTransportException(TTransportException.ExceptionType.NotOpen, e.Message);
            }
        }

        private class ServerTransport : TTransport
        {
            private readonly NamedPipeServerStream PipeStream;

            public ServerTransport(NamedPipeServerStream stream)
            {
                PipeStream = stream;
            }

            public override bool IsOpen => PipeStream != null && PipeStream.IsConnected;

            public override async Task OpenAsync(CancellationToken cancellationToken)
            {
                if (cancellationToken.IsCancellationRequested)
                {
                    await Task.FromCanceled(cancellationToken);
                }
            }

            public override void Close()
            {
                PipeStream?.Dispose();
            }

            public override async ValueTask<int> ReadAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
            {
                if (PipeStream == null)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen);
                }

                return await PipeStream.ReadAsync(buffer, offset, length, cancellationToken);
            }

            public override async Task WriteAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
            {
                if (PipeStream == null)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen);
                }

                // if necessary, send the data in chunks
                // there's a system limit around 0x10000 bytes that we hit otherwise
                // MSDN: "Pipe write operations across a network are limited to 65,535 bytes per write. For more information regarding pipes, see the Remarks section."
                var nBytes = Math.Min(15 * 4096, length); // 16 would exceed the limit
                while (nBytes > 0)
                {
                    await PipeStream.WriteAsync(buffer, offset, nBytes, cancellationToken);
                    offset += nBytes;
                    length -= nBytes;
                    nBytes = Math.Min(nBytes, length);
                }
            }

            public override async Task FlushAsync(CancellationToken cancellationToken)
            {
                if (cancellationToken.IsCancellationRequested)
                {
                    await Task.FromCanceled(cancellationToken);
                }
            }

            protected override void Dispose(bool disposing)
            {
                PipeStream?.Dispose();
            }
        }
    }
}
