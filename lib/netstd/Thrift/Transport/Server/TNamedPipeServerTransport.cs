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
using System.Collections.Generic;
using System.IO;
using System.Diagnostics;

#pragma warning disable IDE0079 // net20 - unneeded suppression
#pragma warning disable IDE0028 // net8 - simplified collection init 
#pragma warning disable IDE0300 // net8 - simplified collection init 
#pragma warning disable IDE0290 // net8 - primary CTOR
#pragma warning disable SYSLIB1054 // net8 - use LibraryImport attribute
#pragma warning disable CS1998  // async no await

namespace Thrift.Transport.Server
{
    [Flags]
    public enum NamedPipeServerFlags
    {
        None = 0x00,
        OnlyLocalClients = 0x01,
    };


    // ReSharper disable once InconsistentNaming
    public class TNamedPipeServerTransport : TServerTransport
    {
        // to manage incoming connections, we set up a task for each stream to listen on
        private struct TaskStreamPair
        {
            public readonly NamedPipeServerStream Stream;
            public Task Task;

            public TaskStreamPair(NamedPipeServerStream stream, Task task)
            {
                Stream = stream;
                Task = task;    
            }
        }

        /// <summary>
        ///     This is the address of the Pipe on the localhost.
        /// </summary>
        private readonly string _pipeAddress;
        private bool _asyncMode = true;
        private volatile bool _isPending = true;
        private readonly List<TaskStreamPair> _streams = new List<TaskStreamPair>();
        private readonly bool _onlyLocalClients = false;  // compatibility default
        private readonly byte _numListenPipes = 1;  // compatibility default

        public TNamedPipeServerTransport(string pipeAddress, TConfiguration config, NamedPipeServerFlags flags, int numListenPipes)
            : base(config)
        {
            if ((numListenPipes < 1) || (numListenPipes > 254))
                throw new ArgumentOutOfRangeException(nameof(numListenPipes), "Value must be in the range of [1..254]");

            _pipeAddress = pipeAddress;
            _onlyLocalClients = flags.HasFlag(NamedPipeServerFlags.OnlyLocalClients);
            _numListenPipes = (byte)numListenPipes;
        }


        public override bool IsOpen() {
            return true;
        }

        public override void Listen()
        {
            // nothing to do here
        }

        private static void Close(NamedPipeServerStream pipe)
        {
            if (pipe != null)
            {
                try
                {
                    if (pipe.IsConnected)
                        pipe.Disconnect();
                }
                finally
                {
                    pipe.Dispose();
                }
            }
        }

        public override void Close()
        {
            try
            {
                if (_streams != null)
                {
                    while(_streams.Count > 0)
                    {
                        Close(_streams[0].Stream);
                        _streams.RemoveAt(0);
                    }
                }
            }
            finally
            {
                _streams.Clear();
                _isPending = false;
            }
        }

        public override bool IsClientPending()
        {
            return _isPending;
        }

        private void EnsurePipeInstances()
        {
            // set up a pool for accepting multiple calls when in multithread mode
            // once connected, we hand that stream over to the processor and create a fresh one
            try
            {
                while (_streams.Count < _numListenPipes)
                    _streams.Add(CreatePipeInstance());
            }
            catch
            {
                // we might not be able to create all requested instances, e.g. due to some existing instances already processing calls
                // if we have at least one pipe to listen on -> Good Enough(tm) 
                if (_streams.Count < 1)
                    throw;  // no pipes is really bad
            }
        }

        private TaskStreamPair CreatePipeInstance()
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

            NamedPipeServerStream instance;
            try
            {
                var handle = CreatePipeNative(_pipeAddress, inbuf, outbuf, _onlyLocalClients);
                if ((handle != null) && (!handle.IsInvalid))
                {
                    instance = new NamedPipeServerStream(PipeDirection.InOut, _asyncMode, false, handle);
                    handle = null; // we don't own it any longer
                }
                else
                {
                    handle?.Dispose();
                    instance = new NamedPipeServerStream(_pipeAddress, direction, maxconn, mode, options, inbuf, outbuf/*, pipesec*/);
                }
            }
            catch (NotImplementedException) // Mono still does not support async, fallback to sync
            {
                if (_asyncMode)
                {
                    options &= (~PipeOptions.Asynchronous);
                    instance = new NamedPipeServerStream(_pipeAddress, direction, maxconn, mode, options, inbuf, outbuf);
                    _asyncMode = false;
                }
                else
                {
                    throw;
                }
            }

            // the task gets added later
            return new TaskStreamPair( instance, null);
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

        [DllImport(Kernel32, SetLastError = true, CharSet = CharSet.Unicode)]
        internal static extern IntPtr CreateNamedPipe(
            string lpName, uint dwOpenMode, uint dwPipeMode,
            uint nMaxInstances, uint nOutBufferSize, uint nInBufferSize, uint nDefaultTimeOut,
            SECURITY_ATTRIBUTES pipeSecurityDescriptor
            );



        // Workaround: create the pipe via API call
        // we have to do it this way, since NamedPipeServerStream() for netstd still lacks a few CTORs and/or arguments
        // and _stream.SetAccessControl(pipesec); only keeps throwing ACCESS_DENIED errors at us
        // References:
        // - https://github.com/dotnet/corefx/issues/30170 (closed, continued in 31190)
        // - https://github.com/dotnet/corefx/issues/31190 System.IO.Pipes.AccessControl package does not work
        // - https://github.com/dotnet/corefx/issues/24040 NamedPipeServerStream: Provide support for WRITE_DAC
        // - https://github.com/dotnet/corefx/issues/34400 Have a mechanism for lower privileged user to connect to a privileged user's pipe
        private static SafePipeHandle CreatePipeNative(string name, int inbuf, int outbuf, bool OnlyLocalClients)
        {
            if (! RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
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
                const uint PIPE_ACCESS_DUPLEX = 0x00000003;
                const uint FILE_FLAG_OVERLAPPED = 0x40000000;
                const uint WRITE_DAC = 0x00040000;
                const uint PIPE_TYPE_BYTE = 0x00000000;
                const uint PIPE_READMODE_BYTE = 0x00000000;
                const uint PIPE_UNLIMITED_INSTANCES = 255;
                const uint PIPE_ACCEPT_REMOTE_CLIENTS = 0x00000000;  // Connections from remote clients can be accepted and checked against the security descriptor for the pipe.
                const uint PIPE_REJECT_REMOTE_CLIENTS = 0x00000008;  // Connections from remote clients are automatically rejected. 

                // any extra flags we want to add
                uint dwPipeModeXtra
                    = (OnlyLocalClients ? PIPE_REJECT_REMOTE_CLIENTS : PIPE_ACCEPT_REMOTE_CLIENTS)
                    ;

                // create the pipe via API call
                var rawHandle = CreateNamedPipe(
                    @"\\.\pipe\" + name,
                    PIPE_ACCESS_DUPLEX | FILE_FLAG_OVERLAPPED | WRITE_DAC,
                    PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | dwPipeModeXtra,
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
                EnsurePipeInstances();

                // fill the list and wait for any task to be completed
                var tasks = new List<Task>();
                for (var i = 0; i < _streams.Count; ++i)
                {
                    if (_streams[i].Task == null)
                    {
                        var pair = _streams[i];
                        pair.Task = Task.Run(async () => await pair.Stream.WaitForConnectionAsync(cancellationToken), cancellationToken);
                        _streams[i] = pair;
                    }

                    tasks.Add(_streams[i].Task);
                }

                // there must be an exact mapping between task index and stream index
                Debug.Assert(_streams.Count == tasks.Count);
                #pragma warning disable IDE0305  // see https://github.com/dotnet/roslyn/issues/70656 - yet unsolved
                var index = Task.WaitAny(tasks.ToArray(), cancellationToken);
                #pragma warning restore IDE0305 
                var trans = new ServerTransport(_streams[index].Stream, Configuration);
                _streams.RemoveAt(index); // pass stream ownership to ServerTransport

                return trans;
            }
            catch (TTransportException)
            {
                Close();
                throw;
            }
            catch (TaskCanceledException)
            {
                Close();
                throw;  // let it bubble up
            }
            catch (Exception e)
            {
                Close();
                throw new TTransportException(TTransportException.ExceptionType.NotOpen, e.Message);
            }
        }

        private class ServerTransport : TEndpointTransport
        {
            private NamedPipeServerStream PipeStream;

            public ServerTransport(NamedPipeServerStream stream, TConfiguration config)
                : base(config)
            {
                PipeStream = stream;
            }

            public override bool IsOpen => PipeStream != null && PipeStream.IsConnected;

            public override Task OpenAsync(CancellationToken cancellationToken)
            {
                cancellationToken.ThrowIfCancellationRequested();
                return Task.CompletedTask;
            }

            public override void Close()
            {
                if (PipeStream != null)
                {
                    if (PipeStream.IsConnected)
                        PipeStream.Disconnect();
                    PipeStream.Dispose();
                    PipeStream = null;
                }
            }

            public override async ValueTask<int> ReadAsync(byte[] buffer, int offset, int length, CancellationToken cancellationToken)
            {
                if (PipeStream == null)
                {
                    throw new TTransportException(TTransportException.ExceptionType.NotOpen);
                }

                CheckReadBytesAvailable(length);
#if NET5_0_OR_GREATER
                var numBytes = await PipeStream.ReadAsync(buffer.AsMemory(offset, length), cancellationToken);
#else
                var numBytes = await PipeStream.ReadAsync(buffer, offset, length, cancellationToken);
#endif
                CountConsumedMessageBytes(numBytes);
                return numBytes;
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
#if NET5_0_OR_GREATER
                    await PipeStream.WriteAsync(buffer.AsMemory(offset, nBytes), cancellationToken);
#else
                    await PipeStream.WriteAsync(buffer, offset, nBytes, cancellationToken);
#endif
                    offset += nBytes;
                    length -= nBytes;
                    nBytes = Math.Min(nBytes, length);
                }
            }

            public override async Task FlushAsync(CancellationToken cancellationToken)
            {
                await PipeStream.FlushAsync(cancellationToken);
                ResetConsumedMessageSize();
            }

            protected override void Dispose(bool disposing)
            {
                if (disposing)
                {
                    if (PipeStream != null)
                    {
                        if (PipeStream.IsConnected)
                            PipeStream.Disconnect();
                        PipeStream.Dispose();
                        PipeStream = null;
                    }
                }
            }
        }
    }
}
