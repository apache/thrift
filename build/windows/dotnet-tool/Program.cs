/*
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
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;

namespace Apache.Thrift.Compiler
{
    /// <summary>
    /// Launcher for the Thrift compiler bundled in this package.
    /// </summary>
    /// <remarks>
    /// A .NET tool package has to have a managed entry point, and the Thrift
    /// compiler is a native executable, so this hands over to it: arguments,
    /// the standard streams and the exit code all pass straight through, and
    /// nothing else is interpreted here.
    /// </remarks>
    internal static class Program
    {
        internal const string CompilerFileName = "thrift.exe";

        private static int Main(string[] args)
        {
            if (!RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            {
                // NuGet installs a tool package on any platform, so say what
                // is wrong rather than failing to start a Windows executable.
                Console.Error.WriteLine(
                    "This package contains the Windows build of the Apache Thrift compiler, " +
                    $"and cannot run on {RuntimeInformation.RuntimeIdentifier}.");
                Console.Error.WriteLine(
                    "See https://thrift.apache.org/download for the other ways to install it.");
                return 1;
            }

            // AppContext.BaseDirectory rather than Assembly.Location: the
            // latter is empty when the host is published as a single file.
            var compiler = Path.Combine(AppContext.BaseDirectory, CompilerFileName);
            if (!File.Exists(compiler))
            {
                Console.Error.WriteLine($"The bundled Thrift compiler is missing: {compiler}");
                return 1;
            }

            var startInfo = new ProcessStartInfo(compiler) { UseShellExecute = false };
            foreach (var arg in args)
            {
                // ArgumentList quotes each argument itself, so an argument
                // holding spaces or quotes survives.
                startInfo.ArgumentList.Add(arg);
            }

            Process? process;
            try
            {
                process = Process.Start(startInfo);
            }
            catch (Win32Exception ex)
            {
                // The compiler links the shared runtime, so it will not start
                // without the Visual C++ redistributable. That is by far the
                // likeliest reason to land here, and the error Windows gives
                // on its own does not say so.
                Console.Error.WriteLine($"Could not start {compiler}: {ex.Message}");
                Console.Error.WriteLine(
                    "The Thrift compiler needs the Microsoft Visual C++ Redistributable (x64). " +
                    "Install it from https://aka.ms/vs/17/release/vc_redist.x64.exe and try again.");
                return 1;
            }

            if (process is null)
            {
                Console.Error.WriteLine($"Could not start {compiler}.");
                return 1;
            }

            using (process)
            {
                process.WaitForExit();
                return process.ExitCode;
            }
        }
    }
}
