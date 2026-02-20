# escape=`
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

FROM mcr.microsoft.com/dotnet/framework/sdk:4.8.1-windowsservercore-ltsc2025

# Restore the default Windows shell for correct batch processing below.
SHELL ["cmd", "/S", "/C"]

# Install Build Tools excluding workloads and components with known issues.
ADD --checksum=sha256:93bf06959261a301aca6406d058ade08f34ae83ca458fe3ed0689aa2105d9ccc https://aka.ms/vs/17/release/vs_BuildTools.exe C:\TEMP\vs_buildtools.exe
RUN C:\TEMP\vs_buildtools.exe --quiet --wait --norestart --nocache `
    --installPath C:\BuildTools `
    --add Microsoft.VisualStudio.Component.VC.CoreBuildTools `
    --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 `
    --add Microsoft.VisualStudio.Component.Windows10SDK.19041 `
 || IF "%ERRORLEVEL%"=="3010" EXIT 0
RUN DEL C:\TEMP\vs_buildtools.exe

# Install CMake
ADD --checksum=sha256:a10fc2527ec0727a5913acd310cddafba1e5b4ca765ca23cc6a53c55eebb7c1 https://github.com/Kitware/CMake/releases/download/v4.1.2/cmake-4.1.2-windows-x86_64.msi C:\TEMP\cmake.msi
RUN msiexec.exe /i C:\TEMP\cmake.msi /qn && `
    SETX PATH "%PATH%;C:\Program Files\CMake\bin" && `
    DEL C:\TEMP\cmake.msi

# Install boost (for the thrift runtime library build)
ADD --checksum=sha256:bff575f21343f602c7b6c3ad3883a28053a1a50cde2153102d30479f85911738 https://boost.teeks99.com/bin/1.88.0/boost_1_88_0-msvc-14.3-64.exe C:\TEMP\boost.exe
ENV BOOST_ROOT=C:\Libraries\boost_1_88_0
RUN C:\TEMP\boost.exe /DIR=%BOOST_ROOT% /SILENT && `
    DEL C:\TEMP\boost.exe && `
    SETX BOOST_LIBRARYDIR "%BOOST_ROOT%\lib64-msvc-14.3" && `
    SETX PATH "%BOOST_LIBRARYDIR%;%PATH%"

# Install chocolatey
RUN @"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" `
    -NoProfile -InputFormat None -ExecutionPolicy Bypass -Command `
    "[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 ; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))" `
    && SETX PATH "%PATH%;%ALLUSERSPROFILE%\chocolatey\bin"

# Install winflexbison (for the thrift compiler build)
RUN choco install winflexbison3 -y

# Install 7zip and curl (used by the libevent and zlib build scripts)
RUN choco install 7zip curl -y

# Install libevent
COPY appveyor\build-libevent.bat C:\TEMP\build-libevent.bat
ENV LIBEVENT_VERSION=2.1.8
ENV WIN3P=C:\TEMP\WIN3P
RUN C:\BuildTools\Common7\Tools\VsDevCmd.bat -arch=x64 -host_arch=x64 && `
    MKDIR C:\TEMP\WIN3P && `
    C:\TEMP\build-libevent.bat && `
    MKDIR C:\Libraries\libevent-%LIBEVENT_VERSION% && `
    MOVE C:\TEMP\WIN3P\libevent-%LIBEVENT_VERSION%-stable\include C:\Libraries\libevent-%LIBEVENT_VERSION% && `
    MOVE C:\TEMP\WIN3P\libevent-%LIBEVENT_VERSION%-stable\lib C:\Libraries\libevent-%LIBEVENT_VERSION% && `
    RMDIR /S /Q C:\TEMP\WIN3P && `
    SETX LIBEVENT_ROOT "C:\Libraries\libevent-%LIBEVENT_VERSION%"

# Install zlib
COPY appveyor\build-zlib.bat C:\TEMP\build-zlib.bat
ENV ZLIB_VERSION=1.3.1
ENV WIN3P=C:\TEMP\WIN3P
RUN C:\BuildTools\Common7\Tools\VsDevCmd.bat -arch=x64 -host_arch=x64 && `
    MKDIR C:\TEMP\WIN3P && `
    C:\TEMP\build-zlib.bat && `
    MOVE C:\TEMP\WIN3P\zlib-inst C:\Libraries\zlib-%ZLIB_VERSION% && `
    RMDIR /S /Q C:\TEMP\WIN3P && `
    SETX ZLIB_ROOT "C:\Libraries\zlib-%ZLIB_VERSION%" && `
    SETX PATH "%ZLIB_ROOT%\bin;%PATH%"

# Install OpenSSL 3.6.1
ADD --checksum=sha256:8ce11c409adbd177ca627115bc697c3b66c414bb36175ebb375341eb426894a8 https://slproweb.com/download/Win64OpenSSL-3_6_1.exe C:\TEMP\openssl.exe
RUN C:\TEMP\openssl.exe /silent && `
    DEL C:\TEMP\openssl.exe && `
    SETX OPENSSL_ROOT "C:\OpenSSL-Win64" && `
    SETX PATH "%OPENSSL_ROOT%\bin;%PATH%"

# Install java
RUN choco install jdk8 -y

# Install python3
RUN choco install python3 -y
RUN pip install setuptools

# Install Adobe Flex 4.6 SDK and set FLEX_HOME so it can be found
ADD --checksum=sha256:622b63f29de44600ff8d4231174a70fcb3085812c0e146a42e91877ca8b46798 http://download.macromedia.com/pub/flex/sdk/flex_sdk_4.6.zip C:\Adobe\Flex\SDK\4.6\SDK.zip
RUN CD C:\Adobe\Flex\SDK\4.6 && `
    7z x SDK.zip && `
    DEL SDK.zip && `
    SETX FLEX_HOME "C:\Adobe\Flex\SDK\4.6"

RUN choco install nodejs -y

# Start developer command prompt with any other commands specified.
ENTRYPOINT C:\BuildTools\Common7\Tools\VsDevCmd.bat -arch=x64 -host_arch=x64 &&

# Default to PowerShell if no other command specified.
CMD powershell.exe -NoLogo -ExecutionPolicy Bypass
