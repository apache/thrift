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

# The .NET SDK image provides the dotnet CLI used by lib/netstd. The C++ build
# uses the Visual Studio 2022 Build Tools installed below; the one thing it
# still needs from a .NET Framework SDK is the 4.7.2 targeting pack, added as
# a Build Tools component below, for the C++/CLI test project.
FROM mcr.microsoft.com/dotnet/sdk:10.0-windowsservercore-ltsc2025

# Restore the default Windows shell for correct batch processing below.
SHELL ["cmd", "/S", "/C"]

# Notes on the RUN commands below:
# - PATH is extended in the machine-scope registry key through PowerShell.
#   SETX would re-embed the expanded PATH on every call and truncates silently
#   at 1024 characters.
# - cmd treats everything after "IF ... " and "FOR ... DO" as the body of that
#   command, including "&& next-command". Cleanup loops are therefore wrapped
#   in parentheses and placed after the environment writes.

RUN IF NOT EXIST C:\TEMP MKDIR C:\TEMP

# Install Build Tools excluding workloads and components with known issues.
RUN curl.exe -fsSL -o C:\TEMP\vs_buildtools.exe https://download.visualstudio.microsoft.com/download/pr/e8db5368-d542-4208-ab91-ea2ac11f00b8/cfbe96493247c4c4280ea7e765ad427f89a76a1507b1144c7edb319069369387/vs_BuildTools.exe && `
    powershell -NoProfile -Command "if ((Get-FileHash 'C:\TEMP\vs_buildtools.exe' -Algorithm SHA256).Hash.ToLower() -ne 'cfbe96493247c4c4280ea7e765ad427f89a76a1507b1144c7edb319069369387') { exit 1 }" && `
    (C:\TEMP\vs_buildtools.exe --quiet --wait --norestart --nocache `
    --installPath C:\BuildTools `
    --add Microsoft.VisualStudio.Component.VC.CoreBuildTools `
    --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 `
    --add Microsoft.VisualStudio.Component.Windows10SDK.19041 `
    --add Microsoft.Net.Component.4.7.2.TargetingPack `
    || IF "%ERRORLEVEL%"=="3010" EXIT 0) && `
    DEL /F /Q C:\TEMP\vs_buildtools.exe && `
    (IF EXIST "C:\ProgramData\Package Cache" RMDIR /S /Q "C:\ProgramData\Package Cache") && `
    (IF EXIST C:\Windows\SoftwareDistribution\Download RMDIR /S /Q C:\Windows\SoftwareDistribution\Download)

# Install CMake
RUN curl.exe -fsSL -o C:\TEMP\cmake.msi https://github.com/Kitware/CMake/releases/download/v4.1.2/cmake-4.1.2-windows-x86_64.msi && `
    powershell -NoProfile -Command "if ((Get-FileHash 'C:\TEMP\cmake.msi' -Algorithm SHA256).Hash.ToLower() -ne 'a10fc2527ec0727a5913acd310cddafdba1e5b4ca765ca23cc6a53c55eebb7c1') { exit 1 }" && `
    msiexec.exe /i C:\TEMP\cmake.msi /qn && `
    powershell -NoProfile -Command "[Environment]::SetEnvironmentVariable('PATH', [Environment]::GetEnvironmentVariable('PATH', 'Machine') + ';C:\Program Files\CMake\bin', 'Machine')" && `
    DEL /F /Q C:\TEMP\cmake.msi && `
    (FOR %D IN (doc man) DO @IF EXIST "C:\Program Files\CMake\%D" RMDIR /S /Q "C:\Program Files\CMake\%D")

# Install boost (for the thrift runtime library build)
# Thrift builds Release with /MD and links Boost dynamically, so the debug
# (-gd-) and static-runtime (-s-, -sgd-) variants are removed after install.
ENV BOOST_ROOT=C:\Libraries\boost_1_88_0
ENV BOOST_LIBRARYDIR=C:\Libraries\boost_1_88_0\lib64-msvc-14.3
RUN curl.exe -fsSL -o C:\TEMP\boost.exe https://boost.teeks99.com/bin/1.88.0/boost_1_88_0-msvc-14.3-64.exe && `
    powershell -NoProfile -Command "if ((Get-FileHash 'C:\TEMP\boost.exe' -Algorithm SHA256).Hash.ToLower() -ne 'bff575f21343f602c7b6c3ad3883a28053a1a50cde2153102d30479f85911738') { exit 1 }" && `
    C:\TEMP\boost.exe /DIR=%BOOST_ROOT% /SILENT && `
    DEL /F /Q C:\TEMP\boost.exe && `
    powershell -NoProfile -Command "[Environment]::SetEnvironmentVariable('PATH', '%BOOST_LIBRARYDIR%;' + [Environment]::GetEnvironmentVariable('PATH', 'Machine'), 'Machine')" && `
    DEL /F /Q "%BOOST_LIBRARYDIR%\*-mt-gd-*" "%BOOST_LIBRARYDIR%\*-mt-s-*" "%BOOST_LIBRARYDIR%\*-mt-sgd-*" && `
    (FOR %D IN (doc libs tools status more) DO @IF EXIST "%BOOST_ROOT%\%D" RMDIR /S /Q "%BOOST_ROOT%\%D")

# Install chocolatey
ENV CHOCO_DIR=C:\ProgramData\chocolatey
RUN @"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" `
    -NoProfile -InputFormat None -ExecutionPolicy Bypass -Command `
    "$env:chocolateyVersion = '2.7.4'; [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 ; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))" && `
    powershell -NoProfile -Command "[Environment]::SetEnvironmentVariable('PATH', [Environment]::GetEnvironmentVariable('PATH', 'Machine') + ';%CHOCO_DIR%\bin', 'Machine')"

# Install winflexbison (for the thrift compiler build) and 7zip (used by the
# libevent and zlib build scripts). curl.exe ships with Windows.
RUN choco install winflexbison3 --version=2.5.24.20210105 -y --no-progress && `
    choco install 7zip --version=26.3.0 -y --no-progress && `
    choco cache remove -y

# Install libevent
COPY appveyor\build-libevent.bat C:\TEMP\build-libevent.bat
ENV LIBEVENT_VERSION=2.1.8
ENV LIBEVENT_ROOT=C:\Libraries\libevent-${LIBEVENT_VERSION}
ENV WIN3P=C:\TEMP\WIN3P
RUN C:\BuildTools\Common7\Tools\VsDevCmd.bat -arch=x64 -host_arch=x64 && `
    MKDIR C:\TEMP\WIN3P && `
    C:\TEMP\build-libevent.bat && `
    MKDIR %LIBEVENT_ROOT% && `
    MOVE C:\TEMP\WIN3P\libevent-%LIBEVENT_VERSION%-stable\include %LIBEVENT_ROOT% && `
    MOVE C:\TEMP\WIN3P\libevent-%LIBEVENT_VERSION%-stable\lib %LIBEVENT_ROOT% && `
    RMDIR /S /Q C:\TEMP\WIN3P

# Install zlib
COPY appveyor\build-zlib.bat C:\TEMP\build-zlib.bat
ENV ZLIB_VERSION=1.3.1
ENV ZLIB_ROOT=C:\Libraries\zlib-${ZLIB_VERSION}
RUN C:\BuildTools\Common7\Tools\VsDevCmd.bat -arch=x64 -host_arch=x64 && `
    MKDIR C:\TEMP\WIN3P && `
    C:\TEMP\build-zlib.bat && `
    MOVE C:\TEMP\WIN3P\zlib-inst %ZLIB_ROOT% && `
    RMDIR /S /Q C:\TEMP\WIN3P && `
    powershell -NoProfile -Command "[Environment]::SetEnvironmentVariable('PATH', '%ZLIB_ROOT%\bin;' + [Environment]::GetEnvironmentVariable('PATH', 'Machine'), 'Machine')"

# Install OpenSSL
ENV OPENSSL_ROOT="C:\Program Files\OpenSSL-Win64"
RUN curl.exe -fsSL -o C:\TEMP\openssl.exe https://slproweb.com/download/Win64OpenSSL-3_6_4.exe && `
    powershell -NoProfile -Command "if ((Get-FileHash 'C:\TEMP\openssl.exe' -Algorithm SHA256).Hash.ToLower() -ne 'b33a9be5307fc42e98e03604ebbdb1fcee89e24c91af6009e965bdcdbc63449c') { exit 1 }" && `
    C:\TEMP\openssl.exe /silent && `
    DEL /F /Q C:\TEMP\openssl.exe && `
    powershell -NoProfile -Command "[Environment]::SetEnvironmentVariable('PATH', '%OPENSSL_ROOT%\bin;' + [Environment]::GetEnvironmentVariable('PATH', 'Machine'), 'Machine')" && `
    (FOR %D IN (tests html) DO @IF EXIST "%OPENSSL_ROOT%\%D" RMDIR /S /Q "%OPENSSL_ROOT%\%D") && `
    (FOR %D IN (MDd MTd) DO @IF EXIST "%OPENSSL_ROOT%\lib\VC\x64\%D" RMDIR /S /Q "%OPENSSL_ROOT%\lib\VC\x64\%D")

# Install python3
RUN choco install python314 --version=3.14.7 -y --no-progress --install-arguments "'Include_doc=0 Include_launcher=0 Include_tcltk=0 Include_test=0'" && `
    choco cache remove -y && `
    C:\Python314\python.exe -m pip install --disable-pip-version-check --no-cache-dir setuptools && `
    (IF EXIST "C:\ProgramData\Package Cache" RMDIR /S /Q "C:\ProgramData\Package Cache") && `
    (IF EXIST "%CHOCO_DIR%\lib\python314" RMDIR /S /Q "%CHOCO_DIR%\lib\python314")

# Install Node.js (LTS line)
RUN choco install nodejs-lts --version=24.20.0 -y --no-progress && `
    choco cache remove -y && `
    (IF EXIST "C:\ProgramData\Package Cache" RMDIR /S /Q "C:\ProgramData\Package Cache") && `
    (IF EXIST "%CHOCO_DIR%\lib\nodejs-lts" RMDIR /S /Q "%CHOCO_DIR%\lib\nodejs-lts")

# Start developer command prompt with any other commands specified.
ENTRYPOINT C:\BuildTools\Common7\Tools\VsDevCmd.bat -arch=x64 -host_arch=x64 &&

# Default to PowerShell if no other command specified.
CMD powershell.exe -NoLogo -ExecutionPolicy Bypass
