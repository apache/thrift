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

FROM microsoft/dotnet-framework:4.7.1

# Restore the default Windows shell for correct batch processing below.
SHELL ["cmd", "/S", "/C"]

# Install Build Tools excluding workloads and components with known issues.
ADD https://aka.ms/vs/15/release/vs_buildtools.exe C:\TEMP\vs_buildtools.exe
RUN C:\TEMP\vs_buildtools.exe --quiet --wait --norestart --nocache `
    --installPath C:\BuildTools `
    --all `
    --remove Microsoft.VisualStudio.Component.Windows10SDK.10240 `
    --remove Microsoft.VisualStudio.Component.Windows10SDK.10586 `
    --remove Microsoft.VisualStudio.Component.Windows10SDK.14393 `
    --remove Microsoft.VisualStudio.Component.Windows81SDK `
 || IF "%ERRORLEVEL%"=="3010" EXIT 0
RUN DEL C:\TEMP\vs_buildtools.exe

# Install CMake
ADD https://github.com/Kitware/CMake/releases/download/v3.13.4/cmake-3.13.4-win64-x64.msi C:\TEMP\cmake.msi
RUN msiexec.exe /i C:\TEMP\cmake.msi /qn && `
    SETX PATH "%PATH%;C:\Program Files\CMake\bin" && `
    DEL C:\TEMP\cmake.msi

# Install boost (for the thrift runtime library build)
ADD https://boost.teeks99.com/bin/1.69.0/boost_1_69_0-msvc-14.1-64.exe C:\TEMP\boost.exe
RUN C:\TEMP\boost.exe /DIR="C:\Libraries\boost_1_69_0" /SILENT && `
    DEL C:\TEMP\boost.exe

# Install chocolatey
RUN @"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" `
    -NoProfile -InputFormat None -ExecutionPolicy Bypass -Command `
    "iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))" `
    && SETX PATH "%PATH%;%ALLUSERSPROFILE%\chocolatey\bin"

# Install winflexbison (for the thrift compiler build)
RUN choco install winflexbison3 -y

# Install 7zip and curl (used by the libevent and zlib build scripts)
RUN choco install 7zip curl -y

# Install libevent
COPY appveyor\build-libevent.bat C:\TEMP\build-libevent.bat
ENV LIBEVENT_VERSION=2.1.8
ENV WIN3P=C:\TEMP\WIN3P
RUN C:\BuildTools\Common7\Tools\VsDevCmd.bat -arch=amd64 && `
    MKDIR C:\TEMP\WIN3P && `
    C:\TEMP\build-libevent.bat && `
    MKDIR C:\Libraries\libevent-%LIBEVENT_VERSION% && `
    MOVE C:\TEMP\WIN3P\libevent-%LIBEVENT_VERSION%-stable\include C:\Libraries\libevent-%LIBEVENT_VERSION% && `
    MOVE C:\TEMP\WIN3P\libevent-%LIBEVENT_VERSION%-stable\lib C:\Libraries\libevent-%LIBEVENT_VERSION% && `
    RMDIR /S /Q C:\TEMP\WIN3P

# Install zlib
COPY appveyor\build-zlib.bat C:\TEMP\build-zlib.bat
ENV ZLIB_VERSION=1.2.11
ENV WIN3P=C:\TEMP\WIN3P
RUN C:\BuildTools\Common7\Tools\VsDevCmd.bat -arch=amd64 && `
    MKDIR C:\TEMP\WIN3P && `
    C:\TEMP\build-zlib.bat && `
    MOVE C:\TEMP\WIN3P\zlib-inst C:\Libraries\zlib-%ZLIB_VERSION% && `
    RMDIR /S /Q C:\TEMP\WIN3P

# Install OpenSSL 1.1.0
ADD http://slproweb.com/download/Win64OpenSSL-1_1_0j.exe C:\TEMP\openssl.exe
RUN C:\TEMP\openssl.exe /silent && `
    DEL C:\TEMP\openssl.exe

# Install java
RUN choco install jdk8 -y

# Install haskell
RUN choco install ghc -y

# Install python3
RUN choco install python3 -y

# Install Adobe Flex 4.6 SDK and set FLEX_HOME so it can be found
ADD http://download.macromedia.com/pub/flex/sdk/flex_sdk_4.6.zip C:\Adobe\Flex\SDK\4.6\SDK.zip
RUN CD C:\Adobe\Flex\SDK\4.6 && `
    7z x SDK.zip && `
    DEL SDK.zip && `
    SETX FLEX_HOME "C:\Adobe\Flex\SDK\4.6"

# Start developer command prompt with any other commands specified.
ENTRYPOINT C:\BuildTools\Common7\Tools\VsDevCmd.bat -arch=amd64 &&

# Default to PowerShell if no other command specified.
CMD ["powershell.exe", "-NoLogo", "-ExecutionPolicy", "Bypass"]
