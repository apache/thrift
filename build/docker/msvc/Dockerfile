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

RUN IF NOT EXIST C:\TEMP MKDIR C:\TEMP

# Install Build Tools excluding workloads and components with known issues.
RUN curl.exe -fsSL -o C:\TEMP\vs_buildtools.exe https://download.visualstudio.microsoft.com/download/pr/e8db5368-d542-4208-ab91-ea2ac11f00b8/cfbe96493247c4c4280ea7e765ad427f89a76a1507b1144c7edb319069369387/vs_BuildTools.exe && `
    powershell -NoProfile -Command "if ((Get-FileHash 'C:\TEMP\vs_buildtools.exe' -Algorithm SHA256).Hash.ToLower() -ne 'cfbe96493247c4c4280ea7e765ad427f89a76a1507b1144c7edb319069369387') { exit 1 }" && `
    (C:\TEMP\vs_buildtools.exe --quiet --wait --norestart --nocache `
    --installPath C:\BuildTools `
    --add Microsoft.VisualStudio.Component.VC.CoreBuildTools `
    --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 `
    --add Microsoft.VisualStudio.Component.Windows10SDK.19041 `
    || IF "%ERRORLEVEL%"=="3010" EXIT 0) && `
    DEL /F /Q C:\TEMP\vs_buildtools.exe && `
    IF EXIST "C:\ProgramData\Package Cache" RMDIR /S /Q "C:\ProgramData\Package Cache" && `
    IF EXIST C:\Windows\SoftwareDistribution\Download RMDIR /S /Q C:\Windows\SoftwareDistribution\Download

# Install CMake
RUN curl.exe -fsSL -o C:\TEMP\cmake.msi https://github.com/Kitware/CMake/releases/download/v4.1.2/cmake-4.1.2-windows-x86_64.msi && `
    powershell -NoProfile -Command "if ((Get-FileHash 'C:\TEMP\cmake.msi' -Algorithm SHA256).Hash.ToLower() -ne 'a10fc2527ec0727a5913acd310cddafdba1e5b4ca765ca23cc6a53c55eebb7c1') { exit 1 }" && `
    msiexec.exe /i C:\TEMP\cmake.msi /qn && `
    SETX PATH "%PATH%;C:\Program Files\CMake\bin" && `
    DEL /F /Q C:\TEMP\cmake.msi && `
    FOR %D IN (doc man) DO @IF EXIST "C:\Program Files\CMake\%D" RMDIR /S /Q "C:\Program Files\CMake\%D"

# Install boost (for the thrift runtime library build)
ENV BOOST_ROOT=C:\Libraries\boost_1_88_0
ENV BOOST_LIBRARYDIR=C:\Libraries\boost_1_88_0\lib64-msvc-14.3
RUN curl.exe -fsSL -o C:\TEMP\boost.exe https://boost.teeks99.com/bin/1.88.0/boost_1_88_0-msvc-14.3-64.exe && `
    powershell -NoProfile -Command "if ((Get-FileHash 'C:\TEMP\boost.exe' -Algorithm SHA256).Hash.ToLower() -ne 'bff575f21343f602c7b6c3ad3883a28053a1a50cde2153102d30479f85911738') { exit 1 }" && `
    C:\TEMP\boost.exe /DIR=%BOOST_ROOT% /SILENT && `
    DEL /F /Q C:\TEMP\boost.exe && `
    FOR %D IN (doc libs tools status more) DO @IF EXIST "%BOOST_ROOT%\%D" RMDIR /S /Q "%BOOST_ROOT%\%D" && `
    SETX BOOST_LIBRARYDIR "%BOOST_ROOT%\lib64-msvc-14.3" && `
    SETX PATH "%BOOST_LIBRARYDIR%;%PATH%"

# Install chocolatey
ENV CHOCO_DIR=C:\ProgramData\chocolatey
RUN @"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" `
    -NoProfile -InputFormat None -ExecutionPolicy Bypass -Command `
    "[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 ; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))" `
    && SETX PATH "%PATH%;%ALLUSERSPROFILE%\chocolatey\bin"

# Install winflexbison (for the thrift compiler build)
RUN choco install winflexbison3 -y --no-progress && `
    choco cache remove -y

# Install 7zip and curl (used by the libevent and zlib build scripts)
RUN choco install 7zip curl -y --no-progress && `
    choco cache remove -y

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

# Install OpenSSL
RUN curl.exe -fsSL -o C:\TEMP\openssl.exe https://slproweb.com/download/Win64OpenSSL-3_6_2.exe && `
    powershell -NoProfile -Command "if ((Get-FileHash 'C:\TEMP\openssl.exe' -Algorithm SHA256).Hash.ToLower() -ne 'c395482a1af33b2cdd4b801b227c864ed049e0f6aff79413d31b4fa916a67b1a') { exit 1 }" && `
    C:\TEMP\openssl.exe /silent && `
    DEL /F /Q C:\TEMP\openssl.exe && `
    IF EXIST "C:\Program Files\OpenSSL-Win64\tests" RMDIR /S /Q "C:\Program Files\OpenSSL-Win64\tests" && `
    SETX OPENSSL_ROOT "C:\Program Files\OpenSSL-Win64" && `
    SETX PATH "%OPENSSL_ROOT%\bin;%PATH%"

# Install java
RUN choco install jdk8 -y --no-progress -params "source=false" && `
    choco cache remove -y && `
    IF EXIST "%CHOCO_DIR%\lib\jdk8" RMDIR /S /Q "%CHOCO_DIR%\lib\jdk8"

# Install python3
RUN choco install python314 -y --no-progress --install-arguments "'Include_doc=0 Include_launcher=0 Include_tcltk=0 Include_test=0'" && `
    choco cache remove -y && `
    C:\Python314\python.exe -m pip install --disable-pip-version-check --no-cache-dir setuptools && `
    IF EXIST "C:\ProgramData\Package Cache" RMDIR /S /Q "C:\ProgramData\Package Cache" && `
    IF EXIST "%CHOCO_DIR%\lib\python314" RMDIR /S /Q "%CHOCO_DIR%\lib\python314"

RUN choco install nodejs -y --no-progress && `
    choco cache remove -y && `
    IF EXIST "C:\ProgramData\Package Cache" RMDIR /S /Q "C:\ProgramData\Package Cache" && `
    IF EXIST "%CHOCO_DIR%\lib\nodejs" RMDIR /S /Q "%CHOCO_DIR%\lib\nodejs" && `
    IF EXIST "%CHOCO_DIR%\lib\nodejs.install" RMDIR /S /Q "%CHOCO_DIR%\lib\nodejs.install"

# Start developer command prompt with any other commands specified.
ENTRYPOINT C:\BuildTools\Common7\Tools\VsDevCmd.bat -arch=x64 -host_arch=x64 &&

# Default to PowerShell if no other command specified.
CMD powershell.exe -NoLogo -ExecutionPolicy Bypass
