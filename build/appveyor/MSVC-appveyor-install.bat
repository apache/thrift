::
:: Licensed under the Apache License, Version 2.0 (the "License");
:: you may not use this file except in compliance with the License.
:: You may obtain a copy of the License at
::
::     http://www.apache.org/licenses/LICENSE-2.0
::
:: Unless required by applicable law or agreed to in writing, software
:: distributed under the License is distributed on an "AS IS" BASIS,
:: WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
:: See the License for the specific language governing permissions and
:: limitations under the License.
::

::
:: Appveyor install script for MSVC
:: Installs (or builds) third party packages we need
::

@ECHO OFF
SETLOCAL EnableDelayedExpansion

CD build\appveyor                         || EXIT /B
CALL cl_banner_install.bat                || EXIT /B
CALL cl_setenv.bat                        || EXIT /B
CALL cl_showenv.bat                       || EXIT /B
MKDIR "%WIN3P%"                           || EXIT /B

choco feature enable -n allowGlobalConfirmation || EXIT /B

:: Things to install when NOT running in appveyor:
IF "%APPVEYOR_BUILD_ID%" == "" (
    cup -y chocolatey                     || EXIT /B
    cinst -y curl                         || EXIT /B
    cinst -y 7zip                         || EXIT /B
    cinst -y python3                      || EXIT /B
    cinst -y openssl.light                || EXIT /B
)

cinst -y jdk8                             || EXIT /B
cinst -y winflexbison3                    || EXIT /B

:: zlib - not available through chocolatey
CD "%APPVEYOR_SCRIPTS%"                   || EXIT /B
call build-zlib.bat                       || EXIT /B

:: libevent - not available through chocolatey
CD "%APPVEYOR_SCRIPTS%"                   || EXIT /B
call build-libevent.bat                   || EXIT /B

:: python packages (correct path to pip set in cl_setenv.bat)
pip.exe ^
    install backports.ssl_match_hostname ^
            ipaddress ^
            six ^
            tornado ^
            twisted                       || EXIT /B

cinst -y ghc                              || EXIT /B

:: Adobe Flex SDK 4.6 for ActionScript
MKDIR "C:\Adobe\Flex\SDK\4.6"             || EXIT /B
appveyor DownloadFile http://download.macromedia.com/pub/flex/sdk/flex_sdk_4.6.zip -FileName C:\Adobe\Flex\SDK\4.6\SDK.zip || EXIT /B
CD "C:\Adobe\Flex\SDK\4.6"                || EXIT /B
7z x SDK.zip                              || EXIT /B
SETX FLEX_HOME "C:\Adobe\Flex\SDK\4.6"
