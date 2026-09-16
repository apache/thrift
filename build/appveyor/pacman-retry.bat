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
:: Runs pacman in the MSYS2 shell %BASH% with the given arguments,
:: making up to three attempts.
::
:: pacman fetches a signature only from the mirror that served the file it
:: signs and does not fall back to another mirror, so a single dropped
:: connection fails the whole transaction. Packages that arrived together
:: with their signature stay in the package cache and are not fetched again.
::
:: pacman has to remain the only command in the bash -lc string. bash then
:: execs it instead of forking, and the core system upgrade, which ends by
:: closing every other MSYS2 process, finds no bash process left to close.
::
:: The attempts are written out one after another because a GOTO loop is not
:: safe here: this file is checked out with LF line endings, and cmd.exe does
:: not find labels reliably in such files.
::

%BASH% -lc "pacman %*" && EXIT /B 0

ECHO pacman failed, attempt 2 of 3 follows in 30 seconds
%BASH% -lc "sleep 30"
%BASH% -lc "pacman %*" && EXIT /B 0

ECHO pacman failed, attempt 3 of 3 follows in 30 seconds
%BASH% -lc "sleep 30"
%BASH% -lc "pacman %*" && EXIT /B 0

EXIT /B %ERRORLEVEL%
