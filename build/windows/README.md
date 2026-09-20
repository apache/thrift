<!--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements. See the NOTICE file
distributed with this work for additional information
regarding copyright ownership. The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied. See the License for the
specific language governing permissions and limitations
under the License.
-->

# Windows helpers

Scripts used by the Windows compiler build in
[`.github/workflows/cmake.yml`](../../.github/workflows/cmake.yml) and by the
Windows packaging in
[`.github/workflows/windows-packages.yml`](../../.github/workflows/windows-packages.yml),
and usable on their own from a developer machine.

## `check-compiler-imports.ps1`

Checks what a Windows executable imports. The `thrift.exe` published with a
release must depend only on Windows itself and on the Visual C++ runtime -
anything else, such as Boost, OpenSSL, zlib or libevent, would have to be
shipped alongside it, and the compiler is handed out as a single file.

```powershell
PS C:\thrift> .\build\windows\check-compiler-imports.ps1 -Path C:\install\bin\thrift.exe
```

It finds `dumpbin.exe` itself through `vswhere.exe`, so it does not have to run
from a Visual Studio developer prompt. Pass `-DumpBin` to point it somewhere
else, and `-AllowedImports` to change the permitted list; wildcards are allowed,
which is how one entry covers the whole UCRT.

The executable must also **not** be statically linked. The project moved away
from a statically linked compiler in `dd8ecde06`, and
[`doc/ReleaseManagement.md`](../../doc/ReleaseManagement.md) states that the
released compiler requires the Visual C++ redistributable. A static build
imports strictly fewer DLLs, every one of them permitted, so the allow list on
its own would wave it through; `-RequiredImport` is what catches it. Pass
`-RequiredImport ''` when a static build really is what you want.

The exit code is 0 when everything is in order and 1 otherwise, so it can be
used as a build step.

## `check-compiler-imports-tests.ps1`

Tests for the above. They feed canned `dumpbin` output through the parser, so
they need neither a compiler nor Windows - any PowerShell will do:

```bash
$ pwsh build/windows/check-compiler-imports-tests.ps1
```

They exist because a check like this fails open in both directions: a parser
that matches nothing reports every executable as clean, and an allow list that
grew far enough rejects nothing. The tests pin down that a release build passes,
that a static build and a third-party dependency are both rejected, and that the
wildcard matching does what the default allow list relies on.

## `build-installer.ps1`

Packages a built `thrift.exe` into a Windows installer with
[Inno Setup](https://jrsoftware.org/isinfo.php) 6.3 or later, which is
preinstalled on the GitHub Windows runner images.

```powershell
PS C:\thrift> .\build\windows\build-installer.ps1 -Version 0.26.0 -Compiler C:\install\bin\thrift.exe -OutputDir dist
```

It writes `thrift-<version>-setup.exe` into the output directory and prints its
path. `ISCC.exe` is located automatically; pass `-Iscc` to override.

## `installer/thrift.iss`

The Inno Setup script itself. It takes the version, the compiler and the output
directory on the command line, so no version number is stored in it and
[`build/veralign.sh`](../veralign.sh) has nothing extra to keep in step.

The installer installs per user by default and needs no elevation; an
administrator can install for all users from the wizard or with `/ALLUSERS`. It
optionally puts the install directory on `PATH`, and takes that entry out again
on uninstall.

The compiler links the shared runtime, so it needs the Microsoft Visual C++
redistributable, which the ASF cannot ship. An interactive install says so when
it cannot find `vcruntime140.dll`, rather than letting the first run fail with a
missing-DLL dialog. It never blocks, and a silent install - which is what a
package manager does - shows nothing; the WinGet and Chocolatey packages declare
the redistributable as a dependency instead, so their users never see it.

## `installer/test-installer.ps1`

Installs and uninstalls a built installer silently and checks what it did:
that the compiler is there and runs, that the install directory landed on
`PATH`, and that uninstalling removed exactly that one `PATH` entry and nothing
else. Inno Setup can append to `PATH` on its own but cannot undo it, so the
removal is hand-written code in `thrift.iss` - the part of the installer most
worth testing.

It changes the `PATH` of the user it runs as, so run it in CI or on a throwaway
machine.

```powershell
PS C:\thrift> .\build\windows\installer\test-installer.ps1 -Installer dist\thrift-0.26.0-setup.exe
```
