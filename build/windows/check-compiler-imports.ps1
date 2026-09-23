#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

<#
.SYNOPSIS
    Checks what a Windows executable imports.

.DESCRIPTION
    The thrift.exe published with an Apache Thrift release must depend only on
    Windows itself and on the Visual C++ runtime. Anything else - Boost,
    OpenSSL, zlib, libevent - would have to be shipped alongside it, and the
    compiler is handed out as a single file.

    It must also not be statically linked. The project moved away from a
    statically linked compiler in dd8ecde06, and doc/ReleaseManagement.md
    states that the released compiler requires the Visual C++ redistributable.
    A static build imports strictly fewer DLLs, all of them permitted, so an
    allow list on its own would not notice; -RequiredImport is what does.

    Release management used to do all of this by hand with Dependency Walker.

.PARAMETER Path
    The executable to check.

.PARAMETER AllowedImports
    DLL names the executable may import, compared case-insensitively.
    Wildcards are allowed, which is how the UCRT is covered without listing
    every api-ms-win-crt DLL by name.

.PARAMETER RequiredImport
    A DLL the executable must import, or an empty string to skip that check.
    Defaults to the Visual C++ runtime, whose absence means a static build.
    Pass -RequiredImport '' when deliberately building a static compiler for
    some other purpose.

.PARAMETER DumpBin
    Full path to dumpbin.exe. Found automatically when omitted: first on PATH,
    then in the Visual Studio installation vswhere reports.

.EXAMPLE
    pwsh build\windows\check-compiler-imports.ps1 -Path C:\install\bin\thrift.exe
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]   $Path,
    [string[]] $AllowedImports = @(
        # Windows itself.
        'kernel32.dll',
        # Reached through the CoCreateGuid call in the Delphi generator.
        'ole32.dll',
        # The Universal C Runtime, which is part of Windows.
        'api-ms-win-crt-*.dll',
        'ucrtbase.dll',
        # The Visual C++ redistributable.
        'msvcp140.dll',
        'vcruntime140.dll',
        'vcruntime140_1.dll'
    ),
    [string]   $RequiredImport = 'vcruntime140.dll',
    [string]   $DumpBin = ''
)

$ErrorActionPreference = 'Stop'

function Fail {
    # -ErrorAction Continue keeps $ErrorActionPreference from turning this into
    # a terminating error, so the exit code below is the one callers observe.
    param([string] $Message)

    Write-Error -Message $Message -ErrorAction Continue
    exit 1
}

function Find-DumpBin {
    param([string] $Override)

    if ($Override) {
        if (-not (Test-Path -LiteralPath $Override)) {
            throw "dumpbin.exe not found at the given -DumpBin path: $Override"
        }
        return $Override
    }

    # Already in a Visual Studio developer prompt?
    $onPath = Get-Command 'dumpbin.exe' -ErrorAction SilentlyContinue
    if ($onPath) { return $onPath.Source }

    $vswhere = Join-Path ${env:ProgramFiles(x86)} 'Microsoft Visual Studio\Installer\vswhere.exe'
    if (-not (Test-Path -LiteralPath $vswhere)) {
        throw "dumpbin.exe is not on PATH and vswhere.exe was not found at $vswhere. Pass -DumpBin explicitly."
    }

    $installPath = & $vswhere -latest -products * `
        -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 `
        -property installationPath
    if (-not $installPath) {
        throw 'vswhere.exe did not report a Visual Studio installation with the C++ build tools.'
    }

    $candidate = Get-ChildItem -Path (Join-Path $installPath 'VC\Tools\MSVC\*\bin\Host*\x64\dumpbin.exe') `
        -ErrorAction SilentlyContinue | Sort-Object FullName -Descending | Select-Object -First 1
    if (-not $candidate) {
        throw "dumpbin.exe was not found below $installPath. Pass -DumpBin explicitly."
    }

    return $candidate.FullName
}

function Get-ImportedModules {
    param([string] $Executable, [string] $DumpBinPath)

    $output = & $DumpBinPath /nologo /dependents $Executable
    if ($LASTEXITCODE -ne 0) {
        throw "dumpbin failed with exit code $LASTEXITCODE"
    }

    # dumpbin lists every dependency - regular and delay loaded - one per line
    # before the "Summary" section, which repeats section names that are not
    # DLLs. Stop there so the summary cannot contribute false positives.
    $modules = [System.Collections.Generic.List[string]]::new()
    foreach ($line in $output) {
        if ($line -match '^\s*Summary\s*$') { break }
        if ($line -match '^\s*([A-Za-z0-9_.+-]+\.dll)\s*$') {
            $modules.Add($Matches[1])
        }
    }

    return $modules
}

if (-not (Test-Path -LiteralPath $Path)) {
    Fail "No such file: $Path"
}
$Path = (Resolve-Path -LiteralPath $Path).Path

try {
    $dumpBinPath = Find-DumpBin -Override $DumpBin
    $imports = Get-ImportedModules -Executable $Path -DumpBinPath $dumpBinPath
}
catch {
    Fail $_.Exception.Message
}

Write-Host "Checking $Path"
Write-Host "  using $dumpBinPath"

if ($imports.Count -eq 0) {
    # A native executable always imports something; an empty list means the
    # output format changed and the check silently stopped testing anything.
    Fail 'dumpbin reported no imported modules at all - the import table could not be read.'
}

$allowed = @($AllowedImports | ForEach-Object { $_.ToLowerInvariant() })

function Test-Allowed {
    # -like rather than equality, so that one 'api-ms-win-crt-*.dll' entry
    # stands for the whole UCRT rather than a dozen literal names.
    param([string] $Module)

    $name = $Module.ToLowerInvariant()
    foreach ($pattern in $allowed) {
        if ($name -like $pattern) { return $true }
    }
    return $false
}

$unexpected = @($imports | Where-Object { -not (Test-Allowed $_) })

Write-Host 'Imported modules:'
foreach ($module in $imports) {
    $mark = if (Test-Allowed $module) { 'ok      ' } else { 'NOT OK  ' }
    Write-Host "  $mark $module"
}

$name = [System.IO.Path]::GetFileName($Path)

if ($unexpected.Count -gt 0) {
    Fail ("$name imports " + ($unexpected -join ', ') + ', which it may not. Allowed: ' +
          ($(if ($AllowedImports) { $AllowedImports -join ', ' } else { '(nothing)' })) + '.')
}

if ($RequiredImport) {
    # A statically linked build imports fewer DLLs, every one of them allowed,
    # so it would sail past the check above. This is what catches it.
    $required = $RequiredImport.ToLowerInvariant()
    if (-not ($imports | Where-Object { $_.ToLowerInvariant() -like $required })) {
        Fail ("$name does not import $RequiredImport. That means it was linked against the " +
              'static runtime, which this project moved away from in dd8ecde06 - the released ' +
              'compiler is expected to require the Visual C++ redistributable. ' +
              "Pass -RequiredImport '' if a static build really is what you want.")
    }
}

Write-Host "$name imports only what it may."
exit 0
