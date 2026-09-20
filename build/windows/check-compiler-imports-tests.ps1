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
    Tests for check-compiler-imports.ps1.

.DESCRIPTION
    Drives check-compiler-imports.ps1 against a stub that prints canned dumpbin
    output, so that the import-table parser is exercised without a compiler
    and on any platform PowerShell runs on.

    The point of these tests is that the check must be able to *fail*. A
    parser that quietly matches nothing would let every executable through.

.EXAMPLE
    pwsh build\windows\check-compiler-imports-tests.ps1
#>

[CmdletBinding()]
param()

$ErrorActionPreference = 'Stop'

$scriptUnderTest = Join-Path $PSScriptRoot 'check-compiler-imports.ps1'
if (-not (Test-Path -LiteralPath $scriptUnderTest)) {
    throw "check-compiler-imports.ps1 not found next to $PSCommandPath"
}

$workDir = Join-Path ([System.IO.Path]::GetTempPath()) ("thrift-compiler-imports-tests-" + [System.Guid]::NewGuid().ToString('N'))
New-Item -Path $workDir -ItemType Directory -Force | Out-Null

# Stands in for the executable under test. Its contents are irrelevant: the
# real import table comes from the stub dumpbin.
$fakeExe = Join-Path $workDir 'thrift.exe'
Set-Content -LiteralPath $fakeExe -Value 'not a real executable' -NoNewline

$failures = [System.Collections.Generic.List[string]]::new()
$passed = 0

function New-StubDumpBin {
    param([string] $Name, [string] $Output, [int] $ExitCode = 0)

    $stub = Join-Path $workDir "$Name.ps1"
    $encoded = [Convert]::ToBase64String([Text.Encoding]::UTF8.GetBytes($Output))
    @"
param()
[Text.Encoding]::UTF8.GetString([Convert]::FromBase64String('$encoded')) -split "`r?`n" | ForEach-Object { Write-Output `$_ }
exit $ExitCode
"@ | Set-Content -LiteralPath $stub
    return $stub
}

function Invoke-Check {
    param([string] $Stub, [string[]] $AllowedImports, [string] $RequiredImport)

    # Hash table splatting, not array splatting: an array would bind every
    # element positionally and the parameter names would arrive as values.
    $checkArgs = @{ Path = $fakeExe; DumpBin = $Stub }
    if ($PSBoundParameters.ContainsKey('AllowedImports')) {
        $checkArgs['AllowedImports'] = $AllowedImports
    }
    if ($PSBoundParameters.ContainsKey('RequiredImport')) {
        $checkArgs['RequiredImport'] = $RequiredImport
    }

    # *>&1 folds every stream into the pipeline so the script under test does
    # not print over the test report, and so its error records cannot become
    # terminating errors here.
    $transcript = & $scriptUnderTest @checkArgs *>&1 | Out-String
    Write-Verbose $transcript
    return $LASTEXITCODE
}

function Assert-ExitCode {
    param([string] $Name, [int] $Actual, [int] $Expected)

    if ($Actual -eq $Expected) {
        Write-Host "  PASS  $Name"
        $script:passed++
    }
    else {
        Write-Host "  FAIL  $Name (expected exit $Expected, got $Actual)"
        $script:failures.Add($Name)
    }
}

# What a correct release build looks like: the import table observed on the
# Windows CI runner.
$releaseBuild = @'

Dump of file thrift.exe

File Type: EXECUTABLE IMAGE

  Image has the following dependencies:

    KERNEL32.dll
    ole32.dll
    MSVCP140.dll
    VCRUNTIME140.dll
    VCRUNTIME140_1.dll
    api-ms-win-crt-heap-l1-1-0.dll
    api-ms-win-crt-runtime-l1-1-0.dll
    api-ms-win-crt-stdio-l1-1-0.dll
    api-ms-win-crt-time-l1-1-0.dll
    api-ms-win-crt-string-l1-1-0.dll
    api-ms-win-crt-filesystem-l1-1-0.dll
    api-ms-win-crt-locale-l1-1-0.dll
    api-ms-win-crt-math-l1-1-0.dll
    api-ms-win-crt-convert-l1-1-0.dll

  Summary

        3000 .data
       1C000 .rdata
'@

# A statically linked build. Every DLL here is on the allow list, so only the
# required-import check can reject it.
$staticBuild = @'

Dump of file thrift.exe

File Type: EXECUTABLE IMAGE

  Image has the following dependencies:

    KERNEL32.dll
    ole32.dll

  Summary

        3000 .data
'@

# Something that should never have been linked into the compiler.
$thirdPartyDependency = @'

Dump of file thrift.exe

File Type: EXECUTABLE IMAGE

  Image has the following dependencies:

    KERNEL32.dll
    VCRUNTIME140.dll
    libcrypto-3-x64.dll

  Summary

        3000 .data
'@

$delayLoaded = @'

Dump of file thrift.exe

File Type: EXECUTABLE IMAGE

  Image has the following dependencies:

    KERNEL32.dll
    VCRUNTIME140.dll

  Image has the following delay load dependencies:

    libcrypto-3-x64.dll

  Summary

        3000 .data
'@

# A summary section must never contribute imports, or the check would report
# dependencies that are not really there.
$summaryMentionsDll = @'

Dump of file thrift.exe

File Type: EXECUTABLE IMAGE

  Image has the following dependencies:

    KERNEL32.dll
    VCRUNTIME140.dll

  Summary

    SOMETHING.dll
'@

$noImportTable = @'

Dump of file thrift.exe

File Type: EXECUTABLE IMAGE

  Summary

        3000 .data
'@

Write-Host 'check-compiler-imports.ps1'

Assert-ExitCode 'a release build passes' `
    (Invoke-Check -Stub (New-StubDumpBin 'release' $releaseBuild)) 0

# The whole point of the -RequiredImport check. Every DLL a static build
# imports is on the allow list, so nothing else here would reject it.
Assert-ExitCode 'a statically linked build fails' `
    (Invoke-Check -Stub (New-StubDumpBin 'static' $staticBuild)) 1

Assert-ExitCode 'a statically linked build passes when that is asked for' `
    (Invoke-Check -Stub (New-StubDumpBin 'staticok' $staticBuild) -RequiredImport '') 0

Assert-ExitCode 'a third party dependency fails' `
    (Invoke-Check -Stub (New-StubDumpBin 'thirdparty' $thirdPartyDependency)) 1

Assert-ExitCode 'a delay loaded third party dependency fails' `
    (Invoke-Check -Stub (New-StubDumpBin 'delay' $delayLoaded)) 1

Assert-ExitCode 'summary section is not parsed as an import' `
    (Invoke-Check -Stub (New-StubDumpBin 'summary' $summaryMentionsDll)) 0

Assert-ExitCode 'unreadable import table fails instead of passing' `
    (Invoke-Check -Stub (New-StubDumpBin 'empty' $noImportTable)) 1

Assert-ExitCode 'dumpbin failure is reported' `
    (Invoke-Check -Stub (New-StubDumpBin 'broken' 'LINK : fatal error LNK1181' 1181)) 1

# The allow list is honoured rather than hard coded, in both directions.
Assert-ExitCode 'a widened allow list accepts the third party dependency' `
    (Invoke-Check -Stub (New-StubDumpBin 'widened' $thirdPartyDependency) `
                  -AllowedImports @('kernel32.dll', 'vcruntime140.dll', 'libcrypto-3-x64.dll')) 0

Assert-ExitCode 'an empty allow list rejects everything' `
    (Invoke-Check -Stub (New-StubDumpBin 'none' $releaseBuild) -AllowedImports @()) 1

# One 'api-ms-win-crt-*.dll' entry has to stand for the whole UCRT; without
# wildcard matching the release build above could not pass at all.
Assert-ExitCode 'a wildcard covers the UCRT' `
    (Invoke-Check -Stub (New-StubDumpBin 'ucrt' $releaseBuild) `
                  -AllowedImports @('kernel32.dll', 'ole32.dll', 'msvcp140.dll',
                                    'vcruntime140.dll', 'vcruntime140_1.dll',
                                    'api-ms-win-crt-*.dll')) 0

Assert-ExitCode 'a wildcard that does not cover the UCRT still rejects it' `
    (Invoke-Check -Stub (New-StubDumpBin 'noucrt' $releaseBuild) `
                  -AllowedImports @('kernel32.dll', 'ole32.dll', 'msvcp140.dll',
                                    'vcruntime140.dll', 'vcruntime140_1.dll',
                                    'api-ms-win-gdi-*.dll')) 1

Remove-Item -LiteralPath $workDir -Recurse -Force -ErrorAction SilentlyContinue

Write-Host ''
if ($failures.Count -gt 0) {
    Write-Host "$($failures.Count) of $($failures.Count + $passed) tests failed:"
    foreach ($failure in $failures) { Write-Host "  $failure" }
    exit 1
}

Write-Host "$passed tests passed."
exit 0
