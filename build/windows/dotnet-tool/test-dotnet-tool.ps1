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
    Checks a packed Apache.Thrift.Compiler tool package.

.DESCRIPTION
    Two things can go wrong with a .NET tool package that wraps a native
    executable, and neither shows up until somebody installs it: the payload
    can be missing from the package, and the package can be laid out so that
    the tool installs but does not run.

    So this looks inside the package, then installs it into a throwaway
    directory and runs it.

    On Windows the bundled compiler is expected to run. Anywhere else the
    launcher is expected to refuse with a message naming the platform, which
    is the behaviour that keeps a Linux user from a confusing failure.

.PARAMETER Package
    The .nupkg to check.

.PARAMETER Version
    The version the package should carry.

.PARAMETER ExpectedCompilerOutput
    Text the bundled executable should print when run. Only checked on
    Windows. Optional.

.EXAMPLE
    pwsh build\windows\dotnet-tool\test-dotnet-tool.ps1 -Package nupkg\Apache.Thrift.Compiler.0.26.0.nupkg -Version 0.26.0
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string] $Package,
    [Parameter(Mandatory = $true)]
    [string] $Version,
    [string] $ExpectedCompilerOutput = ''
)

$ErrorActionPreference = 'Stop'

$PackageId = 'Apache.Thrift.Compiler'
$Tfm = 'net8.0'

$failures = [System.Collections.Generic.List[string]]::new()
$passed = 0

function Assert-True {
    param([string] $Name, [bool] $Condition, [string] $Detail = '')

    if ($Condition) {
        Write-Host "  PASS  $Name"
        $script:passed++
    }
    else {
        Write-Host "  FAIL  $Name$(if ($Detail) { " - $Detail" })"
        $script:failures.Add($Name)
    }
}

if (-not (Test-Path -LiteralPath $Package)) {
    throw "No such file: $Package"
}
$Package = (Resolve-Path -LiteralPath $Package).Path

Write-Host "Checking $Package"

# ---- what is in the package ------------------------------------------------

Add-Type -AssemblyName System.IO.Compression.FileSystem
$archive = [System.IO.Compression.ZipFile]::OpenRead($Package)
try {
    $entries = @($archive.Entries | ForEach-Object { $_.FullName })

    $toolDir = "tools/$Tfm/any"
    foreach ($required in @(
            "$toolDir/DotnetToolSettings.xml",
            "$toolDir/thrift.exe",
            "$toolDir/$PackageId.dll",
            'README.md')) {
        Assert-True "the package contains $required" ($entries -contains $required)
    }

    # The licence files have to reach the user, and they reach them next to the
    # executable rather than at the package root: a tool package is built from
    # the publish output, so a plain None/Pack item never gets in.
    foreach ($legal in @("$toolDir/LICENSE.txt", "$toolDir/NOTICE.txt")) {
        $entry = $archive.GetEntry($legal)
        Assert-True "the package contains a non-empty $legal" `
            (($null -ne $entry) -and ($entry.Length -gt 0)) `
            $(if ($entry) { "length $($entry.Length)" } else { 'not present' })
    }

    # The payload reaches the package through the publish output. Adding it a
    # second time with Pack/PackagePath would put another copy somewhere else.
    $payloads = @($entries | Where-Object { $_ -like '*thrift.exe' })
    Assert-True 'the compiler appears exactly once' ($payloads.Count -eq 1) `
        ("found: " + ($payloads -join ', '))

    function Read-Entry {
        param([string] $Name)

        $entry = $archive.GetEntry($Name)
        if (-not $entry) { return '' }
        $reader = [System.IO.StreamReader]::new($entry.Open())
        try { return $reader.ReadToEnd() } finally { $reader.Dispose() }
    }

    $settings = Read-Entry "$toolDir/DotnetToolSettings.xml"
    Assert-True 'the tool is called thrift' ($settings -match 'Name\s*=\s*"thrift"') $settings
    Assert-True 'the tool entry point is the managed launcher' `
        ($settings -match "EntryPoint\s*=\s*`"$([regex]::Escape($PackageId)).dll`"") $settings

    $nuspec = Read-Entry "$PackageId.nuspec"
    Assert-True 'the package is a DotnetTool package' ($nuspec -match 'packageType\s+name="DotnetTool"')
    Assert-True "the package id is $PackageId" ($nuspec -match "<id>$([regex]::Escape($PackageId))</id>")
    Assert-True "the package version is $Version" ($nuspec -match "<version>$([regex]::Escape($Version))</version>")
    Assert-True 'the package is licensed Apache-2.0' ($nuspec -match '<license type="expression">Apache-2\.0</license>')
}
finally {
    $archive.Dispose()
}

# ---- installing and running it ---------------------------------------------

$toolPath = Join-Path ([System.IO.Path]::GetTempPath()) ("thrift-tool-test-" + [System.Guid]::NewGuid().ToString('N'))
$source = Split-Path -Parent $Package

try {
    # --tool-path keeps this out of the machine wide tool store.
    $install = & dotnet tool install $PackageId `
        --tool-path $toolPath --add-source $source --version $Version 2>&1 | Out-String
    Assert-True 'the tool installs' ($LASTEXITCODE -eq 0) $install

    $command = Join-Path $toolPath ($(if ($IsWindows) { 'thrift.exe' } else { 'thrift' }))
    Assert-True 'the tool is installed as thrift' (Test-Path -LiteralPath $command) "looked for $command"

    if (Test-Path -LiteralPath $command) {
        $output = & $command -version 2>&1 | Out-String
        $exitCode = $LASTEXITCODE

        if ($IsWindows) {
            Assert-True 'the bundled compiler runs' ($exitCode -eq 0) "exit $exitCode, output '$output'"
            if ($ExpectedCompilerOutput) {
                Assert-True "the bundled compiler prints '$ExpectedCompilerOutput'" `
                    ($output -match [regex]::Escape($ExpectedCompilerOutput)) "output '$output'"
            }
        }
        else {
            # NuGet installs the package anywhere, so the launcher has to say
            # what is wrong instead of failing to start a Windows executable.
            Assert-True 'the launcher refuses to run off Windows' ($exitCode -eq 1) `
                "exit $exitCode, output '$output'"
            Assert-True 'the refusal names the platform' `
                ($output -match 'Windows build of the Apache Thrift compiler') "output '$output'"
            Assert-True 'the refusal points somewhere useful' `
                ($output -match 'thrift\.apache\.org/download') "output '$output'"
        }
    }
}
finally {
    Remove-Item -LiteralPath $toolPath -Recurse -Force -ErrorAction SilentlyContinue
}

Write-Host ''
if ($failures.Count -gt 0) {
    Write-Host "$($failures.Count) of $($failures.Count + $passed) checks failed:"
    foreach ($failure in $failures) { Write-Host "  $failure" }
    exit 1
}

Write-Host "$passed checks passed."
exit 0
