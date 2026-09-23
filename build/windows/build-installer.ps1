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
    Packages a built Windows Thrift compiler into an installer.

.DESCRIPTION
    Runs Inno Setup over build/windows/installer/thrift.iss. Inno Setup 6.3 or
    later is required; the GitHub Windows runner images carry it, and locally
    it comes from https://jrsoftware.org/isdl.php.

    The version is passed to the script rather than stored in it, so that
    nothing new has to be kept in step by build/veralign.sh.

.PARAMETER Version
    The version to stamp into the installer, for example 0.25.0.

.PARAMETER Compiler
    The thrift.exe to package.

.PARAMETER OutputDir
    Where to write the installer. Created when it does not exist.

.PARAMETER SourceRoot
    Root of the Thrift source tree, used for LICENSE and NOTICE. Defaults to
    the checkout this script lives in.

.PARAMETER Iscc
    Full path to ISCC.exe. Found automatically when omitted.

.EXAMPLE
    pwsh build\windows\build-installer.ps1 -Version 0.25.0 -Compiler C:\install\bin\thrift.exe -OutputDir dist
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string] $Version,
    [Parameter(Mandatory = $true)]
    [string] $Compiler,
    [string] $OutputDir = 'dist',
    [string] $SourceRoot = '',
    [string] $Iscc = ''
)

$ErrorActionPreference = 'Stop'

function Find-Iscc {
    param([string] $Override)

    if ($Override) {
        if (-not (Test-Path -LiteralPath $Override)) {
            throw "ISCC.exe not found at the given -Iscc path: $Override"
        }
        return $Override
    }

    $onPath = Get-Command 'ISCC.exe' -ErrorAction SilentlyContinue
    if ($onPath) { return $onPath.Source }

    $candidates = @(
        (Join-Path ${env:ProgramFiles(x86)} 'Inno Setup 6\ISCC.exe'),
        (Join-Path $env:ProgramFiles 'Inno Setup 6\ISCC.exe')
    )
    foreach ($candidate in $candidates) {
        if ($candidate -and (Test-Path -LiteralPath $candidate)) { return $candidate }
    }

    throw 'ISCC.exe was not found. Install Inno Setup 6.3 or later, or pass -Iscc.'
}

if ($Version -notmatch '^\d+\.\d+\.\d+$') {
    throw "Version '$Version' is not major.minor.patch."
}
if (-not (Test-Path -LiteralPath $Compiler)) {
    throw "No such file: $Compiler"
}

if (-not $SourceRoot) {
    $SourceRoot = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
}
foreach ($required in @('LICENSE', 'NOTICE')) {
    if (-not (Test-Path -LiteralPath (Join-Path $SourceRoot $required))) {
        throw "$required not found below -SourceRoot $SourceRoot"
    }
}

New-Item -Path $OutputDir -ItemType Directory -Force | Out-Null

$isccPath   = Find-Iscc -Override $Iscc
$script     = Join-Path $PSScriptRoot 'installer\thrift.iss'
$compiler   = (Resolve-Path -LiteralPath $Compiler).Path
$outputPath = (Resolve-Path -LiteralPath $OutputDir).Path

Write-Host "Building the installer with $isccPath"
& $isccPath `
    "/DAppVersion=$Version" `
    "/DSourceExe=$compiler" `
    "/DSourceRoot=$SourceRoot" `
    "/DOutputDir=$outputPath" `
    $script
if ($LASTEXITCODE -ne 0) {
    throw "Inno Setup failed with exit code $LASTEXITCODE"
}

$installer = Join-Path $outputPath "thrift-$Version-setup.exe"
if (-not (Test-Path -LiteralPath $installer)) {
    throw "Inno Setup reported success but $installer is not there."
}

Write-Host "Built $installer"
Write-Output $installer
