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
    Packs the Windows Thrift compiler as a .NET tool.

.DESCRIPTION
    Produces Apache.Thrift.Compiler.<version>.nupkg, installable with
    "dotnet tool install --global Apache.Thrift.Compiler".

    The version is passed to the script rather than stored in the project file,
    so that nothing new has to be kept in step by build/veralign.sh.

.PARAMETER Version
    The package version, for example 0.26.0.

.PARAMETER Compiler
    The thrift.exe to bundle.

.PARAMETER OutputDir
    Where to write the package. Created when it does not exist.

.PARAMETER SourceRoot
    Root of the Thrift source tree, used for LICENSE and NOTICE. Defaults to
    the checkout this script lives in.

.EXAMPLE
    pwsh build\windows\build-dotnet-tool.ps1 -Version 0.26.0 -Compiler C:\install\bin\thrift.exe -OutputDir nupkg
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string] $Version,
    [Parameter(Mandatory = $true)]
    [string] $Compiler,
    [string] $OutputDir = 'nupkg',
    [string] $SourceRoot = ''
)

$ErrorActionPreference = 'Stop'

if ($Version -notmatch '^\d+\.\d+\.\d+$') {
    throw "Version '$Version' is not major.minor.patch."
}
if (-not (Test-Path -LiteralPath $Compiler)) {
    throw "No such file: $Compiler"
}
if (-not (Get-Command 'dotnet' -ErrorAction SilentlyContinue)) {
    throw 'The dotnet CLI was not found. See https://dotnet.microsoft.com/download.'
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

$project  = Join-Path $PSScriptRoot 'dotnet-tool\Apache.Thrift.Compiler.csproj'
$compiler = (Resolve-Path -LiteralPath $Compiler).Path
$output   = (Resolve-Path -LiteralPath $OutputDir).Path
# MSBuild pastes this straight in front of a file name, so it needs to end in a
# separator.
$root     = (Resolve-Path -LiteralPath $SourceRoot).Path.TrimEnd('\', '/') +
            [System.IO.Path]::DirectorySeparatorChar

Write-Host "Packing $project"
Write-Host "  version:  $Version"
Write-Host "  compiler: $compiler"

& dotnet pack $project `
    --configuration Release `
    --output $output `
    -p:Version=$Version `
    -p:ThriftCompiler=$compiler `
    -p:ThriftSourceRoot=$root
if ($LASTEXITCODE -ne 0) {
    throw "dotnet pack failed with exit code $LASTEXITCODE"
}

$package = Join-Path $output "Apache.Thrift.Compiler.$Version.nupkg"
if (-not (Test-Path -LiteralPath $package)) {
    throw "dotnet pack reported success but $package is not there."
}

Write-Host "Built $package"
Write-Output $package
