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
    Builds the Chocolatey package for the Windows Thrift compiler.

.DESCRIPTION
    Renders the templates in build/windows/chocolatey and runs "choco pack".

    The package does not carry the compiler. It downloads the installer
    published with the release and runs it, so that what a user installs is
    what was voted on and signed. Unless a checksum is given, it is computed
    from the file at that very URL, which means this cannot run before the
    release has reached the archive.

.PARAMETER Version
    The released version, for example 0.25.0.

.PARAMETER InstallerUrl
    Where the installer is published. Defaults to the Apache archive, which
    keeps every release; downloads.apache.org only carries the current one.

.PARAMETER Sha256
    The installer's SHA-256. Computed from the downloaded file when omitted.

.PARAMETER OutputDir
    Where the staged package and the .nupkg are written.

.PARAMETER SourceRoot
    Root of the Thrift source tree, used for LICENSE and NOTICE. Defaults to
    the checkout this script lives in.

.PARAMETER StageOnly
    Render the package without packing it, so that the generated files can be
    reviewed on a machine that has no Chocolatey.

.EXAMPLE
    pwsh build/windows/build-chocolatey-package.ps1 -Version 0.25.0
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string] $Version,
    [string] $InstallerUrl = '',
    [string] $Sha256 = '',
    [string] $OutputDir = 'chocolatey-package',
    [string] $SourceRoot = '',
    [switch] $StageOnly
)

$ErrorActionPreference = 'Stop'

if ($Version -notmatch '^\d+\.\d+\.\d+$') {
    throw "Version '$Version' is not major.minor.patch."
}
if (-not $InstallerUrl) {
    $InstallerUrl = "https://archive.apache.org/dist/thrift/$Version/thrift-$Version-setup.exe"
}
if (-not $SourceRoot) {
    $SourceRoot = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
}
foreach ($required in @('LICENSE', 'NOTICE')) {
    if (-not (Test-Path -LiteralPath (Join-Path $SourceRoot $required))) {
        throw "$required not found below -SourceRoot $SourceRoot"
    }
}

if ($Sha256) {
    if ($Sha256 -notmatch '^[0-9a-fA-F]{64}$') {
        throw "Sha256 '$Sha256' is not 64 hexadecimal digits."
    }
    $Sha256 = $Sha256.ToUpperInvariant()
}
else {
    Write-Host "Downloading $InstallerUrl"
    $download = Join-Path ([System.IO.Path]::GetTempPath()) ([System.Guid]::NewGuid().ToString('N') + '.exe')
    try {
        try {
            Invoke-WebRequest -Uri $InstallerUrl -OutFile $download -MaximumRedirection 5
        }
        catch {
            throw ("Could not download $InstallerUrl : " + $_.Exception.Message + "`n" +
                   'If the release was just promoted, the Apache archive may not have picked it up yet. Wait and try again.')
        }
        $Sha256 = (Get-FileHash -Algorithm SHA256 -LiteralPath $download).Hash.ToUpperInvariant()
        Write-Host "  sha256: $Sha256"
    }
    finally {
        Remove-Item -LiteralPath $download -Force -ErrorAction SilentlyContinue
    }
}

$templateDir = Join-Path $PSScriptRoot 'chocolatey'
$stageDir = Join-Path $OutputDir 'staging'
if (Test-Path -LiteralPath $stageDir) {
    Remove-Item -LiteralPath $stageDir -Recurse -Force
}
New-Item -Path (Join-Path $stageDir 'tools') -ItemType Directory -Force | Out-Null

$substitutions = @{
    '__VERSION__'          = $Version
    '__INSTALLER_URL__'    = $InstallerUrl
    '__INSTALLER_SHA256__' = $Sha256
}

foreach ($template in (Get-ChildItem -Path $templateDir -Filter '*.in' -Recurse)) {
    $relative = $template.FullName.Substring($templateDir.Length).TrimStart('\', '/')
    $target = Join-Path $stageDir ($relative -replace '\.in$', '')

    $content = Get-Content -LiteralPath $template.FullName -Raw
    foreach ($key in $substitutions.Keys) {
        $content = $content.Replace($key, $substitutions[$key])
    }

    # A placeholder that survived would be published verbatim.
    $leftovers = [regex]::Matches($content, '__[A-Z0-9_]+__')
    if ($leftovers.Count -gt 0) {
        throw ("$relative still holds placeholders after substitution: " +
               (($leftovers | ForEach-Object { $_.Value } | Sort-Object -Unique) -join ', '))
    }

    New-Item -Path (Split-Path -Parent $target) -ItemType Directory -Force | Out-Null
    [System.IO.File]::WriteAllText($target, $content, [System.Text.UTF8Encoding]::new($false))
    Write-Host "Rendered $target"
}

# Chocolatey shows LICENSE.txt from the tools directory, and the ASF wants the
# notice to travel with the package either way.
Copy-Item -LiteralPath (Join-Path $SourceRoot 'LICENSE') -Destination (Join-Path $stageDir 'tools\LICENSE.txt') -Force
Copy-Item -LiteralPath (Join-Path $SourceRoot 'NOTICE') -Destination (Join-Path $stageDir 'tools\NOTICE.txt') -Force

if ($StageOnly) {
    Write-Host "Staged in $stageDir"
    Write-Output $stageDir
    return
}

if (-not (Get-Command 'choco' -ErrorAction SilentlyContinue)) {
    throw 'Chocolatey was not found. Install it from https://chocolatey.org/install, or pass -StageOnly.'
}

$outputPath = (Resolve-Path -LiteralPath $OutputDir).Path
& choco pack (Join-Path $stageDir 'thrift.nuspec') --output-directory $outputPath
if ($LASTEXITCODE -ne 0) {
    throw "choco pack failed with exit code $LASTEXITCODE"
}

$package = Join-Path $outputPath "thrift.$Version.nupkg"
if (-not (Test-Path -LiteralPath $package)) {
    throw "choco pack reported success but $package is not there."
}

Write-Host "Built $package"
Write-Output $package
