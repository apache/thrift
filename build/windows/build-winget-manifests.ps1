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
    Renders the WinGet manifests for a released Windows Thrift compiler.

.DESCRIPTION
    Fills in the templates in build/windows/winget and writes the three files
    a submission to microsoft/winget-pkgs needs.

    Unless a checksum is given, the installer is downloaded from the very URL
    that goes into the manifest and hashed, so that the manifest cannot claim a
    checksum the published file does not have. That also means this cannot run
    before the release has reached the archive.

.PARAMETER Version
    The released version, for example 0.26.0.

.PARAMETER InstallerUrl
    Where the installer is published. Defaults to the Apache archive, which
    keeps every release; downloads.apache.org only carries the current one, so
    a manifest naming it stops working at the next release.

.PARAMETER Sha256
    The installer's SHA-256. Computed from the downloaded file when omitted.

.PARAMETER ReleaseDate
    The release date, yyyy-MM-dd. Today when omitted.

.PARAMETER OutputDir
    Where to write the manifests. They land in a
    manifests/a/Apache/Thrift/<version> subtree, which is the layout
    winget-pkgs uses and wingetcreate expects.

.EXAMPLE
    pwsh build/windows/build-winget-manifests.ps1 -Version 0.26.0
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string] $Version,
    [string] $InstallerUrl = '',
    [string] $Sha256 = '',
    [string] $ReleaseDate = '',
    [string] $OutputDir = 'winget-manifests'
)

$ErrorActionPreference = 'Stop'

$PackageIdentifier = 'Apache.Thrift'

if ($Version -notmatch '^\d+\.\d+\.\d+$') {
    throw "Version '$Version' is not major.minor.patch."
}
if (-not $InstallerUrl) {
    $InstallerUrl = "https://archive.apache.org/dist/thrift/$Version/thrift-$Version-setup.exe"
}
if (-not $ReleaseDate) {
    $ReleaseDate = (Get-Date).ToString('yyyy-MM-dd')
}
if ($ReleaseDate -notmatch '^\d{4}-\d{2}-\d{2}$') {
    throw "ReleaseDate '$ReleaseDate' is not yyyy-MM-dd."
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

$templateDir = Join-Path $PSScriptRoot 'winget'
$targetDir = Join-Path $OutputDir "manifests/a/Apache/Thrift/$Version"
New-Item -Path $targetDir -ItemType Directory -Force | Out-Null

$substitutions = @{
    '__VERSION__'          = $Version
    '__INSTALLER_URL__'    = $InstallerUrl
    '__INSTALLER_SHA256__' = $Sha256
    '__RELEASE_DATE__'     = $ReleaseDate
}

$written = [System.Collections.Generic.List[string]]::new()
foreach ($suffix in @('', '.installer', '.locale.en-US')) {
    $name = "$PackageIdentifier$suffix.yaml"
    $template = Join-Path $templateDir "$name.in"
    if (-not (Test-Path -LiteralPath $template)) {
        throw "Template not found: $template"
    }

    $content = Get-Content -LiteralPath $template -Raw
    foreach ($key in $substitutions.Keys) {
        $content = $content.Replace($key, $substitutions[$key])
    }

    # A placeholder that survived would be submitted verbatim, so stop here
    # rather than open a pull request with "__VERSION__" in it.
    $leftovers = [regex]::Matches($content, '__[A-Z0-9_]+__')
    if ($leftovers.Count -gt 0) {
        throw ("$name still holds placeholders after substitution: " +
               (($leftovers | ForEach-Object { $_.Value } | Sort-Object -Unique) -join ', '))
    }

    $target = Join-Path $targetDir $name
    # winget-pkgs manifests are UTF-8; no BOM, and no CRLF from PowerShell.
    [System.IO.File]::WriteAllText($target, ($content -replace "`r`n", "`n"),
        [System.Text.UTF8Encoding]::new($false))
    $written.Add($target)
    Write-Host "Wrote $target"
}

Write-Output $targetDir
