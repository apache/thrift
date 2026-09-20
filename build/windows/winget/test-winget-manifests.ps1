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
    Tests build/windows/build-winget-manifests.ps1.

.DESCRIPTION
    Checks what the renderer puts in the manifests and what it refuses to do.
    Whether the result matches the WinGet schemas is a separate question,
    answered by validate_manifests.py.

    Runs anywhere PowerShell does and needs no network: the checksum is passed
    in rather than computed from a download.

.EXAMPLE
    pwsh build/windows/winget/test-winget-manifests.ps1
#>

[CmdletBinding()]
param()

$ErrorActionPreference = 'Stop'

$renderer = Join-Path $PSScriptRoot '..\build-winget-manifests.ps1'
if (-not (Test-Path -LiteralPath $renderer)) {
    throw "build-winget-manifests.ps1 not found next to $PSCommandPath"
}

$workDir = Join-Path ([System.IO.Path]::GetTempPath()) ("thrift-winget-tests-" + [System.Guid]::NewGuid().ToString('N'))

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

function Assert-Throws {
    param([string] $Name, [scriptblock] $Action, [string] $Matching = '')

    try {
        & $Action | Out-Null
        Assert-True $Name $false 'it was accepted'
    }
    catch {
        if ($Matching -and ($_.Exception.Message -notmatch $Matching)) {
            Assert-True $Name $false "wrong message: $($_.Exception.Message)"
        }
        else {
            Assert-True $Name $true
        }
    }
}

$version = '1.2.3'
$sha = 'B' * 64
$url = 'https://archive.apache.org/dist/thrift/1.2.3/thrift-1.2.3-setup.exe'
$date = '2026-09-20'

Write-Host 'build-winget-manifests.ps1'

try {
    $outputDir = Join-Path $workDir 'out'
    $targetDir = & $renderer -Version $version -Sha256 $sha -ReleaseDate $date -OutputDir $outputDir |
        Select-Object -Last 1

    # wingetcreate and winget-pkgs both expect this layout.
    $expectedDir = Join-Path $outputDir "manifests/a/Apache/Thrift/$version"
    Assert-True 'the manifests land in manifests/a/Apache/Thrift/<version>' `
        ((Resolve-Path -LiteralPath $targetDir).Path -eq (Resolve-Path -LiteralPath $expectedDir).Path) `
        "got $targetDir"

    $names = @('Apache.Thrift.yaml', 'Apache.Thrift.installer.yaml', 'Apache.Thrift.locale.en-US.yaml')
    foreach ($name in $names) {
        Assert-True "$name is written" (Test-Path -LiteralPath (Join-Path $targetDir $name))
    }

    $installer = Get-Content -LiteralPath (Join-Path $targetDir 'Apache.Thrift.installer.yaml') -Raw
    $locale = Get-Content -LiteralPath (Join-Path $targetDir 'Apache.Thrift.locale.en-US.yaml') -Raw
    $versionManifest = Get-Content -LiteralPath (Join-Path $targetDir 'Apache.Thrift.yaml') -Raw

    # A placeholder left in place would be submitted verbatim.
    foreach ($name in $names) {
        $content = Get-Content -LiteralPath (Join-Path $targetDir $name) -Raw
        Assert-True "$name holds no placeholders" ($content -notmatch '__[A-Z0-9_]+__')
    }

    Assert-True 'the version reaches every manifest' `
        (($installer -match "PackageVersion: $([regex]::Escape($version))") -and
         ($locale -match "PackageVersion: $([regex]::Escape($version))") -and
         ($versionManifest -match "PackageVersion: $([regex]::Escape($version))"))

    Assert-True 'the checksum reaches the installer manifest' `
        ($installer -match "InstallerSha256: '$sha'")
    Assert-True 'the release date reaches the installer manifest' `
        ($installer -match "ReleaseDate: $([regex]::Escape($date))")

    # The compiler needs the Visual C++ redistributable; without this a WinGet
    # install would succeed and then not run.
    Assert-True 'the redistributable is declared as a dependency' `
        ($installer -match 'PackageIdentifier:\s*Microsoft\.VCRedist\.2015\+\.x64') $installer

    # downloads.apache.org only carries the current release, so a manifest
    # naming it stops working at the next one.
    Assert-True 'the installer URL defaults to the Apache archive' `
        ($installer -match "InstallerUrl: $([regex]::Escape($url))") $installer
    Assert-True 'the installer URL is not downloads.apache.org' `
        ($installer -notmatch 'downloads\.apache\.org')

    # A checksum of nothing but digits is still a string. Unquoted, YAML reads
    # it as a number and the WinGet schema rejects it.
    $numeric = '0' * 64
    $numericDir = & $renderer -Version $version -Sha256 $numeric -ReleaseDate $date `
        -OutputDir (Join-Path $workDir 'numeric') | Select-Object -Last 1
    $numericInstaller = Get-Content -LiteralPath (Join-Path $numericDir 'Apache.Thrift.installer.yaml') -Raw
    Assert-True 'an all digit checksum stays quoted' `
        ($numericInstaller -match "InstallerSha256: '$numeric'") $numericInstaller

    $custom = & $renderer -Version $version -Sha256 $sha -ReleaseDate $date `
        -InstallerUrl 'https://example.invalid/thrift.exe' `
        -OutputDir (Join-Path $workDir 'custom') | Select-Object -Last 1
    $customInstaller = Get-Content -LiteralPath (Join-Path $custom 'Apache.Thrift.installer.yaml') -Raw
    Assert-True 'a given installer URL is used' `
        ($customInstaller -match 'InstallerUrl: https://example\.invalid/thrift\.exe')

    # winget-pkgs manifests are UTF-8 without a BOM, and PowerShell is happy to
    # write both a BOM and CRLF if left to itself.
    $bytes = [System.IO.File]::ReadAllBytes((Join-Path $targetDir 'Apache.Thrift.yaml'))
    Assert-True 'the manifests have no byte order mark' `
        (-not (($bytes.Length -ge 3) -and ($bytes[0] -eq 0xEF) -and ($bytes[1] -eq 0xBB) -and ($bytes[2] -eq 0xBF)))
    Assert-True 'the manifests use LF line endings' ($versionManifest -notmatch "`r")

    # ---- what it refuses ---------------------------------------------------

    Assert-Throws 'a version that is not major.minor.patch is refused' `
        { & $renderer -Version '1.2' -Sha256 $sha -OutputDir (Join-Path $workDir 'bad1') } 'major.minor.patch'

    Assert-Throws 'a checksum that is not 64 hex digits is refused' `
        { & $renderer -Version $version -Sha256 'nope' -OutputDir (Join-Path $workDir 'bad2') } 'hexadecimal'

    Assert-Throws 'a release date that is not yyyy-MM-dd is refused' `
        { & $renderer -Version $version -Sha256 $sha -ReleaseDate '20.09.2026' -OutputDir (Join-Path $workDir 'bad3') } 'yyyy-MM-dd'
}
finally {
    Remove-Item -LiteralPath $workDir -Recurse -Force -ErrorAction SilentlyContinue
}

Write-Host ''
if ($failures.Count -gt 0) {
    Write-Host "$($failures.Count) of $($failures.Count + $passed) checks failed:"
    foreach ($failure in $failures) { Write-Host "  $failure" }
    exit 1
}

Write-Host "$passed checks passed."
exit 0
