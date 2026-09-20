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
    Tests build/windows/build-chocolatey-package.ps1.

.DESCRIPTION
    Renders the package with known inputs and checks what came out: whether
    the download URL and checksum reached the install script, whether the
    licence files are there, and what the script refuses to build.

    Runs anywhere PowerShell does and needs neither Chocolatey nor the
    network: the checksum is passed in rather than computed from a download.

.PARAMETER Package
    An already packed .nupkg to inspect as well. Optional; the packing itself
    needs Chocolatey, so it only happens in CI.

.EXAMPLE
    pwsh build/windows/chocolatey/test-chocolatey-package.ps1
#>

[CmdletBinding()]
param(
    [string] $Package = ''
)

$ErrorActionPreference = 'Stop'

$builder = Join-Path $PSScriptRoot '..\build-chocolatey-package.ps1'
if (-not (Test-Path -LiteralPath $builder)) {
    throw "build-chocolatey-package.ps1 not found next to $PSCommandPath"
}

$workDir = Join-Path ([System.IO.Path]::GetTempPath()) ("thrift-choco-tests-" + [System.Guid]::NewGuid().ToString('N'))

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
$sha = ('D' * 64)

Write-Host 'build-chocolatey-package.ps1'

try {
    $stageDir = & $builder -Version $version -Sha256 $sha -OutputDir (Join-Path $workDir 'out') -StageOnly |
        Select-Object -Last 1

    $nuspecPath = Join-Path $stageDir 'thrift.nuspec'
    $installPath = Join-Path $stageDir 'tools\chocolateyinstall.ps1'

    Assert-True 'a nuspec is rendered' (Test-Path -LiteralPath $nuspecPath)
    Assert-True 'an install script is rendered' (Test-Path -LiteralPath $installPath)
    Assert-True 'no template is left in the staged package' `
        (@(Get-ChildItem -Path $stageDir -Filter '*.in' -Recurse).Count -eq 0)

    # The nuspec has to parse, or choco pack fails at release time.
    $nuspec = [xml](Get-Content -LiteralPath $nuspecPath -Raw)
    Assert-True 'the package id is thrift' ($nuspec.package.metadata.id -eq 'thrift') `
        $nuspec.package.metadata.id
    Assert-True 'the nuspec carries the version' ($nuspec.package.metadata.version -eq $version) `
        $nuspec.package.metadata.version
    Assert-True 'the nuspec names the licence' `
        ($nuspec.package.metadata.licenseUrl -match 'apache\.org/licenses/LICENSE-2\.0')

    # The compiler needs the Visual C++ redistributable; without this a
    # "choco install thrift" would succeed and then not run.
    $dependencies = @($nuspec.package.metadata.dependencies.dependency)
    Assert-True 'the redistributable is declared as a dependency' `
        (@($dependencies | Where-Object { $_.id -eq 'vcredist140' }).Count -eq 1) `
        (($dependencies | ForEach-Object { $_.id }) -join ', ')

    $install = Get-Content -LiteralPath $installPath -Raw
    Assert-True 'the install script holds no placeholders' ($install -notmatch '__[A-Z0-9_]+__')
    Assert-True 'the checksum reaches the install script' ($install -match "checksum64\s*=\s*'$sha'")
    Assert-True 'the checksum type is sha256' ($install -match "checksumType64\s*=\s*'sha256'")

    # downloads.apache.org only carries the current release, so a package
    # naming it stops installing at the next one.
    Assert-True 'the download URL defaults to the Apache archive' `
        ($install -match "url64bit\s*=\s*'https://archive\.apache\.org/dist/thrift/$version/thrift-$version-setup\.exe'") $install
    # Look at the value, not the whole file: the script explains in a comment
    # why downloads.apache.org is the wrong host.
    $urlValue = ([regex]::Match($install, "url64bit\s*=\s*'([^']*)'")).Groups[1].Value
    Assert-True 'the download URL is not downloads.apache.org' `
        ($urlValue -notmatch 'downloads\.apache\.org') $urlValue

    # Chocolatey installs machine wide, and the installer only touches the
    # machine PATH when both of these are passed.
    Assert-True 'the installer is run for all users' ($install -match '/ALLUSERS')
    Assert-True 'the installer is asked to modify PATH' ($install -match 'modifypath')
    Assert-True 'the installer is run silently' ($install -match '/VERYSILENT')

    foreach ($legal in @('tools\LICENSE.txt', 'tools\NOTICE.txt')) {
        $path = Join-Path $stageDir $legal
        Assert-True "$legal is staged and not empty" `
            ((Test-Path -LiteralPath $path) -and ((Get-Item -LiteralPath $path).Length -gt 0))
    }

    $custom = & $builder -Version $version -Sha256 $sha -InstallerUrl 'https://example.invalid/thrift.exe' `
        -OutputDir (Join-Path $workDir 'custom') -StageOnly | Select-Object -Last 1
    $customInstall = Get-Content -LiteralPath (Join-Path $custom 'tools\chocolateyinstall.ps1') -Raw
    Assert-True 'a given download URL is used' `
        ($customInstall -match "url64bit\s*=\s*'https://example\.invalid/thrift\.exe'")

    Assert-Throws 'a version that is not major.minor.patch is refused' `
        { & $builder -Version '1.2' -Sha256 $sha -OutputDir (Join-Path $workDir 'bad1') -StageOnly } 'major.minor.patch'

    Assert-Throws 'a checksum that is not 64 hex digits is refused' `
        { & $builder -Version $version -Sha256 'nope' -OutputDir (Join-Path $workDir 'bad2') -StageOnly } 'hexadecimal'

    # ---- an already packed package, when one was given ---------------------

    if ($Package) {
        Assert-True 'the packed package exists' (Test-Path -LiteralPath $Package) $Package
        if (Test-Path -LiteralPath $Package) {
            Add-Type -AssemblyName System.IO.Compression.FileSystem
            $archive = [System.IO.Compression.ZipFile]::OpenRead((Resolve-Path -LiteralPath $Package).Path)
            try {
                $entries = @($archive.Entries | ForEach-Object { $_.FullName })
                foreach ($required in @('thrift.nuspec', 'tools/chocolateyinstall.ps1',
                                        'tools/LICENSE.txt', 'tools/NOTICE.txt')) {
                    Assert-True "the packed package contains $required" ($entries -contains $required) `
                        ($entries -join ', ')
                }
                # The compiler is downloaded at install time, not carried here.
                Assert-True 'the packed package carries no executable' `
                    (@($entries | Where-Object { $_ -like '*.exe' }).Count -eq 0) ($entries -join ', ')
            }
            finally {
                $archive.Dispose()
            }
        }
    }
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
