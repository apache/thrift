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
    Installs and uninstalls the Windows Thrift compiler installer.

.DESCRIPTION
    Drives a built installer through a silent per user install and uninstall
    and checks what it did to the machine.

    The part worth testing is the PATH handling. Inno Setup can append to PATH
    on its own, but taking the entry out again on uninstall is code in
    thrift.iss, and code that edits PATH gets one shot at being right. The
    test therefore puts another entry behind the Thrift one before it
    uninstalls, so the entry being removed is in the middle of the list, and
    checks afterwards that only the Thrift entry went away.

    This modifies the PATH of the user it runs as. Run it on a throwaway
    machine or in CI, not on a workstation.

.PARAMETER Installer
    The installer to test.

.PARAMETER ExpectedVersion
    Version thrift.exe is expected to report. Optional.

.EXAMPLE
    pwsh build\windows\installer\test-installer.ps1 -Installer dist\thrift-0.26.0-setup.exe
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string] $Installer,
    [string] $ExpectedVersion = ''
)

$ErrorActionPreference = 'Stop'

$EnvironmentKey = 'HKCU:\Environment'

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

function Get-UserPath {
    $value = (Get-ItemProperty -Path $EnvironmentKey -Name 'Path' -ErrorAction SilentlyContinue).Path
    if ($null -eq $value) { return '' }
    return $value
}

function Set-UserPath {
    param([string] $Value)

    if ($Value -eq '') {
        Remove-ItemProperty -Path $EnvironmentKey -Name 'Path' -ErrorAction SilentlyContinue
    }
    else {
        # ExpandString keeps entries such as %USERPROFILE% working.
        Set-ItemProperty -Path $EnvironmentKey -Name 'Path' -Value $Value -Type ExpandString
    }
}

function Test-PathContains {
    param([string] $Path, [string] $Entry)

    return (";$Path;".ToUpperInvariant()).Contains(";$Entry;".ToUpperInvariant())
}

function Invoke-Silently {
    param([string] $Executable, [string[]] $Arguments)

    $process = Start-Process -FilePath $Executable -ArgumentList $Arguments -Wait -PassThru
    return $process.ExitCode
}

if (-not (Test-Path -LiteralPath $Installer)) {
    throw "No such file: $Installer"
}
$Installer = (Resolve-Path -LiteralPath $Installer).Path

$installDir = Join-Path ([System.IO.Path]::GetTempPath()) ("thrift-installer-test-" + [System.Guid]::NewGuid().ToString('N'))
$sentinel = Join-Path $installDir 'sentinel-after-thrift'
$pathBefore = Get-UserPath

Write-Host "Testing $Installer"
Write-Host "  install directory: $installDir"

try {
    # ---- install -----------------------------------------------------------

    $exitCode = Invoke-Silently $Installer @(
        '/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART', '/CURRENTUSER',
        '/TASKS=modifypath', "/DIR=$installDir"
    )
    Assert-True 'the installer succeeds' ($exitCode -eq 0) "exit code $exitCode"

    $compiler = Join-Path $installDir 'thrift.exe'
    Assert-True 'thrift.exe is installed' (Test-Path -LiteralPath $compiler)
    Assert-True 'LICENSE is installed' (Test-Path -LiteralPath (Join-Path $installDir 'LICENSE.txt'))
    Assert-True 'NOTICE is installed' (Test-Path -LiteralPath (Join-Path $installDir 'NOTICE.txt'))

    if (Test-Path -LiteralPath $compiler) {
        $reported = (& $compiler -version) -join ' '
        Assert-True 'the installed compiler runs' ($LASTEXITCODE -eq 0) "exit code $LASTEXITCODE"
        if ($ExpectedVersion) {
            Assert-True "the installed compiler reports $ExpectedVersion" `
                ($reported -match [regex]::Escape($ExpectedVersion)) "reported '$reported'"
        }
    }

    $pathAfterInstall = Get-UserPath
    Assert-True 'the install directory is on PATH' (Test-PathContains $pathAfterInstall $installDir) `
        "PATH is '$pathAfterInstall'"

    # ---- uninstall ---------------------------------------------------------

    # Put an entry behind the Thrift one, so the uninstaller has to remove an
    # entry from the middle of the list rather than from the end.
    Set-UserPath ((Get-UserPath) + ';' + $sentinel)

    $uninstaller = Join-Path $installDir 'unins000.exe'
    Assert-True 'an uninstaller was written' (Test-Path -LiteralPath $uninstaller)

    if (Test-Path -LiteralPath $uninstaller) {
        $exitCode = Invoke-Silently $uninstaller @('/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART')

        # The uninstaller restarts itself from the temp directory, so the
        # process that was waited on returns before the work is done.
        $deadline = (Get-Date).AddSeconds(120)
        while ((Test-Path -LiteralPath $compiler) -and ((Get-Date) -lt $deadline)) {
            Start-Sleep -Milliseconds 500
        }

        Assert-True 'the uninstaller succeeds' ($exitCode -eq 0) "exit code $exitCode"
        Assert-True 'thrift.exe is gone' (-not (Test-Path -LiteralPath $compiler))
    }

    $pathAfterUninstall = Get-UserPath
    Assert-True 'the install directory is off PATH again' `
        (-not (Test-PathContains $pathAfterUninstall $installDir)) "PATH is '$pathAfterUninstall'"
    Assert-True 'the entry behind it survived' (Test-PathContains $pathAfterUninstall $sentinel) `
        "PATH is '$pathAfterUninstall'"

    foreach ($entry in ($pathBefore -split ';' | Where-Object { $_ -ne '' })) {
        if (-not (Test-PathContains $pathAfterUninstall $entry)) {
            Assert-True "the pre-existing entry $entry survived" $false "PATH is '$pathAfterUninstall'"
        }
    }
    Assert-True 'no pre-existing PATH entry was lost' `
        (@($pathBefore -split ';' | Where-Object { $_ -ne '' -and -not (Test-PathContains $pathAfterUninstall $_) }).Count -eq 0)
}
finally {
    Set-UserPath $pathBefore
    Remove-Item -LiteralPath $installDir -Recurse -Force -ErrorAction SilentlyContinue
}

Write-Host ''
if ($failures.Count -gt 0) {
    Write-Host "$($failures.Count) of $($failures.Count + $passed) checks failed:"
    foreach ($failure in $failures) { Write-Host "  $failure" }
    exit 1
}

Write-Host "$passed checks passed."
exit 0
