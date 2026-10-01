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
    Tests for get-voted-compiler.ps1.

.DESCRIPTION
    Signs stand-in executables with a throwaway GPG key and lets
    get-voted-compiler.ps1 check them the way it checks a download from
    dist.apache.org. Nothing is downloaded: the URL checks are exercised with
    URLs the script has to refuse before it fetches anything.

    The point of these tests is that the checks must be able to *fail*. A
    signature check that passes everything would package whatever happens to
    be at the URL.

    Needs gpg on PATH, or pass -Gpg.

.EXAMPLE
    pwsh build/windows/get-voted-compiler-tests.ps1
#>

[CmdletBinding()]
param(
    [string] $Gpg = 'gpg'
)

$ErrorActionPreference = 'Stop'

$scriptUnderTest = Join-Path $PSScriptRoot 'get-voted-compiler.ps1'
if (-not (Test-Path -LiteralPath $scriptUnderTest)) {
    throw "get-voted-compiler.ps1 not found next to $PSCommandPath"
}

$workDir = Join-Path ([System.IO.Path]::GetTempPath()) ("thrift-voted-compiler-tests-" + [System.Guid]::NewGuid().ToString('N'))
New-Item -Path $workDir -ItemType Directory -Force | Out-Null

$failures = [System.Collections.Generic.List[string]]::new()
$passed = 0
$homes = [System.Collections.Generic.List[string]]::new()

function Invoke-Gpg {
    param([string] $GpgHome, [string[]] $Arguments)

    $output = & $Gpg --batch --quiet --no-permission-warning --homedir $GpgHome --pinentry-mode loopback --passphrase '' @Arguments 2>&1
    if ($LASTEXITCODE -ne 0) {
        throw "gpg $($Arguments -join ' ') failed: $output"
    }
}

function New-Signer {
    param([string] $Name)

    $gpgHome = Join-Path $workDir "gnupg-$Name"
    New-Item -Path $gpgHome -ItemType Directory -Force | Out-Null
    $script:homes.Add($gpgHome)
    Invoke-Gpg $gpgHome @('--quick-gen-key', "$Name <$Name@example.invalid>", 'ed25519', 'sign', 'never')
    return $gpgHome
}

function New-Candidate {
    # Lays out what a release directory on dist.apache.org holds for the
    # compiler: the executable, its detached signature and its checksums.
    param(
        [string] $Case,
        [string] $Signer,
        [string] $FileName = 'thrift-1.2.3.exe',
        [string[]] $Checksums = @('SHA512', 'SHA256'),
        [switch] $NoSignature
    )

    $dir = Join-Path $workDir $Case
    New-Item -Path $dir -ItemType Directory -Force | Out-Null
    $exe = Join-Path $dir $FileName
    Set-Content -LiteralPath $exe -Value "stand-in for the compiler, case $Case" -NoNewline
    if (-not $NoSignature) {
        Invoke-Gpg $Signer @('--armor', '--detach-sign', '--output', "$exe.asc", $exe)
    }
    foreach ($algorithm in $Checksums) {
        $hash = (Get-FileHash -Algorithm $algorithm -LiteralPath $exe).Hash.ToLowerInvariant()
        # The format sha512sum writes in binary mode, which is what the
        # release instructions produce.
        Set-Content -LiteralPath "$exe.$($algorithm.ToLowerInvariant())" -Value "$hash *$FileName"
    }
    return $exe
}

function Invoke-Script {
    param([hashtable] $ScriptArgs)

    # *>&1 folds every stream into the pipeline so the script under test does
    # not print over the test report, and so its error records cannot become
    # terminating errors here.
    $transcript = & $scriptUnderTest @ScriptArgs -Gpg $Gpg *>&1 | Out-String
    Write-Verbose $transcript
    return $LASTEXITCODE
}

function Assert-True {
    param([string] $Name, [bool] $Condition, [string] $Detail = '')

    if ($Condition) {
        Write-Host "  PASS  $Name"
        $script:passed++
    }
    else {
        Write-Host "  FAIL  $Name $Detail"
        $script:failures.Add($Name)
    }
}

function Assert-Rejected {
    param([string] $Name, [hashtable] $ScriptArgs)

    $outputDir = Join-Path $workDir ("out-" + [System.Guid]::NewGuid().ToString('N'))
    $ScriptArgs['OutputDir'] = $outputDir
    $exitCode = Invoke-Script $ScriptArgs
    $leftBehind = @(Get-ChildItem -LiteralPath $outputDir -Filter '*.exe' -ErrorAction SilentlyContinue).Count
    Assert-True $Name (($exitCode -ne 0) -and ($leftBehind -eq 0)) "(exit $exitCode, $leftBehind executable(s) written)"
}

try {
    $releaseManager = New-Signer 'release-manager'
    $outsider = New-Signer 'outsider'

    # KEYS holds the release manager's key only.
    $keys = Join-Path $workDir 'KEYS'
    $exported = & $Gpg --batch --no-permission-warning --homedir $releaseManager --armor --export
    Set-Content -LiteralPath $keys -Value $exported

    Write-Host 'Executables that must pass'

    $good = New-Candidate -Case 'good' -Signer $releaseManager
    $outputDir = Join-Path $workDir 'out-good'
    $githubOutput = Join-Path $workDir 'github-output'
    $env:GITHUB_OUTPUT = $githubOutput
    try {
        $exitCode = Invoke-Script @{ Path = $good; KeysPath = $keys; OutputDir = $outputDir }
    }
    finally {
        Remove-Item Env:GITHUB_OUTPUT
    }
    $copied = Join-Path $outputDir 'thrift-1.2.3.exe'
    $goodHash = (Get-FileHash -Algorithm SHA256 -LiteralPath $good).Hash.ToLowerInvariant()
    Assert-True 'a signed executable with matching checksums passes' ($exitCode -eq 0) "(exit $exitCode)"
    Assert-True 'it is copied to the output directory unchanged' `
        ((Test-Path -LiteralPath $copied) -and ((Get-FileHash -Algorithm SHA256 -LiteralPath $copied).Hash.ToLowerInvariant() -eq $goodHash))
    $outputs = if (Test-Path -LiteralPath $githubOutput) { Get-Content -LiteralPath $githubOutput } else { @() }
    Assert-True 'the version goes to GITHUB_OUTPUT' ($outputs -contains 'version=1.2.3') "(got: $($outputs -join ', '))"
    Assert-True 'the sha256 goes to GITHUB_OUTPUT' ($outputs -contains "sha256=$goodHash") "(got: $($outputs -join ', '))"

    # 0.25.0 was published with .sha256 next to the executable, and no .sha512.
    $sha256Only = New-Candidate -Case 'sha256-only' -Signer $releaseManager -Checksums @('SHA256')
    Assert-True 'a .sha256 alone is enough' `
        ((Invoke-Script @{ Path = $sha256Only; KeysPath = $keys; OutputDir = (Join-Path $workDir 'out-sha256') }) -eq 0)

    Write-Host 'Executables that must be refused'

    $tampered = New-Candidate -Case 'tampered' -Signer $releaseManager
    Add-Content -LiteralPath $tampered -Value ' changed after signing' -NoNewline
    Assert-Rejected 'changed after signing' @{ Path = $tampered; KeysPath = $keys }

    # Whoever can replace the executable can usually replace its checksums too;
    # only the signature catches that.
    $resummed = New-Candidate -Case 'resummed' -Signer $releaseManager
    Add-Content -LiteralPath $resummed -Value ' changed after signing' -NoNewline
    foreach ($algorithm in @('SHA512', 'SHA256')) {
        $hash = (Get-FileHash -Algorithm $algorithm -LiteralPath $resummed).Hash.ToLowerInvariant()
        Set-Content -LiteralPath "$resummed.$($algorithm.ToLowerInvariant())" -Value "$hash *thrift-1.2.3.exe"
    }
    Assert-Rejected 'changed after signing, checksums redone' @{ Path = $resummed; KeysPath = $keys }

    $wrongSha512 = New-Candidate -Case 'wrong-sha512' -Signer $releaseManager
    Set-Content -LiteralPath "$wrongSha512.sha512" -Value (('0' * 128) + ' *thrift-1.2.3.exe')
    Assert-Rejected 'one of two checksums is wrong' @{ Path = $wrongSha512; KeysPath = $keys }

    $foreign = New-Candidate -Case 'foreign' -Signer $outsider
    Assert-Rejected 'signed by a key that is not in KEYS' @{ Path = $foreign; KeysPath = $keys }

    $unsigned = New-Candidate -Case 'unsigned' -Signer $releaseManager -NoSignature
    Assert-Rejected 'no signature' @{ Path = $unsigned; KeysPath = $keys }

    $unsummed = New-Candidate -Case 'unsummed' -Signer $releaseManager -Checksums @()
    Assert-Rejected 'no checksum' @{ Path = $unsummed; KeysPath = $keys }

    $installer = New-Candidate -Case 'installer' -Signer $releaseManager -FileName 'thrift-1.2.3-setup.exe'
    Assert-Rejected 'not named thrift-<version>.exe' @{ Path = $installer; KeysPath = $keys }

    Write-Host 'URLs that must be refused before anything is downloaded'

    foreach ($url in @(
            'https://example.com/repos/dist/release/thrift/1.2.3/thrift-1.2.3.exe',
            'http://dist.apache.org/repos/dist/release/thrift/1.2.3/thrift-1.2.3.exe',
            'https://dist.apache.org/repos/dist/release/other/1.2.3/thrift-1.2.3.exe',
            'https://dist.apache.org/repos/dist/release/thrift/1.2.3/thrift-1.2.3-setup.exe',
            'https://dist.apache.org/repos/dist/release/thrift/1.2.3/thrift-1.2.3.exe?x=1',
            'https://dist.apache.org.example.com/repos/dist/release/thrift/1.2.3/thrift-1.2.3.exe'
        )) {
        Assert-Rejected "URL $url" @{ Url = $url }
    }
}
finally {
    # Each home started an agent of its own.
    if (Get-Command 'gpgconf' -ErrorAction SilentlyContinue) {
        foreach ($gpgHome in $homes) {
            & gpgconf --homedir $gpgHome --kill gpg-agent 2>$null
        }
    }
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
