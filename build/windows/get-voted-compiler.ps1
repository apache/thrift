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
    Fetches the Windows Thrift compiler a vote covered, and checks it.

.DESCRIPTION
    The Windows installer of a release has to contain the thrift.exe the vote
    covered, not a new build of the same source: two builds are not the same
    bytes. This downloads that executable from the release candidate's or the
    release's directory on dist.apache.org, together with its detached GPG
    signature, its checksum files and the project's KEYS file, and checks all
    of them before anything is built from it:

    - every checksum file present (.sha512, .sha256) must match, and at least
      one must be there;
    - the signature must be good, and made with a key from KEYS.

    Only https://dist.apache.org/repos/dist/{dev,release}/thrift/ URLs naming a
    thrift-<major>.<minor>.<patch>.exe are accepted, so that a typo cannot
    package something that was never voted on.

    With -Path instead of -Url, the same checks run on files that are already
    local: the executable, with its .asc and checksum files next to it, and
    -KeysPath.

    The executable is copied into -OutputDir under its own name, and the
    version is printed. In GitHub Actions, version and sha256 also go to
    GITHUB_OUTPUT.

.PARAMETER Url
    The executable on dist.apache.org, for example
    https://dist.apache.org/repos/dist/dev/thrift/1.0.0-rc0/thrift-1.0.0.exe.

.PARAMETER Path
    A local executable to check instead.

.PARAMETER KeysUrl
    Where to get KEYS when downloading.

.PARAMETER KeysPath
    The KEYS file to check a local executable against.

.PARAMETER OutputDir
    Where to put the executable once it has passed. Created when it does not
    exist.

.PARAMETER Gpg
    The gpg executable. Defaults to gpg on PATH.

.EXAMPLE
    pwsh build/windows/get-voted-compiler.ps1 -Url https://dist.apache.org/repos/dist/release/thrift/0.25.0/thrift-0.25.0.exe -OutputDir voted
#>

[CmdletBinding(DefaultParameterSetName = 'Url')]
param(
    [Parameter(Mandatory = $true, ParameterSetName = 'Url')]
    [string] $Url,
    [Parameter(ParameterSetName = 'Url')]
    [string] $KeysUrl = 'https://downloads.apache.org/thrift/KEYS',

    [Parameter(Mandatory = $true, ParameterSetName = 'Path')]
    [string] $Path,
    [Parameter(Mandatory = $true, ParameterSetName = 'Path')]
    [string] $KeysPath,

    [string] $OutputDir = 'voted',
    [string] $Gpg = 'gpg'
)

$ErrorActionPreference = 'Stop'

$NamePattern = '^thrift-(\d+\.\d+\.\d+)\.exe$'
$UrlPattern = '^https://dist\.apache\.org/repos/dist/(dev|release)/thrift/[A-Za-z0-9._-]+/(thrift-\d+\.\d+\.\d+\.exe)$'
$ChecksumAlgorithms = @('SHA512', 'SHA256')

function Save-Download {
    param([string] $Uri, [string] $OutFile, [switch] $Optional)

    try {
        Invoke-WebRequest -Uri $Uri -OutFile $OutFile -UseBasicParsing
        return $true
    }
    catch {
        $status = $_.Exception.Response.StatusCode.value__
        if ($Optional -and $status -eq 404) {
            return $false
        }
        throw "Could not download $Uri : $($_.Exception.Message)"
    }
}

function Test-Checksums {
    param([string] $File)

    $checked = 0
    foreach ($algorithm in $ChecksumAlgorithms) {
        $sumFile = "$File.$($algorithm.ToLowerInvariant())"
        if (-not (Test-Path -LiteralPath $sumFile)) {
            continue
        }
        # sha512sum writes "<hash> *<name>" or "<hash>  <name>"; only the
        # hash matters.
        $expected = ((Get-Content -LiteralPath $sumFile -Raw).Trim() -split '\s+')[0].ToLowerInvariant()
        $actual = (Get-FileHash -Algorithm $algorithm -LiteralPath $File).Hash.ToLowerInvariant()
        if ($expected -ne $actual) {
            throw "$algorithm mismatch for $(Split-Path -Leaf $File): the checksum file says $expected, the file has $actual."
        }
        Write-Host "$algorithm matches: $actual"
        $checked++
    }
    if ($checked -eq 0) {
        throw "No checksum file next to $(Split-Path -Leaf $File); expected one of: $(($ChecksumAlgorithms | ForEach-Object { '.' + $_.ToLowerInvariant() }) -join ', ')."
    }
}

function Test-Signature {
    param([string] $File, [string] $Keys)

    $signature = "$File.asc"
    if (-not (Test-Path -LiteralPath $signature)) {
        throw "No signature $(Split-Path -Leaf $signature) next to the executable."
    }

    # A keyring of its own, holding nothing but KEYS, so that no key from the
    # machine this runs on can vouch for the file.
    $gpgHome = Join-Path ([System.IO.Path]::GetTempPath()) ("thrift-voted-compiler-gnupg-" + [System.Guid]::NewGuid().ToString('N'))
    New-Item -Path $gpgHome -ItemType Directory -Force | Out-Null
    try {
        # A KEYS file can hold keys gpg no longer imports. That is not an
        # error here as long as the key that signed the file is among the rest.
        $null = & $Gpg --batch --quiet --no-permission-warning --homedir $gpgHome --import $Keys 2>&1

        $status = & $Gpg --batch --no-permission-warning --homedir $gpgHome --status-fd 1 --verify $signature $File 2>$null
        $verifyExit = $LASTEXITCODE
        # GOODSIG only comes for a good signature by a key that has neither
        # expired nor been revoked; BADSIG, EXPKEYSIG, REVKEYSIG and ERRSIG
        # all mean no.
        $good = @($status | Where-Object { $_ -match '^\[GNUPG:\] GOODSIG ' })
        if ($verifyExit -ne 0 -or $good.Count -eq 0) {
            throw "The signature on $(Split-Path -Leaf $File) does not check out against KEYS: $($status -join ' | ')"
        }
        Write-Host "Signature good: $(($good[0] -split ' ', 4)[3])"
    }
    finally {
        if (Get-Command 'gpgconf' -ErrorAction SilentlyContinue) {
            & gpgconf --homedir $gpgHome --kill gpg-agent 2>$null
        }
        Remove-Item -LiteralPath $gpgHome -Recurse -Force -ErrorAction SilentlyContinue
    }
}

$downloadDir = $null
try {
    if ($PSCmdlet.ParameterSetName -eq 'Url') {
        if ($Url -notmatch $UrlPattern) {
            throw "Not a Windows compiler on dist.apache.org: $Url. Expected https://dist.apache.org/repos/dist/dev/thrift/<dir>/thrift-<version>.exe or .../release/thrift/<dir>/thrift-<version>.exe."
        }
        $name = $Matches[2]

        $downloadDir = Join-Path ([System.IO.Path]::GetTempPath()) ("thrift-voted-compiler-" + [System.Guid]::NewGuid().ToString('N'))
        New-Item -Path $downloadDir -ItemType Directory -Force | Out-Null
        $file = Join-Path $downloadDir $name
        $keys = Join-Path $downloadDir 'KEYS'

        Write-Host "Downloading $Url"
        $null = Save-Download -Uri $Url -OutFile $file
        $null = Save-Download -Uri "$Url.asc" -OutFile "$file.asc" -Optional
        foreach ($algorithm in $ChecksumAlgorithms) {
            $extension = '.' + $algorithm.ToLowerInvariant()
            $null = Save-Download -Uri "$Url$extension" -OutFile "$file$extension" -Optional
        }
        $null = Save-Download -Uri $KeysUrl -OutFile $keys
    }
    else {
        if (-not (Test-Path -LiteralPath $Path)) {
            throw "No such file: $Path"
        }
        $file = (Resolve-Path -LiteralPath $Path).Path
        $keys = $KeysPath
    }

    $name = Split-Path -Leaf $file
    if ($name -notmatch $NamePattern) {
        throw "$name is not named thrift-<major>.<minor>.<patch>.exe."
    }
    $version = $Matches[1]

    Test-Checksums -File $file
    Test-Signature -File $file -Keys $keys

    New-Item -Path $OutputDir -ItemType Directory -Force | Out-Null
    $target = Join-Path $OutputDir $name
    Copy-Item -LiteralPath $file -Destination $target -Force
    $sha256 = (Get-FileHash -Algorithm SHA256 -LiteralPath $target).Hash.ToLowerInvariant()

    if ($env:GITHUB_OUTPUT) {
        "version=$version" | Out-File -FilePath $env:GITHUB_OUTPUT -Append
        "sha256=$sha256" | Out-File -FilePath $env:GITHUB_OUTPUT -Append
    }
    Write-Host "Checked $name, version $version, sha256 $sha256"
    Write-Output $version
}
catch {
    Write-Host "::error::$($_.Exception.Message)"
    exit 1
}
finally {
    if ($downloadDir) {
        Remove-Item -LiteralPath $downloadDir -Recurse -Force -ErrorAction SilentlyContinue
    }
}
