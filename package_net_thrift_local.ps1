<#
Builds and creates local NuGet packages for Apache Thrift without pushing anywhere.

Usage examples:
  ./package_net_thrift_local.ps1
  ./package_net_thrift_local.ps1 -SkipBuild
  ./package_net_thrift_local.ps1 -OutputDir .\artifacts\nuget
#>

[CmdletBinding()]
param(
    [string]$OutputDir,
    [switch]$SkipBuild,
    [switch]$NoSymbols
)

# If this script variable is left blank, an optional parameter value is used.
$PACKAGE_OUTPUT_DIR = "D:\AnsysDevs\local_nuget"

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Write-Pass([string]$Message) {
    Write-Host "[OK] $Message" -ForegroundColor Green
}

function Write-Fail([string]$Message) {
    Write-Host "[FAIL] $Message" -ForegroundColor Red
}

function Stop-On-Fail([string]$Message) {
    Write-Fail $Message
    exit 1
}

function Get-CommandPathOrNull([string]$Name) {
    $cmd = Get-Command $Name -ErrorAction SilentlyContinue
    if ($null -eq $cmd) {
        return $null
    }
    return $cmd.Source
}

$repoRoot = Split-Path -Parent $PSCommandPath
$buildScript = Join-Path $repoRoot "build_net_thrift.ps1"
$nuspec = Join-Path $repoRoot "ApacheThrift.nuspec"
$thriftProject = Join-Path $repoRoot "lib\netstd\Thrift\Thrift.csproj"
$releaseOutput = Join-Path $repoRoot "lib\netstd\Thrift\bin\Release\netstandard2.0"

if ([string]::IsNullOrWhiteSpace($PACKAGE_OUTPUT_DIR)) {
    if (-not [string]::IsNullOrWhiteSpace($OutputDir)) {
        $PACKAGE_OUTPUT_DIR = $OutputDir
    }
    else {
        $PACKAGE_OUTPUT_DIR = Join-Path $repoRoot "out\nuget-local"
    }
}

if (-not [System.IO.Path]::IsPathRooted($PACKAGE_OUTPUT_DIR)) {
    $PACKAGE_OUTPUT_DIR = Join-Path $repoRoot $PACKAGE_OUTPUT_DIR
}

$OutputDir = [System.IO.Path]::GetFullPath($PACKAGE_OUTPUT_DIR)

if (-not (Test-Path $nuspec -PathType Leaf)) {
    Stop-On-Fail "Nuspec file not found: $nuspec"
}

$nugetPath = Get-CommandPathOrNull "nuget"
if ([string]::IsNullOrWhiteSpace($nugetPath)) {
    Stop-On-Fail "nuget.exe was not found in PATH. Add nuget.exe to PATH and re-run."
}
Write-Pass "nuget found: $nugetPath"

if (-not $SkipBuild) {
    if (-not (Test-Path $buildScript -PathType Leaf)) {
        Stop-On-Fail "Build script not found: $buildScript"
    }

    Write-Host "`n==> Building prerequisites and netstd artifacts" -ForegroundColor Cyan
    & $buildScript
}
else {
    Write-Host "`n==> Skipping build (requested)" -ForegroundColor Yellow
}

if (-not (Test-Path $releaseOutput -PathType Container) -or
    -not (Get-ChildItem -Path $releaseOutput -File -ErrorAction SilentlyContinue)) {
    $dotnetPath = Get-CommandPathOrNull "dotnet"
    if ([string]::IsNullOrWhiteSpace($dotnetPath)) {
        Stop-On-Fail "dotnet was not found in PATH. Required to build Release netstandard2.0 artifacts."
    }

    if (-not (Test-Path $thriftProject -PathType Leaf)) {
        Stop-On-Fail "Thrift project file not found: $thriftProject"
    }

    Write-Host "`n==> Building ApacheThrift package artifacts (Release netstandard2.0)" -ForegroundColor Cyan
    & $dotnetPath build $thriftProject -c Release -f netstandard2.0
}

if (-not (Test-Path $releaseOutput -PathType Container)) {
    Stop-On-Fail "Release output folder not found: $releaseOutput"
}

if (-not (Get-ChildItem -Path $releaseOutput -File -ErrorAction SilentlyContinue)) {
    Stop-On-Fail "Release output folder is empty: $releaseOutput"
}

New-Item -Path $OutputDir -ItemType Directory -Force | Out-Null
Write-Pass "Local package output directory: $OutputDir"

$packArgs = @(
    "pack",
    $nuspec,
    "-OutputDirectory", $OutputDir
)

if (-not $NoSymbols) {
    $packArgs += @("-Symbols", "-SymbolPackageFormat", "snupkg")
}

Write-Host "`n==> Packing local NuGet package" -ForegroundColor Cyan
& $nugetPath @packArgs

$packages = @(Get-ChildItem -Path $OutputDir -File -Filter "ApacheThrift*.nupkg" -ErrorAction SilentlyContinue |
    Sort-Object LastWriteTime -Descending)
$symbols = @(Get-ChildItem -Path $OutputDir -File -Filter "ApacheThrift*.snupkg" -ErrorAction SilentlyContinue |
    Sort-Object LastWriteTime -Descending)

if ($packages.Count -eq 0) {
    Stop-On-Fail "No .nupkg file was created in: $OutputDir"
}

Write-Pass "Package creation complete. Local artifacts:"
$packages | ForEach-Object { Write-Host "  $($_.FullName)" }
$symbols | ForEach-Object { Write-Host "  $($_.FullName)" }

Write-Host "`nNo package push was performed." -ForegroundColor Cyan
