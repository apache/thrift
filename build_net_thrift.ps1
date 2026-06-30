<#
This script checks all pre-requisite installs are present, in PATH, and at the
minimum version. If everything is set correctly, it builds the .NET thrift
library for this repository.
#>

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# If these script variables are left blank, environment variables are used.
# If script variables are populated, they override environment variables.
$THRIFT_REPOSITORY_LOC = "D:\AnsysDevs\src\thrift-security-namedpipe"
$BOOST_ROOT = "D:\AnsysDevs\tools\boost\boost_1_91_0"
$BOOST_LIBRARYDIR = ""

function Resolve-PathSetting([string]$ScriptValue, [string]$EnvironmentValue, [string]$FallbackValue) {
	if (-not [string]::IsNullOrWhiteSpace($ScriptValue)) {
		return $ScriptValue
	}

	if (-not [string]::IsNullOrWhiteSpace($EnvironmentValue)) {
		return $EnvironmentValue
	}

	return $FallbackValue
}

function Resolve-BoostLibraryDir([string]$ScriptValue, [string]$EnvironmentValue, [string]$ResolvedBoostRoot) {
	if (-not [string]::IsNullOrWhiteSpace($ScriptValue)) {
		return $ScriptValue
	}

	if (-not [string]::IsNullOrWhiteSpace($EnvironmentValue)) {
		return $EnvironmentValue
	}

	$preferred = Join-Path $ResolvedBoostRoot "lib64-msvc-14.3"
	if (Test-Path $preferred -PathType Container) {
		return $preferred
	}

	$detected = Get-ChildItem -Path $ResolvedBoostRoot -Directory -ErrorAction SilentlyContinue |
		Where-Object { $_.Name -match '^lib64-msvc-\d+\.\d+$' } |
		Sort-Object Name -Descending |
		Select-Object -First 1

	if ($null -ne $detected) {
		return $detected.FullName
	}

	return $preferred
}

$THRIFT_REPOSITORY_LOC = Resolve-PathSetting $THRIFT_REPOSITORY_LOC $env:THRIFT_REPOSITORY_LOC "D:\AnsysDevs\src\thrift-security-namedpipe"
$BOOST_ROOT = Resolve-PathSetting $BOOST_ROOT $env:BOOST_ROOT "D:\AnsysDevs\tools\boost\boost_1_91_0"
$BOOST_LIBRARYDIR = Resolve-BoostLibraryDir $BOOST_LIBRARYDIR $env:BOOST_LIBRARYDIR $BOOST_ROOT

$MIN_DOTNET_SDK = [Version]"10.0.0"
$MIN_CMAKE = [Version]"3.20.0"
$MIN_WIN_FLEX = [Version]"2.6.0"
$MIN_WIN_BISON = [Version]"3.7.0"
$MIN_BOOST = [Version]"1.56.0"

function Write-Pass([string]$Message) {
	Write-Host "[OK] $Message" -ForegroundColor Green
}

function Write-Fail([string]$Message) {
	Write-Host "[FAIL] $Message" -ForegroundColor Red
}

function Quit-On-Fail([string]$Message) {
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

function Parse-VersionOrNull([string]$Raw) {
	if ($Raw -match '(\d+\.\d+(?:\.\d+)?)') {
		return [Version]$Matches[1]
	}
	return $null
}

function Ensure-Versioned-Command(
	[string]$Name,
	[Version]$Minimum,
	[scriptblock]$VersionGetter
) {
	$path = Get-CommandPathOrNull $Name
	if ([string]::IsNullOrWhiteSpace($path)) {
		Quit-On-Fail "$Name is not in PATH."
	}

	$verText = & $VersionGetter
	if ([string]::IsNullOrWhiteSpace($verText)) {
		Quit-On-Fail "$Name is in PATH at '$path' but its version could not be determined."
	}

	$ver = Parse-VersionOrNull $verText
	if ($null -eq $ver) {
		Quit-On-Fail "$Name is in PATH at '$path' but its version string was not parseable: $verText"
	}

	if ($ver -lt $Minimum) {
		Quit-On-Fail "$Name version $ver is below required minimum $Minimum (PATH: $path)."
	}

	Write-Pass "$Name version $ver found in PATH ($path)."
}

function Ensure-Boost() {
	if (-not (Test-Path $BOOST_ROOT -PathType Container)) {
		Quit-On-Fail "BOOST_ROOT folder not found: $BOOST_ROOT"
	}

	if (-not (Test-Path $BOOST_LIBRARYDIR -PathType Container)) {
		Quit-On-Fail "BOOST_LIBRARYDIR folder not found: $BOOST_LIBRARYDIR. Set BOOST_LIBRARYDIR in this script or set `$env:BOOST_LIBRARYDIR."
	}

	$verHeader = Join-Path $BOOST_ROOT "boost\version.hpp"
	if (-not (Test-Path $verHeader -PathType Leaf)) {
		Quit-On-Fail "Cannot read Boost version. Missing file: $verHeader"
	}

	$headerText = Get-Content $verHeader -Raw
	if ($headerText -notmatch '#define\s+BOOST_LIB_VERSION\s+"(\d+)_(\d+)(?:_(\d+))?"') {
		Quit-On-Fail "Unable to parse Boost version from $verHeader"
	}

	$patch = if ([string]::IsNullOrEmpty($Matches[3])) { "0" } else { $Matches[3] }
	$boostVersion = [Version]("{0}.{1}.{2}" -f $Matches[1], $Matches[2], $patch)

	if ($boostVersion -lt $MIN_BOOST) {
		Quit-On-Fail "Boost version $boostVersion is below required minimum $MIN_BOOST (BOOST_ROOT: $BOOST_ROOT)."
	}

	Write-Pass "Boost version $boostVersion found (BOOST_ROOT: $BOOST_ROOT)."
}

function Run-Step([string]$Title, [scriptblock]$Action) {
	Write-Host "`n==> $Title" -ForegroundColor Cyan
	& $Action
}

if (-not (Test-Path $THRIFT_REPOSITORY_LOC -PathType Container)) {
	Quit-On-Fail "Repository folder not found: $THRIFT_REPOSITORY_LOC"
}

Write-Host "Checking prerequisites..." -ForegroundColor Cyan

Ensure-Versioned-Command "dotnet" $MIN_DOTNET_SDK { (& dotnet --version).Trim() }
Ensure-Versioned-Command "cmake" $MIN_CMAKE { ((& cmake --version | Select-Object -First 1)).Trim() }
Ensure-Versioned-Command "win_flex" $MIN_WIN_FLEX { ((& win_flex --version | Select-Object -First 1)).Trim() }
Ensure-Versioned-Command "win_bison" $MIN_WIN_BISON { ((& win_bison --version | Select-Object -First 1)).Trim() }
Ensure-Boost

Write-Pass "All prerequisite checks passed."

$buildDir = Join-Path $THRIFT_REPOSITORY_LOC "out\build"
$netstdDir = Join-Path $THRIFT_REPOSITORY_LOC "lib\netstd"
$slnx = Join-Path $netstdDir "Thrift.slnx"
$compilerOutputDir = Join-Path $buildDir "compiler\cpp\bin\Release"

Run-Step "Configure CMake" {
	cmake -S $THRIFT_REPOSITORY_LOC -B $buildDir -G "Visual Studio 17 2022" -A x64 `
		-DBOOST_ROOT=$BOOST_ROOT `
		-DBOOST_LIBRARYDIR=$BOOST_LIBRARYDIR
}

Run-Step "Build thrift compiler" {
	cmake --build $buildDir --config Release --target thrift-compiler copy-thrift-compiler
}

$thriftExe = Join-Path $compilerOutputDir "thrift.exe"
if (-not (Test-Path $thriftExe -PathType Leaf)) {
	Quit-On-Fail "Compiler build completed but thrift.exe was not found at: $thriftExe"
}

$env:PATH = "$compilerOutputDir;$env:PATH"
Write-Pass "thrift.exe is ready: $thriftExe"

if (-not (Test-Path $slnx -PathType Leaf)) {
	Quit-On-Fail "Solution file not found: $slnx"
}

Run-Step "Build .NET thrift (netstd)" {
	dotnet build $slnx
}

Write-Pass "Done. .NET thrift build completed successfully."

