#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
# *
#   http://www.apache.org/licenses/LICENSE-2.0
# *
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

param(
    [string[]] $TargetVersions    = @(),   # override NET_VERSIONS; when empty all defaults are tested
    [string]   $TargetFolder      = "",    # output dir; when empty uses system temp
    [string]   $ExtraThriftFiles  = ""     # additional .thrift search path; when empty none are added
)

#---- failing tests --------------------------------------------

# expected to fail at Thrift Compiler
$FAIL_THRIFT = @(
	"BrokenConstants.thrift",        # intended to break
	"Buses.thrift",                  # subdir includes don't work here
	"DuplicateImportsTest.thrift",   # subdir includes don't work here
	"Include.thrift",                # subdir includes don't work here
	"MaintenanceFacility.thrift",    # subdir includes don't work here
	"Transporters.thrift")           # subdir includes don't work here

# expected to fail at net Compiler
$FAIL_DOTNET = @(
)

# unexpected but known bugs (TODO: fix them)
$KNOWN_BUGS = @(
    )

$NET_VERSIONS = @(
	"net8",
	"net9",
	"net10"
)
if ($TargetVersions.Count -gt 0) { $NET_VERSIONS = $TargetVersions }


#---- consts --------------------------------------------

$EXE_EXT = ($OsType -eq "Windows") ? ".exe" : ""


#---- functions --------------------------------------------

function FindThriftExe() {
	# prefer debug over release over path
	$exe = "thrift" + $EXE_EXT
	write-host -nonewline Looking for $exe ...

	# if we have a freshly compiled one it might be a better choice
	@("Release","Debug") | foreach{
		if( test-path ([System.IO.Path]::Combine($ROOTDIR,"compiler","cpp",$_,$exe))) { $exe = [System.IO.Path]::Combine($ROOTDIR,"compiler","cpp",$_,$exe) }
		if( test-path ([System.IO.Path]::Combine($ROOTDIR,"compiler","cpp","compiler",$_,$exe))) { $exe = [System.IO.Path]::Combine($ROOTDIR,"compiler","cpp",$_,"compiler",$exe) }
	}
	# cmake build outputs take highest priority (Linux/macOS and cmake on Windows)
	@("cmake_build","cmake_build_compiler") | foreach{
		$candidate = [System.IO.Path]::Combine($ROOTDIR, $_, "compiler", "cpp", "bin", $exe)
		if( test-path $candidate) { $exe = $candidate }
	}

	return $exe
}


function FindDotnet() {
	# prefer debug over release over path
	$exe = "dotnet" + $EXE_EXT
	write-host -nonewline Looking for $exe ...

	# TODO: add arbitraily complex code to locate a suitable dotnet if it is not in the path

	return $exe
}


function InitializeFolder([string] $folder, [string] $pattern) {
	#write-host $folder\$pattern
	if(-not (test-path $folder)) {
		new-item $folder -type directory | out-null
	}
	pushd $folder
	gci . -include $pattern -recurse | foreach{ remove-item $_ }
	popd
}


function CopyFilesFrom([string] $source, $text) {
	#write-host "$source"
	$counter = 0
	if( ($source -ne "") -and (test-path $source)) {
		if( $text -ne $null) {
			write-host -nonewline -foregroundcolor yellow Copying $text ...
		}

		pushd $source
		# recurse dirs
		gci . -directory | foreach {
			$counter += CopyFilesFrom "$_"
		}
		# files within
		gci *.thrift -file | foreach {
			#write-host $_
			$name = $_.name
			copy-item $_ ([System.IO.Path]::Combine($TARGET, $name))
			$counter++
		}
		popd

		if( $text -ne $null) {
			write-host -foregroundcolor yellow $counter files
		}
	}
	return $counter
}

function TestIdlFile([string] $idl) {
	# expected to fail at Thrift Compiler
	$filter = $false
	$FAIL_THRIFT | foreach {
		if( $idl -eq $_) {
			$filter = $true
			write-host "Skipping $_"
		}
	}
	if( $filter) { return $true }

	# compile IDL
	#write-host -nonewline " $idl"
	$gendir      = [System.IO.Path]::Combine($TARGET, "gen-netstd")
	$thrift_csproj = [System.IO.Path]::Combine($ROOTDIR, "lib", "netstd", "Thrift", "Thrift.csproj")
	InitializeFolder  $gendir    "*.cs"
	&$THRIFT_EXE $VERBOSE -r --gen "netstd:$net_version" $idl | out-file ([System.IO.Path]::Combine($TARGET, "thrift.log"))
	if( -not $?) {
		get-content ([System.IO.Path]::Combine($TARGET, "thrift.log")) | out-default
		write-host -foregroundcolor red "Thrift compilation failed: $idl"
		return $false
	}

	# generate solution
	$slnxfile = [System.IO.Path]::Combine($gendir, "$TESTAPP.slnx")
	if( -not (test-path $slnxfile)) {
		$lines = @()
		$lines += "<Solution>"
		$lines += "  <Project Path=`"$TESTAPP.csproj`" />"
		$lines += "  <Project Path=`"$thrift_csproj`" />"
		$lines += "</Solution>"
		$lines | set-content $slnxfile
	}

	# generate program main - always, because of $net_version
	$lines = @()
	$lines += "<Project Sdk=`"Microsoft.NET.Sdk`">"
	$lines += ""
	$lines += "  <PropertyGroup>"
	$lines += "    <OutputType>Exe</OutputType>"
	$lines += "    <TargetFramework>$net_version.0</TargetFramework>"
	$lines += "    <Nullable>enable</Nullable>"
	$lines += "  </PropertyGroup>"
	$lines += ""
	$lines += "  <ItemGroup>"
	$lines += "    <ProjectReference Include=`"$thrift_csproj`" />"
	$lines += "  </ItemGroup>"
	$lines += ""
	$lines += "</Project>"
	$lines += ""
	$lines | set-content ([System.IO.Path]::Combine($gendir, "$TESTAPP.csproj"))

	# generate project file
	$testcs = [System.IO.Path]::Combine($gendir, "$TESTAPP.cs")
	if( -not (test-path $testcs)) {
		$lines = @()
		$lines += "namespace $TESTAPP"
		$lines += "{"
		$lines += "    internal class Program"
		$lines += "    {"
		$lines += "        static void Main(string[] args)"
		$lines += "        {"
		$lines += "            System.Console.WriteLine(`"Hello, World!`");"
		$lines += "        }"
		$lines += "    }"
		$lines += "}"
		$lines | set-content $testcs
	}

	# try to compile the program
	# this should not throw any errors, warnings or hints
	$exe = [System.IO.Path]::Combine($gendir, "bin", "Debug", "$net_version.0", $TESTAPP + $EXE_EXT)
	if( test-path $exe) { remove-item $exe }
	$compilelog = [System.IO.Path]::Combine($TARGET, "compile.log")
	&$DOTNET_EXE build $slnxfile  | out-file $compilelog
	if( -not (test-path $exe)) {

		# expected to fail at Thrift Compiler
		$filter = $false
		$FAIL_DOTNET | foreach {
			if( $idl -eq $_) {
				$filter = $true
				write-host ("C# compilation failed at "+$idl+" - as expected")
			}
		}
		$KNOWN_BUGS | foreach {
			if( $idl -eq $_) {
				$filter = $true
				write-host ("C# compilation failed at "+$idl+" - known issue (TODO)")
			}
		}
		if( $filter) { return $true }

		get-content $compilelog | out-default
		write-host -foregroundcolor red "C# compilation failed: $idl"
		return $false
	}

	# The compiled program is now executed. If it hangs or crashes, we
	# have a serious problem with the generated code.
	$execlog = [System.IO.Path]::Combine($TARGET, "exec.log")
	&"$exe" | out-file $execlog
	if( -not $?) {
		get-content $execlog | out-default
		write-host -foregroundcolor red "Test execution failed: $idl"
		return $false
	}
	return $true
}

#---- main -------------------------------------------------
# CONFIGURATION BEGIN
# configuration settings, adjust as necessary to meet your system setup
$MY_THRIFT_FILES = $ExtraThriftFiles
$VERBOSE = ""  # set any Thrift compiler debug/verbose flag you want

# init
$ROOTDIR = [System.IO.Path]::GetFullPath([System.IO.Path]::Combine($PSScriptRoot, '..', '..', '..', '..'))

# try to find thrift
$THRIFT_EXE = FindThriftExe
&$THRIFT_EXE -version
if( -not $?) {
	write-host -foregroundcolor red ("Missing thrift" + $EXE_EXT)
	exit 1
}

# try to find dotnet
$DOTNET_EXE = FindDotnet
&$DOTNET_EXE --version
if( -not $?) {
	write-host -foregroundcolor red ("Missing dotnet" + $EXE_EXT)
	exit 1
}


# some helpers
if ($TargetFolder -ne "") {
	$TARGET = $TargetFolder
} else {
	$TARGET = [System.IO.Path]::Combine([System.IO.Path]::GetTempPath(), "thrift-testing")
}
$TESTAPP = "TestProject"

# create and/or empty target dirs
InitializeFolder  $TARGET                                                    "*.thrift"
InitializeFolder  ([System.IO.Path]::Combine($TARGET, "gen-netstd"))        "*.cs"

# recurse through thrift WC and "my thrift files" folder
# copies all .thrift files into thrift-testing
CopyFilesFrom "$ROOTDIR"            "Thrift IDL files" | out-null
CopyFilesFrom "$MY_THRIFT_FILES"    "Custom IDL files" | out-null

# codegen and compile all thrift files, one by one to prevent side effects
$count = 0
write-host -foregroundcolor yellow Running codegen tests ..
try {
	pushd "$TARGET"
	$NET_VERSIONS | foreach{
		$net_version = $_
		write-host -foregroundcolor cyan Targeting $net_version
		
		gci *.thrift -file | foreach {
			$count += 1
			$ok = TestIdlFile $_.name
			if( -not $ok) {
				throw "TEST FAILED"               # automated tests
				popd; pause; pushd "$TARGET"      # interactive / debug
			}
		}
	}
	write-host -foregroundcolor green "Success ($count tests executed)"
	exit 0
} catch {
	write-host -foregroundcolor red $_
	exit 1
} finally {
	popd
}

#eof
