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

#---- failing tests --------------------------------------------

# expected to fail at Thrift Compiler
$FAIL_THRIFT = @(
	"BrokenConstants.thrift",        # intended to break
	"DuplicateImportsTest.thrift",   # subdir includes don't work here
	"Include.thrift")   # subdir includes don't work here

# expected to fail at Delphi Compiler
$FAIL_DELPHI = @(
    "Thrift5320.thrift"   # this conflicts with Delphi scopes, but it's a bad practice testcase anyway
)

# unexpected but known bugs (TODO: fix them)
$KNOWN_BUGS = @(
	"IgnoreInitialismsTest.thrift", 
	"NameConflictTest.thrift"
    ) 



#---- functions --------------------------------------------

function FindThriftExe() {
	# prefer debug over release over path
	write-host -nonewline Looking for thrift.exe ...
	$exe = "thrift.exe"
	
	# if we have a freshly compiled one it might be a better choice
	@("Release","Debug") | foreach{
		if( test-path "$ROOTDIR\compiler\cpp\$_\thrift.exe") { $exe = "$ROOTDIR\compiler\cpp\$_\thrift.exe" }
		if( test-path "$ROOTDIR\compiler\cpp\compiler\$_\thrift.exe") { $exe = "$ROOTDIR\compiler\cpp\$_\compiler\thrift.exe" }
	}
	
	return $exe
}


function FindDcc32Exe() {
	# prefer debug over release over path
	write-host -nonewline Looking for dcc32.exe ...
	$exe = "dcc32.exe"
	
	# TODO: add arbitraily complex code to locate a suitable dcc32.exe if it is not in the path
	
	return $exe
}


function InitializeFolder([string] $folder, [string] $pattern) {
	#write-host $folder\$pattern
	if(-not (test-path $folder)) {		
		new-item $folder -type directory | out-null
	}
	pushd $folder
	remove-item $pattern #-recurse
	popd
}


function CopyFilesFrom([string] $source, $text) {
	#write-host "$source"
	if( ($source -ne "") -and (test-path $source)) {
		if( $text -ne $null) {
			write-host -foregroundcolor yellow Copying $text ...
		}
		
		pushd $source
		# recurse dirs
		gci . -directory | foreach {
			CopyFilesFrom "$_"
		}
		# files within
		gci *.thrift -file | foreach {
			#write-host $_
			$name = $_.name
			copy-item $_ "$TARGET\$name"
		}
		popd
	}
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
	InitializeFolder  "$TARGET\gen-delphi"    "*.pas"
	&$THRIFT_EXE $VERBOSE -r --gen delphi:register_types,constprefix,events,xmldoc $idl | out-file "$TARGET\thrift.log"
	if( -not $?) {
		get-content "$TARGET\thrift.log" | out-default
		write-host -foregroundcolor red "Thrift compilation failed: $idl"
		return $false
	}
	
	# generate project dile
	$units = gci "$TARGET\gen-delphi\*.pas"
	$lines = @()
	$lines += "program $TESTAPP;"
	$lines += "{`$APPTYPE CONSOLE}"
	$lines += ""
	$lines += "uses"
	$units | foreach { $name = $_.name.Replace(".pas",""); $lines += " $name," }
	$lines += " Windows, Classes, SysUtils;"
	$lines += ""
	$lines += "begin"
	$lines += "  Writeln('Successfully compiled!');"
	$lines += "  Writeln('List of units:');"
	$units | foreach { $name = $_.name.Replace(".pas",""); $lines += "  Writeln('- $name');" }
	$lines += "  Writeln;"
	$lines += ""
	$lines += "end."
	$lines += ""	
	$lines | set-content "$TARGET\gen-delphi\$TESTAPP.dpr"

	# try to compile the DPR
	# this should not throw any errors, warnings or hints
	$exe = "$TARGET\gen-delphi\$TESTAPP.exe"
	if( test-path $exe) { remove-item $exe }
	&$DCC32_EXE  -B "$TARGET\gen-delphi\$TESTAPP" -U"$UNITSEARCH" -I"$UNITSEARCH" -N"$OUTDCU" -E"$TARGET\gen-delphi" | out-file "$TARGET\compile.log"
	if( -not (test-path $exe)) { 
	
		# expected to fail at Thrift Compiler
		$filter = $false
		$FAIL_DELPHI | foreach {
			if( $idl -eq $_) {
				$filter = $true
				write-host ("Delphi compilation failed at "+$idl+" - as expected")
			}		
		}
		$KNOWN_BUGS | foreach {
			if( $idl -eq $_) {
				$filter = $true
				write-host ("Delphi compilation failed at "+$idl+" - known issue (TODO)")
			}		
		}
		if( $filter) { return $true }

		get-content "$TARGET\compile.log" | out-default
		write-host -foregroundcolor red "Delphi compilation failed: $idl"
		return $false 
	}

	# The compiled program is now executed. If it hangs or crashes, we
	# have a serious problem with the generated code. Expected output
	# is "Successfully compiled:" followed by a list of generated units.
	&"$exe" | out-file "$TARGET\exec.log"
	if( -not $?) {
		get-content "$TARGET\exec.log" | out-default
		write-host -foregroundcolor red "Test execution failed: $idl"
		return $false
	}
	return $true
}

#---- main -------------------------------------------------
# CONFIGURATION BEGIN
# configuration settings, adjust as necessary to meet your system setup
$MY_THRIFT_FILES = ""
$VERBOSE = ""  # set any Thrift compiler debug/verbose flag you want

# init
$ROOTDIR = $PSScriptRoot + "\..\..\..\.."

# try to find thrift.exe
$THRIFT_EXE = FindThriftExe
&$THRIFT_EXE -version
if( -not $?) { 
	write-host -foregroundcolor red Missing thrift.exe
	exit 1
}

# try to find dcc32.exe
$DCC32_EXE = FindDcc32Exe
&$DCC32_EXE --version
if( -not $?) { 
	write-host -foregroundcolor red Missing dcc32.exe
	exit 1
}


# some helpers
$TARGET = "$ROOTDIR\..\thrift-testing"
$TESTAPP = "TestProject"
$UNITSEARCH = "$ROOTDIR\lib\pas\src;$ROOTDIR\lib\delphi\src"
$OUTDCU = "$TARGET\dcu"
   
# create and/or empty target dirs
InitializeFolder  "$TARGET"            "*.thrift"
InitializeFolder  "$TARGET\gen-delphi" "*.pas"
InitializeFolder  "$OUTDCU"            "*.dcu"

# recurse through thrift WC and "my thrift files" folder
# copies all .thrift files into thrift-testing
CopyFilesFrom "$ROOTDIR"            "Thrift IDL files"
CopyFilesFrom "$MY_THRIFT_FILES"    "Custom IDL files"

# codegen and compile all thrift files, one by one to prevent side effects
$count = 0
write-host -foregroundcolor yellow Running codegen tests ..
try {
	pushd "$TARGET"
	gci *.thrift -file | foreach {
		$count += 1
		$ok = TestIdlFile $_.name
		if( -not $ok) { 
			throw "TEST FAILED"               # automated tests
			popd; pause; pushd "$TARGET"      # interactive / debug
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
