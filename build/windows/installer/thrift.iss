;
; Licensed to the Apache Software Foundation (ASF) under one
; or more contributor license agreements. See the NOTICE file
; distributed with this work for additional information
; regarding copyright ownership. The ASF licenses this file
; to you under the Apache License, Version 2.0 (the
; "License"); you may not use this file except in compliance
; with the License. You may obtain a copy of the License at
;
;   http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing,
; software distributed under the License is distributed on an
; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
; KIND, either express or implied. See the License for the
; specific language governing permissions and limitations
; under the License.
;

; Inno Setup script for the Windows Thrift compiler.
;
; Requires Inno Setup 6.3 or later, which is what the GitHub Windows runner
; images ship. Build it with build/windows/build-installer.ps1, or by hand:
;
;   iscc /DAppVersion=0.26.0 ^
;        /DSourceExe=C:\install\bin\thrift.exe ^
;        /DSourceRoot=C:\thrift ^
;        /DOutputDir=C:\out ^
;        build\windows\installer\thrift.iss
;
; The version is never stored in this file. It is passed in, taken from the
; release tag, so that build/veralign.sh has nothing extra to keep in step.

#ifndef AppVersion
  #error Pass the version on the command line, for example /DAppVersion=0.26.0
#endif
#ifndef SourceExe
  #error Pass the compiler on the command line, for example /DSourceExe=C:\install\bin\thrift.exe
#endif
#ifndef SourceRoot
  ; The root of a Thrift source tree, for LICENSE and NOTICE.
  #define SourceRoot "..\..\.."
#endif
#ifndef OutputDir
  #define OutputDir "."
#endif

#define AppName "Apache Thrift Compiler"
#define AppPublisher "Apache Software Foundation"
#define AppUrl "https://thrift.apache.org/"

[Setup]
; Never change AppId. Windows identifies an installed Apache Thrift compiler
; by it, and changing it would turn an upgrade into a second installation.
AppId={{064E0C5D-29EE-47DD-BB5E-172FE25FE936}
AppName={#AppName}
AppVersion={#AppVersion}
AppVerName={#AppName} {#AppVersion}
AppPublisher={#AppPublisher}
AppPublisherURL={#AppUrl}
AppSupportURL=https://thrift.apache.org/docs/
AppUpdatesURL=https://thrift.apache.org/download
VersionInfoVersion={#AppVersion}
VersionInfoCompany={#AppPublisher}
VersionInfoDescription={#AppName} {#AppVersion} Setup
VersionInfoCopyright=Copyright (c) The Apache Software Foundation

DefaultDirName={autopf}\Apache Thrift
DefaultGroupName=Apache Thrift
DisableProgramGroupPage=yes
DisableDirPage=auto
LicenseFile={#SourceRoot}\LICENSE
UninstallDisplayName={#AppName} {#AppVersion}
UninstallDisplayIcon={app}\thrift.exe

; The compiler is a single self-contained executable, so a per-user install
; needs no elevation. An administrator can still install for all users, from
; the wizard or with /ALLUSERS, which is what a machine scope package manager
; install uses.
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog commandline

; thrift.exe is 64 bit.
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible

OutputDir={#OutputDir}
OutputBaseFilename=thrift-{#AppVersion}-setup
Compression=lzma2/max
SolidCompression=yes
WizardStyle=modern
ChangesEnvironment=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "modifypath"; Description: "Add the Thrift compiler to the PATH environment variable"

[Files]
Source: "{#SourceExe}"; DestDir: "{app}"; DestName: "thrift.exe"; Flags: ignoreversion
Source: "{#SourceRoot}\LICENSE"; DestDir: "{app}"; DestName: "LICENSE.txt"; Flags: ignoreversion
Source: "{#SourceRoot}\NOTICE"; DestDir: "{app}"; DestName: "NOTICE.txt"; Flags: ignoreversion

[Icons]
Name: "{group}\Apache Thrift on the web"; Filename: "{#AppUrl}"
Name: "{group}\{cm:UninstallProgram,{#AppName}}"; Filename: "{uninstallexe}"

[Registry]
; Appending with {olddata} lets Inno Setup read and write the value itself.
; Reading PATH in [Code] and writing it back would expand %SystemRoot% and
; friends into the stored value.
Root: HKA; Subkey: "{code:GetEnvironmentKey}"; ValueType: expandsz; ValueName: "Path"; \
    ValueData: "{olddata};{app}"; Flags: preservestringtype; \
    Tasks: modifypath; Check: NeedsAddPath(ExpandConstant('{app}'))

[Code]

function InitializeSetup(): Boolean;
var
  Runtime: String;
begin
  // The compiler links the shared runtime, so it needs the Visual C++
  // redistributable. The ASF cannot ship that, so say so rather than let the
  // first run fail with a missing DLL dialog.
  //
  // Never blocks: a wrong guess here must not stop an install, and a silent
  // install - which is what a package manager does - shows no message at all.
  Result := True;

  Runtime := ExpandConstant('{sys}\vcruntime140.dll');
  if FileExists(Runtime) then
    exit;

  if WizardSilent then
    exit;

  MsgBox('The Microsoft Visual C++ Redistributable does not seem to be installed.' + #13#10 + #13#10 +
         'The Thrift compiler needs it to start. Setup will continue; if thrift.exe ' +
         'does not run afterwards, install the x64 redistributable from ' +
         'https://aka.ms/vs/17/release/vc_redist.x64.exe and try again.',
         mbInformation, MB_OK);
end;

function GetEnvironmentKey(Param: String): String;
begin
  // HKA resolves to HKLM for an all users install and to HKCU otherwise, and
  // the two keep PATH in different places.
  if IsAdminInstallMode then
    Result := 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment'
  else
    Result := 'Environment';
end;

function GetEnvironmentRootKey: Integer;
begin
  if IsAdminInstallMode then
    Result := HKEY_LOCAL_MACHINE
  else
    Result := HKEY_CURRENT_USER;
end;

function ReadPath(var Path: String): Boolean;
begin
  Result := RegQueryStringValue(GetEnvironmentRootKey, GetEnvironmentKey(''), 'Path', Path);
  if not Result then
    Path := '';
end;

// Both callers compare whole entries only, so that a directory is neither
// added twice nor removed because some other entry contains its name.
function PathIndexOf(const Path, Dir: String): Integer;
begin
  Result := Pos(';' + Uppercase(Dir) + ';', ';' + Uppercase(Path) + ';');
end;

function NeedsAddPath(const Dir: String): Boolean;
var
  Path: String;
begin
  ReadPath(Path);
  Result := PathIndexOf(Path, Dir) = 0;
end;

procedure RemoveFromPath(const Dir: String);
var
  Path: String;
  Index: Integer;
begin
  if not ReadPath(Path) then
    exit;

  Index := PathIndexOf(Path, Dir);
  if Index = 0 then
    exit;

  // PathIndexOf searched ';' + Path + ';' and returned the position of the
  // separator in front of the entry. That string is Path with one character
  // prepended, so the entry itself starts at Index within Path.
  //
  // The entry has to go together with exactly one of the separators next to
  // it. Take the one in front where there is one, otherwise the one behind,
  // which leaves neither a leading nor a trailing separator behind.
  if Index > 1 then
    Delete(Path, Index - 1, Length(Dir) + 1)
  else
    Delete(Path, Index, Length(Dir) + 1);

  if Path = '' then
    RegDeleteValue(GetEnvironmentRootKey, GetEnvironmentKey(''), 'Path')
  else
    RegWriteExpandStringValue(GetEnvironmentRootKey, GetEnvironmentKey(''), 'Path', Path);
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  // Inno Setup can append to PATH on its own but cannot undo it, so the
  // uninstaller has to take the entry out again.
  if CurUninstallStep = usPostUninstall then
    RemoveFromPath(ExpandConstant('{app}'));
end;
