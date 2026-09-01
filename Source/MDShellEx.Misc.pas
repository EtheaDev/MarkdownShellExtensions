{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions                                              }
{       (Preview Panel, Thumbnail Icon, MD Text Editor)                        }
{                                                                              }
{       Copyright (c) 2021-2026 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/MarkdownShellExtensions                    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{  The Original Code is uMisc.pas:                                             }
{  Delphi Preview Handler  https://github.com/RRUZ/delphi-preview-handler      }
{                                                                              }
{  The Initial Developer of the Original Code is Rodrigo Ruz V.                }
{  Portions created by Rodrigo Ruz V. are Copyright 2011-2021 Rodrigo Ruz V.   }
{  All Rights Reserved.                                                        }
{******************************************************************************}
unit MDShellEx.Misc;

{$WARN SYMBOL_PLATFORM OFF}

interface
  uses
    SysUtils,
    System.Classes
    ;

resourcestring
  MARKDOWN_FILES = 'MarkDown text files';

const
  AMarkDownFileExt: Array of string = [
  '.md',
  '.mkd',
  '.mdwn',
  '.mdown',
  '.mdtxt',
  '.mdtext',
  '.markdown'
  ];

  function GetDllPath: String;
  function GetTempDirectory: string;
  function GetSpecialFolder(const CSIDL: integer): string;
  procedure GetFileNamesWithExtensions(FileNames: TStrings;
    const PathName: string; const Extensions: string;
    FileAttrib : Integer = SysUtils.faArchive or SysUtils.faReadOnly);
  procedure GetFileNames(FileNames: TStrings;
    const PathWithWildCards: string;
    FileAttrib : Integer = SysUtils.faArchive or SysUtils.faReadOnly);
  procedure GetFullFileNames(FileNames: TStrings;
    const PathWithWildCards: string;
    FileAttrib: Integer = SysUtils.faArchive or SysUtils.faReadOnly );
  function  GetModuleLocation: string;
  function FileWithExtExists(var AFileName: TFileName; const AFileExt: array of string): boolean;
  function GetFileMasks(const AFileExt: array of string;
    const ASeparator: Char = ';'): string;
  function IsFileNameWithExt(const AFileName: TFileName;
    const AFileExt: array of string): boolean;
  procedure Initialize_GDI; stdcall;
  procedure Finalize_GDI; stdcall;
  procedure Initialize; stdcall;
  procedure Finalize; stdcall;

resourcestring
  STextNotFound = 'Text not found';

implementation

uses
  WinAPI.GDIPObj,
  WinAPI.GDIPApi,
  ComObj,
  WinApi.Windows,
  WinApi.ShlObj,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.GraphUtil,
  uRegistry,
  uLogExcept;

procedure Initialize_GDI; stdcall;
begin
  //Initialize GDI+
  TLogPreview.Add('GDI+: Initialize');
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs := False;
  StartupInput.GdiplusVersion := 1;
  GdiplusStartup(gdiplusToken, @StartupInput, nil);
end;

procedure Finalize_GDI; stdcall;
begin
  TLogPreview.Add('GDI+: Finalize');
  GdiplusShutdown(gdiplusToken);
end;

function GetDllPath: String;
var
  Path: array [0 .. MAX_PATH - 1] of Char;
begin
  SetString(Result, Path, GetModuleFileName(HInstance, Path, SizeOf(Path)));
end;

function GetTempDirectory: string;
var
  lpBuffer: array [0 .. MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @lpBuffer);
  Result := StrPas(lpBuffer);
end;

function GetWindowsDirectory: string;
var
  lpBuffer: array [0 .. MAX_PATH] of Char;
begin
  WinApi.Windows.GetWindowsDirectory(@lpBuffer, MAX_PATH);
  Result := StrPas(lpBuffer);
end;

function GetSpecialFolder(const CSIDL: integer): string;
var
  lpszPath: PWideChar;
begin
  lpszPath := StrAlloc(MAX_PATH);
  try
    ZeroMemory(lpszPath, MAX_PATH);
    if SHGetSpecialFolderPath(0, lpszPath, CSIDL, False) then
      Result := lpszPath
    else
      Result := '';
  finally
    StrDispose(lpszPath);
  end;
end;

procedure GetFileNamesWithExtensions(FileNames: TStrings;
  const PathName: string; const Extensions: string;
  FileAttrib: Integer = SysUtils.faArchive or SysUtils.faReadOnly);
const
  FileMask = '*.*';
var
  Rec: TSearchRec;
  Path: string;
begin
  Path := IncludeTrailingBackslash(PathName);
  if FindFirst(Path + FileMask, FileAttrib, Rec) = 0 then
  begin
    try
      repeat
        if AnsiPos(ExtractFileExt(Rec.Name), Extensions) > 0 then
          FileNames.Add(Rec.Name);
      until FindNext(Rec) <> 0;
    finally
      SysUtils.FindClose(Rec);
    end;
  end;
end;

procedure GetFileNames(FileNames: TStrings;
  const PathWithWildCards: string;
  FileAttrib : Integer = SysUtils.faArchive or SysUtils.faReadOnly);
var
  SearchRec : TSearchRec;
  R : Integer;
begin
  R := SysUtils.FindFirst( PathWithWildCards, FileAttrib, SearchRec );
  Try
    while R = 0 do
    begin
      FileNames.Append(SearchRec.Name);
      R := SysUtils.FindNext(SearchRec);
    end;
  Finally
    SysUtils.FindClose(SearchRec);
  End;
end;

procedure GetFullFileNames(FileNames: TStrings;
  const PathWithWildCards: string;
  FileAttrib: Integer = SysUtils.faArchive or SysUtils.faReadOnly );
var
  FilePath : string;
  I : Integer;
begin
  // append files found into FileNames
  GetFileNames(FileNames, PathWithWildCards, FileAttrib );
  // calcola il loro path
  FilePath := ExtractFilePath(PathWithWildCards);
  // aggiunge il path ai nomi dei file
  for I := 0 to Filenames.Count - 1 do
    FileNames[I] := ExpandFileName( FilePath+'\'+FileNames[I] );
end;

function  GetModuleLocation: string;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileName(HInstance, PChar(Result), MAX_PATH);
  Result:=PChar(Result);
end;

procedure Initialize; stdcall;
begin
  Initialize_GDI;
end;

procedure Finalize; stdcall;
begin
  Finalize_GDI;
end;

//NB: TryLoadTextFile used to be declared and implemented here, and called by
//nobody. The files are loaded by TEditingFile.LoadFromFile in the editor and by
//TFrmPreview.LoadFromFile in the preview, both through TStrings with their own
//ANSI fallback.

function FileWithExtExists(var AFileName: TFileName; const AFileExt: array of string): boolean;
var
  I: Integer;
  LExt: string;
  LFileName: TFileName;
begin
  LExt := ExtractFileExt(AFileName);
  if LExt = '' then
    LFileName := AFileName+AFileExt[0];
  Result := FileExists(LFileName);
  if not Result then
  begin
    LFileName := ExtractFilePath(AFileName)+ChangeFileExt(ExtractFileName(AFileName),'');
    for I := Low(AFileExt) to High(AFileExt) do
    begin
      LExt := AFileExt[I];
      if FileExists(LFileName+LExt) then
      begin
        ChangeFileExt(AFileName, LExt);
        Result := True;
        break;
      end;
    end;
  end
  else
    AFileName := LFileName;
end;

function GetFileMasks(const AFileExt: array of string;
  const ASeparator: Char = ';'): string;
var
  I: Integer;
  LExt: string;
begin
  for I := Low(AFileExt) to High(AFileExt) do
  begin
    LExt := AFileExt[I];
    if I > 0 then
      Result := Result + ASeparator;
    Result := Result + '*'+LExt;
  end;
end;

function IsFileNameWithExt(const AFileName: TFileName;
  const AFileExt: array of string): boolean;
var
  I: Integer;
  LFileExt, LExt: string;
begin
  Result := False;
  LFileExt := ExtractFileExt(AFileName);
  for I := Low(AFileExt) to High(AFileExt) do
  begin
    LExt := AFileExt[I];
    Result := SameText(LFileExt, LExt);
    if Result then
      break;
  end;
end;

initialization

end.
