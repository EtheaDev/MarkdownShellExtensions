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
{******************************************************************************}
unit MDShellEx.SingleInstance;

interface

uses
  Winapi.Windows, Winapi.Messages;

const
  // Unique class name registered for the editor's main form window.
  // Used by FindWindow to locate an already-running instance.
  cMDTextEditorWindowClass = 'TfrmMain_EtheaMDTextEditor';

  // Application-defined ID for our WM_COPYDATA payload.
  // Distinguishes our messages from any other WM_COPYDATA traffic.
  cMDTextEditorCopyDataOpenFile: NativeUInt = $4D444554; // 'MDET'

// True if another MDTextEditor instance is currently running (FindWindow
// on our custom window-class name).
function IsAnotherInstanceRunning: Boolean;

// If an existing MDTextEditor instance is running, send it the file path
// via WM_COPYDATA and return True (caller should exit). Otherwise return
// False (caller should start a normal instance).
function SendFileToExistingInstance(const AFileName: string): Boolean;

implementation

function IsAnotherInstanceRunning: Boolean;
begin
  Result := FindWindow(cMDTextEditorWindowClass, nil) <> 0;
end;

function SendFileToExistingInstance(const AFileName: string): Boolean;
var
  LHwnd: HWND;
  LCDS: TCopyDataStruct;
  LPid: DWORD;
begin
  Result := False;
  LHwnd := FindWindow(cMDTextEditorWindowClass, nil);
  if LHwnd = 0 then
    Exit;

  // Allow the target process to bring its window to the foreground
  // (Win7+ otherwise blocks SetForegroundWindow from a non-foreground process).
  if GetWindowThreadProcessId(LHwnd, LPid) <> 0 then
    AllowSetForegroundWindow(LPid);

  LCDS.dwData := cMDTextEditorCopyDataOpenFile;
  // cbData is in bytes and must include the trailing #0.
  LCDS.cbData := (Length(AFileName) + 1) * SizeOf(Char);
  LCDS.lpData := PChar(AFileName);

  SendMessage(LHwnd, WM_COPYDATA, 0, LPARAM(@LCDS));
  Result := True;
end;

end.
