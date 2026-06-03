{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions                                              }
{       (Preview Panel, Thumbnail Icon, MD Text Editor)                        }
{                                                                              }
{       Copyright (c) 2021-2026 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/MDShellExtensions                          }
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
program MDTextEditor;

uses
  System.SysUtils,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  dlgSearchText in 'dlgSearchText.pas' {TextSearchDialog},
  dlgReplaceText in 'dlgReplaceText.pas' {TextReplaceDialog},
  PageControlHook in 'PageControlHook.pas',
  dlgInputUrl in 'dlgInputUrl.pas' {InputUrlDialog},
  MDTextEditor.EditorMainForm in 'MDTextEditor.EditorMainForm.pas' {frmMain},
  MDShellEx.Resources in 'MDShellEx.Resources.pas' {dmResources: TDataModule},
  DPageSetup in 'DPageSetup.pas' {PageSetupDlg},
  MDShellEx.Splash in 'MDShellEx.Splash.pas' {SplashForm},
  dlgConfirmReplace in 'dlgConfirmReplace.pas' {ConfirmReplaceDialog},
  MDShellEx.About in 'MDShellEx.About.pas' {FrmAbout},
  MDShellEx.Misc in 'MDShellEx.Misc.pas',
  SynEditOptionsDialog in 'SynEditOptionsDialog.pas' {fmEditorOptionsDialog},
  MDShellEx.Settings in 'MDShellEx.Settings.pas',
  MDShellEx.SettingsForm in 'MDShellEx.SettingsForm.pas' {MDSettingsForm},
  MDShellEx.Registry in 'MDShellEx.Registry.pas',
  MDShellEx.SingleInstance in 'MDShellEx.SingleInstance.pas',
  vmHtmlToPdf in 'vmHtmlToPdf.pas',
  GitHubAPI in 'GitHubAPI.pas' {fmEditorOptionsDialog},
  Vcl.StyledTaskDialogFormUnit in '..\Ext\StyledComponents\source\Vcl.StyledTaskDialogFormUnit.pas' {StyledTaskDialogForm},
  Skia.Vcl.StyledTaskDialogAnimatedUnit in '..\Ext\StyledComponents\source\Skia.Vcl.StyledTaskDialogAnimatedUnit.pas' {StyledTaskDialogAnimated};

{$R *.res}

var
  LFileParam: string;
begin
  // Single instance: if a file was passed on the command line and another
  // MDTextEditor instance is already running, forward the file to it and
  // exit. Launching without arguments always starts a new instance; whether
  // that new instance restores the previous session is controlled by the
  // user preference "Remember current session at next run".
  if (ParamCount >= 1) and IsAnotherInstanceRunning then
  begin
    LFileParam := ParamStr(1);
    if FileExists(LFileParam) and
       SendFileToExistingInstance(ExpandFileName(LFileParam)) then
      Exit;
  end;

  Application.Initialize;
  Application.MainFormOnTaskBar := True;
  Application.ActionUpdateDelay := 50;
  Application.Title := Title_MDViewer+'- © 2021-2026 Ethea S.r.l.';
  //Uses System Style for border / shadow of Forms
  TStyleManager.FormBorderStyle := TStyleManager.TFormBorderStyle.fbsSystemStyle;
  with TSplashForm.Create(nil) do
  Try
    Show;
    Update;
    Application.HelpFile := '';
    Application.CreateForm(TdmResources, dmResources);
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TPageSetupDlg, PageSetupDlg);
  Application.OnException := frmMain.ManageExceptions;
    Hide;
  Finally
    Free;
  End;
  frmMain.Show;
  Application.Run;
end.
