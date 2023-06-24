{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions                                              }
{       (Preview Panel, Thumbnail Icon, MD Text Editor)                        }
{                                                                              }
{       Copyright (c) 2021-2023 (Ethea S.r.l.)                                 }
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
{  The Original Code is:                                                       }
{  Delphi Preview Handler  https://github.com/RRUZ/delphi-preview-handler      }
{                                                                              }
{  The Initial Developer of the Original Code is Rodrigo Ruz V.                }
{  Portions created by Rodrigo Ruz V. are Copyright 2011-2021 Rodrigo Ruz V.   }
{  All Rights Reserved.                                                        }
{******************************************************************************}
program MDTextEditor;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  System.SysUtils,
  dlgSearchText in 'dlgSearchText.pas' {TextSearchDialog},
  dlgReplaceText in 'dlgReplaceText.pas' {TextReplaceDialog},
  MDTextEditor.ViewerMainForm in 'MDTextEditor.ViewerMainForm.pas' {frmMain},
  MDShellEx.Resources in 'MDShellEx.Resources.pas' {dmResources: TDataModule},
  DPageSetup in 'DPageSetup.pas' {PageSetupDlg},
  MDShellEx.Splash in 'MDShellEx.Splash.pas' {SplashForm},
  dlgConfirmReplace in 'dlgConfirmReplace.pas' {ConfirmReplaceDialog},
  MDShellEx.About in 'MDShellEx.About.pas' {FrmAbout},
  MDShellEx.Misc in 'MDShellEx.Misc.pas',
  SynEditOptionsDialog in 'SynEditOptionsDialog.pas' {fmEditorOptionsDialog},
  MDShellEx.Settings in 'MDShellEx.Settings.pas',
  MDShellEx.SettingsForm in 'MDShellEx.SettingsForm.pas' {SVGSettingsForm},
  MDShellEx.Registry in 'MDShellEx.Registry.pas',
  vmHtmlToPdf in 'vmHtmlToPdf.pas',
  MarkdownCommonMark in '..\Ext\delphi-markdown\source\MarkdownCommonMark.pas',
  MarkdownDaringFireball in '..\Ext\delphi-markdown\source\MarkdownDaringFireball.pas',
  MarkdownHTMLEntities in '..\Ext\delphi-markdown\source\MarkdownHTMLEntities.pas',
  MarkdownProcessor in '..\Ext\delphi-markdown\source\MarkdownProcessor.pas',
  MarkdownUnicodeUtils in '..\Ext\delphi-markdown\source\MarkdownUnicodeUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskBar := True;
  Application.Title := Title_MDViewer+'- © 2021-2022 Ethea S.r.l.';
  with TSplashForm.Create(nil) do
  Try
    Show;
    Update;
    Application.HelpFile := '';
  Application.CreateForm(TdmResources, dmResources);
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TPageSetupDlg, PageSetupDlg);
  Hide;
  Finally
    Free;
  End;
  frmMain.Show;
  Application.Run;
end.
