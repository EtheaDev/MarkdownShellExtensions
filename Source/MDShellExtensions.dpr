{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions                                              }
{       (Preview Panel, Thumbnail Icon, MD Text Editor)                        }
{                                                                              }
{       Copyright (c) 2021-2025 (Ethea S.r.l.)                                 }
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
library MDShellExtensions;
uses
  ComServ,
  MDShellEx.Main in 'MDShellEx.Main.pas',
  MDShellEx.Misc in 'MDShellEx.Misc.pas',
  MDShellEx.Registry in 'MDShellEx.Registry.pas',
  uLogExcept in 'uLogExcept.pas',
  uStreamPreviewHandler in 'uStreamPreviewHandler.pas',
  uCommonPreviewHandler in 'uCommonPreviewHandler.pas',
  uPreviewHandler in 'uPreviewHandler.pas',
  uPreviewContainer in 'uPreviewContainer.pas' {PreviewContainer},
  MDShellEx.PreviewHandler in 'MDShellEx.PreviewHandler.pas',
  MDShellEx.PreviewHandlerRegister in 'MDShellEx.PreviewHandlerRegister.pas',
  MDShellEx.ThumbnailHandler in 'MDShellEx.ThumbnailHandler.pas',
  MDShellEx.ThumbnailHandlerRegister in 'MDShellEx.ThumbnailHandlerRegister.pas',
  MDShellEx.ContextMenuHandler in 'MDShellEx.ContextMenuHandler.pas',
  MDShellEx.PreviewForm in 'MDShellEx.PreviewForm.pas' {FrmPreview},
  MDShellEx.SettingsForm in 'MDShellEx.SettingsForm.pas' {SVGSettingsForm},
  MDShellEx.Settings in 'MDShellEx.Settings.pas',
  MDShellEx.Resources in 'MDShellEx.Resources.pas' {dmResources: TDataModule},
  dlgSearchText in 'dlgSearchText.pas' {TextSearchDialog},
  MDShellEx.About in 'MDShellEx.About.pas' {FrmAbout},
  BegaHtmlPrintPreviewForm in '..\Ext\HTMLViewer\Source\BegaHtmlPrintPreviewForm.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer,
  DllInstall;

  {$R *.res}

begin
end.
