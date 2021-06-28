{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions                                              }
{       (Preview Panel, Thumbnail Icon, MD Text Editor)                        }
{                                                                              }
{       Copyright (c) 2021 (Ethea S.r.l.)                                      }
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
{  The Original Code is uAbout.pas:                                            }
{                                                                              }
{  The Initial Developer of the Original Code is Rodrigo Ruz V.                }
{  Portions created by Rodrigo Ruz V. are Copyright 2011-2021 Rodrigo Ruz V.   }
{  All Rights Reserved.                                                        }
{******************************************************************************}

unit MDShellEx.About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, pngimage, Vcl.ImgList, System.ImageList,
  Vcl.Imaging.GIFImg, SVGIconImage;

resourcestring
  Title_MDViewer = 'Markdown file editor';
  Title_SVGPreview = 'Markdown file preview';
  FReeware_Caption = ' - Freeware';

type
  TFrmAbout = class(TForm)
    Panel1:    TPanel;
    btnOK: TButton;
    TitleLabel: TLabel;
    LabelVersion: TLabel;
    MemoCopyRights: TMemo;
    btnIssues: TButton;
    btnCheckUpdates: TButton;
    LinkLabel1: TLinkLabel;
    SVGIconImage1: TSVGIconImage;
    SVGIconImage2: TSVGIconImage;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnIssuesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LinkLabel1Click(Sender: TObject);
    procedure btnCheckUpdatesClick(Sender: TObject);
  private
    FTitle: string;
    procedure SetTitle(const Value: string);
    { Private declarations }
  public
    procedure DisableButtons;
    property Title: string read FTitle write SetTitle;
  end;

procedure ShowAboutForm(const AParentRect: TRect;
  const ATitle: string);
procedure HideAboutForm;

implementation

uses
  ShellApi, uMisc;

{$R *.dfm}

function GetAboutForm: TFrmAbout;
var
  I: integer;
begin
  Result := Nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I].ClassType = TFrmAbout then
    begin
      Result := Screen.Forms[I] as TFrmAbout;
      exit;
    end;
end;

procedure HideAboutForm;
var
  LFrm: TFrmAbout;
begin
  LFrm := GetAboutForm;
  if LFrm <> nil then
    LFrm.Close;
end;

procedure ShowAboutForm(const AParentRect: TRect; const ATitle: string);
var
  LFrm: TFrmAbout;
begin
  LFrm := GetAboutForm;
  if LFrm <> nil then
  begin
    LFrm.BringToFront;
    exit;
  end;

  LFrm := TFrmAbout.Create(nil);
  try
    if (AparentRect.Left <> 0) and (AparentRect.Right <> 0) then
    begin
      LFrm.Left := (AParentRect.Left + AParentRect.Right - LFrm.Width) div 2;
      LFrm.Top := (AParentRect.Top + AParentRect.Bottom - LFrm.Height) div 2;
    end;
    LFrm.Title := ATitle;
    LFrm.ShowModal;
  finally
    LFrm.Free;
  end;
end;

procedure TFrmAbout.btnCheckUpdatesClick(Sender: TObject);
var
  LBinaryPath, LUpdaterPath: string;
begin
  LBinaryPath := GetModuleLocation();
  LUpdaterPath := ExtractFilePath(LBinaryPath)+'Updater.exe';
  ShellExecute(0, 'open', PChar(LUpdaterPath), PChar(Format('"%s"', [LBinaryPath])), '', SW_SHOWNORMAL);
end;


procedure TFrmAbout.btnOKClick(Sender: TObject);
begin
  Close();
end;

procedure TFrmAbout.btnIssuesClick(Sender: TObject);
begin
   ShellExecute(Handle, 'open',
    PChar('https://github.com/EtheaDev/MarkdownShellExtensions/issues'),
    nil, nil, SW_SHOW);
end;

procedure TFrmAbout.DisableButtons;
begin
  btnOK.OnClick := nil;
  btnCheckUpdates.OnClick := nil;
end;

procedure TFrmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmAbout.FormCreate(Sender: TObject);
var
  FileVersionStr: string;
begin
  TitleLabel.Font.Height := Round(TitleLabel.Font.Height * 1.6);

  FileVersionStr:=uMisc.GetFileVersion(GetModuleLocation());
  {$IFDEF WIN32}
  LabelVersion.Caption := Format('Versione %s (32bit)', [FileVersionStr]);
  {$ELSE}
  LabelVersion.Caption := Format('Versione %s (64bit)', [FileVersionStr]);
  {$ENDIF}
end;

procedure TFrmAbout.LinkLabel1Click(Sender: TObject);
begin
   ShellExecute(Handle, 'open',
    PChar('https://github.com/EtheaDev/MarkdownShellExtensions'), nil, nil, SW_SHOW);
end;

procedure TFrmAbout.SetTitle(const Value: string);
begin
  FTitle := Value;
  Caption := FTitle;
  TitleLabel.Caption := Value + FReeware_Caption;
end;

end.
