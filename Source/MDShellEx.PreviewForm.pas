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
{                                                                              }
{  The Initial Developer of the Original Code is Rodrigo Ruz V.                }
{  Portions created by Rodrigo Ruz V. are Copyright 2011-2021 Rodrigo Ruz V.   }
{  All Rights Reserved.                                                        }
{******************************************************************************}

unit MDShellEx.PreviewForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SynEdit,
  System.Generics.Collections,
  SynEditHighlighter,
  ComCtrls, ToolWin, ImgList,
  Vcl.Menus, SynEditExport,
  SynExportHTML, SynExportRTF, SynEditMiscClasses,
  MDShellEx.Settings, System.ImageList, SynEditCodeFolding,
  SVGIconImageList, SVGIconImageListBase, SVGIconImage, Vcl.VirtualImageList,
  Vcl.OleCtrls, SHDocVw,
  MDShellEx.Resources, HTMLUn2, HtmlView
  ;

type
  TFrmPreview = class(TForm)
    SynEdit: TSynEdit;
    PanelTop: TPanel;
    PanelMD: TPanel;
    StatusBar: TStatusBar;
    SVGIconImageList: TVirtualImageList;
    ToolButtonZoomIn: TToolButton;
    ToolButtonZoomOut: TToolButton;
    ToolBar: TToolBar;
    ToolButtonSettings: TToolButton;
    ToolButtonAbout: TToolButton;
    ToolButtonShowText: TToolButton;
    Splitter: TSplitter;
    ToolBarAllegati: TToolBar;
    HtmlViewer: THtmlViewer;
    procedure FormCreate(Sender: TObject);
    procedure ToolButtonZoomInClick(Sender: TObject);
    procedure ToolButtonZoomOutClick(Sender: TObject);
    procedure ToolButtonSettingsClick(Sender: TObject);
    procedure ToolButtonAboutClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ToolButtonSelectModeClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ToolButtonShowTextClick(Sender: TObject);
    procedure ToolButtonMouseEnter(Sender: TObject);
    procedure ToolButtonMouseLeave(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
  private
    FMDFontSize: Integer;
    FHTMLFontSize: Integer;
    FSimpleText: string;
    FFileName: string;
    FPreviewSettings: TPreviewSettings;
    FMarkDownFile: TMarkDownFile;
    FAllegatiButtons: TObjectList<TToolButton>;

    class var FExtensions: TDictionary<TSynCustomHighlighterClass, TStrings>;
    class var FAParent: TWinControl;

    function DialogPosRect: TRect;
    procedure AppException(Sender: TObject; E: Exception);
    procedure UpdateGUI;
    procedure UpdateFromSettings(const Preview: Boolean);
    procedure SaveSettings;
    procedure SetMDFontSize(const Value: Integer);
    procedure SetHTMLFontSize(const Value: Integer);
    procedure UpdateHighlighter;

    //Visualizzatore Fattura
    procedure ShowMarkDownAsHTML(const ASettings: TSettings;
      const AReloadImages: Boolean);
    procedure AdjustZoom(AValue: Integer);
  protected
  public
    procedure ScaleControls(const ANewPPI: Integer);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class property Extensions: TDictionary<TSynCustomHighlighterClass, TStrings> read FExtensions write FExtensions;
    class property AParent: TWinControl read FAParent write FAParent;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(const AStream: TStream);
    property MDFontSize: Integer read FMDFontSize write SetMDFontSize;
    property HTMLFontSize: Integer read FHTMLFontSize write SetHTMLFontSize;
  end;


implementation

uses
  SynEditTypes
  , Vcl.Clipbrd
{$IFNDEF DISABLE_STYLES}
  , Vcl.Themes
{$ENDIF}
  , WinApi.ActiveX
  , uLogExcept
  , System.Types
  , System.NetEncoding
  , Registry
  , uMisc
  , IOUtils
  , ShellAPI
  , ComObj
  , IniFiles
  , GraphUtil
  , MDShellEx.About
  , MDShellEx.SettingsForm
  ;

{$R *.dfm}
  { TFrmEditor }

procedure TFrmPreview.AppException(Sender: TObject; E: Exception);
begin
  // log unhandled exceptions (TSynEdit, etc)
  TLogPreview.Add('AppException');
  TLogPreview.Add(E);
end;

constructor TFrmPreview.Create(AOwner: TComponent);
begin
  inherited;
  FPreviewSettings := TPreviewSettings.CreateSettings(SynEdit.Highlighter);
  dmResources := TdmResources.Create(nil);
  dmResources.Settings := FPreviewSettings;
  FAllegatiButtons := TObjectList<TToolButton>.Create(True);
end;

destructor TFrmPreview.Destroy;
begin
  FreeAndNil(FAllegatiButtons);
  FreeAndNil(FPreviewSettings);
  FreeAndNil(dmResources);
  inherited;
end;

function TFrmPreview.DialogPosRect: TRect;
begin
  if Self.Parent <> nil then
    GetWindowRect(Self.Parent.ParentWindow, Result)
  else
    Result := TRect.Create(0,0,0,0);
end;

procedure TFrmPreview.UpdateGUI;
begin
  if PanelMD.Visible then
  begin
    Splitter.Top := PanelMD.Top + PanelMD.Height;
    Splitter.Visible := True;
    ToolButtonShowText.Caption := 'Hide Markdown';
    ToolButtonShowText.Hint := 'Hide Markdown file content';
    ToolButtonShowText.ImageName := 'hide-text';
  end
  else
  begin
    Splitter.Visible := False;
    ToolButtonShowText.Caption := 'Show Markdown';
    ToolButtonShowText.Hint := 'Show Markdown file content';
    ToolButtonShowText.ImageName := 'show-text';
  end;
  ToolButtonShowText.Visible := True;
  ToolButtonAbout.Visible := True;
  ToolButtonSettings.Visible := True;
end;

procedure TFrmPreview.UpdateHighlighter;
var
  LBackgroundColor: TColor;
begin
{$IFNDEF DISABLE_STYLES}
  LBackgroundColor := StyleServices.GetSystemColor(clWindow);
{$ELSE}
  LBackgroundColor := clWindow;
{$ENDIF}
  SynEdit.Highlighter := dmResources.GetSynHighlighter(
    FPreviewSettings.UseDarkStyle, LBackgroundColor);
  //Assegna i colori "custom" all'Highlighter
  FPreviewSettings.ReadSettings(SynEdit.Highlighter, nil);
  SynEdit.Gutter.Font.Name := SynEdit.Font.Name;
{$IFNDEF DISABLE_STYLES}
  SynEdit.Gutter.Font.Color := StyleServices.GetSystemColor(clWindowText);
  SynEdit.Gutter.Color := StyleServices.GetSystemColor(clBtnFace);
{$ELSE}
  SynEdit.Gutter.Font.Color := clWindowText;
  SynEdit.Gutter.Color := clBtnFace;
{$ENDIF}
end;

procedure TFrmPreview.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  TLogPreview.Add('TFrmEditor.FormAfterMonitorDpiChanged: '+
  '- Old: '+OldDPI.ToString+' - New: '+NewDPI.ToString);
end;

procedure TFrmPreview.FormCreate(Sender: TObject);
begin
  TLogPreview.Add('TFrmEditor.FormCreate');
  Application.OnException := AppException;
  FSimpleText := StatusBar.SimpleText;
  UpdateFromSettings(False);
  HTMLViewer.OnHotSpotClick := dmResources.HtmlViewerHotSpotClick;
  HTMLViewer.OnImageRequest:= dmResources.HtmlViewerImageRequest;
end;

procedure TFrmPreview.FormDestroy(Sender: TObject);
begin
  HideAboutForm;
  SaveSettings;
  TLogPreview.Add('TFrmEditor.FormDestroy');
  inherited;
end;

procedure TFrmPreview.FormResize(Sender: TObject);
begin
  PanelMD.Height := Round(Self.Height * (FPreviewSettings.SplitterPos / 100));
  Splitter.Top := PanelMD.Height;
  if Self.Width < (500 * Self.ScaleFactor) then
    ToolBar.ShowCaptions := False
  else
    Toolbar.ShowCaptions := True;
  UpdateGUI;
  if FPreviewSettings.RescalingImage then
    ShowMarkDownAsHTML(FPreviewSettings, True);
end;

procedure TFrmPreview.LoadFromFile(const AFileName: string);
var
  LOutStream: TStringStream;
begin
  TLogPreview.Add('TFrmEditor.LoadFromFile Init');
  FFileName := AFileName;
  LOutStream := TStringStream.Create('', TEncoding.UTF8);
  try
    SynEdit.Lines.LoadFromFile(AFileName, TEncoding.UTF8);
    HtmlViewer.ServerRoot := ExtractFilePath(FFileName);
    ShowMarkDownAsHTML(FPreviewSettings, True);
  finally
    LOutStream.Free;
  end;
  TLogPreview.Add('TFrmEditor.LoadFromFile Done');
end;

procedure TFrmPreview.LoadFromStream(const AStream: TStream);
var
  LStringStream: TStringStream;
begin
  TLogPreview.Add('TFrmEditor.LoadFromStream Init');
  AStream.Position := 0;
  LStringStream := TStringStream.Create('',TEncoding.UTF8);
  try
    SynEdit.Lines.LoadFromStream(AStream, TEncoding.UTF8);
    ShowMarkDownAsHTML(FPreviewSettings, True);
  finally
    LStringStream.Free;
  end;
  TLogPreview.Add('TFrmEditor.LoadFromStream Done');
end;

procedure TFrmPreview.SaveSettings;
begin
  if Assigned(FPreviewSettings) then
  begin
    FPreviewSettings.UpdateSettings(SynEdit.Font.Name,
      HtmlViewer.DefFontName,
      MDFontSize, HTMLFontSize,
      (PanelMD.Visible and HtmlViewer.Visible));
    FPreviewSettings.WriteSettings(SynEdit.Highlighter, nil);
  end;
end;

procedure TFrmPreview.ScaleControls(const ANewPPI: Integer);
var
  LCurrentPPI: Integer;
  LNewSize: Integer;
begin
  LCurrentPPI := FCurrentPPI;
  if ANewPPI <> LCurrentPPI then
  begin
    LNewSize := MulDiv(SVGIconImageList.Width, ANewPPI, LCurrentPPI);
    SVGIconImageList.SetSize(LNewSize, LNewSize);
  end;
end;

procedure TFrmPreview.SetMDFontSize(const Value: Integer);
var
  LScaleFactor: Single;
begin
  if (Value >= MinfontSize) and (Value <= MaxfontSize) then
  begin
    TLogPreview.Add('TFrmEditor.SetMDFontSize'+
      ' CurrentPPI: '+Self.CurrentPPI.ToString+
      ' ScaleFactor: '+ScaleFactor.ToString+
      ' Value: '+Value.ToString);
    if FMDFontSize <> 0 then
      LScaleFactor := SynEdit.Font.Size / FMDFontSize
    else
      LScaleFactor := 1;
    FMDFontSize := Value;
    SynEdit.Font.Size := Round(FMDFontSize * LScaleFactor);
    SynEdit.Gutter.Font.Size := SynEdit.Font.Size;
  end;
end;

procedure TFrmPreview.ShowMarkDownAsHTML(const ASettings: TSettings;
  const AReloadImages: Boolean);
var
  LStream: TStringStream;
  LOldPos: Integer;
begin
  try
    if AReloadImages then
      HtmlViewer.clear;
    FMarkDownFile := TMarkDownFile.Create(SynEdit.Lines.Text,
      ASettings.ProcessorDialect, True);

    //Carica il contenuto HTML trasformato dentro l'HTML-Viewer
    LOldPos := HtmlViewer.VScrollBarPosition;
    HtmlViewer.DefFontSize := ASettings.HTMLFontSize;
    HtmlViewer.DefFontName := ASettings.HTMLFontName;
    LStream := TStringStream.Create(FMarkDownFile.HTML);
    try
      HtmlViewer.LoadFromStream(LStream);
      HtmlViewer.VScrollBarPosition := LOldPos;
      HtmlViewer.Visible := True;
    finally
      LStream.Free;
    end;
  except
    on E: Exception do
      ; //non solleva eccezioni
  end;
end;

procedure TFrmPreview.SetHTMLFontSize(const Value: Integer);
var
  LScaleFactor: Single;
begin
  if (Value >= MinfontSize) and (Value <= MaxfontSize) then
  begin
    TLogPreview.Add('TFrmEditor.SetMDFontSize'+
      ' CurrentPPI: '+Self.CurrentPPI.ToString+
      ' ScaleFactor: '+ScaleFactor.ToString+
      ' Value: '+Value.ToString);
    if FHTMLFontSize <> 0 then
      LScaleFactor := HtmlViewer.DefFontSize / FHTMLFontSize
    else
      LScaleFactor := 1;
    FHTMLFontSize := Value;
    HtmlViewer.DefFontSize := Round(FHTMLFontSize * LScaleFactor);
  end;
end;

procedure TFrmPreview.SplitterMoved(Sender: TObject);
begin
  FPreviewSettings.SplitterPos := splitter.Top * 100 div
    (Self.Height - Toolbar.Height);
  SaveSettings;
end;

procedure TFrmPreview.ToolButtonShowTextClick(Sender: TObject);
begin
  PanelMD.Visible := not PanelMD.Visible;
  if not PanelMD.Visible and HTMLViewer.CanFocus then
    HTMLViewer.SetFocus;
  UpdateGUI;
  SaveSettings;
end;

procedure TFrmPreview.ToolButtonAboutClick(Sender: TObject);
begin
  ShowAboutForm(DialogPosRect, Title_SVGPreview);
end;

procedure TFrmPreview.ToolButtonMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := (Sender as TToolButton).Hint;
end;

procedure TFrmPreview.ToolButtonMouseLeave(Sender: TObject);
begin
  StatusBar.SimpleText := FSimpleText;
end;

procedure TFrmPreview.UpdateFromSettings(const Preview: Boolean);
begin
  FPreviewSettings.ReadSettings(SynEdit.Highlighter, nil);
  if FPreviewSettings.MDFontSize >= MinfontSize then
    MDFontSize := FPreviewSettings.MDFontSize
  else
    MDFontSize := MinfontSize;
  SynEdit.Font.Name := FPreviewSettings.MDFontName;

  if FPreviewSettings.HTMLFontSize >= MinfontSize then
    HTMLFontSize := FPreviewSettings.HTMLFontSize
  else
    HTMLFontSize := 12;
  HtmlViewer.DefFontName := FPreviewSettings.HTMLFontName;

  PanelMD.Visible := FPreviewSettings.ShowMarkDown;
{$IFNDEF DISABLE_STYLES}
  TStyleManager.TrySetStyle(FPreviewSettings.StyleName, False);
{$ENDIF}
  //BackgroundTrackBar.Position := FPreviewSettings.LightBackground;
  UpdateHighlighter;
  UpdateGUI;
  if Preview then
    ShowMarkDownAsHTML(FPreviewSettings, True);
end;

procedure TFrmPreview.ToolButtonSettingsClick(Sender: TObject);
begin
  if ShowSettings(DialogPosRect, Title_SVGPreview, SynEdit, FPreviewSettings, True) then
  begin
    FPreviewSettings.WriteSettings(SynEdit.Highlighter, nil);
    UpdateFromSettings(True);
  end;
end;

procedure TFrmPreview.ToolButtonSelectModeClick(Sender: TObject);
begin
  TToolButton(Sender).CheckMenuDropdown;
end;

procedure TFrmPreview.ToolButtonZoomOutClick(Sender: TObject);
begin
  AdjustZoom(-1);
end;

procedure TFrmPreview.ToolButtonZoomInClick(Sender: TObject);
begin
  AdjustZoom(1);
end;

procedure TFrmPreview.AdjustZoom(AValue: Integer);
begin
  if Synedit.Focused then
    MDFontSize := MDFontSize + AValue
  else
  begin
    HTMLFontSize := HTMLFontSize + AValue;
    ShowMarkDownAsHTML(FPreviewSettings, False);
  end;
  SaveSettings;
end;

end.
