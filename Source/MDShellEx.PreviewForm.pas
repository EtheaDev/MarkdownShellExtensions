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
  MDShellEx.Resources, HTMLUn2, HtmlView,
  MDCodeHighlightEmitter,
  UPreviewContainer,
  Vcl.ButtonStylesAttributes, Vcl.StyledButton,
  Vcl.StyledToolbar, Vcl.StyledButtonGroup;

type
  TFrmPreview = class(TPreviewContainer)
    SynEdit: TSynEdit;
    PanelTop: TPanel;
    PanelMD: TPanel;
    StatusBar: TStatusBar;
    SVGIconImageList: TVirtualImageList;
    ToolButtonZoomIn: TStyledToolButton;
    ToolButtonZoomOut: TStyledToolButton;
    StyledToolBar: TStyledToolbar;
    ToolButtonSettings: TStyledToolButton;
    ToolButtonAbout: TStyledToolButton;
    ToolButtonShowText: TStyledToolButton;
    Splitter: TSplitter;
    ToolBarAllegati: TStyledToolbar;
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
    FCodeHighlightEmitter: TCodeHighlightEmitterBase;
    //True while the HTML viewer is being loaded: see the guard in
    //ShowMarkDownAsHTML
    FRendering: Boolean;
    FAllegatiButtons: TObjectList<TStyledToolButton>;

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
  , MDShellEx.Misc
  , IOUtils
  , ShellAPI
  , ComObj
  , IniFiles
  , GraphUtil
  , MDShellEx.About
  , MDShellEx.SettingsForm
  , MarkdownProcessor
  , MarkdownUtils
  ;

{$R *.dfm}

  { TFrmPreview }

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
  //NB: the data module is no longer created here. It is a shared instance
  //owned by MDShellEx.Resources, created on first use: creating and destroying
  //it per preview form meant that, with two handlers alive in the same
  //Explorer process, one could free the module the other was still using.
  dmResources.Settings := FPreviewSettings;
  FAllegatiButtons := TObjectList<TStyledToolButton>.Create(True);
end;

destructor TFrmPreview.Destroy;
begin
  FreeAndNil(FAllegatiButtons);
  //The shared data module survives this form: it must not be left pointing at
  //the settings object we are about to free.
  if dmResourcesCreated and (dmResources.Settings = FPreviewSettings) then
    dmResources.Settings := nil;
  FreeAndNil(FPreviewSettings);
  inherited;
end;

function TFrmPreview.DialogPosRect: TRect;
begin
  Result := ClientToScreen(ActualRect);
end;

procedure TFrmPreview.UpdateGUI;
begin
  if PanelMD.Visible then
  begin
    Splitter.Top := PanelMD.Top + PanelMD.Height;
    Splitter.Visible := True;
    ToolButtonShowText.Caption := 'Hide Source';
    ToolButtonShowText.Hint := 'Hide Markdown file content';
    ToolButtonShowText.ImageName := 'hide-text';
  end
  else
  begin
    Splitter.Visible := False;
    ToolButtonShowText.Caption := 'Show Source';
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
var
  FileVersionStr: string;
begin
  inherited;
  TLogPreview.Add('TFrmEditor.FormCreate');
  //Optional syntax-highlighting emitter for fenced code blocks (nil when the
  //MD_SYNTAX_HIGHLIGHTING define is off).
  FCodeHighlightEmitter := CreateCodeHighlightEmitter;
  FileVersionStr := GetCurrentVersion(GetModuleLocation());
  FSimpleText := Format(StatusBar.SimpleText,
    [FileVersionStr, {$IFDEF WIN32}32{$ELSE}64{$ENDIF}]);
  StatusBar.SimpleText := FSimpleText;
  Application.OnException := AppException;
  UpdateFromSettings(False);
  HTMLViewer.OnHotSpotClick := dmResources.HtmlViewerHotSpotClick;
  HTMLViewer.OnImageRequest:= dmResources.HtmlViewerImageRequest;
end;

procedure TFrmPreview.FormDestroy(Sender: TObject);
begin
  HideAboutForm;
  SaveSettings;
  FCodeHighlightEmitter.Free;
  TLogPreview.Add('TFrmEditor.FormDestroy');
  inherited;
end;

procedure TFrmPreview.FormResize(Sender: TObject);
begin
  PanelMD.Height := Round(Self.Height * (FPreviewSettings.SplitterPos / 100));
  Splitter.Top := PanelMD.Height;
  if Self.Width < (560 * Self.ScaleFactor) then
  begin
    StyledToolBar.ShowCaptions := False;
    StyledToolBar.ButtonWidth := Round(30 * Self.ScaleFactor);
  end
  else
  begin
    StyledToolbar.ShowCaptions := True;
    StyledToolBar.ButtonWidth := Round(110 * Self.ScaleFactor);
  end;
  UpdateGUI;
  if FPreviewSettings.RescalingImage then
    ShowMarkDownAsHTML(FPreviewSettings, True);
end;

procedure TFrmPreview.LoadFromFile(const AFileName: string);
begin
  TLogPreview.Add('TFrmEditor.LoadFromFile Init');
  FFileName := AFileName;
  //NB: the ANSI fallback was missing here (the editor has it). TStrings raises
  //EEncodingError on a file that is not valid UTF-8, so an ANSI markdown file
  //made the preview fail and show an empty pane.
  try
    SynEdit.Lines.LoadFromFile(AFileName, TEncoding.UTF8);
  except
    on E: EEncodingError do
      SynEdit.Lines.LoadFromFile(AFileName, TEncoding.ANSI);
  end;
  HtmlViewer.ServerRoot := ExtractFilePath(FFileName);
  ShowMarkDownAsHTML(FPreviewSettings, True);
  TLogPreview.Add('TFrmEditor.LoadFromFile Done');
end;

procedure TFrmPreview.LoadFromStream(const AStream: TStream);
begin
  TLogPreview.Add('TFrmEditor.LoadFromStream Init');
  //Same ANSI fallback as LoadFromFile. NB: the stream has to be rewound before
  //reading it a second time.
  AStream.Position := 0;
  try
    SynEdit.Lines.LoadFromStream(AStream, TEncoding.UTF8);
  except
    on E: EEncodingError do
    begin
      AStream.Position := 0;
      SynEdit.Lines.LoadFromStream(AStream, TEncoding.ANSI);
    end;
  end;
  //Loaded from a stream: there is no document folder, so relative images
  //cannot be resolved. ServerRoot is left empty on purpose - it used to point
  //at the folder of the DLL, where an "images/logo.png" in the document would
  //pick up whatever happened to sit next to the library.
  HtmlViewer.ServerRoot := '';
  ShowMarkDownAsHTML(FPreviewSettings, True);
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
  LOldPos: Integer;
begin
  //Re-entrancy guard: the code-highlight emitter is shared and not reentrant,
  //so a second rendering started while the first is still running (a modal
  //settings dialog, a resize, a zoom) would free a highlighter still in use.
  if FRendering then
    Exit;
  FRendering := True;
  try
  try
    //NB: read the scroll position *before* Clear, which resets it to zero:
    //reading it afterwards always restored the top of the document.
    LOldPos := HtmlViewer.VScrollBarPosition;
    if AReloadImages then
      HtmlViewer.clear;
    if FCodeHighlightEmitter <> nil then
    begin
      if FPreviewSettings.UseDarkStyle then
        FCodeHighlightEmitter.SetTheme(True, HtmlViewer.DefBackground, clWhite,
          ASettings.HTMLFontName, ASettings.HTMLFontSize)
      else
        FCodeHighlightEmitter.SetTheme(False, HtmlViewer.DefBackground, clBlack,
          ASettings.HTMLFontName, ASettings.HTMLFontSize);
    end;
    FMarkDownFile := TMarkDownFile.Create(SynEdit.Lines.Text,
      ASettings.ProcessorDialect, True, FCodeHighlightEmitter, ASettings.AllowUnsafeHTML,
      ASettings.CustomCSS);

    //Carica il contenuto HTML trasformato dentro l'HTML-Viewer
    HtmlViewer.DefFontSize := ASettings.HTMLFontSize;
    HtmlViewer.DefFontName := ASettings.HTMLFontName;
    //NB: LoadFromString directly. Wrapping the HTML in a TStringStream only to
    //hand it over meant encoding the whole document to UTF-8 and decoding it
    //back into the very string it started from.
    HtmlViewer.LoadFromString(FMarkDownFile.HTML);
    HtmlViewer.VScrollBarPosition := LOldPos;
    HtmlViewer.Visible := True;
  except
    //An exception escaping a preview handler would hit Explorer, so it is not
    //re-raised. It is logged though: this unit uses TLogPreview everywhere
    //else, and a failed preview was otherwise completely invisible.
    on E: Exception do
      TLogPreview.Add(E);
  end;
  finally
    FRendering := False;
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
    (Self.Height - StyledToolbar.Height);
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
  ShowAboutForm(DialogPosRect, Title_SVGPreview, False);
end;

procedure TFrmPreview.ToolButtonMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := (Sender as TStyledToolButton).Hint;
end;

procedure TFrmPreview.ToolButtonMouseLeave(Sender: TObject);
begin
  StatusBar.SimpleText := FSimpleText;
end;

procedure TFrmPreview.UpdateFromSettings(const Preview: Boolean);
var
  LStyle: TStyledButtonDrawType;
begin
  FPreviewSettings.ReadSettings(SynEdit.Highlighter, nil);
  if FPreviewSettings.MDFontSize >= MinfontSize then
    MDFontSize := FPreviewSettings.MDFontSize
  else
    MDFontSize := MinfontSize;

  SynEdit.Font.Name := FPreviewSettings.MDFontName;

  //Rounded Buttons for StyledButtons
  if FPreviewSettings.ButtonDrawRounded then
    LStyle := btRounded
  else
    LStyle := btRoundRect;
  TStyledButton.RegisterDefaultRenderingStyle(LStyle);

  //Rounded Buttons for StyledToolbars
  if FPreviewSettings.ToolbarDrawRounded then
    LStyle := btRounded
  else
    LStyle := btRoundRect;
  TStyledToolbar.RegisterDefaultRenderingStyle(LStyle);
  StyledToolbar.StyleDrawType := LStyle;

  //Rounded Buttons for menus: StyledCategories and StyledButtonGroup
  if FPreviewSettings.MenuDrawRounded then
    LStyle := btRounded
  else
    LStyle := btRoundRect;
  TStyledButtonGroup.RegisterDefaultRenderingStyle(LStyle);

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
  if ShowSettings(DialogPosRect, Title_SVGPreview, SynEdit, SynEdit.Highlighter, FPreviewSettings, True) then
  begin
    FPreviewSettings.WriteSettings(SynEdit.Highlighter, nil);
    UpdateFromSettings(True);
  end;
end;

procedure TFrmPreview.ToolButtonSelectModeClick(Sender: TObject);
begin
  //TStyledToolButton(Sender).CheckMenuDropdown;
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
