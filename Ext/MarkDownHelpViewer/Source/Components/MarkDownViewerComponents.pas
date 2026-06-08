{******************************************************************************}
{                                                                              }
{       Viewer Components to show Markdown and HTML content                    }
{                                                                              }
{       Copyright (c) 2023-2026 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/MarkdownHelpViewer                         }
{                                                                              }
{******************************************************************************}
{  Those Components requires Two Libraries:                                    }
{                                                                              }
{   Delphi Markdown                                                            }
{   https://github.com/grahamegrieve/delphi-markdown                           }
{   Copyright (c) 2011+, Health Intersections Pty Ltd All rights reserved.     }
{                                                                              }
{   HTMLViewer - https://github.com/BerndGabriel/HtmlViewer                    }
{   Copyright (c) 1995 - 2008 by L. David Baldwin                              }
{   opyright (c) 1995 - 2023 by Anders Melander (DitherUnit.pas)               }
{   Copyright (c) 1995 - 2023 by Ron Collins (HtmlGif1.pas)                    }
{   Copyright (c) 2008 - 2009 by Sebastian Zierer (Delphi 2009 Port)           }
{   opyright (c) 2008 - 2010 by Arvid Winkelsdorf (Fixes)                      }
{   Copyright (c) 2009 - 2023 by HtmlViewer Team                               }
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
unit MarkDownViewerComponents;

interface

uses
  System.Classes
  , System.SysUtils
  , Winapi.Windows
  , Vcl.Graphics
  , Vcl.Controls
  , HTMLUn2
  , HtmlView
  , HtmlGlobals
  , MarkdownUtils
  , MarkdownProcessor
  , MDCodeHighlightEmitter
  ;

resourcestring
  MARKDOWN_FILES = 'Markdown text files';
  HTML_FILES = 'HTML text files';

Type
  TFolderName = string;

  TFileNameClicked = procedure (const AFileName: TFileName; out AHandled: Boolean) of Object;
  TURLClicked = procedure (const AURL: string; out AHandled: Boolean) of Object;

  TCustomMarkdownViewer = class(THTMLViewer)
  private
    FFileName: TFileName;
    FMarkdownContent: TStringList;
    FHTMLContent: TStringList;
    FCssStyle: TStringList;
    FProcessorDialect: TMarkdownProcessorDialect;
    FRescalingImage: Boolean;
    FStream: TMemoryStream;
    FImageRequest: TGetImageEvent;
    FTabStop: Boolean;
    FAutoLoadOnHotSpotClick: boolean;
    FOnURLClicked: TURLClicked;
    FOnFileNameClicked: TFileNameClicked;
    FCodeHighlightEmitter: TCodeHighlightEmitterBase;
    procedure SetFileName(const AValue: TFileName);
    procedure SetProcessorDialect(const AValue: TMarkdownProcessorDialect);
    function IsCssStyleStored: Boolean;
    function IsDefFontName: Boolean;
    function IsPrintMarginStored: Boolean;
    function IsPrintScaleStored: Boolean;
    function IsTouchStored: Boolean;

    procedure SetCssStyle(const AValue: TStringList);
    procedure SetRescalingImage(const AValue: Boolean);
    procedure SetHTMLContent(const AValue: TStringList);
    procedure SetMarkdownContent(const AValue: TStringList);
    procedure HtmlViewerImageRequest(Sender: TObject;
      const ASource: UnicodeString; var AStream: TStream);
    procedure ConvertImage(AFileName: string; const AMaxWidth: Integer;
      const ABackgroundColor: TColor);
    function GetOnImageRequest: TGetImageEvent;
    function IsHtmlContentStored: Boolean;
    function GetHelpContext: THelpContext;
    function GetHelpKeyword: String;
    function IsHelpContextStored: Boolean;
    function IsHelpKeywordStored: Boolean;
    procedure SetHelpContext(const AValue: THelpContext);
    procedure SetHelpKeyword(const AValue: String);
    procedure ReadLines(Reader: TReader);
    function InternalGetServerRoot: TFolderName;
    procedure InternalSetServerRoot(const AValue: TFolderName);
    procedure FormControlEnterEvent(Sender: TObject);
    procedure SetTabStop(const Value: Boolean);
    function GetText: string;
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    procedure MDContentChanged(Sender: TObject);
    procedure HTMLContentChanged(Sender: TObject);
    function IsServerRootStored: Boolean;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure AutoLoadFile; virtual;
    procedure SetOnImageRequest(const AValue: TGetImageEvent); reintroduce;
    function FindHelpFile(var AFileName: TFileName; const AContext: Integer;
      const HelpKeyword: string): boolean; virtual;
    procedure Loaded; override;
    function HotSpotClickHandled: Boolean; override;
  public
    procedure LoadFromFile(const AFileName: TFileName);
    procedure ExportToFileHTML(const AFileName: TFileName);
    procedure LoadFromStream(const AStream: TStringStream;
      const IsHTMLContent: Boolean = False);
    procedure LoadFromString(const AValue: string;
      const IsHTMLContent: Boolean = False);
    function TransformContent(const AMarkdownContent: string;
      AProcessorDialect: TMarkdownProcessorDialect = mdCommonMark;
      const ACssStyle: string = ''): string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshViewer(const AReloadImages, ARescalingImage: Boolean;
      const APreservePosition: Boolean = True);

    //specific properties
    property AutoLoadOnHotSpotClick: boolean read FAutoLoadOnHotSpotClick write FAutoLoadOnHotSpotClick default True;
    property CssStyle: TStringList read FCssStyle write SetCssStyle stored IsCssStyleStored;
    property FileName: TFileName read FFileName write SetFileName;
    property ProcessorDialect: TMarkdownProcessorDialect read FProcessorDialect write SetProcessorDialect default mdCommonMark;
    property RescalingImage: Boolean read FRescalingImage write SetRescalingImage default False;
    property HtmlContent: TStringList read FHTMLContent write SetHTMLContent stored IsHtmlContentStored;
    property MarkdownContent: TStringList read FMarkdownContent write SetMarkdownContent;
    property OnImageRequest: TGetImageEvent read GetOnImageRequest write SetOnImageRequest;
    property Lines: TStrings read GetLines write SetLines;
  published
    //Override Help properties because SetHelpKeyword and SetHelpContext are not Virtual
    property HelpKeyword: String read GetHelpKeyword write SetHelpKeyword stored IsHelpKeywordStored;
    property HelpContext: THelpContext read GetHelpContext write SetHelpContext stored IsHelpContextStored;

    //property ServerRoot derived to change type for Property Editor
    property ServerRoot: TFolderName read InternalGetServerRoot write InternalSetServerRoot stored IsServerRootStored;
    property TabStop: Boolean read FTabStop write SetTabStop default False;

    //inherited properties to change default
    property HistoryMaxCount default 0;
    property AlignWithMargins default True;
    property BorderStyle default htSingle;
    property DefBackground default clWindow;
    property DefFontName stored IsDefFontName;
    property DefFontSize default 10;
    property NoSelect default False;
    property PrintMarginBottom stored IsPrintMarginStored;
    property PrintMarginLeft stored IsPrintMarginStored;
    property PrintMarginRight stored IsPrintMarginStored;
    property PrintMarginTop stored IsPrintMarginStored;
    property PrintScale stored IsPrintScaleStored;
    property Text: string read GetText stored False;
    property Touch stored IsTouchStored;

    //Custom Event Handlers
    property OnFileNameClicked: TFileNameClicked read FOnFileNameClicked write FOnFileNameClicked;
    property OnURLClicked: TURLClicked read FOnURLClicked write FOnURLClicked;
  end;

  TMarkdownViewer = class(TCustomMarkdownViewer)
  published
    //specific properties
    property AutoLoadOnHotSpotClick;
    property CssStyle;
    property FileName;
    property ProcessorDialect;
    property RescalingImage;
    property HtmlContent;
    property MarkdownContent;
    property OnImageRequest;
    property OnFileNameClicked;
    property OnURLClicked;
  end;

  THookControlActionLink = class(TControlActionLink)
  protected
    function IsHelpContextLinked: Boolean; override;
  end;

function TryLoadTextFile(const AFileName: TFileName): string;
procedure SaveUTF8File(const AFileName: TFileName;
  const AContent: string);
function GetMarkdownDefaultCSS: string;

procedure RegisterMDViewerServerRoot(const AFolder: string);

implementation

uses
  System.StrUtils
  , HTMLSubs
  , Winapi.GDIPOBJ
  , Winapi.GDIPAPI
  , Winapi.ShLwApi
  , Winapi.ShellAPI
  , Vcl.Imaging.pngImage
  ;

var
  //To automate loading of component content
  _ServerRoot: string;
  AMarkdownFileExt: TArray<String>;
  AHTMLFileExt: TArray<String>;

function FileWithExtExists(var AFileName: TFileName;
  const AFileExt: array of string): boolean;
var
  I: Integer;
  LExt: string;
  LFileName: TFileName;
begin
  LExt := ExtractFileExt(AFileName);
  if LExt = '' then
    LFileName := AFileName+AFileExt[0]
  else
    LFileName := AFileName;
  Result := FileExists(LFileName);
  if not Result then
  begin
    LFileName := ExtractFilePath(AFileName)+ChangeFileExt(ExtractFileName(AFileName),'');
    for I := Low(AFileExt) to High(AFileExt) do
    begin
      LExt := AFileExt[I];
      LFileName := ChangeFileExt(LFileName, LExt);
      if FileExists(LFileName) then
      begin
        AFileName := LFileName;
        Result := True;
        break;
      end;
    end;
  end
  else
    AFileName := LFileName;
end;

procedure RegisterMDViewerServerRoot(const AFolder: string);
begin
  _ServerRoot := IncludeTrailingPathDelimiter(AFolder);
end;

function GetMarkdownDefaultCSS: string;
begin
  Result :=
    '<style type="text/css">'+sLineBreak+
    'code{'+sLineBreak+
    '  font-family: "Consolas", monospace;'+sLineBreak+
    '}'+sLineBreak+
    'pre{'+sLineBreak+
    '  border: 1px solid #ddd;'+sLineBreak+
    '  border-left: 3px solid #f36d33;'+sLineBreak+
    '  overflow: auto;'+sLineBreak+
    '  padding: 1em 1.5em;'+sLineBreak+
    '  display: block;'+sLineBreak+
    '}'+sLineBreak+
    'Blockquote{'+sLineBreak+
    '  border-left: 3px solid #d0d0d0;'+sLineBreak+
    '  padding-left: 0.5em;'+sLineBreak+
    '  margin-left:1em;'+sLineBreak+
    '}'+sLineBreak+
    'Blockquote p{'+sLineBreak+
    '  margin: 0;'+sLineBreak+
    '}'+sLineBreak+
    'table{'+sLineBreak+
    '  border:1px solid;'+sLineBreak+
    '  border-collapse:collapse;'+sLineBreak+
    '}'+sLineBreak+
    'th{'+
    '  padding:5px;'+sLineBreak+
    '  border:1px solid;'+sLineBreak+
    '}'+sLineBreak+
    'td{'+sLineBreak+
    '  padding:5px;'+sLineBreak+
    '  border:1px solid;'+sLineBreak+
    '}'+sLineBreak+
    '</style>'+sLineBreak;
end;

function TryLoadTextFile(const AFileName : TFileName): string;
var
  LStream: TStream;
  LStreamReader: TStreamReader;
begin
  Result := '';
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    try
      LStreamReader := TStreamReader.Create(LStream, TEncoding.UTF8);
      try
        while not LStreamReader.EndOfStream do
          Result := Result + LStreamReader.ReadLine + sLineBreak;
      finally
        FreeAndNil(LStreamReader);
      end;
    except
      On E: EEncodingError do
      begin
        LStream.Position := 0;
        LStreamReader := TStreamReader.Create(LStream, TEncoding.ANSI);
        try
          while not LStreamReader.EndOfStream do
            Result := Result + LStreamReader.ReadLine + sLineBreak;
        finally
          FreeAndNil(LStreamReader);
        end;
      end;
    end;
  finally
    FreeAndNil(LStream);
  end;
end;

procedure SaveUTF8File(const AFileName: TFileName;
  const AContent: string);
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create(AContent, TEncoding.UTF8);
  try
    LStream.SaveToFile(AFileName);
  finally
    FreeAndNil(LStream);
  end;
end;

{ TCustomMarkdownViewer }

constructor TCustomMarkdownViewer.Create(AOwner: TComponent);
begin
  inherited;
  FStream := TMemoryStream.Create;
  FMarkdownContent := TStringList.Create;
  FMarkdownContent.OnChange := MDContentChanged;
  FHTMLContent := TStringList.Create;
  FHTMLContent.OnChange := HTMLContentChanged;
  FCssStyle := TStringList.Create;
  FAutoLoadOnHotSpotClick := True;
  //Optional syntax-highlighting emitter for fenced code blocks (nil when the
  //MD_SYNTAX_HIGHLIGHTING define is off, so no SynEdit dependency is linked).
  FCodeHighlightEmitter := CreateCodeHighlightEmitter;

  inherited OnImageRequest := HtmlViewerImageRequest;

  //inherited properties: default changed
  AlignWithMargins :=  True;
  BorderStyle := htSingle;
  DefBackground := clWindow;
  DefFontName := 'Arial';
  DefFontSize := 10;
  NoSelect := False;

  //Use my version of FormControlEnterEvent (move to link only if TabStop is true)
  SectionList.ControlEnterEvent := FormControlEnterEvent;

  FProcessorDialect := mdCommonMark;
  FCssStyle.Text := GetMarkdownDefaultCSS;

  if _ServerRoot <> '' then
    ServerRoot := _ServerRoot;
end;

procedure TCustomMarkdownViewer.FormControlEnterEvent(Sender: TObject);
var
  Y, Pos: Integer;
begin
  if Sender is TFormControlObj then
  begin
    Y := TFormControlObj(Sender).DrawYY;
    Pos := VScrollBarPosition;
    if (Y < Pos) or (Y > Pos + ClientHeight - 20) then
    begin
      VScrollBarPosition := (Y - ClientHeight div 2);
      Invalidate;
    end;
  end
  else if (Sender is TFontObj) and (TabStop) then
  begin
    Y := TFontObj(Sender).DrawYY;
    Pos := VScrollBarPosition;
    if (Y < Pos) then
      VScrollBarPosition := Y
    else if (Y > Pos + ClientHeight - 30) then
      VScrollBarPosition := (Y - ClientHeight div 2);
    Invalidate;
  end
end;

procedure TCustomMarkdownViewer.ReadLines(Reader: TReader);
begin
  Reader.ReadListBegin;
  FMarkdownContent.Clear;
  while not Reader.EndOfList do
    FMarkdownContent.Add(Reader.ReadString);
  Reader.ReadListEnd;
  LoadFromString(FMarkdownContent.Text, False);
end;

procedure TCustomMarkdownViewer.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Lines.Strings', ReadLines, nil, False);
end;

destructor TCustomMarkdownViewer.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FMarkdownContent);
  FreeAndNil(FHTMLContent);
  FreeAndNil(FCssStyle);
  FreeAndNil(FCodeHighlightEmitter);

  inherited;
end;

procedure TCustomMarkdownViewer.ExportToFileHTML(const AFileName: TFileName);
begin
  SaveUTF8File(AFileName, FHTMLContent.Text);
end;

procedure TCustomMarkdownViewer.SetOnImageRequest(const AValue: TGetImageEvent);
begin
  FImageRequest := AValue;
  if Assigned(FImageRequest) then
    inherited OnImageRequest := FImageRequest
  else
    inherited OnImageRequest := HtmlViewerImageRequest;
end;

function TCustomMarkdownViewer.GetHelpContext: THelpContext;
begin
  Result := inherited HelpContext;
end;

function TCustomMarkdownViewer.GetHelpKeyword: String;
begin
  Result := inherited HelpKeyword;
end;

function TCustomMarkdownViewer.GetLines: TStrings;
begin
  Result := FHTMLContent;
end;

function TCustomMarkdownViewer.GetOnImageRequest: TGetImageEvent;
begin
  Result := FImageRequest;
end;

function TCustomMarkdownViewer.GetText: string;
begin
  Result := inherited Text;
end;

function TCustomMarkdownViewer.InternalGetServerRoot: TFolderName;
begin
  Result := inherited ServerRoot;
end;

function TCustomMarkdownViewer.IsCssStyleStored: Boolean;
begin
  Result := not SameText(
    StringReplace(FCssStyle.Text,sLineBreak,'',[rfReplaceAll]),
    StringReplace(GetMarkdownDefaultCSS, sLineBreak,'',[rfReplaceAll])
    );
end;

function TCustomMarkdownViewer.IsDefFontName: Boolean;
begin
  Result := DefFontName <> 'Arial';
end;

function TCustomMarkdownViewer.IsHelpContextStored: Boolean;
begin
  Result := ((ActionLink = nil) or not THookControlActionLink(ActionLink).IsHelpContextLinked)
    and (HelpContext <> 0);
end;

function TCustomMarkdownViewer.IsHelpKeywordStored: Boolean;
begin
  Result := ((ActionLink = nil) or not THookControlActionLink(ActionLink).IsHelpContextLinked)
    and (Helpkeyword <> '');
end;

function TCustomMarkdownViewer.IsHtmlContentStored: Boolean;
begin
  Result := (FHTMLContent.Text <> '') and  (FMarkdownContent.Text = '');
end;

function TCustomMarkdownViewer.IsPrintMarginStored: Boolean;
begin
  Result := (Round(PrintMarginBottom*10000000000) <> 20000000000) or
    (Round(PrintMarginLeft*10000000000) <> 20000000000) or
    (Round(PrintMarginRight*10000000000) <> 20000000000) or
    (Round(PrintMarginTop*10000000000) <> 20000000000);
end;

function TCustomMarkdownViewer.IsPrintScaleStored: Boolean;
begin
  Result := (Round(PrintScale*10000000000) <> 10000000000);
end;

function TCustomMarkdownViewer.IsServerRootStored: Boolean;
begin
  Result := inherited ServerRoot <> '';
end;

function TCustomMarkdownViewer.IsTouchStored: Boolean;
begin
  Result := (Touch.InteractiveGestures <> [igPan]) or
    (Touch.InteractiveGestureOptions <> [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia]);
end;

procedure TCustomMarkdownViewer.Loaded;
begin
  inherited;
  //Load html content into HtmlViewer, reset scrollbar position
  if FMarkdownContent.text <> '' then
    RefreshViewer(True, FRescalingImage, False);
end;

procedure TCustomMarkdownViewer.LoadFromFile(const AFileName: TFileName);
var
  LExt: string;
  LIsHTMLContent: Boolean;
begin
  //Load file
  LExt := ExtractFileExt(AFileName);
  LIsHTMLContent := SameText(LExt, 'HTML') or SameText(LExt, 'HTM');
  LoadFromString(TryLoadTextFile(AFileName), LIsHTMLContent);
end;

procedure TCustomMarkdownViewer.LoadFromStream(const AStream: TStringStream;
  const IsHTMLContent: Boolean = False);
begin
  LoadFromString(AStream.DataString, IsHTMLContent);
end;

procedure TCustomMarkdownViewer.LoadFromString(const AValue: string;
  const IsHTMLContent: Boolean = False);
begin
  //Load file
  if not IsHTMLContent then
  begin
    FMarkdownContent.Text := AValue;
  end
  else
  begin
    //Do not trasform content
    FHTMLContent.Text := AValue;
    FMarkdownContent.Text := '';
  end;
  //Load html content into HtmlViewer, reset scrollbar position
  RefreshViewer(True, FRescalingImage, False);
end;

procedure TCustomMarkdownViewer.MDContentChanged(Sender: TObject);
begin
  //Transform content into HtmlViewer
  FHTMLContent.Text := TransformContent(FMarkdownContent.Text, FProcessorDialect, FCssStyle.Text);
end;

procedure TCustomMarkdownViewer.RefreshViewer(
  const AReloadImages, ARescalingImage: Boolean;
  const APreservePosition: Boolean = True);
var
  LOldPos: Integer;
begin
  if AReloadImages then
    Self.Clear;
  if FHTMLContent.Text = '' then
    Exit;
  //Load HTML content into HTML-Viewer
  LOldPos := Self.VScrollBarPosition;
  try
    inherited LoadFromString(FHTMLContent.Text);
  finally
    if APreservePosition then
      Self.VScrollBarPosition := LOldPos;
  end;
end;

procedure TCustomMarkdownViewer.SetCssStyle(const AValue: TStringList);
begin
  FCssStyle.Assign(AValue);
end;

procedure TCustomMarkdownViewer.SetFileName(const AValue: TFileName);
begin
  if FFileName <> AValue then
  begin
    FFileName := AValue;
    if (AValue <> '') and FileExists(AValue) then
      LoadFromFile(AValue);
  end;
end;

procedure TCustomMarkdownViewer.AutoLoadFile;
var
  LFileName: TFileName;
  LRootFolder: string;
begin
  LRootFolder := ServerRoot;
  if LRootFolder = '' then
    LRootFolder := _ServerRoot;
  if LRootFolder <> '' then
  begin
    LRootFolder := IncludeTrailingPathDelimiter(LRootFolder);
    case HelpType of
      htKeyword:
      begin
        if HelpKeyword <> '' then
        begin
          LFileName := LRootFolder+HelpKeyword+'.md';
          if FindHelpFile(LFileName, 0, ChangeFileExt(HelpKeyword,'.md')) then
            LoadFromFile(LFileName);
        end;
      end;
      htContext:
      begin
        if HelpContext <> 0 then
        begin
          LFileName := LRootFolder+IntToStr(HelpContext)+'.md';
          if FindHelpFile(LFileName, HelpContext, '') then
            LoadFromFile(LFileName);
        end;
      end;
    end;
  end;
end;

procedure TCustomMarkdownViewer.SetHelpContext(const AValue: THelpContext);
begin
  if AValue <> HelpContext then
  begin
    inherited HelpContext := AValue;
    AutoLoadFile;
  end;
end;

procedure TCustomMarkdownViewer.SetHelpKeyword(const AValue: String);
begin
  if AValue <> HelpKeyword then
  begin
    inherited HelpKeyword := AValue;
    AutoLoadFile;
  end;
end;

procedure TCustomMarkdownViewer.SetHTMLContent(const AValue: TStringList);
begin
  if FHTMLContent.Text <> AValue.Text then
    LoadFromString(AValue.Text, True);
end;

procedure TCustomMarkdownViewer.SetLines(const Value: TStrings);
begin
  FHTMLContent.Assign(Value);
end;

procedure TCustomMarkdownViewer.SetMarkdownContent(const AValue: TStringList);
begin
  if FMarkdownContent.Text <> AValue.Text then
    LoadFromString(AValue.Text, False);
end;

procedure TCustomMarkdownViewer.SetProcessorDialect(
  const AValue: TMarkdownProcessorDialect);
begin
  FProcessorDialect := AValue;
end;

procedure TCustomMarkdownViewer.SetRescalingImage(const AValue: Boolean);
begin
  FRescalingImage := AValue;
end;

procedure TCustomMarkdownViewer.SetTabStop(const Value: Boolean);
begin
  FTabStop := Value;
end;

procedure TCustomMarkdownViewer.InternalSetServerRoot(const AValue: TFolderName);
begin
  if ServerRoot <> AValue then
  begin
    inherited ServerRoot := AValue;
    AutoLoadFile;
  end;
end;

function TCustomMarkdownViewer.TransformContent(const AMarkdownContent: string;
  AProcessorDialect: TMarkdownProcessorDialect = mdCommonMark;
  const ACssStyle: string = ''): string;
var
  LMarkdownProcessor: TMarkdownProcessor;
  LBackground: TColor;
  LForeground: TColor;
  LDark: Boolean;
begin
  //Transform file Markdown in HTML using TMarkdownProcessor
  LMarkdownProcessor := TMarkdownProcessor.CreateDialect(AProcessorDialect);
  Try
    //Optional syntax highlighting of fenced code blocks. The caller owns the
    //emitter, so we detach it before freeing the processor (TConfiguration
    //frees its codeBlockEmitter).
    if FCodeHighlightEmitter <> nil then
    begin
      LBackground := ColorToRGB(DefBackground);
      LDark := (GetRValue(LBackground) * 299 + GetGValue(LBackground) * 587 +
        GetBValue(LBackground) * 114) div 1000 < 128;
      if LDark then
        LForeground := clWhite
      else
        LForeground := clBlack;
      FCodeHighlightEmitter.SetTheme(LDark, DefBackground, LForeground,
        DefFontName, DefFontSize);
      LMarkdownProcessor.Config.codeBlockEmitter := FCodeHighlightEmitter;
    end;
    Result := ACssStyle+LMarkdownProcessor.Process(AMarkdownContent);
  Finally
    if FCodeHighlightEmitter <> nil then
      LMarkdownProcessor.Config.codeBlockEmitter := nil;
    LMarkdownProcessor.Free;
  End;
end;

procedure TCustomMarkdownViewer.ConvertImage(AFileName: string;
  const AMaxWidth: Integer; const ABackgroundColor: TColor);
var
  {$if CompilerVersion > 33}
  LPngImage: TPngImage;
  LBitmap: TBitmap;
  LScaleFactor: double;
  {$endif}
  LImage, LScaledImage: TWICImage;
  LFileExt: string;

  {$if CompilerVersion > 33}
  function PNG4TransparentBitMap(aBitmap: TBitmap): TPNGImage;
  type
    TRGB = packed record B, G, R: byte end;
    TRGBA = packed record B, G, R, A: byte end;
    TRGBAArray = array[0..0] of TRGBA;

  var
    X, Y: integer;
    BmpRGBA: ^TRGBAArray;
    PngRGB: ^TRGB;
  begin
    //201011 Thomas Wassermann
    Result := TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, aBitmap.Width , aBitmap.Height);

    Result.CreateAlpha;
    Result.Canvas.CopyMode:= cmSrcCopy;
    Result.Canvas.Draw(0, 0, aBitmap);

    for Y := 0 to Pred(aBitmap.Height) do
    begin
      BmpRGBA := aBitmap.ScanLine[Y];
      PngRGB:= Result.Scanline[Y];

      for X := 0 to Pred(aBitmap.width) do
      begin
        Result.AlphaScanline[Y][X] :=  BmpRGBA[X].A;
        if aBitmap.AlphaFormat in [afDefined, afPremultiplied] then
        begin
          if BmpRGBA[X].A <> 0 then
          begin
            PngRGB^.B := Round(BmpRGBA[X].B / BmpRGBA[X].A * 255);
            PngRGB^.R := Round(BmpRGBA[X].R / BmpRGBA[X].A * 255);
            PngRGB^.G := Round(BmpRGBA[X].G / BmpRGBA[X].A * 255);
          end else begin
            PngRGB^.B := Round(BmpRGBA[X].B * 255);
            PngRGB^.R := Round(BmpRGBA[X].R * 255);
            PngRGB^.G := Round(BmpRGBA[X].G * 255);
          end;
        end;
        Inc(PngRGB);
      end;
    end;
  end;

  function CalcScaleFactor(const AWidth: integer): double;
  begin
    if AWidth > AMaxWidth then
      Result := AMaxWidth / AWidth
    else
      Result := 1;
  end;

  procedure MakeTransparent(DC: THandle);
  var
    Graphics: TGPGraphics;
  begin
    Graphics := TGPGraphics.Create(DC);
    try
      Graphics.Clear(aclTransparent);
    finally
      Graphics.Free;
    end;
  end;
  {$endif}

begin
  LFileExt := ExtractFileExt(AFileName);
  try
    FStream.Position := 0;
    LImage := nil;
    LScaledImage := nil;
    try
      begin
        LImage := TWICImage.Create;
        LImage.LoadFromStream(FStream);
        {$if CompilerVersion > 33}
        //Rescaling bitmap and save to stream
        LScaleFactor := CalcScaleFactor(LImage.Width);
        if (FRescalingImage) and (LScaleFactor <> 1) then
        begin
          LScaledImage :=  LImage.CreateScaledCopy(
            Round(LImage.Width*LScaleFactor),
            Round(LImage.Height*LScaleFactor),
            wipmHighQualityCubic);
          LBitmap := TBitmap.Create(LScaledImage.Width,LScaledImage.Height);
          try
            MakeTransparent(LBitmap.Canvas.Handle);
            LBitmap.Canvas.Draw(0,0,LScaledImage);
            FStream.Clear;
            if LBitmap.TransparentMode = tmAuto then
              LBitmap.SaveToStream(FStream)
            else
            begin
              LPngImage := PNG4TransparentBitMap(LBitmap);
              try
                LPngImage.SaveToStream(FStream);
              finally
                LPngImage.Free;
              end;
            end;
          finally
            LBitmap.Free;
          end;
        end
        else
          FStream.Position := 0;
        {$else}
        FStream.Position := 0;
        {$endif}
      end;
    finally
      LImage.Free;
      LScaledImage.Free;
    end;
  except
    //don't raise any error
  end;
end;

function TCustomMarkdownViewer.HotSpotClickHandled: Boolean;
var
  LFileName: TFileName;
  LRootFolder: string;
begin
  Result := Inherited HotSpotClickHandled;
  if not Result then
  begin
    //Prepare FileName
    LRootFolder := ServerRoot;
    if LRootFolder = '' then
      LRootFolder := _ServerRoot;
    LFileName := IncludeTrailingPathDelimiter(LRootFolder)+URL;
    //User Event
    if Assigned(FOnFileNameClicked) then
      FOnFileNameClicked(LFileName, Result);
    //Auto load file
    if not Result and FileExists(LFileName) and FAutoLoadOnHotSpotClick then
    begin
      LoadFromFile(LFileName);
      Result := True;
    end
    else if PathIsURL(PChar(URL)) then
    begin
      if Assigned(FOnURLClicked) then
        FOnURLClicked(URL, Result);
      if not Result and FAutoLoadOnHotSpotClick  then
      begin
        //Try to Open an URL
        ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
        Result := True;
      end;
    end;
  end;
end;

procedure TCustomMarkdownViewer.HTMLContentChanged(Sender: TObject);
begin
  //Refresh viewer
  RefreshViewer(True, FRescalingImage);
end;

procedure TCustomMarkdownViewer.HtmlViewerImageRequest(Sender: TObject;
  const ASource: UnicodeString; var AStream: TStream);
var
  LFullName: String;
  LHtmlViewer: THtmlViewer;
  LMaxWidth: Integer;
  LWorkingPath: TFolderName;
Begin
  LHtmlViewer := sender as THtmlViewer;
  AStream := nil;
  if ServerRoot <> '' then
    LWorkingPath := ServerRoot
  else if FFileName <> '' then
    LWorkingPath := ExtractFilePath(FFileName)
  else
    LWorkingPath := _ServerRoot;
  // is "fullName" a local file, if not acquire file from internet
  // replace %20 spaces to normal spaces
  LFullName := StringReplace(ASource,'%20',' ',[rfReplaceAll]);
  If not FileExists(LFullName) then
  begin
    LFullName := IncludeTrailingPathDelimiter(LWorkingPath)+LFullName;
    If not FileExists(LFullName) then
      LFullName := ASource;
  end;

  LFullName := Self.HTMLExpandFilename(LFullName);

  LMaxWidth := LHtmlViewer.ClientWidth - LHtmlViewer.VScrollBar.Width - (LHtmlViewer.MarginWidth * 2);
  if FileExists(LFullName) then  // if local file, load it..
  Begin
    FStream.LoadFromFile(LFullName);
    //Convert image to stretch size of HTMLViewer
    ConvertImage(LFullName, LMaxWidth, LHtmlViewer.DefBackground);
    AStream := FStream;
  end;
End;

function TCustomMarkdownViewer.FindHelpFile(
  var AFileName: TFileName;
  const AContext: Integer; const HelpKeyword: string): boolean;
var
  LHelpFileName: TFileName;
  LName, LPath, LKeyWord: string;
begin
  //WARNING: if changing this function, change also TMarkdownHelpViewer.FindHelpFile
  if HelpKeyword <> '' then
    LKeyWord := HelpKeyword
  else if AContext <> 0 then
    LKeyWord := IntToStr(AContext)+'.md'
  else
    LKeyword := '';

  //First, Try the Keyword only
  LPath := ExtractFilePath(AFileName);
  LHelpFileName := LPath+LKeyword;
  Result := FileWithExtExists(LHelpFileName, AMarkdownFileExt) or
    FileWithExtExists(LHelpFileName, AHTMLFileExt);

  if not Result then
  begin
    //Then, try the Help Name and the Keyword (eg.Home1000.ext)
    LName := ChangeFileExt(ExtractFileName(AFileName),'');
    LHelpFileName := LPath+LName+LKeyword;
    Result := FileWithExtExists(LHelpFileName, AMarkdownFileExt) or
      FileWithExtExists(LHelpFileName, AHTMLFileExt);
    if not Result then
    begin
      //At least, try the Help Name and the Keyword with '_' (eg.Home_1000.ext)
      LHelpFileName := LPath+LName+'_'+LKeyword;
      Result := FileWithExtExists(LHelpFileName, AMarkdownFileExt) or
        FileWithExtExists(LHelpFileName, AHTMLFileExt);
    end;
  end;

  if Result then
    AFileName := LHelpFileName;
end;

{ THookControlActionLink }

function THookControlActionLink.IsHelpContextLinked: Boolean;
begin
  Result := inherited IsHelpContextLinked;
end;

initialization
  SetLength(AMarkdownFileExt, 9);
  AMarkdownFileExt[0] := '.md';
  AMarkdownFileExt[1] := '.mkd';
  AMarkdownFileExt[2] := '.mdwn';
  AMarkdownFileExt[3] := '.mdown';
  AMarkdownFileExt[4] := '.mdtxt';
  AMarkdownFileExt[5] := '.mdtext';
  AMarkdownFileExt[6] := '.markdown';
  AMarkdownFileExt[7] := '.txt';
  AMarkdownFileExt[8] := '.text';

  SetLength(AHTMLFileExt, 2);
  AHTMLFileExt[0] := '.html';
  AHTMLFileExt[1] := '.htm';

end.
