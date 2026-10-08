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

{ TMarkdownViewer: a THTMLViewer that shows Markdown (and HTML) content.
  The conversion is made by TMarkdownViewerEngine (MarkDownViewerCommon), a
  TCustomMarkdownToHTML of the Markdown Processor: the Markdown properties
  (ProcessorDialect, Extensions, AllowUnsafe, MathRendering, CssStyle,
  MarkdownContent, HtmlContent) are the same as TMarkdownToHTML and
  TEdgeMarkdownViewer. }

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
  , MarkDownViewerCommon
  , MDCodeHighlightEmitter
  ;

resourcestring
  MARKDOWN_FILES = 'Markdown text files';
  HTML_FILES = 'HTML text files';

Type
  //declared in MarkDownViewerCommon: here for compatibility
  TFolderName = MarkDownViewerCommon.TFolderName;
  TFileNameClicked = MarkDownViewerCommon.TFileNameClicked;
  TURLClicked = MarkDownViewerCommon.TURLClicked;

  TCustomMarkdownViewer = class(THTMLViewer)
  private
    FFileName: TFileName;
    FEngine: TMarkdownViewerEngine;
    FRescalingImage: Boolean;
    FResetPosition: Boolean;
    FStream: TMemoryStream;
    FImageRequest: TGetImageEvent;
    FTabStop: Boolean;
    FAutoLoadOnHotSpotClick: boolean;
    FOnURLClicked: TURLClicked;
    FOnFileNameClicked: TFileNameClicked;
    FCodeHighlightEmitter: TCodeHighlightEmitterBase;
    procedure SetFileName(const AValue: TFileName);
    function GetProcessorDialect: TMarkdownProcessorDialect;
    procedure SetProcessorDialect(const AValue: TMarkdownProcessorDialect);
    function GetExtensions: TMarkdownExtensions;
    procedure SetExtensions(const AValue: TMarkdownExtensions);
    function GetAllowUnsafe: Boolean;
    procedure SetAllowUnsafe(const AValue: Boolean);
    function GetMathRendering: TMarkdownMathRendering;
    procedure SetMathRendering(const AValue: TMarkdownMathRendering);
    function GetCssStyle: TStringList;
    function GetHTMLContent: TStringList;
    function GetMarkdownContent: TStringList;
    function IsCssStyleStored: Boolean;
    function IsExtensionsStored: Boolean;
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
    procedure HTMLContentChanged(Sender: TObject);
    procedure EngineBeforeProcess(Sender: TObject);
    function IsServerRootStored: Boolean;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadState(Reader: TReader); override;
    procedure AutoLoadFile; virtual;
    procedure SetOnImageRequest(const AValue: TGetImageEvent); reintroduce;
    function FindHelpFile(var AFileName: TFileName; const AContext: Integer;
      const HelpKeyword: string): boolean; virtual;
    procedure Loaded; override;
    function HotSpotClickHandled: Boolean; override;
    /// <summary>The Markdown engine (options, contents, conversion).</summary>
    property Engine: TMarkdownViewerEngine read FEngine;
  public
    procedure LoadFromFile(const AFileName: TFileName);
    procedure ExportToFileHTML(const AFileName: TFileName);
    procedure LoadFromStream(const AStream: TStringStream;
      const IsHTMLContent: Boolean = False);
    procedure LoadFromString(const AValue: string;
      const IsHTMLContent: Boolean = False);
    /// <summary>Converts a Markdown text with the given dialect (and its
    /// default extensions), stylesheet and safe mode.</summary>
    function TransformContent(const AMarkdownContent: string;
      AProcessorDialect: TMarkdownProcessorDialect = DefaultMarkdownDialect;
      const ACssStyle: string = '';
      const AAllowUnsafe: Boolean = False): string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshViewer(const AReloadImages, ARescalingImage: Boolean;
      const APreservePosition: Boolean = True);

    //specific properties
    property AutoLoadOnHotSpotClick: boolean read FAutoLoadOnHotSpotClick write FAutoLoadOnHotSpotClick default True;
    property CssStyle: TStringList read GetCssStyle write SetCssStyle stored IsCssStyleStored;
    property FileName: TFileName read FFileName write SetFileName;
    //Changing the dialect resets Extensions to its defaults
    property ProcessorDialect: TMarkdownProcessorDialect read GetProcessorDialect write SetProcessorDialect default DefaultMarkdownDialect;
    //Optional syntax of mdCommonMark, mdGFM and mdGitHub (ignored by the
    //legacy dialects). Default: the extensions of the dialect + LegacyExtensions
    property Extensions: TMarkdownExtensions read GetExtensions write SetExtensions stored IsExtensionsStored;
    //When True, native HTML (script/iframe/object...) in the markdown is passed
    //through to the output; default False (safe mode).
    property AllowUnsafe: Boolean read GetAllowUnsafe write SetAllowUnsafe default False;
    //HTMLViewer does not run JavaScript: math formulas as images by default
    property MathRendering: TMarkdownMathRendering read GetMathRendering write SetMathRendering default mmrCodeCogsImage;
    property RescalingImage: Boolean read FRescalingImage write SetRescalingImage default False;
    property HtmlContent: TStringList read GetHTMLContent write SetHTMLContent stored IsHtmlContentStored;
    property MarkdownContent: TStringList read GetMarkdownContent write SetMarkdownContent;
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
    property Extensions;
    property AllowUnsafe;
    property MathRendering;
    property RescalingImage;
    property HtmlContent;
    property MarkdownContent;
    property OnImageRequest;
    property OnFileNameClicked;
    property OnURLClicked;
  end;

  //declared in MarkDownViewerCommon: here for compatibility
  THookControlActionLink = MarkDownViewerCommon.THookControlActionLink;

//declared in MarkDownViewerCommon: here for compatibility
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
  , Vcl.Imaging.pngImage
  ;

function TryLoadTextFile(const AFileName: TFileName): string;
begin
  Result := MarkDownViewerCommon.TryLoadTextFile(AFileName);
end;

procedure SaveUTF8File(const AFileName: TFileName;
  const AContent: string);
begin
  MarkDownViewerCommon.SaveUTF8File(AFileName, AContent);
end;

function GetMarkdownDefaultCSS: string;
begin
  Result := MarkDownViewerCommon.GetMarkdownDefaultCSS;
end;

procedure RegisterMDViewerServerRoot(const AFolder: string);
begin
  MarkDownViewerCommon.RegisterMDViewerServerRoot(AFolder);
end;

{ TCustomMarkdownViewer }

constructor TCustomMarkdownViewer.Create(AOwner: TComponent);
begin
  inherited;
  FStream := TMemoryStream.Create;
  FAutoLoadOnHotSpotClick := True;
  //Optional syntax-highlighting emitter for fenced code blocks (nil when the
  //MD_SYNTAX_HIGHLIGHTING define is off, so no SynEdit dependency is linked).
  FCodeHighlightEmitter := CreateCodeHighlightEmitter;

  //The Markdown engine: the viewer is refreshed when its HTML changes
  FEngine := TMarkdownViewerEngine.Create(nil);
  FEngine.CssStyle.Text := GetMarkdownDefaultCSS;
  FEngine.MathRendering := mmrCodeCogsImage;
  FEngine.CodeBlockEmitter := FCodeHighlightEmitter;
  FEngine.OnBeforeProcess := EngineBeforeProcess;
  FEngine.HtmlContent.OnChange := HTMLContentChanged;

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

  if GetMDViewerServerRoot <> '' then
    ServerRoot := GetMDViewerServerRoot;
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
var
  LLines: TStringList;
begin
  //Old property "Lines.Strings": its content is Markdown
  LLines := TStringList.Create;
  try
    Reader.ReadListBegin;
    while not Reader.EndOfList do
      LLines.Add(Reader.ReadString);
    Reader.ReadListEnd;
    LoadFromString(LLines.Text, False);
  finally
    LLines.Free;
  end;
end;

procedure TCustomMarkdownViewer.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Lines.Strings', ReadLines, nil, False);
end;

procedure TCustomMarkdownViewer.ReadState(Reader: TReader);
begin
  //Options and contents are read in any order: a single conversion at the end
  FEngine.BeginUpdate;
  try
    inherited;
  finally
    FEngine.EndUpdate;
  end;
end;

destructor TCustomMarkdownViewer.Destroy;
begin
  if Assigned(FEngine) then
    FEngine.HtmlContent.OnChange := nil;
  FreeAndNil(FEngine);
  FreeAndNil(FStream);
  FreeAndNil(FCodeHighlightEmitter);
  inherited;
end;

procedure TCustomMarkdownViewer.ExportToFileHTML(const AFileName: TFileName);
begin
  FEngine.ExportToFileHTML(AFileName);
end;

procedure TCustomMarkdownViewer.SetOnImageRequest(const AValue: TGetImageEvent);
begin
  FImageRequest := AValue;
  if Assigned(FImageRequest) then
    inherited OnImageRequest := FImageRequest
  else
    inherited OnImageRequest := HtmlViewerImageRequest;
end;

function TCustomMarkdownViewer.GetAllowUnsafe: Boolean;
begin
  Result := FEngine.AllowUnsafe;
end;

function TCustomMarkdownViewer.GetCssStyle: TStringList;
begin
  Result := FEngine.CssStyle;
end;

function TCustomMarkdownViewer.GetExtensions: TMarkdownExtensions;
begin
  Result := FEngine.Extensions;
end;

function TCustomMarkdownViewer.GetHelpContext: THelpContext;
begin
  Result := inherited HelpContext;
end;

function TCustomMarkdownViewer.GetHelpKeyword: String;
begin
  Result := inherited HelpKeyword;
end;

function TCustomMarkdownViewer.GetHTMLContent: TStringList;
begin
  Result := FEngine.HtmlContent;
end;

function TCustomMarkdownViewer.GetLines: TStrings;
begin
  Result := FEngine.HtmlContent;
end;

function TCustomMarkdownViewer.GetMarkdownContent: TStringList;
begin
  Result := FEngine.MarkdownContent;
end;

function TCustomMarkdownViewer.GetMathRendering: TMarkdownMathRendering;
begin
  Result := FEngine.MathRendering;
end;

function TCustomMarkdownViewer.GetOnImageRequest: TGetImageEvent;
begin
  Result := FImageRequest;
end;

function TCustomMarkdownViewer.GetProcessorDialect: TMarkdownProcessorDialect;
begin
  Result := FEngine.ProcessorDialect;
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
    StringReplace(FEngine.CssStyle.Text,sLineBreak,'',[rfReplaceAll]),
    StringReplace(GetMarkdownDefaultCSS, sLineBreak,'',[rfReplaceAll])
    );
end;

function TCustomMarkdownViewer.IsExtensionsStored: Boolean;
begin
  Result := FEngine.Extensions <> TMarkdownViewerEngine.DefaultExtensions(FEngine.ProcessorDialect);
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
  Result := (FEngine.HtmlContent.Text <> '') and (FEngine.MarkdownContent.Text = '');
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
  if FEngine.MarkdownContent.Text <> '' then
    RefreshViewer(True, FRescalingImage, False);
end;

procedure TCustomMarkdownViewer.LoadFromFile(const AFileName: TFileName);
begin
  LoadFromString(TryLoadTextFile(AFileName), IsHTMLFileName(AFileName));
end;

procedure TCustomMarkdownViewer.LoadFromStream(const AStream: TStringStream;
  const IsHTMLContent: Boolean = False);
begin
  LoadFromString(AStream.DataString, IsHTMLContent);
end;

procedure TCustomMarkdownViewer.LoadFromString(const AValue: string;
  const IsHTMLContent: Boolean = False);
begin
  //A new content: the viewer is refreshed once (HTMLContentChanged), from the
  //top of the document
  FResetPosition := True;
  try
    FEngine.LoadFromString(AValue, IsHTMLContent);
  finally
    FResetPosition := False;
  end;
end;

procedure TCustomMarkdownViewer.RefreshViewer(
  const AReloadImages, ARescalingImage: Boolean;
  const APreservePosition: Boolean = True);
var
  LOldPos: Integer;
begin
  //NB: read the scroll position *before* Clear, which resets it to zero:
  //reading it afterwards would always restore the top of the document.
  LOldPos := Self.VScrollBarPosition;
  if AReloadImages then
    Self.Clear;
  if FEngine.HtmlContent.Text = '' then
    Exit;
  //Load HTML content into HTML-Viewer
  try
    inherited LoadFromString(FEngine.HtmlContent.Text);
  finally
    if APreservePosition then
      Self.VScrollBarPosition := LOldPos;
  end;
end;

procedure TCustomMarkdownViewer.SetAllowUnsafe(const AValue: Boolean);
begin
  FEngine.AllowUnsafe := AValue;
end;

procedure TCustomMarkdownViewer.SetCssStyle(const AValue: TStringList);
begin
  FEngine.CssStyle := AValue;
end;

procedure TCustomMarkdownViewer.SetExtensions(const AValue: TMarkdownExtensions);
begin
  FEngine.Extensions := AValue;
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
begin
  if ResolveHelpFile(ServerRoot, HelpType, HelpKeyword, HelpContext,
    FindHelpFile, LFileName) then
    LoadFromFile(LFileName);
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
  if FEngine.HtmlContent.Text <> AValue.Text then
    LoadFromString(AValue.Text, True);
end;

procedure TCustomMarkdownViewer.SetLines(const Value: TStrings);
begin
  FEngine.HtmlContent.Assign(Value);
end;

procedure TCustomMarkdownViewer.SetMarkdownContent(const AValue: TStringList);
begin
  if FEngine.MarkdownContent.Text <> AValue.Text then
    LoadFromString(AValue.Text, False);
end;

procedure TCustomMarkdownViewer.SetMathRendering(const AValue: TMarkdownMathRendering);
begin
  FEngine.MathRendering := AValue;
end;

procedure TCustomMarkdownViewer.SetProcessorDialect(
  const AValue: TMarkdownProcessorDialect);
begin
  FEngine.ProcessorDialect := AValue;
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

procedure TCustomMarkdownViewer.EngineBeforeProcess(Sender: TObject);
var
  LBackground: TColor;
  LForeground: TColor;
  LDark: Boolean;
begin
  //Syntax highlighting of the code blocks with the colors of the viewer
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
  end;
end;

function TCustomMarkdownViewer.TransformContent(const AMarkdownContent: string;
  AProcessorDialect: TMarkdownProcessorDialect = DefaultMarkdownDialect;
  const ACssStyle: string = '';
  const AAllowUnsafe: Boolean = False): string;
begin
  EngineBeforeProcess(Self);
  Result := FEngine.TransformContent(AMarkdownContent, AProcessorDialect,
    ACssStyle, AAllowUnsafe);
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
            //Un-premultiply: the channel is <= alpha, so the result fits a byte
            PngRGB^.B := Round(BmpRGBA[X].B / BmpRGBA[X].A * 255);
            PngRGB^.R := Round(BmpRGBA[X].R / BmpRGBA[X].A * 255);
            PngRGB^.G := Round(BmpRGBA[X].G / BmpRGBA[X].A * 255);
          end else begin
            //Fully transparent pixel: the color channels carry no information.
            //NB: the previous "Round(channel * 255)" could reach 65025 and
            //overflow the byte (range error with {$R+}).
            PngRGB^.B := 0;
            PngRGB^.R := 0;
            PngRGB^.G := 0;
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
    //An image that cannot be decoded must not break the rendering of the whole
    //document, so no error is raised. It is reported to the debug output,
    //otherwise the failure would be completely invisible.
    on E: Exception do
    begin
      {$IFDEF DEBUG}
      OutputDebugString(PChar(Format('MDViewer - image "%s": %s (%s)',
        [AFileName, E.Message, E.ClassName])));
      {$ENDIF}
    end;
  end;
end;

function TCustomMarkdownViewer.HotSpotClickHandled: Boolean;
begin
  Result := Inherited HotSpotClickHandled;
  if not Result then
    Result := HandleLinkClicked(ServerRoot, URL, FAutoLoadOnHotSpotClick,
      FOnFileNameClicked, FOnURLClicked, LoadFromFile);
end;

procedure TCustomMarkdownViewer.HTMLContentChanged(Sender: TObject);
begin
  //Refresh viewer: from the top for a new content, else at the same position
  RefreshViewer(True, FRescalingImage, not FResetPosition);
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
    LWorkingPath := GetMDViewerServerRoot;
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
begin
  Result := FindMarkdownHelpFile(AFileName, AContext, HelpKeyword);
end;

end.
