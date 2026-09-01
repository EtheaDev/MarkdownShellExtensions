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
unit MDShellEx.Resources;

interface

uses
    Windows
  , SysUtils
  , Classes
  , Vcl.Graphics
  , Vcl.ImgList
  , Vcl.Controls
  , System.ImageList
  , SynHighlighterMarkdown
  , SynEditOptionsDialog
  , SynEditPrint
  , SynEditCodeFolding
  , SynEditHighlighter
  , SVGIconImageListBase
  , SVGIconImageList
  , Vcl.BaseImageCollection
  , SVGIconImageCollection
  , Xml.xmldom
  , Xml.XMLIntf
  , Xml.Win.msxmldom
  , Xml.XMLDoc
  , vmHtmlToPdf
  , HtmlGlobals
  , HtmlView
  , MDShellEx.Settings
  , Vcl.Dialogs
  , MarkdownProcessor
  , Vcl.VirtualImageList
  , Vcl.ImageCollection
  , MarkdownUtils;

resourcestring
  FILE_SAVED = 'File "%s" succesfully saved. Do you want to open it now?';

type
  TMarkDownFile = record
  private
    FParsed: Boolean;
    FMarkDownContent: string;
    FProcessorDialect: TMarkdownProcessorDialect;
    FHTML: string;
    FCodeBlockEmitter: TBlockEmitter;
    FAllowUnsafe: Boolean;
    FCSS: string;
    procedure SetMarkDownContent(const AValue: string);
  public
    //Built-in default stylesheet (the <style>...</style> block prepended to the
    //generated HTML). Exposed as a class function so callers (e.g. the Settings
    //GUI) can show/reset it. Pass a non-empty ACSS to Create to override it.
    class function GetDefaultCSS: string; static;
    constructor Create(const AMarkDownContent: string;
      const AProcessorDialect: TMarkdownProcessorDialect;
      const AParseImmediately: Boolean = True;
      const ACodeBlockEmitter: TBlockEmitter = nil;
      const AAllowUnsafe: Boolean = False;
      const ACSS: string = '');

    procedure Clear;
    procedure Parse;

    property Parsed: Boolean read FParsed;
    property MarkDownContent: string read FMarkDownContent write SetMarkDownContent;
    property HTML: string read FHTML;
  end;

  TdmResources = class(TDataModule)
    SynMarkdownSyn: TSynMarkdownSyn;
    SynMarkdownSynDark: TSynMarkdownSyn;
    SVGIconImageCollection: TSVGIconImageCollection;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLoadingImages: Boolean;
    FStream: TMemoryStream;
    FStopImageRequest: Boolean;
    function ConvertImage(AFileName: string;
      const AMaxWidth: Integer; const ABackgroundColor: TColor): Boolean;
    function getStreamData(const AFileName : String;
      const AMaxWidth: Integer; const ABackgroundColor: TColor): TStream;
    function OpenURL(const AUrl: string): Boolean;
    //Settings belongs to the form that supplied it and can already be nil (or
    //be replaced by another consumer), because the data module is shared
    function RescalingImageEnabled: Boolean;
  public
    Settings: TSettings;
    //When True, image loading pumps the message queue to keep the UI responsive
    //(and to let ESC interrupt it). Only the editor sets it: inside the preview
    //handler that would pump *Explorer's* queue from the middle of the HTML
    //layout, and there is no cancel command there to make it worthwhile.
    PumpMessagesWhileLoading: Boolean;
    procedure TryExpandSpaces(const ARootFolder: string;
      var AFileName: TFileName);
    procedure StopLoadingImages(const AStop: Boolean);
    procedure HtmlViewerImageRequest(Sender: TObject; const ASource: UnicodeString;
      var AStream: TStream);
    procedure HtmlViewerHotSpotClick(Sender: TObject; const ASource: ThtString;
      var Handled: Boolean);
    function GetSynHighlighter(const ADarkStyle: boolean;
      const ABackgroundColor: TColor) : TSynCustomHighlighter;
    function LoadFileContent(const AFileName: TFileName;
      const ARootFolder: string; const AMaxWidth: Integer;
      const ABackGroundColor: TColor; out AStream: TStream): Boolean;
    function IsLoadingImages: Boolean;
  end;

//Shared instance of the data module, created on first use and freed when this
//unit is finalized.
//NB: it used to be a plain global variable that each consumer created and
//destroyed on its own - the preview form in its constructor/destructor, the
//editor through Application.CreateForm. With two preview handlers alive in the
//same Explorer process that meant, depending on the destruction order, either
//leaking a data module or freeing the one the other instance was still using.
//It is exposed as a function so that every "dmResources.Something" call site
//keeps working unchanged.
function dmResources: TdmResources;

//True when the shared instance already exists. Needed because dmResources
//creates it on demand: a destructor that only wants to detach itself must not
//resurrect the data module while the application is shutting down.
function dmResourcesCreated: Boolean;

implementation

{$R *.dfm}

uses
  System.StrUtils
  , Vcl.Themes
  , Winapi.GDIPOBJ
  , Winapi.GDIPAPI
  , System.IOUtils
  , System.UITypes
  , Winapi.ShellAPI
  , SynPDF
  , Winapi.Messages
  , Vcl.Forms
  , IdHTTP
  , IdSSLOpenSSL
  , SVGIconImage
  , pngimage
  , JPeg
  , GIFImg
  , SVGInterfaces
  , SVGIconUtils
  , Vcl.Skia
  , uLogExcept
  ;

var
  _dmResources: TdmResources;

function dmResources: TdmResources;
begin
  if _dmResources = nil then
    _dmResources := TdmResources.Create(nil);
  Result := _dmResources;
end;

function dmResourcesCreated: Boolean;
begin
  Result := Assigned(_dmResources);
end;

procedure TdmResources.DataModuleCreate(Sender: TObject);
begin
  FStream := TMemoryStream.Create;
end;

procedure TdmResources.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FStream);
  inherited;
end;

function TdmResources.GetSynHighlighter(
  const ADarkStyle: boolean;
  const ABackgroundColor: TColor): TSynCustomHighlighter;
var
  LSyn: TSynMarkdownSyn;
  I: Integer;

  procedure SetFg(const AAttri: TSynHighlighterAttributes;
    const ALight, ADark: TColor);
  begin
    if ADarkStyle then
      AAttri.Foreground := ADark
    else
      AAttri.Foreground := ALight;
  end;

begin
  if ADarkStyle then
    LSyn := dmResources.SynMarkdownSynDark
  else
    LSyn := dmResources.SynMarkdownSyn;
  //Align every token attribute background to the editor background so the
  //markdown highlighter blends with the current theme (idempotent).
  for I := 0 to LSyn.AttrCount - 1 do
    LSyn.Attribute[I].Background := ABackgroundColor;
  //Theme-aware foreground palette (light / dark), idempotent: keeps good
  //contrast on both light and dark editor backgrounds.
  SetFg(LSyn.HeadingAttri,         clWebMediumBlue, clWebCornflowerBlue);
  SetFg(LSyn.EmphasisAttri,        clWebPurple,     clWebOrchid);
  SetFg(LSyn.StrongAttri,          clWebPurple,     clWebOrchid);
  SetFg(LSyn.CodeAttri,            clWebFirebrick,  clWebSandyBrown);
  SetFg(LSyn.LinkAttri,            clWebRoyalBlue,  clWebDeepSkyBlue);
  SetFg(LSyn.ListAttri,            clWebTeal,       clWebMediumAquamarine);
  SetFg(LSyn.BlockQuoteAttri,      clWebDimGray,    clWebDarkGray);
  SetFg(LSyn.DeleteAttri,          clWebDimGray,    clWebDarkGray);
  SetFg(LSyn.EntityReferenceAttri, clWebSeaGreen,   clWebDarkSeaGreen);
  SetFg(LSyn.HtmlTagAttri,         clWebDarkViolet, clWebOrchid);
  SetFg(LSyn.HtmlAttrNametAttri,   clWebMediumBlue, clWebLightBlue);
  SetFg(LSyn.HtmlAttrValueAttri,   clWebCrimson,    clWebSandyBrown);
  SetFg(LSyn.HtmlCommentAttri,     clWebGreen,      clWebMediumSeaGreen);
  Result := LSyn;
end;

procedure TdmResources.HtmlViewerHotSpotClick(Sender: TObject;
  const ASource: ThtString; var Handled: Boolean);
begin
  Handled := OpenUrl(ASource);
end;

procedure TdmResources.TryExpandSpaces(const ARootFolder: string; var AFileName: TFileName);
var
  LOriginalFileName: TFileName;
begin
  LOriginalFileName := AFileName;
  // if "AFileName" is not a local file (eg. is file from internet)
  // replace %20 spaces to normal spaces
  AFileName := StringReplace(AFileName,'%20',' ',[rfReplaceAll]);
  If not FileExists(AFileName) then
  begin
    //If not exists, try to include ARootFolder into FileName
    AFileName := IncludeTrailingPathDelimiter(ARootFolder)+AFileName;
    //Restore original file name because is not a local file
    If not FileExists(AFileName) then
      AFileName := LOriginalFileName;
  end;
end;

procedure TdmResources.HtmlViewerImageRequest(Sender: TObject;
  const ASource: UnicodeString; var AStream: TStream);
var
  LHtmlViewer: THtmlViewer;
  LFullName: TFileName;
  LMaxWidth: Integer;
begin
  if FStopImageRequest then
    Exit;
  FLoadingImages := True;
  Try
    //NB: pumping the message queue here means doing it in the middle of the
    //HTML layout, so a user action can start a second rendering while the
    //first is still running. The render entry points guard against that; in
    //the shell extension the pumping is off altogether.
    if PumpMessagesWhileLoading then
      Application.ProcessMessages;
    LHtmlViewer := sender as THtmlViewer;
    LMaxWidth := LHtmlViewer.ClientWidth - LHtmlViewer.VScrollBar.Width - (LHtmlViewer.MarginWidth * 2);

    // HTMLViewer needs to be nil'ed
    AStream := nil;

    LFullName := ASource;
    TryExpandSpaces(LHtmlViewer.ServerRoot, LFullName);
    LFullName := LHtmlViewer.HTMLExpandFilename(LFullName);
  
    LoadFileContent(LFullName, LHtmlViewer.ServerRoot, LMaxWidth,
      LHtmlViewer.DefBackground, AStream);
  Finally
    FLoadingImages := False;
  End;
end;

function TdmResources.LoadFileContent(const AFileName: TFileName;
  const ARootFolder: string; const AMaxWidth: Integer;
  const ABackGroundColor: TColor;
  out AStream: TStream): Boolean;
var
  LDownLoadFromWeb: boolean;
Begin
  Result := True;
  try
    if FileExists(AFileName) then  // if local file, load it..
    Begin
      FStream.LoadFromFile(AFileName);
      //Convert image to stretch size of HTMLViewer
      Result := ConvertImage(AFileName, AMaxWidth, ABackGroundColor);
      if not Result then
        Exit;
      AStream := FStream;
    end
    else if SameText('http', Copy(AFileName,1,4)) then
    Begin
      LDownLoadFromWeb := (Settings is TEditorSettings) and
        TEditorSettings(Settings).DownloadFromWEB;
      if LDownLoadFromWeb then
      begin
        //Load from remote. NB: use the returned stream, which is nil when the
        //download or the decoding failed: assigning FStream unconditionally
        //handed the viewer the leftovers of the previous image.
        AStream := getStreamData(AFileName, AMaxWidth, ABackGroundColor);
        Result := AStream <> nil;
      end;
    End;
  except
    //No exception for EInvalidGraphic
    Result := False;
  end;
End;

function TdmResources.OpenURL(const AUrl: string): Boolean;
begin
  ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOWNORMAL);
  Result := True;
end;

function TdmResources.IsLoadingImages: Boolean;
begin
  Result := FLoadingImages;
end;

function TdmResources.RescalingImageEnabled: Boolean;
begin
  //Nil-safe: the data module outlives the forms that set Settings
  Result := Assigned(Settings) and Settings.RescalingImage;
end;

procedure TdmResources.StopLoadingImages(const AStop: Boolean);
begin
  FStopImageRequest := AStop;
end;

function TdmResources.getStreamData(const AFileName : String;
  const AMaxWidth: Integer; const ABackgroundColor: TColor): TStream;
const
  //A server can answer with an HTML "moved" page instead of a redirect header:
  //the link is followed manually, but only a limited number of times.
  MAX_REDIRECT = 5;
  //Only a very small payload can be an error or "moved" page, not an image
  MAX_HTML_ANSWER_SIZE = 1024;

  //Reads the body as text only to inspect a "moved"/"not found" page. The
  //binary content of FStream is never rebuilt from this string: the previous
  //version round-tripped it through an ANSI TStringStream and wrote it to a
  //temporary file that nobody ever read (and nobody ever deleted).
  function IsHtmlAnswer(out AContent: string): Boolean;
  var
    LBytes: TBytes;
  begin
    Result := (FStream.Size > 0) and (FStream.Size < MAX_HTML_ANSWER_SIZE);
    AContent := '';
    if not Result then
      Exit;
    SetLength(LBytes, FStream.Size);
    FStream.Position := 0;
    FStream.ReadBuffer(LBytes[0], Length(LBytes));
    AContent := TEncoding.ANSI.GetString(LBytes);
    FStream.Position := 0;
  end;

  //Extracts the target of the first <a href="..."> of a "moved" page
  function TryGetMovedUrl(const AContent: string; out AUrl: string): Boolean;
  var
    LLowerContent: string;
    P: Integer;
  begin
    Result := False;
    AUrl := '';
    LLowerContent := LowerCase(AContent);
    if (Pos('301 moved permanently', LLowerContent) = 0) and
       (Pos('<html><body>', LLowerContent) = 0) then
      Exit;
    P := Pos('<a href="', LLowerContent);
    if P = 0 then
      Exit;
    //Searched on the lowercase copy, extracted from the original one
    AUrl := Copy(AContent, P + Length('<a href="'), MaxInt);
    P := Pos('"', AUrl);
    if P <= 1 then
    begin
      AUrl := '';
      Exit;
    end;
    AUrl := Copy(AUrl, 1, P - 1);
    Result := True;
  end;

  //File name of an URL, without query string and fragment: it is what selects
  //the decoder in ConvertImage
  function UrlToFileName(const AUrl: string): TFileName;
  var
    LName: string;
    P: Integer;
  begin
    LName := AUrl;
    P := Pos('?', LName);
    if P > 0 then
      LName := Copy(LName, 1, P - 1);
    P := Pos('#', LName);
    if P > 0 then
      LName := Copy(LName, 1, P - 1);
    Result := ExtractFileName(StringReplace(LName, '/', '\', [rfReplaceAll]));
  end;

var
  LIdHTTP   : TIdHTTP;
  LIdSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  LUrl, LMovedUrl, LContent: string;
  LRedirectCount: Integer;
  LDone: Boolean;
Begin
  //downloading Image from WEB
  Result := nil;
  LUrl := AFileName;
  LRedirectCount := 0;
  LIdHTTP := nil;
  LIdSSLIOHandler := nil;
  try
    LIdHTTP := TIdHTTP.Create;
    LIdHTTP.AllowCookies := True;
    LIdHTTP.HandleRedirects := True;
    LIdSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(LIdHTTP);
    LIdSSLIOHandler.DefaultPort := 0;
    LIdSSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    LIdHTTP.IOHandler := LIdSSLIOHandler;

    LIdHTTP.Request.UserAgent :=
      'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0';

    repeat
      LDone := True;
      FStream.Clear;
      try
        LIdHTTP.Get(LUrl, FStream);
      except
        //A network failure simply means no image: it is not reported
        FStream.Clear;
        Exit;
      end;

      if FStream.Size = 0 then
        Exit;

      if IsHtmlAnswer(LContent) then
      begin
        if Pos('Not Found', LContent) > 0 then
          Exit;
        if TryGetMovedUrl(LContent, LMovedUrl) and
          (LRedirectCount < MAX_REDIRECT) then
        begin
          LUrl := LMovedUrl;
          Inc(LRedirectCount);
          LDone := False;
        end;
      end;
    until LDone;

    //The decoder is selected from the extension of the URL actually fetched.
    //No temporary file is involved: ConvertImage works on FStream.
    if ConvertImage(UrlToFileName(LUrl), AMaxWidth, ABackgroundColor) then
      Result := FStream;
  finally
    LIdSSLIOHandler.Free;
    LIdHttp.Free;
  end;
end;

function TdmResources.ConvertImage(AFileName: string;
  const AMaxWidth: Integer; const ABackgroundColor: TColor): Boolean;
var
  LPngImage: TPngImage;
  LBitmap: TBitmap;
  LImage, LScaledImage: TWICImage;
  LFileExt: string;
  LScaleFactor: double;
  LSVG: ISVG;

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

begin
  Result := True;
  LFileExt := ExtractFileExt(AFileName);
  try
    FStream.Position := 0;
    if SameText(LFileExt,'.svg') then
    begin
      LSVG := GlobalSVGFactory.NewSvg;
      LSVG.LoadFromStream(FStream);
      LScaleFactor := CalcScaleFactor(Round(Lsvg.Width));
      if RescalingImageEnabled and (LScaleFactor <> 1) then
      begin
        LBitmap := TBitmap.Create(
          Round(LSVG.Width * LScaleFactor),
          Round(LSVG.Height* LScaleFactor));
      end
      else
      begin
        LBitmap := TBitmap.Create(Round(LSVG.Width), Round(LSVG.Height));
      end;
      try
        LBitmap.PixelFormat := pf32bit;
        MakeTransparent(LBitmap.Canvas.Handle);
        LSVG.PaintTo(LBitmap.Canvas.Handle,
          TRect.Create(0, 0, LBitmap.Width, LBitmap.Height), True);
        FStream.Clear;
        LPngImage := PNG4TransparentBitMap(LBitmap);
        try
          LPngImage.SaveToStream(FStream);
        finally
          LPngImage.Free;
        end;
      finally
        LBitmap.free;
      end;
    end
    else if SameText(LFileExt,'.webp') or SameText(LFileExt,'.wbmp') then
    begin
      LImage := TWICImage.Create;
      try
        LImage.Transparent := True;
        LImage.LoadFromStream(FStream);
        LScaleFactor := CalcScaleFactor(LImage.Width);
        if RescalingImageEnabled and (LScaleFactor <> 1) then
        begin
          //Rescaling bitmap and save to stream
          LScaledImage := LImage.CreateScaledCopy(
            Round(LImage.Width*LScaleFactor),
            Round(LImage.Height*LScaleFactor),
            wipmHighQualityCubic);
          LBitmap := TBitmap.Create(LScaledImage.Width,LScaledImage.Height);
          MakeTransparent(LBitmap.Canvas.Handle);
          LBitmap.Canvas.Draw(0,0,LScaledImage);
        end
        else
        begin
          LBitmap := TBitmap.Create(LImage.Width,LImage.Height);
          MakeTransparent(LBitmap.Canvas.Handle);
          LBitmap.Canvas.Draw(0,0,LImage);
        end;
        try
          FStream.Clear;
          //if LBitmap.TransparentMode = tmAuto then
          //  LBitmap.SaveToStream(FStream)
          //else
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
      finally
        LImage.Free;
      end;
    end
    else
    begin
      LImage := nil;
      LScaledImage := nil;
      try
        begin
          LImage := TWICImage.Create;
          LImage.LoadFromStream(FStream);
          LScaleFactor := CalcScaleFactor(LImage.Width);
          if RescalingImageEnabled and (LScaleFactor <> 1) then
          begin
            //Rescaling bitmap and save to stream
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
        end;
      finally
        LImage.Free;
        LScaledImage.Free;
      end;
    end;
  except
    Result := False;
    //don't raise any error
  end;
end;

{ TMarkDownFile }

procedure TMarkDownFile.Clear;
begin
  FMarkDownContent := '';
  FHTML := '';
  FParsed := False;
end;

constructor TMarkDownFile.Create(const AMarkDownContent: string;
  const AProcessorDialect: TMarkdownProcessorDialect;
  const AParseImmediately: Boolean = True;
  const ACodeBlockEmitter: TBlockEmitter = nil;
  const AAllowUnsafe: Boolean = False;
  const ACSS: string = '');
begin
  Clear;
  FCodeBlockEmitter := ACodeBlockEmitter;
  FProcessorDialect := AProcessorDialect;
  FAllowUnsafe := AAllowUnsafe;
  FCSS := ACSS;
  MarkDownContent := AMarkDownContent;
  if AParseImmediately then
    Parse;
end;

class function TMarkDownFile.GetDefaultCSS: string;
begin
  Result :=
    '<style type="text/css">'+sLineBreak+
    'body{'+sLineBreak+
    '  font-family: Arial, Helvetica, sans-serif;'+sLineBreak+
    '}'+sLineBreak+
    'img{'+sLineBreak+
    '  max-width: 100%;'+sLineBreak+
    '  height: auto;'+sLineBreak+
    '}'+sLineBreak+
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

procedure TMarkDownFile.Parse;
var
  LMDProcessor: TMarkdownProcessor;
begin
  LMDProcessor := TMarkdownProcessor.CreateDialect(FProcessorDialect);
  try
    //Safe mode by default: native HTML (script/iframe/object...) is neutralized.
    //Set to True only when the user explicitly allows unsafe HTML in Settings.
    LMDProcessor.AllowUnsafe := FAllowUnsafe;
    //Optional syntax-highlighting emitter for fenced code blocks.
    //NB: the caller owns the emitter, so we detach it before freeing the
    //processor (TConfiguration.Destroy frees its codeBlockEmitter).
    if FCodeBlockEmitter <> nil then
      LMDProcessor.Config.codeBlockEmitter := FCodeBlockEmitter;
    //Convert MD To HTML. Use the caller-supplied stylesheet (user setting) when
    //provided, otherwise fall back to the built-in default.
    if FCSS <> '' then
      FHTML := FCSS+LMDProcessor.process(FMarkDownContent)
    else
      FHTML := GetDefaultCSS+LMDProcessor.process(FMarkDownContent);
    {$IFDEF DEBUG}
    //Debug aid: dump the generated HTML so it can be inspected.
    //NB: a single file with a fixed name, overwritten at every parse. It used
    //to call TPath.GetTempFileName, which *creates* a file, and then wrote a
    //second one with the .html extension next to it: two orphan files at every
    //parse, and the preview re-parses at each refresh while editing.
    var LStream := TStringStream.Create(FHTML, TEncoding.UTF8);
    try
      LStream.SaveToFile(TPath.Combine(TPath.GetTempPath,
        'MDShellExtensions_LastParsed.html'));
    finally
      LStream.Free;
    end;
    {$ENDIF}
    FParsed := True;
  finally
    if FCodeBlockEmitter <> nil then
      LMDProcessor.Config.codeBlockEmitter := nil;
    LMDProcessor.Free;
  end;
end;

procedure TMarkDownFile.SetMarkDownContent(const AValue: string);
begin
  if FMarkDownContent <> AValue then
  begin
    FMarkDownContent := AValue;
    FParsed := False;
  end;
end;


initialization
  _dmResources := nil;

finalization
  //Owned by the unit: nobody else frees it any more.
  //NB: this runs when the host process unloads the library - for the shell
  //extension that is Explorer's preview surrogate, where a failure would
  //otherwise be completely invisible. The two log lines make it verifiable:
  //if only the first one appears, the destruction of the data module is where
  //it breaks. Both are no-ops unless ENABLELOG (i.e. DEBUG) is set.
  if Assigned(_dmResources) then
  begin
    TLogPreview.Add('MDShellEx.Resources finalization: freeing shared data module');
    FreeAndNil(_dmResources);
    TLogPreview.Add('MDShellEx.Resources finalization: shared data module freed');
  end
  else
    TLogPreview.Add('MDShellEx.Resources finalization: no shared data module to free');

end.
