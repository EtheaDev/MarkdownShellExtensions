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
  , SynHighlighterXML
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
    procedure SetMarkDownContent(const AValue: string);
    function GetDefaultCSS: string;
  public
    constructor Create(const AMarkDownContent: string;
      const AProcessorDialect: TMarkdownProcessorDialect;
      const AParseImmediately: Boolean = True);

    procedure Clear;
    procedure Parse;

    property Parsed: Boolean read FParsed;
    property MarkDownContent: string read FMarkDownContent write SetMarkDownContent;
    property HTML: string read FHTML;
  end;

  TdmResources = class(TDataModule)
    SynXMLSyn: TSynXMLSyn;
    SynXMLSynDark: TSynXMLSyn;
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
  public
    Settings: TSettings;
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

var
  dmResources: TdmResources;

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
  ;

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
begin
  if ADarkStyle then
  begin
    Result := dmResources.SynXMLSynDark;
    SynXMLSynDark.ElementAttri.Background := ABackgroundColor;
    SynXMLSynDark.AttributeAttri.Background := ABackgroundColor;
    SynXMLSynDark.NamespaceAttributeAttri.Background := ABackgroundColor;
    SynXMLSynDark.AttributeValueAttri.Background := ABackgroundColor;
    SynXMLSynDark.NamespaceAttributeValueAttri.Background := ABackgroundColor;
    SynXMLSynDark.TextAttri.Background := ABackgroundColor;
    SynXMLSynDark.CDATAAttri.Background := ABackgroundColor;
    SynXMLSynDark.EntityRefAttri.Background := ABackgroundColor;
    SynXMLSynDark.ProcessingInstructionAttri.Background := ABackgroundColor;
    SynXMLSynDark.CommentAttri.Background := ABackgroundColor;
    SynXMLSynDark.DocTypeAttri.Background := ABackgroundColor;
    SynXMLSynDark.SpaceAttri.Background := ABackgroundColor;
    SynXMLSynDark.SymbolAttri.Background := ABackgroundColor;
  end
  else
  begin
    Result := dmResources.SynXMLSyn;
    SynXMLSyn.ElementAttri.Background := ABackgroundColor;
    SynXMLSyn.AttributeAttri.Background := ABackgroundColor;
    SynXMLSyn.NamespaceAttributeAttri.Background := ABackgroundColor;
    SynXMLSyn.AttributeValueAttri.Background := ABackgroundColor;
    SynXMLSyn.NamespaceAttributeValueAttri.Background := ABackgroundColor;
    SynXMLSyn.TextAttri.Background := ABackgroundColor;
    SynXMLSyn.CDATAAttri.Background := ABackgroundColor;
    SynXMLSyn.EntityRefAttri.Background := ABackgroundColor;
    SynXMLSyn.ProcessingInstructionAttri.Background := ABackgroundColor;
    SynXMLSyn.CommentAttri.Background := ABackgroundColor;
    SynXMLSyn.DocTypeAttri.Background := ABackgroundColor;
    SynXMLSyn.SpaceAttri.Background := ABackgroundColor;
    SynXMLSyn.SymbolAttri.Background := ABackgroundColor;
  end;
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
        //Load from remote
        getStreamData(AFileName, AMaxWidth, ABackGroundColor);
        AStream := FStream;
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

procedure TdmResources.StopLoadingImages(const AStop: Boolean);
begin
  FStopImageRequest := AStop;
end;

function TdmResources.getStreamData(const AFileName : String;
  const AMaxWidth: Integer; const ABackgroundColor: TColor): TStream;
var
  LFileStream: TStringStream;
  bFail     : Boolean;
  bTryAgain : Boolean;
  LIdHTTP   : TIdHTTP;
  LIdSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  LFileName: TFileName;
  LFileContent: string;
Begin
  //downloading Image from WEB
  Result := nil;
  FStream.Clear;
  LIdHTTP := nil;
  LFileStream := nil;
  LFileName := AFileName;
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

    try
      LIdHTTP.Get(LFileName, FStream);
    except
      On Exception do
        FStream.Clear
      else
        raise;
    end;

    if FStream.Size = 0 then
    begin
      bFail:= True;
      bTryAgain := False;
    end
    else
    begin
      // Need To check For Failed Retrieval...
      FStream.Position:= 0;
      if SameText(ExtractFileExt(LFileName),'svg') then
        LFileStream := TStringStream.Create('', TEncoding.UTF8)
      else
        LFileStream := TStringStream.Create('', TEncoding.Ansi);
      LFileStream.LoadFromStream(FStream);
      LFileContent := LFileStream.DataString;
      // Save string to local File
      LFileName := ChangeFileExt(TPath.GetTempFileName,'.svg');
      LFileStream.SaveToFile(LFileName);
      bTryAgain := False;
      bFail     := False;

      if FStream.Size < 1024 then
      Begin
        if Pos('Not Found', LFileContent) > 0 then bFail:= True;
        if (Pos(LowerCase('<title>301 Moved Permanently</title>'), LowerCase(LFileContent)) > 0) or
           (Pos(LowerCase('<html><body>'), LowerCase(LFileContent)) > 0) then
        Begin
          if Pos(LowerCase('<a href="'), LowerCase(LFileContent)) > 0 then
          Begin
            LFileName := Copy(LFileContent, Pos('<a href="', LFileContent) + 9, Length(LFileContent));
            LFileName := Copy(LFileName, 1, Pos('"', LFileName) -1);
            bTryAgain:= True;
          End;
        end;
      end;
    end;

    if bTryAgain then
      // Call Function Again...
      Result := getStreamData(LFileName, AMaxWidth, ABackgroundColor);

    if not bTryAgain And not bFail then
    begin
      ConvertImage(LFileName, AMaxWidth, ABackgroundColor);
      Result := FStream;
    end;
  finally
    LIdSSLIOHandler.Free;
    LIdHttp.Free;
    LFileStream.Free;
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
      if (Settings.RescalingImage) and (LScaleFactor <> 1) then
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
        if (Settings.RescalingImage) and (LScaleFactor <> 1) then
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
          if (Settings.RescalingImage) and (LScaleFactor <> 1) then
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
  const AParseImmediately: Boolean = True);
begin
  Clear;
  FProcessorDialect := AProcessorDialect;
  MarkDownContent := AMarkDownContent;
  if AParseImmediately then
    Parse;
end;

function TMarkDownFile.GetDefaultCSS: string;
begin
  Result :=
    '<style type="text/css">'#10+
    'code{'#10+
    '  font-family: "Consolas", monospace;'#10+
    '}'#10+
    'pre{'#10+
    '  border: 1px solid #ddd;'#10+
    '  border-left: 3px solid #f36d33;'#10+
    '  overflow: auto;'#10+
    '  padding: 1em 1.5em;'#10+
    '  display: block;'#10+
    '}'#10+
    'Blockquote{'#10+
    '  border-left: 3px solid #d0d0d0;'#10+
    '  padding-left: 0.5em;'#10+
    '  margin-left:1em;'#10+
    '}'#10+
    'Blockquote p{'#10+
    '  margin: 0;'#10+
    '}'#10+
    'table{'#10+
    '  border:1px solid;'#10+
    '  border-collapse:collapse;'#10+
    '}'#10+
    'th{'+
    '  padding:5px;'#10+
    '  border:1px solid;'#10+
    '}'#10+
    'td{'#10+
    '  padding:5px;'#10+
    '  border:1px solid;'#10+
    '}'#10+
    '</style>'#10;
end;

procedure TMarkDownFile.Parse;
var
  LMDProcessor: TMarkdownProcessor;
begin
  LMDProcessor := TMarkdownProcessor.CreateDialect(FProcessorDialect);
  try
    LMDProcessor.AllowUnsafe := False;
    //Convert MD To HTML
    FHTML := GetDefaultCSS+LMDProcessor.process(FMarkDownContent);
    {$IFDEF DEBUG}
    with TStringStream.Create(FHTML, TEncoding.UTF8) do
    begin
      var LFileName := ChangeFileExt(TPath.GetTempFileName,'.html');
      SaveToFile(LFileName); //to read HTML file produced
      Free;
    end;
    {$ENDIF}
    FParsed := True;
  finally
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


end.
