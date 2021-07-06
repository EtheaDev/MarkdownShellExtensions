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
  , MarkdownProcessor, Vcl.VirtualImageList, Vcl.ImageCollection
  ;

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
    ImageCollection1: TImageCollection;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FStream: TMemoryStream;
    procedure ConvertImage(AFileName: string;
      const AMaxWidth: Integer; const ABackgroundColor: TColor);
    function getStreamData(const AFileName : String;
      const AMaxWidth: Integer; const ABackgroundColor: TColor): TStream;
    function OpenURL(const AUrl: string): Boolean;
  public
    Settings: TSettings;
    procedure HtmlViewerImageRequest(Sender: TObject; const ASource: UnicodeString;
      var AStream: TStream);
    procedure HtmlViewerHotSpotClick(Sender: TObject; const ASource: ThtString;
      var Handled: Boolean);
    function GetSynHighlighter(const ADarkStyle: boolean;
      const ABackgroundColor: TColor) : TSynCustomHighlighter;
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
  , System.NetEncoding
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
  ;

procedure TdmResources.DataModuleCreate(Sender: TObject);
begin
  FStream := TMemoryStream.Create;
  ImageCollection1.InterpolationMode := icIMModeHighQualityCubic;
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

procedure TdmResources.HtmlViewerImageRequest(Sender: TObject;
  const ASource: UnicodeString; var AStream: TStream);
var
  LFullName: String;
  LHtmlViewer: THtmlViewer;
  LDownLoadFromWeb: boolean;
  LMaxWidth: Integer;
Begin
  LHtmlViewer := sender as THtmlViewer;

  // HTMLViewer needs to be nil'ed
  AStream := nil;

  // is "fullName" a local file, if not aquire file from internet
  If FileExists(ASource) then
    LFullName := ASource
  else If FileExists(LHtmlViewer.ServerRoot+ASource) then
    LFullName := LHtmlViewer.ServerRoot+ASource
  else
    LFullName := ASource;

  LFullName := LHtmlViewer.HTMLExpandFilename(LFullName);

  LMaxWidth := LHtmlViewer.ClientWidth - LHtmlViewer.VScrollBar.Width - (LHtmlViewer.MarginWidth * 2);
  if FileExists(LFullName) then  // if local file, load it..
  Begin
    FStream.LoadFromFile(LFullName);
    //Convert image to stretch size of HTMLViewer
    ConvertImage(LFullName, LMaxWidth, LHtmlViewer.DefBackground);
    AStream := FStream;
  end 
  else if SameText('http', Copy(ASource,1,4)) then
  Begin
    LDownLoadFromWeb := (Settings is TEditorSettings) and
      TEditorSettings(Settings).DownloadFromWEB;
    if LDownLoadFromWeb then
    begin
      //Load from remote
      getStreamData(LFullName, LMaxWidth, LHtmlViewer.DefBackground);
      AStream := FStream;
    end;
  End;
End;

function TdmResources.OpenURL(const AUrl: string): Boolean;
begin
  ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOWNORMAL);
  Result := True;
end;

function TdmResources.getStreamData(const AFileName : String;
  const AMaxWidth: Integer; const ABackgroundColor: TColor): TStream;
var
  sl        : TStringList;
  bFail     : Boolean;
  bTryAgain : Boolean;
  LIdHTTP   : TIdHTTP;
  LIdSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  LFileName: string;
Begin
  Result := nil;
  FStream.Clear;
  LIdHTTP := nil;
  sl := nil;
  LFileName := AFileName;
  LIdSSLIOHandler := nil;
  try
    LIdHTTP := TIdHTTP.Create;
    LIdHTTP.AllowCookies := True;
    sl := TStringList.Create;
    LIdSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(LIdHTTP);
    LIdSSLIOHandler.DefaultPort := 0;
    LIdSSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
    LIdHTTP.IOHandler := LIdSSLIOHandler;

    LIdHTTP.Request.UserAgent :=
      'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0';

    try
      LIdHTTP.Get(LFileName, FStream);
    except
      On E: EIdHTTPProtocolException do
        FStream.Clear
      else
        raise;
    end;
    // Need To check For Failed Retrieval...
    FStream.Position:= 0;
    sl.LoadFromStream(FStream);
    bTryAgain := False;
    bFail     := False;
    if Length(sl.Text) = 0 then
      bFail:= True;
    if FStream.Size = 0 then
      bFail:= True;

    if FStream.Size < 1024 then
    Begin
      if Pos('Not Found', sl.Text) > 0 then bFail:= True;
      if (Pos(LowerCase('<title>301 Moved Permanently</title>'), LowerCase(sl.Text)) > 0) or
         (Pos(LowerCase('<html><body>'), LowerCase(sl.Text)) > 0) then
      Begin
        if Pos(LowerCase('<a href="'), LowerCase(sl.Text)) > 0 then
        Begin
          LFileName := Copy(sl.Text, Pos('<a href="', sl.Text) + 9, Length(sl.Text));
          LFileName := Copy(LFileName, 1, Pos('"', LFileName) -1);
          bTryAgain:= True;
        End;
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
    sl.Free;
  end;
end;

procedure TdmResources.ConvertImage(AFileName: string;
  const AMaxWidth: Integer; const ABackgroundColor: TColor);
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
        //LBitmap.SaveToStream(FStream);
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
    '  color: #A00;'#10+
    '}'#10+
    'pre{'#10+
    '  background: #f4f4f4;'#10+
    '  border: 1px solid #ddd;'#10+
    '  border-left: 3px solid #f36d33;'#10+
    '  color: #555;'#10+
    '  overflow: auto;'#10+
    '  padding: 1em 1.5em;'#10+
    '  display: block;'#10+
    '}'#10+
    'pre code{'#10+
    '  color: inherit;'#10+
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
    '  background: #e0e0e0;'#10+
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
    LMDProcessor.UnSafe := False;
    //Convert MD To HTML
    FHTML := GetDefaultCSS+LMDProcessor.process(FMarkDownContent);
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

initialization

end.
