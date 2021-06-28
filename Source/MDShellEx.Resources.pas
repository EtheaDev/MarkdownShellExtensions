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
  , MarkdownProcessor
  ;

resourcestring
  FILE_SAVED = 'File "%s" salvato correttamente';

type
  TMarkDownFile = record
  private
    FParsed: Boolean;
    FMarkDownContent: string;
    FHTML: string;
    procedure SetMarkDownContent(const AValue: string);
    function GetDefaultCSS: string;
  public
    constructor Create(const AMarkDownContent: string;
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
    MStream: TMemoryStream;
    procedure ConvertImage(FileName: string);
    function getStreamData(FileName: String): TStream;
    function OpenURL(const AUrl: string): Boolean;
  public
    Settings: TSettings;
    procedure HtmlViewerImageRequest(Sender: TObject; const SRC: UnicodeString;
      var Stream: TStream);
    procedure HtmlViewerHotSpotClick(Sender: TObject; const SRC: ThtString;
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
  ;

procedure TdmResources.DataModuleCreate(Sender: TObject);
begin
  MStream := TMemoryStream.Create;
end;

procedure TdmResources.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(MStream);
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
  const SRC: ThtString; var Handled: Boolean);
begin
  Handled := OpenUrl(SRC);
end;

procedure TdmResources.HtmlViewerImageRequest(Sender: TObject;
  const SRC: UnicodeString; var Stream: TStream);
var
  fullName  : String;
  HtmlViewer: THtmlViewer;
Begin
  HtmlViewer := sender as THtmlViewer;

  // HTMLViewer needs to be nil'ed
  Stream := nil;

  // is "fullName" a local file, if not aquire file from internet
  If FileExists(SRC) then
    fullName := SRC
  else If FileExists(HtmlViewer.ServerRoot+SRC) then
    fullName := HtmlViewer.ServerRoot+SRC
  else
    fullName := SRC;

  fullName := HtmlViewer.HTMLExpandFilename(fullName);

  if FileExists(fullName) then  // if local file, load it..
  Begin
    MStream.LoadFromFile(fullName);
    ConvertImage(fullName);
    Stream := MStream;
  end else if Settings.DownloadFromWEB then  // if not local file, download it..
  Begin
    getStreamData(fullName);
    Stream:=MStream;
  End;
End;

function TdmResources.OpenURL(const AUrl: string): Boolean;
begin
  ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOWNORMAL);
  Result := True;
end;

function TdmResources.getStreamData(FileName : String): TStream;
var
  sl        : TStringList;
  bFail     : Boolean;
  bTryAgain : Boolean;
  LIdHTTP   : TIdHTTP;
  LIdSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
Begin
  Result := nil;
  MStream.Clear;
  LIdHTTP := nil;
  sl := nil;
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
      LIdHTTP.Get(FileName, MStream);
    except
      On E: EIdHTTPProtocolException do
        MStream.Clear
      else
        raise;
    end;
    // Need To check For Failed Retrieval...
    MStream.Position:= 0;
    sl.LoadFromStream(MStream);
    bTryAgain := False;
    bFail     := False;
    if Length(sl.Text) = 0 then
      bFail:= True;
    if MStream.Size = 0 then
      bFail:= True;

    if MStream.Size < 1024 then
    Begin
      if Pos('Not Found', sl.Text) > 0 then bFail:= True;
      if (Pos(LowerCase('<title>301 Moved Permanently</title>'), LowerCase(sl.Text)) > 0) or
         (Pos(LowerCase('<html><body>'), LowerCase(sl.Text)) > 0) then
      Begin
        if Pos(LowerCase('<a href="'), LowerCase(sl.Text)) > 0 then
        Begin
          FileName := Copy(sl.Text, Pos('<a href="', sl.Text) + 9, Length(sl.Text));
          FileName := Copy(FileName, 1, Pos('"', FileName) -1);
          bTryAgain:= True;
        End;
      end;
    end;

    if bTryAgain then
      // Call Function Again...
      Result := getStreamData(FileName);

    if not bTryAgain And not bFail then
    begin
      ConvertImage(FileName);
      Result := MStream;
    end;
  finally
    LIdSSLIOHandler.Free;
    LIdHttp.Free;
    sl.Free;
  end;
end;

procedure TdmResources.ConvertImage(FileName:string);
var
  SVG: TSVGGraphic;
  png: TPngImage;
  JPeg: TJPEGImage;
  LBitmap: TBitmap;
  LFileExt: string;
begin
  LBitmap := nil;
  LFileExt := ExtractFileExt(FileName);
  if SameText(LFileExt,'.svg') then
  begin
    svg := TSVGGraphic.Create;
    try
      MStream.Position:=0;
      svg.LoadFromStream(MStream);
      try
        LBitmap := TBitmap.Create(svg.Width,svg.Height);
        LBitmap.PixelFormat := pf32bit;
        {$IFDEF IgnoreAntiAliasedColor}
        MakeTransparent(LBitmap.Canvas.Handle);
        {$ENDIF}
        svg.draw(LBitmap.Canvas, TRect.Create(0, 0, svg.Width,svg.Height));
        MStream.Clear;
        LBitmap.SaveToStream(MStream);
      finally
        LBitmap.free;
      end;
    finally
      svg.free;
    end;
  end
  else if SameText(LFileExt,'.png') then
  begin
    png := TPngImage.Create;
    try
      MStream.Position:=0;
      png.LoadFromStream(MStream);
      try
        LBitmap := TBitmap.Create(png.Width,png.Height);
        LBitmap.PixelFormat := pf32bit;
        {$IFDEF IgnoreAntiAliasedColor}
        MakeTransparent(LBitmap.Canvas.Handle);
        {$ENDIF}
        png.draw(LBitmap.Canvas, TRect.Create(0, 0, png.Width,png.Height));
        MStream.Clear;
        LBitmap.SaveToStream(MStream);
      finally
        LBitmap.free;
      end;
    finally
      png.Free;
    end;
  end
  else if SameText(LFileExt,'.jpg') or SameText(LFileExt,'.jpeg') then
  begin
    JPeg := TJPEGImage.Create;
    try
      MStream.Position:=0;
      JPeg.LoadFromStream(MStream);
      try
        LBitmap := TBitmap.Create(JPeg.Width,JPeg.Height);
        LBitmap.PixelFormat := pf32bit;
        {$IFDEF IgnoreAntiAliasedColor}
        MakeTransparent(LBitmap.Canvas.Handle);
        {$ENDIF}
        LBitmap.Canvas.Draw(0,0, JPeg);
        MStream.Clear;
        LBitmap.SaveToStream(MStream);
      finally
        LBitmap.free;
      end;
    finally
      JPeg.Free;
    end;
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
  const AParseImmediately: Boolean = True);
begin
  Clear;

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
  LMDProcessor := TMarkdownProcessor.CreateDialect(mdCommonMark);
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
