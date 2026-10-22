{******************************************************************************}
{                                                                              }
{       Viewer Components to show Markdown and HTML content                    }
{       Common part of TMarkdownViewer and TEdgeMarkdownViewer                 }
{                                                                              }
{       Copyright (c) 2023-2026 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/MarkdownHelpViewer                         }
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
unit MarkDownViewerCommon;

{ The part of the Markdown viewers that does not depend on the rendering
  control (HTMLViewer or Edge): the Markdown engine (TMarkdownViewerEngine, a
  TCustomMarkdownToHTML of the Markdown Processor), the resolution of the help
  files, the routing of the clicked links and the file utilities. }

interface

uses
  System.Classes
  , System.SysUtils
  , Vcl.Controls
  , MarkdownUtils
  , MarkdownProcessor
  , MarkdownProcessorComponents
  ;

type
  //A distinct type: the design-time folder editor applies only to it
  TFolderName = type string;

  TFileNameClicked = procedure (const AFileName: TFileName; out AHandled: Boolean) of Object;
  TURLClicked = procedure (const AURL: string; out AHandled: Boolean) of Object;

  //Finds the help file of a keyword or context (TMarkdownViewer.FindHelpFile)
  TFindHelpFileFunc = function (var AFileName: TFileName; const AContext: Integer;
    const AHelpKeyword: string): Boolean of object;
  TLoadFileProc = procedure (const AFileName: TFileName) of object;

  /// <summary>The Markdown engine of the viewers: it converts MarkdownContent
  /// into HtmlContent with the options of the viewer. OnBeforeProcess is fired
  /// before every conversion (e.g. to adapt the code highlighting to the
  /// colors of the viewer).</summary>
  TMarkdownViewerEngine = class(TCustomMarkdownToHTML)
  private
    FOnBeforeProcess: TNotifyEvent;
  protected
    function CreateProcessor: TMarkdownProcessor; override;
  public
    property OnBeforeProcess: TNotifyEvent read FOnBeforeProcess write FOnBeforeProcess;
  end;

  //Access to the protected IsHelpContextLinked of the action link
  THookControlActionLink = class(TControlActionLink)
  public
    function IsHelpContextLinked: Boolean; override;
  end;

var
  AMarkdownFileExt: TArray<String>;
  AHTMLFileExt: TArray<String>;

/// <summary>Loads a text file: BOM, else UTF-8 when valid, else ANSI.</summary>
function TryLoadTextFile(const AFileName: TFileName): string;
procedure SaveUTF8File(const AFileName: TFileName; const AContent: string);
/// <summary>The default stylesheet of the viewers: MarkdownBaseCSS of the
/// Markdown Processor (without the font of the page) in a style tag.</summary>
function GetMarkdownDefaultCSS: string;

/// <summary>The folder used by the viewers without ServerRoot.</summary>
procedure RegisterMDViewerServerRoot(const AFolder: string);
function GetMDViewerServerRoot: string;

function IsHTMLFileName(const AFileName: TFileName): Boolean;
/// <summary>True when AFileName (or AFileName with one of the extensions)
/// exists: AFileName becomes the name found.</summary>
function FileWithExtExists(var AFileName: TFileName;
  const AFileExt: array of string): Boolean;
/// <summary>Help file of a keyword or context, near AFileName: the keyword,
/// the help name + the keyword, the help name + '_' + the keyword.</summary>
function FindMarkdownHelpFile(var AFileName: TFileName;
  const AContext: Integer; const AHelpKeyword: string): Boolean;
/// <summary>The help file to show for HelpType/HelpKeyword/HelpContext in
/// ARootFolder (empty: the registered server root).</summary>
function ResolveHelpFile(const ARootFolder: string; const AHelpType: THelpType;
  const AHelpKeyword: string; const AHelpContext: THelpContext;
  const AFindHelpFile: TFindHelpFileFunc; out AFileName: TFileName): Boolean;
/// <summary>The local file of the URL of an image or of a link, resolved as
/// a browser (WebView2) does: a path relative to ARootFolder, with "/" and
/// percent-encoded ("../images/my%20image.png"), or a file URL
/// ("file:///C:/folder/image.png", "file://server/share/image.png"). False
/// for a web URL and for a URL that is not valid: an absolute Windows path
/// ("C:\folder\image.png", "C:/folder/image.png": for a browser "C:" is a
/// scheme) or a backslash encoded as "%5C" (as CommonMark encodes "\").</summary>
function URLToLocalFileName(const ARootFolder, AURL: string;
  out AFileName: TFileName): Boolean;
/// <summary>Opens a local video file (an extension of IsVideoUrl: .mp4,
/// .webm...) with the player of the system: True when AFileName is a video
/// that exists. The link of a video in a viewer that cannot play it (the
/// &lt;video&gt; of mexVideo in HTMLViewer).</summary>
function OpenVideoFile(const AFileName: string): Boolean;
/// <summary>Routing of a clicked link: OnFileNameClicked and auto-load for the
/// files of ARootFolder, OnURLClicked and the default browser for the URLs.</summary>
function HandleLinkClicked(const ARootFolder, AURL: string;
  const AAutoLoad: Boolean; const AOnFileNameClicked: TFileNameClicked;
  const AOnURLClicked: TURLClicked; const ALoadFile: TLoadFileProc): Boolean;

/// <summary>The items of a dialect combo: the dialects in the order of their
/// ordinal value (ItemIndex = Ord(dialect)), without the "md" prefix; the
/// default dialect is marked with " (Default)".</summary>
procedure FillDialectItems(const AItems: TStrings);
function DialectDisplayName(const ADialect: TMarkdownProcessorDialect): string;
/// <summary>The dialect stored in the settings: the name of the enumerative
/// (e.g. "mdGitHub", also without "md"), or its ordinal value written by the
/// previous versions. The ordinal 1 (mdCommonMark, the old default) becomes
/// mdGitHub, so that the existing installations get the complete support; an
/// empty or unknown value is DefaultMarkdownDialect.</summary>
function DialectFromIniValue(const AValue: string): TMarkdownProcessorDialect;
function DialectToIniValue(const ADialect: TMarkdownProcessorDialect): string;

implementation

uses
  System.TypInfo
  , System.StrUtils
  , Winapi.Windows
  , Winapi.ShLwApi
  , Winapi.ShellAPI
  , MarkdownHtmlRenderer
  ;

var
  //To automate loading of component content
  _ServerRoot: string;

{ TMarkdownViewerEngine }

function TMarkdownViewerEngine.CreateProcessor: TMarkdownProcessor;
begin
  if Assigned(FOnBeforeProcess) then
    FOnBeforeProcess(Self);
  Result := inherited CreateProcessor;
end;

{ THookControlActionLink }

function THookControlActionLink.IsHelpContextLinked: Boolean;
begin
  Result := inherited IsHelpContextLinked;
end;

function TryLoadTextFile(const AFileName: TFileName): string;
begin
  Result := MarkdownProcessorComponents.TryLoadTextFile(AFileName);
end;

procedure SaveUTF8File(const AFileName: TFileName; const AContent: string);
begin
  MarkdownProcessorComponents.SaveUTF8File(AFileName, AContent);
end;

function GetMarkdownDefaultCSS: string;
begin
  //The rules of the Markdown Processor (MarkdownBaseCSS), without the font of
  //the page: the viewers use the font of their settings (DefFontName)
  Result := '<style type="text/css">' + sLineBreak +
    StringReplace(MarkdownBaseCSS, #10, sLineBreak, [rfReplaceAll]) +
    '</style>' + sLineBreak;
end;

procedure RegisterMDViewerServerRoot(const AFolder: string);
begin
  _ServerRoot := IncludeTrailingPathDelimiter(AFolder);
end;

function GetMDViewerServerRoot: string;
begin
  Result := _ServerRoot;
end;

function IsHTMLFileName(const AFileName: TFileName): Boolean;
var
  I: Integer;
  LExt: string;
begin
  //NB: ExtractFileExt returns the extension *with* the dot ('.html')
  LExt := ExtractFileExt(AFileName);
  Result := False;
  for I := Low(AHTMLFileExt) to High(AHTMLFileExt) do
  begin
    if SameText(LExt, AHTMLFileExt[I]) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function FileWithExtExists(var AFileName: TFileName;
  const AFileExt: array of string): Boolean;
var
  I: Integer;
  LExt: string;
  LFileName: TFileName;
begin
  Result := False;
  if Length(AFileExt) = 0 then
    Exit;
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

function FindMarkdownHelpFile(var AFileName: TFileName;
  const AContext: Integer; const AHelpKeyword: string): Boolean;
var
  LHelpFileName: TFileName;
  LName, LPath, LKeyWord: string;
begin
  //WARNING: if changing this function, change also TMarkdownHelpViewer.FindHelpFile
  if AHelpKeyword <> '' then
    LKeyWord := AHelpKeyword
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

function ResolveHelpFile(const ARootFolder: string; const AHelpType: THelpType;
  const AHelpKeyword: string; const AHelpContext: THelpContext;
  const AFindHelpFile: TFindHelpFileFunc; out AFileName: TFileName): Boolean;
var
  LRootFolder: string;
begin
  Result := False;
  AFileName := '';
  LRootFolder := ARootFolder;
  if LRootFolder = '' then
    LRootFolder := _ServerRoot;
  if LRootFolder = '' then
    Exit;
  LRootFolder := IncludeTrailingPathDelimiter(LRootFolder);
  case AHelpType of
    htKeyword:
      if AHelpKeyword <> '' then
      begin
        AFileName := LRootFolder+AHelpKeyword+'.md';
        Result := AFindHelpFile(AFileName, 0, ChangeFileExt(AHelpKeyword,'.md'));
      end;
    htContext:
      if AHelpContext <> 0 then
      begin
        AFileName := LRootFolder+IntToStr(AHelpContext)+'.md';
        Result := AFindHelpFile(AFileName, AHelpContext, '');
      end;
  end;
end;

//The percent-encoded UTF-8 sequences of a URL (not "+": it is not a space in
//a path); a "%" not followed by two hex digits stays as it is
function DecodeURLPath(const AValue: string): string;
var
  LBytes, LChunk: TBytes;
  LCount, I, LStart, LHex: Integer;
begin
  if Pos('%', AValue) = 0 then
    Exit(AValue);
  //each char at most 3 bytes in UTF-8 (a surrogate pair: 2 x 3 >= 4)
  SetLength(LBytes, Length(AValue) * 3);
  LCount := 0;
  I := 1;
  while I <= Length(AValue) do
  begin
    if (AValue[I] = '%') and (I + 2 <= Length(AValue)) and
      CharInSet(AValue[I + 1], ['0'..'9', 'A'..'F', 'a'..'f']) and
      CharInSet(AValue[I + 2], ['0'..'9', 'A'..'F', 'a'..'f']) then
    begin
      LHex := System.SysUtils.StrToInt('$' + Copy(AValue, I + 1, 2));
      LBytes[LCount] := Byte(LHex);
      Inc(LCount);
      Inc(I, 3);
    end
    else
    begin
      //the chars up to the next escape, in UTF-8
      LStart := I;
      Inc(I);
      while (I <= Length(AValue)) and (AValue[I] <> '%') do
        Inc(I);
      LChunk := TEncoding.UTF8.GetBytes(Copy(AValue, LStart, I - LStart));
      if Length(LChunk) > 0 then
        Move(LChunk[0], LBytes[LCount], Length(LChunk));
      Inc(LCount, Length(LChunk));
    end;
  end;
  Result := TEncoding.UTF8.GetString(LBytes, 0, LCount);
end;

function URLToLocalFileName(const ARootFolder, AURL: string;
  out AFileName: TFileName): Boolean;
var
  LURL, LPath: string;
  I: Integer;
begin
  Result := False;
  AFileName := '';
  LURL := Trim(AURL);
  //the fragment and the query are not part of the file
  I := Pos('#', LURL);
  if I > 0 then
    LURL := Copy(LURL, 1, I - 1);
  I := Pos('?', LURL);
  if I > 0 then
    LURL := Copy(LURL, 1, I - 1);
  if (LURL = '') or ContainsText(LURL, '%5C') then
    Exit;
  if StartsText('file:', LURL) then
  begin
    LPath := Copy(LURL, 6, MaxInt);
    if StartsStr('///', LPath) then
      //file:///C:/folder/file
      LPath := Copy(LPath, 4, MaxInt)
    else if StartsStr('//', LPath) then
      //file://server/share/file
      LPath := '\\' + Copy(LPath, 3, MaxInt)
    else
      Exit;
    LPath := StringReplace(DecodeURLPath(LPath), '/', '\', [rfReplaceAll]);
    if not (StartsStr('\\', LPath) or ((Length(LPath) >= 3) and
      CharInSet(LPath[1], ['A'..'Z', 'a'..'z']) and (LPath[2] = ':') and
      (LPath[3] = '\'))) then
      Exit;
  end
  else
  begin
    //A scheme (http:, mailto:...): not a local file. Also a drive ("C:\",
    //"C:/"), that a browser does not resolve
    I := 1;
    if CharInSet(LURL[1], ['A'..'Z', 'a'..'z']) then
      while (I < Length(LURL)) and CharInSet(LURL[I + 1],
        ['A'..'Z', 'a'..'z', '0'..'9', '+', '-', '.']) do
        Inc(I);
    if (I < Length(LURL)) and (LURL[I + 1] = ':') and
      CharInSet(LURL[1], ['A'..'Z', 'a'..'z']) then
      Exit;
    //relative to the folder of the document ("/" as a browser on the root);
    //a browser reads "\" in a relative path as "/"
    LPath := StringReplace(DecodeURLPath(LURL), '/', '\', [rfReplaceAll]);
    while StartsStr('\', LPath) do
      Delete(LPath, 1, 1);
    if ARootFolder <> '' then
      LPath := IncludeTrailingPathDelimiter(ARootFolder) + LPath;
  end;
  AFileName := ExpandFileName(LPath);
  Result := True;
end;

function OpenVideoFile(const AFileName: string): Boolean;
begin
  Result := IsVideoUrl(AFileName) and FileExists(AFileName);
  if Result then
    ShellExecute(0, 'open', PChar(AFileName), nil, nil, SW_SHOWNORMAL);
end;

function HandleLinkClicked(const ARootFolder, AURL: string;
  const AAutoLoad: Boolean; const AOnFileNameClicked: TFileNameClicked;
  const AOnURLClicked: TURLClicked; const ALoadFile: TLoadFileProc): Boolean;
var
  LFileName: TFileName;
  LRootFolder: string;
  LLocalFile: Boolean;
begin
  Result := False;
  //Prepare FileName
  LRootFolder := ARootFolder;
  if LRootFolder = '' then
    LRootFolder := _ServerRoot;
  LLocalFile := URLToLocalFileName(LRootFolder, AURL, LFileName);
  if not LLocalFile then
    LFileName := IncludeTrailingPathDelimiter(LRootFolder)+AURL;
  //User Event
  if Assigned(AOnFileNameClicked) then
    AOnFileNameClicked(LFileName, Result);
  //Handled by the event: it is not also an URL to open (a file:/// link, as
  //the links of TEdgeMarkdownViewer, would reach OnURLClicked too)
  if Result then
    Exit;
  //Auto load file
  if not Result and FileExists(LFileName) and AAutoLoad then
  begin
    //a video with its player, a document in the viewer
    if not OpenVideoFile(LFileName) then
      ALoadFile(LFileName);
    Result := True;
  end
  else if not LLocalFile and PathIsURL(PChar(AURL)) then
  begin
    //a web URL (a local file, also missing, is never opened as an URL)
    if Assigned(AOnURLClicked) then
      AOnURLClicked(AURL, Result);
    if not Result and AAutoLoad then
    begin
      //Try to Open an URL
      ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOWNORMAL);
      Result := True;
    end;
  end;
end;

function DialectDisplayName(const ADialect: TMarkdownProcessorDialect): string;
begin
  Result := GetEnumName(TypeInfo(TMarkdownProcessorDialect), Ord(ADialect));
  if StartsText('md', Result) then
    Result := Copy(Result, 3, MaxInt);
  if ADialect = DefaultMarkdownDialect then
    Result := Result + ' (Default)';
end;

procedure FillDialectItems(const AItems: TStrings);
var
  LDialect: TMarkdownProcessorDialect;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    for LDialect := Low(TMarkdownProcessorDialect) to High(TMarkdownProcessorDialect) do
      AItems.Add(DialectDisplayName(LDialect));
  finally
    AItems.EndUpdate;
  end;
end;

function DialectFromIniValue(const AValue: string): TMarkdownProcessorDialect;
var
  LValue: string;
  LOrdinal: Integer;
begin
  Result := DefaultMarkdownDialect;
  LValue := Trim(AValue);
  if LValue = '' then
    Exit;
  if TryStrToInt(LValue, LOrdinal) then
  begin
    //Ordinal value written by the previous versions
    if LOrdinal = Ord(mdCommonMark) then
      //CommonMark was the default: the complete support of the GitHub dialect
      Result := mdGitHub
    else if (LOrdinal >= Ord(Low(TMarkdownProcessorDialect))) and
      (LOrdinal <= Ord(High(TMarkdownProcessorDialect))) then
      Result := TMarkdownProcessorDialect(LOrdinal);
  end
  else
  begin
    //Name of the enumerative, with or without the "md" prefix
    LOrdinal := GetEnumValue(TypeInfo(TMarkdownProcessorDialect), LValue);
    if LOrdinal < 0 then
      LOrdinal := GetEnumValue(TypeInfo(TMarkdownProcessorDialect), 'md' + LValue);
    if LOrdinal >= 0 then
      Result := TMarkdownProcessorDialect(LOrdinal);
  end;
end;

function DialectToIniValue(const ADialect: TMarkdownProcessorDialect): string;
begin
  Result := GetEnumName(TypeInfo(TMarkdownProcessorDialect), Ord(ADialect));
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
