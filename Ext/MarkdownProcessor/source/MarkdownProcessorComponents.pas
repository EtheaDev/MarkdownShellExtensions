{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       TMarkdownToHTML: non-visual component to convert Markdown to HTML      }
{                                                                              }
{       Copyright (c) 2026 (Ethea S.r.l.)                                      }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/MarkdownProcessor                          }
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
unit MarkdownProcessorComponents;

{ TMarkdownToHTML holds the options of the conversion (dialect, extensions,
  safe mode, math rendering, stylesheet) and converts MarkdownContent into
  HtmlContent every time the content or an option changes.
  By default the component enables, besides the extensions of the dialect, all
  the legacy (Ethea) extensions (LegacyExtensions): they only act on their own
  syntax (~x~, ^x^, ++x++, ==x==, heading ids, [[...]]...).
  The properties have the same names as the ones of TMarkdownViewer
  (MarkdownHelpViewer project).
  Compatible with Delphi XE3: no inline variables. }

interface

uses
  System.Classes,
  System.SysUtils,
  MarkdownUtils,
  MarkdownProcessor;

const
  /// <summary>The legacy (Ethea) extensions, enabled by default in the component
  /// for mdCommonMark, mdGFM and mdGitHub.</summary>
  LegacyExtensions: TMarkdownExtensions = [mexSubscript, mexSuperscript, mexInsert,
    mexMark, mexSmartTypography, mexHeadingAttributes, mexAutoHeadingIds, mexWikiLinks];

type
  TCustomMarkdownToHTML = class(TComponent)
  private
    FFileName: TFileName;
    FMarkdownContent: TStringList;
    FHTMLContent: TStringList;
    FCssStyle: TStringList;
    FProcessorDialect: TMarkdownProcessorDialect;
    FAllowUnsafe: Boolean;
    FExtensions: TMarkdownExtensions;
    FMathRendering: TMarkdownMathRendering;
    FCodeBlockEmitter: TBlockEmitter;
    FSpecialLinkEmitter: TSpanEmitter;
    FOnChange: TNotifyEvent;
    FUpdateCount: Integer;
    procedure SetFileName(const AValue: TFileName);
    procedure SetProcessorDialect(const AValue: TMarkdownProcessorDialect);
    procedure SetAllowUnsafe(const AValue: Boolean);
    procedure SetExtensions(const AValue: TMarkdownExtensions);
    procedure SetMathRendering(const AValue: TMarkdownMathRendering);
    procedure SetCssStyle(const AValue: TStringList);
    procedure SetHTMLContent(const AValue: TStringList);
    procedure SetMarkdownContent(const AValue: TStringList);
    function IsCssStyleStored: Boolean;
    function IsHtmlContentStored: Boolean;
    function IsExtensionsStored: Boolean;
    procedure MDContentChanged(Sender: TObject);
    procedure OptionChanged(Sender: TObject);
  protected
    procedure Loaded; override;
    procedure DoChange; virtual;
    /// <summary>Creates the processor with the options of the component.
    /// Override to configure it further.</summary>
    function CreateProcessor: TMarkdownProcessor; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>The default extensions of the component for a dialect: the ones
    /// of the dialect plus LegacyExtensions (new engine dialects only).</summary>
    class function DefaultExtensions(const ADialect: TMarkdownProcessorDialect): TMarkdownExtensions;
    /// <summary>Converts MarkdownContent again into HtmlContent (done
    /// automatically when the content or an option changes).</summary>
    procedure UpdateHTML;
    /// <summary>Suspends the conversions while several options or contents are
    /// assigned; the last EndUpdate converts once.</summary>
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure LoadFromFile(const AFileName: TFileName);
    procedure LoadFromStream(const AStream: TStringStream;
      const IsHTMLContent: Boolean = False);
    procedure LoadFromString(const AValue: string;
      const IsHTMLContent: Boolean = False);
    procedure ExportToFileHTML(const AFileName: TFileName);
    /// <summary>Converts a Markdown text with the options of the component:
    /// CssStyle followed by the HTML fragment.</summary>
    function TransformContent(const AMarkdownContent: string): string; overload;
    /// <summary>Converts a Markdown text with the given dialect (and its default
    /// extensions), stylesheet and safe mode, as TMarkdownViewer.TransformContent.</summary>
    function TransformContent(const AMarkdownContent: string;
      AProcessorDialect: TMarkdownProcessorDialect;
      const ACssStyle: string = '';
      const AAllowUnsafe: Boolean = False): string; overload;

    /// <summary>Stylesheet written before the HTML fragment (default:
    /// MarkdownDefaultCSS); empty for the bare fragment.</summary>
    property CssStyle: TStringList read FCssStyle write SetCssStyle stored IsCssStyleStored;
    /// <summary>Assigning an existing file loads it.</summary>
    property FileName: TFileName read FFileName write SetFileName;
    /// <summary>Changing the dialect resets Extensions to its defaults.</summary>
    property ProcessorDialect: TMarkdownProcessorDialect read FProcessorDialect write SetProcessorDialect default DefaultMarkdownDialect;
    /// <summary>When True raw HTML (script, iframe...) and every link are kept:
    /// use only with trusted content. Default False (safe mode).</summary>
    property AllowUnsafe: Boolean read FAllowUnsafe write SetAllowUnsafe default False;
    /// <summary>Optional syntax of mdCommonMark, mdGFM and mdGitHub (ignored by
    /// the legacy dialects). Default: DefaultExtensions(ProcessorDialect).</summary>
    property Extensions: TMarkdownExtensions read FExtensions write SetExtensions stored IsExtensionsStored;
    /// <summary>mmrMarkup for KaTeX/MathJax (viewers with JavaScript),
    /// mmrCodeCogsImage for viewers without JavaScript.</summary>
    property MathRendering: TMarkdownMathRendering read FMathRendering write SetMathRendering default mmrMarkup;
    /// <summary>The result of the conversion (it can also be assigned directly).</summary>
    property HtmlContent: TStringList read FHTMLContent write SetHTMLContent stored IsHtmlContentStored;
    property MarkdownContent: TStringList read FMarkdownContent write SetMarkdownContent;
    /// <summary>Receives every code block (not owned by the component).</summary>
    property CodeBlockEmitter: TBlockEmitter read FCodeBlockEmitter write FCodeBlockEmitter;
    /// <summary>Receives the [[...]] links with mexWikiLinks (not owned).</summary>
    property SpecialLinkEmitter: TSpanEmitter read FSpecialLinkEmitter write FSpecialLinkEmitter;
    /// <summary>Fired after HtmlContent changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TMarkdownToHTML = class(TCustomMarkdownToHTML)
  published
    property ProcessorDialect;
    property Extensions;
    property AllowUnsafe;
    property MathRendering;
    property CssStyle;
    property FileName;
    property MarkdownContent;
    property HtmlContent;
    property OnChange;
  end;

/// <summary>Loads a text file: the BOM, when present, decides the encoding,
/// otherwise UTF-8 if the bytes are valid UTF-8, else ANSI.</summary>
function TryLoadTextFile(const AFileName: TFileName): string;
procedure SaveUTF8File(const AFileName: TFileName; const AContent: string);
/// <summary>The default stylesheet (MarkdownDefaultCSS).</summary>
function GetMarkdownDefaultCSS: string;

implementation

const
  AHTMLFileExt: array[0..1] of string = ('.htm', '.html');

function GetMarkdownDefaultCSS: string;
begin
  Result := MarkdownDefaultCSS;
end;

// True when the buffer is a valid UTF-8 sequence: it is how UTF-8 is told from
// ANSI in a file without BOM
function IsValidUTF8(const ABytes: TBytes): Boolean;
var
  I, LLen, LTrailing: Integer;
  B: Byte;
begin
  LLen := Length(ABytes);
  I := 0;
  while I < LLen do
  begin
    B := ABytes[I];
    // initialized here: older compilers do not see the Exit of the last branch
    LTrailing := 0;
    if B >= $80 then
    begin
      if (B and $E0) = $C0 then
        LTrailing := 1
      else if (B and $F0) = $E0 then
        LTrailing := 2
      else if (B and $F8) = $F0 then
        LTrailing := 3
      else
        Exit(False);
    end;
    if I + LTrailing >= LLen then
      Exit(False);
    while LTrailing > 0 do
    begin
      Inc(I);
      if (ABytes[I] and $C0) <> $80 then
        Exit(False);
      Dec(LTrailing);
    end;
    Inc(I);
  end;
  Result := True;
end;

function TryLoadTextFile(const AFileName: TFileName): string;
var
  LStream: TFileStream;
  LBytes: TBytes;
  LEncoding: TEncoding;
  LPreambleLen: Integer;
begin
  Result := '';
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LBytes, LStream.Size);
    if Length(LBytes) > 0 then
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    FreeAndNil(LStream);
  end;
  if Length(LBytes) = 0 then
    Exit;
  LEncoding := nil;
  LPreambleLen := TEncoding.GetBufferEncoding(LBytes, LEncoding);
  if LPreambleLen = 0 then
  begin
    if IsValidUTF8(LBytes) then
      LEncoding := TEncoding.UTF8
    else
      LEncoding := TEncoding.ANSI;
  end;
  Result := LEncoding.GetString(LBytes, LPreambleLen, Length(LBytes) - LPreambleLen);
end;

procedure SaveUTF8File(const AFileName: TFileName; const AContent: string);
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

{ TCustomMarkdownToHTML }

constructor TCustomMarkdownToHTML.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProcessorDialect := DefaultMarkdownDialect;
  FExtensions := DefaultExtensions(FProcessorDialect);
  FMathRendering := mmrMarkup;
  FMarkdownContent := TStringList.Create;
  FHTMLContent := TStringList.Create;
  FCssStyle := TStringList.Create;
  FCssStyle.Text := GetMarkdownDefaultCSS;
  FMarkdownContent.OnChange := MDContentChanged;
  FCssStyle.OnChange := OptionChanged;
end;

destructor TCustomMarkdownToHTML.Destroy;
begin
  FreeAndNil(FMarkdownContent);
  FreeAndNil(FHTMLContent);
  FreeAndNil(FCssStyle);
  inherited;
end;

class function TCustomMarkdownToHTML.DefaultExtensions(
  const ADialect: TMarkdownProcessorDialect): TMarkdownExtensions;
var
  LProcessor: TMarkdownProcessor;
begin
  LProcessor := TMarkdownProcessor.CreateDialect(ADialect);
  try
    Result := LProcessor.Config.Extensions;
  finally
    LProcessor.Free;
  end;
  // the legacy dialects (DaringFireball, TxtMark) ignore the extensions
  if ADialect in [mdCommonMark, mdGFM, mdGitHub] then
    Result := Result + LegacyExtensions;
end;

procedure TCustomMarkdownToHTML.Loaded;
begin
  inherited;
  // the HTML is not stored when there is Markdown: it is rebuilt here
  UpdateHTML;
end;

procedure TCustomMarkdownToHTML.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TCustomMarkdownToHTML.CreateProcessor: TMarkdownProcessor;
begin
  Result := TMarkdownProcessor.CreateDialect(FProcessorDialect);
  Result.AllowUnsafe := FAllowUnsafe;
  Result.Config.Extensions := FExtensions;
  Result.Config.MathRendering := FMathRendering;
end;

function TCustomMarkdownToHTML.TransformContent(const AMarkdownContent: string): string;
var
  LProcessor: TMarkdownProcessor;
begin
  LProcessor := CreateProcessor;
  try
    // the configuration frees its emitters: they are detached before freeing it
    LProcessor.Config.codeBlockEmitter := FCodeBlockEmitter;
    LProcessor.Config.specialLinkEmitter := FSpecialLinkEmitter;
    try
      Result := FCssStyle.Text + LProcessor.Process(AMarkdownContent);
    finally
      LProcessor.Config.codeBlockEmitter := nil;
      LProcessor.Config.specialLinkEmitter := nil;
    end;
  finally
    LProcessor.Free;
  end;
end;

function TCustomMarkdownToHTML.TransformContent(const AMarkdownContent: string;
  AProcessorDialect: TMarkdownProcessorDialect; const ACssStyle: string;
  const AAllowUnsafe: Boolean): string;
var
  LProcessor: TMarkdownProcessor;
begin
  LProcessor := TMarkdownProcessor.CreateDialect(AProcessorDialect);
  try
    LProcessor.AllowUnsafe := AAllowUnsafe;
    LProcessor.Config.MathRendering := FMathRendering;
    LProcessor.Config.codeBlockEmitter := FCodeBlockEmitter;
    LProcessor.Config.specialLinkEmitter := FSpecialLinkEmitter;
    try
      Result := ACssStyle + LProcessor.Process(AMarkdownContent);
    finally
      LProcessor.Config.codeBlockEmitter := nil;
      LProcessor.Config.specialLinkEmitter := nil;
    end;
  finally
    LProcessor.Free;
  end;
end;

procedure TCustomMarkdownToHTML.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomMarkdownToHTML.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      UpdateHTML;
  end;
end;

procedure TCustomMarkdownToHTML.UpdateHTML;
begin
  if (csLoading in ComponentState) or (FUpdateCount > 0) then
    Exit;
  // HTML assigned directly (no Markdown) is kept as it is
  if FMarkdownContent.Text = '' then
    Exit;
  FHTMLContent.Text := TransformContent(FMarkdownContent.Text);
  DoChange;
end;

procedure TCustomMarkdownToHTML.MDContentChanged(Sender: TObject);
begin
  if FMarkdownContent.Text = '' then
  begin
    FHTMLContent.Clear;
    DoChange;
  end
  else
    UpdateHTML;
end;

procedure TCustomMarkdownToHTML.OptionChanged(Sender: TObject);
begin
  UpdateHTML;
end;

procedure TCustomMarkdownToHTML.LoadFromFile(const AFileName: TFileName);
var
  I: Integer;
  LExt: string;
  LIsHTMLContent: Boolean;
begin
  LExt := ExtractFileExt(AFileName);
  LIsHTMLContent := False;
  for I := Low(AHTMLFileExt) to High(AHTMLFileExt) do
    if SameText(LExt, AHTMLFileExt[I]) then
    begin
      LIsHTMLContent := True;
      Break;
    end;
  LoadFromString(TryLoadTextFile(AFileName), LIsHTMLContent);
end;

procedure TCustomMarkdownToHTML.LoadFromStream(const AStream: TStringStream;
  const IsHTMLContent: Boolean);
begin
  LoadFromString(AStream.DataString, IsHTMLContent);
end;

procedure TCustomMarkdownToHTML.LoadFromString(const AValue: string;
  const IsHTMLContent: Boolean);
var
  LOldMDChange: TNotifyEvent;
begin
  // a single conversion: OnChange of the Markdown is suspended while assigning
  LOldMDChange := FMarkdownContent.OnChange;
  FMarkdownContent.OnChange := nil;
  try
    if IsHTMLContent then
    begin
      FMarkdownContent.Text := '';
      FHTMLContent.Text := AValue;
    end
    else
      FMarkdownContent.Text := AValue;
  finally
    FMarkdownContent.OnChange := LOldMDChange;
  end;
  if IsHTMLContent then
    DoChange
  else
    MDContentChanged(Self); // an empty Markdown clears the HTML
end;

procedure TCustomMarkdownToHTML.ExportToFileHTML(const AFileName: TFileName);
begin
  SaveUTF8File(AFileName, FHTMLContent.Text);
end;

procedure TCustomMarkdownToHTML.SetFileName(const AValue: TFileName);
begin
  if FFileName <> AValue then
  begin
    FFileName := AValue;
    if (AValue <> '') and FileExists(AValue) then
      LoadFromFile(AValue);
  end;
end;

procedure TCustomMarkdownToHTML.SetProcessorDialect(const AValue: TMarkdownProcessorDialect);
begin
  if FProcessorDialect <> AValue then
  begin
    FProcessorDialect := AValue;
    // also while loading: Extensions, when stored, is read after the dialect
    FExtensions := DefaultExtensions(AValue);
    UpdateHTML;
  end;
end;

procedure TCustomMarkdownToHTML.SetAllowUnsafe(const AValue: Boolean);
begin
  if FAllowUnsafe <> AValue then
  begin
    FAllowUnsafe := AValue;
    UpdateHTML;
  end;
end;

procedure TCustomMarkdownToHTML.SetExtensions(const AValue: TMarkdownExtensions);
begin
  if FExtensions <> AValue then
  begin
    FExtensions := AValue;
    UpdateHTML;
  end;
end;

procedure TCustomMarkdownToHTML.SetMathRendering(const AValue: TMarkdownMathRendering);
begin
  if FMathRendering <> AValue then
  begin
    FMathRendering := AValue;
    UpdateHTML;
  end;
end;

procedure TCustomMarkdownToHTML.SetCssStyle(const AValue: TStringList);
begin
  FCssStyle.Assign(AValue);
end;

procedure TCustomMarkdownToHTML.SetHTMLContent(const AValue: TStringList);
begin
  if FHTMLContent.Text <> AValue.Text then
    LoadFromString(AValue.Text, True);
end;

procedure TCustomMarkdownToHTML.SetMarkdownContent(const AValue: TStringList);
begin
  if FMarkdownContent.Text <> AValue.Text then
    LoadFromString(AValue.Text, False);
end;

function TCustomMarkdownToHTML.IsCssStyleStored: Boolean;
begin
  // compared without line breaks: TStringList may change them
  Result := not SameText(
    StringReplace(StringReplace(FCssStyle.Text, #13, '', [rfReplaceAll]), #10, '', [rfReplaceAll]),
    StringReplace(StringReplace(GetMarkdownDefaultCSS, #13, '', [rfReplaceAll]), #10, '', [rfReplaceAll]));
end;

function TCustomMarkdownToHTML.IsHtmlContentStored: Boolean;
begin
  Result := (FHTMLContent.Text <> '') and (FMarkdownContent.Text = '');
end;

function TCustomMarkdownToHTML.IsExtensionsStored: Boolean;
begin
  Result := FExtensions <> DefaultExtensions(FProcessorDialect);
end;

end.
