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
unit MDCodeHighlightEmitter;

{
  Syntax-highlighting code-block emitter for the MarkdownProcessor.

  It is plugged into TConfiguration.codeBlockEmitter: for every fenced code
  block the processor calls emitBlock(out_, lines, meta), where "meta" is the
  language declared after the opening fence (es. ```delphi). We map the language
  to a SynEdit highlighter and use TSynExporterHTML (inline CSS) to produce
  colored HTML embedded into the generated document.

  THTMLViewer has no JavaScript engine, so client-side highlighters (Prism,
  highlight.js) cannot be used: the coloring must be produced here, in Delphi.

  The whole mechanism is guarded by the MD_SYNTAX_HIGHLIGHTING define (see
  MDSyntaxHighlight.inc). When it is turned off, this unit does NOT
  reference any SynEdit unit and CreateCodeHighlightEmitter returns nil, so a
  documentation-only consumer is not forced to link the SynEdit suite.

  Callers always work with the SynEdit-free base type TCodeHighlightEmitterBase
  and the CreateCodeHighlightEmitter factory, so they compile in both modes.
}

interface

uses
  System.Classes
  , Vcl.Graphics
  , MarkdownUtils;

type
  //SynEdit-free base type, always available so that callers do not depend on
  //the highlighting implementation (nor on SynEdit) at compile time.
  TCodeHighlightEmitterBase = class(TBlockEmitter)
  public
    procedure SetTheme(const ADark: Boolean; const ABackground, AForeground: TColor;
      const AFontName: string; const AFontSize: Integer); virtual; abstract;
  end;

//Returns a code-block highlighting emitter, or nil when MD_SYNTAX_HIGHLIGHTING
//is disabled (in that case the processor uses its default code emission).
//The caller owns the returned instance.
function CreateCodeHighlightEmitter: TCodeHighlightEmitterBase;

implementation

{$IFDEF MD_SYNTAX_HIGHLIGHTING}

uses
  Winapi.Windows
  , System.SysUtils
  , System.Generics.Collections
  , SynEditHighlighter
  , SynEditMiscProcs
  , SynExportHTML
  //--- highlighter suite (one unit per supported language) ---
  , SynHighlighterPas
  , SynHighlighterDfm
  , SynHighlighterDWS
  , SynHighlighterCpp
  , SynHighlighterCS
  , SynHighlighterJScript
  , SynHighlighterJSON
  , SynHighlighterPython
  , SynHighlighterSQL
  , SynHighlighterJava
  , SynHighlighterPHP
  , SynHighlighterHtml
  , SynHighlighterXML
  , SynHighlighterCss
  , SynHighlighterIni
  , SynHighlighterBat
  , SynHighlighterYAML
  , SynHighlighterPerl
  , SynHighlighterRuby
  , SynHighlighterHaskell
  , SynHighlighterFortran
  , SynHighlighterCobol
  , SynHighlighterAsm
  , SynHighlighterVB
  , SynHighlighterVBScript
  , SynHighlighterTclTk
  , SynHighlighterTeX
  , SynHighlighterInno
  , SynHighlighterModelica
  , SynHighlighterEiffel
  , SynHighlighterRexx
  , SynHighlighterKix
  , SynHighlighterIDL
  , SynHighlighterWebIDL
  , SynHighlighterBaan
  , SynHighlighterCache
  , SynHighlighterCPM
  , SynHighlighterDml
  , SynHighlighterDOT
  , SynHighlighterFoxpro
  , SynHighlighterGalaxy
  , SynHighlighterGWS
  , SynHighlighterLLVM
  , SynHighlighterLDraw
  , SynHighlighterSml
  , SynHighlighterST
  , SynHighlighterSDD
  , SynHighlighterRC
  , SynHighlighterUNIXShellScript
  , SynHighlighterUnreal
  , SynHighlighterADSP21xx
  , SynHighlighterCAC
  ;

type
  TSynCustomHighlighterClass = class of TSynCustomHighlighter;

  TSynHighlightBlockEmitter = class(TCodeHighlightEmitterBase)
  private
    FExporter: TSynExporterHTML;
    FHighlighters: TObjectDictionary<string, TSynCustomHighlighter>;
    FLangMap: TDictionary<string, TSynCustomHighlighterClass>;
    FDark: Boolean;
    FCodeBackground: TColor;
    FCodeForeground: TColor;
    FFontName: string;
    FFontSize: Integer;
    procedure BuildLangMap;
    function NormalizeLang(const AMeta: string): string;
    function GetHighlighter(const ALang: string): TSynCustomHighlighter;
    procedure ApplyColorScheme(const AHighlighter: TSynCustomHighlighter);
    procedure EmitPlainBlock(out_: TStringBuilder; lines: TStringList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure emitBlock(out_: TStringBuilder; lines: TStringList; meta: String); override;
    procedure SetTheme(const ADark: Boolean; const ABackground, AForeground: TColor;
      const AFontName: string; const AFontSize: Integer); override;
  end;

{ TSynHighlightBlockEmitter }

constructor TSynHighlightBlockEmitter.Create;
begin
  inherited Create;
  FExporter := TSynExporterHTML.Create(nil);
  FExporter.InlineCSS := True;
  FExporter.CreateHTMLFragment := True;
  FExporter.SuppressFragmentInfo := True;
  FExporter.UseBackground := True;
  //TObjectDictionary owns the highlighter instances and frees them.
  FHighlighters := TObjectDictionary<string, TSynCustomHighlighter>.Create([doOwnsValues]);
  FLangMap := TDictionary<string, TSynCustomHighlighterClass>.Create;
  //Sensible defaults; overridden by SetTheme.
  FDark := False;
  FCodeBackground := $00F8F8F8;
  FCodeForeground := clBlack;
  FFontName := 'Consolas';
  FFontSize := 10;
  BuildLangMap;
end;

destructor TSynHighlightBlockEmitter.Destroy;
begin
  FLangMap.Free;
  //Detach the exporter from its highlighter while the cached highlighters are
  //still alive, then free them, then the exporter. Avoids the exporter holding
  //a dangling highlighter reference (see SetTheme note).
  if Assigned(FExporter) then
    FExporter.Highlighter := nil;
  FHighlighters.Free;
  FExporter.Free;
  inherited;
end;

procedure TSynHighlightBlockEmitter.BuildLangMap;

  procedure Map(const AAliases: array of string; AClass: TSynCustomHighlighterClass);
  var
    LAlias: string;
  begin
    for LAlias in AAliases do
      FLangMap.AddOrSetValue(LAlias, AClass);
  end;

begin
  //Popular languages first (rich set of aliases), then the rest of the suite.
  Map(['pascal', 'delphi', 'pas', 'dpr', 'objectpascal', 'lpr'], TSynPasSyn);
  Map(['dfm', 'lfm', 'fmx'], TSynDfmSyn);
  Map(['dws', 'dwscript', 'pascalscript'], TSynDWSSyn);
  Map(['c', 'cpp', 'c++', 'h', 'hpp', 'cc', 'cxx', 'hxx'], TSynCppSyn);
  Map(['cs', 'csharp'], TSynCSSyn);
  Map(['js', 'javascript', 'ts', 'typescript', 'jsx', 'tsx', 'node'], TSynJScriptSyn);
  Map(['json', 'jsonc'], TSynJSONSyn);
  Map(['python', 'py'], TSynPythonSyn);
  Map(['sql', 'mysql', 'plsql', 'tsql'], TSynSQLSyn);
  Map(['java'], TSynJavaSyn);
  Map(['php'], TSynPHPSyn);
  Map(['html', 'htm', 'xhtml'], TSynHTMLSyn);
  Map(['xml', 'xsd', 'xsl', 'xslt', 'svg', 'wsdl', 'dtd'], TSynXMLSyn);
  Map(['css'], TSynCssSyn);
  Map(['ini', 'cfg', 'conf'], TSynIniSyn);
  Map(['bat', 'batch', 'cmd'], TSynBatSyn);
  Map(['yaml', 'yml'], TSynYAMLSyn);
  Map(['perl', 'pl', 'pm'], TSynPerlSyn);
  Map(['ruby', 'rb'], TSynRubySyn);
  Map(['haskell', 'hs'], TSynHaskellSyn);
  Map(['fortran', 'f', 'f90', 'f95'], TSynFortranSyn);
  Map(['cobol', 'cob'], TSynCobolSyn);
  Map(['asm', 'assembler', 'nasm'], TSynAsmSyn);
  Map(['vb', 'vbnet', 'visualbasic'], TSynVBSyn);
  Map(['vbs', 'vbscript'], TSynVBScriptSyn);
  Map(['tcl', 'tk'], TSynTclTkSyn);
  Map(['tex', 'latex'], TSynTeXSyn);
  Map(['inno', 'iss', 'innosetup'], TSynInnoSyn);
  Map(['modelica', 'mo'], TSynModelicaSyn);
  Map(['eiffel', 'e'], TSynEiffelSyn);
  Map(['rexx'], TSynRexxSyn);
  Map(['kix', 'kixtart'], TSynKixSyn);
  Map(['idl'], TSynIdlSyn);
  Map(['webidl'], TSynWebIDLSyn);
  Map(['baan'], TSynBaanSyn);
  Map(['cache', 'objectscript', 'intersystems'], TSynCacheSyn);
  Map(['cpm'], TSynCPMSyn);
  Map(['dml'], TSynDmlSyn);
  Map(['dot', 'graphviz', 'gv'], TSynDOTSyn);
  Map(['foxpro', 'prg'], TSynFoxproSyn);
  Map(['galaxy'], TSynGalaxySyn);
  Map(['gws'], TSynGWScriptSyn);
  Map(['llvm', 'll', 'llvmir'], TSynLLVMIRSyn);
  Map(['ldraw', 'ldr'], TSynLDRSyn);
  Map(['sml'], TSynSMLSyn);
  Map(['st'], TSynSTSyn);
  Map(['sdd'], TSynSDDSyn);
  Map(['rc', 'resource'], TSynRCSyn);
  Map(['sh', 'bash', 'shell', 'zsh', 'ksh'], TSynUNIXShellScriptSyn);
  Map(['unrealscript', 'uc', 'unreal'], TSynUnrealSyn);
  Map(['adsp21xx', 'adsp'], TSynADSP21xxSyn);
  Map(['cac'], TSynCACSyn);
end;

function TSynHighlightBlockEmitter.NormalizeLang(const AMeta: string): string;
var
  LMeta: string;
  P: Integer;
begin
  //GFM info-string: the language is the first word after the fence.
  LMeta := Trim(AMeta);
  P := 1;
  while (P <= Length(LMeta)) and (LMeta[P] > ' ') do
    Inc(P);
  Result := LowerCase(Copy(LMeta, 1, P - 1));
end;

procedure TSynHighlightBlockEmitter.ApplyColorScheme(
  const AHighlighter: TSynCustomHighlighter);
var
  I: Integer;
  LAttri: TSynHighlighterAttributes;
  LName: string;
  LMatched: Boolean;
  //Theme palette (VS Code-like)
  CKeyword, CComment, CString, CNumber, CPreproc, CSymbol, CVariable: TColor;

  function Brighten(AColor: TColor): TColor;
  var
    R, G, B: Byte;
  begin
    AColor := ColorToRGB(AColor);
    R := GetRValue(AColor); G := GetGValue(AColor); B := GetBValue(AColor);
    Result := RGB(R + (255 - R) div 2, G + (255 - G) div 2, B + (255 - B) div 2);
  end;

  function Luminance(AColor: TColor): Integer;
  begin
    AColor := ColorToRGB(AColor);
    Result := (GetRValue(AColor) * 299 + GetGValue(AColor) * 587 +
      GetBValue(AColor) * 114) div 1000;
  end;

  procedure SetFg(const AColor: TColor; const ABold, AItalic: Boolean);
  begin
    LAttri.Foreground := AColor;
    if ABold then LAttri.Style := LAttri.Style + [fsBold];
    if AItalic then LAttri.Style := LAttri.Style + [fsItalic];
    LMatched := True;
  end;

begin
  //Many SynEdit highlighters do not set default foreground colors (the token
  //attributes have Foreground = clNone), so without an explicit scheme those
  //languages would render uncolored. We assign a consistent, theme-aware
  //palette by matching the attribute Name. Attributes we don't recognize keep
  //the highlighter's own color (and, in dark mode, get a contrast fix).
  if FDark then
  begin
    CKeyword := RGB($56, $9C, $D6);   // blue
    CComment := RGB($6A, $99, $55);   // green
    CString  := RGB($CE, $91, $78);   // orange/brown
    CNumber  := RGB($B5, $CE, $A8);   // light green
    CPreproc := RGB($C5, $86, $C0);   // purple
    CSymbol  := RGB($D4, $D4, $D4);   // light gray
    CVariable:= RGB($9C, $DC, $FE);   // light blue
  end
  else
  begin
    CKeyword := RGB($00, $00, $FF);   // blue
    CComment := RGB($00, $80, $00);   // green
    CString  := RGB($A3, $15, $15);   // dark red
    CNumber  := RGB($09, $86, $58);   // teal
    CPreproc := RGB($80, $00, $80);   // purple
    CSymbol  := RGB($55, $55, $55);   // gray
    CVariable:= RGB($00, $10, $80);   // dark blue
  end;

  for I := 0 to AHighlighter.AttrCount - 1 do
  begin
    LAttri := AHighlighter.Attribute[I];
    LName := LowerCase(LAttri.Name);
    LMatched := False;
    if (Pos('comment', LName) > 0) or (Pos('documentation', LName) > 0) then
      SetFg(CComment, False, True)
    else if (Pos('string', LName) > 0) or (Pos('character', LName) > 0) then
      SetFg(CString, False, False)
    else if (Pos('preprocessor', LName) > 0) or (Pos('pragma', LName) > 0) then
      SetFg(CPreproc, False, False)
    else if (Pos('reserved', LName) > 0) or (LName = 'key') or
            (Pos('datatype', LName) > 0) or (Pos('directive', LName) > 0) then
      SetFg(CKeyword, True, False)
    else if (Pos('number', LName) > 0) or (Pos('float', LName) > 0) or
            (Pos('hex', LName) > 0) then
      SetFg(CNumber, False, False)
    else if (Pos('symbol', LName) > 0) then
      SetFg(CSymbol, False, False)
    else if (Pos('variable', LName) > 0) then
      SetFg(CVariable, False, False);

    //Unrecognized attribute with its own color: keep it, but ensure contrast
    //against a dark background.
    if (not LMatched) and FDark then
    begin
      if (LAttri.Foreground <> clNone) and (LAttri.Foreground <> clDefault) and
         (Luminance(LAttri.Foreground) < 110) then
        LAttri.Foreground := Brighten(LAttri.Foreground);
    end;
  end;
end;

function TSynHighlightBlockEmitter.GetHighlighter(
  const ALang: string): TSynCustomHighlighter;
var
  LClass: TSynCustomHighlighterClass;
begin
  if FHighlighters.TryGetValue(ALang, Result) then
    Exit;
  if not FLangMap.TryGetValue(ALang, LClass) then
    Exit(nil);
  Result := LClass.Create(nil);
  ApplyColorScheme(Result);
  FHighlighters.Add(ALang, Result);
end;

procedure TSynHighlightBlockEmitter.EmitPlainBlock(out_: TStringBuilder;
  lines: TStringList);
var
  I: Integer;
  C: Char;
  S: string;
begin
  //Fallback identical in spirit to the default decorator, but with a
  //theme-aware background so unknown languages still match the look.
  out_.Append('<pre style="background-color:').Append(ColorToHTML(FCodeBackground))
      .Append(';color:').Append(ColorToHTML(FCodeForeground)).Append(';"><code>');
  for I := 0 to lines.Count - 1 do
  begin
    S := lines[I];
    for C in S do
      case C of
        '&': out_.Append('&amp;');
        '<': out_.Append('&lt;');
        '>': out_.Append('&gt;');
      else
        out_.Append(C);
      end;
    if I < lines.Count - 1 then
      out_.Append(#10);
  end;
  out_.Append('</code></pre>'#10);
end;

procedure TSynHighlightBlockEmitter.emitBlock(out_: TStringBuilder;
  lines: TStringList; meta: String);
var
  LHighlighter: TSynCustomHighlighter;
begin
  LHighlighter := GetHighlighter(NormalizeLang(meta));
  if LHighlighter = nil then
  begin
    EmitPlainBlock(out_, lines);
    Exit;
  end;
  FExporter.Highlighter := LHighlighter;
  FExporter.Color := FCodeBackground;
  FExporter.Font.Name := FFontName;
  FExporter.Font.Size := FFontSize;
  FExporter.Font.Color := FCodeForeground;
  FExporter.Clear;
  FExporter.ExportAll(lines);
  out_.Append(FExporter.ExportedText).Append(#10);
end;

procedure TSynHighlightBlockEmitter.SetTheme(const ADark: Boolean;
  const ABackground, AForeground: TColor; const AFontName: string;
  const AFontSize: Integer);
begin
  //No-op when nothing changed, so callers can invoke this before every
  //refresh without invalidating the highlighter cache each time.
  if (ADark = FDark) and (ABackground = FCodeBackground) and
     (AForeground = FCodeForeground) and (AFontName = FFontName) and
     (AFontSize = FFontSize) then
    Exit;
  FDark := ADark;
  FCodeBackground := ABackground;
  FCodeForeground := AForeground;
  FFontName := AFontName;
  FFontSize := AFontSize;
  //Token colors depend on the theme: drop cached highlighters so they get
  //recreated (and re-adjusted) on next use.
  //IMPORTANT: detach the exporter from its current highlighter BEFORE freeing
  //the cached highlighters. TSynCustomExporter.SetHighlighter keeps a raw
  //reference and never nulls it when the highlighter is destroyed (it has no
  //Notification handler), so clearing the cache while the exporter still points
  //to one of these instances leaves a dangling pointer: the next emitBlock ->
  //SetHighlighter would dereference it (FreeNotification on freed memory) and
  //raise an Access/External Violation (reliably on Win64).
  FExporter.Highlighter := nil;
  FHighlighters.Clear;
end;

function CreateCodeHighlightEmitter: TCodeHighlightEmitterBase;
begin
  Result := TSynHighlightBlockEmitter.Create;
end;

{$ELSE}

function CreateCodeHighlightEmitter: TCodeHighlightEmitterBase;
begin
  //Syntax highlighting disabled at compile time: no emitter, the processor
  //uses its default <pre><code> emission. No SynEdit unit is referenced.
  Result := nil;
end;

{$ENDIF}

end.
