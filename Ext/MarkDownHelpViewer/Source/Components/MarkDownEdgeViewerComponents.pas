{******************************************************************************}
{                                                                              }
{       Viewer Components to show Markdown and HTML content                    }
{       TEdgeMarkdownViewer: rendering with Microsoft Edge WebView2            }
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
unit MarkDownEdgeViewerComponents;

{ TEdgeMarkdownViewer: a TEdgeBrowser (Microsoft Edge WebView2) that shows
  Markdown (and HTML) content, with math formulas (KaTeX) and mermaid diagrams.
  The Markdown properties are the same as TMarkdownViewer and TMarkdownToHTML
  (TMarkdownViewerEngine of MarkDownViewerCommon).
  Requires Delphi 12 or later (the WebView2 interfaces of Delphi 11 lack
  SetVirtualHostNameToFolderMapping and PrintToPdf). At runtime it requires
  the WebView2 runtime and WebView2Loader.dll (see EdgeAvailable). }

{$IF CompilerVersion < 36}
  {$MESSAGE FATAL 'TEdgeMarkdownViewer requires Delphi 12 or later'}
{$IFEND}

interface

uses
  System.Classes
  , System.SysUtils
  , Winapi.Windows
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Edge
  , Winapi.WebView2
  , MarkdownUtils
  , MarkdownProcessor
  , MarkDownViewerCommon
  , MDCodeHighlightEmitter
  ;

const
  //Virtual hosts of the page: the folder of the document and ScriptsFolder
  MDViewerContentHost = 'mdviewer-content.local';
  MDViewerScriptsHost = 'mdviewer-scripts.local';
  //Scripts from CDN, when there is no local scripts folder (the same versions
  //of the Scripts folder distributed with the viewers)
  KaTeXCDN = 'https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/';
  MermaidCDN = 'https://cdn.jsdelivr.net/npm/mermaid@11.17.2/dist/';
  //The local scripts folder searched next to the executable (see FindScriptsFolder)
  MDViewerScriptsFolderName = 'Scripts';

type
  TCustomEdgeMarkdownViewer = class(TCustomEdgeBrowser)
  private
    FEngine: TMarkdownViewerEngine;
    FCodeHighlightEmitter: TCodeHighlightEmitterBase;
    FFileName: TFileName;
    FBaseFolder: string;
    FMappedFolder: string;
    FMappedScriptsFolder: string;
    FServerRoot: TFolderName;
    FScriptsFolder: TFolderName;
    FAutoLoadOnHotSpotClick: Boolean;
    FResetPosition: Boolean;
    FPendingRefresh: Boolean;
    FScrollY: Integer;
    FScrollMax: Integer;
    FExpectedScrollY: Integer;
    FExporting: Boolean;
    FOnScrollChanged: TNotifyEvent;
    FDefFontName: TFontName;
    FDefFontSize: Integer;
    FDefFontColor: TColor;
    FDefBackground: TColor;
    FDefHotSpotColor: TColor;
    FOnFileNameClicked: TFileNameClicked;
    FOnURLClicked: TURLClicked;
    FOnContentLoaded: TNotifyEvent;
    FOnSaveAs: TNotifyEvent;
    FDocumentFileName: string;
    function GetProcessorDialect: TMarkdownProcessorDialect;
    procedure SetProcessorDialect(const AValue: TMarkdownProcessorDialect);
    function GetExtensions: TMarkdownExtensions;
    procedure SetExtensions(const AValue: TMarkdownExtensions);
    function GetAllowUnsafe: Boolean;
    procedure SetAllowUnsafe(const AValue: Boolean);
    function GetMathRendering: TMarkdownMathRendering;
    procedure SetMathRendering(const AValue: TMarkdownMathRendering);
    function GetCssStyle: TStringList;
    procedure SetCssStyle(const AValue: TStringList);
    function GetHTMLContent: TStringList;
    procedure SetHTMLContent(const AValue: TStringList);
    function GetMarkdownContent: TStringList;
    procedure SetMarkdownContent(const AValue: TStringList);
    procedure SetFileName(const AValue: TFileName);
    procedure SetServerRoot(const AValue: TFolderName);
    procedure SetScriptsFolder(const AValue: TFolderName);
    procedure SetDefFontName(const AValue: TFontName);
    procedure SetDefFontSize(const AValue: Integer);
    procedure SetDefFontColor(const AValue: TColor);
    procedure SetDefBackground(const AValue: TColor);
    procedure SetDefHotSpotColor(const AValue: TColor);
    function GetHelpContext: THelpContext;
    function GetHelpKeyword: String;
    procedure SetHelpContext(const AValue: THelpContext);
    procedure SetHelpKeyword(const AValue: String);
    function IsHelpContextStored: Boolean;
    function IsHelpKeywordStored: Boolean;
    function IsCssStyleStored: Boolean;
    function IsExtensionsStored: Boolean;
    function IsHtmlContentStored: Boolean;
    function IsDefFontNameStored: Boolean;
    procedure HTMLContentChanged(Sender: TObject);
    procedure EngineBeforeProcess(Sender: TObject);
    procedure WebViewCreateCompleted(Sender: TCustomEdgeBrowser; AResult: HResult);
    procedure WebViewNavigationStarting(Sender: TCustomEdgeBrowser;
      Args: TNavigationStartingEventArgs);
    procedure WebViewNavigationCompleted(Sender: TCustomEdgeBrowser;
      IsSuccess: Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS);
    procedure WebViewNewWindowRequested(Sender: TCustomEdgeBrowser;
      Args: TNewWindowRequestedEventArgs);
    procedure WebViewMessageReceived(Sender: TCustomEdgeBrowser;
      Args: TWebMessageReceivedEventArgs);
    function GetContentFolder: string;
    function GetEffectiveScriptsFolder: string;
    function GetScrollRatio: Double;
    procedure UpdateFolderMappings;
    procedure EnsureWebView;
    function LinkClicked(const AURI: string): Boolean;
    function ScriptsURL: string;
    function PageStyle: string;
    function PageScripts(const AHTML: string; const AScrollY: Integer): string;
    procedure RegisterSaveAsHandler;
    function IsDarkBackground: Boolean;
    procedure UpdateColorScheme;
    procedure DoSaveAs;
  protected
    procedure CreateWnd; override;
    procedure ReadState(Reader: TReader); override;
    procedure Loaded; override;
    procedure AutoLoadFile; virtual;
    function FindHelpFile(var AFileName: TFileName; const AContext: Integer;
      const HelpKeyword: string): Boolean; virtual;
    /// <summary>The full HTML page shown: head with style and scripts, body
    /// with HtmlContent.</summary>
    function BuildPage(const AScrollY: Integer): string; virtual;
    /// <summary>The Markdown engine (options, contents, conversion).</summary>
    property Engine: TMarkdownViewerEngine read FEngine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>True when WebView2 can be used on this machine: the loader
    /// (WebView2Loader.dll) and the WebView2 runtime are available.</summary>
    class function EdgeAvailable: Boolean;
    /// <summary>The local folder of KaTeX and mermaid.js distributed with the
    /// application: "Scripts" next to the executable or in its parent folder
    /// (e.g. Bin32\..\Scripts); empty when it does not exist.</summary>
    class function FindScriptsFolder: string;
    /// <summary>The folder of the scripts used: ScriptsFolder, else the one
    /// found by FindScriptsFolder; empty: from CDN.</summary>
    property EffectiveScriptsFolder: string read GetEffectiveScriptsFolder;
    procedure LoadFromFile(const AFileName: TFileName);
    procedure LoadFromStream(const AStream: TStringStream;
      const IsHTMLContent: Boolean = False);
    procedure LoadFromString(const AValue: string;
      const IsHTMLContent: Boolean = False);
    /// <summary>Saves HtmlContent (stylesheet + HTML fragment).</summary>
    procedure ExportToFileHTML(const AFileName: TFileName);
    /// <summary>Saves the full page (with style and scripts).</summary>
    procedure ExportToFileHTMLPage(const AFileName: TFileName);
    /// <summary>Converts a Markdown text with the given dialect (and its
    /// default extensions), stylesheet and safe mode.</summary>
    function TransformContent(const AMarkdownContent: string;
      AProcessorDialect: TMarkdownProcessorDialect = DefaultMarkdownDialect;
      const ACssStyle: string = '';
      const AAllowUnsafe: Boolean = False): string;
    /// <summary>Shows HtmlContent again (at the same position, if required).</summary>
    procedure RefreshViewer(const APreservePosition: Boolean = True);
    procedure CopyToClipboard;
    procedure SelectAll;
    /// <summary>True when the focus is in the browser (its window is a child
    /// of the control: Focused is always False).</summary>
    function HasFocus: Boolean;
    /// <summary>Scrolls the page to a position between 0 (top) and 1 (end):
    /// it does not fire OnScrollChanged.</summary>
    procedure ScrollToRatio(const ARatio: Double);
    /// <summary>The position of the page, between 0 (top) and 1 (end).</summary>
    property ScrollRatio: Double read GetScrollRatio;
    /// <summary>Fired when the user scrolls the page (not by ScrollToRatio).</summary>
    property OnScrollChanged: TNotifyEvent read FOnScrollChanged write FOnScrollChanged;
    /// <summary>Saves the page as PDF (asynchronous: OnPrintToPDFCompleted).</summary>
    function ExportToPDF(const AFileName: TFileName): Boolean;
    /// <summary>Asks the file name (proposed from DocumentFileName) and saves
    /// the full page with ExportToFileHTMLPage.</summary>
    procedure SaveAsHTMLPage;
    /// <summary>The file of the document shown (set by LoadFromFile): the
    /// name proposed by SaveAsHTMLPage.</summary>
    property DocumentFileName: string read FDocumentFileName write FDocumentFileName;

    property AutoLoadOnHotSpotClick: Boolean read FAutoLoadOnHotSpotClick write FAutoLoadOnHotSpotClick default True;
    property CssStyle: TStringList read GetCssStyle write SetCssStyle stored IsCssStyleStored;
    property FileName: TFileName read FFileName write SetFileName;
    //Changing the dialect resets Extensions to its defaults
    property ProcessorDialect: TMarkdownProcessorDialect read GetProcessorDialect write SetProcessorDialect default DefaultMarkdownDialect;
    //Optional syntax of mdCommonMark, mdGFM and mdGitHub (ignored by the
    //legacy dialects). Default: the extensions of the dialect + LegacyExtensions
    property Extensions: TMarkdownExtensions read GetExtensions write SetExtensions stored IsExtensionsStored;
    property AllowUnsafe: Boolean read GetAllowUnsafe write SetAllowUnsafe default False;
    //WebView2 runs JavaScript: math formulas for KaTeX by default
    property MathRendering: TMarkdownMathRendering read GetMathRendering write SetMathRendering default mmrMarkup;
    property HtmlContent: TStringList read GetHTMLContent write SetHTMLContent stored IsHtmlContentStored;
    property MarkdownContent: TStringList read GetMarkdownContent write SetMarkdownContent;
    //The folder of the help files (relative links and images)
    property ServerRoot: TFolderName read FServerRoot write SetServerRoot;
    //The local folder of KaTeX and mermaid.js (offline use): katex\katex.min.css,
    //katex\katex.min.js, katex\contrib\auto-render.min.js, katex\fonts and
    //mermaid\mermaid.min.js. Empty: the "Scripts" folder next to the
    //executable (FindScriptsFolder), if it exists, else from CDN
    property ScriptsFolder: TFolderName read FScriptsFolder write SetScriptsFolder;
    //Base style of the page (the same names as THTMLViewer)
    property DefFontName: TFontName read FDefFontName write SetDefFontName stored IsDefFontNameStored;
    property DefFontSize: Integer read FDefFontSize write SetDefFontSize default 10;
    property DefFontColor: TColor read FDefFontColor write SetDefFontColor default clWindowText;
    property DefBackground: TColor read FDefBackground write SetDefBackground default clWindow;
    property DefHotSpotColor: TColor read FDefHotSpotColor write SetDefHotSpotColor default clBlue;
    property OnFileNameClicked: TFileNameClicked read FOnFileNameClicked write FOnFileNameClicked;
    property OnURLClicked: TURLClicked read FOnURLClicked write FOnURLClicked;
    //Fired when a content has been shown
    property OnContentLoaded: TNotifyEvent read FOnContentLoaded write FOnContentLoaded;
    //"Save as" of the browser (context menu, Ctrl+S): the page shown has no
    //file name and refers to the virtual hosts, so the browser does not save
    //it; the full page is saved by SaveAsHTMLPage, or by this event if assigned
    //(from Delphi 13; with Delphi 12 "Save as" is removed from the context menu)
    property OnSaveAs: TNotifyEvent read FOnSaveAs write FOnSaveAs;
  published
    //Override Help properties because SetHelpKeyword and SetHelpContext are not Virtual
    property HelpKeyword: String read GetHelpKeyword write SetHelpKeyword stored IsHelpKeywordStored;
    property HelpContext: THelpContext read GetHelpContext write SetHelpContext stored IsHelpContextStored;
  end;

  TEdgeMarkdownViewer = class(TCustomEdgeMarkdownViewer)
  published
    property Align;
    property AlignWithMargins;
    property Anchors;
    property Constraints;
    property HelpType;
    property Margins;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEnter;
    property OnExit;
    property UserDataFolder;
    property OnPrintToPDFCompleted;
    //specific properties
    property AutoLoadOnHotSpotClick;
    property CssStyle;
    property FileName;
    property ProcessorDialect;
    property Extensions;
    property AllowUnsafe;
    property MathRendering;
    property HtmlContent;
    property MarkdownContent;
    property ServerRoot;
    property ScriptsFolder;
    property DefFontName;
    property DefFontSize;
    property DefFontColor;
    property DefBackground;
    property DefHotSpotColor;
    property OnFileNameClicked;
    property OnURLClicked;
    property OnContentLoaded;
    property OnSaveAs;
  end;

implementation

uses
  System.NetEncoding
  , System.StrUtils
  , System.IOUtils
  , Winapi.ActiveX
  , Winapi.EdgeUtils
  , Vcl.Dialogs
  ;

const
  DefaultFontName = 'Arial';

var
  //The scripts folder found next to the executable (FindScriptsFolder)
  _ScriptsFolderSearched: Boolean;
  _FoundScriptsFolder: string;

function ColorToHTML(const AColor: TColor): string;
var
  LRGB: Cardinal;
begin
  LRGB := Cardinal(ColorToRGB(AColor));
  Result := Format('#%.2x%.2x%.2x', [GetRValue(LRGB), GetGValue(LRGB), GetBValue(LRGB)]);
end;

//A WebView2 string (to free with CoTaskMemFree)
function TakeWebViewString(var AValue: PWideChar): string;
begin
  if AValue <> nil then
  begin
    Result := AValue;
    CoTaskMemFree(AValue);
    AValue := nil;
  end
  else
    Result := '';
end;

{ TCustomEdgeMarkdownViewer }

class function TCustomEdgeMarkdownViewer.EdgeAvailable: Boolean;
type
  TGetAvailableBrowserVersion = function(BrowserExecutableFolder: PWideChar;
    out VersionInfo: PWideChar): HResult; stdcall;
var
  LModule: HMODULE;
  LGetVersion: TGetAvailableBrowserVersion;
  LVersion: PWideChar;
begin
  //The loader (WebView2Loader.dll, loaded by IsEdgeAvailable)...
  Result := IsEdgeAvailable;
  if not Result then
    Exit;
  //...and the WebView2 runtime installed
  LModule := GetModuleHandle('WebView2Loader.dll');
  @LGetVersion := GetProcAddress(LModule, 'GetAvailableCoreWebView2BrowserVersionString');
  if Assigned(LGetVersion) then
  begin
    LVersion := nil;
    Result := Succeeded(LGetVersion(nil, LVersion)) and (LVersion <> nil);
    TakeWebViewString(LVersion);
  end;
end;

constructor TCustomEdgeMarkdownViewer.Create(AOwner: TComponent);
begin
  inherited;
  FAutoLoadOnHotSpotClick := True;
  FDefFontName := DefaultFontName;
  FDefFontSize := 10;
  FDefFontColor := clWindowText;
  FDefBackground := clWindow;
  FDefHotSpotColor := clBlue;
  FExpectedScrollY := -1;
  //Optional syntax-highlighting emitter for fenced code blocks (nil when the
  //MD_SYNTAX_HIGHLIGHTING define is off, so no SynEdit dependency is linked).
  FCodeHighlightEmitter := CreateCodeHighlightEmitter;

  //The Markdown engine: the viewer is refreshed when its HTML changes
  FEngine := TMarkdownViewerEngine.Create(nil);
  FEngine.CssStyle.Text := GetMarkdownDefaultCSS;
  FEngine.MathRendering := mmrMarkup;
  FEngine.CodeBlockEmitter := FCodeHighlightEmitter;
  FEngine.OnBeforeProcess := EngineBeforeProcess;
  FEngine.HtmlContent.OnChange := HTMLContentChanged;

  //Events of the browser used by the viewer
  OnCreateWebViewCompleted := WebViewCreateCompleted;
  OnNavigationStarting := WebViewNavigationStarting;
  OnNavigationCompleted := WebViewNavigationCompleted;
  OnNewWindowRequested := WebViewNewWindowRequested;
  OnWebMessageReceived := WebViewMessageReceived;
end;

destructor TCustomEdgeMarkdownViewer.Destroy;
begin
  if Assigned(FEngine) then
    FEngine.HtmlContent.OnChange := nil;
  FreeAndNil(FEngine);
  FreeAndNil(FCodeHighlightEmitter);
  inherited;
end;

procedure TCustomEdgeMarkdownViewer.CreateWnd;
begin
  //The WebView2 data (cache, cookies) in the local application data: the
  //folder of the executable may be not writable (Program Files)
  if (UserDataFolder = '') and not (csDesigning in ComponentState) then
    UserDataFolder := TPath.Combine(TPath.Combine(GetEnvironmentVariable('LOCALAPPDATA'),
      ChangeFileExt(ExtractFileName(ParamStr(0)), '')), 'WebView2');
  inherited;
  //A content assigned before the window: the WebView can be created now
  if FPendingRefresh then
    EnsureWebView;
end;

procedure TCustomEdgeMarkdownViewer.EnsureWebView;
begin
  //TEdgeBrowser creates the WebView only in Navigate: here it is created
  //(asynchronously: WebViewCreateCompleted) to show the content
  if not (csDesigning in ComponentState) and HandleAllocated and
    (BrowserControlState = TBrowserControlState.None) then
    CreateWebView;
end;

procedure TCustomEdgeMarkdownViewer.ReadState(Reader: TReader);
begin
  //Options and contents are read in any order: a single conversion at the end
  FEngine.BeginUpdate;
  try
    inherited;
  finally
    FEngine.EndUpdate;
  end;
end;

procedure TCustomEdgeMarkdownViewer.Loaded;
begin
  inherited;
  if FEngine.HtmlContent.Text <> '' then
    RefreshViewer(False);
end;

procedure TCustomEdgeMarkdownViewer.WebViewCreateCompleted(Sender: TCustomEdgeBrowser;
  AResult: HResult);
begin
  if Succeeded(AResult) then
  begin
    RegisterSaveAsHandler;
    FMappedFolder := '';
    FMappedScriptsFolder := '';
    //The content set before the creation of the WebView
    if FPendingRefresh or (FEngine.HtmlContent.Text <> '') then
      RefreshViewer(False);
  end;
end;

function TCustomEdgeMarkdownViewer.GetContentFolder: string;
begin
  //The folder of the document shown, else the help folder
  if FBaseFolder <> '' then
    Result := FBaseFolder
  else if FServerRoot <> '' then
    Result := FServerRoot
  else
    Result := GetMDViewerServerRoot;
  if Result <> '' then
    Result := ExcludeTrailingPathDelimiter(ExpandFileName(Result));
end;

procedure TCustomEdgeMarkdownViewer.UpdateFolderMappings;
var
  LWebView3: ICoreWebView2_3;
  LFolder: string;
begin
  if not Supports(DefaultInterface, ICoreWebView2_3, LWebView3) then
    Exit;
  //The folder of the document: relative images and links
  LFolder := GetContentFolder;
  if not SameText(LFolder, FMappedFolder) then
  begin
    if FMappedFolder <> '' then
      LWebView3.ClearVirtualHostNameToFolderMapping(MDViewerContentHost);
    if (LFolder <> '') and DirectoryExists(LFolder) then
      LWebView3.SetVirtualHostNameToFolderMapping(MDViewerContentHost,
        PChar(LFolder), COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND_ALLOW);
    FMappedFolder := LFolder;
  end;
  //The local scripts (KaTeX, mermaid)
  LFolder := GetEffectiveScriptsFolder;
  if LFolder <> '' then
    LFolder := ExcludeTrailingPathDelimiter(ExpandFileName(LFolder));
  if not SameText(LFolder, FMappedScriptsFolder) then
  begin
    if FMappedScriptsFolder <> '' then
      LWebView3.ClearVirtualHostNameToFolderMapping(MDViewerScriptsHost);
    if (LFolder <> '') and DirectoryExists(LFolder) then
      LWebView3.SetVirtualHostNameToFolderMapping(MDViewerScriptsHost,
        PChar(LFolder), COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND_ALLOW);
    FMappedScriptsFolder := LFolder;
  end;
end;

function TCustomEdgeMarkdownViewer.ScriptsURL: string;
begin
  if GetEffectiveScriptsFolder <> '' then
    Result := 'https://' + MDViewerScriptsHost + '/'
  else
    Result := '';
end;

function TCustomEdgeMarkdownViewer.PageStyle: string;
begin
  //The base style: the stylesheet of HtmlContent (CssStyle) comes after it.
  //color-scheme: scrollbars and form controls as the background of the page
  //(else they follow the theme of Windows: a dark scrollbar on a light page)
  Result :=
    '<style type="text/css">' + sLineBreak +
    Format(':root{color-scheme:%s;}', [IfThen(IsDarkBackground, 'dark', 'light')]) + sLineBreak +
    Format('body{font-family:"%s",sans-serif;font-size:%dpt;color:%s;background-color:%s;}',
      [FDefFontName, FDefFontSize, ColorToHTML(FDefFontColor), ColorToHTML(FDefBackground)]) + sLineBreak +
    Format('a{color:%s;}', [ColorToHTML(FDefHotSpotColor)]) + sLineBreak +
    //the styles of alerts, mermaid, math... are in CssStyle (MarkdownBaseCSS):
    //here only the refinements HTMLViewer would not understand
    '.markdown-alert>:last-child{margin-bottom:0;}' + sLineBreak +
    '.markdown-alert-title{margin-top:0;}' + sLineBreak +
    //printed (and PDF) pages: dark text on white paper, also with a dark theme
    '@media print{body{color:#000;background-color:#fff;}}' + sLineBreak +
    '</style>' + sLineBreak;
end;

function TCustomEdgeMarkdownViewer.PageScripts(const AHTML: string;
  const AScrollY: Integer): string;
var
  LKaTeX, LMermaid: string;
begin
  Result := '';
  //the exported page is opened outside the viewer, where the virtual host of
  //the scripts does not exist: always from CDN
  if (GetEffectiveScriptsFolder <> '') and not FExporting then
  begin
    LKaTeX := ScriptsURL + 'katex/';
    LMermaid := ScriptsURL + 'mermaid/';
  end
  else
  begin
    LKaTeX := KaTeXCDN;
    LMermaid := MermaidCDN;
  end;
  //KaTeX typesets \( \) and \[ \] (mmrMarkup): only when there are formulas
  if Pos('class="math"', AHTML) > 0 then
    Result := Result +
      '<link rel="stylesheet" href="' + LKaTeX + 'katex.min.css">' + sLineBreak +
      '<script defer src="' + LKaTeX + 'katex.min.js"></script>' + sLineBreak +
      '<script defer src="' + LKaTeX + 'contrib/auto-render.min.js"' +
      ' onload="renderMathInElement(document.body);"></script>' + sLineBreak;
  //mermaid.js draws the <pre class="mermaid"> diagrams
  if Pos('class="mermaid"', AHTML) > 0 then
    Result := Result +
      '<script src="' + LMermaid + 'mermaid.min.js"></script>' + sLineBreak +
      '<script>document.addEventListener("DOMContentLoaded",function(){' +
      'mermaid.initialize({startOnLoad:false});mermaid.run();});</script>' + sLineBreak;
  //Links to an anchor of the page (the <base> would navigate away), position
  //of the page (to keep it when the content is shown again)
  Result := Result +
    '<script>' + sLineBreak +
    'document.addEventListener("click",function(e){var a=e.target.closest("a");' +
    'if(a){var h=a.getAttribute("href");if(h&&h.charAt(0)==="#"){e.preventDefault();' +
    'var t=document.getElementById(decodeURIComponent(h.substring(1)));' +
    'if(t)t.scrollIntoView();}}});' + sLineBreak +
    'window.addEventListener("scroll",function(){if(window.chrome&&chrome.webview)' +
    'chrome.webview.postMessage("scrollY:"+Math.round(window.scrollY)+":"+' +
    'Math.max(0,Math.round(document.documentElement.scrollHeight-window.innerHeight)));});' + sLineBreak;
  if AScrollY > 0 then
    Result := Result +
      Format('window.addEventListener("load",function(){window.scrollTo(0,%d);});', [AScrollY]) + sLineBreak;
  Result := Result + '</script>' + sLineBreak;
end;

function TCustomEdgeMarkdownViewer.BuildPage(const AScrollY: Integer): string;
var
  LHTML, LBase: string;
begin
  LHTML := FEngine.HtmlContent.Text;
  //the exported page keeps the relative links (no virtual host)
  if (GetContentFolder <> '') and not FExporting then
    LBase := '<base href="https://' + MDViewerContentHost + '/">' + sLineBreak
  else
    LBase := '';
  Result :=
    '<!DOCTYPE html>' + sLineBreak +
    '<html>' + sLineBreak +
    '<head>' + sLineBreak +
    '<meta charset="utf-8">' + sLineBreak +
    LBase +
    PageStyle +
    PageScripts(LHTML, AScrollY) +
    '</head>' + sLineBreak +
    '<body>' + sLineBreak +
    LHTML +
    '</body>' + sLineBreak +
    '</html>' + sLineBreak;
end;

procedure TCustomEdgeMarkdownViewer.RefreshViewer(const APreservePosition: Boolean);
var
  LScrollY: Integer;
begin
  if not WebViewCreated then
  begin
    //Shown when the WebView is ready (WebViewCreateCompleted)
    FPendingRefresh := True;
    EnsureWebView;
    Exit;
  end;
  FPendingRefresh := False;
  if APreservePosition then
    LScrollY := FScrollY
  else
    LScrollY := 0;
  FScrollY := LScrollY;
  //the scroll that restores the position is not a user scroll
  if LScrollY > 0 then
    FExpectedScrollY := LScrollY;
  UpdateFolderMappings;
  UpdateColorScheme;
  NavigateToString(BuildPage(LScrollY));
end;

procedure TCustomEdgeMarkdownViewer.HTMLContentChanged(Sender: TObject);
begin
  //From the top for a new content, else at the same position
  RefreshViewer(not FResetPosition);
end;

procedure TCustomEdgeMarkdownViewer.WebViewNavigationStarting(
  Sender: TCustomEdgeBrowser; Args: TNavigationStartingEventArgs);
var
  LURI: PWideChar;
  LURIString: string;
begin
  LURI := nil;
  Args.ArgsInterface.Get_uri(LURI);
  LURIString := TakeWebViewString(LURI);
  //The page of the viewer (NavigateToString)
  if LURIString.StartsWith('data:', True) or LURIString.StartsWith('about:', True) then
    Exit;
  //A clicked link: handled by the viewer
  Args.ArgsInterface.Set_Cancel(1);
  LinkClicked(LURIString);
end;

procedure TCustomEdgeMarkdownViewer.WebViewNewWindowRequested(
  Sender: TCustomEdgeBrowser; Args: TNewWindowRequestedEventArgs);
var
  LURI: PWideChar;
begin
  //target="_blank": no new browser window
  LURI := nil;
  Args.ArgsInterface.Get_uri(LURI);
  Args.ArgsInterface.Set_Handled(1);
  LinkClicked(TakeWebViewString(LURI));
end;

procedure TCustomEdgeMarkdownViewer.WebViewNavigationCompleted(
  Sender: TCustomEdgeBrowser; IsSuccess: Boolean;
  WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS);
begin
  if IsSuccess and Assigned(FOnContentLoaded) then
    FOnContentLoaded(Self);
end;

procedure TCustomEdgeMarkdownViewer.WebViewMessageReceived(
  Sender: TCustomEdgeBrowser; Args: TWebMessageReceivedEventArgs);
var
  LMessage: PWideChar;
  LText: string;
  LParts: TArray<string>;
begin
  LMessage := nil;
  if Succeeded(Args.ArgsInterface.TryGetWebMessageAsString(LMessage)) then
  begin
    LText := TakeWebViewString(LMessage);
    if LText.StartsWith('scrollY:') then
    begin
      LParts := Copy(LText, 9, MaxInt).Split([':']);
      if Length(LParts) > 0 then
        FScrollY := StrToIntDef(LParts[0], FScrollY);
      if Length(LParts) > 1 then
        FScrollMax := StrToIntDef(LParts[1], FScrollMax);
      if FExpectedScrollY >= 0 then
      begin
        //the scroll requested by ScrollToRatio
        if Abs(FScrollY - FExpectedScrollY) <= 2 then
        begin
          FExpectedScrollY := -1;
          Exit;
        end;
        FExpectedScrollY := -1;
      end;
      if Assigned(FOnScrollChanged) then
        FOnScrollChanged(Self);
    end;
  end;
end;

function TCustomEdgeMarkdownViewer.LinkClicked(const AURI: string): Boolean;
var
  LPrefix, LRelative: string;
  LHashPos: Integer;
begin
  LPrefix := 'https://' + MDViewerContentHost + '/';
  if AURI.StartsWith(LPrefix, True) then
  begin
    //A file of the folder of the document
    LRelative := Copy(AURI, Length(LPrefix) + 1, MaxInt);
    LHashPos := Pos('#', LRelative);
    if LHashPos > 0 then
      LRelative := Copy(LRelative, 1, LHashPos - 1);
    LHashPos := Pos('?', LRelative);
    if LHashPos > 0 then
      LRelative := Copy(LRelative, 1, LHashPos - 1);
    LRelative := StringReplace(TNetEncoding.URL.Decode(LRelative), '/', '\', [rfReplaceAll]);
    Result := HandleLinkClicked(GetContentFolder, LRelative, FAutoLoadOnHotSpotClick,
      FOnFileNameClicked, FOnURLClicked, LoadFromFile);
  end
  else
    Result := HandleLinkClicked(GetContentFolder, AURI, FAutoLoadOnHotSpotClick,
      FOnFileNameClicked, FOnURLClicked, LoadFromFile);
end;

function TCustomEdgeMarkdownViewer.IsDarkBackground: Boolean;
var
  LBackground: TColor;
begin
  LBackground := ColorToRGB(FDefBackground);
  Result := (GetRValue(LBackground) * 299 + GetGValue(LBackground) * 587 +
    GetBValue(LBackground) * 114) div 1000 < 128;
end;

procedure TCustomEdgeMarkdownViewer.UpdateColorScheme;
var
  LWebView13: ICoreWebView2_13;
  LProfile: ICoreWebView2Profile;
begin
  //Context menu and prefers-color-scheme as the theme of the viewer, not as
  //the theme of Windows
  if Supports(DefaultInterface, ICoreWebView2_13, LWebView13) and
    Succeeded(LWebView13.Get_Profile(LProfile)) and Assigned(LProfile) then
  begin
    if IsDarkBackground then
      LProfile.Set_PreferredColorScheme(COREWEBVIEW2_PREFERRED_COLOR_SCHEME_DARK)
    else
      LProfile.Set_PreferredColorScheme(COREWEBVIEW2_PREFERRED_COLOR_SCHEME_LIGHT);
  end;
end;

procedure TCustomEdgeMarkdownViewer.EngineBeforeProcess(Sender: TObject);
begin
  //Syntax highlighting of the code blocks with the colors of the viewer
  if FCodeHighlightEmitter <> nil then
    FCodeHighlightEmitter.SetTheme(IsDarkBackground, FDefBackground, FDefFontColor,
      FDefFontName, FDefFontSize);
end;

procedure TCustomEdgeMarkdownViewer.LoadFromFile(const AFileName: TFileName);
begin
  FBaseFolder := ExtractFilePath(ExpandFileName(AFileName));
  LoadFromString(TryLoadTextFile(AFileName), IsHTMLFileName(AFileName));
  FDocumentFileName := ExpandFileName(AFileName);
end;

procedure TCustomEdgeMarkdownViewer.LoadFromStream(const AStream: TStringStream;
  const IsHTMLContent: Boolean);
begin
  LoadFromString(AStream.DataString, IsHTMLContent);
end;

procedure TCustomEdgeMarkdownViewer.LoadFromString(const AValue: string;
  const IsHTMLContent: Boolean);
begin
  //A new content: shown once (HTMLContentChanged), from the top
  FResetPosition := True;
  try
    FEngine.LoadFromString(AValue, IsHTMLContent);
  finally
    FResetPosition := False;
  end;
end;

procedure TCustomEdgeMarkdownViewer.ExportToFileHTML(const AFileName: TFileName);
begin
  FEngine.ExportToFileHTML(AFileName);
end;

procedure TCustomEdgeMarkdownViewer.RegisterSaveAsHandler;
var
{$IF CompilerVersion >= 37}
  LWebView25: ICoreWebView2_25;
{$IFEND}
  LWebView11: ICoreWebView2_11;
  LToken: EventRegistrationToken;
begin
  //The page is a data: URL (NavigateToString), with links to the virtual
  //hosts: the "Save as" of the browser would propose that URL as file name
  //and would save a page that does not work outside the viewer
{$IF CompilerVersion >= 37}
  if Supports(DefaultInterface, ICoreWebView2_25, LWebView25) then
  begin
    //The full page (ExportToFileHTMLPage) instead of the save of the browser
    LWebView25.add_SaveAsUIShowing(
      Callback<ICoreWebView2, ICoreWebView2SaveAsUIShowingEventArgs>.CreateAs<ICoreWebView2SaveAsUIShowingEventHandler>(
        function(const Sender: ICoreWebView2; const Args: ICoreWebView2SaveAsUIShowingEventArgs): HResult stdcall
        begin
          Args.Set_Cancel(1);
          //the dialog after the event of the browser
          TThread.ForceQueue(nil,
            procedure
            begin
              DoSaveAs;
            end);
          Result := S_OK;
        end), LToken);
    Exit;
  end;
{$IFEND}
  //WebView2 without the event: no "Save as" in the context menu
  if Supports(DefaultInterface, ICoreWebView2_11, LWebView11) then
    LWebView11.add_ContextMenuRequested(
      Callback<ICoreWebView2, ICoreWebView2ContextMenuRequestedEventArgs>.CreateAs<ICoreWebView2ContextMenuRequestedEventHandler>(
        function(const Sender: ICoreWebView2; const Args: ICoreWebView2ContextMenuRequestedEventArgs): HResult stdcall
        var
          LItems: ICoreWebView2ContextMenuItemCollection;
          LItem: ICoreWebView2ContextMenuItem;
          LCount, I: SYSUINT;
          LName: PWideChar;
          LIsSaveAs: Boolean;
        begin
          Result := S_OK;
          if Succeeded(Args.Get_MenuItems(LItems)) and Succeeded(LItems.Get_Count(LCount)) then
            for I := LCount downto 1 do
              if Succeeded(LItems.GetValueAtIndex(I - 1, LItem)) and
                Succeeded(LItem.Get_Name(LName)) then
              begin
                LIsSaveAs := SameText(LName, 'saveAs');
                CoTaskMemFree(LName);
                if LIsSaveAs then
                  LItems.RemoveValueAtIndex(I - 1);
              end;
        end), LToken);
end;

procedure TCustomEdgeMarkdownViewer.DoSaveAs;
begin
  if Assigned(FOnSaveAs) then
    FOnSaveAs(Self)
  else
    SaveAsHTMLPage;
end;

procedure TCustomEdgeMarkdownViewer.SaveAsHTMLPage;
var
  LDialog: TSaveDialog;
begin
  LDialog := TSaveDialog.Create(nil);
  try
    LDialog.Filter := 'HTML (*.html;*.htm)|*.html;*.htm';
    LDialog.DefaultExt := 'html';
    LDialog.Options := LDialog.Options + [ofOverwritePrompt, ofPathMustExist];
    if FDocumentFileName <> '' then
    begin
      LDialog.InitialDir := ExtractFilePath(FDocumentFileName);
      LDialog.FileName := ChangeFileExt(ExtractFileName(FDocumentFileName), '.html');
    end
    else
      LDialog.InitialDir := GetContentFolder;
    if LDialog.Execute then
      ExportToFileHTMLPage(LDialog.FileName);
  finally
    LDialog.Free;
  end;
end;

procedure TCustomEdgeMarkdownViewer.ExportToFileHTMLPage(const AFileName: TFileName);
begin
  FExporting := True;
  try
    SaveUTF8File(AFileName, BuildPage(0));
  finally
    FExporting := False;
  end;
end;

function TCustomEdgeMarkdownViewer.HasFocus: Boolean;
var
  LFocus: HWND;
begin
  LFocus := GetFocus;
  Result := HandleAllocated and (LFocus <> 0) and
    ((LFocus = Handle) or IsChild(Handle, LFocus));
end;

function TCustomEdgeMarkdownViewer.GetScrollRatio: Double;
begin
  if FScrollMax > 0 then
    Result := FScrollY / FScrollMax
  else
    Result := 0;
end;

procedure TCustomEdgeMarkdownViewer.ScrollToRatio(const ARatio: Double);
var
  LRatio: Double;
begin
  LRatio := ARatio;
  if LRatio < 0 then
    LRatio := 0
  else if LRatio > 1 then
    LRatio := 1;
  //the scroll message that follows is not a user scroll
  FExpectedScrollY := Round(LRatio * FScrollMax);
  FScrollY := FExpectedScrollY;
  if WebViewCreated then
    ExecuteScript(Format('window.scrollTo(0,Math.round(%s*Math.max(0,' +
      'document.documentElement.scrollHeight-window.innerHeight)));',
      [FloatToStr(LRatio, TFormatSettings.Invariant)]));
end;

function TCustomEdgeMarkdownViewer.ExportToPDF(const AFileName: TFileName): Boolean;
begin
  Result := WebViewCreated and PrintToPDF(AFileName, nil);
end;

function TCustomEdgeMarkdownViewer.TransformContent(const AMarkdownContent: string;
  AProcessorDialect: TMarkdownProcessorDialect; const ACssStyle: string;
  const AAllowUnsafe: Boolean): string;
begin
  EngineBeforeProcess(Self);
  Result := FEngine.TransformContent(AMarkdownContent, AProcessorDialect,
    ACssStyle, AAllowUnsafe);
end;

procedure TCustomEdgeMarkdownViewer.CopyToClipboard;
begin
  if WebViewCreated then
    ExecuteScript('document.execCommand("copy");');
end;

procedure TCustomEdgeMarkdownViewer.SelectAll;
begin
  if WebViewCreated then
    ExecuteScript('document.execCommand("selectAll");');
end;

procedure TCustomEdgeMarkdownViewer.AutoLoadFile;
var
  LFileName: TFileName;
begin
  if ResolveHelpFile(FServerRoot, HelpType, HelpKeyword, HelpContext,
    FindHelpFile, LFileName) then
    LoadFromFile(LFileName);
end;

function TCustomEdgeMarkdownViewer.FindHelpFile(var AFileName: TFileName;
  const AContext: Integer; const HelpKeyword: string): Boolean;
begin
  Result := FindMarkdownHelpFile(AFileName, AContext, HelpKeyword);
end;

function TCustomEdgeMarkdownViewer.GetHelpContext: THelpContext;
begin
  Result := inherited HelpContext;
end;

function TCustomEdgeMarkdownViewer.GetHelpKeyword: String;
begin
  Result := inherited HelpKeyword;
end;

procedure TCustomEdgeMarkdownViewer.SetHelpContext(const AValue: THelpContext);
begin
  if AValue <> HelpContext then
  begin
    inherited HelpContext := AValue;
    AutoLoadFile;
  end;
end;

procedure TCustomEdgeMarkdownViewer.SetHelpKeyword(const AValue: String);
begin
  if AValue <> HelpKeyword then
  begin
    inherited HelpKeyword := AValue;
    AutoLoadFile;
  end;
end;

function TCustomEdgeMarkdownViewer.IsHelpContextStored: Boolean;
begin
  Result := ((ActionLink = nil) or not THookControlActionLink(ActionLink).IsHelpContextLinked)
    and (HelpContext <> 0);
end;

function TCustomEdgeMarkdownViewer.IsHelpKeywordStored: Boolean;
begin
  Result := ((ActionLink = nil) or not THookControlActionLink(ActionLink).IsHelpContextLinked)
    and (HelpKeyword <> '');
end;

function TCustomEdgeMarkdownViewer.IsCssStyleStored: Boolean;
begin
  Result := not SameText(
    StringReplace(FEngine.CssStyle.Text, sLineBreak, '', [rfReplaceAll]),
    StringReplace(GetMarkdownDefaultCSS, sLineBreak, '', [rfReplaceAll]));
end;

function TCustomEdgeMarkdownViewer.IsExtensionsStored: Boolean;
begin
  Result := FEngine.Extensions <> TMarkdownViewerEngine.DefaultExtensions(FEngine.ProcessorDialect);
end;

function TCustomEdgeMarkdownViewer.IsHtmlContentStored: Boolean;
begin
  Result := (FEngine.HtmlContent.Text <> '') and (FEngine.MarkdownContent.Text = '');
end;

function TCustomEdgeMarkdownViewer.IsDefFontNameStored: Boolean;
begin
  Result := FDefFontName <> DefaultFontName;
end;

function TCustomEdgeMarkdownViewer.GetAllowUnsafe: Boolean;
begin
  Result := FEngine.AllowUnsafe;
end;

function TCustomEdgeMarkdownViewer.GetCssStyle: TStringList;
begin
  Result := FEngine.CssStyle;
end;

function TCustomEdgeMarkdownViewer.GetExtensions: TMarkdownExtensions;
begin
  Result := FEngine.Extensions;
end;

function TCustomEdgeMarkdownViewer.GetHTMLContent: TStringList;
begin
  Result := FEngine.HtmlContent;
end;

function TCustomEdgeMarkdownViewer.GetMarkdownContent: TStringList;
begin
  Result := FEngine.MarkdownContent;
end;

function TCustomEdgeMarkdownViewer.GetMathRendering: TMarkdownMathRendering;
begin
  Result := FEngine.MathRendering;
end;

function TCustomEdgeMarkdownViewer.GetProcessorDialect: TMarkdownProcessorDialect;
begin
  Result := FEngine.ProcessorDialect;
end;

procedure TCustomEdgeMarkdownViewer.SetAllowUnsafe(const AValue: Boolean);
begin
  FEngine.AllowUnsafe := AValue;
end;

procedure TCustomEdgeMarkdownViewer.SetCssStyle(const AValue: TStringList);
begin
  FEngine.CssStyle := AValue;
end;

procedure TCustomEdgeMarkdownViewer.SetExtensions(const AValue: TMarkdownExtensions);
begin
  FEngine.Extensions := AValue;
end;

procedure TCustomEdgeMarkdownViewer.SetHTMLContent(const AValue: TStringList);
begin
  if FEngine.HtmlContent.Text <> AValue.Text then
    LoadFromString(AValue.Text, True);
end;

procedure TCustomEdgeMarkdownViewer.SetMarkdownContent(const AValue: TStringList);
begin
  if FEngine.MarkdownContent.Text <> AValue.Text then
    LoadFromString(AValue.Text, False);
end;

procedure TCustomEdgeMarkdownViewer.SetMathRendering(const AValue: TMarkdownMathRendering);
begin
  FEngine.MathRendering := AValue;
end;

procedure TCustomEdgeMarkdownViewer.SetProcessorDialect(
  const AValue: TMarkdownProcessorDialect);
begin
  FEngine.ProcessorDialect := AValue;
end;

procedure TCustomEdgeMarkdownViewer.SetFileName(const AValue: TFileName);
begin
  if FFileName <> AValue then
  begin
    FFileName := AValue;
    if (AValue <> '') and FileExists(AValue) then
      LoadFromFile(AValue);
  end;
end;

procedure TCustomEdgeMarkdownViewer.SetServerRoot(const AValue: TFolderName);
begin
  if FServerRoot <> AValue then
  begin
    FServerRoot := AValue;
    AutoLoadFile;
  end;
end;

class function TCustomEdgeMarkdownViewer.FindScriptsFolder: string;
var
  LExeFolder, LFolder: string;
begin
  //Searched once: the folder of the executable does not change
  if not _ScriptsFolderSearched then
  begin
    _ScriptsFolderSearched := True;
    _FoundScriptsFolder := '';
    LExeFolder := ExtractFilePath(ParamStr(0));
    for LFolder in [LExeFolder + MDViewerScriptsFolderName,
      LExeFolder + '..\' + MDViewerScriptsFolderName] do
      if FileExists(IncludeTrailingPathDelimiter(LFolder) + 'katex\katex.min.js') and
        FileExists(IncludeTrailingPathDelimiter(LFolder) + 'mermaid\mermaid.min.js') then
      begin
        _FoundScriptsFolder := ExcludeTrailingPathDelimiter(ExpandFileName(LFolder));
        Break;
      end;
  end;
  Result := _FoundScriptsFolder;
end;

function TCustomEdgeMarkdownViewer.GetEffectiveScriptsFolder: string;
begin
  if FScriptsFolder <> '' then
    Result := FScriptsFolder
  else if csDesigning in ComponentState then
    Result := ''
  else
    Result := FindScriptsFolder;
end;

procedure TCustomEdgeMarkdownViewer.SetScriptsFolder(const AValue: TFolderName);
begin
  if FScriptsFolder <> AValue then
  begin
    FScriptsFolder := AValue;
    if FEngine.HtmlContent.Text <> '' then
      RefreshViewer;
  end;
end;

procedure TCustomEdgeMarkdownViewer.SetDefBackground(const AValue: TColor);
begin
  if FDefBackground <> AValue then
  begin
    FDefBackground := AValue;
    //code highlighting with the new colors (the new HTML refreshes the viewer)
    if FEngine.MarkdownContent.Text <> '' then
      FEngine.UpdateHTML
    else
      RefreshViewer;
  end;
end;

procedure TCustomEdgeMarkdownViewer.SetDefFontColor(const AValue: TColor);
begin
  if FDefFontColor <> AValue then
  begin
    FDefFontColor := AValue;
    RefreshViewer;
  end;
end;

procedure TCustomEdgeMarkdownViewer.SetDefFontName(const AValue: TFontName);
begin
  if FDefFontName <> AValue then
  begin
    FDefFontName := AValue;
    RefreshViewer;
  end;
end;

procedure TCustomEdgeMarkdownViewer.SetDefFontSize(const AValue: Integer);
begin
  if FDefFontSize <> AValue then
  begin
    FDefFontSize := AValue;
    RefreshViewer;
  end;
end;

procedure TCustomEdgeMarkdownViewer.SetDefHotSpotColor(const AValue: TColor);
begin
  if FDefHotSpotColor <> AValue then
  begin
    FDefHotSpotColor := AValue;
    RefreshViewer;
  end;
end;

end.
