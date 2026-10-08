{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       CommonMark / GFM engine: HTML renderer                                 }
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
{  The output format follows the HTML renderer of commonmark.js (BSD-2-Clause, }
{  Copyright (c) 2014 John MacFarlane): see docs/LICENSE-commonmark.js.txt.    }
{                                                                              }
{******************************************************************************}
unit MarkdownHtmlRenderer;

{ Renders a CommonMark/GFM syntax tree as an HTML fragment, in the exact form
  of the specification examples. Every node kind has a virtual method, so a
  descendant can change the markup of single elements.

  Safe mode (AllowUnsafe = False): raw HTML blocks and inlines are replaced by
  "<!-- raw HTML omitted -->", and javascript:, vbscript:, file: and data:
  destinations (except data:image/png|gif|jpeg|webp) are emptied. }

interface

uses
  System.SysUtils,
  System.Classes,
  MarkdownUtils,
  MarkdownAST;

type
  TMarkdownHtmlRenderer = class
  private
    FOut: TStringBuilder;
    FLastOut: Char;
    FDisableTags: Integer;
    FAllowUnsafe: Boolean;
    FSoftBreak: string;
    FTagFilter: Boolean;
    FWalker: TMarkdownWalker;
    FSpecialLinkEmitter: TSpanEmitter;
    FCodeBlockEmitter: TBlockEmitter;
    FMermaid: Boolean;
    FMathRendering: TMarkdownMathRendering;
  protected
    /// <summary>Writes raw text.</summary>
    procedure Lit(const S: string);
    /// <summary>Writes S HTML-escaped.</summary>
    procedure Esc(const S: string);
    /// <summary>Writes a line ending unless the output already ends with one.</summary>
    procedure Cr;
    /// <summary>Writes a tag unless tags are disabled (image alt text).</summary>
    procedure Tag(const S: string);
    function SafeUrl(const Url: string): string; virtual;
    function TagsDisabled: Boolean;

    procedure RenderNode(Node: TMarkdownNode; Entering: Boolean); virtual;
    // blocks
    procedure RenderDocument(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderBlockQuote(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderList(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderListItem(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderParagraph(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderHeading(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderThematicBreak(Node: TMarkdownNode); virtual;
    procedure RenderCodeBlock(Node: TMarkdownNode); virtual;
    procedure RenderHtmlBlock(Node: TMarkdownNode); virtual;
    // inlines
    procedure RenderText(Node: TMarkdownNode); virtual;
    procedure RenderSoftBreak(Node: TMarkdownNode); virtual;
    procedure RenderHardBreak(Node: TMarkdownNode); virtual;
    procedure RenderCode(Node: TMarkdownNode); virtual;
    procedure RenderEmphasis(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderStrong(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderLink(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderImage(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderHtmlInline(Node: TMarkdownNode); virtual;
    // GFM
    procedure RenderTable(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderTableSection(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderTableRow(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderTableCell(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderStrikethrough(Node: TMarkdownNode; Entering: Boolean); virtual;
    procedure RenderTaskMarker(Node: TMarkdownNode); virtual;
    // math (KaTeX/MathJax ready)
    procedure RenderMathBlock(Node: TMarkdownNode); virtual;
    procedure RenderMathInline(Node: TMarkdownNode); virtual;
    /// <summary>Math as an image of latex.codecogs.com (mmrCodeCogsImage).</summary>
    function MathImage(const Latex: string): string; virtual;
    procedure RenderMermaid(Node: TMarkdownNode); virtual;
    // GitHub alerts
    procedure RenderAlert(Node: TMarkdownNode; Entering: Boolean); virtual;
    // legacy extensions
    procedure RenderSimpleInline(Node: TMarkdownNode; Entering: Boolean; const TagName: string); virtual;
    procedure RenderWikiLink(Node: TMarkdownNode); virtual;
    /// <summary>Renders the children of Node into a string and skips them in
    /// the main walk.</summary>
    function RenderChildren(Node: TMarkdownNode): string;
    /// <summary>Raw HTML as emitted in unsafe mode (tag filter applied).</summary>
    function RawHtml(const Html: string): string; virtual;
    /// <summary>Kinds not handled above (extensions).</summary>
    procedure RenderOther(Node: TMarkdownNode; Entering: Boolean); virtual;

    property Output: TStringBuilder read FOut;
  public
    constructor Create;
    function Render(Document: TMarkdownNode): string;
    /// <summary>True: raw HTML and every URL are emitted as they are.</summary>
    property AllowUnsafe: Boolean read FAllowUnsafe write FAllowUnsafe;
    /// <summary>Output for a soft line break (default: line feed).</summary>
    property SoftBreak: string read FSoftBreak write FSoftBreak;
    /// <summary>GFM "disallowed raw HTML": in unsafe mode the tags title,
    /// textarea, style, xmp, iframe, noembed, noframes, script, plaintext are
    /// neutralized (their "&lt;" becomes "&amp;lt;").</summary>
    property TagFilter: Boolean read FTagFilter write FTagFilter;
    /// <summary>Receives the rendered content of [[...]] (mexWikiLinks); when
    /// unassigned the brackets are written as text. Not owned.</summary>
    property SpecialLinkEmitter: TSpanEmitter read FSpecialLinkEmitter write FSpecialLinkEmitter;
    /// <summary>When assigned, receives every code block (fenced and indented,
    /// mermaid and chart included): the lines without line endings and the
    /// whole info string as meta; it writes the HTML of the block. Not owned.</summary>
    property CodeBlockEmitter: TBlockEmitter read FCodeBlockEmitter write FCodeBlockEmitter;
    /// <summary>```mermaid fences as &lt;pre class="mermaid"&gt; (mexMermaid).</summary>
    property Mermaid: Boolean read FMermaid write FMermaid;
    property MathRendering: TMarkdownMathRendering read FMathRendering write FMathRendering;
  end;

/// <summary>True for javascript:, vbscript:, file: and non-image data: URLs.</summary>
function IsDangerousUrl(const Url: string): Boolean;

implementation

uses
  MarkdownTextUtils,
  MarkdownGFM,
  MarkdownAlerts;

const
  OmittedHtml = '<!-- raw HTML omitted -->';

function IsDangerousUrl(const Url: string): Boolean;
const
  MaxProbe = 32;
var
  Probe: string;
  I: Integer;
begin
  // Browsers ignore control characters and spaces in a scheme: drop them all,
  // so that tricks like "java&#9;script:" fail closed.
  Probe := '';
  for I := 1 to Length(Url) do
  begin
    if Length(Probe) >= MaxProbe then
      Break;
    if Url[I] > ' ' then
      Probe := Probe + Url[I];
  end;
  Probe := LowerCase(Probe);
  if StartsAt(Probe, 1, 'javascript:') or StartsAt(Probe, 1, 'vbscript:') or
     StartsAt(Probe, 1, 'file:') then
    Exit(True);
  if not StartsAt(Probe, 1, 'data:') then
    Exit(False);
  Result := not (StartsAt(Probe, 1, 'data:image/png') or StartsAt(Probe, 1, 'data:image/gif') or
    StartsAt(Probe, 1, 'data:image/jpeg') or StartsAt(Probe, 1, 'data:image/webp'));
end;

{ TMarkdownHtmlRenderer }

constructor TMarkdownHtmlRenderer.Create;
begin
  inherited Create;
  FSoftBreak := #10;
end;

function TMarkdownHtmlRenderer.Render(Document: TMarkdownNode): string;
var
  Node: TMarkdownNode;
  Entering: Boolean;
begin
  FOut := TStringBuilder.Create;
  try
    FLastOut := #10;
    FDisableTags := 0;
    FWalker.Init(Document);
    while FWalker.Next(Node, Entering) do
      RenderNode(Node, Entering);
    Result := FOut.ToString;
  finally
    FreeAndNil(FOut);
  end;
end;

procedure TMarkdownHtmlRenderer.Lit(const S: string);
begin
  if S = '' then
    Exit;
  FOut.Append(S);
  FLastOut := S[Length(S)];
end;

procedure TMarkdownHtmlRenderer.Esc(const S: string);
begin
  Lit(EscapeHtml(S));
end;

procedure TMarkdownHtmlRenderer.Cr;
begin
  if FLastOut <> #10 then
    Lit(#10);
end;

procedure TMarkdownHtmlRenderer.Tag(const S: string);
begin
  if FDisableTags > 0 then
    Exit;
  Lit(S);
end;

function TMarkdownHtmlRenderer.TagsDisabled: Boolean;
begin
  Result := FDisableTags > 0;
end;

function TMarkdownHtmlRenderer.SafeUrl(const Url: string): string;
begin
  if not FAllowUnsafe and IsDangerousUrl(Url) then
    Result := ''
  else
    Result := Url;
end;

procedure TMarkdownHtmlRenderer.RenderNode(Node: TMarkdownNode; Entering: Boolean);
begin
  case Node.Kind of
    nkDocument: RenderDocument(Node, Entering);
    nkBlockQuote: RenderBlockQuote(Node, Entering);
    nkList: RenderList(Node, Entering);
    nkListItem: RenderListItem(Node, Entering);
    nkParagraph: RenderParagraph(Node, Entering);
    nkHeading: RenderHeading(Node, Entering);
    nkThematicBreak: RenderThematicBreak(Node);
    nkCodeBlock: RenderCodeBlock(Node);
    nkHtmlBlock: RenderHtmlBlock(Node);
    nkText: RenderText(Node);
    nkSoftBreak: RenderSoftBreak(Node);
    nkHardBreak: RenderHardBreak(Node);
    nkCode: RenderCode(Node);
    nkEmphasis: RenderEmphasis(Node, Entering);
    nkStrong: RenderStrong(Node, Entering);
    nkLink: RenderLink(Node, Entering);
    nkImage: RenderImage(Node, Entering);
    nkHtmlInline: RenderHtmlInline(Node);
    nkTable: RenderTable(Node, Entering);
    nkTableHead, nkTableBody: RenderTableSection(Node, Entering);
    nkTableRow: RenderTableRow(Node, Entering);
    nkTableCell: RenderTableCell(Node, Entering);
    nkStrikethrough: RenderStrikethrough(Node, Entering);
    nkTaskMarker: RenderTaskMarker(Node);
    nkMathBlock: RenderMathBlock(Node);
    nkMathInline: RenderMathInline(Node);
    nkAlert: RenderAlert(Node, Entering);
    nkSubscript: RenderSimpleInline(Node, Entering, 'sub');
    nkSuperscript: RenderSimpleInline(Node, Entering, 'sup');
    nkInsert: RenderSimpleInline(Node, Entering, 'ins');
    nkMark: RenderSimpleInline(Node, Entering, 'mark');
    nkWikiLink:
      if Entering then
        RenderWikiLink(Node);
  else
    RenderOther(Node, Entering);
  end;
end;

procedure TMarkdownHtmlRenderer.RenderDocument(Node: TMarkdownNode; Entering: Boolean);
begin
end;

procedure TMarkdownHtmlRenderer.RenderBlockQuote(Node: TMarkdownNode; Entering: Boolean);
begin
  Cr;
  if Entering then
  begin
    Tag('<blockquote>');
    Cr;
  end
  else
  begin
    Tag('</blockquote>');
    Cr;
  end;
end;

procedure TMarkdownHtmlRenderer.RenderList(Node: TMarkdownNode; Entering: Boolean);
var
  TagName: string;
begin
  if Node.ListType = mltBullet then
    TagName := 'ul'
  else
    TagName := 'ol';
  Cr;
  if Entering then
  begin
    if (Node.ListType = mltOrdered) and (Node.ListStart <> 1) then
      Tag('<' + TagName + ' start="' + IntToStr(Node.ListStart) + '">')
    else
      Tag('<' + TagName + '>');
  end
  else
    Tag('</' + TagName + '>');
  Cr;
end;

procedure TMarkdownHtmlRenderer.RenderListItem(Node: TMarkdownNode; Entering: Boolean);
begin
  if Entering then
    Tag('<li>')
  else
  begin
    Tag('</li>');
    Cr;
  end;
end;

procedure TMarkdownHtmlRenderer.RenderParagraph(Node: TMarkdownNode; Entering: Boolean);
var
  GrandParent: TMarkdownNode;
begin
  // paragraphs of tight lists are rendered without <p>
  if Node.Parent <> nil then
  begin
    GrandParent := Node.Parent.Parent;
    if (GrandParent <> nil) and (GrandParent.Kind = nkList) and GrandParent.ListTight then
      Exit;
  end;
  if Entering then
  begin
    Cr;
    Tag('<p>');
  end
  else
  begin
    Tag('</p>');
    Cr;
  end;
end;

procedure TMarkdownHtmlRenderer.RenderHeading(Node: TMarkdownNode; Entering: Boolean);
begin
  if Entering then
  begin
    Cr;
    if Node.Id <> '' then
      Tag('<h' + IntToStr(Node.Level) + ' id="' + EscapeHtml(Node.Id) + '">')
    else
      Tag('<h' + IntToStr(Node.Level) + '>');
  end
  else
  begin
    Tag('</h' + IntToStr(Node.Level) + '>');
    Cr;
  end;
end;

procedure TMarkdownHtmlRenderer.RenderThematicBreak(Node: TMarkdownNode);
begin
  Cr;
  Tag('<hr />');
  Cr;
end;

procedure TMarkdownHtmlRenderer.RenderCodeBlock(Node: TMarkdownNode);
var
  Info: string;
  P, Start: Integer;
  Lines: TStringList;
  Literal: string;
begin
  if Assigned(FCodeBlockEmitter) then
  begin
    Lines := TStringList.Create;
    try
      Literal := Node.Literal;
      Start := 1;
      for P := 1 to Length(Literal) do
        if Literal[P] = #10 then
        begin
          Lines.Add(Copy(Literal, Start, P - Start));
          Start := P + 1;
        end;
      if Start <= Length(Literal) then
        Lines.Add(Copy(Literal, Start, MaxInt));
      Cr;
      FCodeBlockEmitter.emitBlock(FOut, Lines, Node.Info);
      if FOut.Length > 0 then
        FLastOut := FOut.Chars[FOut.Length - 1];
      Cr;
    finally
      Lines.Free;
    end;
    Exit;
  end;
  Info := Node.Info;
  // the language is the first word of the info string
  P := 1;
  while (P <= Length(Info)) and not CharInSet(Info[P], [' ', #9, #10, #13]) do
    Inc(P);
  Info := Copy(Info, 1, P - 1);
  if FMermaid and SameText(Info, 'mermaid') then
  begin
    RenderMermaid(Node);
    Exit;
  end;
  Cr;
  if Info <> '' then
    Tag('<pre><code class="language-' + EscapeHtml(Info) + '">')
  else
    Tag('<pre><code>');
  Esc(Node.Literal);
  Tag('</code></pre>');
  Cr;
end;

procedure TMarkdownHtmlRenderer.RenderHtmlBlock(Node: TMarkdownNode);
begin
  Cr;
  if FAllowUnsafe then
    Lit(RawHtml(Node.Literal))
  else
    Lit(OmittedHtml);
  Cr;
end;

procedure TMarkdownHtmlRenderer.RenderText(Node: TMarkdownNode);
begin
  Esc(Node.Literal);
end;

procedure TMarkdownHtmlRenderer.RenderSoftBreak(Node: TMarkdownNode);
begin
  Lit(FSoftBreak);
end;

procedure TMarkdownHtmlRenderer.RenderHardBreak(Node: TMarkdownNode);
begin
  Tag('<br />');
  Cr;
end;

procedure TMarkdownHtmlRenderer.RenderCode(Node: TMarkdownNode);
begin
  Tag('<code>');
  Esc(Node.Literal);
  Tag('</code>');
end;

procedure TMarkdownHtmlRenderer.RenderEmphasis(Node: TMarkdownNode; Entering: Boolean);
begin
  if Entering then
    Tag('<em>')
  else
    Tag('</em>');
end;

procedure TMarkdownHtmlRenderer.RenderStrong(Node: TMarkdownNode; Entering: Boolean);
begin
  if Entering then
    Tag('<strong>')
  else
    Tag('</strong>');
end;

procedure TMarkdownHtmlRenderer.RenderLink(Node: TMarkdownNode; Entering: Boolean);
begin
  if Entering then
  begin
    if TagsDisabled then
      Exit;
    Lit('<a href="' + EscapeHtml(SafeUrl(Node.Destination)) + '"');
    if Node.Title <> '' then
      Lit(' title="' + EscapeHtml(Node.Title) + '"');
    Lit('>');
  end
  else
    Tag('</a>');
end;

procedure TMarkdownHtmlRenderer.RenderImage(Node: TMarkdownNode; Entering: Boolean);
begin
  if Entering then
  begin
    if FDisableTags = 0 then
      Lit('<img src="' + EscapeHtml(SafeUrl(Node.Destination)) + '" alt="');
    Inc(FDisableTags);
  end
  else
  begin
    Dec(FDisableTags);
    if FDisableTags = 0 then
    begin
      if Node.Title <> '' then
        Lit('" title="' + EscapeHtml(Node.Title));
      Lit('" />');
    end;
  end;
end;

procedure TMarkdownHtmlRenderer.RenderHtmlInline(Node: TMarkdownNode);
begin
  if FAllowUnsafe then
    Lit(RawHtml(Node.Literal))
  else
    Lit(OmittedHtml);
end;

function TMarkdownHtmlRenderer.RawHtml(const Html: string): string;
begin
  if FTagFilter then
    Result := FilterDisallowedRawHtml(Html)
  else
    Result := Html;
end;

procedure TMarkdownHtmlRenderer.RenderTable(Node: TMarkdownNode; Entering: Boolean);
begin
  Cr;
  if Entering then
    Tag('<table>')
  else
    Tag('</table>');
  Cr;
end;

procedure TMarkdownHtmlRenderer.RenderTableSection(Node: TMarkdownNode; Entering: Boolean);
var
  TagName: string;
begin
  if Node.Kind = nkTableHead then
    TagName := 'thead'
  else
    TagName := 'tbody';
  Cr;
  if Entering then
    Tag('<' + TagName + '>')
  else
    Tag('</' + TagName + '>');
  Cr;
end;

procedure TMarkdownHtmlRenderer.RenderTableRow(Node: TMarkdownNode; Entering: Boolean);
begin
  Cr;
  if Entering then
    Tag('<tr>')
  else
    Tag('</tr>');
  Cr;
end;

procedure TMarkdownHtmlRenderer.RenderTableCell(Node: TMarkdownNode; Entering: Boolean);
var
  TagName: string;
begin
  if (Node.Parent <> nil) and (Node.Parent.Parent <> nil) and (Node.Parent.Parent.Kind = nkTableHead) then
    TagName := 'th'
  else
    TagName := 'td';
  if Entering then
  begin
    Cr;
    case Node.Align of
      mtaLeft: Tag('<' + TagName + ' align="left">');
      mtaCenter: Tag('<' + TagName + ' align="center">');
      mtaRight: Tag('<' + TagName + ' align="right">');
    else
      Tag('<' + TagName + '>');
    end;
  end
  else
  begin
    Tag('</' + TagName + '>');
    Cr;
  end;
end;

procedure TMarkdownHtmlRenderer.RenderStrikethrough(Node: TMarkdownNode; Entering: Boolean);
begin
  if Entering then
    Tag('<del>')
  else
    Tag('</del>');
end;

procedure TMarkdownHtmlRenderer.RenderTaskMarker(Node: TMarkdownNode);
begin
  if Node.Checked then
    Tag('<input checked="" disabled="" type="checkbox">')
  else
    Tag('<input disabled="" type="checkbox">');
end;

procedure TMarkdownHtmlRenderer.RenderOther(Node: TMarkdownNode; Entering: Boolean);
begin
  // extension nodes are rendered by the units that introduce them
end;


procedure TMarkdownHtmlRenderer.RenderMathBlock(Node: TMarkdownNode);
begin
  if FMathRendering = mmrCodeCogsImage then
  begin
    Cr;
    Tag('<div class="math" style="text-align:center;">');
    Tag(MathImage(Node.Literal));
    Tag('</div>');
    Cr;
    Exit;
  end;
  Cr;
  Tag('<div class="math">');
  Lit('\[' + #10);
  Esc(Node.Literal);
  Lit(#10'\]');
  Tag('</div>');
  Cr;
end;

procedure TMarkdownHtmlRenderer.RenderMathInline(Node: TMarkdownNode);
begin
  if FMathRendering = mmrCodeCogsImage then
  begin
    if TagsDisabled then
      Esc(Node.Literal) // inside the alt text of an image
    else
      Lit(MathImage(Node.Literal));
    Exit;
  end;
  Tag('<span class="math">');
  if Node.IsDisplay then
    Lit('\[')
  else
    Lit('\(');
  Esc(Node.Literal);
  if Node.IsDisplay then
    Lit('\]')
  else
    Lit('\)');
  Tag('</span>');
end;


procedure TMarkdownHtmlRenderer.RenderAlert(Node: TMarkdownNode; Entering: Boolean);
var
  CssName: string;
begin
  Cr;
  if Entering then
  begin
    CssName := LowerCase(Node.AlertType);
    Tag('<div class="markdown-alert markdown-alert-' + EscapeHtml(CssName) + '">');
    Cr;
    Tag('<p class="markdown-alert-title">' + EscapeHtml(AlertTitle(Node.AlertType)) + '</p>');
  end
  else
    Tag('</div>');
  Cr;
end;


procedure TMarkdownHtmlRenderer.RenderSimpleInline(Node: TMarkdownNode; Entering: Boolean;
  const TagName: string);
begin
  if Entering then
    Tag('<' + TagName + '>')
  else
    Tag('</' + TagName + '>');
end;

function TMarkdownHtmlRenderer.RenderChildren(Node: TMarkdownNode): string;
var
  SavedOut: TStringBuilder;
  SavedLast: Char;
  SavedWalker: TMarkdownWalker;
  Sibling, Visited: TMarkdownNode;
  Entering: Boolean;
begin
  SavedOut := FOut;
  SavedLast := FLastOut;
  SavedWalker := FWalker;
  FOut := TStringBuilder.Create;
  try
    Sibling := Node.FirstChild;
    while Sibling <> nil do
    begin
      FWalker.Init(Sibling);
      while FWalker.Next(Visited, Entering) do
        RenderNode(Visited, Entering);
      Sibling := Sibling.Next;
    end;
    Result := FOut.ToString;
  finally
    FOut.Free;
    FOut := SavedOut;
    FLastOut := SavedLast;
    FWalker := SavedWalker;
  end;
  // continue the main walk after the node
  FWalker.ResumeAt(Node, False);
end;

procedure TMarkdownHtmlRenderer.RenderWikiLink(Node: TMarkdownNode);
var
  Inner: string;
begin
  Inner := RenderChildren(Node);
  if Assigned(FSpecialLinkEmitter) then
  begin
    FSpecialLinkEmitter.emitSpan(FOut, Inner);
    if FOut.Length > 0 then
      FLastOut := FOut.Chars[FOut.Length - 1];
  end
  else
    Lit('[[' + Inner + ']]');
end;


function TMarkdownHtmlRenderer.MathImage(const Latex: string): string;
begin
  Result := '<img class="math" src="https://latex.codecogs.com/png.image?' +
    EscapeHtml(EncodeUrlComponent(Latex)) + '" alt="' + EscapeHtml(Latex) + '" />';
end;

procedure TMarkdownHtmlRenderer.RenderMermaid(Node: TMarkdownNode);
begin
  // the markup mermaid.js looks for; the page loads mermaid.js itself
  Cr;
  Tag('<pre class="mermaid">');
  Esc(Node.Literal);
  Tag('</pre>');
  Cr;
end;

end.
