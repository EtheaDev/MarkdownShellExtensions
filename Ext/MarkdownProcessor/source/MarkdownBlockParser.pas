{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       CommonMark / GFM engine: block structure parser                        }
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
{  The algorithm is the one of the CommonMark specification appendix "A        }
{  parsing strategy", as implemented by commonmark.js (BSD-2-Clause,           }
{  Copyright (c) 2014 John MacFarlane): see docs/LICENSE-commonmark.js.txt.    }
{                                                                              }
{******************************************************************************}
unit MarkdownBlockParser;

{ Phase 1 of the CommonMark parsing strategy: the input is consumed line by
  line, each line first matching the open container blocks (block quotes,
  list items...), then possibly opening new blocks, and finally adding its
  text to a leaf block (paragraph, code block, HTML block). Lazy paragraph
  continuation is supported. After all lines, phase 2 (inline parsing) is run
  on the content of paragraphs and headings. }

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  MarkdownUtils,
  MarkdownAST,
  MarkdownLinkRefs;

type
  /// <summary>Parses the inline content of a paragraph/heading node.</summary>
  TMarkdownInlineParserProc = reference to procedure(Block: TMarkdownNode; const Content: string);

  TMarkdownBlockParser = class
  private
    FDoc: TMarkdownNode;
    FTip: TMarkdownNode;
    FOldTip: TMarkdownNode;
    FLastMatchedContainer: TMarkdownNode;
    FLine: string;
    FLineNumber: Integer;
    FOffset: Integer;          // 1-based index into FLine
    FColumn: Integer;          // 0-based virtual column (tabs expanded)
    FNextNonspace: Integer;
    FNextNonspaceColumn: Integer;
    FIndent: Integer;
    FIndented: Boolean;
    // last FindNextNonspace result on this line (columns are absolute, so it
    // stays valid while the offset does not pass it): keeps deep nesting fast
    FScanLine: Integer;
    FScanIndex: Integer;
    FScanColumn: Integer;
    FBlank: Boolean;
    FPartiallyConsumedTab: Boolean;
    FAllClosed: Boolean;
    FLastLineLength: Integer;
    FRefMap: TLinkReferenceMap;
    FUnlinked: TMarkdownNodeList;
    FInlineParser: TMarkdownInlineParserProc;
    FExtensions: TMarkdownExtensions;

    function CharAt(Index: Integer): Char; inline;
    procedure FindNextNonspace;
    procedure AdvanceNextNonspace;
    procedure AdvanceOffset(Count: Integer; Columns: Boolean);
    procedure AddLine;
    function AddChild(Kind: TMarkdownNodeKind; Offset: Integer): TMarkdownNode;
    procedure CloseUnmatchedBlocks;
    procedure Finalize(Block: TMarkdownNode; LineNumber: Integer);
    procedure IncorporateLine(const Line: string);
    procedure ProcessInlines;
    procedure Discard(Node: TMarkdownNode);

    // block behaviors
    function ContinueBlock(Container: TMarkdownNode): Integer;
    procedure FinalizeBlock(Block: TMarkdownNode);
    function CanContain(Parent: TMarkdownNodeKind; Child: TMarkdownNodeKind): Boolean;
    function AcceptsLines(Kind: TMarkdownNodeKind): Boolean;
    function ResolveReferenceDefinitions(Block: TMarkdownNode): Boolean;

    // block starts: 0 = no match, 1 = matched container, 2 = matched leaf
    function TryStartBlock(var Container: TMarkdownNode): Integer;
    function StartBlockQuote: Integer;
    function StartAtxHeading: Integer;
    function StartFencedCode: Integer;
    function StartHtmlBlock(Container: TMarkdownNode): Integer;
    function StartSetextHeading(Container: TMarkdownNode): Integer;
    function StartThematicBreak: Integer;
    function StartListItem(Container: TMarkdownNode): Integer;
    function StartIndentedCode: Integer;
    function StartTable(Container: TMarkdownNode): Integer;
    function StartMathBlock: Integer;
    procedure BuildTable(Table: TMarkdownNode);

    function IsThematicBreakLine(Index: Integer): Boolean;
    function HtmlBlockStartKind(Index: Integer; Container: TMarkdownNode): Integer;
    function HtmlBlockEnds(Kind: Integer; const Text: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>Parses Source and returns the document node. The caller owns
    /// it and must call ReleaseOwnership when done.</summary>
    function Parse(const Source: string): TMarkdownNode;
    property RefMap: TLinkReferenceMap read FRefMap;
    /// <summary>Called for every paragraph and heading after the block phase.
    /// When unassigned, TMarkdownInlineParser is used.</summary>
    property InlineParser: TMarkdownInlineParserProc read FInlineParser write FInlineParser;
    /// <summary>Enabled extensions (tables, task lists...): see TMarkdownExtension.</summary>
    property Extensions: TMarkdownExtensions read FExtensions write FExtensions;
  end;

implementation

uses
  MarkdownTextUtils,
  MarkdownInlineParser,
  MarkdownGFM,
  MarkdownMath,
  MarkdownAlerts,
  MarkdownLegacyExt;

const
  CodeIndent = 4;

  HtmlBlock6Tags: array[0..62] of string = (
    'address', 'article', 'aside', 'base', 'basefont', 'blockquote', 'body',
    'caption', 'center', 'col', 'colgroup', 'dd', 'details', 'dialog', 'dir',
    'div', 'dl', 'dt', 'fieldset', 'figcaption', 'figure', 'footer', 'form',
    'frame', 'frameset', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'head', 'header',
    'hr', 'html', 'iframe', 'legend', 'li', 'link', 'main', 'menu', 'menuitem',
    'nav', 'noframes', 'ol', 'optgroup', 'option', 'p', 'param', 'search',
    'section', 'summary', 'table', 'tbody', 'td', 'tfoot', 'th', 'thead',
    'title', 'tr', 'track', 'ul', '');

  HtmlBlock1Tags: array[0..3] of string = ('script', 'pre', 'textarea', 'style');

function PlainText(Node: TMarkdownNode): string; forward;

{ TMarkdownBlockParser }

constructor TMarkdownBlockParser.Create;
begin
  inherited Create;
  FRefMap := TLinkReferenceMap.Create;
  FUnlinked := TMarkdownNodeList.Create;
end;

destructor TMarkdownBlockParser.Destroy;
begin
  FUnlinked.Free;
  FRefMap.Free;
  inherited;
end;

function TMarkdownBlockParser.CharAt(Index: Integer): Char;
begin
  if (Index >= 1) and (Index <= Length(FLine)) then
    Result := FLine[Index]
  else
    Result := #0;
end;

procedure TMarkdownBlockParser.Discard(Node: TMarkdownNode);
begin
  Node.Unlink;
  FUnlinked.Add(Node);
end;

function TMarkdownBlockParser.Parse(const Source: string): TMarkdownNode;
var
  I, Start, Len: Integer;
  LineCount: Integer;
  Node: TMarkdownNode;
begin
  FRefMap.Clear;
  FDoc := TMarkdownNode.CreateDocument;
  try
    FDoc.IsOpen := True;
    FDoc.StartLine := 1;
    FDoc.StartColumn := 1;
    FTip := FDoc;
    FOldTip := FDoc;
    FLineNumber := 0;
    FLastLineLength := 0;

    // split on CRLF, LF, CR; a final line ending does not start a new line
    Len := Length(Source);
    Start := 1;
    I := 1;
    LineCount := 0;
    while I <= Len do
    begin
      if (Source[I] = #10) or (Source[I] = #13) then
      begin
        IncorporateLine(Copy(Source, Start, I - Start));
        Inc(LineCount);
        if (Source[I] = #13) and (I < Len) and (Source[I + 1] = #10) then
          Inc(I);
        Start := I + 1;
      end;
      Inc(I);
    end;
    if Start <= Len then
    begin
      IncorporateLine(Copy(Source, Start, Len - Start + 1));
      Inc(LineCount);
    end;

    while FTip <> nil do
      Finalize(FTip, LineCount);

    ProcessInlines;
    if mexAlerts in FExtensions then
      ApplyGitHubAlerts(FDoc, FUnlinked);

    for Node in FUnlinked do
      Node.Free;
    FUnlinked.Clear;
    Result := FDoc;
    FDoc := nil;
  except
    for Node in FUnlinked do
      Node.Free;
    FUnlinked.Clear;
    FDoc.ReleaseOwnership;
    FDoc := nil;
    raise;
  end;
end;

procedure TMarkdownBlockParser.ProcessInlines;
var
  Walker: TMarkdownWalker;
  Node: TMarkdownNode;
  Entering: Boolean;
  Blocks: TMarkdownNodeList;
  Content: string;
  Inlines: TMarkdownInlineParser;
  Checked: Boolean;
  Marker: TMarkdownNode;
  HeadingId: string;
  Slugs: TSlugRegistry;
begin
  Blocks := TMarkdownNodeList.Create;
  Inlines := TMarkdownInlineParser.Create(FDoc, FRefMap);
  Slugs := TSlugRegistry.Create;
  try
    Inlines.Extensions := FExtensions;
    Walker.Init(FDoc);
    while Walker.Next(Node, Entering) do
      if Entering and (Node.Kind in [nkParagraph, nkHeading, nkTableCell]) then
        Blocks.Add(Node);
    for Node in Blocks do
    begin
      Content := TrimSpaceTabNewline(Node.StringContent);
      Node.StringContent := '';
      if Node.Kind = nkTableCell then
        Content := UnescapeTablePipes(Content)
      else if (Node.Kind = nkHeading) and (mexHeadingAttributes in FExtensions) and
        ExtractHeadingId(Content, HeadingId) then
      begin
        // # Title {#id}
        Node.Id := HeadingId;
        Slugs.Reserve(HeadingId);
      end
      else if (mexTaskLists in FExtensions) and (Node.Kind = nkParagraph) and
         (Node.Parent <> nil) and (Node.Parent.Kind = nkListItem) and
         (Node.Parent.FirstChild = Node) and MatchTaskListMarker(Content, Checked) then
      begin
        // GFM task list item: "[ ]" / "[x]" at the start of the first paragraph
        Marker := TMarkdownNode.Create(nkTaskMarker, FDoc);
        Marker.Checked := Checked;
        Node.AppendChild(Marker);
        Content := Copy(Content, 4, MaxInt);
      end;
      if Assigned(FInlineParser) then
        FInlineParser(Node, Content)
      else
        Inlines.Parse(Node, Content);
      if (Node.Kind = nkHeading) and (Node.Id = '') and (mexAutoHeadingIds in FExtensions) then
        Node.Id := Slugs.Unique(GitHubSlug(PlainText(Node)));
    end;
  finally
    Slugs.Free;
    Inlines.Free;
    Blocks.Free;
  end;
end;

procedure TMarkdownBlockParser.FindNextNonspace;
var
  I, Cols: Integer;
  C: Char;
begin
  if (FScanLine = FLineNumber) and (FOffset <= FScanIndex) then
  begin
    I := FScanIndex;
    Cols := FScanColumn;
  end
  else
  begin
    I := FOffset;
    Cols := FColumn;
    while I <= Length(FLine) do
    begin
      C := FLine[I];
      if C = ' ' then
      begin
        Inc(I);
        Inc(Cols);
      end
      else if C = #9 then
      begin
        Inc(I);
        Inc(Cols, 4 - (Cols mod 4));
      end
      else
        Break;
    end;
    FScanLine := FLineNumber;
    FScanIndex := I;
    FScanColumn := Cols;
  end;
  FBlank := I > Length(FLine);
  FNextNonspace := I;
  FNextNonspaceColumn := Cols;
  FIndent := FNextNonspaceColumn - FColumn;
  FIndented := FIndent >= CodeIndent;
end;

procedure TMarkdownBlockParser.AdvanceNextNonspace;
begin
  FOffset := FNextNonspace;
  FColumn := FNextNonspaceColumn;
  FPartiallyConsumedTab := False;
end;

procedure TMarkdownBlockParser.AdvanceOffset(Count: Integer; Columns: Boolean);
var
  CharsToTab, CharsToAdvance: Integer;
begin
  while (Count > 0) and (FOffset <= Length(FLine)) do
  begin
    if FLine[FOffset] = #9 then
    begin
      CharsToTab := 4 - (FColumn mod 4);
      if Columns then
      begin
        FPartiallyConsumedTab := CharsToTab > Count;
        if CharsToTab > Count then
          CharsToAdvance := Count
        else
          CharsToAdvance := CharsToTab;
        Inc(FColumn, CharsToAdvance);
        if not FPartiallyConsumedTab then
          Inc(FOffset);
        Dec(Count, CharsToAdvance);
      end
      else
      begin
        FPartiallyConsumedTab := False;
        Inc(FColumn, CharsToTab);
        Inc(FOffset);
        Dec(Count);
      end;
    end
    else
    begin
      FPartiallyConsumedTab := False;
      Inc(FOffset);
      Inc(FColumn);
      Dec(Count);
    end;
  end;
end;

procedure TMarkdownBlockParser.AddLine;
var
  CharsToTab: Integer;
begin
  if FPartiallyConsumedTab then
  begin
    Inc(FOffset); // skip over the tab
    CharsToTab := 4 - (FColumn mod 4);
    FTip.StringContent := FTip.StringContent + RepeatChar(' ', CharsToTab);
  end;
  FTip.StringContent := FTip.StringContent + Copy(FLine, FOffset, MaxInt) + #10;
end;

function TMarkdownBlockParser.AddChild(Kind: TMarkdownNodeKind; Offset: Integer): TMarkdownNode;
begin
  while not CanContain(FTip.Kind, Kind) do
    Finalize(FTip, FLineNumber - 1);
  Result := TMarkdownNode.Create(Kind, FDoc);
  Result.IsOpen := True;
  Result.StartLine := FLineNumber;
  Result.StartColumn := Offset;
  FTip.AppendChild(Result);
  FTip := Result;
end;

procedure TMarkdownBlockParser.CloseUnmatchedBlocks;
var
  Parent: TMarkdownNode;
begin
  if not FAllClosed then
  begin
    while FOldTip <> FLastMatchedContainer do
    begin
      Parent := FOldTip.Parent;
      Finalize(FOldTip, FLineNumber - 1);
      FOldTip := Parent;
    end;
    FAllClosed := True;
  end;
end;

procedure TMarkdownBlockParser.Finalize(Block: TMarkdownNode; LineNumber: Integer);
var
  Above: TMarkdownNode;
begin
  Above := Block.Parent;
  Block.IsOpen := False;
  Block.EndLine := LineNumber;
  Block.EndColumn := FLastLineLength;
  FinalizeBlock(Block);
  FTip := Above;
end;

procedure TMarkdownBlockParser.IncorporateLine(const Line: string);
var
  MatchedLeaf: Boolean;
  Container, LastChild: TMarkdownNode;
  Res: Integer;
begin
  Container := FDoc;
  FOldTip := FTip;
  FOffset := 1;
  FColumn := 0;
  FBlank := False;
  FPartiallyConsumedTab := False;
  Inc(FLineNumber);
  if Pos(#0, Line) > 0 then
    FLine := StringReplace(Line, #0, ReplacementChar, [rfReplaceAll])
  else
    FLine := Line;

  // 1. match the open containers
  LastChild := Container.LastChild;
  while (LastChild <> nil) and LastChild.IsOpen do
  begin
    Container := LastChild;
    FindNextNonspace;
    Res := ContinueBlock(Container);
    if Res = 2 then
      Exit; // closing fence: the line is done
    if Res = 1 then
    begin
      Container := Container.Parent;
      Break;
    end;
    LastChild := Container.LastChild;
  end;
  FAllClosed := Container = FOldTip;
  FLastMatchedContainer := Container;

  // 2. look for new block starts
  // paragraphs and tables accept lines but can be interrupted by new blocks
  MatchedLeaf := not (Container.Kind in [nkParagraph, nkTable]) and AcceptsLines(Container.Kind);
  while not MatchedLeaf do
  begin
    FindNextNonspace;
    Res := TryStartBlock(Container);
    if Res = 0 then
    begin
      AdvanceNextNonspace;
      Break;
    end;
    if Res = 2 then
      MatchedLeaf := True;
  end;

  // 3. add the rest of the line to the right block
  if not FAllClosed and not FBlank and (FTip.Kind = nkParagraph) then
    AddLine // lazy paragraph continuation
  else
  begin
    CloseUnmatchedBlocks;
    if AcceptsLines(Container.Kind) then
    begin
      AddLine;
      if (Container.Kind = nkHtmlBlock) and (Container.HtmlBlockType >= 1) and
         (Container.HtmlBlockType <= 5) and
         HtmlBlockEnds(Container.HtmlBlockType, Copy(FLine, FOffset, MaxInt)) then
      begin
        FLastLineLength := Length(FLine);
        Finalize(Container, FLineNumber);
      end;
    end
    else if (FOffset <= Length(FLine)) and not FBlank then
    begin
      AddChild(nkParagraph, FOffset);
      AdvanceNextNonspace;
      AddLine;
    end;
  end;
  FLastLineLength := Length(FLine);
end;

{ Block behaviors }

function TMarkdownBlockParser.ContinueBlock(Container: TMarkdownNode): Integer;
var
  I, FenceLen: Integer;
  C: Char;
begin
  Result := 0;
  case Container.Kind of
    nkDocument, nkList:
      Result := 0;

    nkBlockQuote:
      if not FIndented and (CharAt(FNextNonspace) = '>') then
      begin
        AdvanceNextNonspace;
        AdvanceOffset(1, False);
        if IsSpaceOrTab(CharAt(FOffset)) then
          AdvanceOffset(1, True);
      end
      else
        Result := 1;

    nkListItem:
      if FBlank then
      begin
        if Container.FirstChild = nil then
          Result := 1 // a blank line after an empty list item closes it
        else
          AdvanceNextNonspace;
      end
      else if FIndent >= Container.MarkerOffset + Container.ListPadding then
        AdvanceOffset(Container.MarkerOffset + Container.ListPadding, True)
      else
        Result := 1;

    nkHeading, nkThematicBreak:
      Result := 1;

    nkCodeBlock, nkMathBlock:
      if Container.IsFenced then
      begin
        // closing fence?
        if (FIndent <= 3) and (CharAt(FNextNonspace) = Container.FenceChar) then
        begin
          I := FNextNonspace;
          C := Container.FenceChar;
          while CharAt(I) = C do
            Inc(I);
          FenceLen := I - FNextNonspace;
          while IsSpaceOrTab(CharAt(I)) do
            Inc(I);
          if (FenceLen >= Container.FenceLength) and (I > Length(FLine)) then
          begin
            FLastLineLength := Length(FLine);
            Finalize(Container, FLineNumber);
            Exit(2);
          end;
        end;
        // skip the optional spaces of the fence offset
        I := Container.FenceOffset;
        while (I > 0) and IsSpaceOrTab(CharAt(FOffset)) do
        begin
          AdvanceOffset(1, True);
          Dec(I);
        end;
      end
      else
      begin
        if FIndent >= CodeIndent then
          AdvanceOffset(CodeIndent, True)
        else if FBlank then
          AdvanceNextNonspace
        else
          Result := 1;
      end;

    nkHtmlBlock:
      if FBlank and ((Container.HtmlBlockType = 6) or (Container.HtmlBlockType = 7)) then
        Result := 1;

    nkParagraph, nkTable:
      if FBlank then
        Result := 1;
  else
    Result := 1;
  end;
end;

function TMarkdownBlockParser.CanContain(Parent: TMarkdownNodeKind; Child: TMarkdownNodeKind): Boolean;
begin
  case Parent of
    nkDocument, nkBlockQuote, nkListItem:
      Result := Child <> nkListItem;
    nkList:
      Result := Child = nkListItem;
  else
    Result := False;
  end;
end;

function TMarkdownBlockParser.AcceptsLines(Kind: TMarkdownNodeKind): Boolean;
begin
  Result := Kind in [nkParagraph, nkCodeBlock, nkHtmlBlock, nkTable, nkMathBlock];
end;

function TMarkdownBlockParser.ResolveReferenceDefinitions(Block: TMarkdownNode): Boolean;
var
  Consumed, Start: Integer;
  Content: string;
begin
  // the definitions are consumed with an offset: the content is copied once
  Result := False;
  Content := Block.StringContent;
  Start := 1;
  while (Start <= Length(Content)) and (Content[Start] = '[') do
  begin
    Consumed := ParseLinkReferenceDefinition(Content, Start, FRefMap);
    if Consumed = 0 then
      Break;
    Inc(Start, Consumed);
    Result := True;
  end;
  if Result then
    Block.StringContent := Copy(Content, Start, MaxInt);
end;

// Text content of a node (as a browser's textContent): text, code and math literals
function PlainText(Node: TMarkdownNode): string;
var
  Walker: TMarkdownWalker;
  Child: TMarkdownNode;
  Entering: Boolean;
begin
  Result := '';
  Walker.Init(Node);
  while Walker.Next(Child, Entering) do
    if Entering then
      case Child.Kind of
        nkText, nkCode, nkMathInline:
          Result := Result + Child.Literal;
        nkSoftBreak, nkHardBreak:
          Result := Result + ' ';
      end;
end;

function EndsWithBlankLine(Block: TMarkdownNode): Boolean;
begin
  Result := (Block.Next <> nil) and (Block.EndLine <> Block.Next.StartLine - 1);
end;

procedure TMarkdownBlockParser.FinalizeBlock(Block: TMarkdownNode);
var
  Content, FirstLine: string;
  P, Q, LineCount: Integer;
  Item, SubItem: TMarkdownNode;
begin
  case Block.Kind of
    nkParagraph:
      if ResolveReferenceDefinitions(Block) and IsBlankText(Block.StringContent) then
        Discard(Block);

    nkCodeBlock:
      begin
        Content := Block.StringContent;
        Block.StringContent := '';
        if Block.IsFenced then
        begin
          // the first line is the info string
          P := Pos(#10, Content);
          FirstLine := Copy(Content, 1, P - 1);
          Block.Info := UnescapeString(TrimSpaceTab(FirstLine));
          Block.Literal := Copy(Content, P + 1, MaxInt);
          // ```math is display math
          if (mexMath in FExtensions) and IsMathInfoString(Block.Info) then
          begin
            Block.Kind := nkMathBlock;
            while (Block.Literal <> '') and (Block.Literal[Length(Block.Literal)] = #10) do
              SetLength(Block.Literal, Length(Block.Literal) - 1);
          end;
        end
        else
        begin
          // indented: drop the trailing blank lines (Content ends with #10)
          P := Length(Content);
          while P > 0 do
          begin
            Q := P - 1;
            while (Q > 0) and (Content[Q] <> #10) do
              Dec(Q);
            if (Q = 0) or not IsBlankText(Copy(Content, Q + 1, P - Q - 1)) then
              Break;
            P := Q;
          end;
          Block.Literal := Copy(Content, 1, P);
          LineCount := 0;
          for Q := 1 to Length(Block.Literal) do
            if Block.Literal[Q] = #10 then
              Inc(LineCount);
          if LineCount > 0 then
            Block.EndLine := Block.StartLine + LineCount - 1;
        end;
      end;

    nkHtmlBlock:
      begin
        Content := Block.StringContent;
        Block.StringContent := '';
        // remove the final line endings (and spaces-only lines after them)
        P := Length(Content);
        while P > 0 do
        begin
          while (P > 0) and (Content[P] = ' ') do
            Dec(P);
          if (P > 0) and (Content[P] = #10) then
          begin
            Dec(P);
            SetLength(Content, P);
          end
          else
            Break;
        end;
        Block.Literal := Content;
      end;

    nkTable:
      BuildTable(Block);

    nkMathBlock:
      begin
        // the first line is the rest of the opening $$ line
        Content := Block.StringContent;
        Block.StringContent := '';
        P := Pos(#10, Content);
        if P = 0 then
          Block.Literal := ''
        else
          Block.Literal := Copy(Content, P + 1, MaxInt);
        while (Block.Literal <> '') and (Block.Literal[Length(Block.Literal)] = #10) do
          SetLength(Block.Literal, Length(Block.Literal) - 1);
      end;

    nkListItem:
      if Block.LastChild <> nil then
      begin
        Block.EndLine := Block.LastChild.EndLine;
        Block.EndColumn := Block.LastChild.EndColumn;
      end
      else
      begin
        Block.EndLine := Block.StartLine;
        Block.EndColumn := Block.ListPadding + Block.MarkerOffset;
      end;

    nkList:
      begin
        Item := Block.FirstChild;
        while (Item <> nil) and Block.ListTight do
        begin
          if (Item.Next <> nil) and EndsWithBlankLine(Item) then
          begin
            Block.ListTight := False;
            Break;
          end;
          // blank lines between the children of an item make the list loose
          SubItem := Item.FirstChild;
          while SubItem <> nil do
          begin
            if (SubItem.Next <> nil) and EndsWithBlankLine(SubItem) then
            begin
              Block.ListTight := False;
              Break;
            end;
            SubItem := SubItem.Next;
          end;
          Item := Item.Next;
        end;
        if Block.LastChild <> nil then
        begin
          Block.EndLine := Block.LastChild.EndLine;
          Block.EndColumn := Block.LastChild.EndColumn;
        end;
      end;
  end;
end;

{ Block starts }

function TMarkdownBlockParser.TryStartBlock(var Container: TMarkdownNode): Integer;
begin
  // A line that is not indented and does not start with a special character
  // cannot open a block (performance shortcut of the reference implementation).
  if not FIndented and not CharInSet(CharAt(FNextNonspace),
     ['#', '`', '~', '*', '+', '_', '=', '<', '>', '-', '0'..'9', '|', ':', '$']) then
    Exit(0);

  Result := StartBlockQuote;
  if Result = 0 then
    Result := StartAtxHeading;
  if Result = 0 then
    Result := StartFencedCode;
  if (Result = 0) and (mexMath in FExtensions) then
    Result := StartMathBlock;
  if Result = 0 then
    Result := StartHtmlBlock(Container);
  if Result = 0 then
    Result := StartSetextHeading(Container);
  if Result = 0 then
    Result := StartThematicBreak;
  if Result = 0 then
    Result := StartListItem(Container);
  if Result = 0 then
    Result := StartIndentedCode;
  if (Result = 0) and (mexTables in FExtensions) then
    Result := StartTable(Container);
  if Result <> 0 then
    Container := FTip;
end;

function TMarkdownBlockParser.StartBlockQuote: Integer;
begin
  Result := 0;
  if FIndented or (CharAt(FNextNonspace) <> '>') then
    Exit;
  AdvanceNextNonspace;
  AdvanceOffset(1, False);
  // optional following space
  if IsSpaceOrTab(CharAt(FOffset)) then
    AdvanceOffset(1, True);
  CloseUnmatchedBlocks;
  AddChild(nkBlockQuote, FNextNonspace);
  Result := 1;
end;

function TMarkdownBlockParser.StartAtxHeading: Integer;
var
  I, Level, ContentStart, ContentEnd, J: Integer;
  Heading: TMarkdownNode;
begin
  Result := 0;
  if FIndented or (CharAt(FNextNonspace) <> '#') then
    Exit;
  I := FNextNonspace;
  while CharAt(I) = '#' do
    Inc(I);
  Level := I - FNextNonspace;
  if (Level > 6) or not ((I > Length(FLine)) or IsSpaceOrTab(FLine[I])) then
    Exit;

  AdvanceNextNonspace;
  AdvanceOffset(Level, False);
  CloseUnmatchedBlocks;
  Heading := AddChild(nkHeading, FNextNonspace);
  Heading.Level := Level;

  // content without the optional closing sequence of #
  ContentStart := FOffset;
  ContentEnd := Length(FLine);
  while (ContentEnd >= ContentStart) and IsSpaceOrTab(FLine[ContentEnd]) do
    Dec(ContentEnd);
  J := ContentEnd;
  while (J >= ContentStart) and (FLine[J] = '#') do
    Dec(J);
  if J < ContentEnd then
  begin
    // a closing sequence must be preceded by a space or tab (or be the whole content)
    if J < ContentStart then
      ContentEnd := ContentStart - 1
    else if IsSpaceOrTab(FLine[J]) then
      ContentEnd := J;
  end;
  Heading.StringContent := Copy(FLine, ContentStart, ContentEnd - ContentStart + 1);
  AdvanceOffset(Length(FLine) - FOffset + 1, False);
  Result := 2;
end;

function TMarkdownBlockParser.StartFencedCode: Integer;
var
  I, FenceLen: Integer;
  C: Char;
  Block: TMarkdownNode;
begin
  Result := 0;
  if FIndented then
    Exit;
  C := CharAt(FNextNonspace);
  if (C <> '`') and (C <> '~') then
    Exit;
  I := FNextNonspace;
  while CharAt(I) = C do
    Inc(I);
  FenceLen := I - FNextNonspace;
  if FenceLen < 3 then
    Exit;
  // the info string of a backtick fence cannot contain backticks
  if (C = '`') and (Pos('`', Copy(FLine, I, MaxInt)) > 0) then
    Exit;
  CloseUnmatchedBlocks;
  Block := AddChild(nkCodeBlock, FNextNonspace);
  Block.IsFenced := True;
  Block.FenceLength := FenceLen;
  Block.FenceChar := C;
  Block.FenceOffset := FIndent;
  AdvanceNextNonspace;
  AdvanceOffset(FenceLen, False);
  Result := 2;
end;

function TMarkdownBlockParser.HtmlBlockStartKind(Index: Integer; Container: TMarkdownNode): Integer;
var
  K, Len, After: Integer;
  Name: string;
  C: Char;
  IsClosing: Boolean;
begin
  Result := 0;
  if CharAt(Index) <> '<' then
    Exit;
  // 1: <script, <pre, <textarea, <style
  for K := Low(HtmlBlock1Tags) to High(HtmlBlock1Tags) do
    if StartsAtIgnoreCase(FLine, Index + 1, HtmlBlock1Tags[K]) then
    begin
      C := CharAt(Index + 1 + Length(HtmlBlock1Tags[K]));
      if (C = #0) or (C = '>') or IsSpaceOrTab(C) then
        Exit(1);
    end;
  if StartsAt(FLine, Index, '<!--') then
    Exit(2);
  if StartsAt(FLine, Index, '<?') then
    Exit(3);
  if StartsAt(FLine, Index, '<!') and IsAsciiLetter(CharAt(Index + 2)) then
    Exit(4);
  if StartsAt(FLine, Index, '<![CDATA[') then
    Exit(5);
  // 6: known block tags
  IsClosing := CharAt(Index + 1) = '/';
  Name := HtmlTagNameAt(FLine, Index);
  if Name <> '' then
  begin
    for K := Low(HtmlBlock6Tags) to High(HtmlBlock6Tags) do
      if (HtmlBlock6Tags[K] <> '') and (Name = HtmlBlock6Tags[K]) then
      begin
        After := Index + 1 + Length(Name);
        if IsClosing then
          Inc(After);
        C := CharAt(After);
        if (C = #0) or (C = '>') or IsSpaceOrTab(C) or
           ((C = '/') and (CharAt(After + 1) = '>')) then
          Exit(6);
        Break;
      end;
  end;
  // 7: any complete open/closing tag alone on the line; cannot interrupt a paragraph
  if (Container.Kind = nkParagraph) or
     (not FAllClosed and not FBlank and (FTip.Kind = nkParagraph)) then
    Exit;
  if IsClosing then
    Len := ScanHtmlClosingTag(FLine, Index)
  else
  begin
    if (Name = 'script') or (Name = 'style') or (Name = 'pre') or (Name = 'textarea') then
      Exit;
    Len := ScanHtmlOpenTag(FLine, Index);
  end;
  if Len = 0 then
    Exit;
  After := Index + Len;
  while IsSpaceOrTab(CharAt(After)) do
    Inc(After);
  if After > Length(FLine) then
    Result := 7;
end;

function TMarkdownBlockParser.HtmlBlockEnds(Kind: Integer; const Text: string): Boolean;
var
  Lower: string;
begin
  case Kind of
    1:
      begin
        Lower := LowerCase(Text);
        Result := (Pos('</script>', Lower) > 0) or (Pos('</pre>', Lower) > 0) or
                  (Pos('</textarea>', Lower) > 0) or (Pos('</style>', Lower) > 0);
      end;
    2: Result := Pos('-->', Text) > 0;
    3: Result := Pos('?>', Text) > 0;
    4: Result := Pos('>', Text) > 0;
    5: Result := Pos(']]>', Text) > 0;
  else
    Result := False;
  end;
end;

function TMarkdownBlockParser.StartHtmlBlock(Container: TMarkdownNode): Integer;
var
  Kind: Integer;
  Block: TMarkdownNode;
begin
  Result := 0;
  if FIndented or (CharAt(FNextNonspace) <> '<') then
    Exit;
  Kind := HtmlBlockStartKind(FNextNonspace, Container);
  if Kind = 0 then
    Exit;
  CloseUnmatchedBlocks;
  // the offset is not advanced: the spaces are part of the HTML block
  Block := AddChild(nkHtmlBlock, FOffset);
  Block.HtmlBlockType := Kind;
  Result := 2;
end;

function TMarkdownBlockParser.StartSetextHeading(Container: TMarkdownNode): Integer;
var
  I: Integer;
  C: Char;
  Heading: TMarkdownNode;
begin
  Result := 0;
  if FIndented or (Container.Kind <> nkParagraph) then
    Exit;
  C := CharAt(FNextNonspace);
  if (C <> '=') and (C <> '-') then
    Exit;
  I := FNextNonspace;
  while CharAt(I) = C do
    Inc(I);
  while IsSpaceOrTab(CharAt(I)) do
    Inc(I);
  if I <= Length(FLine) then
    Exit;

  CloseUnmatchedBlocks;
  // the paragraph may start with link reference definitions
  ResolveReferenceDefinitions(Container);
  if Container.StringContent = '' then
    Exit;
  Heading := TMarkdownNode.Create(nkHeading, FDoc);
  Heading.IsOpen := True;
  Heading.StartLine := Container.StartLine;
  Heading.StartColumn := Container.StartColumn;
  if C = '=' then
    Heading.Level := 1
  else
    Heading.Level := 2;
  Heading.StringContent := Container.StringContent;
  Container.InsertAfter(Heading);
  Discard(Container);
  FTip := Heading;
  AdvanceOffset(Length(FLine) - FOffset + 1, False);
  Result := 2;
end;

function TMarkdownBlockParser.IsThematicBreakLine(Index: Integer): Boolean;
var
  C: Char;
  Count: Integer;
begin
  Result := False;
  C := CharAt(Index);
  if (C <> '*') and (C <> '-') and (C <> '_') then
    Exit;
  Count := 0;
  while Index <= Length(FLine) do
  begin
    if FLine[Index] = C then
      Inc(Count)
    else if not IsSpaceOrTab(FLine[Index]) then
      Exit;
    Inc(Index);
  end;
  Result := Count >= 3;
end;

function TMarkdownBlockParser.StartThematicBreak: Integer;
begin
  Result := 0;
  if FIndented or not IsThematicBreakLine(FNextNonspace) then
    Exit;
  CloseUnmatchedBlocks;
  AddChild(nkThematicBreak, FNextNonspace);
  AdvanceOffset(Length(FLine) - FOffset + 1, False);
  Result := 2;
end;

function TMarkdownBlockParser.StartListItem(Container: TMarkdownNode): Integer;
var
  I, MarkerLen, Start, SpacesStartCol, SpacesStartOffset, SpacesAfterMarker: Integer;
  ListType: TMarkdownListType;
  Bullet, Delimiter, NextC: Char;
  Padding: Integer;
  BlankItem: Boolean;
  MarkerOffset: Integer;
  List, Item: TMarkdownNode;
begin
  Result := 0;
  if FIndented and (Container.Kind <> nkList) then
    Exit;
  if FIndent >= 4 then
    Exit;
  MarkerOffset := FIndent;
  Bullet := #0;
  Delimiter := #0;
  Start := 1;
  I := FNextNonspace;
  if CharInSet(CharAt(I), ['*', '+', '-']) then
  begin
    ListType := mltBullet;
    Bullet := CharAt(I);
    MarkerLen := 1;
  end
  else if CharInSet(CharAt(I), ['0'..'9']) then
  begin
    while CharInSet(CharAt(I), ['0'..'9']) and (I - FNextNonspace < 9) do
      Inc(I);
    if not CharInSet(CharAt(I), ['.', ')']) then
      Exit;
    Start := StrToInt(Copy(FLine, FNextNonspace, I - FNextNonspace));
    // an ordered list can interrupt a paragraph only when it starts with 1
    if (Container.Kind = nkParagraph) and (Start <> 1) then
      Exit;
    ListType := mltOrdered;
    Delimiter := CharAt(I);
    MarkerLen := I - FNextNonspace + 1;
  end
  else
    Exit;

  // the marker must be followed by a space, a tab or the end of the line
  NextC := CharAt(FNextNonspace + MarkerLen);
  if not ((NextC = #0) or IsSpaceOrTab(NextC)) then
    Exit;
  // an item interrupting a paragraph cannot be empty
  if (Container.Kind = nkParagraph) and IsBlankText(Copy(FLine, FNextNonspace + MarkerLen, MaxInt)) then
    Exit;

  // match: compute the padding
  AdvanceNextNonspace;
  AdvanceOffset(MarkerLen, True);
  SpacesStartCol := FColumn;
  SpacesStartOffset := FOffset;
  repeat
    AdvanceOffset(1, True);
    NextC := CharAt(FOffset);
  until not ((FColumn - SpacesStartCol < 5) and IsSpaceOrTab(NextC));
  BlankItem := FOffset > Length(FLine);
  SpacesAfterMarker := FColumn - SpacesStartCol;
  if (SpacesAfterMarker >= 5) or (SpacesAfterMarker < 1) or BlankItem then
  begin
    Padding := MarkerLen + 1;
    FColumn := SpacesStartCol;
    FOffset := SpacesStartOffset;
    FPartiallyConsumedTab := False;
    if IsSpaceOrTab(CharAt(FOffset)) then
      AdvanceOffset(1, True);
  end
  else
    Padding := MarkerLen + SpacesAfterMarker;

  CloseUnmatchedBlocks;
  // add the list if needed
  if (FTip.Kind <> nkList) or (FTip.ListType <> ListType) or
     (FTip.ListDelimiter <> Delimiter) or (FTip.BulletChar <> Bullet) then
  begin
    List := AddChild(nkList, FNextNonspace);
    List.ListType := ListType;
    List.BulletChar := Bullet;
    List.ListDelimiter := Delimiter;
    List.ListStart := Start;
    List.ListPadding := Padding;
    List.MarkerOffset := MarkerOffset;
  end;
  Item := AddChild(nkListItem, FNextNonspace);
  Item.ListType := ListType;
  Item.BulletChar := Bullet;
  Item.ListDelimiter := Delimiter;
  Item.ListStart := Start;
  Item.ListPadding := Padding;
  Item.MarkerOffset := MarkerOffset;
  Result := 1;
end;

function TMarkdownBlockParser.StartIndentedCode: Integer;
begin
  Result := 0;
  if not FIndented or (FTip.Kind in [nkParagraph, nkTable]) or FBlank then
    Exit;
  AdvanceOffset(CodeIndent, True);
  CloseUnmatchedBlocks;
  AddChild(nkCodeBlock, FOffset);
  Result := 2;
end;

{ GFM tables }

function TMarkdownBlockParser.StartTable(Container: TMarkdownNode): Integer;
var
  Aligns: TTableAligns;
  Content, Header, Rest, DelimiterRow: string;
  P: Integer;
  Parent, Table: TMarkdownNode;
begin
  Result := 0;
  // the delimiter row turns the last line of a paragraph into the header row
  if FIndented or (Container.Kind <> nkParagraph) or (FTip <> Container) then
    Exit;
  DelimiterRow := Copy(FLine, FNextNonspace, MaxInt);
  if not ParseTableDelimiterRow(DelimiterRow, Aligns) then
    Exit;
  Content := Container.StringContent;
  if (Content <> '') and (Content[Length(Content)] = #10) then
    SetLength(Content, Length(Content) - 1);
  P := Length(Content);
  while (P > 0) and (Content[P] <> #10) do
    Dec(P);
  Header := Copy(Content, P + 1, MaxInt);
  Rest := Copy(Content, 1, P);
  if Length(SplitTableRow(Header)) <> Length(Aligns) then
    Exit;

  CloseUnmatchedBlocks;
  Parent := Container.Parent;
  Table := TMarkdownNode.Create(nkTable, FDoc);
  Table.IsOpen := True;
  Table.StartLine := FLineNumber - 1;
  Table.StartColumn := Container.StartColumn;
  Table.StringContent := Header + #10 + DelimiterRow;
  if Rest = '' then
  begin
    Discard(Container);
    FTip := Parent;
  end
  else
  begin
    // the lines before the header stay a paragraph
    Container.StringContent := Rest;
    Finalize(Container, FLineNumber - 2);
  end;
  Parent.AppendChild(Table);
  FTip := Table;
  AdvanceOffset(Length(FLine) - FOffset + 1, False);
  Result := 2;
end;

procedure TMarkdownBlockParser.BuildTable(Table: TMarkdownNode);
var
  Lines: TTableCells;
  Aligns: TTableAligns;
  Cells: TTableCells;
  Section, Row, Cell: TMarkdownNode;
  I, J, Start, LineCount: Integer;
  Content: string;
begin
  // StringContent: header row, delimiter row, body rows (one per line)
  Content := Table.StringContent;
  Table.StringContent := '';
  SetLength(Lines, 0);
  LineCount := 0;
  Start := 1;
  for I := 1 to Length(Content) + 1 do
    if (I > Length(Content)) or (Content[I] = #10) then
    begin
      if I > Start then
      begin
        SetLength(Lines, LineCount + 1);
        Lines[LineCount] := Copy(Content, Start, I - Start);
        Inc(LineCount);
      end;
      Start := I + 1;
    end;
  if (LineCount < 2) or not ParseTableDelimiterRow(Lines[1], Aligns) then
    Exit;

  Section := nil;
  for I := 0 to LineCount - 1 do
  begin
    if I = 1 then
      Continue; // delimiter row
    if I = 0 then
    begin
      Section := TMarkdownNode.Create(nkTableHead, FDoc);
      Table.AppendChild(Section);
    end
    else if I = 2 then
    begin
      Section := TMarkdownNode.Create(nkTableBody, FDoc);
      Table.AppendChild(Section);
    end;
    Row := TMarkdownNode.Create(nkTableRow, FDoc);
    if I = 0 then
      Row.StartLine := Table.StartLine
    else
      Row.StartLine := Table.StartLine + I;
    Row.EndLine := Row.StartLine;
    Section.AppendChild(Row);
    Cells := SplitTableRow(Lines[I]);
    // a row has exactly as many cells as the header: extra cells are dropped,
    // missing ones are empty
    for J := 0 to High(Aligns) do
    begin
      Cell := TMarkdownNode.Create(nkTableCell, FDoc);
      Cell.Align := Aligns[J];
      Cell.StartLine := Row.StartLine;
      Cell.EndLine := Row.StartLine;
      if J <= High(Cells) then
        Cell.StringContent := Cells[J];
      Row.AppendChild(Cell);
    end;
    if Section.StartLine = 0 then
      Section.StartLine := Row.StartLine;
    Section.EndLine := Row.StartLine;
  end;
end;


{ Math }

function TMarkdownBlockParser.StartMathBlock: Integer;
var
  Block: TMarkdownNode;
begin
  // a line with exactly $$ opens a display math block, closed by another $$
  Result := 0;
  if FIndented or not IsMathFenceLine(FLine, FNextNonspace) then
    Exit;
  CloseUnmatchedBlocks;
  Block := AddChild(nkMathBlock, FNextNonspace);
  Block.IsFenced := True;
  Block.FenceChar := '$';
  Block.FenceLength := 2;
  Block.FenceOffset := FIndent;
  AdvanceNextNonspace;
  AdvanceOffset(Length(FLine) - FOffset + 1, False);
  Result := 2;
end;

end.
