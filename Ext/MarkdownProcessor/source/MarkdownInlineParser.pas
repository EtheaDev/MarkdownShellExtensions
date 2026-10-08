{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       CommonMark / GFM engine: inline parser                                 }
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
{  The algorithm (delimiter stack, "process emphasis", bracket stack) is the   }
{  one of the CommonMark specification appendix, as implemented by             }
{  commonmark.js (BSD-2-Clause, Copyright (c) 2014 John MacFarlane):           }
{  see docs/LICENSE-commonmark.js.txt.                                         }
{                                                                              }
{******************************************************************************}
unit MarkdownInlineParser;

{ Phase 2 of the CommonMark parsing strategy: the content of paragraphs and
  headings becomes inline nodes. Emphasis is resolved with the delimiter stack
  ("process emphasis", with the openers_bottom optimization that keeps it
  linear) and links/images with the bracket stack. }

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  MarkdownUtils,
  MarkdownAST,
  MarkdownLinkRefs,
  MarkdownMath;

type
  TInlineDelimiter = class
  public
    DelimChar: Char;
    NumDelims: Integer;
    OrigDelims: Integer;
    Node: TMarkdownNode;
    Previous: TInlineDelimiter;
    Next: TInlineDelimiter;
    CanOpen: Boolean;
    CanClose: Boolean;
  end;

  TInlineBracket = class
  public
    Node: TMarkdownNode;
    Previous: TInlineBracket;
    PreviousDelimiter: TInlineDelimiter;
    Index: Integer;        // position of '[' in the subject
    IsImage: Boolean;
    Active: Boolean;
    BracketAfter: Boolean;
  end;

  TMarkdownInlineParser = class
  private
    FSubject: string;
    FPos: Integer;
    FDocument: TMarkdownNode;
    FRefMap: TLinkReferenceMap;
    FDelimiters: TInlineDelimiter;
    FBrackets: TInlineBracket;
    FDiscarded: TMarkdownNodeList;
    // backtick runs already scanned: length -> last start position
    FBacktickRuns: TDictionary<Integer, Integer>;
    FBackticksScanned: Boolean;
    FExtensions: TMarkdownExtensions;
    FMathCache: TMathScanCache;
    // closers of HTML comments, processing instructions, CDATA, declarations:
    // start of the last search and its result (0 = not found)
    FCloserFrom: array[0..3] of Integer;
    FCloserAt: array[0..3] of Integer;
    FNoWikiCloseFrom: Integer;
    function FindCloser(Slot: Integer; const Closer: string; From: Integer): Integer;
    function ScanHtml(Index: Integer): Integer;

    function Peek: Char; inline;
    function NewNode(Kind: TMarkdownNodeKind): TMarkdownNode;
    function NewText(const S: string): TMarkdownNode;
    procedure DiscardNode(Node: TMarkdownNode);

    function ParseInline(Block: TMarkdownNode): Boolean;
    function ParseNewline(Block: TMarkdownNode): Boolean;
    function ParseBackslash(Block: TMarkdownNode): Boolean;
    function ParseBackticks(Block: TMarkdownNode): Boolean;
    function ParseOpenBracket(Block: TMarkdownNode): Boolean;
    function ParseBang(Block: TMarkdownNode): Boolean;
    function ParseCloseBracket(Block: TMarkdownNode): Boolean;
    function ParseAutolink(Block: TMarkdownNode): Boolean;
    function ParseHtmlTag(Block: TMarkdownNode): Boolean;
    function ParseEntity(Block: TMarkdownNode): Boolean;
    function ParseString(Block: TMarkdownNode): Boolean;
    function HandleDelim(C: Char; Block: TMarkdownNode): Boolean;
    function ScanDelims(C: Char; out NumDelims: Integer; out CanOpen, CanClose: Boolean): Boolean;

    procedure AddBracket(Node: TMarkdownNode; Index: Integer; Image: Boolean);
    procedure RemoveBracket;
    procedure RemoveDelimiter(Delim: TInlineDelimiter);
    procedure RemoveDelimitersBetween(Bottom, Top: TInlineDelimiter);
    procedure ProcessEmphasis(StackBottom: TInlineDelimiter);
    procedure MergeTextNodes(Block: TMarkdownNode);
    procedure Cleanup;
    // GFM
    function ParseExtendedAutolink(Block: TMarkdownNode): Boolean;
    function ParseMath(Block: TMarkdownNode): Boolean;
    // legacy extensions
    function ParseWikiLink(Block: TMarkdownNode): Boolean;
    procedure ProcessSmartTypography(Block: TMarkdownNode);
    function ExactDelimiterKind(C: Char; Count: Integer; out Kind: TMarkdownNodeKind): Boolean;
    procedure ProcessEmailAutolinks(Block: TMarkdownNode);
    procedure SplitEmailAutolinks(TextNode: TMarkdownNode);
  protected
    /// <summary>True for the characters that stop a plain text run.</summary>
    function IsSpecialChar(C: Char): Boolean; virtual;
  public
    constructor Create(ADocument: TMarkdownNode; ARefMap: TLinkReferenceMap);
    destructor Destroy; override;
    /// <summary>Parses Content (already trimmed) into inline children of Block.</summary>
    procedure Parse(Block: TMarkdownNode; const Content: string);
    /// <summary>Enabled extensions (strikethrough, autolinks...).</summary>
    property Extensions: TMarkdownExtensions read FExtensions write FExtensions;
  end;

implementation

uses
  System.Math,
  MarkdownTextUtils,
  MarkdownGFM,
  MarkdownLegacyExt,
  System.StrUtils;

{ TMarkdownInlineParser }

constructor TMarkdownInlineParser.Create(ADocument: TMarkdownNode; ARefMap: TLinkReferenceMap);
begin
  inherited Create;
  FDocument := ADocument;
  FRefMap := ARefMap;
  FDiscarded := TMarkdownNodeList.Create;
  FBacktickRuns := TDictionary<Integer, Integer>.Create;
end;

destructor TMarkdownInlineParser.Destroy;
begin
  Cleanup;
  FBacktickRuns.Free;
  FDiscarded.Free;
  inherited;
end;

procedure TMarkdownInlineParser.Cleanup;
var
  Delim: TInlineDelimiter;
  Bracket: TInlineBracket;
  Node: TMarkdownNode;
begin
  while FDelimiters <> nil do
  begin
    Delim := FDelimiters;
    FDelimiters := Delim.Previous;
    Delim.Free;
  end;
  while FBrackets <> nil do
  begin
    Bracket := FBrackets;
    FBrackets := Bracket.Previous;
    Bracket.Free;
  end;
  for Node in FDiscarded do
    Node.Free;
  FDiscarded.Clear;
end;

function TMarkdownInlineParser.Peek: Char;
begin
  if FPos <= Length(FSubject) then
    Result := FSubject[FPos]
  else
    Result := #0;
end;

function TMarkdownInlineParser.NewNode(Kind: TMarkdownNodeKind): TMarkdownNode;
begin
  Result := TMarkdownNode.Create(Kind, FDocument);
end;

function TMarkdownInlineParser.NewText(const S: string): TMarkdownNode;
begin
  Result := NewNode(nkText);
  Result.Literal := S;
end;

procedure TMarkdownInlineParser.DiscardNode(Node: TMarkdownNode);
begin
  Node.Unlink;
  FDiscarded.Add(Node);
end;

function TMarkdownInlineParser.IsSpecialChar(C: Char): Boolean;
begin
  case C of
    #10, '`', '[', ']', '\', '!', '<', '&', '*', '_':
      Result := True;
    '~':
      Result := (mexStrikethrough in FExtensions) or (mexSubscript in FExtensions);
    '^':
      Result := mexSuperscript in FExtensions;
    '+':
      Result := mexInsert in FExtensions;
    '=':
      Result := mexMark in FExtensions;
    '$':
      Result := mexMath in FExtensions;
    // www. and http(s):// ftp:// extended autolinks
    'w', 'h', 'H', 'f', 'F':
      Result := mexAutolinks in FExtensions;
  else
    Result := False;
  end;
end;

procedure TMarkdownInlineParser.Parse(Block: TMarkdownNode; const Content: string);
begin
  FSubject := Content;
  FPos := 1;
  FDelimiters := nil;
  FBrackets := nil;
  FBacktickRuns.Clear;
  FBackticksScanned := False;
  FillChar(FMathCache, SizeOf(FMathCache), 0);
  FillChar(FCloserFrom, SizeOf(FCloserFrom), 0);
  FillChar(FCloserAt, SizeOf(FCloserAt), 0);
  FNoWikiCloseFrom := 0;
  try
    while ParseInline(Block) do
      ;
    ProcessEmphasis(nil);
    MergeTextNodes(Block);
    if mexSmartTypography in FExtensions then
      ProcessSmartTypography(Block);
    if mexAutolinks in FExtensions then
      ProcessEmailAutolinks(Block);
  finally
    Cleanup;
  end;
end;

function TMarkdownInlineParser.ParseInline(Block: TMarkdownNode): Boolean;
var
  C: Char;
  Handled: Boolean;
begin
  if FPos > Length(FSubject) then
    Exit(False);
  C := FSubject[FPos];
  case C of
    #10: Handled := ParseNewline(Block);
    '\': Handled := ParseBackslash(Block);
    '`': Handled := ParseBackticks(Block);
    '*', '_': Handled := HandleDelim(C, Block);
    '[':
      Handled := ((mexWikiLinks in FExtensions) and ParseWikiLink(Block)) or ParseOpenBracket(Block);
    '!': Handled := ParseBang(Block);
    ']': Handled := ParseCloseBracket(Block);
    '<': Handled := ParseAutolink(Block) or ParseHtmlTag(Block);
    '&': Handled := ParseEntity(Block);
    '~', '^', '+', '=':
      if IsSpecialChar(C) then
        Handled := HandleDelim(C, Block)
      else
        Handled := ParseString(Block);
    '$':
      Handled := ((mexMath in FExtensions) and ParseMath(Block)) or ParseString(Block);
    'w', 'h', 'H', 'f', 'F':
      // when the extension is off these are ordinary characters
      Handled := ((mexAutolinks in FExtensions) and ParseExtendedAutolink(Block)) or
        ParseString(Block);
  else
    Handled := ParseString(Block);
  end;
  if not Handled then
  begin
    Inc(FPos);
    Block.AppendChild(NewText(C));
  end;
  Result := True;
end;

function TMarkdownInlineParser.ParseString(Block: TMarkdownNode): Boolean;
var
  Start: Integer;
begin
  Start := FPos;
  while (FPos <= Length(FSubject)) and not IsSpecialChar(FSubject[FPos]) do
    Inc(FPos);
  Result := FPos > Start;
  if Result then
    Block.AppendChild(NewText(Copy(FSubject, Start, FPos - Start)));
end;

function TMarkdownInlineParser.ParseNewline(Block: TMarkdownNode): Boolean;
var
  LastC: TMarkdownNode;
  Lit: string;
  HardBreak: Boolean;
  P: Integer;
begin
  Inc(FPos); // the line ending
  LastC := Block.LastChild;
  if (LastC <> nil) and (LastC.Kind = nkText) and (LastC.Literal <> '') and
     (LastC.Literal[Length(LastC.Literal)] = ' ') then
  begin
    Lit := LastC.Literal;
    HardBreak := (Length(Lit) >= 2) and (Lit[Length(Lit) - 1] = ' ');
    P := Length(Lit);
    while (P > 0) and (Lit[P] = ' ') do
      Dec(P);
    LastC.Literal := Copy(Lit, 1, P);
    if HardBreak then
      Block.AppendChild(NewNode(nkHardBreak))
    else
      Block.AppendChild(NewNode(nkSoftBreak));
  end
  else
    Block.AppendChild(NewNode(nkSoftBreak));
  // gobble the leading spaces of the next line
  while (FPos <= Length(FSubject)) and (FSubject[FPos] = ' ') do
    Inc(FPos);
  Result := True;
end;

function TMarkdownInlineParser.ParseBackslash(Block: TMarkdownNode): Boolean;
begin
  Inc(FPos);
  if Peek = #10 then
  begin
    Inc(FPos);
    Block.AppendChild(NewNode(nkHardBreak));
    // the leading spaces of the next line are not content
    while (FPos <= Length(FSubject)) and (FSubject[FPos] = ' ') do
      Inc(FPos);
  end
  else if (FPos <= Length(FSubject)) and IsAsciiPunctuation(FSubject[FPos]) then
  begin
    Block.AppendChild(NewText(FSubject[FPos]));
    Inc(FPos);
  end
  else
    Block.AppendChild(NewText('\'));
  Result := True;
end;

function TMarkdownInlineParser.ParseBackticks(Block: TMarkdownNode): Boolean;
var
  Start, TickLen, AfterOpen, RunStart, RunLen, LastStart: Integer;
  Contents: string;
  Node: TMarkdownNode;
  I: Integer;
  AllSpaces: Boolean;
begin
  Start := FPos;
  while Peek = '`' do
    Inc(FPos);
  TickLen := FPos - Start;
  AfterOpen := FPos;

  // known not to have a closer of this length after this point?
  if FBackticksScanned and
     (not FBacktickRuns.TryGetValue(TickLen, LastStart) or (LastStart < AfterOpen)) then
  begin
    Block.AppendChild(NewText(Copy(FSubject, Start, TickLen)));
    Exit(True);
  end;

  while FPos <= Length(FSubject) do
  begin
    if FSubject[FPos] <> '`' then
    begin
      Inc(FPos);
      Continue;
    end;
    RunStart := FPos;
    while Peek = '`' do
      Inc(FPos);
    RunLen := FPos - RunStart;
    FBacktickRuns.AddOrSetValue(RunLen, RunStart);
    if RunLen = TickLen then
    begin
      Node := NewNode(nkCode);
      Contents := Copy(FSubject, AfterOpen, RunStart - AfterOpen);
      // line endings become spaces
      for I := 1 to Length(Contents) do
        if Contents[I] = #10 then
          Contents[I] := ' ';
      // strip one space on each side, unless the content is only spaces
      AllSpaces := True;
      for I := 1 to Length(Contents) do
        if Contents[I] <> ' ' then
        begin
          AllSpaces := False;
          Break;
        end;
      if (Length(Contents) >= 2) and not AllSpaces and (Contents[1] = ' ') and
         (Contents[Length(Contents)] = ' ') then
        Contents := Copy(Contents, 2, Length(Contents) - 2);
      Node.Literal := Contents;
      Block.AppendChild(Node);
      Exit(True);
    end;
  end;
  // no closing run: the opening backticks are literal text
  FBackticksScanned := True;
  FPos := AfterOpen;
  Block.AppendChild(NewText(Copy(FSubject, Start, TickLen)));
  Result := True;
end;

function TMarkdownInlineParser.ScanDelims(C: Char; out NumDelims: Integer;
  out CanOpen, CanClose: Boolean): Boolean;
var
  StartPos, BeforeIdx: Integer;
  BeforeIsWhitespace, BeforeIsPunctuation, AfterIsWhitespace, AfterIsPunctuation: Boolean;
  LeftFlanking, RightFlanking: Boolean;
begin
  StartPos := FPos;
  NumDelims := 0;
  while Peek = C do
  begin
    Inc(NumDelims);
    Inc(FPos);
  end;
  Result := NumDelims > 0;
  if not Result then
    Exit;

  // the start and the end of the subject count as whitespace
  if StartPos = 1 then
  begin
    BeforeIsWhitespace := True;
    BeforeIsPunctuation := False;
  end
  else
  begin
    BeforeIdx := PreviousCodePointIndex(FSubject, StartPos);
    BeforeIsWhitespace := IsUnicodeWhitespace(FSubject[BeforeIdx]);
    BeforeIsPunctuation := IsUnicodePunctuationAt(FSubject, BeforeIdx);
  end;
  if FPos > Length(FSubject) then
  begin
    AfterIsWhitespace := True;
    AfterIsPunctuation := False;
  end
  else
  begin
    AfterIsWhitespace := IsUnicodeWhitespace(FSubject[FPos]);
    AfterIsPunctuation := IsUnicodePunctuationAt(FSubject, FPos);
  end;

  LeftFlanking := not AfterIsWhitespace and
    (not AfterIsPunctuation or BeforeIsWhitespace or BeforeIsPunctuation);
  RightFlanking := not BeforeIsWhitespace and
    (not BeforeIsPunctuation or AfterIsWhitespace or AfterIsPunctuation);
  if C = '_' then
  begin
    CanOpen := LeftFlanking and (not RightFlanking or BeforeIsPunctuation);
    CanClose := RightFlanking and (not LeftFlanking or AfterIsPunctuation);
  end
  else
  begin
    CanOpen := LeftFlanking;
    CanClose := RightFlanking;
  end;
  FPos := StartPos;
end;

function TMarkdownInlineParser.HandleDelim(C: Char; Block: TMarkdownNode): Boolean;
var
  NumDelims, StartPos: Integer;
  CanOpen, CanClose: Boolean;
  Node: TMarkdownNode;
  Delim: TInlineDelimiter;
begin
  Result := ScanDelims(C, NumDelims, CanOpen, CanClose);
  if not Result then
    Exit;
  StartPos := FPos;
  Inc(FPos, NumDelims);
  Node := NewText(Copy(FSubject, StartPos, NumDelims));
  Block.AppendChild(Node);
  if CanOpen or CanClose then
  begin
    Delim := TInlineDelimiter.Create;
    Delim.DelimChar := C;
    Delim.NumDelims := NumDelims;
    Delim.OrigDelims := NumDelims;
    Delim.Node := Node;
    Delim.Previous := FDelimiters;
    Delim.Next := nil;
    Delim.CanOpen := CanOpen;
    Delim.CanClose := CanClose;
    if FDelimiters <> nil then
      FDelimiters.Next := Delim;
    FDelimiters := Delim;
  end;
end;

procedure TMarkdownInlineParser.RemoveDelimiter(Delim: TInlineDelimiter);
begin
  if Delim.Previous <> nil then
    Delim.Previous.Next := Delim.Next;
  if Delim.Next = nil then
    FDelimiters := Delim.Previous // top of the stack
  else
    Delim.Next.Previous := Delim.Previous;
  Delim.Free;
end;

procedure TMarkdownInlineParser.RemoveDelimitersBetween(Bottom, Top: TInlineDelimiter);
var
  Delim, NextDelim: TInlineDelimiter;
begin
  if Bottom.Next = Top then
    Exit;
  Delim := Bottom.Next;
  while (Delim <> nil) and (Delim <> Top) do
  begin
    NextDelim := Delim.Next;
    Delim.Free;
    Delim := NextDelim;
  end;
  Bottom.Next := Top;
  Top.Previous := Bottom;
end;

procedure TMarkdownInlineParser.ProcessEmphasis(StackBottom: TInlineDelimiter);
var
  Opener, Closer, OldCloser, TempStack: TInlineDelimiter;
  OpenerInl, CloserInl, Emph, Tmp, NextNode: TMarkdownNode;
  OpenersBottom: array[0..25] of TInlineDelimiter;
  EmphKind: TMarkdownNodeKind;
  BottomIndex, UseDelims, I: Integer;
  OpenerFound, OddMatch: Boolean;
begin
  for I := Low(OpenersBottom) to High(OpenersBottom) do
    OpenersBottom[I] := StackBottom;

  // find the first closer above StackBottom; when the top of the stack is
  // StackBottom there is none (without this check the walk would go down the
  // whole stack, quadratic on long paragraphs with many links)
  Closer := FDelimiters;
  if Closer = StackBottom then
    Closer := nil;
  while (Closer <> nil) and (Closer.Previous <> StackBottom) do
    Closer := Closer.Previous;

  // move forward, looking for closers
  while Closer <> nil do
  begin
    if not Closer.CanClose then
    begin
      Closer := Closer.Next;
      Continue;
    end;

    // look back for the first matching opener; the search bound depends on
    // the delimiter kind, on "can open" and on the length mod 3 (* and _),
    // or on the run length (~, whose opener and closer must be equal)
    // ~ ^ + = need an opener with the same run length: their bound depends
    // on the length only
    case Closer.DelimChar of
      '_': BottomIndex := 2;
      '*': BottomIndex := 8;
      '~': BottomIndex := 14;
      '^': BottomIndex := 17;
      '+': BottomIndex := 20;
    else
      BottomIndex := 23;
    end;
    if BottomIndex >= 14 then
      Inc(BottomIndex, Min(Closer.OrigDelims, 3) - 1)
    else
    begin
      if Closer.CanOpen then
        Inc(BottomIndex, 3);
      Inc(BottomIndex, Closer.OrigDelims mod 3);
    end;

    Opener := Closer.Previous;
    OpenerFound := False;
    while (Opener <> nil) and (Opener <> StackBottom) and (Opener <> OpenersBottom[BottomIndex]) do
    begin
      if (Opener.DelimChar = Closer.DelimChar) and Opener.CanOpen then
      begin
        if (Closer.DelimChar <> '*') and (Closer.DelimChar <> '_') then
          // ~x~ ~~x~~ ^x^ ++x++ ==x==: same length on both sides, a known kind
          OddMatch := (Opener.NumDelims <> Closer.NumDelims) or
            not ExactDelimiterKind(Closer.DelimChar, Closer.NumDelims, EmphKind)
        else
          OddMatch := (Closer.CanOpen or Opener.CanClose) and (Closer.OrigDelims mod 3 <> 0) and
            ((Opener.OrigDelims + Closer.OrigDelims) mod 3 = 0);
        if not OddMatch then
        begin
          OpenerFound := True;
          Break;
        end;
      end;
      Opener := Opener.Previous;
    end;
    OldCloser := Closer;

    if not OpenerFound then
    begin
      Closer := Closer.Next;
      // lower bound for future searches of openers
      OpenersBottom[BottomIndex] := OldCloser.Previous;
      // a closer that cannot open and has no opener can be removed
      if not OldCloser.CanOpen then
        RemoveDelimiter(OldCloser);
      Continue;
    end;

    // number of delimiters used: 2 (strong) when both sides have at least 2
    if (Closer.DelimChar <> '*') and (Closer.DelimChar <> '_') then
      UseDelims := Closer.NumDelims
    else if (Closer.NumDelims >= 2) and (Opener.NumDelims >= 2) then
      UseDelims := 2
    else
      UseDelims := 1;
    OpenerInl := Opener.Node;
    CloserInl := Closer.Node;
    Dec(Opener.NumDelims, UseDelims);
    Dec(Closer.NumDelims, UseDelims);
    OpenerInl.Literal := Copy(OpenerInl.Literal, 1, Length(OpenerInl.Literal) - UseDelims);
    CloserInl.Literal := Copy(CloserInl.Literal, 1, Length(CloserInl.Literal) - UseDelims);

    // the content between the delimiters goes into the new node
    if (Closer.DelimChar <> '*') and (Closer.DelimChar <> '_') then
    begin
      ExactDelimiterKind(Closer.DelimChar, UseDelims, EmphKind);
      Emph := NewNode(EmphKind);
    end
    else if UseDelims = 1 then
      Emph := NewNode(nkEmphasis)
    else
      Emph := NewNode(nkStrong);
    Tmp := OpenerInl.Next;
    while (Tmp <> nil) and (Tmp <> CloserInl) do
    begin
      NextNode := Tmp.Next;
      Emph.AppendChild(Tmp);
      Tmp := NextNode;
    end;
    OpenerInl.InsertAfter(Emph);

    RemoveDelimitersBetween(Opener, Closer);

    // used up delimiters disappear, with their text nodes
    if Opener.NumDelims = 0 then
    begin
      DiscardNode(OpenerInl);
      RemoveDelimiter(Opener);
    end;
    if Closer.NumDelims = 0 then
    begin
      DiscardNode(CloserInl);
      TempStack := Closer.Next;
      RemoveDelimiter(Closer);
      Closer := TempStack;
    end;
  end;

  // remove all the delimiters above StackBottom
  while (FDelimiters <> nil) and (FDelimiters <> StackBottom) do
    RemoveDelimiter(FDelimiters);
end;

procedure TMarkdownInlineParser.AddBracket(Node: TMarkdownNode; Index: Integer; Image: Boolean);
var
  Bracket: TInlineBracket;
begin
  if FBrackets <> nil then
    FBrackets.BracketAfter := True;
  Bracket := TInlineBracket.Create;
  Bracket.Node := Node;
  Bracket.Previous := FBrackets;
  Bracket.PreviousDelimiter := FDelimiters;
  Bracket.Index := Index;
  Bracket.IsImage := Image;
  Bracket.Active := True;
  FBrackets := Bracket;
end;

procedure TMarkdownInlineParser.RemoveBracket;
var
  Bracket: TInlineBracket;
begin
  Bracket := FBrackets;
  FBrackets := Bracket.Previous;
  Bracket.Free;
end;

function TMarkdownInlineParser.ParseOpenBracket(Block: TMarkdownNode): Boolean;
var
  Node: TMarkdownNode;
begin
  Node := NewText('[');
  Block.AppendChild(Node);
  AddBracket(Node, FPos, False);
  Inc(FPos);
  Result := True;
end;

function TMarkdownInlineParser.ParseBang(Block: TMarkdownNode): Boolean;
var
  Node: TMarkdownNode;
begin
  Inc(FPos);
  if Peek = '[' then
  begin
    Node := NewText('![');
    Block.AppendChild(Node);
    AddBracket(Node, FPos, True);
    Inc(FPos);
  end
  else
    Block.AppendChild(NewText('!'));
  Result := True;
end;

function TMarkdownInlineParser.ParseCloseBracket(Block: TMarkdownNode): Boolean;
var
  StartPos, SavePos, BeforeLabel, LabelLen, BeforeTitle: Integer;
  Opener: TInlineBracket;
  IsImage, Matched, HasTitle: Boolean;
  Dest, Title, RefLabel: string;
  Ref: TLinkReference;
  Node, Tmp, NextNode, OpenerNode: TMarkdownNode;
  PrevDelimiter: TInlineDelimiter;
begin
  Result := True;
  Inc(FPos);
  StartPos := FPos;

  Opener := FBrackets;
  if Opener = nil then
  begin
    Block.AppendChild(NewText(']'));
    Exit;
  end;
  if not Opener.Active then
  begin
    Block.AppendChild(NewText(']'));
    RemoveBracket;
    Exit;
  end;

  IsImage := Opener.IsImage;
  Matched := False;
  Dest := '';
  Title := '';
  SavePos := FPos;

  // inline link: [text](destination "title")
  if Peek = '(' then
  begin
    Inc(FPos);
    SkipSpacesAndOneNewline(FSubject, FPos);
    if ScanLinkDestination(FSubject, FPos, Dest) then
    begin
      BeforeTitle := FPos;
      SkipSpacesAndOneNewline(FSubject, FPos);
      // the title must be separated from the destination by whitespace
      HasTitle := False;
      if FPos <> BeforeTitle then
        HasTitle := ScanLinkTitle(FSubject, FPos, Title);
      if not HasTitle then
        Title := '';
      SkipSpacesAndOneNewline(FSubject, FPos);
      if Peek = ')' then
      begin
        Inc(FPos);
        Matched := True;
      end;
    end;
    if not Matched then
      FPos := SavePos;
  end;

  if not Matched then
  begin
    // full, collapsed or shortcut reference link
    BeforeLabel := FPos;
    LabelLen := ScanLinkLabel(FSubject, FPos);
    RefLabel := '';
    if LabelLen > 2 then
      RefLabel := Copy(FSubject, BeforeLabel, LabelLen)
    else if not Opener.BracketAfter then
      // empty or missing second label: the first label is the reference
      RefLabel := Copy(FSubject, Opener.Index, StartPos - Opener.Index);
    if LabelLen = 0 then
      FPos := SavePos
    else
      Inc(FPos, LabelLen);
    if (RefLabel <> '') and (Length(RefLabel) - 2 <= 999) and
       FRefMap.TryGet(NormalizeLinkLabel(Copy(RefLabel, 2, Length(RefLabel) - 2)), Ref) then
    begin
      Dest := Ref.Destination;
      Title := Ref.Title;
      Matched := True;
    end;
  end;

  if not Matched then
  begin
    RemoveBracket;
    FPos := StartPos;
    Block.AppendChild(NewText(']'));
    Exit;
  end;

  if IsImage then
    Node := NewNode(nkImage)
  else
    Node := NewNode(nkLink);
  Node.Destination := Dest;
  Node.Title := Title;
  OpenerNode := Opener.Node;
  Tmp := OpenerNode.Next;
  while Tmp <> nil do
  begin
    NextNode := Tmp.Next;
    Node.AppendChild(Tmp);
    Tmp := NextNode;
  end;
  Block.AppendChild(Node);
  PrevDelimiter := Opener.PreviousDelimiter;
  ProcessEmphasis(PrevDelimiter);
  RemoveBracket;
  DiscardNode(OpenerNode);

  // no links in links: deactivate the earlier link openers
  if not IsImage then
  begin
    Opener := FBrackets;
    while Opener <> nil do
    begin
      if not Opener.IsImage then
        Opener.Active := False;
      Opener := Opener.Previous;
    end;
  end;
end;

function TMarkdownInlineParser.ParseAutolink(Block: TMarkdownNode): Boolean;
var
  I, Start, SchemeLen, DotPart: Integer;
  C: Char;
  Dest: string;
  Node: TMarkdownNode;
begin
  Result := False;
  Start := FPos + 1;
  // URI autolink: <scheme:...> with a 2..32 character scheme
  I := Start;
  if (I <= Length(FSubject)) and IsAsciiLetter(FSubject[I]) then
  begin
    Inc(I);
    while (I <= Length(FSubject)) and
          (IsAsciiAlphaNum(FSubject[I]) or CharInSet(FSubject[I], ['.', '+', '-'])) do
      Inc(I);
    SchemeLen := I - Start;
    if (SchemeLen >= 2) and (SchemeLen <= 32) and (I <= Length(FSubject)) and (FSubject[I] = ':') then
    begin
      Inc(I);
      while (I <= Length(FSubject)) and (FSubject[I] > ' ') and
            (FSubject[I] <> '<') and (FSubject[I] <> '>') do
        Inc(I);
      if (I <= Length(FSubject)) and (FSubject[I] = '>') then
      begin
        Dest := Copy(FSubject, Start, I - Start);
        Node := NewNode(nkLink);
        Node.Destination := NormalizeUri(Dest);
        Node.IsAutolink := True;
        Node.AppendChild(NewText(Dest));
        Block.AppendChild(Node);
        FPos := I + 1;
        Exit(True);
      end;
    end;
  end;

  // e-mail autolink
  I := Start;
  while (I <= Length(FSubject)) and
        (IsAsciiAlphaNum(FSubject[I]) or CharInSet(FSubject[I], ['.', '!', '#', '$', '%', '&', '''',
          '*', '+', '/', '=', '?', '^', '_', '`', '{', '|', '}', '~', '-'])) do
    Inc(I);
  if (I = Start) or (I > Length(FSubject)) or (FSubject[I] <> '@') then
    Exit;
  Inc(I);
  // domain labels: [a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])? separated by dots
  while True do
  begin
    DotPart := I;
    if (I > Length(FSubject)) or not IsAsciiAlphaNum(FSubject[I]) then
      Exit;
    while (I <= Length(FSubject)) and (IsAsciiAlphaNum(FSubject[I]) or (FSubject[I] = '-')) do
      Inc(I);
    if (I - DotPart > 63) or (FSubject[I - 1] = '-') then
      Exit;
    if I > Length(FSubject) then
      Exit;
    C := FSubject[I];
    if C = '>' then
      Break;
    if C <> '.' then
      Exit;
    Inc(I);
  end;
  Dest := Copy(FSubject, Start, I - Start);
  Node := NewNode(nkLink);
  Node.Destination := NormalizeUri('mailto:' + Dest);
  Node.IsAutolink := True;
  Node.AppendChild(NewText(Dest));
  Block.AppendChild(Node);
  FPos := I + 1;
  Result := True;
end;

function TMarkdownInlineParser.ParseHtmlTag(Block: TMarkdownNode): Boolean;
var
  Len: Integer;
  Node: TMarkdownNode;
begin
  Len := ScanHtml(FPos);
  Result := Len > 0;
  if not Result then
    Exit;
  Node := NewNode(nkHtmlInline);
  Node.Literal := Copy(FSubject, FPos, Len);
  Block.AppendChild(Node);
  Inc(FPos, Len);
end;

function TMarkdownInlineParser.ParseEntity(Block: TMarkdownNode): Boolean;
var
  Decoded: string;
  Len: Integer;
begin
  Result := TryDecodeEntity(FSubject, FPos, Decoded, Len);
  if Result then
  begin
    Block.AppendChild(NewText(Decoded));
    Inc(FPos, Len);
  end;
end;

procedure TMarkdownInlineParser.MergeTextNodes(Block: TMarkdownNode);
var
  Stack: TMarkdownNodeList;
  Container, Child, NextNode: TMarkdownNode;
begin
  // adjacent text nodes (produced by delimiters and brackets that did not
  // match) are merged; containers are visited with an explicit stack
  Stack := TMarkdownNodeList.Create;
  try
    Stack.Add(Block);
    while Stack.Count > 0 do
    begin
      Container := Stack[Stack.Count - 1];
      Stack.Delete(Stack.Count - 1);
      Child := Container.FirstChild;
      while Child <> nil do
      begin
        if Child.Kind = nkText then
        begin
          NextNode := Child.Next;
          while (NextNode <> nil) and (NextNode.Kind = nkText) do
          begin
            Child.Literal := Child.Literal + NextNode.Literal;
            DiscardNode(NextNode);
            NextNode := Child.Next;
          end;
        end
        else if Child.IsContainer then
          Stack.Add(Child);
        Child := Child.Next;
      end;
    end;
  finally
    Stack.Free;
  end;
end;

{ GFM extended autolinks }

function TMarkdownInlineParser.ParseExtendedAutolink(Block: TMarkdownNode): Boolean;
var
  Len: Integer;
  IsWww: Boolean;
  Text: string;
  Node: TMarkdownNode;
begin
  Result := False;
  // no autolinks inside the text of a link or an image
  if FBrackets <> nil then
    Exit;
  Len := 0;
  IsWww := FSubject[FPos] = 'w';
  if IsWww then
  begin
    if CanStartExtendedAutolink(FSubject, FPos) then
      Len := ScanWwwAutolink(FSubject, FPos);
  end
  else if (FPos = 1) or not IsAsciiLetter(FSubject[FPos - 1]) then
    Len := ScanUrlAutolink(FSubject, FPos);
  if Len = 0 then
    Exit;
  Text := Copy(FSubject, FPos, Len);
  Node := NewNode(nkLink);
  if IsWww then
    Node.Destination := NormalizeUri('http://' + Text)
  else
    Node.Destination := NormalizeUri(Text);
  Node.IsAutolink := True;
  Node.AppendChild(NewText(Text));
  Block.AppendChild(Node);
  Inc(FPos, Len);
  Result := True;
end;

procedure TMarkdownInlineParser.ProcessEmailAutolinks(Block: TMarkdownNode);
var
  Stack, Texts: TMarkdownNodeList;
  Container, Child: TMarkdownNode;
begin
  // e-mail autolinks are found in the text nodes, outside links and images
  Stack := TMarkdownNodeList.Create;
  Texts := TMarkdownNodeList.Create;
  try
    Stack.Add(Block);
    while Stack.Count > 0 do
    begin
      Container := Stack[Stack.Count - 1];
      Stack.Delete(Stack.Count - 1);
      Child := Container.FirstChild;
      while Child <> nil do
      begin
        if (Child.Kind = nkText) and (Pos('@', Child.Literal) > 0) then
          Texts.Add(Child)
        else if Child.IsContainer and not (Child.Kind in [nkLink, nkImage]) then
          Stack.Add(Child);
        Child := Child.Next;
      end;
    end;
    for Child in Texts do
      SplitEmailAutolinks(Child);
  finally
    Texts.Free;
    Stack.Free;
  end;
end;

procedure TMarkdownInlineParser.SplitEmailAutolinks(TextNode: TMarkdownNode);
var
  S, Address: string;
  From, Start, Len: Integer;
  Last, Link, Piece: TMarkdownNode;
  Found: Boolean;
begin
  S := TextNode.Literal;
  From := 1;
  Last := TextNode;
  Found := False;
  while FindEmailAutolink(S, From, Start, Len) do
  begin
    if not Found then
      TextNode.Literal := Copy(S, From, Start - From)
    else if Start > From then
    begin
      Piece := NewText(Copy(S, From, Start - From));
      Last.InsertAfter(Piece);
      Last := Piece;
    end;
    Found := True;
    Address := Copy(S, Start, Len);
    Link := NewNode(nkLink);
    Link.Destination := NormalizeUri('mailto:' + Address);
    Link.IsAutolink := True;
    Link.AppendChild(NewText(Address));
    Last.InsertAfter(Link);
    Last := Link;
    From := Start + Len;
  end;
  if not Found then
    Exit;
  if From <= Length(S) then
    Last.InsertAfter(NewText(Copy(S, From, MaxInt)));
  if TextNode.Literal = '' then
    DiscardNode(TextNode);
end;


{ Math }

function TMarkdownInlineParser.ParseMath(Block: TMarkdownNode): Boolean;
var
  Literal: string;
  IsDisplay: Boolean;
  Len: Integer;
  Node: TMarkdownNode;
begin
  Result := ScanInlineMath(FSubject, FPos, FMathCache, Literal, IsDisplay, Len);
  if not Result then
  begin
    // a dollar run that is not a formula is plain text, as a whole
    Len := 0;
    while (FPos + Len <= Length(FSubject)) and (FSubject[FPos + Len] = '$') do
      Inc(Len);
    Block.AppendChild(NewText(Copy(FSubject, FPos, Len)));
    Inc(FPos, Len);
    Exit(True);
  end;
  Node := NewNode(nkMathInline);
  Node.Literal := Literal;
  Node.IsDisplay := IsDisplay;
  Block.AppendChild(Node);
  Inc(FPos, Len);
end;


{ Legacy extensions }

function TMarkdownInlineParser.ExactDelimiterKind(C: Char; Count: Integer;
  out Kind: TMarkdownNodeKind): Boolean;
begin
  Result := True;
  Kind := nkStrikethrough;
  case C of
    '~':
      // with both subscript and strikethrough: ~x~ is subscript, ~~x~~ strikethrough
      if (Count = 1) and (mexSubscript in FExtensions) then
        Kind := nkSubscript
      else if (Count <= 2) and (mexStrikethrough in FExtensions) then
        Kind := nkStrikethrough
      else
        Result := False;
    '^':
      if (Count = 1) and (mexSuperscript in FExtensions) then
        Kind := nkSuperscript
      else
        Result := False;
    '+':
      if (Count = 2) and (mexInsert in FExtensions) then
        Kind := nkInsert
      else
        Result := False;
    '=':
      if (Count = 2) and (mexMark in FExtensions) then
        Kind := nkMark
      else
        Result := False;
  else
    Result := False;
  end;
end;

function TMarkdownInlineParser.ParseWikiLink(Block: TMarkdownNode): Boolean;
var
  CloseAt: Integer;
  Inner: string;
  Node: TMarkdownNode;
  Sub: TMarkdownInlineParser;
begin
  // [[content]]: the content is parsed as inlines and given, rendered, to
  // TConfiguration.specialLinkEmitter
  Result := False;
  if (FPos + 1 > Length(FSubject)) or (FSubject[FPos + 1] <> '[') then
    Exit;
  if (FNoWikiCloseFrom > 0) and (FPos >= FNoWikiCloseFrom) then
    Exit;
  CloseAt := PosEx(']]', FSubject, FPos + 2);
  if CloseAt = 0 then
    FNoWikiCloseFrom := FPos;
  if CloseAt <= FPos + 2 then
    Exit;
  CloseAt := CloseAt - FPos - 1;
  Inner := Copy(FSubject, FPos + 2, CloseAt - 1);
  if (Pos('[', Inner) > 0) or (Pos(#10, Inner) > 0) then
    Exit;
  Node := NewNode(nkWikiLink);
  Node.Literal := Inner;
  Block.AppendChild(Node);
  Sub := TMarkdownInlineParser.Create(FDocument, FRefMap);
  try
    Sub.Extensions := FExtensions - [mexWikiLinks];
    Sub.Parse(Node, Inner);
  finally
    Sub.Free;
  end;
  Inc(FPos, CloseAt + 3);
  Result := True;
end;

procedure TMarkdownInlineParser.ProcessSmartTypography(Block: TMarkdownNode);
var
  Stack: TMarkdownNodeList;
  Container, Child: TMarkdownNode;
begin
  // text nodes only (code spans, raw HTML and math are not text), and not the
  // text of autolinks, which must stay equal to their destination
  Stack := TMarkdownNodeList.Create;
  try
    Stack.Add(Block);
    while Stack.Count > 0 do
    begin
      Container := Stack[Stack.Count - 1];
      Stack.Delete(Stack.Count - 1);
      Child := Container.FirstChild;
      while Child <> nil do
      begin
        if Child.Kind = nkText then
          Child.Literal := ApplySmartTypography(Child.Literal)
        else if Child.IsContainer and not ((Child.Kind = nkLink) and Child.IsAutolink) then
          Stack.Add(Child);
        Child := Child.Next;
      end;
    end;
  finally
    Stack.Free;
  end;
end;


{ Raw HTML with cached closer searches }

function TMarkdownInlineParser.FindCloser(Slot: Integer; const Closer: string; From: Integer): Integer;
begin
  // the searches move forward: a cached result is reused while it is still
  // valid, so texts full of unclosed "<!--" stay linear
  if (FCloserFrom[Slot] > 0) and (From >= FCloserFrom[Slot]) then
  begin
    if FCloserAt[Slot] = 0 then
      Exit(0);
    if FCloserAt[Slot] >= From then
      Exit(FCloserAt[Slot]);
  end;
  Result := PosEx(Closer, FSubject, From);
  FCloserFrom[Slot] := From;
  FCloserAt[Slot] := Result;
end;

function TMarkdownInlineParser.ScanHtml(Index: Integer): Integer;
var
  EndPos: Integer;
begin
  Result := 0;
  if Index >= Length(FSubject) then
    Exit;
  case FSubject[Index + 1] of
    '?':
      begin
        EndPos := FindCloser(1, '?>', Index + 2);
        if EndPos > 0 then
          Result := EndPos + 2 - Index;
      end;
    '!':
      if StartsAt(FSubject, Index, '<!--') then
      begin
        if StartsAt(FSubject, Index, '<!-->') then
          Exit(5);
        if StartsAt(FSubject, Index, '<!--->') then
          Exit(6);
        EndPos := FindCloser(0, '-->', Index + 4);
        if EndPos > 0 then
          Result := EndPos + 3 - Index;
      end
      else if StartsAt(FSubject, Index, '<![CDATA[') then
      begin
        EndPos := FindCloser(2, ']]>', Index + 9);
        if EndPos > 0 then
          Result := EndPos + 3 - Index;
      end
      else if (Index + 2 <= Length(FSubject)) and IsAsciiLetter(FSubject[Index + 2]) then
      begin
        EndPos := FindCloser(3, '>', Index + 3);
        if EndPos > 0 then
          Result := EndPos + 1 - Index;
      end;
  else
    Result := ScanHtmlInline(FSubject, Index);
  end;
end;

end.
