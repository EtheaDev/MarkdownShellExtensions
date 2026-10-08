{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       CommonMark / GFM engine: GitHub Flavored Markdown extensions           }
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
{  The rules follow the GFM specification 0.29 and its reference              }
{  implementation cmark-gfm (BSD-2-Clause, GitHub, Inc.).                       }
{                                                                              }
{******************************************************************************}
unit MarkdownGFM;

{ Helpers of the five GFM 0.29 extensions, used by the block parser (tables,
  task list items), the inline parser (strikethrough, extended autolinks) and
  the renderer (tag filter). }

interface

uses
  System.SysUtils,
  MarkdownAST;

type
  TTableCells = TArray<string>;
  TTableAligns = TArray<TMarkdownTableAlign>;

{ Tables }

/// <summary>Splits a table row into trimmed cells: leading and trailing pipes
/// are optional, "\|" does not split. Returns an empty array for a blank line.</summary>
function SplitTableRow(const Line: string): TTableCells;
/// <summary>True if Line is a delimiter row (cells like ---, :--, --:, :-:);
/// Aligns receives the alignment of each column.</summary>
function ParseTableDelimiterRow(const Line: string; out Aligns: TTableAligns): Boolean;
/// <summary>Replaces "\|" with "|" in a cell, before its inline parsing.</summary>
function UnescapeTablePipes(const Cell: string): string;

{ Task list items }

/// <summary>Matches "[ ]", "[x]" or "[X]" followed by a space or tab at the start
/// of the first paragraph of a list item. Len is 3 (the marker only).</summary>
function MatchTaskListMarker(const Content: string; out Checked: Boolean): Boolean;

{ Extended autolinks (positions are 1-based in S) }

/// <summary>Length of a www. autolink starting at S[Index], or 0.</summary>
function ScanWwwAutolink(const S: string; Index: Integer): Integer;
/// <summary>Length of an http://, https:// or ftp:// autolink starting at S[Index], or 0.</summary>
function ScanUrlAutolink(const S: string; Index: Integer): Integer;
/// <summary>True when an extended autolink may start at Index (start of text,
/// whitespace, or one of * _ ~ ( before it).</summary>
function CanStartExtendedAutolink(const S: string; Index: Integer): Boolean;
/// <summary>Finds the first e-mail autolink in S at or after From. Start and Len
/// locate the address.</summary>
function FindEmailAutolink(const S: string; From: Integer; out Start, Len: Integer): Boolean;

{ Disallowed raw HTML }

/// <summary>Replaces "&lt;" with "&amp;lt;" for the tags filtered by GFM: title,
/// textarea, style, xmp, iframe, noembed, noframes, script, plaintext.</summary>
function FilterDisallowedRawHtml(const Html: string): string;

implementation

uses
  MarkdownTextUtils;

{ Tables }

function SplitTableRow(const Line: string): TTableCells;
var
  S: string;
  I, Start, Count: Integer;
  Cells: TTableCells;

  procedure AddCell(const Cell: string);
  begin
    if Count = Length(Cells) then
      SetLength(Cells, Count * 2 + 4);
    Cells[Count] := TrimSpaceTab(Cell);
    Inc(Count);
  end;

begin
  Count := 0;
  SetLength(Cells, 0);
  S := TrimSpaceTab(Line);
  if S = '' then
    Exit(nil);
  I := 1;
  if S[1] = '|' then
    Inc(I);
  Start := I;
  while I <= Length(S) do
  begin
    if (S[I] = '\') and (I < Length(S)) then
      Inc(I, 2)
    else if S[I] = '|' then
    begin
      AddCell(Copy(S, Start, I - Start));
      Inc(I);
      Start := I;
    end
    else
      Inc(I);
  end;
  // the last cell, unless the row ends with a pipe
  if Start <= Length(S) then
    AddCell(Copy(S, Start, Length(S) - Start + 1))
  else if (Count = 0) then
    AddCell('');
  SetLength(Cells, Count);
  Result := Cells;
end;

function ParseTableDelimiterRow(const Line: string; out Aligns: TTableAligns): Boolean;
var
  Cells: TTableCells;
  I, J, First, Last: Integer;
  Cell: string;
  Left, Right: Boolean;
begin
  Result := False;
  SetLength(Aligns, 0);
  Cells := SplitTableRow(Line);
  if Length(Cells) = 0 then
    Exit;
  SetLength(Aligns, Length(Cells));
  for I := 0 to High(Cells) do
  begin
    Cell := Cells[I];
    if Cell = '' then
      Exit;
    First := 1;
    Last := Length(Cell);
    Left := Cell[First] = ':';
    if Left then
      Inc(First);
    Right := (Last >= First) and (Cell[Last] = ':');
    if Right then
      Dec(Last);
    if Last < First then
      Exit;
    for J := First to Last do
      if Cell[J] <> '-' then
        Exit;
    if Left and Right then
      Aligns[I] := mtaCenter
    else if Left then
      Aligns[I] := mtaLeft
    else if Right then
      Aligns[I] := mtaRight
    else
      Aligns[I] := mtaNone;
  end;
  Result := True;
end;

function UnescapeTablePipes(const Cell: string): string;
begin
  Result := StringReplace(Cell, '\|', '|', [rfReplaceAll]);
end;

{ Task list items }

function MatchTaskListMarker(const Content: string; out Checked: Boolean): Boolean;
begin
  Checked := False;
  Result := (Length(Content) >= 4) and (Content[1] = '[') and (Content[3] = ']') and
    CharInSet(Content[2], [' ', 'x', 'X']) and IsSpaceOrTab(Content[4]);
  if Result then
    Checked := Content[2] <> ' ';
end;

{ Extended autolinks }

function IsAsciiSpace(C: Char): Boolean; inline;
begin
  Result := (C = ' ') or (C = #9) or (C = #10) or (C = #11) or (C = #12) or (C = #13);
end;

function IsValidHostChar(const S: string; Index: Integer): Boolean;
var
  C: Char;
begin
  C := S[Index];
  if Ord(C) < $80 then
    Result := IsAsciiAlphaNum(C)
  else
    Result := not IsUnicodeWhitespace(C) and not IsUnicodePunctuationAt(S, Index);
end;

// cmark-gfm check_domain: Index is the first character of the domain;
// returns the length of the domain part, or 0 when it is not valid.
function CheckDomain(const S: string; Index: Integer; AllowShort: Boolean): Integer;
var
  I, Size, Np, UScore1, UScore2: Integer;
begin
  Size := Length(S) - Index + 1;
  Np := 0;
  UScore1 := 0;
  UScore2 := 0;
  I := 1;
  while I < Size - 1 do
  begin
    case S[Index + I] of
      '_': Inc(UScore2);
      '.':
        begin
          UScore1 := UScore2;
          UScore2 := 0;
          Inc(Np);
        end;
      '-': ;
    else
      if not IsValidHostChar(S, Index + I) then
        Break;
    end;
    Inc(I);
  end;
  if (UScore1 > 0) or (UScore2 > 0) then
    Exit(0);
  if AllowShort or (Np > 0) then
    Result := I
  else
    Result := 0;
end;

// cmark-gfm autolink_delim: trims the trailing characters that are not part
// of the link. Index is the start of the link, LinkEnd its length.
function AutolinkDelim(const S: string; Index, LinkEnd: Integer): Integer;
var
  I, NewEnd, Opening, Closing: Integer;
  C: Char;
begin
  for I := 0 to LinkEnd - 1 do
    if S[Index + I] = '<' then
    begin
      LinkEnd := I;
      Break;
    end;

  while LinkEnd > 0 do
  begin
    C := S[Index + LinkEnd - 1];
    if CharInSet(C, ['?', '!', '.', ',', ':', '*', '_', '~', '''', '"']) then
      Dec(LinkEnd)
    else if C = ';' then
    begin
      // a trailing entity-like sequence (&xxx;) is not part of the link
      NewEnd := LinkEnd - 2;
      while (NewEnd > 0) and IsAsciiLetter(S[Index + NewEnd]) do
        Dec(NewEnd);
      if (NewEnd < LinkEnd - 2) and (S[Index + NewEnd] = '&') then
        LinkEnd := NewEnd
      else
        Dec(LinkEnd);
    end
    else if C = ')' then
    begin
      // keep balanced parentheses, drop an unbalanced closing one
      Opening := 0;
      Closing := 0;
      for I := 0 to LinkEnd - 1 do
        if S[Index + I] = '(' then
          Inc(Opening)
        else if S[Index + I] = ')' then
          Inc(Closing);
      if Closing <= Opening then
        Break;
      Dec(LinkEnd);
    end
    else
      Break;
  end;
  Result := LinkEnd;
end;

function CanStartExtendedAutolink(const S: string; Index: Integer): Boolean;
var
  C: Char;
begin
  if Index <= 1 then
    Exit(True);
  C := S[Index - 1];
  Result := IsAsciiSpace(C) or CharInSet(C, ['*', '_', '~', '(']);
end;

function ExtendToWhitespace(const S: string; Index, LinkEnd: Integer): Integer;
begin
  while (Index + LinkEnd <= Length(S)) and not IsAsciiSpace(S[Index + LinkEnd]) do
    Inc(LinkEnd);
  Result := LinkEnd;
end;

function ScanWwwAutolink(const S: string; Index: Integer): Integer;
var
  LinkEnd: Integer;
begin
  Result := 0;
  if not StartsAt(S, Index, 'www.') then
    Exit;
  LinkEnd := CheckDomain(S, Index, False);
  if LinkEnd = 0 then
    Exit;
  LinkEnd := ExtendToWhitespace(S, Index, LinkEnd);
  Result := AutolinkDelim(S, Index, LinkEnd);
end;

function ScanUrlAutolink(const S: string; Index: Integer): Integer;
var
  SchemeLen, DomainLen, LinkEnd: Integer;
begin
  Result := 0;
  if StartsAtIgnoreCase(S, Index, 'http://') then
    SchemeLen := 7
  else if StartsAtIgnoreCase(S, Index, 'https://') then
    SchemeLen := 8
  else if StartsAtIgnoreCase(S, Index, 'ftp://') then
    SchemeLen := 6
  else
    Exit;
  if (Index + SchemeLen > Length(S)) or not IsAsciiAlphaNum(S[Index + SchemeLen]) then
    Exit;
  DomainLen := CheckDomain(S, Index + SchemeLen, True);
  if DomainLen = 0 then
    Exit;
  LinkEnd := ExtendToWhitespace(S, Index, SchemeLen + DomainLen);
  Result := AutolinkDelim(S, Index, LinkEnd);
end;

function FindEmailAutolink(const S: string; From: Integer; out Start, Len: Integer): Boolean;
var
  At, Rewind, LinkEnd, Nb, Np: Integer;
  C: Char;
  SlashSeen: Boolean;
begin
  Result := False;
  Start := 0;
  Len := 0;
  At := From;
  while True do
  begin
    // next '@'
    while (At <= Length(S)) and (S[At] <> '@') do
      Inc(At);
    if At > Length(S) then
      Exit;

    // local part: [A-Za-z0-9.+-_]+ before the '@', not preceded by '/'
    Rewind := 0;
    SlashSeen := False;
    while At - Rewind - 1 >= From do
    begin
      C := S[At - Rewind - 1];
      if IsAsciiAlphaNum(C) or CharInSet(C, ['.', '+', '-', '_']) then
        Inc(Rewind)
      else
      begin
        SlashSeen := C = '/';
        Break;
      end;
    end;
    if (Rewind > 0) and not SlashSeen then
    begin
      // domain: alphanumerics, '-', '_', and dots followed by an alphanumeric
      Nb := 0;
      Np := 0;
      LinkEnd := 0;
      while At + LinkEnd <= Length(S) do
      begin
        C := S[At + LinkEnd];
        if IsAsciiAlphaNum(C) then
        else if C = '@' then
        begin
          Inc(Nb);
          // a second @ makes the candidate invalid (as cmark-gfm): stop here,
          // which keeps "a@b@c@..." linear
          if Nb > 1 then
            Break;
        end
        else if (C = '.') and (At + LinkEnd < Length(S)) and IsAsciiAlphaNum(S[At + LinkEnd + 1]) then
          Inc(Np)
        else if (C <> '-') and (C <> '_') then
          Break;
        Inc(LinkEnd);
      end;
      if (LinkEnd >= 2) and (Nb = 1) and (Np > 0) and
         (IsAsciiLetter(S[At + LinkEnd - 1]) or (S[At + LinkEnd - 1] = '.')) then
      begin
        LinkEnd := AutolinkDelim(S, At, LinkEnd);
        if LinkEnd > 0 then
        begin
          Start := At - Rewind;
          Len := Rewind + LinkEnd;
          Exit(True);
        end;
      end;
    end;
    Inc(At);
  end;
end;

{ Disallowed raw HTML }

function IsFilteredTag(const S: string; Index: Integer): Boolean;
const
  Tags: array[0..8] of string = ('title', 'textarea', 'style', 'xmp', 'iframe',
    'noembed', 'noframes', 'script', 'plaintext');
var
  K, I: Integer;
  C: Char;
begin
  Result := False;
  I := Index + 1;
  if (I <= Length(S)) and (S[I] = '/') then
    Inc(I);
  for K := Low(Tags) to High(Tags) do
    if StartsAtIgnoreCase(S, I, Tags[K]) then
    begin
      if I + Length(Tags[K]) > Length(S) then
        Exit(False);
      C := S[I + Length(Tags[K])];
      if IsAsciiSpace(C) or (C = '>') then
        Exit(True);
      if (C = '/') and (I + Length(Tags[K]) + 1 <= Length(S)) and (S[I + Length(Tags[K]) + 1] = '>') then
        Exit(True);
    end;
end;

function FilterDisallowedRawHtml(const Html: string): string;
var
  I: Integer;
  SB: TStringBuilder;
begin
  if Pos('<', Html) = 0 then
    Exit(Html);
  SB := TStringBuilder.Create(Length(Html) + 16);
  try
    for I := 1 to Length(Html) do
      if (Html[I] = '<') and IsFilteredTag(Html, I) then
        SB.Append('&lt;')
      else
        SB.Append(Html[I]);
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

end.
