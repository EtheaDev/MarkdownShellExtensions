{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       CommonMark / GFM engine: math extension ($...$, $$...$$, ```math)      }
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
{  The rules of the $ syntax are adapted from Markdown4D (MIT, (c) 2026 GDK     }
{  Software), whose corpus Tests/specs/math.json pins them:                    }
{  see Tests/LICENSE-Markdown4D.txt.                                           }
{                                                                              }
{******************************************************************************}
unit MarkdownMath;

{ Math extension (mexMath). Output is KaTeX/MathJax ready:
    inline   $x$     -> <span class="math">\(x\)</span>
    inline   $$x$$   -> <span class="math">\[x\]</span>
    block    $$ ... $$ lines, or a ```math fence -> <div class="math">\[ ... \]</div>
  Rules of the inline form: the opening run may not follow a letter or digit
  and may not be followed by whitespace; the closing run may not follow
  whitespace and may not be followed by a letter or digit. A single-dollar
  formula stays on one line; a backtick (code span) or "\$" never closes it.
  This keeps "$100 and $200" plain text. The GitLab form $`...`$ is accepted. }

interface

uses
  System.SysUtils;

type
  /// <summary>Per-text cache of failed closer searches (keeps the scan
  /// linear on texts full of unmatched dollars). Zero it before a new text.</summary>
  TMathScanCache = array[0..2] of Integer;

/// <summary>Scans an inline formula starting at S[Index] = '$'. On success
/// Literal is the formula, IsDisplay is True for $$...$$, and Len is the
/// number of source characters consumed.</summary>
function ScanInlineMath(const S: string; Index: Integer; var Cache: TMathScanCache;
  out Literal: string; out IsDisplay: Boolean; out Len: Integer): Boolean;

/// <summary>True if the line, from Index, holds exactly "$$" and then only
/// spaces or tabs: the fence of a display math block.</summary>
function IsMathFenceLine(const Line: string; Index: Integer): Boolean;

/// <summary>True if the first word of a fenced code block info string is
/// "math" (any case): the block is display math.</summary>
function IsMathInfoString(const Info: string): Boolean;

implementation

uses
  System.StrUtils,
  MarkdownTextUtils;

const
  MaxMathDelimiterLength = 2;

function DollarRun(const S: string; Index: Integer): Integer;
begin
  Result := 0;
  while (Index + Result <= Length(S)) and (S[Index + Result] = '$') do
    Inc(Result);
end;

function CharBeforeIsWord(const S: string; Index: Integer): Boolean;
begin
  if Index <= 1 then
    Result := False
  else
    Result := IsLetterOrDigitAt(S, PreviousCodePointIndex(S, Index));
end;

function ScanBacktickMath(const S: string; Index: Integer; var Cache: TMathScanCache;
  out Literal: string; out Len: Integer): Boolean;
var
  ContentStart, CloserStart: Integer;
begin
  // $`formula`$
  Result := False;
  if CharBeforeIsWord(S, Index) then
    Exit;
  ContentStart := Index + 2;
  // no "`$" after an earlier position: none after this one either
  if (Cache[0] > 0) and (ContentStart >= Cache[0]) then
    Exit;
  CloserStart := PosEx('`$', S, ContentStart);
  if CloserStart = 0 then
    Cache[0] := ContentStart;
  if CloserStart <= ContentStart then
    Exit;
  Literal := Copy(S, ContentStart, CloserStart - ContentStart);
  Len := CloserStart + 2 - Index;
  Result := True;
end;

function ScanInlineMath(const S: string; Index: Integer; var Cache: TMathScanCache;
  out Literal: string; out IsDisplay: Boolean; out Len: Integer): Boolean;
var
  Delim, ContentStart, I, Run, CloserStart: Integer;
  C: Char;
begin
  Result := False;
  Literal := '';
  IsDisplay := False;
  Len := 0;
  Delim := DollarRun(S, Index);
  if Delim > MaxMathDelimiterLength then
    Delim := MaxMathDelimiterLength;
  ContentStart := Index + Delim;

  if (Delim = 1) and (ContentStart <= Length(S)) and (S[ContentStart] = '`') then
    Exit(ScanBacktickMath(S, Index, Cache, Literal, Len));

  // opener: not glued to a preceding word, not followed by whitespace
  if CharBeforeIsWord(S, Index) or (ContentStart > Length(S)) or
     IsUnicodeWhitespace(S[ContentStart]) then
    Exit;
  // a closer valid for this opener would have been valid for an earlier one
  // whose search failed up to Cache[Delim]
  if ContentStart < Cache[Delim] then
    Exit;

  I := ContentStart;
  while I <= Length(S) do
  begin
    C := S[I];
    // a code span outranks math; a single-dollar formula stays on its line
    if (C = '`') or ((C = #10) and (Delim < MaxMathDelimiterLength)) then
    begin
      Cache[Delim] := I;
      Exit;
    end;
    if C = '\' then
    begin
      Inc(I, 2);
      Continue;
    end;
    if C <> '$' then
    begin
      Inc(I);
      Continue;
    end;
    Run := DollarRun(S, I);
    CloserStart := I + Run - Delim;
    // closer: content not empty, not preceded by whitespace, not glued to a following word
    if (Run >= Delim) and (CloserStart > ContentStart) and
       not IsUnicodeWhitespace(S[CloserStart - 1]) and
       not IsLetterOrDigitAt(S, CloserStart + Delim) then
    begin
      Literal := Copy(S, ContentStart, CloserStart - ContentStart);
      IsDisplay := Delim = MaxMathDelimiterLength;
      Len := CloserStart + Delim - Index;
      Exit(True);
    end;
    Inc(I, Run);
  end;
  Cache[Delim] := I;
end;

function IsMathFenceLine(const Line: string; Index: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if DollarRun(Line, Index) <> 2 then
    Exit;
  I := Index + 2;
  while (I <= Length(Line)) and IsSpaceOrTab(Line[I]) do
    Inc(I);
  Result := I > Length(Line);
end;

function IsMathInfoString(const Info: string): Boolean;
var
  P: Integer;
begin
  P := 1;
  while (P <= Length(Info)) and not CharInSet(Info[P], [' ', #9, #10, #13]) do
    Inc(P);
  Result := SameText(Copy(Info, 1, P - 1), 'math');
end;

end.
