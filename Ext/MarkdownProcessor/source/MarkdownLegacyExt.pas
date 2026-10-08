{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       CommonMark / GFM engine: legacy (Ethea) extensions                     }
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
unit MarkdownLegacyExt;

(* The non-standard syntax of the old mdCommonMark dialect, available on the
  new engine as individual flags (all off by default):
    mexSubscript ~x~, mexSuperscript ^x^, mexInsert ++x++, mexMark ==x==
      (resolved by the delimiter stack of MarkdownInlineParser)
    mexSmartTypography   -- --- ... (C) (R) (TM) << >> "quotes"
    mexHeadingAttributes # Title {#id}
    mexAutoHeadingIds    GitHub-style slugs (github-slugger)
    mexWikiLinks         [[...]] (given to TConfiguration.specialLinkEmitter)
  Smart typography emits Unicode characters (the old engine emitted the
  equivalent HTML entities). *)

interface

uses
  System.SysUtils,
  System.Generics.Collections;

/// <summary>Smart typography of a text run: -- and --- become en/em dashes,
/// ... an ellipsis, (C) (R) (TM) the symbols, &lt;&lt; &gt;&gt; guillemets and straight
/// double quotes curly quotes (opening when not preceded by a letter or digit).</summary>
function ApplySmartTypography(const S: string): string;

/// <summary>Removes a trailing "{#id}" from heading content. Returns False and
/// leaves Content unchanged when there is none.</summary>
function ExtractHeadingId(var Content: string; out Id: string): Boolean;

/// <summary>GitHub heading slug: lower case, punctuation and symbols removed
/// (except - and _), spaces replaced by -.</summary>
function GitHubSlug(const Text: string): string;

type
  /// <summary>Keeps heading ids unique: a repeated slug gets -1, -2, ...</summary>
  TSlugRegistry = class
  private
    FCounts: TDictionary<string, Integer>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reserve(const Id: string);
    function Unique(const Slug: string): string;
  end;

implementation

uses
  MarkdownTextUtils;

function ApplySmartTypography(const S: string): string;
var
  I, N: Integer;
  SB: TStringBuilder;
  C, Before, After: Char;
begin
  SB := TStringBuilder.Create(Length(S));
  try
    N := Length(S);
    I := 1;
    while I <= N do
    begin
      C := S[I];
      if (C = '-') and (I + 1 <= N) and (S[I + 1] = '-') then
      begin
        if (I + 2 <= N) and (S[I + 2] = '-') then
        begin
          SB.Append(#$2014); // em dash
          Inc(I, 3);
        end
        else
        begin
          SB.Append(#$2013); // en dash
          Inc(I, 2);
        end;
        Continue;
      end;
      if (C = '.') and StartsAt(S, I, '...') then
      begin
        SB.Append(#$2026);
        Inc(I, 3);
        Continue;
      end;
      if C = '(' then
      begin
        if StartsAt(S, I, '(C)') then
        begin
          SB.Append(#$00A9);
          Inc(I, 3);
          Continue;
        end;
        if StartsAt(S, I, '(R)') then
        begin
          SB.Append(#$00AE);
          Inc(I, 3);
          Continue;
        end;
        if StartsAt(S, I, '(TM)') then
        begin
          SB.Append(#$2122);
          Inc(I, 4);
          Continue;
        end;
      end;
      if (C = '<') and StartsAt(S, I, '<<') then
      begin
        SB.Append(#$00AB);
        Inc(I, 2);
        Continue;
      end;
      if (C = '>') and StartsAt(S, I, '>>') then
      begin
        SB.Append(#$00BB);
        Inc(I, 2);
        Continue;
      end;
      if C = '"' then
      begin
        if I > 1 then
          Before := S[I - 1]
        else
          Before := ' ';
        if I < N then
          After := S[I + 1]
        else
          After := ' ';
        if not IsLetterOrDigitAt(Before, 1) and (After <> ' ') then
          SB.Append(#$201C) // opening
        else if (Before <> ' ') and not IsLetterOrDigitAt(After, 1) then
          SB.Append(#$201D) // closing
        else
          SB.Append(C);
        Inc(I);
        Continue;
      end;
      SB.Append(C);
      Inc(I);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function ExtractHeadingId(var Content: string; out Id: string): Boolean;
var
  Trimmed: string;
  Open, I: Integer;
begin
  Result := False;
  Id := '';
  Trimmed := TrimSpaceTab(Content);
  if (Trimmed = '') or (Trimmed[Length(Trimmed)] <> '}') then
    Exit;
  Open := Length(Trimmed) - 1;
  while (Open >= 1) and (Trimmed[Open] <> '{') do
    Dec(Open);
  if (Open < 1) or (Open + 1 > Length(Trimmed)) or (Trimmed[Open + 1] <> '#') then
    Exit;
  // an escaped brace is text
  if (Open > 1) and (Trimmed[Open - 1] = '\') then
    Exit;
  Id := Copy(Trimmed, Open + 2, Length(Trimmed) - Open - 2);
  if Id = '' then
    Exit;
  for I := 1 to Length(Id) do
    if IsUnicodeWhitespace(Id[I]) then
      Exit;
  Content := TrimSpaceTab(Copy(Trimmed, 1, Open - 1));
  Result := True;
end;

function GitHubSlug(const Text: string): string;
var
  Lower: string;
  I, Len: Integer;
  C: Char;
  SB: TStringBuilder;
  Keep: Boolean;
begin
  Lower := AnsiLowerCase(Text);
  SB := TStringBuilder.Create(Length(Lower));
  try
    I := 1;
    while I <= Length(Lower) do
    begin
      C := Lower[I];
      // a surrogate pair is one code point
      Len := 1;
      if (Ord(C) >= $D800) and (Ord(C) <= $DBFF) and (I < Length(Lower)) and
         (Ord(Lower[I + 1]) >= $DC00) and (Ord(Lower[I + 1]) <= $DFFF) then
        Len := 2;
      if C = ' ' then
      begin
        SB.Append('-');
        Inc(I);
        Continue;
      end;
      if (C = '-') or (C = '_') then
        Keep := True
      else if Ord(C) < $80 then
        Keep := IsAsciiAlphaNum(C)
      else
        // letters, digits and marks stay; punctuation, symbols, spaces go
        Keep := not IsUnicodePunctuationAt(Lower, I) and not IsUnicodeWhitespace(C);
      if Keep then
        SB.Append(Copy(Lower, I, Len));
      Inc(I, Len);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

{ TSlugRegistry }

constructor TSlugRegistry.Create;
begin
  inherited Create;
  FCounts := TDictionary<string, Integer>.Create;
end;

destructor TSlugRegistry.Destroy;
begin
  FCounts.Free;
  inherited;
end;

procedure TSlugRegistry.Reserve(const Id: string);
begin
  if not FCounts.ContainsKey(Id) then
    FCounts.Add(Id, 0);
end;

function TSlugRegistry.Unique(const Slug: string): string;
var
  Count: Integer;
begin
  Result := Slug;
  if not FCounts.TryGetValue(Slug, Count) then
  begin
    FCounts.Add(Slug, 0);
    Exit;
  end;
  repeat
    Inc(Count);
    FCounts[Slug] := Count;
    Result := Slug + '-' + IntToStr(Count);
  until not FCounts.ContainsKey(Result);
  FCounts.Add(Result, 0);
end;

end.
