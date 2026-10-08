{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       CommonMark / GFM engine: character classes and text helpers            }
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
unit MarkdownTextUtils;

{ Helpers shared by the CommonMark/GFM block parser, inline parser and
  renderer. Strings are 1-based. Compatible with Delphi XE3. }

interface

uses
  System.SysUtils;

const
  /// <summary>Replacement for NUL and invalid code points.</summary>
  ReplacementChar = #$FFFD;

function IsSpaceOrTab(C: Char): Boolean; inline;
function IsAsciiDigit(C: Char): Boolean; inline;
function IsAsciiLetter(C: Char): Boolean; inline;
function IsAsciiAlphaNum(C: Char): Boolean; inline;
function IsHexDigit(C: Char): Boolean; inline;
function IsAsciiPunctuation(C: Char): Boolean;
/// <summary>Spec: Zs category, tab, line feed, form feed, carriage return.</summary>
function IsUnicodeWhitespace(C: Char): Boolean;
/// <summary>Spec 0.31.2: categories P* and S*. Index is the first UTF-16 unit
/// of the code point. False when Index is outside S (start/end of text).</summary>
function IsUnicodePunctuationAt(const S: string; Index: Integer): Boolean;
/// <summary>True when the code point at Index (first unit) is Unicode whitespace,
/// or Index is outside S.</summary>
function IsUnicodeWhitespaceAt(const S: string; Index: Integer): Boolean;
/// <summary>True when the code point at Index is a Unicode letter or decimal
/// digit. False when Index is outside S.</summary>
function IsLetterOrDigitAt(const S: string; Index: Integer): Boolean;
/// <summary>Index of the first UTF-16 unit of the code point that ends at Index - 1.</summary>
function PreviousCodePointIndex(const S: string; Index: Integer): Integer;

/// <summary>True if S contains only spaces, tabs and line endings.</summary>
function IsBlankText(const S: string): Boolean;
/// <summary>Removes leading and trailing spaces, tabs and line endings.</summary>
function TrimSpaceTabNewline(const S: string): string;
function TrimSpaceTab(const S: string): string;
function RepeatChar(C: Char; Count: Integer): string;

/// <summary>Escapes &amp; &lt; &gt; &quot; for HTML text and attributes.</summary>
function EscapeHtml(const S: string): string;

/// <summary>Decodes an entity or numeric character reference starting at
/// S[Index] = '&amp;'. Len is the number of source characters consumed.</summary>
function TryDecodeEntity(const S: string; Index: Integer; out Decoded: string; out Len: Integer): Boolean;
/// <summary>Processes backslash escapes and entity references.</summary>
function UnescapeString(const S: string): string;

/// <summary>Percent-encodes a URL as the reference implementation does (mdurl):
/// existing %XX sequences are kept, unsafe characters are UTF-8 encoded.</summary>
function NormalizeUri(const S: string): string;
/// <summary>Percent-encodes everything but A-Z a-z 0-9 - _ . ~ (UTF-8), as
/// encodeURIComponent: for a value placed in a URL query.</summary>
function EncodeUrlComponent(const S: string): string;
/// <summary>Normalizes the content of a link label (without brackets) for
/// matching: trimmed, inner whitespace collapsed, Unicode case folded.</summary>
function NormalizeLinkLabel(const S: string): string;

{ HTML scanners: each returns the length of the construct starting at
  S[Index] = '<', or 0 when there is none. Whitespace is spaces, tabs and
  line endings. }
function ScanHtmlOpenTag(const S: string; Index: Integer): Integer;
function ScanHtmlClosingTag(const S: string; Index: Integer): Integer;
function ScanHtmlComment(const S: string; Index: Integer): Integer;
function ScanHtmlProcessingInstruction(const S: string; Index: Integer): Integer;
function ScanHtmlDeclaration(const S: string; Index: Integer): Integer;
function ScanHtmlCData(const S: string; Index: Integer): Integer;
/// <summary>Any inline raw HTML construct (spec section "Raw HTML").</summary>
function ScanHtmlInline(const S: string; Index: Integer): Integer;
/// <summary>Tag name of an open or closing tag at S[Index] = '&lt;' (lower case), or ''.</summary>
function HtmlTagNameAt(const S: string; Index: Integer): string;
/// <summary>True if Sub occurs in S at Index (case sensitive).</summary>
function StartsAt(const S: string; Index: Integer; const Sub: string): Boolean;
/// <summary>True if Sub occurs in S at Index, ASCII case insensitive.</summary>
function StartsAtIgnoreCase(const S: string; Index: Integer; const Sub: string): Boolean;

implementation

uses
  System.Character,
  MarkdownEntities;

function IsSpaceOrTab(C: Char): Boolean;
begin
  Result := (C = ' ') or (C = #9);
end;

function IsAsciiDigit(C: Char): Boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

function IsAsciiLetter(C: Char): Boolean;
begin
  Result := ((C >= 'a') and (C <= 'z')) or ((C >= 'A') and (C <= 'Z'));
end;

function IsAsciiAlphaNum(C: Char): Boolean;
begin
  Result := IsAsciiLetter(C) or IsAsciiDigit(C);
end;

function IsHexDigit(C: Char): Boolean;
begin
  Result := IsAsciiDigit(C) or ((C >= 'a') and (C <= 'f')) or ((C >= 'A') and (C <= 'F'));
end;

function IsAsciiPunctuation(C: Char): Boolean;
begin
  case C of
    '!', '"', '#', '$', '%', '&', '''', '(', ')', '*', '+', ',', '-', '.', '/',
    ':', ';', '<', '=', '>', '?', '@', '[', '\', ']', '^', '_', '`', '{', '|', '}', '~':
      Result := True;
  else
    Result := False;
  end;
end;

function UnicodeCategoryAt(const S: string; Index: Integer): TUnicodeCategory;
var
  C: Char;
  Code: UCS4Char;
begin
  // The string+index overloads of the RTL are avoided on purpose: their
  // indexing (0 or 1 based) differs between Delphi versions.
  C := S[Index];
  if (C >= #$D800) and (C <= #$DBFF) and (Index < Length(S)) and
     (S[Index + 1] >= #$DC00) and (S[Index + 1] <= #$DFFF) then
  begin
    Code := $10000 + ((UCS4Char(Ord(C)) - $D800) shl 10) + (UCS4Char(Ord(S[Index + 1])) - $DC00);
{$IF CompilerVersion >= 25.0}
    Result := Char.GetUnicodeCategory(Code);
{$ELSE}
    Result := TCharacter.GetUnicodeCategory(Code);
{$IFEND}
  end
  else
{$IF CompilerVersion >= 25.0}
    Result := C.GetUnicodeCategory;
{$ELSE}
    Result := TCharacter.GetUnicodeCategory(C);
{$IFEND}
end;

function IsUnicodeWhitespace(C: Char): Boolean;
begin
  case C of
    #9, #10, #12, #13, ' ':
      Result := True;
  else
    if Ord(C) < $80 then
      Result := False
    else
      Result := UnicodeCategoryAt(C, 1) = TUnicodeCategory.ucSpaceSeparator;
  end;
end;

function IsUnicodeWhitespaceAt(const S: string; Index: Integer): Boolean;
begin
  if (Index < 1) or (Index > Length(S)) then
    Result := True
  else
    Result := IsUnicodeWhitespace(S[Index]);
end;

function IsUnicodePunctuationAt(const S: string; Index: Integer): Boolean;
var
  C: Char;
begin
  if (Index < 1) or (Index > Length(S)) then
    Exit(False);
  C := S[Index];
  if Ord(C) < $80 then
    Exit(IsAsciiPunctuation(C));
  case UnicodeCategoryAt(S, Index) of
    TUnicodeCategory.ucConnectPunctuation, TUnicodeCategory.ucDashPunctuation,
    TUnicodeCategory.ucClosePunctuation, TUnicodeCategory.ucFinalPunctuation,
    TUnicodeCategory.ucInitialPunctuation, TUnicodeCategory.ucOtherPunctuation,
    TUnicodeCategory.ucOpenPunctuation, TUnicodeCategory.ucCurrencySymbol,
    TUnicodeCategory.ucModifierSymbol, TUnicodeCategory.ucMathSymbol,
    TUnicodeCategory.ucOtherSymbol:
      Result := True;
  else
    Result := False;
  end;
end;

function IsLetterOrDigitAt(const S: string; Index: Integer): Boolean;
var
  C: Char;
begin
  if (Index < 1) or (Index > Length(S)) then
    Exit(False);
  C := S[Index];
  if Ord(C) < $80 then
    Exit(IsAsciiAlphaNum(C));
  case UnicodeCategoryAt(S, Index) of
    TUnicodeCategory.ucLowercaseLetter, TUnicodeCategory.ucModifierLetter,
    TUnicodeCategory.ucOtherLetter, TUnicodeCategory.ucTitlecaseLetter,
    TUnicodeCategory.ucUppercaseLetter, TUnicodeCategory.ucDecimalNumber:
      Result := True;
  else
    Result := False;
  end;
end;

function PreviousCodePointIndex(const S: string; Index: Integer): Integer;
begin
  Result := Index - 1;
  if (Result > 1) and (Result <= Length(S)) and
     (S[Result] >= #$DC00) and (S[Result] <= #$DFFF) and
     (S[Result - 1] >= #$D800) and (S[Result - 1] <= #$DBFF) then
    Dec(Result);
end;

function IsBlankText(const S: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    case S[I] of
      ' ', #9, #10, #13: ;
    else
      Exit(False);
    end;
  Result := True;
end;

function TrimSpaceTabNewline(const S: string): string;
var
  First, Last: Integer;
begin
  First := 1;
  Last := Length(S);
  while (First <= Last) and CharInSet(S[First], [' ', #9, #10, #13]) do
    Inc(First);
  while (Last >= First) and CharInSet(S[Last], [' ', #9, #10, #13]) do
    Dec(Last);
  Result := Copy(S, First, Last - First + 1);
end;

function TrimSpaceTab(const S: string): string;
var
  First, Last: Integer;
begin
  First := 1;
  Last := Length(S);
  while (First <= Last) and IsSpaceOrTab(S[First]) do
    Inc(First);
  while (Last >= First) and IsSpaceOrTab(S[Last]) do
    Dec(Last);
  Result := Copy(S, First, Last - First + 1);
end;

function RepeatChar(C: Char; Count: Integer): string;
begin
  if Count <= 0 then
    Exit('');
  Result := StringOfChar(C, Count);
end;

function EscapeHtml(const S: string): string;
var
  I: Integer;
  SB: TStringBuilder;
begin
  I := 1;
  while (I <= Length(S)) and not CharInSet(S[I], ['&', '<', '>', '"']) do
    Inc(I);
  if I > Length(S) then
    Exit(S);
  SB := TStringBuilder.Create(Length(S) + 16);
  try
    for I := 1 to Length(S) do
      case S[I] of
        '&': SB.Append('&amp;');
        '<': SB.Append('&lt;');
        '>': SB.Append('&gt;');
        '"': SB.Append('&quot;');
      else
        SB.Append(S[I]);
      end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function CodePointToString(Code: Cardinal): string;
begin
  if (Code = 0) or (Code > $10FFFF) or ((Code >= $D800) and (Code <= $DFFF)) then
    Exit(ReplacementChar);
  if Code < $10000 then
    Result := Char(Code)
  else
  begin
    Dec(Code, $10000);
    Result := Char($D800 + (Code shr 10)) + Char($DC00 + (Code and $3FF));
  end;
end;

function TryDecodeEntity(const S: string; Index: Integer; out Decoded: string; out Len: Integer): Boolean;
var
  I, Start, Digits: Integer;
  Code: Cardinal;
  IsHex: Boolean;
begin
  Result := False;
  Decoded := '';
  Len := 0;
  if (Index > Length(S)) or (S[Index] <> '&') then
    Exit;
  I := Index + 1;
  if (I <= Length(S)) and (S[I] = '#') then
  begin
    Inc(I);
    IsHex := (I <= Length(S)) and CharInSet(S[I], ['x', 'X']);
    if IsHex then
      Inc(I);
    Start := I;
    Code := 0;
    while I <= Length(S) do
    begin
      if IsHex and IsHexDigit(S[I]) then
        Code := Code * 16 + Cardinal(StrToInt('$' + S[I]))
      else if not IsHex and IsAsciiDigit(S[I]) then
        Code := Code * 10 + Cardinal(Ord(S[I]) - Ord('0'))
      else
        Break;
      if Code > $10FFFF then
        Code := $110000; // keep it invalid, avoid overflow
      Inc(I);
    end;
    Digits := I - Start;
    if (Digits = 0) or (IsHex and (Digits > 6)) or (not IsHex and (Digits > 7)) then
      Exit;
    if (I > Length(S)) or (S[I] <> ';') then
      Exit;
    Decoded := CodePointToString(Code);
    Len := I - Index + 1;
    Exit(True);
  end;
  // named reference: [A-Za-z][A-Za-z0-9]{1,31};
  Start := I;
  if (I > Length(S)) or not IsAsciiLetter(S[I]) then
    Exit;
  while (I <= Length(S)) and IsAsciiAlphaNum(S[I]) and (I - Start < 32) do
    Inc(I);
  if (I > Length(S)) or (S[I] <> ';') or (I - Start < 2) then
    Exit;
  if not LookupEntity(Copy(S, Start, I - Start), Decoded) then
    Exit;
  Len := I - Index + 1;
  Result := True;
end;

function UnescapeString(const S: string): string;
var
  I, Len: Integer;
  SB: TStringBuilder;
  Decoded: string;
begin
  if (Pos('\', S) = 0) and (Pos('&', S) = 0) then
    Exit(S);
  SB := TStringBuilder.Create(Length(S));
  try
    I := 1;
    while I <= Length(S) do
    begin
      if (S[I] = '\') and (I < Length(S)) and IsAsciiPunctuation(S[I + 1]) then
      begin
        SB.Append(S[I + 1]);
        Inc(I, 2);
      end
      else if (S[I] = '&') and TryDecodeEntity(S, I, Decoded, Len) then
      begin
        SB.Append(Decoded);
        Inc(I, Len);
      end
      else
      begin
        SB.Append(S[I]);
        Inc(I);
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function NormalizeUri(const S: string): string;
const
  Hex: array[0..15] of Char = '0123456789ABCDEF';
var
  I: Integer;
  C: Char;
  Code: Cardinal;
  SB: TStringBuilder;

  procedure AppendByte(B: Byte);
  begin
    SB.Append('%');
    SB.Append(Hex[B shr 4]);
    SB.Append(Hex[B and $F]);
  end;

begin
  SB := TStringBuilder.Create(Length(S) + 16);
  try
    I := 1;
    while I <= Length(S) do
    begin
      C := S[I];
      if (C = '%') and (I + 2 <= Length(S)) and IsHexDigit(S[I + 1]) and IsHexDigit(S[I + 2]) then
      begin
        SB.Append(Copy(S, I, 3));
        Inc(I, 3);
        Continue;
      end;
      if (Ord(C) < $80) and (IsAsciiAlphaNum(C) or CharInSet(C, [';', '/', '?', ':', '@', '&', '=', '+',
        '$', ',', '-', '_', '.', '!', '~', '*', '''', '(', ')', '#'])) then
      begin
        SB.Append(C);
        Inc(I);
        Continue;
      end;
      // UTF-8 percent encoding of the code point
      Code := Ord(C);
      if (Code >= $D800) and (Code <= $DBFF) and (I < Length(S)) and
         (Ord(S[I + 1]) >= $DC00) and (Ord(S[I + 1]) <= $DFFF) then
      begin
        Code := $10000 + ((Code - $D800) shl 10) + (Cardinal(Ord(S[I + 1])) - $DC00);
        Inc(I);
      end
      else if (Code >= $D800) and (Code <= $DFFF) then
        Code := $FFFD;
      if Code < $80 then
        AppendByte(Code)
      else if Code < $800 then
      begin
        AppendByte($C0 or (Code shr 6));
        AppendByte($80 or (Code and $3F));
      end
      else if Code < $10000 then
      begin
        AppendByte($E0 or (Code shr 12));
        AppendByte($80 or ((Code shr 6) and $3F));
        AppendByte($80 or (Code and $3F));
      end
      else
      begin
        AppendByte($F0 or (Code shr 18));
        AppendByte($80 or ((Code shr 12) and $3F));
        AppendByte($80 or ((Code shr 6) and $3F));
        AppendByte($80 or (Code and $3F));
      end;
      Inc(I);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function EncodeUrlComponent(const S: string): string;
const
  Hex: array[0..15] of Char = '0123456789ABCDEF';
var
  Bytes: TBytes;
  I: Integer;
  B: Byte;
  SB: TStringBuilder;
begin
  Bytes := TEncoding.UTF8.GetBytes(S);
  SB := TStringBuilder.Create(Length(Bytes) * 3);
  try
    for I := 0 to High(Bytes) do
    begin
      B := Bytes[I];
      if ((B >= Ord('A')) and (B <= Ord('Z'))) or ((B >= Ord('a')) and (B <= Ord('z'))) or
         ((B >= Ord('0')) and (B <= Ord('9'))) or (B = Ord('-')) or (B = Ord('_')) or
         (B = Ord('.')) or (B = Ord('~')) then
        SB.Append(Char(B))
      else
      begin
        SB.Append('%');
        SB.Append(Hex[B shr 4]);
        SB.Append(Hex[B and $F]);
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function NormalizeLinkLabel(const S: string): string;
var
  I: Integer;
  SB: TStringBuilder;
  InSpace: Boolean;
  Trimmed: string;
begin
  Trimmed := TrimSpaceTabNewline(S);
  SB := TStringBuilder.Create(Length(Trimmed));
  try
    InSpace := False;
    for I := 1 to Length(Trimmed) do
      if CharInSet(Trimmed[I], [' ', #9, #10, #13]) then
      begin
        if not InSpace then
          SB.Append(' ');
        InSpace := True;
      end
      else
      begin
        SB.Append(Trimmed[I]);
        InSpace := False;
      end;
    // Unicode case fold: lower then upper (as the reference implementation),
    // plus the full folding of sharp s (U+00DF, U+1E9E) to "SS".
    Result := AnsiUpperCase(AnsiLowerCase(SB.ToString));
    Result := StringReplace(Result, #$00DF, 'SS', [rfReplaceAll]);
    Result := StringReplace(Result, #$1E9E, 'SS', [rfReplaceAll]);
  finally
    SB.Free;
  end;
end;

{ HTML scanners }

function IsHtmlWhitespace(C: Char): Boolean; inline;
begin
  Result := (C = ' ') or (C = #9) or (C = #10) or (C = #13) or (C = #12) or (C = #11);
end;

function SkipHtmlWhitespace(const S: string; I: Integer): Integer;
begin
  while (I <= Length(S)) and IsHtmlWhitespace(S[I]) do
    Inc(I);
  Result := I;
end;

// Returns the index after the tag name starting at I, or 0
function ScanTagName(const S: string; I: Integer): Integer;
begin
  if (I > Length(S)) or not IsAsciiLetter(S[I]) then
    Exit(0);
  Inc(I);
  while (I <= Length(S)) and (IsAsciiAlphaNum(S[I]) or (S[I] = '-')) do
    Inc(I);
  Result := I;
end;

function HtmlTagNameAt(const S: string; Index: Integer): string;
var
  I, EndName: Integer;
begin
  Result := '';
  if (Index > Length(S)) or (S[Index] <> '<') then
    Exit;
  I := Index + 1;
  if (I <= Length(S)) and (S[I] = '/') then
    Inc(I);
  EndName := ScanTagName(S, I);
  if EndName > 0 then
    Result := LowerCase(Copy(S, I, EndName - I));
end;

function ScanHtmlOpenTag(const S: string; Index: Integer): Integer;
var
  I, AfterSpace: Integer;
  Quote: Char;
begin
  Result := 0;
  if (Index > Length(S)) or (S[Index] <> '<') then
    Exit;
  I := ScanTagName(S, Index + 1);
  if I = 0 then
    Exit;
  // attributes: each one must be preceded by whitespace
  while True do
  begin
    AfterSpace := SkipHtmlWhitespace(S, I);
    if (AfterSpace = I) or (AfterSpace > Length(S)) then
      Break;
    if not (IsAsciiLetter(S[AfterSpace]) or (S[AfterSpace] = '_') or (S[AfterSpace] = ':')) then
      Break;
    // attribute name
    I := AfterSpace + 1;
    while (I <= Length(S)) and (IsAsciiAlphaNum(S[I]) or CharInSet(S[I], ['_', '.', ':', '-'])) do
      Inc(I);
    // optional value specification
    AfterSpace := SkipHtmlWhitespace(S, I);
    if (AfterSpace <= Length(S)) and (S[AfterSpace] = '=') then
    begin
      I := SkipHtmlWhitespace(S, AfterSpace + 1);
      if I > Length(S) then
        Exit;
      if (S[I] = '"') or (S[I] = '''') then
      begin
        Quote := S[I];
        Inc(I);
        while (I <= Length(S)) and (S[I] <> Quote) do
          Inc(I);
        if I > Length(S) then
          Exit;
        Inc(I);
      end
      else
      begin
        AfterSpace := I;
        while (I <= Length(S)) and not IsHtmlWhitespace(S[I]) and
              not CharInSet(S[I], ['"', '''', '=', '<', '>', '`']) do
          Inc(I);
        if I = AfterSpace then
          Exit;
      end;
    end;
  end;
  I := SkipHtmlWhitespace(S, I);
  if (I <= Length(S)) and (S[I] = '/') then
    Inc(I);
  if (I <= Length(S)) and (S[I] = '>') then
    Result := I - Index + 1;
end;

function ScanHtmlClosingTag(const S: string; Index: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (Index + 1 > Length(S)) or (S[Index] <> '<') or (S[Index + 1] <> '/') then
    Exit;
  I := ScanTagName(S, Index + 2);
  if I = 0 then
    Exit;
  I := SkipHtmlWhitespace(S, I);
  if (I <= Length(S)) and (S[I] = '>') then
    Result := I - Index + 1;
end;

function StartsAt(const S: string; Index: Integer; const Sub: string): Boolean;
var
  K: Integer;
begin
  if (Index < 1) or (Index + Length(Sub) - 1 > Length(S)) then
    Exit(False);
  for K := 1 to Length(Sub) do
    if S[Index + K - 1] <> Sub[K] then
      Exit(False);
  Result := True;
end;

function StartsAtIgnoreCase(const S: string; Index: Integer; const Sub: string): Boolean;
var
  K: Integer;
  A, B: Char;
begin
  if (Index < 1) or (Index + Length(Sub) - 1 > Length(S)) then
    Exit(False);
  for K := 1 to Length(Sub) do
  begin
    A := S[Index + K - 1];
    B := Sub[K];
    if (A >= 'A') and (A <= 'Z') then
      A := Char(Ord(A) + 32);
    if (B >= 'A') and (B <= 'Z') then
      B := Char(Ord(B) + 32);
    if A <> B then
      Exit(False);
  end;
  Result := True;
end;

function FindFrom(const S: string; Index: Integer; const Sub: string): Integer;
var
  I: Integer;
begin
  for I := Index to Length(S) - Length(Sub) + 1 do
    if StartsAt(S, I, Sub) then
      Exit(I);
  Result := 0;
end;

function ScanHtmlComment(const S: string; Index: Integer): Integer;
var
  EndPos: Integer;
begin
  Result := 0;
  if not StartsAt(S, Index, '<!--') then
    Exit;
  if StartsAt(S, Index, '<!-->') then
    Exit(5);
  if StartsAt(S, Index, '<!--->') then
    Exit(6);
  EndPos := FindFrom(S, Index + 4, '-->');
  if EndPos > 0 then
    Result := EndPos + 3 - Index;
end;

function ScanHtmlProcessingInstruction(const S: string; Index: Integer): Integer;
var
  EndPos: Integer;
begin
  Result := 0;
  if not StartsAt(S, Index, '<?') then
    Exit;
  EndPos := FindFrom(S, Index + 2, '?>');
  if EndPos > 0 then
    Result := EndPos + 2 - Index;
end;

function ScanHtmlDeclaration(const S: string; Index: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  if not StartsAt(S, Index, '<!') or (Index + 2 > Length(S)) or not IsAsciiLetter(S[Index + 2]) then
    Exit;
  I := Index + 3;
  while (I <= Length(S)) and (S[I] <> '>') do
    Inc(I);
  if I <= Length(S) then
    Result := I - Index + 1;
end;

function ScanHtmlCData(const S: string; Index: Integer): Integer;
var
  EndPos: Integer;
begin
  Result := 0;
  if not StartsAt(S, Index, '<![CDATA[') then
    Exit;
  EndPos := FindFrom(S, Index + 9, ']]>');
  if EndPos > 0 then
    Result := EndPos + 3 - Index;
end;

function ScanHtmlInline(const S: string; Index: Integer): Integer;
begin
  Result := 0;
  if (Index >= Length(S)) or (S[Index] <> '<') then
    Exit;
  case S[Index + 1] of
    '/': Result := ScanHtmlClosingTag(S, Index);
    '?': Result := ScanHtmlProcessingInstruction(S, Index);
    '!':
      begin
        Result := ScanHtmlComment(S, Index);
        if Result = 0 then
          Result := ScanHtmlCData(S, Index);
        if Result = 0 then
          Result := ScanHtmlDeclaration(S, Index);
      end;
  else
    Result := ScanHtmlOpenTag(S, Index);
  end;
end;

end.
