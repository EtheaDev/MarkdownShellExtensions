{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       CommonMark / GFM engine: link reference definitions                    }
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
{  The scanners follow commonmark.js (BSD-2-Clause, Copyright (c) 2014         }
{  John MacFarlane): see docs/LICENSE-commonmark.js.txt.                       }
{                                                                              }
{******************************************************************************}
unit MarkdownLinkRefs;

{ Link reference definitions ("[label]: destination 'title'") and the scanners
  for link labels, destinations and titles, shared by the block parser and the
  inline parser. Positions are 1-based indexes into the subject string. }

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TLinkReference = record
    Destination: string;
    Title: string;
  end;

  /// <summary>Link reference definitions of a document, keyed by normalized label.</summary>
  TLinkReferenceMap = class
  private
    FItems: TDictionary<string, TLinkReference>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    /// <summary>The first definition of a label wins.</summary>
    procedure AddIfMissing(const NormalizedLabel, Destination, Title: string);
    function TryGet(const NormalizedLabel: string; out Reference: TLinkReference): Boolean;
    function Count: Integer;
  end;

/// <summary>Length (brackets included) of the link label starting at S[Index] = '[',
/// or 0 when there is none. At most 999 characters inside the brackets.</summary>
function ScanLinkLabel(const S: string; Index: Integer): Integer;
/// <summary>Scans a link destination at Index. On success Index is moved past it
/// and Destination is unescaped and URL-normalized.</summary>
function ScanLinkDestination(const S: string; var Index: Integer; out Destination: string): Boolean;
/// <summary>Scans a link title ("...", '...' or (...)) at Index.</summary>
function ScanLinkTitle(const S: string; var Index: Integer; out Title: string): Boolean;
/// <summary>Skips spaces and tabs, at most one line ending, then spaces and tabs.</summary>
procedure SkipSpacesAndOneNewline(const S: string; var Index: Integer);
/// <summary>Parses a link reference definition starting at S[Start]. Returns the
/// number of characters consumed (0 when there is none) and adds it to Refs.</summary>
function ParseLinkReferenceDefinition(const S: string; Start: Integer; Refs: TLinkReferenceMap): Integer;

implementation

uses
  MarkdownTextUtils;

{ TLinkReferenceMap }

constructor TLinkReferenceMap.Create;
begin
  inherited Create;
  FItems := TDictionary<string, TLinkReference>.Create;
end;

destructor TLinkReferenceMap.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TLinkReferenceMap.Clear;
begin
  FItems.Clear;
end;

procedure TLinkReferenceMap.AddIfMissing(const NormalizedLabel, Destination, Title: string);
var
  Ref: TLinkReference;
begin
  if FItems.ContainsKey(NormalizedLabel) then
    Exit;
  Ref.Destination := Destination;
  Ref.Title := Title;
  FItems.Add(NormalizedLabel, Ref);
end;

function TLinkReferenceMap.TryGet(const NormalizedLabel: string; out Reference: TLinkReference): Boolean;
begin
  Result := FItems.TryGetValue(NormalizedLabel, Reference);
end;

function TLinkReferenceMap.Count: Integer;
begin
  Result := FItems.Count;
end;

{ Scanners }

function ScanLinkLabel(const S: string; Index: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (Index > Length(S)) or (S[Index] <> '[') then
    Exit;
  I := Index + 1;
  while I <= Length(S) do
  begin
    case S[I] of
      '\':
        if I < Length(S) then
          Inc(I);
      '[':
        Exit;
      ']':
        begin
          if I - Index - 1 > 999 then
            Exit;
          Exit(I - Index + 1);
        end;
    end;
    Inc(I);
  end;
end;

function ScanLinkDestination(const S: string; var Index: Integer; out Destination: string): Boolean;
var
  I, Start, OpenParens: Integer;
  C: Char;
begin
  Result := False;
  Destination := '';
  I := Index;
  if (I <= Length(S)) and (S[I] = '<') then
  begin
    Inc(I);
    Start := I;
    while I <= Length(S) do
    begin
      C := S[I];
      if C = '>' then
      begin
        Destination := NormalizeUri(UnescapeString(Copy(S, Start, I - Start)));
        Index := I + 1;
        Exit(True);
      end;
      if (C = '<') or (C = #10) or (C = #13) then
        Exit;
      if (C = '\') and (I < Length(S)) and (S[I + 1] <> #10) and (S[I + 1] <> #13) then
        Inc(I);
      Inc(I);
    end;
    Exit;
  end;

  Start := I;
  OpenParens := 0;
  // parentheses may nest up to 32 levels (the limit allowed by the spec keeps
  // unclosed "[a](b" sequences linear)
  while I <= Length(S) do
  begin
    C := S[I];
    if (C = '\') and (I < Length(S)) and IsAsciiPunctuation(S[I + 1]) then
      Inc(I, 2)
    else if C = '(' then
    begin
      Inc(OpenParens);
      if OpenParens > 32 then
        Exit;
      Inc(I);
    end
    else if C = ')' then
    begin
      if OpenParens < 1 then
        Break;
      Dec(OpenParens);
      Inc(I);
    end
    else if (C <= ' ') or (C = #$7F) then
      Break
    else
      Inc(I);
  end;
  if (I = Start) and ((I > Length(S)) or (S[I] <> ')')) then
    Exit;
  if OpenParens <> 0 then
    Exit;
  Destination := NormalizeUri(UnescapeString(Copy(S, Start, I - Start)));
  Index := I;
  Result := True;
end;

function ScanLinkTitle(const S: string; var Index: Integer; out Title: string): Boolean;
var
  I: Integer;
  Opener, Closer: Char;
begin
  Result := False;
  Title := '';
  if Index > Length(S) then
    Exit;
  Opener := S[Index];
  case Opener of
    '"': Closer := '"';
    '''': Closer := '''';
    '(': Closer := ')';
  else
    Exit;
  end;
  I := Index + 1;
  while I <= Length(S) do
  begin
    if S[I] = '\' then
    begin
      Inc(I, 2);
      Continue;
    end;
    if S[I] = Closer then
    begin
      Title := UnescapeString(Copy(S, Index + 1, I - Index - 1));
      Index := I + 1;
      Exit(True);
    end;
    if (Opener = '(') and (S[I] = '(') then
      Exit;
    Inc(I);
  end;
end;

procedure SkipSpacesAndOneNewline(const S: string; var Index: Integer);
begin
  while (Index <= Length(S)) and IsSpaceOrTab(S[Index]) do
    Inc(Index);
  if (Index <= Length(S)) and (S[Index] = #10) then
  begin
    Inc(Index);
    while (Index <= Length(S)) and IsSpaceOrTab(S[Index]) do
      Inc(Index);
  end;
end;

function AtLineEnd(const S: string; var Index: Integer): Boolean;
var
  I: Integer;
begin
  I := Index;
  while (I <= Length(S)) and IsSpaceOrTab(S[I]) do
    Inc(I);
  if I > Length(S) then
  begin
    Index := I;
    Exit(True);
  end;
  if S[I] = #10 then
  begin
    Index := I + 1;
    Exit(True);
  end;
  Result := False;
end;

function ParseLinkReferenceDefinition(const S: string; Start: Integer; Refs: TLinkReferenceMap): Integer;
var
  I, LabelLen, BeforeTitle: Integer;
  RawLabel, Dest, Title, NormLabel: string;
  HasTitle: Boolean;
begin
  Result := 0;
  LabelLen := ScanLinkLabel(S, Start);
  if LabelLen = 0 then
    Exit;
  RawLabel := Copy(S, Start + 1, LabelLen - 2);
  I := Start + LabelLen;
  if (I > Length(S)) or (S[I] <> ':') then
    Exit;
  Inc(I);
  SkipSpacesAndOneNewline(S, I);
  if not ScanLinkDestination(S, I, Dest) then
    Exit;
  // a destination without <> cannot be empty in a definition
  if (Dest = '') and (S[I - 1] <> '>') then
    Exit;

  BeforeTitle := I;
  SkipSpacesAndOneNewline(S, I);
  HasTitle := (I <> BeforeTitle) and ScanLinkTitle(S, I, Title);
  if not HasTitle then
    I := BeforeTitle;

  if not AtLineEnd(S, I) then
  begin
    if not HasTitle then
      Exit;
    // the title is not followed by the line end: the definition may still be
    // valid without it, if the destination ends its line
    HasTitle := False;
    I := BeforeTitle;
    if not AtLineEnd(S, I) then
      Exit;
  end;

  NormLabel := NormalizeLinkLabel(RawLabel);
  if NormLabel = '' then
    Exit;
  if not HasTitle then
    Title := '';
  Refs.AddIfMissing(NormLabel, Dest, Title);
  Result := I - Start;
end;

end.
