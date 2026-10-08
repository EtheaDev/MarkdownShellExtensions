{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       CommonMark / GFM engine: GitHub alerts                                 }
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
{  The rules follow GitHub and Markdown4D (MIT, (c) 2026 GDK Software):        }
{  see Tests/LICENSE-Markdown4D.txt.                                           }
{                                                                              }
{******************************************************************************}
unit MarkdownAlerts;

{ GitHub alerts (mexAlerts):

    > [!NOTE]
    > Useful information.

  A block quote at the top level of the document whose first paragraph starts
  with a line holding only [!NOTE], [!TIP], [!IMPORTANT], [!WARNING] or
  [!CAUTION] (any case) becomes an nkAlert node; the marker line is removed.
  Rendered as GitHub does:

    <div class="markdown-alert markdown-alert-note">
    <p class="markdown-alert-title">Note</p>
    ...
    </div> }

interface

uses
  System.SysUtils,
  MarkdownAST;

/// <summary>Turns the qualifying top-level block quotes of Document into
/// alerts. Removed nodes are unlinked and added to Removed (freed by the caller).</summary>
procedure ApplyGitHubAlerts(Document: TMarkdownNode; Removed: TMarkdownNodeList);

/// <summary>Title of an alert type (NOTE -> Note).</summary>
function AlertTitle(const AlertType: string): string;

implementation

uses
  MarkdownTextUtils;

const
  AlertTypes: array[0..4] of string = ('NOTE', 'TIP', 'IMPORTANT', 'WARNING', 'CAUTION');

function AlertTitle(const AlertType: string): string;
begin
  Result := UpperCase(Copy(AlertType, 1, 1)) + LowerCase(Copy(AlertType, 2, MaxInt));
end;

function ParseMarker(const Text: string; out AlertType: string): Boolean;
var
  Trimmed, Name: string;
  K: Integer;
begin
  Result := False;
  AlertType := '';
  Trimmed := TrimSpaceTabNewline(Text);
  if (Length(Trimmed) < 4) or not StartsAt(Trimmed, 1, '[!') or (Trimmed[Length(Trimmed)] <> ']') then
    Exit;
  Name := UpperCase(Copy(Trimmed, 3, Length(Trimmed) - 3));
  for K := Low(AlertTypes) to High(AlertTypes) do
    if Name = AlertTypes[K] then
    begin
      AlertType := Name;
      Exit(True);
    end;
end;

procedure TagIfAlert(Quote: TMarkdownNode; Removed: TMarkdownNodeList);
var
  Paragraph, Child, NextChild: TMarkdownNode;
  Line, AlertType: string;
begin
  Paragraph := Quote.FirstChild;
  if (Paragraph = nil) or (Paragraph.Kind <> nkParagraph) then
    Exit;
  // the marker is the whole first line: only text up to the first line break
  Line := '';
  Child := Paragraph.FirstChild;
  while (Child <> nil) and not (Child.Kind in [nkSoftBreak, nkHardBreak]) do
  begin
    if Child.Kind <> nkText then
      Exit;
    Line := Line + Child.Literal;
    Child := Child.Next;
  end;
  if not ParseMarker(Line, AlertType) then
    Exit;

  // remove the marker line and its line break
  if Child <> nil then
    Child := Child.Next;
  while Paragraph.FirstChild <> Child do
  begin
    NextChild := Paragraph.FirstChild;
    NextChild.Unlink;
    Removed.Add(NextChild);
  end;
  if Paragraph.FirstChild = nil then
  begin
    Paragraph.Unlink;
    Removed.Add(Paragraph);
  end;
  Quote.Kind := nkAlert;
  Quote.AlertType := AlertType;
end;

procedure ApplyGitHubAlerts(Document: TMarkdownNode; Removed: TMarkdownNodeList);
var
  Child: TMarkdownNode;
begin
  // only top-level quotes, as on GitHub: a quote in a list or in another
  // quote keeps its marker as text
  Child := Document.FirstChild;
  while Child <> nil do
  begin
    if Child.Kind = nkBlockQuote then
      TagIfAlert(Child, Removed);
    Child := Child.Next;
  end;
end;

end.
