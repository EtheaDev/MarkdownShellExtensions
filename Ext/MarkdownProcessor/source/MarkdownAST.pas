{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       CommonMark / GFM engine: abstract syntax tree                          }
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
{  The node model (linked children, walker with entering/exiting events)       }
{  follows commonmark.js (BSD-2-Clause, Copyright (c) 2014 John MacFarlane):   }
{  see docs/LICENSE-commonmark.js.txt.                                         }
{                                                                              }
{******************************************************************************}
unit MarkdownAST;

{ The tree produced by the CommonMark/GFM engine.

  Internally the parser and the renderer work on TMarkdownNode objects. The
  public, read-only view is IMarkdownNode. Every node of a tree forwards its
  reference count to the document node, so holding any IMarkdownNode keeps the
  whole tree alive, and the tree is freed when the last reference goes away.

  Compatible with Delphi XE3: no inline variables, no [weak]. }

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TMarkdownNodeKind = (
    // blocks
    nkDocument, nkBlockQuote, nkList, nkListItem, nkParagraph, nkHeading,
    nkThematicBreak, nkCodeBlock, nkHtmlBlock,
    nkTable, nkTableHead, nkTableBody, nkTableRow, nkTableCell,
    nkAlert, nkMathBlock,
    // inlines
    nkText, nkSoftBreak, nkHardBreak, nkCode, nkEmphasis, nkStrong,
    nkStrikethrough, nkLink, nkImage, nkHtmlInline, nkMathInline, nkTaskMarker,
    nkSubscript, nkSuperscript, nkInsert, nkMark, nkWikiLink);

  TMarkdownListType = (mltBullet, mltOrdered);

  TMarkdownTableAlign = (mtaNone, mtaLeft, mtaCenter, mtaRight);

  /// <summary>Read-only view of a node of the syntax tree.</summary>
  IMarkdownNode = interface
    ['{4C8B7E2A-1D35-4F0B-9E6A-7B2C5D8E1F03}']
    function GetKind: TMarkdownNodeKind;
    function GetParent: IMarkdownNode;
    function GetFirstChild: IMarkdownNode;
    function GetLastChild: IMarkdownNode;
    function GetNext: IMarkdownNode;
    function GetPrevious: IMarkdownNode;
    function GetChildCount: Integer;
    function GetChild(Index: Integer): IMarkdownNode;
    function GetLiteral: string;
    function GetLevel: Integer;
    function GetInfo: string;
    function GetIsFenced: Boolean;
    function GetDestination: string;
    function GetTitle: string;
    function GetIsAutolink: Boolean;
    function GetListType: TMarkdownListType;
    function GetListStart: Integer;
    function GetListTight: Boolean;
    function GetListDelimiter: Char;
    function GetBulletChar: Char;
    function GetChecked: Boolean;
    function GetAlign: TMarkdownTableAlign;
    function GetAlertType: string;
    function GetIsDisplay: Boolean;
    function GetStartLine: Integer;
    function GetEndLine: Integer;
    function GetId: string;

    property Kind: TMarkdownNodeKind read GetKind;
    property Parent: IMarkdownNode read GetParent;
    property FirstChild: IMarkdownNode read GetFirstChild;
    property LastChild: IMarkdownNode read GetLastChild;
    property Next: IMarkdownNode read GetNext;
    property Previous: IMarkdownNode read GetPrevious;
    property ChildCount: Integer read GetChildCount;
    property Children[Index: Integer]: IMarkdownNode read GetChild;
    /// <summary>Text, code span, code block, raw HTML and math content.</summary>
    property Literal: string read GetLiteral;
    /// <summary>Heading level (1..6).</summary>
    property Level: Integer read GetLevel;
    /// <summary>Whole info string of a fenced code block.</summary>
    property Info: string read GetInfo;
    property IsFenced: Boolean read GetIsFenced;
    /// <summary>Link/image destination (already normalized).</summary>
    property Destination: string read GetDestination;
    property Title: string read GetTitle;
    property IsAutolink: Boolean read GetIsAutolink;
    property ListType: TMarkdownListType read GetListType;
    property ListStart: Integer read GetListStart;
    property ListTight: Boolean read GetListTight;
    property ListDelimiter: Char read GetListDelimiter;
    property BulletChar: Char read GetBulletChar;
    /// <summary>Task list marker state.</summary>
    property Checked: Boolean read GetChecked;
    /// <summary>Table cell alignment.</summary>
    property Align: TMarkdownTableAlign read GetAlign;
    /// <summary>GitHub alert type: NOTE, TIP, IMPORTANT, WARNING, CAUTION.</summary>
    property AlertType: string read GetAlertType;
    /// <summary>Inline math written as $$...$$.</summary>
    property IsDisplay: Boolean read GetIsDisplay;
    /// <summary>First and last source line (1-based) of a block node.</summary>
    property StartLine: Integer read GetStartLine;
    property EndLine: Integer read GetEndLine;
    /// <summary>Heading id ({#id} or automatic slug), when enabled.</summary>
    property Id: string read GetId;
  end;

  TMarkdownNode = class;

  TMarkdownNodeList = TList<TMarkdownNode>;

  /// <summary>A node of the syntax tree. Fields are public for the parser and
  /// the renderer; consumers should use IMarkdownNode.</summary>
  TMarkdownNode = class(TObject, IInterface, IMarkdownNode)
  private
    FRefCount: Integer; // used on the document node only
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IMarkdownNode }
    function GetKind: TMarkdownNodeKind;
    function GetParent: IMarkdownNode;
    function GetFirstChild: IMarkdownNode;
    function GetLastChild: IMarkdownNode;
    function GetNext: IMarkdownNode;
    function GetPrevious: IMarkdownNode;
    function GetChildCount: Integer;
    function GetChild(Index: Integer): IMarkdownNode;
    function GetLiteral: string;
    function GetLevel: Integer;
    function GetInfo: string;
    function GetIsFenced: Boolean;
    function GetDestination: string;
    function GetTitle: string;
    function GetIsAutolink: Boolean;
    function GetListType: TMarkdownListType;
    function GetListStart: Integer;
    function GetListTight: Boolean;
    function GetListDelimiter: Char;
    function GetBulletChar: Char;
    function GetChecked: Boolean;
    function GetAlign: TMarkdownTableAlign;
    function GetAlertType: string;
    function GetIsDisplay: Boolean;
    function GetStartLine: Integer;
    function GetEndLine: Integer;
    function GetId: string;
  public
    Kind: TMarkdownNodeKind;
    Document: TMarkdownNode;
    Parent: TMarkdownNode;
    FirstChild: TMarkdownNode;
    LastChild: TMarkdownNode;
    Next: TMarkdownNode;
    Prev: TMarkdownNode;

    Literal: string;
    Level: Integer;
    Info: string;
    Destination: string;
    Title: string;
    IsAutolink: Boolean;
    IsFenced: Boolean;
    Checked: Boolean;
    Align: TMarkdownTableAlign;
    AlertType: string;
    IsDisplay: Boolean;
    Id: string;

    // list / list item data
    ListType: TMarkdownListType;
    ListTight: Boolean;
    BulletChar: Char;
    ListStart: Integer;
    ListDelimiter: Char;
    ListPadding: Integer;
    MarkerOffset: Integer;

    // source position
    StartLine: Integer;
    StartColumn: Integer;
    EndLine: Integer;
    EndColumn: Integer;

    // block parser state
    IsOpen: Boolean;
    StringContent: string;
    FenceChar: Char;
    FenceLength: Integer;
    FenceOffset: Integer;
    HtmlBlockType: Integer;
    LastLineBlank: Boolean;

    constructor Create(AKind: TMarkdownNodeKind; ADocument: TMarkdownNode);
    /// <summary>Creates a document node: its reference count starts at 1,
    /// owned by the creator, who must call ReleaseOwnership.</summary>
    class function CreateDocument: TMarkdownNode;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    /// <summary>Drops the creator's reference of a document: frees the tree
    /// unless an IMarkdownNode still references it.</summary>
    procedure ReleaseOwnership;

    function IsBlock: Boolean;
    function IsContainer: Boolean;
    procedure AppendChild(Child: TMarkdownNode);
    procedure PrependChild(Child: TMarkdownNode);
    procedure InsertAfter(Sibling: TMarkdownNode);
    procedure InsertBefore(Sibling: TMarkdownNode);
    /// <summary>Detaches the node (and its subtree) from the tree, without freeing it.</summary>
    procedure Unlink;
    /// <summary>Frees all children, without recursion (safe on deep trees).</summary>
    procedure FreeChildren;
  end;

  /// <summary>Iterative depth-first walk with entering/exiting events
  /// (no recursion, safe on deeply nested documents).</summary>
  TMarkdownWalker = record
  private
    FRoot: TMarkdownNode;
    FCurrent: TMarkdownNode;
    FEntering: Boolean;
  public
    procedure Init(ARoot: TMarkdownNode);
    /// <summary>Returns False when the walk is over.</summary>
    function Next(out Node: TMarkdownNode; out Entering: Boolean): Boolean;
    /// <summary>Restarts the walk at the given node, entering it.</summary>
    procedure ResumeAt(Node: TMarkdownNode; Entering: Boolean);
  end;

implementation

{ TMarkdownNode }

constructor TMarkdownNode.Create(AKind: TMarkdownNodeKind; ADocument: TMarkdownNode);
begin
  inherited Create;
  Kind := AKind;
  if ADocument = nil then
    Document := Self
  else
    Document := ADocument;
  ListTight := True;
  ListStart := 1;
end;

class function TMarkdownNode.CreateDocument: TMarkdownNode;
begin
  Result := TMarkdownNode.Create(nkDocument, nil);
end;

procedure TMarkdownNode.AfterConstruction;
begin
  inherited;
  // A document starts owned by its creator (see ReleaseOwnership).
  if Document = Self then
    FRefCount := 1;
end;

destructor TMarkdownNode.Destroy;
begin
  FreeChildren;
  inherited;
end;

procedure TMarkdownNode.ReleaseOwnership;
begin
  _Release;
end;

function TMarkdownNode.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TMarkdownNode._AddRef: Integer;
begin
  Inc(Document.FRefCount);
  Result := Document.FRefCount;
end;

function TMarkdownNode._Release: Integer;
var
  Doc: TMarkdownNode;
begin
  Doc := Document;
  Dec(Doc.FRefCount);
  Result := Doc.FRefCount;
  if Result = 0 then
    Doc.Free;
end;

function TMarkdownNode.IsBlock: Boolean;
begin
  Result := Kind <= nkMathBlock;
end;

function TMarkdownNode.IsContainer: Boolean;
begin
  case Kind of
    nkDocument, nkBlockQuote, nkList, nkListItem, nkParagraph, nkHeading,
    nkTable, nkTableHead, nkTableBody, nkTableRow, nkTableCell, nkAlert,
    nkEmphasis, nkStrong, nkStrikethrough, nkLink, nkImage,
    nkSubscript, nkSuperscript, nkInsert, nkMark, nkWikiLink:
      Result := True;
  else
    Result := False;
  end;
end;

procedure TMarkdownNode.AppendChild(Child: TMarkdownNode);
begin
  Child.Unlink;
  Child.Parent := Self;
  if LastChild <> nil then
  begin
    LastChild.Next := Child;
    Child.Prev := LastChild;
    LastChild := Child;
  end
  else
  begin
    FirstChild := Child;
    LastChild := Child;
  end;
end;

procedure TMarkdownNode.PrependChild(Child: TMarkdownNode);
begin
  Child.Unlink;
  Child.Parent := Self;
  if FirstChild <> nil then
  begin
    FirstChild.Prev := Child;
    Child.Next := FirstChild;
    FirstChild := Child;
  end
  else
  begin
    FirstChild := Child;
    LastChild := Child;
  end;
end;

procedure TMarkdownNode.InsertAfter(Sibling: TMarkdownNode);
begin
  Sibling.Unlink;
  Sibling.Next := Next;
  if Sibling.Next <> nil then
    Sibling.Next.Prev := Sibling;
  Sibling.Prev := Self;
  Next := Sibling;
  Sibling.Parent := Parent;
  if (Parent <> nil) and (Sibling.Next = nil) then
    Parent.LastChild := Sibling;
end;

procedure TMarkdownNode.InsertBefore(Sibling: TMarkdownNode);
begin
  Sibling.Unlink;
  Sibling.Prev := Prev;
  if Sibling.Prev <> nil then
    Sibling.Prev.Next := Sibling;
  Sibling.Next := Self;
  Prev := Sibling;
  Sibling.Parent := Parent;
  if (Parent <> nil) and (Sibling.Prev = nil) then
    Parent.FirstChild := Sibling;
end;

procedure TMarkdownNode.Unlink;
begin
  if Prev <> nil then
    Prev.Next := Next
  else if Parent <> nil then
    Parent.FirstChild := Next;
  if Next <> nil then
    Next.Prev := Prev
  else if Parent <> nil then
    Parent.LastChild := Prev;
  Parent := nil;
  Next := nil;
  Prev := nil;
end;

procedure TMarkdownNode.FreeChildren;
var
  Stack: TMarkdownNodeList;
  Node, Child, NextChild: TMarkdownNode;
begin
  if FirstChild = nil then
    Exit;
  Stack := TMarkdownNodeList.Create;
  try
    Child := FirstChild;
    while Child <> nil do
    begin
      Stack.Add(Child);
      Child := Child.Next;
    end;
    FirstChild := nil;
    LastChild := nil;
    while Stack.Count > 0 do
    begin
      Node := Stack[Stack.Count - 1];
      Stack.Delete(Stack.Count - 1);
      Child := Node.FirstChild;
      while Child <> nil do
      begin
        NextChild := Child.Next;
        Stack.Add(Child);
        Child := NextChild;
      end;
      Node.FirstChild := nil;
      Node.LastChild := nil;
      Node.Free;
    end;
  finally
    Stack.Free;
  end;
end;

function TMarkdownNode.GetKind: TMarkdownNodeKind;
begin
  Result := Kind;
end;

function TMarkdownNode.GetParent: IMarkdownNode;
begin
  Result := Parent;
end;

function TMarkdownNode.GetFirstChild: IMarkdownNode;
begin
  Result := FirstChild;
end;

function TMarkdownNode.GetLastChild: IMarkdownNode;
begin
  Result := LastChild;
end;

function TMarkdownNode.GetNext: IMarkdownNode;
begin
  Result := Next;
end;

function TMarkdownNode.GetPrevious: IMarkdownNode;
begin
  Result := Prev;
end;

function TMarkdownNode.GetChildCount: Integer;
var
  Child: TMarkdownNode;
begin
  Result := 0;
  Child := FirstChild;
  while Child <> nil do
  begin
    Inc(Result);
    Child := Child.Next;
  end;
end;

function TMarkdownNode.GetChild(Index: Integer): IMarkdownNode;
var
  Child: TMarkdownNode;
begin
  Child := FirstChild;
  while (Child <> nil) and (Index > 0) do
  begin
    Dec(Index);
    Child := Child.Next;
  end;
  if Child = nil then
    raise EListError.CreateFmt('Child index out of bounds (%d)', [Index]);
  Result := Child;
end;

function TMarkdownNode.GetLiteral: string;
begin
  Result := Literal;
end;

function TMarkdownNode.GetLevel: Integer;
begin
  Result := Level;
end;

function TMarkdownNode.GetInfo: string;
begin
  Result := Info;
end;

function TMarkdownNode.GetIsFenced: Boolean;
begin
  Result := IsFenced;
end;

function TMarkdownNode.GetDestination: string;
begin
  Result := Destination;
end;

function TMarkdownNode.GetTitle: string;
begin
  Result := Title;
end;

function TMarkdownNode.GetIsAutolink: Boolean;
begin
  Result := IsAutolink;
end;

function TMarkdownNode.GetListType: TMarkdownListType;
begin
  Result := ListType;
end;

function TMarkdownNode.GetListStart: Integer;
begin
  Result := ListStart;
end;

function TMarkdownNode.GetListTight: Boolean;
begin
  Result := ListTight;
end;

function TMarkdownNode.GetListDelimiter: Char;
begin
  Result := ListDelimiter;
end;

function TMarkdownNode.GetBulletChar: Char;
begin
  Result := BulletChar;
end;

function TMarkdownNode.GetChecked: Boolean;
begin
  Result := Checked;
end;

function TMarkdownNode.GetAlign: TMarkdownTableAlign;
begin
  Result := Align;
end;

function TMarkdownNode.GetAlertType: string;
begin
  Result := AlertType;
end;

function TMarkdownNode.GetIsDisplay: Boolean;
begin
  Result := IsDisplay;
end;

function TMarkdownNode.GetStartLine: Integer;
begin
  Result := StartLine;
end;

function TMarkdownNode.GetEndLine: Integer;
begin
  Result := EndLine;
end;

function TMarkdownNode.GetId: string;
begin
  Result := Id;
end;

{ TMarkdownWalker }

procedure TMarkdownWalker.Init(ARoot: TMarkdownNode);
begin
  FRoot := ARoot;
  FCurrent := ARoot;
  FEntering := True;
end;

procedure TMarkdownWalker.ResumeAt(Node: TMarkdownNode; Entering: Boolean);
begin
  FCurrent := Node;
  FEntering := Entering;
end;

function TMarkdownWalker.Next(out Node: TMarkdownNode; out Entering: Boolean): Boolean;
var
  Cur: TMarkdownNode;
begin
  Cur := FCurrent;
  if Cur = nil then
  begin
    Node := nil;
    Entering := False;
    Exit(False);
  end;
  Node := Cur;
  Entering := FEntering;
  if FEntering and Cur.IsContainer then
  begin
    if Cur.FirstChild <> nil then
    begin
      FCurrent := Cur.FirstChild;
      FEntering := True;
    end
    else
      FEntering := False; // stay on the node, exiting next
  end
  else if Cur = FRoot then
    FCurrent := nil
  else if Cur.Next = nil then
  begin
    FCurrent := Cur.Parent;
    FEntering := False;
  end
  else
  begin
    FCurrent := Cur.Next;
    FEntering := True;
  end;
  Result := True;
end;

end.
