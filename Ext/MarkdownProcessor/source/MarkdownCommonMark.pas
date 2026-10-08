{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       CommonMark dialect                                                     }
{                                                                              }
{       Copyright (c) 2022-2026 (Ethea S.r.l.)                                 }
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
unit MarkdownCommonMark;

{ mdCommonMark: CommonMark 0.31.2 on the new engine (MarkdownBlockParser,
  MarkdownInlineParser, MarkdownHtmlRenderer). It no longer derives from
  TMarkdownDaringFireball: the legacy dialects keep the old engine.
  mdGFM: the same engine with the GFM 0.29 extensions enabled.
  mdGitHub (the default dialect): what github.com renders, GFM plus math,
  alerts and mermaid diagrams.
  Both can enable more syntax through Config.Extensions.
  See docs/COMMONMARK_PLAN.md. }

interface

uses
  System.SysUtils,
  System.Classes,
  MarkdownProcessor,
  MarkdownUtils,
  MarkdownAST;

type
  TMarkdownCommonMark = class(TMarkdownProcessor)
  protected
    function GetAllowUnSafe: boolean; override;
    procedure SetAllowUnSafe(const Value: boolean); override;
    /// <summary>Parses into a document node owned by the caller (ReleaseOwnership).</summary>
    function ParseDocument(const ASource: string): TMarkdownNode; virtual;
    function RenderDocument(ADocument: TMarkdownNode): string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Process(const ASource: string): string; override;
    function Parse(const ASource: string): IMarkdownNode; override;
    function Render(const ADocument: IMarkdownNode): string; override;
  end;

  /// <summary>mdGFM: GitHub Flavored Markdown 0.29 (CommonMark + tables, task
  /// lists, strikethrough, extended autolinks, disallowed raw HTML).</summary>
  TMarkdownGFM = class(TMarkdownCommonMark)
  public
    constructor Create;
  end;

  /// <summary>mdGitHub: GFM + math + alerts + mermaid, as github.com.</summary>
  TMarkdownGitHub = class(TMarkdownCommonMark)
  public
    constructor Create;
  end;

implementation

uses
  MarkdownBlockParser,
  MarkdownHtmlRenderer;

{ TMarkdownCommonMark }

constructor TMarkdownCommonMark.Create;
begin
  inherited Create;
  Config := TConfiguration.Create(True);
  Config.Dialect := mdCommonMark;
end;

destructor TMarkdownCommonMark.Destroy;
begin
  Config.Free;
  inherited;
end;

function TMarkdownCommonMark.GetAllowUnSafe: boolean;
begin
  Result := not Config.safeMode;
end;

procedure TMarkdownCommonMark.SetAllowUnSafe(const Value: boolean);
begin
  Config.safeMode := not Value;
end;

function TMarkdownCommonMark.ParseDocument(const ASource: string): TMarkdownNode;
var
  Parser: TMarkdownBlockParser;
begin
  Parser := TMarkdownBlockParser.Create;
  try
    Parser.Extensions := Config.Extensions;
    Result := Parser.Parse(ASource);
  finally
    Parser.Free;
  end;
end;

function TMarkdownCommonMark.RenderDocument(ADocument: TMarkdownNode): string;
var
  Renderer: TMarkdownHtmlRenderer;
begin
  Renderer := TMarkdownHtmlRenderer.Create;
  try
    Renderer.AllowUnsafe := AllowUnsafe;
    Renderer.TagFilter := mexTagFilter in Config.Extensions;
    if mexWikiLinks in Config.Extensions then
      Renderer.SpecialLinkEmitter := Config.specialLinkEmitter;
    Renderer.CodeBlockEmitter := Config.codeBlockEmitter;
    Renderer.Mermaid := mexMermaid in Config.Extensions;
    Renderer.MathRendering := Config.MathRendering;
    Result := Renderer.Render(ADocument);
  finally
    Renderer.Free;
  end;
end;

function TMarkdownCommonMark.Process(const ASource: string): string;
var
  Document: TMarkdownNode;
begin
  Document := ParseDocument(ASource);
  try
    Result := RenderDocument(Document);
  finally
    Document.ReleaseOwnership;
  end;
end;

function TMarkdownCommonMark.Parse(const ASource: string): IMarkdownNode;
var
  Document: TMarkdownNode;
begin
  Document := ParseDocument(ASource);
  Result := Document;
  Document.ReleaseOwnership;
end;

function TMarkdownCommonMark.Render(const ADocument: IMarkdownNode): string;
var
  Node: TMarkdownNode;
begin
  Node := ADocument as TMarkdownNode;
  Result := RenderDocument(Node.Document);
end;

{ TMarkdownGFM }

constructor TMarkdownGFM.Create;
begin
  inherited Create;
  Config.Dialect := mdGFM;
  Config.Extensions := GFMExtensions;
end;


{ TMarkdownGitHub }

constructor TMarkdownGitHub.Create;
begin
  inherited Create;
  Config.Dialect := mdGitHub;
  Config.Extensions := GitHubExtensions;
end;

end.
