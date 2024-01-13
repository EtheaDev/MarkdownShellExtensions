{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       Delphi version of FPC-markdown by Miguel A. Risco-Castillo             }
{                                                                              }
{       Copyright (c) 2022-2024 (Ethea S.r.l.)                                 }
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
Unit MarkdownTxtMark;

interface

uses
  SysUtils, Classes, TypInfo,
  MarkdownDaringFireball, MarkdownUtils;

Type

  TMarkdownTxtMark = class(TMarkdownDaringFireball)
  private
  protected
  public
    Constructor Create;
    Destructor Destroy; override;
    function process(source: String): String; override;
  end;

implementation


{ TMarkdownTxtMark }

constructor TMarkdownTxtMark.Create;
begin
  inherited;
  Config.Dialect:=mdTxtMark;
end;

destructor TMarkdownTxtMark.Destroy;
begin

  inherited;
end;

function TMarkdownTxtMark.process(source: String): String;
begin
  result:=inherited process(source);
end;


end.
