{******************************************************************************}
{                                                                              }
{       MarkDown Processor                                                     }
{       Delphi version of FPC-markdown by Miguel A. Risco-Castillo             }
{                                                                              }
{       Copyright (c) 2022-2025 (Ethea S.r.l.)                                 }
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
{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}
Unit MarkdownProcessor;

interface

uses
  System.Classes
  , System.SysUtils
  , MarkdownUtils
  ;

Type

  { TMarkdownProcessor }

  TMarkdownProcessor = {abstract} class
  private
    FConfig: TConfiguration;
  protected
    function GetAllowUnSafe: boolean; virtual; abstract;
    procedure SetAllowUnSafe(const Value: boolean); virtual; abstract;
  public
    class function CreateDialect(dialect : TMarkdownProcessorDialect) : TMarkdownProcessor;
    function process(source : String) : String; virtual; abstract;
    function processFile(source: String; Encoding: TEncoding = nil): String; virtual;
    property config: TConfiguration read FConfig write FConfig;
    // when AllowUnsafe = true, then the processor can create scripts etc.
    property AllowUnsafe : boolean read GetAllowUnSafe write SetAllowUnSafe;
  end;

implementation

uses
  MarkdownDaringFireball
  , MarkdownCommonMark
  , MarkdownTxtMark
  ;

{ TMarkdownProcessor }

class function TMarkdownProcessor.CreateDialect(dialect: TMarkdownProcessorDialect): TMarkdownProcessor;
begin
  case dialect of
    mdDaringFireball : result := TMarkdownDaringFireball.Create;
    mdCommonMark : result := TMarkdownCommonMark.Create;
    mdTxtMark : result := TMarkdownTxtMark.Create;
  else
    raise Exception.Create('Unknown Markdown dialect');
  end;
end;

function TMarkdownProcessor.processFile(source: String; Encoding: TEncoding): String;
var
  markdown:TStringList;
begin
  result:='';
  markdown := TStringList.Create;
  try
    markdown.LoadFromFile(source, Encoding);
    result:=process(markdown.Text);
  finally
    if assigned(markdown) then markdown.Free;
  end;
end;

end.
