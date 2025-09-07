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
unit MarkdownMathCode;

interface

uses
  System.Classes,
  System.SysUtils,
  MarkdownUtils;

function checkMathCode(out_: TStringBuilder; s: String; start: integer): integer;

implementation

function checkMathCode(out_: TStringBuilder; s: String; start: integer
  ): integer;
var
  temp: TStringBuilder;
  position: integer;
  code: String;
begin
  temp := TStringBuilder.Create();
  try
    // Check for mathcode {a^2+b^2=c^2} and generate link
    temp.Clear;
    position := TUtils.readUntil(temp, s, start + 1, [' ', '$', #10]);
    if (position <> -1) and (s[1 + position] = '$') and
       (s[position] <> '\') then
    begin
      code:= temp.ToString();
      out_.append('<img src="https://chart.googleapis.com/chart?cht=tx&chl=');
      TUtils.codeEncode(out_, code, 0);
      out_.append('" alt="');
      TUtils.appendValue(out_, code, 0, Length(code));
      out_.append(' "/>');
      exit(position);
    end;
    result := -1;
  finally
    temp.Free;
  end;
end;

end.

