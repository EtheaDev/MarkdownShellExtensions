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
  System.StrUtils,
  MarkdownUtils;

function checkMathCode(out_: TStringBuilder; s: String; start: integer): integer;

implementation

const
  equationapi = '<img src="https://latex.codecogs.com/png.image?';
  //equationapi = '<img src="https://latex.codecogs.com/svg.image?';
  //equationapi = '<img src="https://chart.googleapis.com/chart?cht=tx&chl='; // deprecated

// Check for enclosed mathcode ${a^2+b^2=c^2}$ or $${a^2+b^2=c^2}$$ and generate link
function checkMathCode(out_: TStringBuilder; s: String; start: integer): integer;
var
  temp: TStringBuilder;
  position: integer;
  code: String;
  delimiterCount: integer;
begin
  temp := TStringBuilder.Create();
  try
    temp.Clear;
    if (start + 2 <= Length(s)) and (s[start + 2] = '$') then
      delimiterCount := 2
    else
      delimiterCount := 1;
    position := TUtils.readRawUntil(temp, s, start + delimiterCount, ['$', #10]);
    if     (position <> -1)         // end was found
       and (s[1 + position] = '$')  // closing $ was found
       and (s[position] <> '\')     // is not a \$
       and (s[position] <> ' ')     // is not a space before $
       and not CharInSet(s[1 + position], ['0'..'9'])  // no digit after the closing $
    then
    begin
      code:= temp.ToString();
      if delimiterCount = 2 then out_.append('<div style="text-align:center;">');
      out_.append(equationapi);
      TUtils.codeEncode(out_, code, 0);
      out_.append('" alt="');
      TUtils.appendValue(out_, code, 0, Length(code));
      out_.append(IfThen(delimiterCount = 1, ' "/>', ' "/></div>'));
      exit(position + delimiterCount - 1);
    end;
    result := -1;
  finally
    temp.Free;
  end;
end;

end.

