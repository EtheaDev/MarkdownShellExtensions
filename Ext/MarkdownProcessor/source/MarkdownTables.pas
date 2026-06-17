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
unit MarkdownTables;

interface

uses
  System.Classes,
  System.SysUtils,
  MarkdownUtils;

type

  { TTable }

  TTable = class
  public
    class var
      Rows:integer;
      Cols:integer;
      Emitter:TEmitter;
    class procedure RowSplit (Input: string; const Strings: TStrings);
    class function ProcTable(var L: TLine): string;
    class procedure emitTableLines(out_: TStringBuilder; lines: TLine);
    class function hasFormatChars(L: Tline): integer;
    class function isRow(L: Tline): boolean;
    class function ColCount(L: Tline): integer;
    class procedure AlignFormat(f: string; sl: TStringList);
  end;


implementation

class procedure TTable.RowSplit (Input: string; const Strings: TStrings);
var
  i,j:integer;
  s:string;
begin
  if not Assigned(Strings) then exit;
  i:=1;
  j:=Length(Input);
  if Input[i]='|' then inc(i);
  if (Input[j]='|') and (Input[j-1]<>'\') then dec(j);
  s:='';
  Strings.Clear;
  while (i<=j) do
  begin
    if ((Input[i]='|') and (Input[i-1]<>'\')) then
    begin
      Strings.Append(s);
      s:='';
    end else s:=s+Input[i];
    inc(i);
  end;
  Strings.Append(s);
end;

class function TTable.ProcTable(var L: TLine): string;
(*
| First Header | Second Header | Third Header |
| :----------- | :-----------: | -----------: |
| Left         |    Center     |        Right |
| Second row   |  **strong**   |     *italic* |
*)

  //Emit the inline markdown of a single cell in isolation, so that inline
  //constructs (emphasis, subscript ~, strike ~~, code...) cannot span across
  //cell/row boundaries (e.g. a lone '~' matching a '~' in another cell).
  function EmitCell(const ACell: string): string;
  var
    LCell: TStringBuilder;
  begin
    if not Assigned(Emitter) then
      Exit(ACell);
    LCell := TStringBuilder.Create;
    try
      Emitter.recursiveEmitLine(LCell, ACell, 0, mtNONE);
      Result := LCell.ToString;
    finally
      LCell.Free;
    end;
  end;

var
  cell,outs,t,t2:string;
  orig:TLine;
  first:boolean;
  col:integer;
  tsl:TStringList;
  ColAlign:TStringList;

begin
  outs:='';
  first:=true;
  tsl:=TStringList.Create;
  ColAlign:=TStringList.Create;
  orig:=L;

  while (orig<>nil) and not(orig.isEmpty) do
  begin
    t:=trim(orig.Value);
    t2:='';
    RowSplit(t, tsl);
    if (first) then  // get header
    begin
      AlignFormat(orig.next.Value,ColAlign); // get format row
      col:=0;
      for cell in tsl do
      begin
        t2:=t2+'    <th'+ColAlign[col]+EmitCell(trim(cell))+'</th>'#10;
        inc(col);
      end;
      first:=false;
      orig:=orig.next;  // skip table format row
    end else
    begin
      col:=0;
      for cell in tsl do
      begin
        t2:=t2+'    <td'+ColAlign[col]+EmitCell(trim(cell))+'</td>'#10;
        inc(col);
      end;
    end;
    if t2<>'' then
    begin
      outs := outs+'  <tr>'#10+t2+'  </tr>'#10;
      inc(Rows);
    end;
    orig:=orig.next;
  end;
  t:='<table>'#10;
  t:=t+outs+'</table>'#10+'<br/>'#10;
  tsl.free;
  ColAlign.free;
  ProcTable:=t;
end;

class procedure TTable.emitTableLines(out_: TStringBuilder; lines: TLine);
var line:TLine;
begin
  line := lines;
  //ProcTable already emits each cell's inline markdown in isolation and builds
  //the literal <table>/<tr>/<td> structure, so append it as-is. (Emitting the
  //whole table through recursiveEmitLine would let inline tokens span cells.)
  out_.Append(ProcTable(line));
end;

class function TTable.hasFormatChars(L: Tline): integer;
var
  i,j:integer;
  sepCount: integer;
  hasDash: boolean;
begin
  Cols:=0;
  if not Assigned(L) or L.isEmpty then exit(0);
  i:=L.leading+1;
  j:=Length(L.value)-L.trailing;
  if i>4 then exit(0); // more than 4 spaces of indentation

  // Skip edge pipes for internal separator counting
  if L.value[i]='|' then inc(i);
  if L.value[j]='|' then dec(j);

  sepCount := 0;
  hasDash := false;

  while (i<=j) do
  begin
    if not CharInSet(L.value[i], [' ','|','-',':']) then
      exit(0);
    if L.value[i]='|' then inc(sepCount);
    if L.value[i]='-' then hasDash := true;
    inc(i);
  end;

  // Must have at least one dash to be a format row
  if not hasDash then exit(0);

  // Store internal separator count for row checks
  Cols := sepCount;

  // Return positive value even for single-column (0 internal separators)
  if sepCount = 0 then
    result := 1
  else
    result := sepCount;
end;

class function TTable.isRow(L: Tline): boolean;
var
  c:integer;
  s:string;
begin
  c := ColCount(L);
  s := Trim(L.value);
  // Must contain a pipe somewhere and match the format's separator count
  exit((Pos('|', s) > 0) and (c = Cols));
end;

class function TTable.ColCount(L: Tline): integer;
var
  i,j,r:integer;
begin
  if not Assigned(L) or L.isEmpty then exit(0);
  r:=0;
  i:=L.leading+1;
  j:=Length(L.value)-L.trailing;
  if i>4 then exit(0); // more of 4 spaces of identation
  if L.value[i]='|' then inc(i);
  if L.value[j]='|' then dec(j);
  while (i<=j) do
  begin
    if (L.value[i]='|') and (L.value[i-1]<>'\') then inc(r);
    inc(i);
  end;
  exit(r);
end;

class procedure TTable.AlignFormat(f: string; sl: TStringList);
var
  i:integer;
begin
  f:=trim(f);
  if f[1]='|' then f:=Copy(f,2);
  if f[length(f)]='|' then f:=Copy(f,1,length(f)-1);
  RowSplit(f, sl);
  for i:=0 to sl.Count-1 do
  begin
    if sl[i].Contains(':-') and sl[i].Contains('-:') then sl[i]:=' align="center">'
    else if sl[i].Contains(':-') then sl[i]:=' align="left">'
    else if sl[i].Contains('-:') then sl[i]:=' align="right">'
    else sl[i]:='>';
  end;
end;

end.

