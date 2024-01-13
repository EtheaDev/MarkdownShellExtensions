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
Unit MarkdownDaringFireball;

{
  This code was translated from TxtMark (https://github.com/rjeschke/txtmark)

  Copyright (C) 2011-2015 René Jeschke <rene_jeschke@yahoo.de>
  Copyright (C) 2015+ Grahame Grieve <grahameg@gmail.com> (pascal port)

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}
interface

uses
  SysUtils, Classes, TypInfo,
  MarkdownProcessor, MarkdownUtils, MarkdownTables;

type

  TMarkdownDaringFireball = class(TMarkdownProcessor)
  private
    Femitter: TEmitter;
    FuseExtensions: boolean;
    function readLines(reader : TMarkdownReader): TBlock;
    procedure initListBlock(root: TBlock);
    procedure recurse(root: TBlock; listMode: boolean);
  protected
    function GetAllowUnSafe: boolean; override;
    procedure SetAllowUnSafe(const value: boolean); override;
  public
    Constructor Create;
    Destructor Destroy; override;
    function process(source: String): String; override;
  end;

implementation

{ TMarkdownDaringFireball }

constructor TMarkdownDaringFireball.Create;
begin
  inherited Create;
  Config := TConfiguration.Create(true);
  Femitter := TEmitter.Create(config);
  TTable.Emitter:=Femitter;
end;

destructor TMarkdownDaringFireball.Destroy;
begin
  Femitter.Free;
  Config.Free;
  inherited;
end;

function TMarkdownDaringFireball.GetAllowUnSafe: boolean;
begin
  result := not Config.safeMode;
end;

procedure TMarkdownDaringFireball.initListBlock(root: TBlock);
var
  line: TLine;
  t: TLineType;
begin
  line := root.lines;
  line := line.next;
  while (line <> nil) do
  begin
    t := line.getLineType(Config);
    if ((t in [ltOLIST,ltULIST,ltBLIST]) or
      (not line.isEmpty and (line.prevEmpty and (line.leading = 0) and not(t in [ltOLIST,ltULIST,ltBLIST])))
      ) then
      root.split(line.previous).type_ := btLIST_ITEM;
    line := line.next;
  end;
  root.split(root.lineTail).type_ := btLIST_ITEM;
end;

function TMarkdownDaringFireball.process(source: String): String;
var
  out_: TStringBuilder;
  parent, block: TBlock;
  rdr : TMarkdownReader;
begin
  FuseExtensions := Config.isDialect([mdTxtMark,mdCommonMark]);
  rdr := TMarkdownReader.Create(source);
  try
    out_ := TStringBuilder.Create;
    try
      parent := readLines(rdr);
      try
        parent.removeSurroundingEmptyLines;
        recurse(parent, false);
        block := parent.blocks;
        while (block <> nil) do
        begin
          Femitter.emit(out_, block);
          block := block.next;
        end;
        result := out_.ToString;
      finally
        parent.Free;
      end;
    finally
      out_.Free;
    end;
  finally
    rdr.Free;
  end;
end;

function TMarkdownDaringFireball.readLines(reader : TMarkdownReader): TBlock;
var
  block: TBlock;
  sb: TStringBuilder;
  c, ch: char;
  position, np: integer;
  eol, isLinkRef, lineAdded: boolean;
  lastLinkRef, lr: TLinkRef;
  line: TLine;
  id, link, comment: String;
begin
  block := TBlock.Create;
  sb := TStringBuilder.Create;
  try
    c := reader.read();
    lastLinkRef := nil;
    while (c <> #0) do
    begin
      sb.Clear;
      position := 0;
      eol := false;
      while (not eol) do
      begin
        case c of
          #0:
            eol := true;
          #10:
            begin
              c := reader.read();
              if (c = #13) then
                c := reader.read();
              eol := true;
            end;
          #13:
            begin
              c := reader.read();
              if (c = #10) then
                c := reader.read();
              eol := true;
            end;
          #9:
            begin
              np := position + (4 - (position and 3));
              while (position < np) do
              begin
                sb.append(' ');
                inc(position);
              end;
              c := reader.read();
            end;
        else
          if (c <> '<') or (not Config.panicMode) then
          begin
            inc(position);
            sb.append(c);
          end
          else
          begin
            inc(position, 4);
            sb.append('&lt;');
          end;
          c := reader.read();
        end;
      end;

      lineAdded := false;
      line := TLine.Create;
      try
        line.value := sb.ToString();
        line.Init();

        // Check for link definitions
        isLinkRef := false;
        id := '';
        link := '';
        comment := '';
        if (not line.isEmpty) and (line.leading < 4) and (line.value[1 + line.leading] = '[') then
        begin
          line.position := line.leading + 1;
          // Read ID up to ']'
          id := line.readUntil([']']);
          // Is ID valid and are there any more characters?
          if (id <> '') and ((line.position + 2) < Length(line.value)) then
          begin
            // Check for ':' ([...]:...)
            if (line.value[1 + line.position + 1] = ':') then
            begin
              line.position := line.position + 2;
              if line.skipSpaces() then
              begin
                // Check for link syntax
                if (line.value[1 + line.position] = '<') then
                begin
                  line.position := line.position + 1;
                  link := line.readUntil(['>']);
                  line.position := line.position + 1;
                end
                else
                  link := line.readUntil([' ', #10]);

                // Is link valid?
                if (link <> '') then
                begin
                  // Any non-whitespace characters following?
                  if (line.skipSpaces()) then
                  begin
                    ch := line.value[1 + line.position];
                    // Read comment
                    if (ch = '"') or (ch = '''') or (ch = '(') then
                    begin
                      line.position := line.position + 1;
                      if ch = '(' then
                        comment := line.readUntil([')'])
                      else
                        comment := line.readUntil([ch]);
                      // Valid linkRef only if comment is valid
                      if (comment <> '') then
                        isLinkRef := true;
                    end;
                  end
                  else
                    isLinkRef := true;
                end;
              end;
            end;
          end;
        end;

        if (isLinkRef) then
        begin
          if (LowerCase(id) = '$profile$') then
          begin
            if LowerCase(link) = 'extended' then
            begin
              FuseExtensions:=true;
              Config.Dialect:=mdTxtMark;
            end;
            lastLinkRef := nil;
          end
          else
          begin
            // Store linkRef and skip line
            lr := TLinkRef.Create(link, comment, (comment <> '') and (Length(link) = 1) and (link[1] = '*'));
            Femitter.addLinkRef(id, lr);
            if (comment = '') then
              lastLinkRef := lr;
          end;
        end
        else
        begin
          comment := '';
          // Check for multi-line linkRef
          if (not line.isEmpty and (lastLinkRef <> nil)) then
          begin
            line.position := line.leading;
            ch := line.value[1 + line.position];
            if (ch = '"') or (ch = '''') or (ch = '(') then
            begin
              line.position := line.position + 1;
              if ch = '(' then
                comment := line.readUntil([')'])
              else
                comment := line.readUntil([ch]);
            end;
            if (comment <> '') then
              lastLinkRef.title := comment;
            lastLinkRef := nil;
          end;

          // No multi-line linkRef, store line
          if (comment = '') then
          begin
            line.position := 0;
            block.AppendLine(line);
            lineAdded := true;
          end;
        end;
      finally
        if not lineAdded then
          line.Free;
      end;
    end;
    result := block;
  finally
    sb.Free;
  end;
end;

procedure TMarkdownDaringFireball.recurse(root: TBlock; listMode: boolean);
var
  block, list: TBlock;
  line: TLine;
  type_, t: TLineType;
  wasEmpty: boolean;
  bt: TBlockType;
begin
  line := root.lines;
  if (listMode) then
  begin
    root.removeListIndent(Config);
    if (Config.isDialect([mdTxtMark,mdCommonMark]) and (root.lines <> nil) and (root.lines.getLineType(Config) <> ltCODE)) then
      root.id := root.lines.stripID();
  end;

  while (line <> nil) and line.isEmpty do
    line := line.next;
  if (line = nil) then
    exit;

  while (line <> nil) do
  begin
    type_ := line.getLineType(Config);
    case type_ of
      ltOTHER:
        begin
          wasEmpty := line.prevEmpty;
          while (line <> nil) and (not line.isEmpty) do
          begin
            t := line.getLineType(Config);
            if (listMode or FuseExtensions) and (t in [ltOLIST,ltULIST,ltBLIST]) then
              break;
            if (FuseExtensions and (t in [ltCODE, ltFENCED_CODE])) then
              break;
            if (t in [ltHEADLINE, ltHEADLINE1, ltHEADLINE2, ltHR, ltBQUOTE, ltXML, ltTABLE]) then
              break;
            line := line.next;
          end;

          if (line <> nil) and not line.isEmpty then
          begin
            if (listMode and not wasEmpty) then
              bt := btNONE
            else
              bt := btPARAGRAPH;
            if line = nil then
              root.split(root.lineTail).type_ := bt
            else
              root.split(line.previous).type_ := bt;
            root.removeLeadingEmptyLines();
          end
          else
          begin
            if (listMode and ((line = nil) or (not line.isEmpty)) and not wasEmpty) then
              bt := btNONE
            else
              bt := btPARAGRAPH;
            root.removeLeadingEmptyLines();
            if (line <> nil) then
              root.split(line.previous).type_ := bt
            else
              root.split(root.lineTail).type_ := bt;
          end;
          line := root.lines;
        end;
      ltCODE:
        begin
          while (line <> nil) and (line.isEmpty or (line.leading > 3)) do
            line := line.next;
          if (line <> nil) then
            block := root.split(line.previous)
          else
            block := root.split(root.lineTail);
          block.type_ := btCODE;
          block.removeSurroundingEmptyLines();
        end;
      ltXML:
        begin
          if (line.previous <> nil) then
            // FIXME ... this looks wrong
            root.split(line.previous);
          root.split(line.xmlEndLine).type_ := btXML;
          root.removeLeadingEmptyLines();
          line := root.lines;
        end;
      ltBQUOTE:
        begin
          while (line <> nil) do
          begin
            if (not line.isEmpty and (line.prevEmpty and (line.leading = 0) and (line.getLineType(Config) <> ltBQUOTE))) then
              break;
            line := line.next;
          end;
          if line <> nil then
            block := root.split(line.previous)
          else
            block := root.split(root.lineTail);
          block.type_ := btBLOCKQUOTE;
          block.removeSurroundingEmptyLines();
          block.removeBlockQuotePrefix();
          recurse(block, false);
          line := root.lines;
        end;
      ltHR:
        begin
          if (line.previous <> nil) then
            // FIXME ... this looks wrong
            root.split(line.previous);
          root.split(line).type_ := btRULER;
          root.removeLeadingEmptyLines();
          line := root.lines;
        end;
      ltFENCED_CODE:
        begin
          line := line.next;
          while (line <> nil) do
          begin
            if (line.getLineType(Config) = ltFENCED_CODE) then break;
            line := line.next;
          end;
          if (line <> nil) then
            line := line.next; //skip close fenced code
          if line <> nil then
            block := root.split(line.previous) // end of fenced code
          else
            block := root.split(root.lineTail); // end of the block
          block.removeSurroundingEmptyLines();  // trim block
          block.type_ := btFENCED_CODE;
          block.meta := TUtils.getMetaFromFence(block.lines.value);
          block.lines.setEmpty();  // blank first fenced code
          if (block.lineTail.getLineType(Config) = ltFENCED_CODE) then
            block.lineTail.setEmpty(); // blank last fenced code
          block.removeSurroundingEmptyLines(); //trim block
        end;
      ltHEADLINE, ltHEADLINE1, ltHEADLINE2:
        begin
          if (line.previous <> nil) then
            root.split(line.previous);
          if (type_ <> ltHEADLINE) then
            line.next.setEmpty();
          block := root.split(line);
          block.type_ := btHEADLINE;
          if (type_ <> ltHEADLINE) then
            if type_ = ltHEADLINE1 then
              block.hlDepth := 1
            else
              block.hlDepth := 2;
          if Config.isDialect([mdTxtMark,mdCommonMark]) then
            block.id := block.lines.stripID();
          block.transfromHeadline();
          root.removeLeadingEmptyLines();
          line := root.lines;
        end;
      ltOLIST, ltULIST, ltBLIST:
        begin
          while (line <> nil) do
          begin
            t := line.getLineType(Config);
            if (not line.isEmpty and (line.prevEmpty and (line.leading = 0) and (not(t = type_)))) then
              break;
            line := line.next;
          end;
          if line <> nil then
            list := root.split(line.previous)
          else
            list := root.split(root.lineTail);
          if type_ in [ltOLIST, ltBLIST] then
            list.type_ := btORDERED_LIST
          else
            list.type_ := btUNORDERED_LIST;
          list.lines.prevEmpty := false;
          list.lineTail.nextEmpty := false;
          list.removeSurroundingEmptyLines();
          list.lineTail.nextEmpty := false;
          list.lines.prevEmpty := list.lineTail.nextEmpty;
          initListBlock(list);
          block := list.blocks;
          while (block <> nil) do
          begin
            recurse(block, true);
            block := block.next;
          end;
          list.expandListParagraphs();
        end;
      ltTABLE:
        begin
          while (line <> nil) do
          begin
            if not TTable.isRow(line) then break;
            line := line.next;
          end;
          if line <> nil then
            block := root.split(line.previous)
          else
            block := root.split(root.lineTail);
          block.removeSurroundingEmptyLines();
          block.type_ := btTABLE
        end;
      else
          line := line.next;
    end;
  end;
end;

procedure TMarkdownDaringFireball.SetAllowUnSafe(const value: boolean);
begin
  Config.safeMode := not value;
end;

end.
