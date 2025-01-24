{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions                                              }
{       (Preview Panel, Thumbnail Icon, MD Text Editor)                        }
{                                                                              }
{       Copyright (c) 2021-2025 (Ethea S.r.l.)                                 }
{       Author: Ariel Montes                                                   }
{                                                                              }
{       https://github.com/EtheaDev/MarkdownShellExtensions                    }
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
unit MDShellEx.dlgCreateTable;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.NumberBox, Vcl.StyledButton;

type
  TdlgCreateTable = class(TForm)
    gbTableSize: TGroupBox;
    paTable: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape14: TShape;
    Shape15: TShape;
    Shape16: TShape;
    Shape17: TShape;
    Shape18: TShape;
    Shape19: TShape;
    Shape20: TShape;
    Shape21: TShape;
    Shape22: TShape;
    Shape23: TShape;
    Shape24: TShape;
    Shape25: TShape;
    Shape26: TShape;
    Shape27: TShape;
    Shape28: TShape;
    Shape29: TShape;
    Shape30: TShape;
    Shape31: TShape;
    Shape32: TShape;
    Shape33: TShape;
    Shape34: TShape;
    Shape35: TShape;
    Shape36: TShape;
    Shape37: TShape;
    Shape38: TShape;
    Shape39: TShape;
    Shape40: TShape;
    Shape41: TShape;
    Shape42: TShape;
    Shape43: TShape;
    Shape44: TShape;
    Shape45: TShape;
    Shape46: TShape;
    Shape47: TShape;
    Shape48: TShape;
    Shape49: TShape;
    Shape50: TShape;
    Shape51: TShape;
    Shape52: TShape;
    Shape53: TShape;
    Shape54: TShape;
    Shape55: TShape;
    Shape56: TShape;
    Shape57: TShape;
    Shape58: TShape;
    Shape59: TShape;
    Shape60: TShape;
    Shape61: TShape;
    Shape62: TShape;
    Shape63: TShape;
    Shape64: TShape;
    Shape65: TShape;
    Shape66: TShape;
    Shape67: TShape;
    Shape68: TShape;
    Shape69: TShape;
    Shape70: TShape;
    Shape71: TShape;
    Shape72: TShape;
    Shape73: TShape;
    Shape74: TShape;
    Shape75: TShape;
    Shape76: TShape;
    Shape77: TShape;
    Shape78: TShape;
    Shape79: TShape;
    Shape80: TShape;
    Shape81: TShape;
    lbSize: TLabel;
    lbColumns: TLabel;
    nbColumns: TNumberBox;
    lbRows: TLabel;
    nbRows: TNumberBox;
    rdTableAlignment: TRadioGroup;
    cbPlaceholders: TCheckBox;
    btConfirm: TStyledButton;
    btCancel: TStyledButton;
    procedure FormCreate(Sender: TObject);
    procedure paTableMouseLeave(Sender: TObject);
    procedure ShapeMouseEnter(Sender: TObject);
    procedure ShapeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btConfirmClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    FSelectedRow: Integer;
    FSelectedCol: Integer;
    FPositionRow: Integer;
    FPositionCol: Integer;
    FSelectedShape: Boolean;
    procedure GetRowCol(const Tag: Integer; out Row, Col: Integer);
    procedure UpdateShapes;
    function GenerateMarkDownTable(const Cols, Rows: Integer): string;
  public
    ATableText: string;
  end;

var
  dlgCreateTable: TdlgCreateTable;

implementation

{$R *.dfm}

uses
  Themes;

procedure TdlgCreateTable.FormCreate(Sender: TObject);
begin
  UpdateShapes;
end;

procedure TdlgCreateTable.GetRowCol(const Tag: Integer; out Row, Col: Integer);
begin
  Row := Tag div 10;
  Col := Tag - (Row * 10);
end;

procedure TdlgCreateTable.paTableMouseLeave(Sender: TObject);
begin
  //Get the current cursor position
  var MousePos: TPoint;
  GetCursorPos(MousePos);
  MousePos := paTable.ScreenToClient(MousePos);

  //Check if the cursor has left the paTable component
  if (MousePos.X >= paTable.Width) or (MousePos.X <= 0) or
    (MousePos.Y >= paTable.Height) or (MousePos.Y <= 0) then
  begin
    //Clean Col and Row to their default values if there is no selected Shape
    if not FSelectedShape then
    begin
      FSelectedCol := 0;
      FSelectedRow := 0;
      FPositionRow := 0;
      FPositionCol := 0;
      UpdateShapes;
    end;
  end;
end;

procedure TdlgCreateTable.ShapeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FSelectedShape := not FSelectedShape;
  GetRowCol((Sender as TShape).Tag, FPositionRow, FPositionCol);
  //Update Col and Row if there is a selected Shape
  if FSelectedShape then
  begin
    FSelectedCol := FPositionCol;
    FSelectedRow := FPositionRow;
    //Update NumberBox values based on the selected Shape
    nbColumns.ValueInt := FSelectedCol;
    nbRows.ValueInt := FSelectedRow;
    //Prevent NumberBox value modification while Shape is selected
    nbColumns.Enabled := False;
    nbRows.Enabled := False;
  end
  else
  begin
    nbColumns.Enabled := True;
    nbRows.Enabled := True;
  end;
  UpdateShapes;
end;

procedure TdlgCreateTable.ShapeMouseEnter(Sender: TObject);
begin
  if not FSelectedShape then
  begin
    GetRowCol((Sender as TShape).Tag, FPositionRow, FPositionCol);
    UpdateShapes;
  end;
end;

procedure TdlgCreateTable.UpdateShapes;
var
  LRow, LCol: Integer;
  LShape: TShape;
begin
  //Highlight the first and second rows simultaneously to indicate that a
  //minimum of 2 columns is required
  if FPositionCol = 1 then
    FPositionCol := 2;
  //Update shapes color
  for var I := 0 to ComponentCount -1 do
  begin
    if Components[I] is TShape then
    begin
      LShape := TShape(Components[I]);
      GetRowCol(LShape.Tag, LRow, LCol);
      if (LRow <= FPositionRow) and (LCol <= FPositionCol) then
      begin
        if FSelectedShape then
          LShape.Brush.Color := TStyleManager.ActiveStyle.GetSystemColor(clBtnText)
        else
          LShape.Brush.Color := TStyleManager.ActiveStyle.GetSystemColor(clHighlight);
      end
      else
        LShape.Brush.Color := TStyleManager.ActiveStyle.GetSystemColor(clBtnShadow);
    end;
  end;
  //Update the number of highlighted columns and rows
  if FPositionRow+FPositionCol > 0 then
    lbSize.Caption := Format('%dx%d', [FPositionCol, FPositionRow])
  else
    lbSize.Caption := '';
end;

function TdlgCreateTable.GenerateMarkDownTable(const Cols, Rows: Integer): string;
var
  LTable, LLine: string;
  LCol, LRow, LAlign: Integer;
  LPlaceholders: boolean;
begin
  LAlign := rdTableAlignment.ItemIndex;
  LPlaceholders := cbPlaceholders.Checked;

  //Add header
  LTable := '|';
  for LCol := 1 to Cols do
  begin
    if LPlaceholders then
      LTable := LTable+'  Header  |'
    else
      LTable := LTable+'     |';
  end;
  LTable := LTable+sLineBreak;

  //Add alignment
  LTable := LTable+'|';
  for LCol := 1 to Cols do
  begin
    if LPlaceholders then
      LLine := '------'
    else
      LLine := '-';
    //Left
    if LAlign	= 0 then
      LTable := LTable+' :'+LLine+'- |'
    //Center
    else if LAlign = 1 then
      LTable := LTable+' :'+LLine+': |'
    //Right
    else if LAlign = 2 then
      LTable := LTable+' -'+LLine+': |';
  end;
  LTable := LTable+sLineBreak;

  //Add rows
  for LRow := 1 to Rows do
  begin
    LTable := LTable+'|';
    for LCol := 1 to Cols do
    begin
      if LPlaceholders then
        LTable := LTable+'   Row    |'
      else
        LTable := LTable+'     |';
    end;
    LTable := LTable+sLineBreak;
  end;
  result := LTable;
end;

procedure TdlgCreateTable.btConfirmClick(Sender: TObject);
begin
  if FSelectedCol < 2 then
  begin
    //Get Col value from NumberBox when no Shape selected
    if nbColumns.ValueInt >= 2 then
      FSelectedCol := nbColumns.ValueInt
    //Enforce minimum Col value of 2 with Enter key
    else
      FSelectedCol := 2;
  end;

  if FSelectedRow < 1 then
  begin
    //Get Row value from NumberBox when no Shape selected
    if nbRows.ValueInt >= 1 then
      FSelectedRow := nbRows.ValueInt
    //Enforce non-zero, positive Row values with Enter key
    else
      FSelectedRow := 1;
  end;

  ATableText := GenerateMarkDownTable(FSelectedCol, FSelectedRow);
  Close;
  ModalResult := mrOk;
end;

procedure TdlgCreateTable.btCancelClick(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

end.
