unit u_dzVclHelpers;

interface

uses
  Grids,
  u_dzVclUtils;

type
  ICustomGridHelper = interface
    function GetNonfixedRow: Integer;
    function Resize: Boolean; overload;
    function Resize(_Options: TResizeOptionSet; _RowOffset: Integer = -1): Boolean; overload;
    function Resize(_Options: TResizeOptionSet;
      const _ConstantCols: array of Integer; _RowOffset: Integer = -1): Boolean; overload;
  end;

type
  IStringGridHelper = interface(ICustomGridHelper)
    procedure Clear;
    function AppendRow(_OverwriteEmptyFirst: Boolean = True): Integer; overload;
    function AppendRow(const _Columns: array of string; _OverwriteEmptyFirst: Boolean = True): Integer; overload;
    function DeleteRow(_Row: Integer): Boolean;
  end;

function GetControlHelper(_sg: TStringGrid): IStringGridHelper;

implementation

uses
  Math;

type
  TStringGridHelper = class(TInterfacedObject, IStringGridHelper)
  private
    FStringGrid: TStringGrid;
  private
    function AppendRow(_OverwriteEmptyFirst: Boolean = True): Integer; overload;
    function AppendRow(const _Columns: array of string; _OverwriteEmptyFirst: Boolean = True): Integer; overload;
    procedure Clear;
    function DeleteRow(_Row: Integer): Boolean;
  private
    function GetNonfixedRow: Integer;
    function Resize: Boolean; overload;
    function Resize(_Options: TResizeOptionSet; _RowOffset: Integer = -1): Boolean; overload;
    function Resize(_Options: TResizeOptionSet;
      const _ConstantCols: array of Integer; _RowOffset: Integer = -1): Boolean; overload;
  public
    constructor Create(_sg: TStringGrid);
  end;

function GetControlHelper(_sg: TStringGrid): IStringGridHelper;
begin
  Result := TStringGridHelper.Create(_sg);
end;

{ TStringGridHelper }

function TStringGridHelper.AppendRow(_OverwriteEmptyFirst: Boolean): Integer;
var
  row: Integer;
  col: Integer;
  IsEmptyRow: Boolean;
begin
  Result := FStringGrid.RowCount;
  if _OverwriteEmptyFirst and (Result = FStringGrid.FixedRows + 1) then begin
    IsEmptyRow := True;
    row := Result - 1;
    for col := 0 to FStringGrid.ColCount - 1 do begin
      if FStringGrid.Cells[col, row] <> '' then begin
        IsEmptyRow := False;
        Break;
      end;
    end;
    if IsEmptyRow then begin
      Result := row;
    end else begin
      FStringGrid.RowCount := Result + 1;
    end;
  end else begin
    FStringGrid.RowCount := Result + 1;
  end;
end;

function TStringGridHelper.AppendRow(const _Columns: array of string; _OverwriteEmptyFirst: Boolean): Integer;
var
  col: Integer;
  len: Integer;
begin
  Result := AppendRow(_OverwriteEmptyFirst);
  len := Length(_Columns);
  for col := 0 to Min(FStringGrid.ColCount - 1, len) do begin
    FStringGrid.Cells[col, Result] := _Columns[col];
  end;
end;

procedure TStringGridHelper.Clear;
begin
  TStringGrid_Clear(FStringGrid);
end;

constructor TStringGridHelper.Create(_sg: TStringGrid);
begin
  inherited Create;
  FStringGrid := _sg;
end;

function TStringGridHelper.DeleteRow(_Row: Integer): Boolean;
begin
  Result := TStringGrid_DeleteRow(FStringGrid, _Row);
end;

function TStringGridHelper.GetNonfixedRow: Integer;
begin
  Result := TGrid_GetNonfixedRow(FStringGrid);
end;

function TStringGridHelper.Resize: Boolean;
begin
  Result := TGrid_Resize(FStringGrid);
end;

function TStringGridHelper.Resize(_Options: TResizeOptionSet; _RowOffset: Integer): Boolean;
begin
  Result := TGrid_Resize(FStringGrid, _Options, _RowOffset);
end;

function TStringGridHelper.Resize(_Options: TResizeOptionSet; const _ConstantCols: array of Integer;
  _RowOffset: Integer): Boolean;
begin
  Result := TGrid_Resize(FStringGrid, _Options, _ConstantCols, _RowOffset);
end;

end.

