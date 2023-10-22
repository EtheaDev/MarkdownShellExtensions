unit u_dzCsvWriter;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzTypes,
  u_dzCsvColumnList,
  u_dzNullableTypesUtils,
  u_dzNullableDate,
  u_dzNullableTime,
  u_dzNullableExtended,
  u_dzNullableInteger,
  u_dzConvertUtils;

type
  TdzCsvWriter = class
  public
  private
    FColumns: TdzCsvColumnList;
    FFormatSettings: TFormatSettings;
    FBoolToStr: TBoolToStr;
    function GetColumns(_Idx: Integer): ICsvColumnDefinition;
  public
    constructor Create; overload;
    constructor Create(const _FormatSettings: TFormatSettings); overload;
    destructor Destroy; override;
    ///<summary>
    /// Sets the default for the boolean strings, can only be called before adding any columns.
    /// @raises ECsvWriter when called if a column definition already exists. </sumary>
    procedure SetBoolStrings(const _BoolToStr: TBoolToStr);
    function AddColumnDefinition(const _Name: string; const _Type: TCsvColumnType): ICsvColumnDefinition;
    function ColumnCount: Integer;
    property Columns[_Idx: Integer]: ICsvColumnDefinition read GetColumns;
    procedure SetValue(const _Column: string; const _Value: string); overload;
{$IFDEF UNICODE}
    procedure SetValue(const _Column: string; const _Value: AnsiString); overload;
{$ENDIF}
    procedure SetValue(const _Column: string; const _Value: Int8); overload;
    procedure SetValue(const _Column: string; const _Value: Int16); overload;
    procedure SetValue(const _Column: string; const _Value: Integer); overload;
    procedure SetValue(const _Column: string; const _Value: Int64); overload;
    procedure SetValue(const _Column: string; const _Value: UInt8); overload;
    procedure SetValue(const _Column: string; const _Value: UInt16); overload;
    procedure SetValue(const _Column: string; const _Value: UInt32); overload;
    procedure SetValue(const _Column: string; const _Value: UInt64); overload;
    procedure SetValue(const _Column: string; _Value: TNullableDate); overload;
    procedure SetValue(const _Column: string; _Value: TNullableTime); overload;
    procedure SetValue(const _Column: string; const _Value: Extended); overload;
    procedure SetValue(const _Column: string; const _Value: Boolean); overload;
    function GetValue(const _Column: string): string; overload;
    function GetValue(_Idx: Integer): string; overload;
    procedure Clear;
    function Captions: string;
    function Line: string;
    function GetFormatSettings: TFormatSettings;
    procedure WriteCaptionsToStream(_st: TStream);
    procedure WriteLineToStream(_st: TStream);
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  u_dzClassUtils,
  u_dzStringUtils;

function _(const _s: string): string;
begin
  Result := dzGetText(_s);
end;

{ TdzCsvWriter }

constructor TdzCsvWriter.Create;
begin
  Create(GetUserDefaultLocaleSettings);
end;

constructor TdzCsvWriter.Create(const _FormatSettings: TFormatSettings);
begin
  inherited Create;
  FColumns := TdzCsvColumnList.Create;
  FFormatSettings := _FormatSettings;
  FBoolToStr := TBoolToStr.CreateYN;
end;

destructor TdzCsvWriter.Destroy;
begin
  FreeAndNil(FColumns);
  inherited;
end;

function TdzCsvWriter.Captions: string;
var
  i: Integer;
begin
  if FColumns.Count = 0 then
    raise ECsvWriter.Create(_('No columns have been defined'));
  Result := FColumns[0].Name;
  for i := 1 to FColumns.Count - 1 do begin
    Result := Result + FFormatSettings.ListSeparator + FColumns[i].Name;
  end;
end;

procedure TdzCsvWriter.Clear;
var
  i: Integer;
begin
  for i := 0 to ColumnCount - 1 do
    (Columns[i] as ICsvColumn).Clear;
end;

function TdzCsvWriter.ColumnCount: Integer;
begin
  Result := FColumns.Count;
end;

function TdzCsvWriter.GetColumns(_Idx: Integer): ICsvColumnDefinition;
begin
  Result := FColumns[_Idx];
end;

function TdzCsvWriter.GetFormatSettings: TFormatSettings;
begin
  Result := FFormatSettings;
end;

function TdzCsvWriter.GetValue(const _Column: string): string;
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  Result := col.GetValue;
end;

function TdzCsvWriter.GetValue(_Idx: Integer): string;
var
  col: ICsvColumn;
begin
  col := FColumns[_Idx] as ICsvColumn;
  Result := col.GetValue;
end;

function TdzCsvWriter.Line: string;
var
  i: Integer;
begin
  if FColumns.Count = 0 then
    raise ECsvWriter.Create(_('No columns have been defined'));
  Result := GetValue(0);
  for i := 1 to FColumns.Count - 1 do begin
    Result := Result + FFormatSettings.ListSeparator + GetValue(i);
  end;
end;

procedure TdzCsvWriter.SetValue(const _Column: string; const _Value: Int8);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;

procedure TdzCsvWriter.SetValue(const _Column: string; const _Value: Int16);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;

procedure TdzCsvWriter.SetValue(const _Column: string; const _Value: Integer);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;

procedure TdzCsvWriter.SetValue(const _Column: string; const _Value: Int64);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;

procedure TdzCsvWriter.SetValue(const _Column: string; const _Value: UInt8);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;

procedure TdzCsvWriter.SetValue(const _Column: string; const _Value: UInt16);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;

procedure TdzCsvWriter.SetValue(const _Column: string; const _Value: UInt32);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;

procedure TdzCsvWriter.SetValue(const _Column: string; const _Value: UInt64);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;

procedure TdzCsvWriter.SetValue(const _Column, _Value: string);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;

{$IFDEF UNICODE}
procedure TdzCsvWriter.SetValue(const _Column: string; const _Value: AnsiString);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;
{$ENDIF}

procedure TdzCsvWriter.SetValue(const _Column: string; _Value: TNullableDate);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;

procedure TdzCsvWriter.SetValue(const _Column: string; const _Value: Extended);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;

procedure TdzCsvWriter.SetValue(const _Column: string; _Value: TNullableTime);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;

procedure TdzCsvWriter.SetBoolStrings(const _BoolToStr: TBoolToStr);
begin
  if FColumns.Count > 0 then
    raise ECsvWriter.Create(_('Cannot set the bool strings when a column already exists.'));
  FBoolToStr := _BoolToStr;
end;

procedure TdzCsvWriter.SetValue(const _Column: string; const _Value: Boolean);
var
  col: ICsvColumn;
begin
  col := FColumns.GetColumn(_Column);
  col.SetValue(_Value);
end;

procedure TdzCsvWriter.WriteCaptionsToStream(_st: TStream);
begin
  TStream_WriteStringLn(_st, AnsiString(Captions));
end;

procedure TdzCsvWriter.WriteLineToStream(_st: TStream);
begin
  TStream_WriteStringLn(_st, AnsiString(Line));
end;

function TdzCsvWriter.AddColumnDefinition(const _Name: string; const _Type: TCsvColumnType): ICsvColumnDefinition;
var
  col: TdzCsvColumn;
begin
  if FColumns.HasColumn(_Name) then
    raise ECsvWriter.CreateFmt(_('Column %s already exists.'), [_Name]);
  col := TdzCsvColumn.Create(_Name, _Type, @FFormatSettings);
  Result := col;
  if _Type = cctBoolean then
    Result.SetBoolStrings(FBoolToStr);
  FColumns.Add(Result);
end;

{$ENDIF DELPHI2007_UP}
end.
