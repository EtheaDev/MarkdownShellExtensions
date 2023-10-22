unit u_dzDynamicExtendedArray;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  SysUtils,
  Classes;

type
  ///<summary>
  /// Class that simulates a dynamic array of Extended with custom low and high index
  /// similar to the static declaration array[low..high] of extended but created dynamically </summary>
  TdzDynamicExtendedArray = class
  private
    FValues: array of Extended;
    FLow: Integer;
    FHigh: Integer;
    function GetValues(_Idx: Integer): Extended;
    procedure SetValues(_Idx: Integer; const _Value: Extended);
  public
    constructor Create(_Low, _High: Integer);
    function Length: Integer;
    function LowValue: Extended;
    function HighValue: Extended;
    function AsCsv: string;
    property Low: Integer read FLow;
    property High: Integer read FHigh;
    property Values[_Idx: Integer]: Extended read GetValues write SetValues; default;
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  RTLConsts,
  u_dzCsvWriter,
  u_dzMiscUtils,
  u_dzCsvColumnList;

{ TdzDynamicExtendedArray }

constructor TdzDynamicExtendedArray.Create(_Low, _High: Integer);
begin
  inherited Create;
  FLow := _Low;
  FHigh := _High;
  SetLength(FValues, FHigh - FLow + 1);
end;

function TdzDynamicExtendedArray.AsCsv: string;
var
  sl: TStringList;
  csv: TdzCsvWriter;
  i: Integer;
begin
  InitializeNil(sl, csv);
  try
    sl := TStringList.Create;
    csv := TdzCsvWriter.Create;
    csv.AddColumnDefinition('Idx', cctInteger);
    csv.AddColumnDefinition('Value', cctFloat).SetDecimals(8);
    sl.Add(csv.Captions);
    for i := Low to High do begin
      csv.SetValue('Idx', i);
      csv.SetValue('Value', Values[i]);
      sl.Add(csv.Line);
    end;
    Result := sl.Text;
  finally
    FreeAndNil(sl, csv);
  end;
end;

function TdzDynamicExtendedArray.Length: Integer;
begin
  Result := System.Length(FValues);
end;

function TdzDynamicExtendedArray.GetValues(_Idx: Integer): Extended;
begin
  if _Idx < FLow then
    raise EListError.CreateFmt(SListIndexError, [_Idx]);
  if _Idx > FHigh then
    raise EListError.CreateFmt(SListIndexError, [_Idx]);

  Result := FValues[_Idx - FLow];
end;

function TdzDynamicExtendedArray.LowValue: Extended;
begin
  Result := Values[FLow];
end;

function TdzDynamicExtendedArray.HighValue: Extended;
begin
  Result := Values[FHigh];
end;

procedure TdzDynamicExtendedArray.SetValues(_Idx: Integer; const _Value: Extended);
begin
  if _Idx < FLow then
    raise EListError.CreateFmt(SListIndexError, [_Idx]);
  if _Idx > FHigh then
    raise EListError.CreateFmt(SListIndexError, [_Idx]);

  FValues[_Idx - FLow] := _Value;
end;

{$IFDEF debug}

procedure AssertAsCsvLinked;
var
  arr: TdzDynamicExtendedArray;
begin
  arr := TdzDynamicExtendedArray.Create(0, 0);
  try
    arr.AsCsv;
  finally
    FreeAndNil(arr);
  end;
end;

initialization
  AssertAsCsvLinked;
{$ENDIF debug}
{$ENDIF DELPHI2007_UP}
end.
