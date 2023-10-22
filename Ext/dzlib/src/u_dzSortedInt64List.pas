unit u_dzSortedInt64List;

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
  u_dzQuicksort,
  u_dzTypes;

type
  EdzSortedInt64List = class(EdzException)
  end;
  EdzSortedInt64ListDuplicates = class(EdzSortedInt64List)
  end;
  EdzSortedInt64ListNotSorted = class(EdzSortedInt64List)
  end;

type
  ///<summary>
  /// Sorted list for storing Int64 items </summary>
  TSortedInt64List = class
  private
    type
      TInt64Arr = array of Int64;
  private
    FData: TInt64Arr;
    FCount: Integer;
    FAllowDuplicates: Boolean;
    function GetCapacity: Integer;
    procedure SetCapacity(const _NewCapacity: Integer);
    procedure Grow;
    function GetItems(_Idx: Integer): Int64;
    function CompareToIdx(const _Key; _Idx: Integer): Integer;
    procedure AtInsert(_Idx: Integer; _Value: Int64);
    function CalcNewCapacity: Integer;
  public
    constructor Create; overload;
    constructor Create(_InitialCapacity: Integer); overload;
    constructor Create(_AllowDuplicates: Boolean; _InitialCapacity: Integer); overload;
    constructor Create(_AllowDuplicates: Boolean); overload;
    function Add(_Value: Int64): Integer;
    ///<summary>
    /// In contrast to Add this assumes that the new value is larger than all the existing
    /// values and simply appends it to the end of the list. It's the responsibility of
    /// the caller to make sure this is the case. </summary>
    function Append(_Value: Int64): Integer;
    ///<summary>
    /// Checks whether all entries are sorted in ascending order. (in case you used Append and
    /// want to be sure you did not make a mistake)
    /// @raises EdzSortedInt64List if the items are not sorted. </summary>
    procedure CheckSorted;
    property Count: Integer read FCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Items[_Idx: Integer]: Int64 read GetItems; default;
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  Math,
  RTLConsts;

{ TSortedInt64List }

function TSortedInt64List.Add(_Value: Int64): Integer;
begin
  if BinarySearch(0, FCount - 1, Result, _Value, CompareToIdx, FAllowDuplicates) then begin
    if not FAllowDuplicates then
      raise EdzSortedInt64ListDuplicates.CreateFmt(_('List does not allow duplicates ($0%x)'), [_Value]);
  end;
  AtInsert(Result, _Value);
end;

function TSortedInt64List.Append(_Value: Int64): Integer;
begin
  if FCount >= Capacity then
    Grow;
  Result := FCount;
  Inc(FCount);
  FData[Result] := _Value;
end;

procedure TSortedInt64List.AtInsert(_Idx: Integer; _Value: Int64);
var
  NewData: TInt64Arr;
begin
  if FCount >= Capacity then begin
    SetLength(NewData, CalcNewCapacity);
    Move(FData[0], NewData[0], SizeOf(_Value) * _Idx);
    NewData[_Idx] := _Value;
    Move(FData[_Idx], NewData[_Idx + 1], SizeOf(_Value) * (FCount - _Idx));
    FData := NewData;
  end else begin
    Move(FData[_Idx], FData[_Idx + 1], SizeOf(_Value) * (FCount - _Idx));
    FData[_Idx] := _Value;
  end;
  Inc(FCount);
end;

procedure TSortedInt64List.CheckSorted;
var
  i: Integer;
  Res: Integer;
begin
  for i := 1 to FCount - 1 do begin
    Res := CompareValue(FData[i - 1], FData[i]);
    if Res = 1 then
      raise EdzSortedInt64ListNotSorted.CreateFmt(_('List is not sorted at index %d'), [i]);
    if (Res = 0) and not FAllowDuplicates then
      raise EdzSortedInt64ListDuplicates.CreateFmt(_('List contains duplicates at index %d but duplicates are not allowed'), [i]);
  end;
end;

function TSortedInt64List.CompareToIdx(const _Key; _Idx: Integer): Integer;
var
  Key: Int64 absolute _Key;
begin
  Result := CompareValue(Key, FData[_Idx]);
end;

constructor TSortedInt64List.Create;
begin
  Create(False, 100);
end;

constructor TSortedInt64List.Create(_AllowDuplicates: Boolean; _InitialCapacity: Integer);
begin
  inherited Create;
  FAllowDuplicates := _AllowDuplicates;
  FCount := 0;
  SetLength(FData, _InitialCapacity);
end;

constructor TSortedInt64List.Create(_AllowDuplicates: Boolean);
begin
  Create(_AllowDuplicates, 100);
end;

constructor TSortedInt64List.Create(_InitialCapacity: Integer);
begin
  Create(False, _InitialCapacity);
end;

function TSortedInt64List.GetCapacity: Integer;
begin
  Result := Length(FData);
end;

function TSortedInt64List.GetItems(_Idx: Integer): Int64;
begin
  Result := FData[_Idx];
end;

function TSortedInt64List.CalcNewCapacity: Integer;
begin
  Result := Capacity;
  Result := Result + Result div 10;
  if Result = 0 then
    Result := 10;
end;

procedure TSortedInt64List.Grow;
begin
  Capacity := CalcNewCapacity;
end;

procedure TSortedInt64List.SetCapacity(const _NewCapacity: Integer);
begin
  if _NewCapacity < Count then
    raise EdzSortedInt64List.CreateFmt(_('List capacity out of bounds (%d)'), [_NewCapacity]);
  if _NewCapacity <> Capacity then
    SetLength(FData, _NewCapacity);
end;

{$ENDIF DELPHI2007_UP}

end.
