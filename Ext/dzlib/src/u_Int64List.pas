unit u_Int64List deprecated; // use u_dzInt64List

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes,
  u_dzTranslator;

type
  ///<summary>
  /// A List for storing Int64 values
  /// Note: If you use Add to add a huge amount of items to the list, you will
  ///       likely get an EOutOfMemory exception before actually running out of memory
  ///       because of memory framentation. If you know how many items you want to
  ///       store, set Capacity to that number. Capacity can be set to a maximum of
  ///       Classes.MaxListSize which means the list takes up around 1 GB of memory. </summary>
  TInt64List = class
  private
{$IFDEF MAXLISTSIZE_IS_DEPRECATED}
    // In Delphi XE2+ MaxListSize is deprecated (probably due to the 64 bit compiler).
    // We don't care for now. 1 gigabyte for this list is still huge.
    const
      MaxListSize = MaxInt div 16;
{$ENDIF}
  private
    FData: array of Int64;
    FCapacity: integer;
    FCount: integer;
    procedure Grow;
    procedure SetCapacity(_NewCapacity: integer);
    function GetItems(_Idx: integer): Int64;
    procedure SetItems(_Idx: integer; const _Value: Int64);
  public
    function Add(_Value: Int64): integer;
    property Items[_Idx: integer]: Int64 read GetItems write SetItems;
    property Capacity: integer read FCapacity write SetCapacity;
    property Count: integer read FCount;
  end;

implementation

{ TInt64List }

function TInt64List.Add(_Value: Int64): integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FData[Result] := _Value;
  Inc(FCount);
end;

procedure TInt64List.Grow;
var
  Delta: integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TInt64List.SetCapacity(_NewCapacity: integer);
begin
  if _NewCapacity <> FCapacity then begin
    if (_NewCapacity < FCount) then
      raise Exception.CreateFmt(_('Cannot set list capacity (%d) to less than current item count (%d).'),
        [_NewCapacity, FCount]);
    if (_NewCapacity > MaxListSize) then
      raise Exception.CreateFmt(_('Cannot set list capacity (%d) higher than MaxListSize (%d).'),
        [_NewCapacity, MaxListSize]);

    SetLength(FData, _NewCapacity);
    FCapacity := _NewCapacity;
  end;
end;

function TInt64List.GetItems(_Idx: integer): Int64;
begin
  if (_Idx < 0) or (_Idx >= FCount) then
    raise Exception.CreateFmt(_('List index out of bounds (%d) (Count=%d)'), [_Idx, FCount]);
  Result := FData[_Idx];
end;

procedure TInt64List.SetItems(_Idx: integer; const _Value: Int64);
begin
  if (_Idx < 0) or (_Idx >= FCount) then
    raise Exception.CreateFmt(_('List index out of bounds (%d) (Count=%d)'), [_Idx, FCount]);
  FData[_Idx] := _Value;
end;

end.

