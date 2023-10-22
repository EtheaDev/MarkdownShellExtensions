unit u_dzTempBufferDouble;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  Windows,
  u_dzTypes;

type
  TdzTempBufferDouble = class
  private
    type
      TBuffer = record
        Values: TDoubleArray;
        Count: Integer;
      end;
      PBuffer = ^TBuffer;
  private
    FBuffer: PBuffer;
    FSpare: PBuffer;
    FCapacity: Integer;
    procedure AllocBuf(var _Buffer: PBuffer);
    procedure FreeBuf(var _Buffer: PBuffer);
  public
    constructor Create(_Capacity: Integer);
    destructor Destroy; override;
    function TryAdd(const _Value: Double): Boolean;
    function TryGet(var _Values: TDoubleArray): Boolean;
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

{ TdzTempBufferDouble }

constructor TdzTempBufferDouble.Create(_Capacity: Integer);
begin
  inherited Create;
  FCapacity := _Capacity;
  AllocBuf(FBuffer);
  AllocBuf(FSpare);
end;

destructor TdzTempBufferDouble.Destroy;
begin
  FCapacity := 0;
  FreeBuf(FSpare);
  FreeBuf(FBuffer);
  inherited;
end;

procedure TdzTempBufferDouble.AllocBuf(var _Buffer: PBuffer);
begin
  New(_Buffer);
  SetLength(_Buffer^.Values, FCapacity);
  _Buffer.Count := 0;
end;

procedure TdzTempBufferDouble.FreeBuf(var _Buffer: PBuffer);
begin
  if Assigned(_Buffer) then begin
    SetLength(_Buffer.Values, 0);
    Dispose(_Buffer);
    _Buffer := nil;
  end;
end;

function TdzTempBufferDouble.TryGet(var _Values: TDoubleArray): Boolean;
var
  Buf: PBuffer;
  cnt: Integer;
  i: Integer;
begin
  Result := True;
  FSpare.Count := 0;
  Buf := PBuffer(InterlockedExchange(Integer(FBuffer), Integer(FSpare)));
  cnt := Buf.Count;
  SetLength(_Values, cnt);
  for i := 0 to cnt - 1 do
    _Values[i] := Buf.Values[i];
  FSpare := Buf;
  FSpare.Count := 0;
end;

function TdzTempBufferDouble.TryAdd(const _Value: Double): Boolean;
var
  cnt: Integer;
  Buf: PBuffer;
begin
  Buf := FBuffer;
  cnt := Buf.Count;
  if cnt >= FCapacity then begin
    Result := False;
    Exit;
  end;
  Buf.Values[cnt] := _Value;
  Inc(cnt);
  Buf.Count := cnt;
  Result := (Buf = FBuffer);
end;

{$ENDIF DELPHI2007_UP}

end.
