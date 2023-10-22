///<summary> statistical tools </summary>
unit u_dzStatistics;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  u_dzTranslator,
  u_dzRingBuffer,
  u_dzTypes;

type
  TMovingAverage = class
  private
    FCount: Integer;
    FSum: Extended;
    FMaxCount: Integer;
    FQueue: TdzRingQueue;
  public
    constructor Create(_MaxCount: Integer);
    destructor Destroy; override;
    procedure Add(_Value: Extended);
    function GetAverage: Extended;
    property Count: Integer read FCount;
  end;

implementation

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TMovingAverage }

constructor TMovingAverage.Create(_MaxCount: Integer);
begin
  inherited Create;
  FSum := 0;
  FCount := 0;
  FMaxCount := _MaxCount;
  FQueue := TdzRingQueue.Create(SizeOf(Extended), FMaxCount);
end;

destructor TMovingAverage.Destroy;
begin
  FreeAndNil(FQueue);
  inherited;
end;

procedure TMovingAverage.Add(_Value: Extended);
var
  ValueToRemove: Extended;
begin
  if FCount >= FMaxCount then begin
    FQueue.ExtractFront(ValueToRemove);
    FSum := FSum - ValueToRemove;
  end else
    Inc(FCount);
  FQueue.InsertEnd(_Value);
  FSum := FSum + _Value;
end;

function TMovingAverage.GetAverage: Extended;
begin
  if FCount > 0 then
    Result := FSum / FCount
  else
    raise EdzException.Create(_('Cannot calculate moving average on zero elements'));
end;

end.

