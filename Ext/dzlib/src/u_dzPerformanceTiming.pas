unit u_dzPerformanceTiming;

interface

uses
  SysUtils,
  Classes,
  u_dzStopwatch;

///<summary>
/// for timing that takes the number of calls into account </summary>
type
  TPerformanceTiming = record
    FCallCount: Integer;
    FStopwatch: TStopwatch;
    procedure Reset;
    function GetAverageMS: Integer;
    function GetTotalMS: Int64;
    function Start: Boolean; inline;
    function Stop: Boolean; inline;
  end;

implementation

{ TPerformanceTiming }

function TPerformanceTiming.GetAverageMS: Integer;
begin
  if FCallCount > 0 then
    Result := FStopwatch.ElapsedMilliseconds div FCallCount
  else
    Result := 0;
end;

function TPerformanceTiming.GetTotalMS: Int64;
begin
  Result := FStopwatch.ElapsedMilliseconds;
end;

procedure TPerformanceTiming.Reset;
begin
  FCallCount := 0;
  FStopwatch.Reset;
end;

function TPerformanceTiming.Start: Boolean;
begin
  Inc(FCallCount);
  FStopwatch.Start;
  Result := True;
end;

function TPerformanceTiming.Stop: Boolean;
begin
  FStopwatch.Stop;
  Result := True;
end;

end.
