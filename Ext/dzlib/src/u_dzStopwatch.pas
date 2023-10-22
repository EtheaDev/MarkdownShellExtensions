unit u_dzStopwatch;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  Classes,
  u_dzNullableTimespan,
  u_dzTypes;

type
  TStopwatch = record
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
  private
{$ENDIF}
    FElapsedTicks: Int64;
    FIsRunning: Boolean;
    FStartTicks: Int64;
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
    function GetElapsedDateTimeTicks: Int64;
  public
    class function Create: TStopwatch; static;
    class function GetTimeStamp: Int64; static;
    procedure Reset;
    procedure Start; inline;
    class function StartNew: TStopwatch; static;
    procedure Stop; inline;
    function Elapsed: TNullableTimespan;
    function ElapsedMilliseconds: Int64;
    ///<summary>
    /// Elapsed Milliseconds as UInt32, cut off at MaxUInt32 </summary>
    function ElapsedMilliseconds32: UInt32;
    function ElapsedTicks: Int64;
    class function Frequency: Int64; static;
    class function IsHighResolution: Boolean; static;
    property IsRunning: Boolean read FIsRunning;
{$ENDIF}
  end;

function TStopWatch_Create: TStopwatch;
function TStopWatch_GetTimeStamp: Int64;
procedure TStopWatch_Reset(var _Stopwatch: TStopwatch);
procedure TStopWatch_Start(var _Stopwatch: TStopwatch);
function TStopWatch_StartNew: TStopwatch;
procedure TStopWatch_Stop(var _Stopwatch: TStopwatch);
function TStopWatch_ElapsedMilliseconds(const _Stopwatch: TStopwatch): Int64;
function TStopWatch_ElapsedMilliseconds32(const _Stopwatch: TStopwatch): UInt32;
function TStopWatch_ElapsedTicks(const _Stopwatch: TStopwatch): Int64;
function TStopWatch_Frequency: Int64;
function TStopWatch_IsHighResolution: Boolean;
function TStopWatch_IsRunning(const _Stopwatch: TStopwatch): Boolean;

implementation

uses
  u_dzConvertUtils;

const
  TicksPerMillisecond = 10000;
  TicksPerSecond = TicksPerMillisecond * 1000;

var
  gblFrequency: Int64;
  gblIsHighResolution: Boolean;
  gblTickFrequency: Double;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
{ TStopwatch }

class function TStopwatch.Create: TStopwatch;
begin
  Result.Reset;
end;

class function TStopwatch.Frequency: Int64;
begin
  Result := gblFrequency;
end;

function TStopwatch.Elapsed: TNullableTimespan;
begin
  Result.AssignMilliseconds(GetElapsedDateTimeTicks / TicksPerMillisecond);
end;

function TStopwatch.GetElapsedDateTimeTicks: Int64;
begin
  Result := ElapsedTicks;
  if gblIsHighResolution then
    Result := Trunc(Result * gblTickFrequency);
end;

function TStopwatch.ElapsedMilliseconds: Int64;
begin
  Result := GetElapsedDateTimeTicks div Int64(TicksPerMillisecond);
end;

function TStopwatch.ElapsedMilliseconds32: UInt32;
begin
  Result := ReduceToUInt32(ElapsedMilliseconds);
end;

function TStopwatch.ElapsedTicks: Int64;
begin
  Result := FElapsedTicks;
  if FIsRunning then
    Result := Result + GetTimeStamp - FStartTicks;
end;

class function TStopwatch.GetTimeStamp: Int64;
begin
  if gblIsHighResolution then
    QueryPerformanceCounter(Result)
  else
    Result := GetTickCount * Int64(TicksPerMillisecond);
end;

class function TStopwatch.IsHighResolution: Boolean;
begin
  Result := gblIsHighResolution;
end;

procedure TStopwatch.Reset;
begin
  FElapsedTicks := 0;
  FIsRunning := False;
  FStartTicks := 0;
end;

procedure TStopwatch.Start;
begin
  if not FIsRunning then begin
    FStartTicks := GetTimeStamp;
    FIsRunning := True;
  end;
end;

class function TStopwatch.StartNew: TStopwatch;
begin
  Result.Reset;
  Result.Start;
end;

procedure TStopwatch.Stop;
begin
  if FIsRunning then begin
    FElapsedTicks := FElapsedTicks + GetTimeStamp - FStartTicks;
    FIsRunning := False;
  end;
end;
{$ENDIF}

function TStopWatch_GetElapsedDateTimeTicks(const _Stopwatch: TStopwatch): Int64;
begin
  Result := TStopWatch_ElapsedTicks(_Stopwatch);
  if gblIsHighResolution then
    Result := Trunc(Result * gblTickFrequency);
end;

function TStopWatch_Create: TStopwatch;
begin
  Result.FElapsedTicks := 0;
  Result.FIsRunning := False;
  Result.FStartTicks := 0;
end;

function TStopWatch_GetTimeStamp: Int64;
begin
  if gblIsHighResolution then
    QueryPerformanceCounter(Result)
  else
    Result := GetTickCount * Int64(TicksPerMillisecond);
end;

procedure TStopWatch_Reset(var _Stopwatch: TStopwatch);
begin
  _Stopwatch.FElapsedTicks := 0;
  _Stopwatch.FIsRunning := False;
  _Stopwatch.FStartTicks := 0;
end;

procedure TStopWatch_Start(var _Stopwatch: TStopwatch);
begin
  if not _Stopwatch.FIsRunning then begin
    _Stopwatch.FStartTicks := TStopWatch_GetTimeStamp;
    _Stopwatch.FIsRunning := True;
  end;
end;

function TStopWatch_StartNew: TStopwatch;
begin
  Result := TStopWatch_Create;
  TStopWatch_Start(Result);
end;

procedure TStopWatch_Stop(var _Stopwatch: TStopwatch);
begin
  if _Stopwatch.FIsRunning then begin
    _Stopwatch.FElapsedTicks := _Stopwatch.FElapsedTicks + TStopWatch_GetTimeStamp - _Stopwatch.FStartTicks;
    _Stopwatch.FIsRunning := False;
  end;
end;

function TStopWatch_ElapsedMilliseconds(const _Stopwatch: TStopwatch): Int64;
begin
  Result := TStopWatch_GetElapsedDateTimeTicks(_Stopwatch) div Int64(TicksPerMillisecond);
end;

function TStopWatch_ElapsedMilliseconds32(const _Stopwatch: TStopwatch): UInt32;
begin
  Result := ReduceToUInt32(TStopWatch_ElapsedMilliseconds(_Stopwatch));
end;

function TStopWatch_ElapsedTicks(const _Stopwatch: TStopwatch): Int64;
begin
  Result := _Stopwatch.FElapsedTicks;
  if _Stopwatch.FIsRunning then
    Result := Result + TStopWatch_GetTimeStamp - _Stopwatch.FStartTicks;
end;

function TStopWatch_Frequency: Int64;
begin
  Result := gblFrequency;
end;

function TStopWatch_IsHighResolution: Boolean;
begin
  Result := gblIsHighResolution;
end;

function TStopWatch_IsRunning(const _Stopwatch: TStopwatch): Boolean;
begin
  Result := _Stopwatch.FIsRunning;
end;

procedure InitVariables;
begin
  if not QueryPerformanceFrequency(gblFrequency) then begin
    // no high performance timer available, use WinAPI GetTickCount
    gblIsHighResolution := False;
    gblFrequency := TicksPerSecond;
    gblTickFrequency := 1.0;
  end else begin
    gblIsHighResolution := True;
    gblTickFrequency := TicksPerSecond / gblFrequency;
  end;
end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
procedure Test;
var
  Stopwatch: TStopwatch;
begin
  Stopwatch.Reset;
  Stopwatch.Start;
  Sleep(1000);
  Stopwatch.Stop;
  Stopwatch.Elapsed.InMicroseconds;
end;
{$ENDIF}

initialization
  InitVariables;
//  Test;
end.
