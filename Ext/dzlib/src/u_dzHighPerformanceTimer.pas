///<summary>
/// Implements a timer based on the Windows High-Resolution Performance Counter, using the
/// API functions QueryPerformanceFrequency and QueryPerformanceCounter. </summary>
unit u_dzHighPerformanceTimer;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils;

type
  ITimerInterval = interface ['{83136104-6D4A-4F58-9996-3161B2D53A20}']
    ///<summary>
    /// Returns the value of the High-Resolution Performance Counter at the time when the interval
    /// was started </summary>
    function GetStartValue: Int64;
    ///<summary>
    /// Returns the value of the High-Resolution Performance Counter the time when the interval
    /// was started, converted to seconds. </summary>
    function GetStartSeconds: Extended;
    ///<summary>
    /// Returns the difference between GetCurrentValue and GetStartValue. </summary>
    function GetIntervalValue: Int64;
    ///<summary>
    /// Returns GetIntervalValue converted to seconds. </summary>
    function GetIntervalSeconds: Extended;
    function GetIntervalSecondsStr(const _Format: string = '%.5f'): string;
    ///<summary>
    /// Returns GetIntervalValue converted to milliseconds. </summary>
    function GetIntervalMilliseconds: Extended;
    function GetIntervalMillisecondsStr(const _Format: string = '%.3f'): string;
    ///<summary>
    /// While running, this returns the current value of the High-Resolution Performance Counter.
    /// When stopped, it returns the value of the High-Resolution Performance Counter at the time
    /// it was stopped.  </summary>
    function GetCurrentValue: Int64;
    ///<summary>
    /// Returns GetCurrentValue, converted to seconds. </summary>
    function GetCurrentSeconds: Extended;
    ///<summary>
    /// Returns true while the interval Counter has not been stopped. </summary>
    function IsRunning: Boolean;
    ///<summary>
    /// Stops the Counter interval. After this call, GetIntervalXxx and GetCurrentXxx no longer
    /// change. </summary>
    procedure Stop;
  end;

type
  THighPerformanceTimer = class
  private
    FFrequency: Int64;
  public
    ///<summary>
    /// Creates a THighPerformanceTimer, queries the frequency of the High-Resolution Performance
    /// Counter and stores it internally </summary>
    constructor Create;
    ///<summary>
    /// Starts an interval and returns an ITimerInterval interface </summary>
    function StartInterval: ITimerInterval;
    ///<summary>
    /// Returns the current value of the High-Resolution Performance Counter </summary>
    function GetValue: Int64;
    ///<summary>
    /// Returns the frequency of the High-Resolution Performance Counter </summary>
    function GetFrequency: Int64;
    ///<summary>
    /// Returns the value of the High-Resolution Performance Counter converted to seconds </summary>
    function GetSeconds: Extended;
    ///<summary>
    /// Converts a High-Resolution Performance Counter value to seconds </summary>
    function ValueToSeconds(_HPTValue: Int64): Extended;
    ///<summary>
    /// Converts the High-Resolution Performance Counter value to a string, optionally adds the
    /// value in seconds. This is just a convenience function for logging. </summary>
    function ValueToString(_HPTValue: Int64; _IncludeSeconds: Boolean = True): string;
  end;

var
  // a global THighPerformanceTimer instance, created at startup
  gblHighPerformanceTimer: THighPerformanceTimer = nil;

implementation

uses
  u_dzTranslator;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
begin
  Result := dzlibGetText(_s);
end;

type
  ///<summary>
  /// Implements the ITimerInterval interface. Returned by THighPerformanceTimer.StartInterval. </summary>
  TTimerInterval = class(TInterfacedObject, ITimerInterval)
  private
    FFrequency: Int64;
    FStartHPFCounter: Int64;
    FStopHPFCounter: Int64;
  private // implementation for ITimerInterval
    function GetStartValue: Int64;
    function GetStartSeconds: Extended;
    function GetIntervalValue: Int64;
    function GetIntervalSeconds: Extended;
    function GetIntervalSecondsStr(const _Format: string = '%.5f'): string;
    function GetIntervalMilliseconds: Extended;
    function GetIntervalMillisecondsStr(const _Format: string = '%.3f'): string;
    function GetCurrentValue: Int64;
    function GetCurrentSeconds: Extended;
    function IsRunning: Boolean;
    procedure Stop;
  public
    constructor Create(_Frequency: Int64; _StartHPFCounter: Int64);
  end;

{ THighPerformanceTimer }

constructor THighPerformanceTimer.Create;
begin
  inherited Create;
  if False = QueryPerformanceFrequency(FFrequency) then //FI:C109
    raise Exception.Create(_('The hardware does not support a high-resolution performance counter.'));
end;

function THighPerformanceTimer.GetFrequency: Int64;
begin
  Result := FFrequency;
end;

function THighPerformanceTimer.GetValue: Int64;
begin
  Win32Check(QueryPerformanceCounter(Result));
end;

function THighPerformanceTimer.GetSeconds: Extended;
begin
  Result := GetValue / FFrequency;
end;

function THighPerformanceTimer.ValueToSeconds(_HPTValue: Int64): Extended;
begin
  Result := _HPTValue / FFrequency;
end;

function THighPerformanceTimer.ValueToString(_HPTValue: Int64; _IncludeSeconds: Boolean): string;
begin
  Result := IntToStr(_HPTValue);
  if _IncludeSeconds then
    Result := Result + ' (' + Format('%.6f', [ValueToSeconds(_HPTValue)]) + ' [sec])';
end;

function THighPerformanceTimer.StartInterval: ITimerInterval;
var
  StartTime: Int64;
begin
  Win32Check(QueryPerformanceCounter(StartTime));
  Result := TTimerInterval.Create(FFrequency, StartTime);
end;

{ TTimerInterval }

constructor TTimerInterval.Create(_Frequency: Int64; _StartHPFCounter: Int64);
begin
  inherited Create;
  FFrequency := _Frequency;
  FStartHPFCounter := _StartHPFCounter;
end;

function TTimerInterval.GetCurrentValue: Int64;
begin
  if FStopHPFCounter <> 0 then
    Result := FStopHPFCounter
  else
    Win32Check(QueryPerformanceCounter(Result));
end;

function TTimerInterval.GetCurrentSeconds: Extended;
begin
  Result := GetCurrentValue / FFrequency;
end;

function TTimerInterval.GetIntervalValue: Int64;
begin
  Result := GetCurrentValue;
  Dec(Result, FStartHPFCounter);
end;

function TTimerInterval.GetIntervalMilliseconds: Extended;
begin
  Result := GetIntervalSeconds * 1000;
end;

function TTimerInterval.GetIntervalMillisecondsStr(const _Format: string = '%.3f'): string;
begin
  Result := Format(_Format, [GetIntervalMilliseconds]);
end;

function TTimerInterval.GetIntervalSeconds: Extended;
begin
  Result := GetIntervalValue / FFrequency;
end;

function TTimerInterval.GetIntervalSecondsStr(const _Format: string = '%.5f'): string;
begin
  Result := Format(_Format, [GetIntervalSeconds]);
end;

function TTimerInterval.GetStartValue: Int64;
begin
  Result := FStartHPFCounter;
end;

function TTimerInterval.GetStartSeconds: Extended;
begin
  Result := FStartHPFCounter / FFrequency;
end;

function TTimerInterval.IsRunning: Boolean;
begin
  Result := FStopHPFCounter = 0;
end;

procedure TTimerInterval.Stop;
begin
  if FStopHPFCounter = 0 then
    Win32Check(QueryPerformanceCounter(FStopHPFCounter));
end;

initialization
  gblHighPerformanceTimer := THighPerformanceTimer.Create;
finalization
  FreeAndNil(gblHighPerformanceTimer);
end.
