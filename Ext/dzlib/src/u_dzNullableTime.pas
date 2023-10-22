unit u_dzNullableTime;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  SysUtils,
  u_dzTranslator,
  u_dzStringUtils,
  u_dzNullableTimespan,
  u_dzNullableExtended;

type
  TNullableTime = record
  private
    FIsValid: IInterface;
    FValue: TDateTime;
  public
    procedure Invalidate;
    function Value: TDateTime;
    function IsValid: Boolean;
    procedure CheckIsValid(const _ErrorMsg: string);
    function GetValue(out _Value: TDateTime): Boolean;
    procedure Encode(_Hour, _Minutes, _Seconds, _MSeconds: Word);
    procedure Decode(out _Hour, _Minutes, _Seconds, _MSeconds: Word);
    procedure AssignVariant(_v: Variant);
    function ToVariant: Variant;
    function TryAssignStr(const _s: string): Boolean;
    procedure AssignIso(const _s: string; const _InvalidStr: string = '');
    procedure AssignStr(const _s: string; const _InvalidStr: string = '');
    procedure AssignHours(_Hours: Extended);
    procedure AssignMinutes(_Minutes: Extended);
    procedure AssignSeconds(_Seconds: Extended);
    function ForDisplay(_IncludeSeconds: Boolean = True; _Include100th: Boolean = False): string;
    function Dump: string;
    function ToHHmmSS: string; overload;
    ///<summary>
    /// @param NullValue is the string to return, if the time is not valid </summary>
    function ToHHmmSS(const _NullValue: string): string; overload;
    ///<summary>
    /// @param Decimals is the number of decimals for the fractions of a second, defaults to 3
    /// @param DecimalSeparator is the decimal separator to use, defaults to ','
    ///                         if set to #0, the current decimal separator will be used.
    /// @returns ToHHmmSS + DecimalSeparator + the number of decimals given in the Decimals parameter </summary>
    function ToHHmmSSz(_Decimals: Integer = 3; _DecimalSeparator: Char = '.'): string;
    function ToHHmm: string; overload;
    function ToHHmm(_Separator: Char): string; overload;
    ///<summary>
    /// @param NullValue is the string to return, if the time is not valid </summary>
    function ToHHmm(_Separator: Char; const _NullValue: string): string; overload; deprecated; // use ToHHmmDef!
    function ToHHmmDef(const _NullValue: string; _Separator: Char = #0): string;
    function Hour: Word;
    function Minutes: Word;
    function Seconds: Word;
    function MSecs: Word;
    ///<summary>
    /// @returns an invalid TNullableExtended if the time is invalid </summary>
    function InHours: TNullableExtended;
    ///<summary>
    /// @returns an invalid TNullableExtended if the time is invalid </summary>
    function InMinutes: TNullableExtended;
    ///<summary>
    /// @returns an invalid TNullableExtended if the time is invalid </summary>
    function InSeconds: TNullableExtended;
    procedure AddSeconds(_Seconds: Extended);
    procedure SubtractSeconds(_Seconds: Extended);
    class operator Implicit(_Value: TDateTime): TNullableTime;
    class operator Implicit(_a: TNullableTime): TDateTime;
    ///<summary>
    /// Converts a string to an TNullableTime.
    /// Empty strings result in an invalid TNullableTime
    /// Strings, that cannot be converted to a time, raise an EConvertError exception. </summary>
    class operator Explicit(const _s: string): TNullableTime; // use AssignStr instead
    class operator Explicit(_a: TNullableTime): string; // use ToHHmmSS or ForDisplay instead
    class operator Subtract(_a, _b: TNullableTime): TNullableTimespan;
    class operator Subtract(_a: TNullableTime; _b: TNullableTimespan): TNullableTime;
    class operator GreaterThan(_a, _b: TNullableTime): Boolean;
    class operator LessThan(_a, _b: TNullableTime): Boolean;
    class operator GreaterThanOrEqual(_a, _b: TNullableTime): Boolean;
    class operator LessThanOrEqual(_a, _b: TNullableTime): Boolean;
    class operator Equal(_a, _b: TNullableTime): Boolean;
    class operator NotEqual(_a, _b: TNullableTime): Boolean;
    class operator Divide(_a: TNullableTime; _b: Extended): TNullableTime;
    // todo: Adding two times is actually an odd concept, one of them should be a time span.
    //       The same applies to Divide and Negative too.
    //       Maybe these should be deprecated.
    class operator Add(_a, _b: TNullableTime): TNullableTime;
    class operator Add(_a: TNullableTime; _b: TNullableTimespan): TNullableTime;
    class operator Negative(_a: TNullableTime): TNullableTime;
    class function FromVariant(_v: Variant): TNullableTime; static;
    class function Now: TNullableTime; static;
    class function Zero: TNullableTime; static;
    ///<summary>
    /// Returns an invalid TNullableDate </summary>
    class function Invalid: TNullableTime; static;
    class function FromHours(_Hours: Extended): TNullableTime; overload; static;
    ///<summary>
    /// Creates a TNullableTime by assigning an Hour
    /// @returns an invalid TNullableTime is Hours is invalid </summary>
    class function FromHours(_Hours: TNullableExtended): TNullableTime; overload; static;
    class function FromSeconds(_Seconds: Extended): TNullableTime; static;
    class function FromDateTime(_dt: TDateTime): TNullableTime; static;
    class function EncodeFrom(_Hour, _Minutes, _Seconds, _MSeconds: Word): TNullableTime; static;
  end;

type
  TdzNullableTime = TNullableTime deprecated;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  SysConst,
  DateUtils,
  Math,
  Variants,
  u_dzDateUtils,
  u_dzConvertUtils,
  u_dzNullableTypesUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TNullableTime }

class operator TNullableTime.Explicit(const _s: string): TNullableTime;
begin
  Result.FIsValid := nil;
  if _s = '' then
    Exit;

  if not TryIso2Time(_s, Result.FValue)
    and not TryStrToTime(_s, Result.FValue) then
    raise EConvertError.CreateFmt(_('Cannot convert "%s" to %s.'), [_s, 'NullableTime']);

  Result.FIsValid := GetNullableTypesFlagInterface;
end;

class operator TNullableTime.Add(_a, _b: TNullableTime): TNullableTime;
begin
  Result.FValue := _a.Value + _b.Value;
  Result.FIsValid := GetNullableTypesFlagInterface;
end;

class operator TNullableTime.Add(_a: TNullableTime; _b: TNullableTimespan): TNullableTime;
begin
  Result.FValue := _a.Value + _b.InDays;
  Result.FIsValid := GetNullableTypesFlagInterface;
end;

procedure TNullableTime.AddSeconds(_Seconds: Extended);
begin
  FValue := Value + _Seconds / SecondsPerDay;
end;

function TNullableTime.Hour: Word;
var
  m: Word;
  s: Word;
  ms: Word;
begin
  DecodeTime(Value, Result, m, s, ms);
end;

function TNullableTime.Minutes: Word;
var
  h: Word;
  s: Word;
  ms: Word;
begin
  DecodeTime(Value, h, Result, s, ms);
end;

function TNullableTime.MSecs: Word;
var
  h: Word;
  m: Word;
  s: Word;
begin
  DecodeTime(Value, h, m, s, Result);
end;

function TNullableTime.Seconds: Word;
var
  h: Word;
  m: Word;
  ms: Word;
begin
  DecodeTime(Value, h, m, Result, ms);
end;

class operator TNullableTime.Subtract(_a: TNullableTime; _b: TNullableTimespan): TNullableTime;
var
  Secs: Extended;
begin
  Secs := _a.InSeconds - _b.InSeconds;
  if Secs < 0 then
    raise Exception.CreateFmt(_('Substracting %s from %s results in a negative time'),
      [_b.ForDisplay, _a.ForDisplay(True, True)]);
  Result.AssignSeconds(Secs);
end;

procedure TNullableTime.SubtractSeconds(_Seconds: Extended);
begin
  AddSeconds(-_Seconds);
end;

function TNullableTime.ToHHmm: string;
begin
  Result := Time2Iso(Value, False);
end;

function TNullableTime.ToHHmm(_Separator: Char): string;
begin
  Result := Time2Iso(Value, False, False, _Separator)
end;

function TNullableTime.ToHHmm(_Separator: Char; const _NullValue: string): string;
begin
  Result := ToHHmmDef(_NullValue, _Separator);
end;

function TNullableTime.ToHHmmDef(const _NullValue: string; _Separator: Char): string;
begin
  if IsValid then
    Result := Time2Iso(Value, False, False, _Separator)
  else
    Result := _NullValue;
end;

function TNullableTime.ToHHmmSS: string;
begin
  Result := Time2Iso(Value, True);
end;

function TNullableTime.ToHHmmSS(const _NullValue: string): string;
begin
  if IsValid then
    Result := Time2Iso(Value, True)
  else
    Result := _NullValue;
end;

function TNullableTime.ToHHmmSSz(_Decimals: Integer = 3; _DecimalSeparator: Char = '.'): string;
var
  Secs: Extended;
  Factor: Extended;
  IntFactor: Int64;
  IntSecs: Int64;
begin
  Result := ToHHmmSS;
  if _Decimals > 0 then begin
    Secs := Self.InSeconds;
    Secs := Frac(Secs);
    Factor := Power(10, _Decimals);
    Secs := Secs * Factor;
    Secs := Round(Secs);
    IntFactor := Round(Factor);
    IntSecs := Round(Secs);
    if IntSecs >= IntFactor then
      IntSecs := 0;
    if _DecimalSeparator = #0 then
{$IF declared(FormatSettings)}
      _DecimalSeparator := FormatSettings.DecimalSeparator;
{$ELSE}
      _DecimalSeparator := {FormatSettings.}DecimalSeparator;
{$IFEND}
    Result := Result + _DecimalSeparator + Long2DecN(IntSecs, _Decimals);
  end;
end;

function TNullableTime.ToVariant: Variant;
begin
  if IsValid then
    Result := Time2Iso(FValue)
  else
    Result := Variants.Null;
end;

procedure TNullableTime.AssignHours(_Hours: Extended);
begin
  FValue := _Hours / HoursPerDay;
  FIsValid := GetNullableTypesFlagInterface
end;

procedure TNullableTime.AssignMinutes(_Minutes: Extended);
begin
  FValue := _Minutes / MinutesPerDay;
  FIsValid := GetNullableTypesFlagInterface
end;

procedure TNullableTime.AssignSeconds(_Seconds: Extended);
begin
  FValue := _Seconds / SecondsPerDay;
  FIsValid := GetNullableTypesFlagInterface
end;

procedure TNullableTime.AssignIso(const _s, _InvalidStr: string);
begin
  if _s = _InvalidStr then
    FIsValid := nil
  else if TryIso2Time(_s, FValue) then
    FIsValid := GetNullableTypesFlagInterface
  else
    raise EConvertError.CreateFmt(SInvalidTime, [_s]);
end;

function TNullableTime.TryAssignStr(const _s: string): Boolean;
begin
  Result := TryIso2Time(_s, FValue) or TryStrToTime(_s, FValue);
  if Result then
    FIsValid := GetNullableTypesFlagInterface
end;

procedure TNullableTime.AssignStr(const _s, _InvalidStr: string);
begin
  if _s = _InvalidStr then
    FIsValid := nil
  else if not TryAssignStr(_s) then
    raise EConvertError.CreateFmt(SInvalidTime, [_s]);
end;

procedure TNullableTime.AssignVariant(_v: Variant);
begin
  if VarIsStr(_v) and TryIso2Time(_v, FValue) then
    FIsValid := GetNullableTypesFlagInterface
  else
    FIsValid := nil;
end;

function TNullableTime.Dump: string;
begin
  if IsValid then
    Result := Time2Iso(FValue, True, True)
  else
    Result := '<invalid>';
end;

function TNullableTime.ForDisplay(_IncludeSeconds: Boolean = True; _Include100th: Boolean = False): string;
var
  h, m, s, cs: Word;
begin
  if _Include100th then
    _IncludeSeconds := True;

  h := Trunc(Value * SecondsPerDay / SecondsPerHour); // allow for 24:00 -> not mod 24;
  m := Trunc(Value * SecondsPerDay / SecondsPerMinute) mod MinutesPerHour;
  s := Trunc(Value * SecondsPerDay) mod SecondsPerMinute;
  cs := Trunc(Value * SecondsPerDay * 100) mod 100;

  if not _IncludeSeconds then begin
    if s > 29 then begin
      Inc(m);
    end;
  end else if not _Include100th then begin
    if cs > 49 then
      Inc(s);
  end;
  if s > 59 then begin
    s := s - 60;
    Inc(m);
  end;
  if m > 59 then begin
    m := m - 60;
    Inc(h);
  end;

  Result := Format('%.2d:%.2d', [h, m]);
  if _IncludeSeconds then begin
    Result := Result + Format(':%.2d', [s]);
    if _Include100th then
      Result := Result + Format('%.2f', [cs / 100]);
  end;
end;

procedure TNullableTime.Decode(out _Hour, _Minutes, _Seconds, _MSeconds: Word);
begin
  DecodeTime(Value, _Hour, _Minutes, _Seconds, _MSeconds);
end;

class operator TNullableTime.Divide(_a: TNullableTime; _b: Extended): TNullableTime;
begin
  Result := _a.Value / _b;
end;

procedure TNullableTime.Encode(_Hour, _Minutes, _Seconds, _MSeconds: Word);

  function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean;
  // Copied from SysUtils.TryEncodeTime of Delphi 2007 with the following change:
  //   if (Hour < HoursPerDay) ....
  // changed to
  //   if (Hour <= HoursPerDay) ...
  // to allow for 24:00
  begin
    Result := False;
    if (Hour <= HoursPerDay) and (Min < MinsPerHour) and (Sec < SecsPerMin) and (MSec < MSecsPerSec) then begin
      Time := (Hour * (MinsPerHour * SecsPerMin * MSecsPerSec) +
        Min * (SecsPerMin * MSecsPerSec) +
        Sec * MSecsPerSec +
        MSec) / MSecsPerDay;
      Result := True;
    end;
  end;

begin
  if TryEncodeTime(_Hour, _Minutes, _Seconds, _MSeconds, FValue) then
    FIsValid := GetNullableTypesFlagInterface
  else
    FIsValid := nil;
end;

class function TNullableTime.EncodeFrom(_Hour, _Minutes, _Seconds, _MSeconds: Word): TNullableTime;
begin
  Result.Encode(_Hour, _Minutes, _Seconds, _MSeconds);
end;

class operator TNullableTime.Explicit(_a: TNullableTime): string;
begin
  if _a.IsValid then
    Result := Time2Iso(_a.FValue)
  else
    Result := '';
end;

class function TNullableTime.FromHours(_Hours: Extended): TNullableTime;
begin
  Result.AssignHours(_Hours);
end;

class function TNullableTime.FromDateTime(_dt: TDateTime): TNullableTime;
begin
  Result := _dt;
end;

class function TNullableTime.FromHours(_Hours: TNullableExtended): TNullableTime;
begin
  if _Hours.IsValid then
    Result.AssignHours(_Hours.Value);
end;

class function TNullableTime.FromSeconds(_Seconds: Extended): TNullableTime;
begin
  Result.AssignSeconds(_Seconds);
end;

class function TNullableTime.FromVariant(_v: Variant): TNullableTime;
begin
  Result.AssignVariant(_v);
end;

class operator TNullableTime.Implicit(_Value: TDateTime): TNullableTime;
begin
  Result.FValue := TimeOf(_Value);
  Result.FIsValid := GetNullableTypesFlagInterface;
end;

class operator TNullableTime.Implicit(_a: TNullableTime): TDateTime;
begin
  Result := _a.Value;
end;

class function TNullableTime.Invalid: TNullableTime;
begin
  Result.Invalidate;
end;

function TNullableTime.GetValue(out _Value: TDateTime): Boolean;
begin
  Result := IsValid;
  if Result then
    _Value := FValue;
end;

class operator TNullableTime.GreaterThan(_a, _b: TNullableTime): Boolean;
begin
  Result := (_a.Value > _b.Value);
end;

class operator TNullableTime.GreaterThanOrEqual(_a, _b: TNullableTime): Boolean;
begin
  Result := (_a.Value >= _b.Value);
end;

function TNullableTime.InHours: TNullableExtended;
begin
  if IsValid then
    Result := FValue * HoursPerDay;
end;

function TNullableTime.InMinutes: TNullableExtended;
begin
  if IsValid then
    Result := FValue * MinutesPerDay;
end;

function TNullableTime.InSeconds: TNullableExtended;
begin
  if IsValid then
    Result := FValue * SecondsPerDay;
end;

procedure TNullableTime.Invalidate;
begin
  FIsValid := nil;
end;

function TNullableTime.IsValid: Boolean;
begin
  Result := FIsValid <> nil;
end;

procedure TNullableTime.CheckIsValid(const _ErrorMsg: string);
begin
  if not IsValid then
    raise EInvalidValue.Create(_ErrorMsg);
end;

class operator TNullableTime.LessThan(_a, _b: TNullableTime): Boolean;
begin
  Result := (_a.Value < _b.Value);
end;

class operator TNullableTime.LessThanOrEqual(_a, _b: TNullableTime): Boolean;
begin
  Result := (_a.Value <= _b.Value);
end;

class operator TNullableTime.Subtract(_a, _b: TNullableTime): TNullableTimespan;
begin
  if _a.IsValid and _b.IsValid then
    Result.AssignDays(_a.Value - _b.Value)
  else
    Result.Invalidate;
end;

class operator TNullableTime.Equal(_a, _b: TNullableTime): Boolean;
begin
  Result := (_a.Value = _b.Value);
end;

class operator TNullableTime.Negative(_a: TNullableTime): TNullableTime;
begin
  Result.FValue := _a.Value;
  Result.FIsValid := GetNullableTypesFlagInterface;
end;

class operator TNullableTime.NotEqual(_a, _b: TNullableTime): Boolean;
begin
  Result := (_a.Value <> _b.Value);
end;

class function TNullableTime.Now: TNullableTime;
begin
  Result := SysUtils.Now;
end;

function TNullableTime.Value: TDateTime;
begin
  if not IsValid then
    raise EInvalidValue.Create(_('NullableTime value is invalid.'));
  Result := FValue;
end;

class function TNullableTime.Zero: TNullableTime;
begin
  Result := 0;
end;

{$IFDEF debug}

procedure AssertDumpAvailable;
begin
  TNullableTime.Zero.Dump;
end;

initialization
  AssertDumpAvailable;
{$ENDIF}
{$ENDIF DELPHI2007_UP}
end.
