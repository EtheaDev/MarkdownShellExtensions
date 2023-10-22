unit u_dzNullableTimespan;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF SUPPORTS_ENHANCED_RECORDS}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  SysUtils,
  u_dzTranslator,
  u_dzNullableExtended;

type
  TNullableTimespan = record
  private
    FIsValid: IInterface;
    ///<summary>
    /// Note: This used to be a double, encoded like in TDateTime but since that turned out
    ///       to have rather severe rounding errors I now use an Int64 each for days and
    ///       Microseconds. The actual timespan is always the sum of both.
    ///       They should always have the same sign. </summary>
    FFullDays: Int64;
    FMicroSeconds: Int64;
    procedure CheckIsValid;
    procedure SetDaysAndMicroseconds(_FullDays: Int64; _MicroSeconds: Int64); inline;
  public
    procedure Invalidate;
    function IsValid: Boolean;
    function InDays: Double;
    function InFullDays: Int64;
    function InHours: Double;
    function InFullHours: Int64;
    function InMinutes: Double;
    function InFullMinutes: Int64;
    function InSeconds: Double;
    function InFullSeconds: Int64;
    function InMilliseconds: Double;
    function InFullMilliseconds: Int64;
    function InMicroseconds: Double;
    function InFullMicroseconds: Int64;
    function TryGetDays(out _Days: Double): Boolean; overload;
    function TryGetDays(out _Days: Int64): Boolean; overload;
    function TryGetDays(out _Days: Integer): Boolean; overload;
    function GetDays(out _Days: Double): Boolean; overload; deprecated; // use TryGetDays
    function GetDays(out _Days: Int64): Boolean; overload; deprecated; // use TryGetDays
    function GetDays(out _Days: Integer): Boolean; overload; deprecated; // use TryGetDays
    function TryGetHours(out _Hours: Double): Boolean; overload;
    function TryGetHours(out _Hours: Int64): Boolean; overload;
    function TryGetHours(out _Hours: Integer): Boolean; overload;
    function GetHours(out _Hours: Double): Boolean; overload; deprecated; // use TryGetHours
    function GetHours(out _Hours: Int64): Boolean; overload; deprecated; // use TryGetHours
    function GetHours(out _Hours: Integer): Boolean; overload; deprecated; // use TryGetHours
    function TryGetMinutes(out _Minutes: Double): Boolean; overload;
    function TryGetMinutes(out _Minutes: Int64): Boolean; overload;
    function TryGetMinutes(out _Minutes: Integer): Boolean; overload;
    function GetMinutes(out _Minutes: Double): Boolean; overload; deprecated; // use TryGetMinutes
    function GetMinutes(out _Minutes: Int64): Boolean; overload; deprecated; // use TryGetMinutes
    function GetMinutes(out _Minutes: Integer): Boolean; overload; deprecated; // use TryGetMinutes
    function TryGetSeconds(out _Seconds: Double): Boolean; overload;
    function TryGetSeconds(out _Seconds: Int64): Boolean; overload;
    function TryGetSeconds(out _Seconds: Integer): Boolean; overload;
    function GetSeconds(out _Seconds: Double): Boolean; overload; deprecated; // use TryGetSeconds
    function GetSeconds(out _Seconds: Int64): Boolean; overload; deprecated; // use TryGetSeconds
    function GetSeconds(out _Seconds: Integer): Boolean; overload; deprecated; // use TryGetSeconds
    function TryGetMilliSeconds(out _MilliSeconds: Double): Boolean; overload;
    function TryGetMilliSeconds(out _MilliSeconds: Int64): Boolean; overload;
    function TryGetMilliSeconds(out _MilliSeconds: Integer): Boolean; overload;
    function GetMilliSeconds(out _MilliSeconds: Double): Boolean; overload; deprecated; // use TryGetMillSeconds
    function GetMilliSeconds(out _MilliSeconds: Int64): Boolean; overload; deprecated; // use TryGetMillSeconds
    function GetMilliSeconds(out _MilliSeconds: Integer): Boolean; overload; deprecated; // use TryGetMillSeconds
    procedure GetDaysHoursMinutesSeconds(out _Days, _Hours, _Minutes, _Seconds: Int64); overload;
    procedure GetDaysHoursMinutesSeconds(out _Days, _Hours, _Minutes: Int64; out _Seconds: Double); overload;
    ///<summary> Generates a string of the form 'hh<separator>mm'
    ///          @param Separator is used to separate the hour and minute part,
    ///                           if Separator is #0, no separator is used.
    ///          @param NullValue is the value used if the TNullableTimespan value is not valid. </summary>
    function ToHHmm(_Separator: Char = #0; const _NullValue: string = ''): string;
    ///<summary> Converts the value to a string representation of InHours with the given
    ///          number of decimals. Returns an NullValue, if invalid.</summary>
    function ToHourStr(_Decimals: Integer = 1; const _NullValue: string = ''): string;
    function ForDisplay: string;
    ///<summary> Calculates the value from the given Days, Hours, Minutes, Seconds and
    ///          Milliseconds. All these values can be higher than the usual maximum value
    ///          eg. you could passs 26 hours and 100 seconds and still get a valid
    ///          result of 1 day, 2 hours, 1 Minute and 40 seconds.
    ///          Note: you cannot assign month's or years because they vary in length </summary>
    procedure Assign(_Days, _Hours, _Minutes, _Seconds, _MilliSeconds: Word);
    ///<summary> Note that Days is not a TDateTime value representing a date but just a number
    ///          of days with possibly fractions. </summary>
    procedure AssignDays(_Days: Double);
    procedure AssignHours(_Hours: Extended); overload;
    procedure AssignHours(_Hours: TNullableExtended); overload;
    procedure AssignMinutes(_Minutes: Extended); overload;
    procedure AssignMinutes(_Minutes: Int64); overload;
    procedure AssignSeconds(_Seconds: Extended); overload;
    procedure AssignSeconds(_Seconds: Int64); overload;
    procedure AssignMilliseconds(_MilliSeconds: Int64); overload;
    procedure AssignMilliseconds(_MilliSeconds: Extended); overload;
    procedure AssignMicroseconds(_MicroSeconds: Int64);
    procedure AssignZero;
    ///<summary>
    /// Interprets a string as an hour, supporting three formats:
    /// * hours with decimals
    /// * <hours>h<minutes>
    /// * <hours>:<minutes>
    /// The formats are tried in the above order.
    /// An empty string results in an invalid TNullableTimespan (self.IsValid = false).
    /// @raises EConvertError if the string cannot be converted. </summary>
    procedure AssignHoursStr(const _s: string);

    procedure Add(_Value: TNullableTimespan);
    procedure AddHours(_Hours: Extended);

    class operator GreaterThan(_a, _b: TNullableTimespan): Boolean;
    class operator LessThan(_a, _b: TNullableTimespan): Boolean;
    class operator GreaterThanOrEqual(_a, _b: TNullableTimespan): Boolean;
    class operator LessThanOrEqual(_a, _b: TNullableTimespan): Boolean;
    class operator Equal(_a, _b: TNullableTimespan): Boolean;
    class operator NotEqual(_a, _b: TNullableTimespan): Boolean;
    class operator Add(_a, _b: TNullableTimespan): TNullableTimespan;
    class operator Subtract(_a, _b: TNullableTimespan): TNullableTimespan;
    class operator Divide(_a: TNullableTimespan; _b: Integer): TNullableTimespan;
    class operator Divide(_a: TNullableTimespan; _b: Extended): TNullableTimespan;
    class operator Multiply(_a: TNullableTimespan; _b: Integer): TNullableTimespan;
    class operator Multiply(_a: TNullableTimespan; _b: Extended): TNullableTimespan;

    class function Zero: TNullableTimespan; static;
    class function FromDays(_Days: Double): TNullableTimespan; static;
    class function FromHours(_Hours: Extended): TNullableTimespan; overload; static;
    class function FromHours(_Hours: TNullableExtended): TNullableTimespan; overload; static;
    class function FromHoursStr(const _s: string): TNullableTimespan; overload; static;
    class function FromSeconds(_Seconds: Extended): TNullableTimespan; static;
    class function Combine(_Days, _Hours, _Minutes, _Seconds, _MilliSeconds: Word): TNullableTimespan; static;
  end;

type
  TdzNullableTimespan = TNullableTimespan deprecated;

{$ENDIF SUPPORTS_ENHANCED_RECORDS}

implementation

{$IFDEF SUPPORTS_ENHANCED_RECORDS}

uses
  Math,
  DateUtils,
  u_dzConvertUtils,
  u_dzDateUtils,
  u_dzNullableTypesUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TNullableTimespan }

// Inlined method must be implemented before called
procedure TNullableTimespan.SetDaysAndMicroseconds(_FullDays: Int64; _MicroSeconds: Int64);
begin
  FFullDays := _FullDays;
  FMicroSeconds := _MicroSeconds;
  FIsValid := GetNullableTypesFlagInterface;
end;

procedure TNullableTimespan.AssignDays(_Days: Double);
begin
  SetDaysAndMicroseconds(Trunc(_Days), Round(Frac(_Days) / OneMicrosecond));
end;

procedure TNullableTimespan.AssignHours(_Hours: Extended);
begin
  AssignDays(_Hours * OneHour);
end;

procedure TNullableTimespan.AssignHours(_Hours: TNullableExtended);
begin
  if _Hours.IsValid then
    AssignHours(_Hours.Value)
  else
    Invalidate;
end;

procedure TNullableTimespan.AssignHoursStr(const _s: string);
var
  flt: Extended;
begin
  if _s = '' then begin
    Invalidate;
    Exit;
  end;

  if TryStr2Float(_s, flt, #0) then
    AssignHours(flt)
  else if TryHHmm2Hours(_s, flt, 'h') then
    AssignHours(flt)
  else if TryHHmm2Hours(_s, flt, ':') then
    AssignHours(flt)
  else
    raise EConvertError.CreateFmt(_('Could not convert "%s" to %s'), [_s, 'TNullableTimespan']);
end;

procedure TNullableTimespan.AssignMicroseconds(_MicroSeconds: Int64);
begin
  SetDaysAndMicroseconds(_MicroSeconds div MillisecondsPerDay div 10, (_MicroSeconds mod MicrosecondsPerDay));
end;

procedure TNullableTimespan.AssignMilliseconds(_MilliSeconds: Int64);
begin
  SetDaysAndMicroseconds(_MilliSeconds div MillisecondsPerDay, (_MilliSeconds mod MillisecondsPerDay) * MicrosecondsPerMillisecond);
end;

procedure TNullableTimespan.AssignMilliseconds(_MilliSeconds: Extended);
begin
  AssignDays(_MilliSeconds / MillisecondsPerDay);
end;

procedure TNullableTimespan.AssignMinutes(_Minutes: Extended);
begin
  AssignDays(_Minutes * OneMinute);
end;

procedure TNullableTimespan.AssignMinutes(_Minutes: Int64);
begin
  SetDaysAndMicroseconds(_Minutes div MinutesPerDay, (_Minutes mod MinutesPerDay) * MicrosecondsPerMinute);
end;

procedure TNullableTimespan.AssignSeconds(_Seconds: Int64);
begin
  SetDaysAndMicroseconds(_Seconds div SecondsPerDay, (_Seconds mod SecondsPerDay) * MicrosecondsPerSecond);
end;

procedure TNullableTimespan.AssignSeconds(_Seconds: Extended);
begin
  AssignDays(_Seconds * OneSecond);
end;

procedure TNullableTimespan.AssignZero;
begin
  AssignDays(0);
end;

class function TNullableTimespan.Combine(_Days, _Hours, _Minutes, _Seconds,
  _MilliSeconds: Word): TNullableTimespan;
begin
  Result.Assign(_Days, _Hours, _Minutes, _Seconds, _MilliSeconds);
end;

class operator TNullableTimespan.Divide(_a: TNullableTimespan; _b: Extended): TNullableTimespan;
begin
  Result.AssignDays(_a.InDays / _b);
end;

class operator TNullableTimespan.Divide(_a: TNullableTimespan; _b: Integer): TNullableTimespan;
begin
  Result.AssignDays(_a.InDays / _b);
end;

class operator TNullableTimespan.Equal(_a, _b: TNullableTimespan): Boolean;
begin
  Result := SameValue(_a.InDays, _b.InDays);
end;

class function TNullableTimespan.FromDays(_Days: Double): TNullableTimespan;
begin
  Result.AssignDays(_Days);
end;

class function TNullableTimespan.FromHours(_Hours: TNullableExtended): TNullableTimespan;
begin
  Result.AssignHours(_Hours)
end;

class function TNullableTimespan.FromHoursStr(const _s: string): TNullableTimespan;
begin
  Result.AssignHoursStr(_s);
end;

class function TNullableTimespan.FromHours(_Hours: Extended): TNullableTimespan;
begin
  Result.AssignHours(_Hours);
end;

class function TNullableTimespan.FromSeconds(_Seconds: Extended): TNullableTimespan;
begin
  Result.AssignSeconds(_Seconds);
end;

class operator TNullableTimespan.Add(_a, _b: TNullableTimespan): TNullableTimespan;
begin
  Result := _a;
  Result.Add(_b);
end;

procedure TNullableTimespan.Add(_Value: TNullableTimespan);
begin
  FFullDays := FFullDays + _Value.FFullDays;
  FMicroSeconds := FMicroSeconds + _Value.FMicroSeconds;
  while FMicroSeconds > MicrosecondsPerDay do begin
    Inc(FFullDays);
    Dec(FMicroSeconds, MicrosecondsPerDay);
  end;
end;

procedure TNullableTimespan.AddHours(_Hours: Extended);
begin
  AssignHours(InHours + _Hours);
end;

procedure TNullableTimespan.Assign(_Days, _Hours, _Minutes, _Seconds, _MilliSeconds: Word);
begin
  SetDaysAndMicroseconds(_Days, _Hours * MicrosecondsPerHour + _Minutes * MicrosecondsPerMinute
    + _Seconds + MicrosecondsPerSecond + _MilliSeconds * MicrosecondsPerMillisecond);
end;

function TNullableTimespan.TryGetDays(out _Days: Double): Boolean;
begin
  Result := IsValid;
  if Result then begin
    _Days := FFullDays + FMicroSeconds * OneMicrosecond;
  end;
end;

function TNullableTimespan.GetDays(out _Days: Double): Boolean;
begin
  Result := TryGetDays(_Days);
end;

function TNullableTimespan.TryGetDays(out _Days: Int64): Boolean;
begin
  Result := IsValid;
  if Result then
    _Days := InFullDays;
end;

function TNullableTimespan.GetDays(out _Days: Int64): Boolean;
begin
  Result := TryGetDays(_Days);
end;

function TNullableTimespan.TryGetDays(out _Days: Integer): Boolean;
begin
  Result := IsValid;
  if Result then
    _Days := InFullDays;
end;

function TNullableTimespan.GetDays(out _Days: Integer): Boolean;
begin
  Result := TryGetDays(_Days);
end;

function TNullableTimespan.TryGetHours(out _Hours: Double): Boolean;
begin
  Result := IsValid;
  if Result then
    _Hours := InHours;
end;

function TNullableTimespan.GetHours(out _Hours: Double): Boolean;
begin
  Result := TryGetHours(_Hours);
end;

function TNullableTimespan.TryGetHours(out _Hours: Int64): Boolean;
begin
  Result := IsValid;
  if Result then
    _Hours := InFullHours;
end;

function TNullableTimespan.GetHours(out _Hours: Int64): Boolean;
begin
  Result := TryGetHours(_Hours);
end;

function TNullableTimespan.TryGetHours(out _Hours: Integer): Boolean;
begin
  Result := IsValid;
  if Result then
    _Hours := InFullHours;
end;

function TNullableTimespan.GetHours(out _Hours: Integer): Boolean;
begin
  Result := TryGetHours(_Hours);
end;

function TNullableTimespan.TryGetMinutes(out _Minutes: Double): Boolean;
begin
  Result := IsValid;
  if Result then
    _Minutes := InMinutes;
end;

function TNullableTimespan.GetMinutes(out _Minutes: Double): Boolean;
begin
  Result := TryGetMinutes(_Minutes);
end;

function TNullableTimespan.TryGetMinutes(out _Minutes: Int64): Boolean;
begin
  Result := IsValid;
  if Result then
    _Minutes := InFullMinutes;
end;

function TNullableTimespan.GetMinutes(out _Minutes: Int64): Boolean;
begin
  Result := TryGetMinutes(_Minutes);
end;

function TNullableTimespan.TryGetMinutes(out _Minutes: Integer): Boolean;
begin
  Result := IsValid;
  if Result then
    _Minutes := InFullMinutes;
end;

function TNullableTimespan.GetMinutes(out _Minutes: Integer): Boolean;
begin
  Result := TryGetMinutes(_Minutes);
end;

function TNullableTimespan.TryGetSeconds(out _Seconds: Double): Boolean;
begin
  Result := IsValid;
  if Result then
    _Seconds := InSeconds;
end;

function TNullableTimespan.GetSeconds(out _Seconds: Double): Boolean;
begin
  Result := TryGetSeconds(_Seconds);
end;

function TNullableTimespan.TryGetSeconds(out _Seconds: Int64): Boolean;
begin
  Result := IsValid;
  if Result then
    _Seconds := InFullSeconds;
end;

function TNullableTimespan.GetSeconds(out _Seconds: Int64): Boolean;
begin
  Result := TryGetSeconds(_Seconds);
end;

function TNullableTimespan.TryGetSeconds(out _Seconds: Integer): Boolean;
begin
  Result := IsValid;
  if Result then
    _Seconds := InFullSeconds;
end;

function TNullableTimespan.GetSeconds(out _Seconds: Integer): Boolean;
begin
  Result := TryGetSeconds(_Seconds);
end;

function TNullableTimespan.TryGetMilliSeconds(out _MilliSeconds: Double): Boolean;
begin
  Result := IsValid;
  if Result then
    _MilliSeconds := InMilliseconds;
end;

function TNullableTimespan.GetMilliSeconds(out _MilliSeconds: Double): Boolean;
begin
  Result := TryGetMilliSeconds(_MilliSeconds);
end;

function TNullableTimespan.TryGetMilliSeconds(out _MilliSeconds: Int64): Boolean;
begin
  Result := IsValid;
  if Result then
    _MilliSeconds := InFullMilliseconds;
end;

function TNullableTimespan.GetMilliSeconds(out _MilliSeconds: Int64): Boolean;
begin
  Result := TryGetMilliSeconds(_MilliSeconds);
end;

function TNullableTimespan.TryGetMilliSeconds(out _MilliSeconds: Integer): Boolean;
begin
  Result := IsValid;
  if Result then
    _MilliSeconds := InFullMilliseconds;
end;

function TNullableTimespan.GetMilliSeconds(out _MilliSeconds: Integer): Boolean;
begin
  Result := TryGetMilliSeconds(_MilliSeconds);
end;

class operator TNullableTimespan.GreaterThan(_a, _b: TNullableTimespan): Boolean;
begin
  Result := (_a.InDays > _b.InDays);
end;

class operator TNullableTimespan.LessThan(_a, _b: TNullableTimespan): Boolean;
begin
  Result := (_a.InDays < _b.InDays);
end;

class operator TNullableTimespan.LessThanOrEqual(_a, _b: TNullableTimespan): Boolean;
begin
  Result := not (_a > _b);
end;

class operator TNullableTimespan.Multiply(_a: TNullableTimespan; _b: Integer): TNullableTimespan;
begin
  Result.AssignDays(_a.InDays * _b);
end;

class operator TNullableTimespan.Multiply(_a: TNullableTimespan; _b: Extended): TNullableTimespan;
begin
  Result.AssignDays(_a.InDays * _b);
end;

class operator TNullableTimespan.NotEqual(_a, _b: TNullableTimespan): Boolean;
begin
  Result := not (_a = _b);
end;

class operator TNullableTimespan.Subtract(_a, _b: TNullableTimespan): TNullableTimespan;
begin
  Result.AssignDays(_a.InDays - _b.InDays);
end;

function TNullableTimespan.ToHHmm(_Separator: Char; const _NullValue: string): string;
var
  Separator: string;
  IsNegative: Boolean;
  FullHours: Integer;
  FullMinutes: Integer;
begin
  if IsValid then begin
    if _Separator <> #0 then
      Separator := _Separator
    else
      Separator := '';

    FullHours := Self.InFullHours;
    FullMinutes := Self.InFullMinutes;
    IsNegative := (FullHours < 0) or (FullMinutes < 0);
    FullHours := Abs(FullHours);
    FullMinutes := Abs(FullMinutes);
    Result := Format('%.2d', [FullHours]) + Separator + Format('%.2d', [FullMinutes mod 60]);
    if IsNegative then
      Result := '-' + Result;
  end else
    Result := _NullValue;
end;

function TNullableTimespan.ToHourStr(_Decimals: Integer = 1; const _NullValue: string = ''): string;
begin
  if IsValid then
    Result := Float2Str(InHours, _Decimals, DecimalSeparator)
  else
    Result := _NullValue;
end;

procedure TNullableTimespan.GetDaysHoursMinutesSeconds(out _Days, _Hours, _Minutes, _Seconds: Int64);
begin
  _Seconds := InFullSeconds;
  _Minutes := InFullMinutes;
  _Hours := InFullHours;
  _Days := InFullDays;
  _Seconds := _Seconds - _Minutes * 60;
  _Minutes := _Minutes - _Hours * 60;
  _Hours := _Hours - _Days * 24;
end;

procedure TNullableTimespan.GetDaysHoursMinutesSeconds(out _Days, _Hours, _Minutes: Int64; out _Seconds: Double);
begin
  _Seconds := InSeconds;
  _Minutes := InFullMinutes;
  _Hours := InFullHours;
  _Days := InFullDays;
  _Seconds := _Seconds - _Minutes * 60;
  _Minutes := _Minutes - _Hours * 60;
  _Hours := _Hours - _Days * 24;
end;

function AppendToStr(const _s: string; const _ToAppend: string; const _Delimiter: string = ' '): string;
begin
  Result := _s;
  if Result <> '' then
    Result := Result + _Delimiter;
  Result := Result + _ToAppend;
end;

function TNullableTimespan.ForDisplay: string;
var
  d: Int64;
  h: Int64;
  m: Int64;
  s: Double;
begin
  if IsValid then begin
    Result := '';
    GetDaysHoursMinutesSeconds(d, h, m, s);
    if d > 0 then
      Result := AppendToStr(Result, Format(_('%d days'), [d]));
    if h > 0 then
      Result := AppendToStr(Result, Format(_('%d hours'), [h]));
    if (d = 0) then
      Result := AppendToStr(Result, Format(_('%d minutes'), [m]));
    if (d = 0) and (h = 0) then
      Result := AppendToStr(Result, Format(_('%.2f seconds'), [s]));
  end else
    Result := _('invalid');
end;

class function TNullableTimespan.Zero: TNullableTimespan;
begin
  Result.AssignZero;
end;

class operator TNullableTimespan.GreaterThanOrEqual(_a, _b: TNullableTimespan): Boolean;
begin
  Result := not (_a < _b);
end;

procedure TNullableTimespan.Invalidate;
begin
  FIsValid := nil;
end;

function TNullableTimespan.IsValid: Boolean;
begin
  Result := Assigned(FIsValid);
end;

procedure TNullableTimespan.CheckIsValid;
begin
  if not IsValid then
    raise EInvalidValue.Create(_('NullableTimespan value is invalid'));
end;

function TNullableTimespan.InDays: Double;
begin
  if not TryGetDays(Result) then
    raise EInvalidValue.Create(_('NullableTimespan value is invalid'));
end;

function TNullableTimespan.InFullDays: Int64;
begin
  CheckIsValid;
  Result := FFullDays;
end;

function TNullableTimespan.InHours: Double;
begin
  CheckIsValid;
  Result := FFullDays * HoursPerDay + FMicroSeconds / MicrosecondsPerHour;
end;

function TNullableTimespan.InFullHours: Int64;
begin
  CheckIsValid;
  Result := FFullDays * HoursPerDay + FMicroSeconds div MicrosecondsPerHour;
end;

function TNullableTimespan.InMinutes: Double;
begin
  CheckIsValid;
  Result := FFullDays * MinutesPerDay + FMicroSeconds / MicrosecondsPerMinute;
end;

function TNullableTimespan.InFullMinutes: Int64;
begin
  CheckIsValid;
  Result := FFullDays * MinutesPerDay + FMicroSeconds div MicrosecondsPerMinute;
end;

function TNullableTimespan.InSeconds: Double;
begin
  CheckIsValid;
  Result := FFullDays * SecondsPerDay + FMicroSeconds / MicrosecondsPerSecond;
end;

function TNullableTimespan.InFullSeconds: Int64;
begin
  CheckIsValid;
  Result := FFullDays * SecondsPerDay + FMicroSeconds div MicrosecondsPerSecond;
end;

function TNullableTimespan.InMilliseconds: Double;
begin
  CheckIsValid;
  Result := FFullDays * MillisecondsPerDay + FMicroSeconds / MicrosecondsPerMillisecond;
end;

function TNullableTimespan.InFullMilliseconds: Int64;
begin
  CheckIsValid;
  Result := FFullDays * MillisecondsPerDay + FMicroSeconds div MicrosecondsPerMillisecond;
end;

function TNullableTimespan.InMicroseconds: Double;
begin
  CheckIsValid;
  Result := FFullDays * MicrosecondsPerDay + FMicroSeconds;
end;

function TNullableTimespan.InFullMicroseconds: Int64;
begin
  CheckIsValid;
  Result := FFullDays * MicrosecondsPerDay + FMicroSeconds;
end;

{$ENDIF SUPPORTS_ENHANCED_RECORDS}

end.
