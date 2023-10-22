unit u_dzIso8601;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  SysUtils,
  Math,
  u_dzTranslator,
  u_dzTypes;

type
  EIso8601Error = class(EdzException)
  end;

type
  TIsoDateSeparator = (dsDash, dsNone);
  TIsoTimeSeparator = (tsColon, tsNone);
  // according to https://en.wikipedia.org/wiki/ISO_8601
  // a comma is the preferred decimal separator.
  TIsoDecimalSeparator = (dsComma, dsPoint);

type
  // conversion functions for ISO 8601 date / time format
  TIso8601 = record
  private
    FDateSeparator: TIsoDateSeparator;
    FTimeSeparator: TIsoTimeSeparator;
    FDecimalSeparator: TIsoDecimalSeparator;
    function YearToStr(_Year: Integer): string;
    function GetDateSeparator: string;
    function GetTimeSeparator: string;
    function GetDecimalSeparator: string;
  public
    property DateSeparator: TIsoDateSeparator read FDateSeparator;
    property TimeSeparator: TIsoTimeSeparator read FTimeSeparator;
    property DecimalSeparator: TIsoDecimalSeparator read FDecimalSeparator;

    class function Create(_DecimalSeparator: TIsoDecimalSeparator = dsPoint): TIso8601; overload; static;
    class function Create(_DateSeparator: TIsoDateSeparator; _TimeSeparator: TIsoTimeSeparator;
      _DecimalSeparator: TIsoDecimalSeparator): TIso8601; overload; static;
    procedure Init(_DecimalSeparator: TIsoDecimalSeparator); overload;
    procedure Init(_DateSeparator: TIsoDateSeparator; _TimeSeparator: TIsoTimeSeparator;
      _DecimalSeparator: TIsoDecimalSeparator); overload;

    function ToCalendarDate(_Date: TDateTime): string; overload;
    function ToCalendarDate(_Year: Integer; _Month, _Day: Word): string; overload;
    function ToCalendarDate(_Year: Integer; _Month: Word): string; overload;

    function ToOrdinalDate(_Date: TDateTime): string; overload;
    function ToOrdinalDate(_Year: Integer; _DayOfYear: Word): string; overload;

    function ToWeekDate(_Date: TDateTime; _IncludeDay: Boolean = True): string; overload;
    function ToWeekDate(_Year: Integer; _Week: Word; _DayOfWeek: Word): string; overload;
    function ToWeekDate(_Year: Integer; _Week: Word): string; overload;

    function ToTime(_Time: TDateTime; _IncludeSeconds: Boolean = True;
      _IncludeDecimals: Boolean = False): string; overload;
    function ToTime(_Hour, _Minute, _Seconds, _Milliseconds: Word): string; overload;
    function ToTime(_Hour, _Minute, _Seconds: Word): string; overload;
    function ToTime(_Hour, _Minute: Word): string; overload;

    function ToCalendarDateAndTime(_DateTime: TDateTime; _IncludeSeconds: Boolean = True;
      _IncludeDecimals: Boolean = False): string; overload;

    procedure FromCalendarDate(const _s: string; out _Date: TDateTime); overload;
    ///<summary>
    /// @param Tail returns any part of the string after the year </summary>
    procedure FromCalendarDate(const _s: string; out _Year: Integer; out _Tail: string); overload;
    ///<summary>
    /// @raises EIso8601Error if the input is not a valid date or contains more parts than just the year. </summary>
    procedure FromCalendarDate(const _s: string; out _Year: Integer); overload;
    ///<summary>
    /// @param Tail returns any part of the string after the month </summary>
    procedure FromCalendarDate(const _s: string; out _Year: Integer; out _Month: Word; out _Tail: string); overload;
    ///<summary>
    /// @raises EIso8601Error if the input is not a valid date or contains more parts than just year and month. </summary>
    procedure FromCalendarDate(const _s: string; out _Year: Integer; out _Month: Word); overload;
    ///<summary>
    /// @param Tail returns any part of the string after the day </summary>
    procedure FromCalendarDate(const _s: string; out _Year: Integer; out _Month, _Day: Word; out _Tail: string); overload;
    ///<summary>
    /// @raises EIso8601Error if the input is not a valid date or contains more parts than just the date. </summary>
    procedure FromCalendarDate(const _s: string; out _Year: Integer; out _Month, _Day: Word); overload;

    ///<summary>
    /// @param Tail returns any part of the string after the hour </summary>
    procedure FromTime(const _s: string; out _Hours: Word; out _Tail: string); overload;
    ///<summary>
    /// @raises EIso8601Error if the input is not a valid time or contains more parts than just the hour. </summary>
    procedure FromTime(const _s: string; out _Hours: Word); overload;

    ///<summary>
    /// @param Tail returns any part of the string after the minutes </summary>
    procedure FromTime(const _s: string; out _Hours, _Minutes: Word; out _Tail: string); overload;
    ///<summary>
    /// @raises EIso8601Error if the input is not a valid time or contains more parts than just hour and minutes. </summary>
    procedure FromTime(const _s: string; out _Hours, _Minutes: Word); overload;

    ///<summary>
    /// @param Tail returns any part of the string after the seconds </summary>
    procedure FromTime(const _s: string; out _Hours, _Minutes: Word; out _Seconds: Extended;
      out _Tail: string); overload;
    ///<summary>
    /// @raises EIso8601Error if the input is not a valid time or contains more parts than just hour, minutes and seconds. </summary>
    procedure FromTime(const _s: string; out _Hours, _Minutes: Word; out _Seconds: Extended); overload;

    procedure FromDateAndTime(const _s: string; out _Year: Integer; out _Month, _Day: Word;
      out _Hours, _Minutes: Word; out _Tail: string); overload;
    procedure FromDateAndTime(const _s: string; out _Year: Integer; out _Month, _Day: Word;
      out _Hours, _Minutes: Word); overload;
    procedure FromDateAndTime(const _s: string; out _Year: Integer; out _Month, _Day: Word;
      out _Hours, _Minutes: Word; out _Seconds: Extended; out _Tail: string); overload;
    procedure FromDateAndTime(const _s: string; out _Year: Integer; out _Month, _Day: Word;
      out _Hours, _Minutes: Word; out _Seconds: Extended); overload;
    function FromDateAndTime(const _s: string): TDateTime; overload;
  end;

{$IFDEF DEBUG}
function DumpIso(_DateTime: TDateTime): string;
{$ENDIF DEBUG}

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  DateUtils,
  u_dzConvertUtils;

function _(const _s: string): string; inline;
begin
  Result := dzGetText(_s);
end;

{ TIso8601 }

procedure TIso8601.FromCalendarDate(const _s: string; out _Date: TDateTime);
var
  Year: Integer;
  Month: Word;
  Day: Word;
begin
  FromCalendarDate(_s, Year, Month, Day);
  if Year < 0 then
    raise EIso8601Error.CreateFmt(_('"%s" cannot be converted to a TDateTime value (negative years are not supported)'), [_s]);

  _Date := EncodeDate(Year, Month, Day);
end;

procedure TIso8601.FromCalendarDate(const _s: string; out _Year: Integer; out _Tail: string);
var
  s: string;
  c: Char;
  YearStr: string;
  Sign: Integer;
begin
  s := _s;
  if s = '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid calendar date format'), [_s]);
  c := s[1];
  case c of
    '+': begin
        Sign := 1;
        s := Copy(s, 2);
      end;
    '0'..'9': Sign := 1;
    '-': begin
        Sign := -1;
        s := Copy(s, 2);
      end;
  else
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid calendar date format'), [_s]);
  end;
  YearStr := Copy(s, 1, 4);
  if Length(YearStr) <> 4 then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid calendar date format'), [_s]);
  _Year := Dec2Long(YearStr);
  _Year := _Year * Sign;
  _Tail := Copy(s, 5);
end;

procedure TIso8601.FromCalendarDate(const _s: string; out _Year: Integer);
var
  Tail: string;
begin
  FromCalendarDate(_s, _Year, Tail);
  if Tail <> '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid calendar date format for just the year'), [_s]);
end;

procedure TIso8601.FromCalendarDate(const _s: string; out _Year: Integer; out _Month: Word;
  out _Tail: string);
var
  s: string;
  c: Char;
  MonthStr: string;
begin
  FromCalendarDate(_s, _Year, s);
  if s = '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid calendar date format for year and month'), [_s]);

  case DateSeparator of
    dsDash: begin
        c := s[1];
        if c <> GetDateSeparator then
          raise EIso8601Error.CreateFmt(_('"%s" is not a valid calendar date format for year and month'), [_s]);
        s := Copy(s, 2);
      end;
  else //  dsNone:
  end;
  MonthStr := Copy(s, 1, 2);
  if Length(MonthStr) <> 2 then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid calendar date format for year and month'), [_s]);
  _Month := Dec2Long(MonthStr);
  _Tail := Copy(s, 3);
end;

procedure TIso8601.FromCalendarDate(const _s: string; out _Year: Integer; out _Month: Word);
var
  Tail: string;
begin
  FromCalendarDate(_s, _Year, _Month, Tail);
  if Tail <> '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid calendar date format for year and month'), [_s]);
end;

procedure TIso8601.FromCalendarDate(const _s: string; out _Year: Integer; out _Month, _Day: Word;
  out _Tail: string);
var
  s: string;
  c: Char;
  DayStr: string;
begin
  FromCalendarDate(_s, _Year, _Month, s);
  if s = '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid calendar date format for year, month and day'), [_s]);
  case DateSeparator of
    dsDash: begin
        c := s[1];
        if c <> GetDateSeparator then
          raise EIso8601Error.CreateFmt(_('"%s" is not a valid calendar date format for year, month and day'), [_s]);
        s := Copy(s, 2);
      end;
  else //  dsNone:
  end;
  DayStr := Copy(s, 1, 2);
  if Length(DayStr) <> 2 then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid calendar date format for year, month and day'), [_s]);
  _Day := Dec2Long(DayStr);
  _Tail := Copy(s, 3);
end;

class function TIso8601.Create(_DecimalSeparator: TIsoDecimalSeparator): TIso8601;
begin
  Result.Init(_DecimalSeparator);
end;

class function TIso8601.Create(_DateSeparator: TIsoDateSeparator; _TimeSeparator: TIsoTimeSeparator;
  _DecimalSeparator: TIsoDecimalSeparator): TIso8601;
begin
  Result.Init(_DateSeparator, _TimeSeparator, _DecimalSeparator);
end;

procedure TIso8601.FromCalendarDate(const _s: string; out _Year: Integer; out _Month, _Day: Word);
var
  s: string;
begin
  FromCalendarDate(_s, _Year, _Month, _Day, s);
  if s <> '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid calendar date format for year, month and day'), [_s]);
end;

procedure TIso8601.FromDateAndTime(const _s: string; out _Year: Integer; out _Month, _Day, _Hours,
  _Minutes: Word; out _Tail: string);
var
  s: string;
begin
  FromCalendarDate(_s, _Year, _Month, _Day, s);
  if s = '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid date and time format for hours and minutes'), [_s]);
  if s[1] <> 'T' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid date and time format for hours and minutes'), [_s]);
  FromTime(Copy(s, 2), _Hours, _Minutes, _Tail);
end;

procedure TIso8601.FromDateAndTime(const _s: string; out _Year: Integer; out _Month, _Day, _Hours,
  _Minutes: Word);
var
  Tail: string;
begin
  FromDateAndTime(_s, _Year, _Month, _Day, _Hours, _Minutes, Tail);
  if Tail <> '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid date and time format for hours and minutes'), [_s]);
end;

procedure TIso8601.FromDateAndTime(const _s: string; out _Year: Integer; out _Month, _Day: Word;
  out _Hours, _Minutes: Word; out _Seconds: Extended; out _Tail: string);
var
  s: string;
  c: Char;
  SecondsStr: string;
  FullSeconds: Word;
  i: Integer;
  Fractions: Extended;
begin
  FromDateAndTime(_s, _Year, _Month, _Day, _Hours, _Minutes, s);
  if s = '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid date and time format for hours, minutes and seconds'), [_s]);
  case TimeSeparator of
    tsColon: begin
        c := s[1];
        if c <> GetTimeSeparator then
          raise EIso8601Error.CreateFmt(_('"%s" is not a valid date and time format for hours, minutes and seconds'), [_s]);
        s := Copy(s, 2);
      end;
  else // tsNone: ;
  end;
  SecondsStr := Copy(s, 1, 2);
  if Length(SecondsStr) <> 2 then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid date and time format for hours, minutes and seconds'), [_s]);
  FullSeconds := Dec2Long(SecondsStr);
  s := Copy(s, 3);
  if s = '' then begin
    _Seconds := FullSeconds;
    _Tail := '';
    Exit; //==>
  end;
  c := s[1];
  if c = GetDecimalSeparator then begin
    s := Copy(s, 2);
    i := 1;
    while (i < Length(s)) and isDecDigit(s[i]) do begin
      Inc(i)
    end;
    s := Copy(s, 1, i);
    _Tail := Copy(s, i + 1);
    s := '0.' + s;
    Fractions := Str2Float(s, _s, '.');
    _Seconds := FullSeconds + Fractions;
  end else begin
    _Seconds := FullSeconds;
    _Tail := s;
  end;
end;

procedure TIso8601.FromDateAndTime(const _s: string; out _Year: Integer; out _Month, _Day: Word;
  out _Hours, _Minutes: Word; out _Seconds: Extended);
var
  Tail: string;
begin
  FromDateAndTime(_s, _Year, _Month, _Day, _Hours, _Minutes, _Seconds, Tail);
  if Tail <> '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid date and time format for hours, minutes and seconds'), [_s]);
end;

function TIso8601.FromDateAndTime(const _s: string): TDateTime;
var
  Year: Integer;
  Month: Word;
  Day: Word;
  Hours: Word;
  Minutes: Word;
  Seconds: Extended;
begin
  FromDateAndTime(_s, Year, Month, Day, Hours, Minutes, Seconds);
  if Year < 0 then
    raise EIso8601Error.CreateFmt(_('"%s" cannot be converted to a TDateTime value (negative years are not supported)'), [_s]);
  Result := EncodeDate(Year, Month, Day) + encodetime(Hours, Minutes, 0, 0) + Seconds / SecondsPerDay;
end;

procedure TIso8601.FromTime(const _s: string; out _Hours: Word; out _Tail: string);
var
  HourStr: string;
begin
  HourStr := Copy(_s, 1, 2);
  if Length(HourStr) <> 2 then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid time format for hours only'), [_s]);
  _Hours := Dec2Long(HourStr);
  _Tail := Copy(_s, 3);
end;

procedure TIso8601.FromTime(const _s: string; out _Hours: Word);
var
  Tail: string;
begin
  FromTime(_s, _Hours, Tail);
  if Tail <> '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid time format for hours only'), [_s]);
end;

procedure TIso8601.FromTime(const _s: string; out _Hours, _Minutes: Word; out _Tail: string);
var
  s: string;
  c: Char;
  MinutesStr: string;
begin
  FromTime(_s, _Hours, s);
  if s = '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid time format for hours and minutes'), [_s]);
  case TimeSeparator of
    tsColon: begin
        c := s[1];
        if c <> GetTimeSeparator then
          raise EIso8601Error.CreateFmt(_('"%s" is not a valid time format for hours and minutes'), [_s]);
        s := Copy(s, 2);
      end;
  else // tsNone: ;
  end;
  MinutesStr := Copy(s, 1, 2);
  if Length(MinutesStr) <> 2 then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid time format for hours and minutes'), [_s]);
  _Minutes := Dec2Long(MinutesStr);
  _Tail := Copy(s, 3);
end;

procedure TIso8601.FromTime(const _s: string; out _Hours, _Minutes: Word);
var
  Tail: string;
begin
  FromTime(_s, _Hours, _Minutes, Tail);
  if Tail <> '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid time format for hours and minutes'), [_s]);
end;

procedure TIso8601.FromTime(const _s: string; out _Hours, _Minutes: Word; out _Seconds: Extended;
  out _Tail: string);
var
  s: string;
  SecondsStr: string;
  c: Char;
  FullSeconds: Word;
  i: Integer;
  Fractions: Extended;
begin
  FromTime(_s, _Hours, _Minutes, s);
  if s = '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid time format for hours, minutes and seconds'), [_s]);
  case TimeSeparator of
    tsColon: begin
        c := s[1];
        if c <> GetTimeSeparator then
          raise EIso8601Error.CreateFmt(_('"%s" is not a valid time format for hours, minutes and seconds'), [_s]);
        s := Copy(s, 2);
      end;
  else // tsNone: ;
  end;
  SecondsStr := Copy(s, 1, 2);
  if Length(SecondsStr) <> 2 then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid time format for hours, minutes and seconds'), [_s]);
  FullSeconds := Dec2Long(SecondsStr);
  s := Copy(s, 3);
  if s = '' then begin
    _Seconds := FullSeconds;
    _Tail := '';
    Exit; //==>
  end;
  c := s[1];
  if c = GetDecimalSeparator then begin
    s := Copy(s, 2);
    i := 1;
    while (i < Length(s)) and isDecDigit(s[i]) do begin
      Inc(i)
    end;
    s := Copy(s, 1, i);
    _Tail := Copy(s, i + 1);
    s := '0.' + s;
    Fractions := Str2Float(s, _s, '.');
    _Seconds := FullSeconds + Fractions;
  end else begin
    _Seconds := FullSeconds;
    _Tail := s;
  end;
end;

procedure TIso8601.FromTime(const _s: string; out _Hours, _Minutes: Word; out _Seconds: Extended);
var
  Tail: string;
begin
  FromTime(_s, _Hours, _Minutes, _Seconds, Tail);
  if Tail <> '' then
    raise EIso8601Error.CreateFmt(_('"%s" is not a valid time format for hours, minutes and seconds'), [_s]);
end;

function TIso8601.GetDateSeparator: string;
begin
  case DateSeparator of
    dsNone: Result := '';
  else // dsDash:
    Result := '-';
  end;
end;

function TIso8601.GetDecimalSeparator: string;
begin
  case DecimalSeparator of
    dsPoint: Result := '.';
  else //    dsComma:
    Result := ',';
  end;
end;

function TIso8601.GetTimeSeparator: string;
begin
  case TimeSeparator of
    tsNone: Result := '';
  else //    tsColon:
    Result := ':';
  end;
end;

procedure TIso8601.Init(_DecimalSeparator: TIsoDecimalSeparator);
begin
  FDateSeparator := dsDash;
  FTimeSeparator := tsColon;
  FDecimalSeparator := _DecimalSeparator;
end;

procedure TIso8601.Init(_DateSeparator: TIsoDateSeparator; _TimeSeparator: TIsoTimeSeparator;
  _DecimalSeparator: TIsoDecimalSeparator);
begin
  FDateSeparator := _DateSeparator;
  FTimeSeparator := _TimeSeparator;
  FDecimalSeparator := _DecimalSeparator;
end;

function TIso8601.ToCalendarDate(_Year: Integer; _Month, _Day: Word): string;
var
  Separator: string;
begin
  if not InRange(_Month, 1, 12) then
    raise EIso8601Error.CreateFmt(_('%d is not a valid month value'), [_Month]);
  if not InRange(_Day, 1, 31) then
    raise EIso8601Error.CreateFmt(_('%d is not a valid day value'), [_Month]);

  Separator := GetDateSeparator;
  Result := YearToStr(_Year) + Separator + Long2Dec2(_Month) + Separator + Long2Dec2(_Day)
end;

function TIso8601.ToCalendarDate(_Date: TDateTime): string;
var
  Year: Word;
  Month: Word;
  Day: Word;
begin
  DecodeDate(_Date, Year, Month, Day);
  Result := ToCalendarDate(Year, Month, Day);
end;

function TIso8601.ToCalendarDate(_Year: Integer; _Month: Word): string;
begin
  if not InRange(_Month, 1, 12) then
    raise EIso8601Error.CreateFmt(_('%d is not a valid month value'), [_Month]);

  // if the day is ommitted, only yyy-mm is allowed
  Result := YearToStr(_Year) + '-' + Long2Dec2(_Month)
end;

function TIso8601.ToCalendarDateAndTime(_DateTime: TDateTime; _IncludeSeconds,
  _IncludeDecimals: Boolean): string;
begin
  Result := ToCalendarDate(_DateTime) + 'T' + ToTime(_DateTime, _IncludeSeconds, _IncludeDecimals);
end;

function TIso8601.ToOrdinalDate(_Year: Integer; _DayOfYear: Word): string;
begin
  Result := YearToStr(_Year) + GetDateSeparator + Long2DecN(_DayOfYear, 3);
end;

function TIso8601.ToOrdinalDate(_Date: TDateTime): string;
var
  DayOfYear: Word;
  Year: Word;
begin
  DayOfYear := DayOfTheYear(_Date);
  Year := DateUtils.YearOf(_Date);
  Result := ToOrdinalDate(Year, DayOfYear);
end;

function TIso8601.ToTime(_Hour, _Minute, _Seconds, _Milliseconds: Word): string;
var
  Separator: string;
begin
  if not InRange(_Hour, 0, 23) then begin
    if _Hour <> 24 then
      raise EIso8601Error.CreateFmt(_('%d is not a valid hour value'), [_Hour]);
    // Hour = 24 is only allowed for 24:00:00
    if (_Minute <> 0) or (_Seconds <> 0) then
      raise EIso8601Error.Create(_('for hour = 24 minutes and seconds must be 0'));
  end;
  if not InRange(_Minute, 0, 59) then
    raise EIso8601Error.CreateFmt(_('%d is not a valid minutes value'), [_Hour]);
  if not InRange(_Seconds, 0, 60) then begin
    // 60 is only used to denote an added leap second, but formally it's allowed
    raise EIso8601Error.CreateFmt(_('%d is not a valid seconds value'), [_Hour]);
  end;
  if not InRange(_Milliseconds, 0, 999) then
    raise EIso8601Error.CreateFmt(_('%d is not a valid milliseconds value'), [_Hour]);

  Separator := GetTimeSeparator;
  Result := Long2Dec2(_Hour) + Separator + Long2Dec2(_Minute) + Separator + Long2Dec2(_Seconds)
    + GetDecimalSeparator + Long2DecN(_Milliseconds, 3);
end;

function TIso8601.ToTime(_Hour, _Minute, _Seconds: Word): string;
var
  Separator: string;
begin
  if not InRange(_Hour, 0, 23) then begin
    if _Hour <> 24 then
      raise EIso8601Error.CreateFmt(_('%d is not a valid hour value'), [_Hour]);
    // Hour = 24 is only allowed for 24:00:00
    if (_Minute <> 0) or (_Seconds <> 0) then
      raise EIso8601Error.Create(_('for hour = 24 minutes and seconds must be 0'));
  end;
  if not InRange(_Minute, 0, 59) then
    raise EIso8601Error.CreateFmt(_('%d is not a valid minutes value'), [_Hour]);
  if not InRange(_Seconds, 0, 60) then begin
    // 60 is only used to denote an added leap second, but formally it's allowed
    raise EIso8601Error.CreateFmt(_('%d is not a valid seconds value'), [_Hour]);
  end;

  Separator := GetTimeSeparator;
  Result := Long2Dec2(_Hour) + Separator + Long2Dec2(_Minute) + Separator + Long2Dec2(_Seconds);
end;

function TIso8601.ToTime(_Hour, _Minute: Word): string;
var
  Separator: string;
begin
  if not InRange(_Hour, 0, 23) then begin
    if _Hour <> 24 then
      raise EIso8601Error.CreateFmt(_('%d is not a valid hour value'), [_Hour]);
    // Hour = 24 is only allowed for 24:00:00
    if _Minute <> 0 then
      raise EIso8601Error.Create(_('for hour = 24 minutes must be 0'));
  end;
  if not InRange(_Minute, 0, 59) then
    raise EIso8601Error.CreateFmt(_('%d is not a valid minutes value'), [_Hour]);

  Separator := GetTimeSeparator;
  Result := Long2Dec2(_Hour) + Separator + Long2Dec2(_Minute);
end;

function TIso8601.ToTime(_Time: TDateTime; _IncludeSeconds, _IncludeDecimals: Boolean): string;
var
  Hours: Word;
  Minutes: Word;
  Seconds: Word;
  Msec: Word;
begin
  DecodeTime(_Time, Hours, Minutes, Seconds, Msec);
  if _IncludeDecimals then
    Result := ToTime(Hours, Minutes, Seconds, Msec)
  else if _IncludeSeconds then
    Result := ToTime(Hours, Minutes, Seconds)
  else
    Result := ToTime(Hours, Minutes);
end;

function TIso8601.ToWeekDate(_Date: TDateTime; _IncludeDay: Boolean = True): string;
var
  Year: Word;
  Month: Word;
  Day: Word;
  Week: Word;
  DoW: Word;
begin
  DecodeDate(_Date, Year, Month, Day);
  Week := WeekOfTheYear(_Date);
  // there is an oddity in ISO8601 for days that belong to the first week of the following year
  // and days that belong to the last week of the previous year, e.g.:
  // 2008-12-29  (a Monday) belongs of the first week of 2009 which means it is '2009-W01-1
  // 2010-01-03  (a Sunday) belongs of the last week of 2009 which means it is '2009-W53-7
  if (Month = 12) and (Day > 28) then begin
    // the 28 DEC is always in the last week of the year
    if Week = 1 then
      Inc(Year);
  end else if (Month = 1) and (Day < 4) then
     // the 4 JAN is always in the first week of the year
    if Week <> 1 then begin
      Dec(Year);
    end;
  if _IncludeDay then begin
    DoW := DayOfTheWeek(_Date);
    Result := ToWeekDate(Year, Week, DoW);
  end else
    Result := ToWeekDate(Year, Week);
end;

function TIso8601.ToWeekDate(_Year: Integer; _Week, _DayOfWeek: Word): string;
var
  Separator: string;
begin
  if not InRange(_Week, 1, 53) then
    raise EIso8601Error.CreateFmt(_('%d is not a valid week value'), [_Week]);
  if not InRange(_DayOfWeek, 1, 7) then
    raise EIso8601Error.CreateFmt(_('%d is not a valid day of week value'), [_Week]);

  Separator := GetDateSeparator;
  Result := YearToStr(_Year) + Separator + 'W' + Long2Dec2(_Week) + Separator + IntToStr(_DayOfWeek);
end;

function TIso8601.ToWeekDate(_Year: Integer; _Week: Word): string;
var
  Separator: string;
begin
  if not InRange(_Week, 1, 53) then
    raise EIso8601Error.CreateFmt(_('%d is not a valid week value'), [_Week]);

  Separator := GetDateSeparator;
  Result := YearToStr(_Year) + Separator + 'W' + Long2Dec2(_Week);
end;

function TIso8601.YearToStr(_Year: Integer): string;
begin
  if _Year < 0 then
    Result := '-' + Long2Dec4(-_Year)
  else
    Result := Long2Dec4(_Year);
end;

function DumpIso(_DateTime: TDateTime): string;
var
  Iso: TIso8601;
begin
  Iso.Init(dsPoint);
  Result := Iso.ToCalendarDateAndTime(_DateTime, True, True);
end;

{$IFDEF DEBUG}
initialization
  DumpIso(Now);
{$ENDIF DEBUG}
{$ENDIF DELPHI2007_UP}
end.
