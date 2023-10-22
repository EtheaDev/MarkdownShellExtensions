unit u_dzNullableDate;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  SysUtils,
  Variants,
  u_dzTranslator,
  u_dzTypes,
  u_dzDateUtils;

type
  TdzDayOfWeek = record
  public
    enum: TDayOfWeekEnum;
    function AsString: string;
  end;

type
  TdzDay = record
  private
    FDayOfWeek: TDayOfWeekEnum;
  public
    Number: TDayOfMonthNumbers;
    ///<summary> This cannot just be a field because of an apparent compiler bug </summary>
    function DayOfWeek: TdzDayOfWeek;
    procedure Init(_Number: TDayOfMonthNumbers; _DOW: TDayOfWeekEnum);
  end;

type
  TdzMonth = record
  public
    Number: TMonthNumbers;
    function AsString: string;
  end;

type
  TNullableDate = record
  private
    FIsValid: IInterface;
    FValue: TDateTime;
  public
    procedure Invalidate;
    function Value: TDateTime;
    function IsValid: Boolean;
    procedure CheckIsValid(const _ErrorMsg: string);
    function GetValue(out _Value: TDateTime): Boolean;
    procedure Assign(_Value: TNullableDate);
    ///<summary>
    /// Assigns a variant representation of a date
    /// @param v is the variant to assign
    /// @param ErrorHandling determines if an invalid parameter raises an exception or
    ///                       just makes the dzNullableDate invalid.
    /// @returns true, if the variant was a valid date, false otherwise
    /// if v is NULL, the dzNullableDate will be invalid afterwards </summary>
    function AssignVariant(_v: Variant; _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean;
    function ToVariant: Variant;
    ///<summary>
    /// Assigns a string represantation of a date in ISO 8601 format
    /// @param s is the string to assign
    /// @param InvalidStr is considered an invalid date
    /// @param ErrorHandling determines if an invalid parameter raises an exception or
    ///                       just makes the dzNullableDate invalid.
    /// @raises EConvertError if s <> InvalidStr but cannot be converted </summary>
    /// @returns true, if the string was a valid date, false otherwise
    /// if s is an empty string, the dzNullableDate will be invalid afterwards </summary>
    function AssignIso(const _s: string; const _InvalidStr: string;
      _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean; overload;
    function AssignIso(const _s: string; _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean; overload;
    ///<summary>
    /// Assigns a date string in German (european?) format dd.mm.yyyy
    /// @param s is the string to assign
    /// @param InvalidStr is considered an invalid date
    /// @param ErrorHandling determines if an invalid parameter raises an exception or
    ///                       just makes the dzNullableDate invalid.
    /// @raises EConvertError if s <> InvalidStr but cannot be converted </summary>
    /// @returns true, if the string was a valid date, false otherwise
    /// if s is an empty string, the dzNullableDate will be invalid afterwards </summary>
    function AssignDDMMYYYY(const _s: string; const _InvalidStr: string = '';
      _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean;
    ///<summary>
    /// Tries to assign a date string. Date formats are tried in the following order:
    /// * format configured in Windows
    /// * German dd.mm.yyyy
    /// * ISO 8601 (yyyy-mm-dd)
    /// * United Kingdom: dd/mm/yyyy
    /// (I had to decide between the sane UK format or the brain dead US format, i chose the UK format.)
    /// @param s is the string to assign
    /// @param InvalidStr is considered an invalid date
    /// @param ErrorHandling determines if an invalid parameter raises an exception or
    ///                       just makes the dzNullableDate invalid.
    /// @raises EConvertError if s <> InvalidStr but cannot be converted </summary>
    /// @returns true, if the string was a valid date, false otherwise
    /// if s is an empty string, the dzNullableDate will be invalid afterwards </summary>
    function AssignStr(const _s: string; const _InvalidStr: string = '';
      _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean;
    ///<summary>
    /// Tries to assign a date string in the brain dead US format (mm/dd/(yy)yy)
    /// @param s is the string to assign
    /// @param InvalidStr is considered an invalid date
    /// @raises EConvertError if s <> InvalidStr but cannot be converted </summary>
//    procedure AssignBraindeadUS(const _s: string; const _InvalidStr: string = '');
    procedure Encode(_Year, _Month, _Day: Word);
    procedure Decode(out _Year, _Month, _Day: Word);
    ///<summary>
    /// Change only the year </summary>
    procedure SetYear(_Value: Word);
    ///<summary>
    /// Change only the month </summary>
    procedure SetMonth(_Value: Word);
    ///<summary>
    /// Change only the day </summary>
    procedure SetDay(_Value: Word);
    ///<summary>
    /// returns true if the Month and Day matches month and day of the current value </summary>
    function IsDate(_Month, _Day: Word): Boolean; overload;
    ///<summary>
    /// returns true if the Year, Month and Day matches year, month and day the current value </summary>
    function IsDate(_Year, _Month, _Day: Word): Boolean; overload;
    function Year: Word;
    function Month: TdzMonth;
    function Day: TdzDay;
    ///<summary>
    /// @returns the day of the year of the current date value </summary>
    function DayOfTheYear: Word;
    ///<summary>
    /// @returns the week of the year of the current date value
    /// Note that this week might still belong to the previous year.
    ///       (e.g. 2017-01-01 belonged week 53 of year 2016)
    ///       To get the year to which it belongs, use the overloaded function
    ///       that also returns the year. </summary>
    function WeekOfTheYear: Word; overload;
    ///<summary>
    /// @param Year is the year to which the week belongs (e.g. 2017-01-01 belonged week 53 of
    ///             year 2016)
    /// @returns the week of the year of the current date value. </summary>
    function WeekOfTheYear(out _Year: Word): Word; overload;
    function Dump: string;
    function ForDisplay: string; overload;
    function ForDisplay(const _Default: string): string; overload;
    ///<summary>
    /// @returns the date in DD.mm.YYYY format if valid
    /// @raises an EInvalidValue exception if not. </summary>
    function ToDDmmYYYY: string; overload;
    ///<summary>
    /// Returns the date in DD.mm.YYYY format if valid, or the Default if not</summary>
    function ToDDmmYYYY(const _Default: string): string; overload;
    ///<summary>
    /// @returns the date in YYYY-mm-DD format if valid
    /// @raises an EInvalidValue exception if not. </summary>
    function ToYYYYmmDD: string; overload;
    ///<summary>
    /// Returns the date in YYYY-mm-DD format if valid, or the Default if not</summary>
    function ToYYYYmmDD(const _Default: string): string; overload;
    ///<summary>
    /// @returns #MM/DD/YYYY# for using in a query in MS-Access (when everything else fails) </summary>
    function ToMsAccessLiteral: string;
    ///<summary>
    /// Converts the current date to a string using the given format.
    /// @Format is a string with the same definition as in DateTimeToString </summary>
    function ToStringFmt(_Format: string): string;
    procedure AddDays(_Value: Integer); deprecated; // use IncDay instead
    procedure AddMonths(_Value: Integer); deprecated; // use IncMonth instead
    procedure AddYears(_Value: Integer); deprecated; // use IncYear instead
    ///<summary>
    /// Increments the current day and return Self </summary>
    function IncDay(_by: Integer): TNullableDate;
    ///<summary>
    /// Increments the current month and return Self </summary>
    function IncMonth(_by: Integer): TNullableDate;
    ///<summary>
    /// Increments the current year and return Self </summary>
    function IncYear(_by: Integer): TNullableDate;
    ///<summary>
    /// Decrements the current day and return Self </summary>
    function DecDay(_by: Integer): TNullableDate;
    ///<summary>
    /// Decrements the current month and return Self </summary>
    function DecMonth(_by: Integer): TNullableDate;
    ///<summary>
    /// Decrements the current year and return Self </summary>
    function DecYear(_by: Integer): TNullableDate;
    ///<summary>
    /// Converts the date part of a TDateTime to a TNullableDate
    /// NOTE: I'm not a great fan of Implicit conversions as they are not discoverable
    /// See also FromDateTime </summary>
    class operator Implicit(_Value: TDateTime): TNullableDate;
    ///<summary>
    /// Converts the date part of a TDateTime to a TNullableDate </summary>
    class function FromDateTime(_Value: TDateTime): TNullableDate; static;
    // Unfortunately TDate is declared in Controls and since I don't want this to depend
    // on that unit, I can't implement conversion to and from TDate.
    class operator Implicit(_a: TNullableDate): TDateTime;
    ///<summary> Converts a string to an TNullableDate,
    ///          empty strings result in an invalid TNullableDate
    ///          Strings, that are not dates raise an EConvertError exception. </summary>
    class operator Explicit(const _s: string): TNullableDate;
    ///<summary>
    /// Converts to a string, if invalid, the result is an empty string </summary>
    class operator Explicit(_a: TNullableDate): string;
    class function FromVariant(_v: Variant): TNullableDate; static;
    class operator NotEqual(_a, _b: TNullableDate): Boolean;
    class operator Equal(_a, _b: TNullableDate): Boolean;
    class operator GreaterThan(_a, _b: TNullableDate): Boolean;
    class operator GreaterThanOrEqual(_a, _b: TNullableDate): Boolean;
    class operator LessThan(_a, _b: TNullableDate): Boolean;
    class operator LessThanOrEqual(_a, _b: TNullableDate): Boolean;
    class operator Add(_Date: TNullableDate; _Days: Integer): TNullableDate;
    class operator Subtract(_Date: TNullableDate; _Days: Integer): TNullableDate;
    class function Today: TNullableDate; static;
    class function Tomorrow: TNullableDate; static;
    class function Yesterday: TNullableDate; static;
    ///<summary>
    /// Initializes a new TNullableDate from the given values and returns it </summary>
    class function EncodeFrom(_Year, _Month, _Day: Word): TNullableDate; static;
    ///<summary>
    /// Returns an invalid TNullableDate </summary>
    class function Invalid: TNullableDate; static;
    ///<summary>
    /// Compares two TNullableDate values
    /// @returns -1, if Value1 < Value2, 0 if they are the same, 1 if Value1 > Value2
    /// @raises EInvalidValue if one of the values is not valid </summary>
    class function Compare(_Value1, _Value2: TNullableDate): Integer; static;
    class function FromString(const _s: string): TNullableDate; static;
  end;

type
  TdzNullableDate = TNullableDate deprecated;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  SysConst,
  DateUtils,
  Math,
  u_dzNullableTypesUtils,
  u_dzVariantUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TdzDayOfWeek }

function TdzDayOfWeek.AsString: string;
begin
  Result := u_dzDateUtils.DayOfWeek2Str(enum);
end;

{ TdzDay }

function TdzDay.DayOfWeek: TdzDayOfWeek;
begin
  Result.enum := FDayOfWeek;
end;

procedure TdzDay.Init(_Number: TDayOfMonthNumbers; _DOW: TDayOfWeekEnum);
begin
  Number := _Number;
  FDayOfWeek := _DOW;
end;

{ TdzMonth }

function TdzMonth.AsString: string;
begin
  Result := u_dzDateUtils.Month2Str(Number);
end;

{ TNullableDate }

class function TNullableDate.FromString(const _s: string): TNullableDate;
begin
  Result.AssignStr(_s, '');
end;

class operator TNullableDate.Explicit(const _s: string): TNullableDate;
begin
  Result.AssignStr(_s, '');
end;

class operator TNullableDate.Add(_Date: TNullableDate; _Days: Integer): TNullableDate;
begin
  Result := _Date.Value + _Days;
end;

procedure TNullableDate.AddDays(_Value: Integer);
begin
  FValue := DateUtils.IncDay(Value, _Value);
end;

procedure TNullableDate.AddMonths(_Value: Integer);
begin
  FValue := SysUtils.IncMonth(Value, _Value);
end;

procedure TNullableDate.AddYears(_Value: Integer);
begin
  FValue := DateUtils.IncYear(Value, _Value);
end;

procedure TNullableDate.Assign(_Value: TNullableDate);
begin
  if _Value.IsValid then begin
    FValue := _Value.Value;
    FIsValid := GetNullableTypesFlagInterface;
  end else
    FIsValid := nil;
end;

function TNullableDate.AssignDDMMYYYY(const _s: string; const _InvalidStr: string = '';
  _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean;
begin
  Result := False;
  Invalidate;
  if _s <> _InvalidStr then begin
    Result := Tryddmmyyyy2Date(_s, FValue);
    if Result then
      FIsValid := GetNullableTypesFlagInterface
    else begin
      if (_ErrorHandling = ehRaiseException) then begin
{$TYPEDADDRESS OFF}
        raise EConvertError.CreateResFmt(@SInvalidDate, [_s]);
{$IFDEF TYPEDADDRESS_IS_ON}{$TYPEDADDRESS ON}{$ENDIF}
      end;
    end;
  end;
end;

function TNullableDate.AssignIso(const _s: string; _ErrorHandling: TErrorHandlingEnum): Boolean;
begin
  Invalidate;
  Result := TryIso2Date(_s, FValue);
  if Result then
    FIsValid := GetNullableTypesFlagInterface
  else begin
    if (_ErrorHandling = ehRaiseException) then begin
{$TYPEDADDRESS OFF}
      raise EConvertError.CreateResFmt(@SInvalidDate, [_s]);
{$IFDEF TYPEDADDRESS_IS_ON}{$TYPEDADDRESS ON}{$ENDIF}
    end;
  end;
end;

function TNullableDate.AssignIso(const _s: string; const _InvalidStr: string;
  _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean;
begin
  Result := False;
  Invalidate;
  if _s <> _InvalidStr then begin
    Result := TryIso2Date(_s, FValue);
    if Result then
      FIsValid := GetNullableTypesFlagInterface
    else begin
      if (_ErrorHandling = ehRaiseException) then begin
{$TYPEDADDRESS OFF}
        raise EConvertError.CreateResFmt(@SInvalidDate, [_s]);
{$IFDEF TYPEDADDRESS_IS_ON}{$TYPEDADDRESS ON}{$ENDIF}
      end;
    end;
  end;
end;

function TNullableDate.AssignStr(const _s: string; const _InvalidStr: string = '';
  _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean;
begin
  Result := False;
  Invalidate;
  if _s <> _InvalidStr then begin
    Result := TryStr2Date(_s, FValue);
    if Result then
      FIsValid := GetNullableTypesFlagInterface
    else begin
      if (_ErrorHandling = ehRaiseException) then begin
{$TYPEDADDRESS OFF}
        raise EConvertError.CreateResFmt(@SInvalidDate, [_s]);
{$IFDEF TYPEDADDRESS_IS_ON}{$TYPEDADDRESS ON}{$ENDIF}
      end;
    end;
  end;
end;

function TNullableDate.AssignVariant(_v: Variant;
  _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean;
begin
  Result := False;
  Invalidate;
  if VarIsNull(_v) or VarIsEmpty(_v) then
    Exit;
  Result := TryVar2DateTime(_v, FValue);
  if Result then begin
    FValue := DateOf(FValue);
    FIsValid := GetNullableTypesFlagInterface
  end else begin
    if (_ErrorHandling = ehRaiseException) then begin
{$TYPEDADDRESS OFF}
      raise EConvertError.CreateResFmt(@SInvalidDate, [Var2Str(_v)]);
{$IFDEF TYPEDADDRESS_IS_ON}{$TYPEDADDRESS ON}{$ENDIF}
    end;
  end;
end;

class function TNullableDate.Compare(_Value1, _Value2: TNullableDate): Integer;
begin
  Result := Math.CompareValue(_Value1.Value, _Value2.Value);
end;

function TNullableDate.Day: TdzDay;
var
  Year, Month, TheDay: Word;
begin
  Decode(Year, Month, TheDay);
  Result.Init(TheDay, u_dzDateUtils.GetDayOfTheWeek(Value));
end;

procedure TNullableDate.Decode(out _Year, _Month, _Day: Word);
begin
  DecodeDate(Value, _Year, _Month, _Day);
end;

function TNullableDate.DecDay(_by: Integer): TNullableDate;
begin
  FValue := DateUtils.IncDay(Value, -_by);
  Result := Self;
end;

function TNullableDate.DecMonth(_by: Integer): TNullableDate;
begin
  FValue := SysUtils.IncMonth(Value, -_by);
  Result := Self;
end;

function TNullableDate.DecYear(_by: Integer): TNullableDate;
begin
  FValue := DateUtils.IncYear(Value, -_by);
  Result := Self;
end;

function TNullableDate.Dump: string;
begin
  if IsValid then
    Result := DateTime2Iso(FValue)
  else
    Result := '<invalid>';
end;

procedure TNullableDate.Encode(_Year, _Month, _Day: Word);
begin
  if TryEncodeDate(_Year, _Month, _Day, FValue) then
    FIsValid := GetNullableTypesFlagInterface
  else
    FIsValid := nil;
end;

class function TNullableDate.EncodeFrom(_Year, _Month, _Day: Word): TNullableDate;
begin
  Result.Encode(_Year, _Month, _Day);
end;

class operator TNullableDate.Explicit(_a: TNullableDate): string;
begin
  if _a.IsValid then
    Result := _a.ToYYYYmmDD('')
  else
    Result := '';
end;

function TNullableDate.ForDisplay: string;
begin
  Result := DateToStr(Value)
end;

function TNullableDate.ForDisplay(const _Default: string): string;
begin
  if IsValid then
    Result := DateToStr(Value)
  else
    Result := _Default;
end;

class function TNullableDate.FromVariant(_v: Variant): TNullableDate;
begin
  Result.AssignVariant(_v);
end;

class function TNullableDate.FromDateTime(_Value: TDateTime): TNullableDate;
begin
  Result.FValue := DateOf(_Value);
  if _Value <> 0 then begin
    Result.FIsValid := GetNullableTypesFlagInterface
  end else
    Result.FIsValid := nil;
end;

class operator TNullableDate.Implicit(_Value: TDateTime): TNullableDate;
begin
  Result := FromDateTime(_Value);
end;

class operator TNullableDate.Implicit(_a: TNullableDate): TDateTime;
begin
  Result := DateOf(_a.Value);
end;

function TNullableDate.GetValue(out _Value: TDateTime): Boolean;
begin
  Result := IsValid;
  if Result then
    _Value := DateOf(FValue);
end;

class operator TNullableDate.GreaterThan(_a, _b: TNullableDate): Boolean;
begin
  Result := _a.Value > _b.Value;
end;

class operator TNullableDate.GreaterThanOrEqual(_a, _b: TNullableDate): Boolean;
begin
  Result := _a.Value >= _b.Value;
end;

function TNullableDate.IncDay(_by: Integer): TNullableDate;
begin
  FValue := DateUtils.IncDay(Value, _by);
  Result := Self;
end;

function TNullableDate.IncMonth(_by: Integer): TNullableDate;
begin
  FValue := SysUtils.IncMonth(Value, _by);
  Result := Self;
end;

function TNullableDate.IncYear(_by: Integer): TNullableDate;
begin
  FValue := DateUtils.IncYear(Value, _by);
  Result := Self;
end;

class function TNullableDate.Invalid: TNullableDate;
begin
  Result.Invalidate;
end;

procedure TNullableDate.Invalidate;
begin
  FIsValid := nil;
end;

function TNullableDate.IsDate(_Month, _Day: Word): Boolean;
var
  Year: Word;
  Month: Word;
  Day: Word;
begin
  Decode(Year, Month, Day);
  Result := (Month = _Month) and (Day = _Day)
end;

function TNullableDate.IsDate(_Year, _Month, _Day: Word): Boolean;
var
  Year: Word;
  Month: Word;
  Day: Word;
begin
  Decode(Year, Month, Day);
  Result := (Year = _Year) and (Month = _Month) and (Day = _Day);
end;

function TNullableDate.IsValid: Boolean;
begin
  Result := FIsValid <> nil;
end;

procedure TNullableDate.CheckIsValid(const _ErrorMsg: string);
begin
  if not IsValid then
    raise EInvalidValue.Create(_ErrorMsg);
end;

class operator TNullableDate.LessThan(_a, _b: TNullableDate): Boolean;
begin
  Result := _a.Value < _b.Value;
end;

class operator TNullableDate.LessThanOrEqual(_a, _b: TNullableDate): Boolean;
begin
  Result := _a.Value <= _b.Value;
end;

function TNullableDate.Month: TdzMonth;
var
  Year, TheMonth, Day: Word;
begin
  Decode(Year, TheMonth, Day);
  Result.Number := TheMonth;
end;

class operator TNullableDate.NotEqual(_a, _b: TNullableDate): Boolean;
begin
  Result := _a.Value <> _b.Value;
end;

procedure TNullableDate.SetDay(_Value: Word);
var
  Year: Word;
  Month: Word;
  Day: Word;
begin
  Decode(Year, Month, Day);
  Encode(Year, Month, _Value);
end;

procedure TNullableDate.SetMonth(_Value: Word);
var
  Year: Word;
  Month: Word;
  Day: Word;
begin
  Decode(Year, Month, Day);
  Encode(Year, _Value, Day);
end;

procedure TNullableDate.SetYear(_Value: Word);
var
  Year: Word;
  Month: Word;
  Day: Word;
begin
  Decode(Year, Month, Day);
  Encode(_Value, Month, Day);
end;

class operator TNullableDate.Subtract(_Date: TNullableDate; _Days: Integer): TNullableDate;
begin
  Result := _Date.Value - _Days;
end;

class operator TNullableDate.Equal(_a, _b: TNullableDate): Boolean;
begin
  Result := _a.Value = _b.Value;
end;

class function TNullableDate.Today: TNullableDate;
begin
  Result := SysUtils.Date;
end;

function TNullableDate.ToDDmmYYYY(const _Default: string): string;
begin
  if IsValid then
    Result := Date2ddmmyyyy(Value)
  else
    Result := _Default;
end;

class function TNullableDate.Tomorrow: TNullableDate;
begin
  Result := Today.IncDay(1);
end;

function TNullableDate.ToMsAccessLiteral: string;
begin
  Result := ToStringFmt('"#"mm"/"dd"/"yyyy"#"');
end;

function TNullableDate.ToDDmmYYYY: string;
begin
  Result := Date2ddmmyyyy(Value);
end;

function TNullableDate.ToStringFmt(_Format: string): string;
begin
  DateTimeToString(Result, _Format, Value); // do not translate
end;

function TNullableDate.ToVariant: Variant;
begin
  if IsValid then
    Result := FValue
  else
    Result := Variants.Null;
end;

function TNullableDate.ToYYYYmmDD(const _Default: string): string;
begin
  if IsValid then
    Result := DateTime2Iso(Value)
  else
    Result := _Default;
end;

function TNullableDate.ToYYYYmmDD: string;
begin
  Result := DateTime2Iso(Value);
end;

function TNullableDate.Value: TDateTime;
begin
  if not IsValid then
    raise EInvalidValue.Create(_('NullableDate value is invalid'));
  Result := DateOf(FValue);
end;

function TNullableDate.DayOfTheYear: Word;
begin
  Result := DateUtils.DayOfTheYear(FValue);
end;

function TNullableDate.WeekOfTheYear(out _Year: Word): Word;
begin
  Result := DateUtils.WeekOfTheYear(FValue, _Year);
end;

function TNullableDate.WeekOfTheYear: Word;
var
  TheYear: Word;
begin
  Result := WeekOfTheYear(TheYear);
end;

function TNullableDate.Year: Word;
var
  Month, Day: Word;
begin
  Decode(Result, Month, Day);
end;

class function TNullableDate.Yesterday: TNullableDate;
begin
  Result := Today.DecDay(1);
end;

{$IFDEF debug}

procedure ForceLinkerToIncludeDump;
var
  d: TNullableDate;
begin
  d.Dump;
end;

initialization
  ForceLinkerToIncludeDump;
{$ENDIF debug}
{$ENDIF DELPHI2007_UP}
end.
