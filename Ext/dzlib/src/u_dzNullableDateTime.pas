unit u_dzNullableDateTime;

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
  u_dzNullableDate,
  u_dzNullableTime,
  u_dzNullableTimespan;

type
  TNullableDateTime = record
  private
    FIsValid: IInterface;
    FValue: TDateTime;
  public
    procedure Invalidate;
    function Value: TDateTime;
    function Date: TNullableDate;
    function Time: TNullableTime;
    function IsValid: Boolean; inline;
    function GetValue(out _Value: TDateTime): Boolean;
    procedure AssignVariant(_a: Variant);
    ///<summary>
    /// Assigns the values from d and t.
    /// @raises EInvalidValue if one of the parameters is not valid </summary>
    procedure AssignDateAndTime(_d: TNullableDate; _t: TNullableTime);
    function ToVariant: Variant;
    function ToString: string;
    function Dump: string;
    function ForDisplay: string;
    ///<summary>
    /// Assigns a new date part to the current value, keeping the time part (or setting it 0, if
    /// there was no previous value) </summary>
    procedure AssignDate(_Value: TDateTime);
    ///<summary>
    /// Assigns a new time part to the current value, keeping the date part (or setting it 0, if
    /// there was no previous value) </summary>
    procedure AssignTime(_Value: TDateTime);
    function AssignStr(const _Value: string): Boolean;
    procedure AssignNow;
    procedure Encode(Year, Month, Day: Word; Hour, Min, Sec, MSec: Word);
    class operator Negative(_a: TNullableDateTime): TNullableDateTime;
    class operator Positive(_a: TNullableDateTime): TNullableDateTime;
//    class operator Inc(_a: TNullableDateTime): TNullableDateTime;
//    class operator Dec(_a: TNullableDateTime): TNullableDateTime;
    procedure IncSeconds(_Seconds: Integer);
    procedure IncMinutes(_Minutes: Integer);
    procedure IncHours(_Hours: Integer);
    class operator Implicit(_Value: TDateTime): TNullableDateTime;
    class operator Implicit(_a: TNullableDateTime): TDateTime;
    class operator Implicit(_a: TNullableDate): TNullableDateTime;
    class operator Explicit(const _s: string): TNullableDateTime;
    class operator Explicit(_a: TNullableDateTime): string;
    class operator LessThan(_a: TNullableDateTime; _b: TDateTime): Boolean;
    class operator LessThanOrEqual(_a: TNullableDateTime; _b: TDateTime): Boolean;
    class operator GreaterThan(_a: TNullableDateTime; _b: TDateTime): Boolean;
    class operator GreaterThanOrEqual(_a: TNullableDateTime; _b: TDateTime): Boolean;
    class operator Equal(_a: TNullableDateTime; _b: TDateTime): Boolean;
    class operator NotEqual(_a: TNullableDateTime; _b: TDateTime): Boolean;
    class operator Subtract(_a: TNullableDateTime; _b: TNullableDateTime): TNullableTimespan;

    /// <summary> invalid values are considered smaller than any valid values
    /// and equal to each other </summary>
    class function Compare(_a, _b: TNullableDateTime): Integer; static;
    class function Invalid: TNullableDateTime; static;
    class function FromDateAndTime(_d: TNullableDate; _t: TNullableTime): TNullableDateTime; static;
    class function FromVariant(_a: Variant): TNullableDateTime; static;
    class function Now: TNullableDateTime; static;
  end;

type
  TdzNullableDateTime = TNullableDateTime;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  DateUtils,
  Types,
  u_dzNullableTypesUtils,
  u_dzVariantUtils,
  u_dzDateUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TNullableDateTime }

procedure TNullableDateTime.Invalidate;
begin
  FIsValid := nil;
end;

function TNullableDateTime.IsValid: Boolean;
begin
  Result := Assigned(FIsValid);
end;

procedure TNullableDateTime.AssignDate(_Value: TDateTime);
begin
  if IsValid then
    FValue := TimeOf(FValue)
  else
    FValue := 0;
  ReplaceDate(FValue, _Value);
  FIsValid := GetNullableTypesFlagInterface;
end;

procedure TNullableDateTime.AssignDateAndTime(_d: TNullableDate; _t: TNullableTime);
begin
  if _d.IsValid and _t.IsValid then
    Encode(_d.Year, _d.Month.Number, _d.Day.Number, _t.Hour, _t.Minutes, _t.Seconds, _t.MSecs)
  else
    Invalidate;
end;

procedure TNullableDateTime.AssignNow;
begin
  Self := TNullableDateTime.Now;
end;

function TNullableDateTime.AssignStr(const _Value: string): Boolean;
begin
  Result := TryStr2DateTime(_Value, FValue);
  if Result then
    FIsValid := GetNullableTypesFlagInterface
  else
    FIsValid := nil;
end;

procedure TNullableDateTime.AssignTime(_Value: TDateTime);
begin
  if IsValid then
    FValue := DateOf(FValue)
  else
    FValue := 0;
  ReplaceTime(FValue, _Value);
  FIsValid := GetNullableTypesFlagInterface;
end;

procedure TNullableDateTime.AssignVariant(_a: Variant);
begin
  if TryVar2DateTime(_a, FValue) then
    FIsValid := GetNullableTypesFlagInterface
  else
    FIsValid := nil;
end;

function TNullableDateTime.ToString: string;
begin
  Result := string(Self);
end;

function TNullableDateTime.ToVariant: Variant;
begin
  if IsValid then
    Result := Value
  else
    Result := Variants.Null;
end;

class function TNullableDateTime.Compare(_a, _b: TNullableDateTime): Integer;
begin
  Result := DateUtils.CompareDateTime(_a, _b);
end;

function TNullableDateTime.Date: TNullableDate;
begin
  if IsValid then
    Result := Trunc(FValue)
  else
    Result.Invalidate;
end;

function TNullableDateTime.Time: TNullableTime;
begin
  if IsValid then
    Result := Frac(FValue)
  else
    Result.Invalidate;
end;

function TNullableDateTime.Dump: string;
begin
  if IsValid then
    Result := string(Self)
  else
    Result := '<invalid>';
end;

class operator TNullableDateTime.Explicit(_a: TNullableDateTime): string;
begin
  Result := DateTimeToStr(_a.Value);
end;

class operator TNullableDateTime.Explicit(const _s: string): TNullableDateTime;
begin
  Result.AssignStr(_s);
end;

function TNullableDateTime.ForDisplay: string;
begin
  Result := DateTimeToStr(Value);
end;

class function TNullableDateTime.FromDateAndTime(_d: TNullableDate; _t: TNullableTime): TNullableDateTime;
begin
  Result.AssignDateAndTime(_d, _t);
end;

class function TNullableDateTime.FromVariant(_a: Variant): TNullableDateTime;
begin
  Result.AssignVariant(_a);
end;

function TNullableDateTime.GetValue(out _Value: TDateTime): Boolean;
begin
  Result := IsValid;
  if Result then
    _Value := FValue;
end;

class operator TNullableDateTime.LessThan(_a: TNullableDateTime; _b: TDateTime): Boolean;
begin
  Result := (DateUtils.CompareDateTime(_a.Value, _b) = LessThanValue);
end;

class operator TNullableDateTime.LessThanOrEqual(_a: TNullableDateTime; _b: TDateTime): Boolean;
begin
  Result := (DateUtils.CompareDateTime(_a.Value, _b) <> GreaterThanValue);
end;

procedure TNullableDateTime.Encode(Year, Month, Day, Hour, Min, Sec, MSec: Word);
var
  DatePart: TDateTime;
  TimePart: TDateTime;
begin
  if TryEncodeDate(Year, Month, Day, DatePart) and TryEncodeTime(Hour, Min, Sec, MSec, TimePart) then
    FIsValid := GetNullableTypesFlagInterface
  else
    FIsValid := nil;
  if IsValid then begin
    FValue := Trunc(DatePart) + Frac(TimePart);
  end;
end;

class operator TNullableDateTime.Equal(_a: TNullableDateTime; _b: TDateTime): Boolean;
begin
  Result := (DateUtils.CompareDateTime(_a.Value, _b) = EqualsValue);
end;

class operator TNullableDateTime.NotEqual(_a: TNullableDateTime; _b: TDateTime): Boolean;
begin
  Result := (DateUtils.CompareDateTime(_a.Value, _b) <> EqualsValue);
end;

class function TNullableDateTime.Now: TNullableDateTime;
begin
  Result := SysUtils.Now;
end;

class operator TNullableDateTime.GreaterThan(_a: TNullableDateTime; _b: TDateTime): Boolean;
begin
  Result := (DateUtils.CompareDateTime(_a.Value, _b) = GreaterThanValue);
end;

class operator TNullableDateTime.GreaterThanOrEqual(_a: TNullableDateTime; _b: TDateTime): Boolean;
begin
  Result := (DateUtils.CompareDateTime(_a.Value, _b) <> LessThanValue);
end;

class operator TNullableDateTime.Implicit(_a: TNullableDateTime): TDateTime;
begin
  Result := _a.Value;
end;

class operator TNullableDateTime.Implicit(_Value: TDateTime): TNullableDateTime;
begin
  Result.FValue := _Value;
  Result.FIsValid := GetNullableTypesFlagInterface;
end;

class operator TNullableDateTime.Implicit(_a: TNullableDate): TNullableDateTime;
begin
  if _a.IsValid then begin
    Result.FValue := _a;
    Result.FIsValid := GetNullableTypesFlagInterface;
  end else
    Result.Invalidate;
end;

procedure TNullableDateTime.IncHours(_Hours: Integer);
begin
  FValue := Value + OneHour * _Hours;
end;

procedure TNullableDateTime.IncMinutes(_Minutes: Integer);
begin
  FValue := Value + OneMinute * _Minutes;
end;

procedure TNullableDateTime.IncSeconds(_Seconds: Integer);
begin
  FValue := Value + OneSecond * _Seconds;
end;

class function TNullableDateTime.Invalid: TNullableDateTime;
begin
  Result.Invalidate;
end;

class operator TNullableDateTime.Negative(_a: TNullableDateTime): TNullableDateTime;
begin
  Result := -_a.Value;
end;

class operator TNullableDateTime.Positive(_a: TNullableDateTime): TNullableDateTime;
begin
  Result := _a.Value;
end;

class operator TNullableDateTime.Subtract(_a: TNullableDateTime;
  _b: TNullableDateTime): TNullableTimespan;
begin
  Result.AssignDays(_a.Value - _b.Value);
end;

function TNullableDateTime.Value: TDateTime;
begin
  if not IsValid then
    raise EInvalidValue.Create(_('NullableDateTime is invalid'));
  Result := FValue;
end;

{$ENDIF DELPHI2007_UP}

end.
