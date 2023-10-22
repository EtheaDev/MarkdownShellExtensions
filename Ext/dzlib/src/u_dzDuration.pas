{.GXFormatter.config=twm}
unit u_dzDuration;

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
  u_dzTypes,
  u_dzNullableTypesUtils;

type
  // todo: consolidate with TNullableTimespan
  TdzDuration = record
  private
    F1000thSec: Int64;
    ///<summary>
    /// This field uses a trick described in
    /// https://web.archive.org/web/20110820220749/http://blogs.embarcadero.com/abauer/2008/09/18/38869
    /// Since this is an interface, the compiler enforces it to be NIL whenever a new instance of this
    /// record is created. See start of implementation in u_dzNullableTypesUtils for the hack
    /// that makes this a little bit more efficient. </summary>
    FIsValid: IInterface;
    procedure AssignValue(_1000thSec: Int64); inline;
    function Value: Int64;
  public
    procedure AssignZero;
    procedure AssignDays(_Value: Extended);
    procedure AssignHours(_Value: Extended);
    procedure AssignMinutes(_Value: Extended);
    procedure AssignSeconds(_Value: Extended);
    function IsValid: Boolean; inline;
    procedure Invalidate;
    function InSeconds: Extended;
    function InMinutes: Extended;
    function InHours: Extended;
    function InHoursStr: string;
    function InDays: Extended;
    class function Zero: TdzDuration; static;
    class function OneSecond: TdzDuration; static;
    class function OneMinute: TdzDuration; static;
    class function OneHour: TdzDuration; static;
    class function OneDay: TdzDuration; static;
    class function Invalid: TdzDuration; static;
    class function FromDays(_Value: Extended): TdzDuration; static;
    class function FromHours(_Value: Extended): TdzDuration; static;
    class function FromMinutes(_Value: Extended): TdzDuration; static;
    class function FromSeconds(_Value: Extended): TdzDuration; static;
//    class operator GreaterThan(_a, _b: TdzDuration): boolean; inline;
//    class operator GreaterThanOrEqual(_a, _b: TdzDuration): boolean; inline;
//    class operator LessThanOrEqual(const _a, _b: TdzDuration): boolean; inline;
    class operator LessThan(_a, _b: TdzDuration): Boolean; inline;
    class operator NotEqual(_a, _b: TdzDuration): Boolean; inline;
    class operator Equal(_a, _b: TdzDuration): Boolean; inline;
    class operator Add(_a, _b: TdzDuration): TdzDuration; inline;
    class operator Subtract(_a, _b: TdzDuration): TdzDuration; inline;
//    class operator Divide(_a, _b: TdzDuration): extended; inline;
//    class operator Divide(_a: TdzDuration; _Divisor: extended): TdzDuration; inline;
//    class operator Multiply(_a: TdzDuration; _Factor: Extended): TdzDuration; inline;
//    class operator Negative(_a: TdzDuration): TdzDuration; inline;
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  SysConst,
  StrUtils,
  DateUtils,
  u_dzStringUtils,
  u_dzDateUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TdzDuration }

class operator TdzDuration.Add(_a, _b: TdzDuration): TdzDuration;
begin
  Result.AssignValue(_a.Value + _b.Value);
end;

procedure TdzDuration.AssignDays(_Value: Extended);
begin
  AssignValue(Round(_Value * 24 * 60 * 60 * 1000));
end;

procedure TdzDuration.AssignHours(_Value: Extended);
begin
  AssignValue(Round(_Value * 60 * 60 * 1000));
end;

procedure TdzDuration.AssignMinutes(_Value: Extended);
begin
  AssignValue(Round(_Value * 60 * 1000));
end;

procedure TdzDuration.AssignSeconds(_Value: Extended);
begin
  AssignValue(Round(_Value * 1000));
end;

procedure TdzDuration.AssignValue(_1000thSec: Int64);
begin
  F1000thSec := _1000thSec;
  FIsValid := GetNullableTypesFlagInterface;
end;

procedure TdzDuration.AssignZero;
begin
  AssignValue(0);
end;

class operator TdzDuration.Equal(_a, _b: TdzDuration): Boolean;
begin
  Result := _a.Value = _b.Value;
end;

class function TdzDuration.FromDays(_Value: Extended): TdzDuration;
begin
  Result.AssignDays(_Value);
end;

class function TdzDuration.FromHours(_Value: Extended): TdzDuration;
begin
  Result.AssignHours(_Value);
end;

class function TdzDuration.FromMinutes(_Value: Extended): TdzDuration;
begin
  Result.AssignMinutes(_Value);
end;

class function TdzDuration.FromSeconds(_Value: Extended): TdzDuration;
begin
  Result.AssignSeconds(_Value);
end;

function TdzDuration.InDays: Extended;
begin
  Result := Value / 24 / 60 / 60 / 1000;
end;

function TdzDuration.InHours: Extended;
begin
  Result := Value / 60 / 60 / 1000;
end;

function TdzDuration.InHoursStr: string;
var
  Minutes: Int64;
begin
  Minutes := Round(InMinutes);
  if Minutes < 0 then
    Result := Format('-%dh%.2d', [-Minutes div 60, -Minutes mod 60])
  else
    Result := Format('%dh%.2d', [Minutes div 60, Minutes mod 60]);
end;

function TdzDuration.InMinutes: Extended;
begin
  Result := Value / 60 / 1000;

end;

function TdzDuration.InSeconds: Extended;
begin
  Result := Value / 1000;
end;

class function TdzDuration.Invalid: TdzDuration;
begin
  Result.Invalidate;
end;

procedure TdzDuration.Invalidate;
begin
  FIsValid := nil;
end;

function TdzDuration.IsValid: Boolean;
begin
  Result := Assigned(FIsValid);
end;

class operator TdzDuration.LessThan(_a, _b: TdzDuration): Boolean;
begin
  Result := (_a.Value < _b.Value);
end;

class operator TdzDuration.NotEqual(_a, _b: TdzDuration): Boolean;
begin
  Result := (_a.Value <> _b.Value);
end;

class function TdzDuration.OneDay: TdzDuration;
begin
  Result.AssignValue(24 * 60 * 60 * 1000);
end;

class function TdzDuration.OneHour: TdzDuration;
begin
  Result.AssignValue(60 * 60 * 1000);
end;

class function TdzDuration.OneMinute: TdzDuration;
begin
  Result.AssignValue(60 * 1000);
end;

class function TdzDuration.OneSecond: TdzDuration;
begin
  Result.AssignValue(1000);
end;

class operator TdzDuration.Subtract(_a, _b: TdzDuration): TdzDuration;
begin
  Result.AssignValue(_a.Value - _b.Value);
end;

function TdzDuration.Value: Int64;
begin
  if not IsValid then
    raise EdzDateUtilsException.Create(_('TdzDuration value is invalid.'));
  Result := F1000thSec;
end;

class function TdzDuration.Zero: TdzDuration;
begin
  Result.AssignZero;
end;

{$ENDIF DELPHI2007_UP}

end.
