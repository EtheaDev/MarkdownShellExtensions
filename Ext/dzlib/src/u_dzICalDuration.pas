unit u_dzICalDuration;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  SysUtils,
  u_dzTranslator;

type
  TdzNullableDuration = record
  private
    FIsValid: IInterface;
    FWeeks: integer;
    FDays: integer;
    FHours: integer;
    FMinutes: integer;
    FSeconds: integer;
    procedure SetDays(const _Value: integer);
    procedure SetHours(const _Value: integer);
    procedure SetMinutes(const _Value: integer);
    procedure SetSeconds(const _Value: integer);
    procedure SetWeeks(const _Value: integer);
  public
    procedure Invalidate;
    function IsValid: boolean; inline;
    function GetValue(out _Value: TDateTime): boolean;
    function Dump: string;
    property Weeks: integer read FWeeks write SetWeeks;
    property Days: integer read FDays write SetDays;
    property Hours: integer read FHours write SetHours;
    property Minutes: integer read FMinutes write SetMinutes;
    property Seconds: integer read FSeconds write SetSeconds;
    function InDays: extended;
    function InHours: extended;
    function InMinutes: extended;
    function InSeconds: integer;
    class operator Explicit(_a: TdzNullableDuration): string;
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  DateUtils,
  u_dzNullableTypesUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TdzNullableDuration }

function TdzNullableDuration.Dump: string;
begin
  if IsValid then
    Result := Format('%dW%dDT%dH%dM%dS', [FWeeks, FDays, FHours, FMinutes, FSeconds]) // do not translate!
  else
    Result := '<invalid>';
end;

class operator TdzNullableDuration.Explicit(_a: TdzNullableDuration): string;
begin
  if _a.Weeks = 0 then
    Result := ''
  else if _a.Weeks = 1 then
    Result := _('1 week')
  else
    Result := Format(_('%d weeks'), [_a.Weeks]);

  if _a.Days = 0 then
  else if _a.Days = 1 then
    Result := Result + ' ' + _('1 day')
  else
    Result := Result + ' ' + Format(_('%d days'), [_a.Days]);

  if _a.Hours = 0 then
  else if _a.Hours = 1 then
    Result := Result + ' ' + _('1 hour')
  else
    Result := Result + ' ' + Format(_('%d hours'), [_a.Hours]);

  if _a.Minutes = 0 then
  else if _a.Minutes = 1 then
    Result := Result + ' ' + _('%d minutes')
  else
    Result := Result + ' ' + Format(_('%d minutes'), [_a.Minutes]);

  if _a.Seconds = 0 then
  else if _a.Seconds = 1 then
    Result := Result + ' ' + _('1 second')
  else
    Result := Result + ' ' + Format(_('%d Seconds'), [_a.Seconds]);

  Result := TrimLeft(Result);
end;

function TdzNullableDuration.GetValue(out _Value: TDateTime): boolean;
begin
  Result := IsValid;
  if Result then
    _Value := InDays;
end;

function TdzNullableDuration.InDays: extended;
begin
  if not IsValid then
    raise EInvalidValue.Create(_('NullableDuration is invalid'));
  Result := FWeeks * DaysPerWeek + FDays + FHours * OneHour + FMinutes * OneMinute + FSeconds * OneSecond;
end;

function TdzNullableDuration.InHours: extended;
begin
  Result := InDays / OneHour;
end;

function TdzNullableDuration.InMinutes: extended;
begin
  Result := InDays / OneMinute;
end;

function TdzNullableDuration.InSeconds: integer;
begin
  Result := Round(InDays / OneSecond);
end;

procedure TdzNullableDuration.Invalidate;
begin
  FIsValid := nil;
  FWeeks := 0;
  FDays := 0;
  FHours := 0;
  FMinutes := 0;
  FSeconds := 0;
end;

function TdzNullableDuration.IsValid: boolean;
begin
  Result := Assigned(FIsValid);
end;

procedure TdzNullableDuration.SetDays(const _Value: integer);
begin
  FDays := _Value;
  FIsValid := GetNullableTypesFlagInterface;
end;

procedure TdzNullableDuration.SetHours(const _Value: integer);
begin
  FHours := _Value;
  FIsValid := GetNullableTypesFlagInterface;
end;

procedure TdzNullableDuration.SetMinutes(const _Value: integer);
begin
  FMinutes := _Value;
  FIsValid := GetNullableTypesFlagInterface;
end;

procedure TdzNullableDuration.SetSeconds(const _Value: integer);
begin
  FSeconds := _Value;
  FIsValid := GetNullableTypesFlagInterface;
end;

procedure TdzNullableDuration.SetWeeks(const _Value: integer);
begin
  FWeeks := _Value;
  FIsValid := GetNullableTypesFlagInterface;
end;

{$ENDIF DELPHI2007_UP}

end.

