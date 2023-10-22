unit u_dzNullableString;

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
  TNullableString = record
  private
    FIsValid: IInterface;
    FValue: string;
  public
    procedure Invalidate;
    function Value: string;
    function IsValid: Boolean; inline;
    function GetValue(out _Value: string): Boolean;
    procedure AssignVariant(_a: Variant);
    function ToVariant: Variant;
    function Dump: string;
    class operator Add(_a, _b: TNullableString): TNullableString;
    class operator Implicit(_Value: string): TNullableString;
    class operator Implicit(_a: TNullableString): string;
    class operator Explicit(const _s: string): TNullableString;
    class operator Explicit(_a: TNullableString): string;
    class operator Add(_a: TNullableString; _b: string): string;
    class operator LessThan(_a, _b: TNullableString): Boolean;
    class operator GreaterThan(_a, _b: TNullableString): Boolean;
    class operator LessThanOrEqual(_a, _b: TNullableString): Boolean;
    class operator GreaterThanOrEqual(_a, _b: TNullableString): Boolean;
    /// <summary> invalid values are considered smaller than any valid values
    /// and equal to each other </summary>
    class function Compare(_a, _b: TNullableString): Integer; static;
    class function Invalid: TNullableString; static;
    class function FromVariant(_a: Variant): TNullableString; static;
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  Variants,
  u_dzNullableTypesUtils,
  u_dzVariantUtils;

{ TNullableString }

class operator TNullableString.Add(_a, _b: TNullableString): TNullableString;
begin
  if not _a.IsValid or not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot add two NullableString values if one of them is not valid.'));
  Result := _a.Value + _b.Value;
end;

class operator TNullableString.Add(_a: TNullableString; _b: string): string;
begin
  Result := _a.Value + _b;
end;

procedure TNullableString.AssignVariant(_a: Variant);
begin
  if TryVar2Str(_a, FValue) then
    FIsValid := GetNullableTypesFlagInterface
  else
    FIsValid := nil;
end;

class function TNullableString.Compare(_a, _b: TNullableString): Integer;
begin
  Result := CompareStr(_a.Value, _b.Value);
end;

function TNullableString.Dump: string;
begin
  if IsValid then
    Result := FValue
  else
    Result := '*NULL*';
end;

class operator TNullableString.Explicit(_a: TNullableString): string;
begin
  if _a.IsValid then
    Result := _a.Value
  else
    Result := _a;
end;

class operator TNullableString.Explicit(const _s: string): TNullableString;
begin
  Result.FValue := _s;
  Result.FIsValid := GetNullableTypesFlagInterface;
end;

class function TNullableString.FromVariant(_a: Variant): TNullableString;
begin
  Result.AssignVariant(_a);
end;

function TNullableString.GetValue(out _Value: string): Boolean;
begin
  Result := IsValid;
  if Result then
    _Value := FValue;
end;

class operator TNullableString.GreaterThan(_a, _b: TNullableString): Boolean;
begin
  Result := _a.Value > _b.Value;
end;

class operator TNullableString.GreaterThanOrEqual(_a,
  _b: TNullableString): Boolean;
begin
  Result := _a.Value >= _b.Value;
end;

class operator TNullableString.Implicit(_Value: string): TNullableString;
begin
  Result.FValue := _Value;
  Result.FIsValid := GetNullableTypesFlagInterface;
end;

class operator TNullableString.Implicit(_a: TNullableString): string;
begin
  Result := _a.Value;
end;

class function TNullableString.Invalid: TNullableString;
begin
  Result.Invalidate;
end;

procedure TNullableString.Invalidate;
begin
  FIsValid := nil;
end;

function TNullableString.IsValid: Boolean;
begin
  Result := FIsValid <> nil;
end;

class operator TNullableString.LessThan(_a, _b: TNullableString): Boolean;
begin
  Result := _a.Value < _b.Value;
end;

class operator TNullableString.LessThanOrEqual(_a,
  _b: TNullableString): Boolean;
begin
  Result := _a.Value <= _b.Value;
end;

function TNullableString.ToVariant: Variant;
begin
  if IsValid then
    Result := Value
  else
    Result := Variants.Null;
end;

function TNullableString.Value: string;
begin
  if not IsValid then
    raise EInvalidValue.Create(_('NullableString value is invalid.'));
  Result := FValue;
end;

{$ENDIF DELPHI2007_UP}

end.

