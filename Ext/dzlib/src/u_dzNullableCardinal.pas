unit u_dzNullableCardinal;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  u_dzTranslator,
  u_dzNullableTypesUtils,
  SysUtils;

type
  TNullableCardinal = record
  private
    FIsValid: IInterface;
    FValue: Cardinal;
  public
    procedure Invalidate;
    function Value: Cardinal;
    function IsValid: Boolean; inline;
    function GetValue(out _Value: Cardinal): Boolean;
    procedure AssignVariant(_a: Variant);
    function ToVariant: Variant;
    function Dump: string;
    class operator Add(_a, _b: TNullableCardinal): TNullableCardinal;
    class operator Implicit(_Value: Cardinal): TNullableCardinal;
    class operator Implicit(_a: TNullableCardinal): Cardinal;
    ///<summary> Converts a string to a TNullableCardinal,
    ///          empty strings result in an invalid TNullableCardinal
    ///          Strings, that are not cardinals raise an EConvertError exception. </summary>
    class operator Explicit(const _s: string): TNullableCardinal;
    class operator Explicit(_a: TNullableCardinal): string;
    class operator Add(_a: TNullableCardinal; _b: Cardinal): Cardinal;
    class operator Subtract(_a: TNullableCardinal; _b: Cardinal): Cardinal;
    class operator LessThan(_a, _b: TNullableCardinal): Boolean;
    class operator GreaterThan(_a, _b: TNullableCardinal): Boolean;
    class operator LessThanOrEqual(_a, _b: TNullableCardinal): Boolean;
    class operator GreaterThanOrEqual(_a, _b: TNullableCardinal): Boolean;
    class operator Multiply(_a: TNullableCardinal; _b: Cardinal): Cardinal;
    class operator Multiply(_a: Cardinal; _b: TNullableCardinal): Cardinal;
    /// <summary> invalid values are considered smaller than any valid values
    /// and equal to each other </summary>
    class function Compare(_a, _b: TNullableCardinal): Integer; static;
    class function Invalid: TNullableCardinal; static;
    class function FromVariant(_a: Variant): TNullableCardinal; static;
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  Variants,
  u_dzVariantUtils;

function _(const _s: string): string;
begin
  Result := dzlibGetText(_s);
end;

{ TNullableCardinal }

class operator TNullableCardinal.Add(_a, _b: TNullableCardinal): TNullableCardinal;
begin
  if not _a.IsValid or not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot add two NullableCardinal values if one of them is not valid.'));
  Result := _a.Value + _b.Value;
end;

class operator TNullableCardinal.Explicit(const _s: string): TNullableCardinal;
var
  Int: Int64;
begin
  Result.FIsValid := nil;
  if _s = '' then
    Exit;

  if not TryStrToInt64(_s, Int) or (Int shr 32 = 0) then
    raise EConvertError.CreateFmt(_('Cannot convert "%s" to %s.'), [_s, 'NullableCardinal']);

  Result.FValue := Cardinal(Int and $FFFFFFFF);
  Result.FIsValid := GetNullableTypesFlagInterface;
end;

class operator TNullableCardinal.Explicit(_a: TNullableCardinal): string;
begin
  if _a.IsValid then
    Result := IntToStr(_a.Value)
  else
    Result := '';
end;

class function TNullableCardinal.FromVariant(_a: Variant): TNullableCardinal;
begin
  Result.AssignVariant(_a);
end;

class operator TNullableCardinal.Implicit(_Value: Cardinal): TNullableCardinal;
begin
  Result.FValue := _Value;
  Result.FIsValid := GetNullableTypesFlagInterface;
end;

class operator TNullableCardinal.Implicit(_a: TNullableCardinal): Cardinal;
begin
  Result := _a.Value;
end;

class operator TNullableCardinal.Add(_a: TNullableCardinal; _b: Cardinal): Cardinal;
begin
  Result := _a.Value + _b;
end;

procedure TNullableCardinal.AssignVariant(_a: Variant);
var
  Int: Int64;
begin
  if TryVar2Int64(_a, Int) and (Int shr 32 = 0) then begin
    FValue := Cardinal(Int and $FFFFFFFF);
    FIsValid := GetNullableTypesFlagInterface
  end else
    FIsValid := nil;
end;

class function TNullableCardinal.Compare(_a, _b: TNullableCardinal): Integer;
begin
  if _a.IsValid then begin
    if _b.IsValid then
      Result := _a.Value - _b.Value
    else
      Result := 1;
  end else if not _b.IsValid then
    Result := 0
  else
    Result := -1;
end;

function TNullableCardinal.Dump: string;
begin
  if IsValid then
    Result := IntToStr(FValue)
  else
    Result := '<invalid>';
end;

function TNullableCardinal.ToVariant: Variant;
begin
  if IsValid then
    Result := Value
  else
    Result := Variants.Null;
end;

function TNullableCardinal.GetValue(out _Value: Cardinal): Boolean;
begin
  Result := IsValid;
  if Result then
    _Value := FValue;
end;

class operator TNullableCardinal.GreaterThan(_a, _b: TNullableCardinal): Boolean;
begin
  Result := _a.Value > _b.Value;
end;

class operator TNullableCardinal.GreaterThanOrEqual(_a, _b: TNullableCardinal): Boolean;
begin
  Result := _a.Value >= _b.Value;
end;

procedure TNullableCardinal.Invalidate;
begin
  FIsValid := nil;
end;

function TNullableCardinal.IsValid: Boolean;
begin
  Result := FIsValid <> nil;
end;

class operator TNullableCardinal.LessThan(_a, _b: TNullableCardinal): Boolean;
begin
  Result := _a.Value < _b.Value;
end;

class operator TNullableCardinal.LessThanOrEqual(_a, _b: TNullableCardinal): Boolean;
begin
  Result := _a.Value <= _b.Value;
end;

class operator TNullableCardinal.Multiply(_a: TNullableCardinal; _b: Cardinal): Cardinal;
begin
  Result := _a.Value * _b;
end;

class operator TNullableCardinal.Multiply(_a: Cardinal; _b: TNullableCardinal): Cardinal;
begin
  Result := _a * _b.Value;
end;

class operator TNullableCardinal.Subtract(_a: TNullableCardinal; _b: Cardinal): Cardinal;
begin
  Result := _a.Value - _b;
end;

class function TNullableCardinal.Invalid: TNullableCardinal;
begin
  Result.Invalidate;
end;

function TNullableCardinal.Value: Cardinal;
begin
  if not IsValid then
    raise EInvalidValue.Create(_('NullableCardinal value is invalid.'));
  Result := FValue;
end;

{$ENDIF DELPHI2007_UP}

end.
