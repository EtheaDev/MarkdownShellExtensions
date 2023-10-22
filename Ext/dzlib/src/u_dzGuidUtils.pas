unit u_dzGuidUtils;

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
  u_dzNullableTypesUtils;

type
  TNullableGuid = record
  private
    FValue: TGuid;
    FIsValid: IInterface;
    procedure DeclareValid; inline;
  public
    ///<summary> Generates a new GUID using WinAPI calls </summary>
    procedure GenerateNew;
    ///<summary> Convert to a string </summary>
    function ToString: string;
    ///<summary> convert to a variant for using for database fields / parameters) </summary>
    function ToVariant: Variant;
    //<summary> Convert from a string </summary>
    procedure AssignString(const _s: string);
    ///<summary>
    /// convert from a variant for assigning a database field
    /// @returns true, if the assignent worked, false otherwise </summary>
    function AssignVariant(_v: Variant): Boolean;
    ///<summary> explicit cast to string "string(GUID)" converts to standard string form </summary>
    class operator Explicit(_a: TNullableGuid): string;
    ///<summary> explicit cast converts from standard string form </summary>
    class operator Explicit(const _a: string): TNullableGuid;
    ///<summary> compares two NullableGuids, returns true, if the are equal, raises exception if one
    ///          is not valid </summary>
    class operator Equal(_a, _b: TNullableGuid): Boolean;
    ///<summary> compares two NullableGuids, returns truw if they are different or at least one is invalid </summary>
    class operator NotEqual(_a, _b: TNullableGuid): Boolean;
    ///<summary> implicit conversion from TGUID </summary>
    class operator Implicit(_a: TGuid): TNullableGuid;
    ///<summary> returns the GUID, if valid, raises an exception otherwise </summary>
    function Value: TGuid;
    ///<summary> returns true, if valid, false otherwise </summary>
    function IsValid: Boolean;
    ///<summary> invalidates the GUID </summary>
    procedure Invalidate;
    ///<summary> returns a new, valid GUID </summary>
    class function Invalid: TNullableGuid; static;
    class function Generate: TNullableGuid; static;
    class function FromVariant(_v: Variant): TNullableGuid; static;
    class function FromString(const _s: string): TNullableGuid; static;
  end;

///<summary> Tries to convert a string to a GUID, returns true if successfull </summary>
function TryStr2GUID(_s: string; out _GUID: TGuid): Boolean;
///<summary> Tries to convert a variant to a GUID, returns true if successfull </summary>
function TryVar2GUID(const _v: Variant; out _GUID: TGuid): Boolean;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  ActiveX,
  u_dzVariantUtils,
  u_dzStringUtils,
  u_dzConvertUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

// the old implementation required curly string representation
//function TryStr2GUID(_s: string; out _GUID: TGuid): Boolean;
//begin
//  Result := Succeeded(CLSIDFromString(PWideChar(WideString(_s)), _GUID));
//end;

function TryStr2GUID(_s: string; out _GUID: TGuid): Boolean;
var
  Len: Integer;
  s: string;
  Value: ULong;
  i: Integer;
begin
  Result := False;
  // a GUI in curly string representation looks like this:
  // {00020400-0000-0000-C000-000000000046}
  //  123456789012345678901234567890123456 = 36 + 2 characters
  Len := Length(_s);
  if (Len = 38) then begin
    if (_s[1] <> '{') or (_s[Len] <> '}') then
      Exit; //==>
    // if it is in curly string representation, we remove the curly braces
    _s := Copy(_s, 2, 36);
  end else if Len <> 36 then
    Exit; //==>
  s := Copy(_s, 1, 8);
  if not TryHex2Long(s, _GUID.D1) then
    Exit; //==>
  s := Copy(_s, 10, 4);
  if not TryHex2Long(s, Value) then
    Exit; //==>
  _GUID.D2 := Value;
  s := Copy(_s, 15, 4);
  if not TryHex2Long(s, Value) then
    Exit; //==>
  _GUID.D3 := Value;

  // The next 4 character group is stored in _GUID.D4[0] and _GUID.D[1]
  // (WTF came up with that representation?)
  s := Copy(_s, 20, 2);
  if not TryHex2Long(s, Value) then
    Exit; //==>
  _GUID.D4[0] := Value;
  s := Copy(_s, 22, 2);
  if not TryHex2Long(s, Value) then
    Exit; //==>
  _GUID.D4[1] := Value;

  // the last 12 character group is stored in _GUID.D4[2..7]
  for i := 2 to Length(_GUID.D4) - 1 do begin
    s := Copy(_s, 25 + (i - 2) * 2, 2);
    if not TryHex2Long(s, Value) then
      Exit; //==>
    _GUID.D4[i] := Value;
  end;
  // lastly test that the dashes have been at the correct positions
  if (_s[9] = '-') and (_s[14] = '-') and (_s[19] = '-') and (_s[24] = '-') then
    Result := True;
end;

function TryVar2GUID(const _v: Variant; out _GUID: TGuid): Boolean;
var
  s: string;
begin
  Result := TryVar2Str(_v, s);
  if Result then
    Result := TryStr2GUID(s, _GUID);
end;

{ TNullableGuid }

procedure TNullableGuid.AssignString(const _s: string);
begin
  if TryStr2GUID(_s, FValue) then
    DeclareValid;
end;

function TNullableGuid.AssignVariant(_v: Variant): Boolean;
begin
  Result := TryVar2GUID(_v, FValue);
  if Result then
    DeclareValid
  else
    Invalidate;
end;

function TNullableGuid.ToString: string;
begin
  Result := string(Self);
end;

function TNullableGuid.ToVariant: Variant;
begin
  Result := string(Self);
end;

function TNullableGuid.Value: TGuid;
begin
  if not IsValid then
    raise Exception.Create('TNullableGuid is not valid');
  Result := FValue;
end;

class function TNullableGuid.Invalid: TNullableGuid;
begin
  Result.Invalidate;
end;

procedure TNullableGuid.Invalidate;
begin
  FIsValid := nil;
end;

procedure TNullableGuid.DeclareValid;
begin
  FIsValid := GetNullableTypesFlagInterface;
end;

class operator TNullableGuid.Equal(_a, _b: TNullableGuid): Boolean;
begin
  Result := IsEqualGUID(_a.Value, _b.Value);
end;

class operator TNullableGuid.NotEqual(_a, _b: TNullableGuid): Boolean;
begin
  if _a.IsValid and _b.IsValid then
    Result := not IsEqualGUID(_a.Value, _b.Value)
  else
    Result := _a.IsValid or _b.IsValid;
end;

class operator TNullableGuid.Explicit(const _a: string): TNullableGuid;
begin
  if _a = '' then
    Result.Invalidate
  else
    Result := StringToGUID(_a);
end;

class function TNullableGuid.FromString(const _s: string): TNullableGuid;
var
  gd: TGuid;
begin
  if not TryStr2GUID(_s, gd) then
    if not TryStr2GUID('{' + _s + '}', gd) then
      raise Exception.CreateFmt('NullableGuid.FromString: ' + _('"%s" is no valid GUID'), [_s]);
  Result := gd;
end;

class function TNullableGuid.FromVariant(_v: Variant): TNullableGuid;
begin
  Result.AssignVariant(_v);
end;

class operator TNullableGuid.Explicit(_a: TNullableGuid): string;
begin
  if _a.IsValid then
    Result := GUIDToString(_a.FValue)
  else
    Result := '';
end;

class function TNullableGuid.Generate: TNullableGuid;
begin
  Result.GenerateNew;
end;

procedure TNullableGuid.GenerateNew;
begin
  if Succeeded(CreateGUID(FValue)) then
    DeclareValid
  else
    Invalidate;
end;

class operator TNullableGuid.Implicit(_a: TGuid): TNullableGuid;
begin
  Result.FValue := _a;
  Result.DeclareValid;
end;

function TNullableGuid.IsValid: Boolean;
begin
  Result := Assigned(FIsValid);
end;

{$ENDIF DELPHI2007_UP}

end.
