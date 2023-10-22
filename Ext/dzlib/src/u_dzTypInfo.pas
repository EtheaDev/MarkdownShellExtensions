unit u_dzTypInfo;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  SysUtils,
  Classes,
  TypInfo,
  u_dzTranslator;

  // I am not going to proved support for Variant properties here. I hate Variants.

const
  ///<summary> property types that can be converted to string </summary>
  STRING_PROPERTY_TYPES = [
{$IFDEF FPC}
    tkAString,
    tkUString,
{$ENDIF}
{$IFDEF SUPPORTS_UNICODE}
    tkUString,
{$ENDIF SUPPORTS_UNICODE}
    tkChar, tkString, tkWChar, tkLString, tkWString];
  ///<summary> property types that can be converted to float </summary>
  FLOAT_PROPERTY_TYPES = [tkInteger, tkFloat];

const
  NilMethod: TMethod = (Code: nil; Data: nil);

function TryGetStringProperty(_Instance: TObject; const _Name: string; out _Value: string): Boolean;
function GetStringProperty(_Instance: TObject; const _Name: string; const _Default: string): string; overload;
function GetStringProperty(_Instance: TObject; const _Name: string): string; overload;

{$IFDEF SUPPORTS_EXTENDED}
function TryGetFloatProperty(_Instance: TObject; const _Name: string; out _Value: Extended): Boolean; overload;
{$ENDIF}
function TryGetFloatProperty(_Instance: TObject; const _Name: string; out _Value: Double): Boolean; overload;
function TryGetFloatProperty(_Instance: TObject; const _Name: string; out _Value: Single): Boolean; overload;

function TryGetIntProperty(_Instance: TObject; const _Name: string; out _Value: Integer): Boolean;
function TrySetIntProperty(_Instance: TObject; const _Name: string; _Value: Integer): Boolean;
function TryGetBoolProperty(_Instance: TObject; const _Name: string; out _Value: Boolean): Boolean;
///<summary>
/// Reads an enum type property with type checking.
/// @param TypeInfo is the PTypeInfo for the enum, pass TypeInfo(YourEnumType) here </summary>
function TryGetEnumProperty(_Instance: TObject; const _Name: string; const _TypeInfo: PTypeInfo;
  out _Value: Integer): Boolean; overload;
///<summary>
/// Reads an enum type property without type checking </summary>
function TryGetEnumProperty(_Instance: TObject; const _Name: string;
  out _Value: Integer): Boolean; overload;

{$IFDEF SUPPORTS_EXTENDED}
function GetFloatProperty(_Instance: TObject; const _Name: string; const _Default: Extended): Extended; overload;
function GetFloatProperty(_Instance: TObject; const _Name: string): Extended; overload;
function TrySetFloatProperty(_Instance: TObject; const _Name: string; const _Value: Extended): Boolean; overload;
{$ELSE}
function GetFloatProperty(_Instance: TObject; const _Name: string; const _Default: Double): Double; overload;
function GetFloatProperty(_Instance: TObject; const _Name: string): Double; overload;
function TrySetFloatProperty(_Instance: TObject; const _Name: string; const _Value: Double): Boolean; overload;
{$ENDIF SUPPORTS_EXTENDED}

function TryGetObjectProperty(_Instance: TObject; const _Name: string; out _Value: TObject): Boolean;
function GetObjectProperty(_Instance: TObject; const _Name: string; _Default: TObject): TObject; overload;
function GetObjectProperty(_Instance: TObject; const _Name: string): TObject; overload;
function TrySetObjectProperty(_Instance: TObject; const _Name: string; _Value: TObject): Boolean;

function TryGetEventProperty(_Instance: TObject; const _Name: string; out _Value: TMethod): Boolean;
function GetEventProperty(_Instance: TObject; const _Name: string; _Default: TMethod): TMethod; overload;
function GetEventProperty(_Instance: TObject; const _Name: string): TMethod; overload;
function TrySetEventProperty(_Instance: TObject; const _Name: string; _Value: TMethod): Boolean;
procedure SetEventProperty(_Instance: TObject; const _Name: string; _Value: TMethod);

function TryGetEnumValues(_Instance: TObject; const _Name: string; _sl: TStrings): Boolean;
procedure GetEnumValues(_Instance: TObject; const _Name: string; _sl: TStrings);

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

function _(const _s: string): string; inline;
begin
  Result := dzlibGetText(_s);
end;

function TryGetStringProperty(_Instance: TObject; const _Name: string; out _Value: string): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind in STRING_PROPERTY_TYPES);
  if Result then
    _Value := GetPropValue(_Instance, PropInfo)
end;

function GetStringProperty(_Instance: TObject; const _Name: string; const _Default: string): string;
begin
  if not TryGetStringProperty(_Instance, _Name, Result) then
    Result := _Default;
end;

function GetStringProperty(_Instance: TObject; const _Name: string): string; overload;
begin
  if not TryGetStringProperty(_Instance, _Name, Result) then
    raise EPropertyError.CreateFmt(_('String property %s not found.'), [_Name]);
end;

{$IFDEF SUPPORTS_EXTENDED}
function TryGetFloatProperty(_Instance: TObject; const _Name: string; out _Value: Extended): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind in FLOAT_PROPERTY_TYPES);
  if Result then
    _Value := GetPropValue(_Instance, PropInfo)
end;
{$ENDIF}

function TryGetFloatProperty(_Instance: TObject; const _Name: string; out _Value: Double): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind in FLOAT_PROPERTY_TYPES);
  if Result then
    _Value := GetPropValue(_Instance, PropInfo)
end;

function TryGetFloatProperty(_Instance: TObject; const _Name: string; out _Value: Single): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind in FLOAT_PROPERTY_TYPES);
  if Result then
    _Value := GetPropValue(_Instance, PropInfo)
end;

{$IFDEF SUPPORTS_EXTENDED}
function GetFloatProperty(_Instance: TObject; const _Name: string; const _Default: Extended): Extended;
begin
  if not TryGetFloatProperty(_Instance, _Name, Result) then
    Result := _Default;
end;

function GetFloatProperty(_Instance: TObject; const _Name: string): Extended;
begin
  if not TryGetFloatProperty(_Instance, _Name, Result) then
    raise EPropertyError.CreateFmt(_('Float property %s not found.'), [_Name]);
end;

function TrySetFloatProperty(_Instance: TObject; const _Name: string; const _Value: Extended): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind in FLOAT_PROPERTY_TYPES);
  if Result then
    SetPropValue(_Instance, PropInfo, _Value);
end;

{$ELSE}
function GetFloatProperty(_Instance: TObject; const _Name: string; const _Default: Double): Double;
begin
  if not TryGetFloatProperty(_Instance, _Name, Result) then
    Result := _Default;
end;

function GetFloatProperty(_Instance: TObject; const _Name: string): Double;
begin
  if not TryGetFloatProperty(_Instance, _Name, Result) then
    raise EPropertyError.CreateFmt(_('Float property %s not found.'), [_Name]);
end;

function TrySetFloatProperty(_Instance: TObject; const _Name: string; const _Value: Double): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind in FLOAT_PROPERTY_TYPES);
  if Result then
    SetPropValue(_Instance, PropInfo, _Value);
end;
{$ENDIF}

function TryGetIntProperty(_Instance: TObject; const _Name: string; out _Value: Integer): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkInteger);
  if Result then
    _Value := GetOrdProp(_Instance, PropInfo);
end;

function TrySetIntProperty(_Instance: TObject; const _Name: string; _Value: Integer): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkInteger);
  if Result then
    SetOrdProp(_Instance, PropInfo, _Value);
end;

function TryGetBoolProperty(_Instance: TObject; const _Name: string; out _Value: Boolean): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkEnumeration)
    and (GetTypeData(PropInfo.PropType^)^.BaseType^ = TypeInfo(Boolean));
  if Result then
    _Value := Boolean(GetOrdProp(_Instance, PropInfo));
end;

function TryGetEnumProperty(_Instance: TObject; const _Name: string; const _TypeInfo: PTypeInfo;
  out _Value: Integer): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkEnumeration)
    and (GetTypeData(PropInfo.PropType^)^.BaseType^ = _TypeInfo);
  if Result then
    _Value := GetOrdProp(_Instance, PropInfo);
end;

function TryGetEnumProperty(_Instance: TObject; const _Name: string; out _Value: Integer): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkEnumeration);
  if Result then
    _Value := GetOrdProp(_Instance, PropInfo);
end;

function TryGetObjectProperty(_Instance: TObject; const _Name: string; out _Value: TObject): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkClass);
  if Result then
    _Value := TObject(GetOrdProp(_Instance, PropInfo));
end;

function GetObjectProperty(_Instance: TObject; const _Name: string; _Default: TObject): TObject;
begin
  if not TryGetObjectProperty(_Instance, _Name, Result) then
    Result := _Default;
end;

function GetObjectProperty(_Instance: TObject; const _Name: string): TObject;
begin
  if not TryGetObjectProperty(_Instance, _Name, Result) then
    raise EPropertyError.CreateFmt(_('Object property %s not found.'), [_Name]);
end;

function TrySetObjectProperty(_Instance: TObject; const _Name: string; _Value: TObject): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkClass);
  if Result then begin
    SetObjectProp(_Instance, PropInfo, _Value);
  end;
end;

function TryGetEventProperty(_Instance: TObject; const _Name: string; out _Value: TMethod): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkMethod);
  if Result then begin
    _Value := GetMethodProp(_Instance, PropInfo);
  end;
end;

function GetEventProperty(_Instance: TObject; const _Name: string; _Default: TMethod): TMethod;
begin
  if not TryGetEventProperty(_Instance, _Name, Result) then
    Result := _Default;
end;

function GetEventProperty(_Instance: TObject; const _Name: string): TMethod;
begin
  if not TryGetEventProperty(_Instance, _Name, Result) then
    raise EPropertyError.CreateFmt(_('Event property %s not found.'), [_Name]);
end;

function TrySetEventProperty(_Instance: TObject; const _Name: string; _Value: TMethod): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkMethod);
  if Result then begin
    SetMethodProp(_Instance, PropInfo, _Value);
  end;
end;

procedure SetEventProperty(_Instance: TObject; const _Name: string; _Value: TMethod);
begin
  if not TrySetEventProperty(_Instance, _Name, _Value) then
    raise EPropertyError.CreateFmt(_('Event property %s not found.'), [_Name]);
end;

function TryGetEnumValues(_Instance: TObject; const _Name: string; _sl: TStrings): Boolean;
var
  T: PTypeData;
  i: Integer;
  PropInfo: PPropInfo;
  PropType: PTypeInfo;
begin
  PropInfo := GetPropInfo(_Instance, _Name);
  Result := Assigned(PropInfo);
  if not Result then
    Exit; //==>

  PropType := PropInfo.PropType^;
  Result := PropType^.Kind = tkEnumeration;
  if not Result then
    Exit; //==>

  _sl.Clear;
  T := GetTypeData(GetTypeData(PropType).BaseType^);
  for i := T.MinValue to T.MaxValue do begin
    _sl.AddObject(GetEnumName(PropType, i), Pointer(i));
  end;
end;

procedure GetEnumValues(_Instance: TObject; const _Name: string; _sl: TStrings);
begin
  if not TryGetEnumValues(_Instance, _Name, _sl) then
    raise EPropertyError.CreateFmt(_('Enum property %s not found.'), [_Name]);
end;

{$ENDIF DELPHI2007_UP}

end.
