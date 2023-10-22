unit u_dzAdvancedObject;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  Classes,
  TypInfo,
  u_dzTypes,
  u_dzTranslator;

type
  EPropertyNotFound = class(EdzException)

  end;

type
{$M+}
  ///<summary>
  /// Advanced object class with helper functions to access published properties (of descendants) </summary>
  TAdvancedObject = class
  public
    type
      TStringArray = array of string;
  public
    function HasProperty(const _Name: string; out _Type: TTypeKind): Boolean; overload;
    class function HasProperty(_Instance: TObject; const _Name: string; out _Type: TTypeKind): Boolean; overload;

    function HasEnumProperty(const _Name: string): Boolean; overload;
    class function HasEnumProperty(_Instance: TObject; const _Name: string): Boolean; overload;

    function HasStringProperty(const _Name: string): Boolean; overload;
    class function HasStringProperty(_Instance: TObject; const _Name: string): Boolean; overload;

    function HasFloatProperty(const _Name: string): Boolean; overload;
    class function HasFloatProperty(_Instance: TObject; const _Name: string): Boolean; overload;

    function HasIntProperty(const _Name: string): Boolean; overload;
    class function HasIntProperty(_Instance: TObject; const _Name: string): Boolean; overload;

    function HasObjectProperty(const _Name: string): Boolean; overload;
    class function HasObjectProperty(_Instance: TObject; const _Name: string): Boolean; overload;

    function HasEventProperty(const _Name: string): Boolean; overload;
    class function HasEventProperty(_Instance: TObject; const _Name: string): Boolean; overload;

    ///<summary>
    /// Calls HasEnumProperty and raises an exception if that call returns false </summary>
    class procedure CheckHasEnumProperty(_Instance: TObject; const _Name: string);

    ///<summary>
    /// Calls HasIntProperty and raises an exception if that call returns false </summary>
    class procedure CheckHasIntProperty(_Instance: TObject; const _Name: string);

    ///<summary>
    /// Access a string property
    /// @param Name is the name of the property
    /// @param Value is the value of the property, only valid if Result is true
    /// @returns true, if the property exists and is a string property
    ///          false if it either doesn't exist or has a different type </summary>
    function TryGetStringProperty(const _Name: string; out _Value: string): Boolean; overload;
    class function TryGetStringProperty(_Instance: TObject; const _Name: string; out _Value: string): Boolean; overload;
    function GetStringProperty(const _Name: string; const _Default: string): string; overload;
    class function GetStringProperty(_Instance: TObject; const _Name: string; const _Default: string): string; overload;
    function GetStringProperty(const _Name: string): string; overload;
    class function GetStringProperty(_Instance: TObject; const _Name: string): string; overload;
    ///<summary>
    /// Sets a string property, returns true on success
    /// @param Name is the name of the property to set
    /// @param Value is the value to set the property to
    /// @returns true, if the property could be set
    ///          false otherwise </summary>
    function SetStringProperty(const _Name: string; const _Value: string): Boolean; overload;
    class function SetStringProperty(_Instance: TObject; const _Name: string; const _Value: string): Boolean; overload;

    ///<summary>
    /// Access a boolean property
    /// @param Name is the name of the property
    /// @param Value is the value of the property, only valid if Result is true
    /// @returns true, if the property exists and is an enum(!) property
    ///          false if it either doesn't exist or has a different type
    /// Note: It is not possible to distinguish between Boolean and any other enum type
    ///       So, this will Get or Set any enum property. </summary>
    function TryGetBoolProperty(const _Name: string; out _Value: Boolean): Boolean; overload;
    class function TryGetBoolProperty(_Instance: TObject; const _Name: string; out _Value: Boolean): Boolean; overload;
    function GetBoolProperty(const _Name: string): Boolean; overload;
    class function GetBoolProperty(_Instance: TObject; const _Name: string): Boolean; overload;
    function SetBoolProperty(const _Name: string; _Value: Boolean): Boolean; overload;
    class function SetBoolProperty(_Instance: TObject; const _Name: string; _Value: Boolean): Boolean; overload;

    ///<summary>
    /// Access a float property
    /// @param Name is the name of the property
    /// @param Value is the value of the property, only valid if Result is true
    /// @returns true, if the property exists and is a float property
    ///          false if it either doesn't exist or has a different type </summary>
{$IFDEF SUPPORTS_EXTENDED}
    function TryGetFloatProperty(const _Name: string; out _Value: Extended): Boolean; overload;
{$ENDIF}
    function TryGetFloatProperty(const _Name: string; out _Value: Double): Boolean; overload;
    function TryGetFloatProperty(const _Name: string; out _Value: Single): Boolean; overload;
{$IFDEF SUPPORTS_EXTENDED}
    class function GetFloatProperty(_Instance: TObject; const _Name: string): Extended; overload;
    class function GetFloatProperty(_Instance: TObject; const _Name: string; const _Default: Extended): Extended; overload;
    function GetFloatProperty(const _Name: string; const _Default: Extended): Extended; overload;
    function GetFloatProperty(const _Name: string): Extended; overload;
    class function SetFloatProperty(_Instance: TObject; const _Name: string; const _Value: Extended): Boolean; overload;
{$ELSE}
    class function GetFloatProperty(_Instance: TObject; const _Name: string): Double; overload;
    class function GetFloatProperty(_Instance: TObject; const _Name: string; const _Default: Double): Double; overload;
    function GetFloatProperty(const _Name: string; const _Default: Double): Double; overload;
    function GetFloatProperty(const _Name: string): Double; overload;
    class function SetFloatProperty(_Instance: TObject; const _Name: string; const _Value: Double): Boolean; overload;
{$ENDIF}

    ///<summary>
    /// Access an integer property
    /// @param Name is the name of the property
    /// @param Value is the value of the property, only valid if Result is true
    /// @returns true, if the property exists and is an integer property
    ///          false if it either doesn't exist or has a different type </summary>
    function TryGetIntProperty(const _Name: string; out _Value: Integer): Boolean; overload;
    class function TryGetIntProperty(_Instance: TObject; const _Name: string; out _Value: Integer): Boolean; overload;
    function GetIntProperty(const _Name: string): Integer; overload;
    class function GetIntProperty(_Instance: TObject; const _Name: string): Integer; overload;
    function SetIntProperty(const _Name: string; _Value: Integer): Boolean; overload;
    class function SetIntProperty(_Instance: TObject; const _Name: string; _Value: Integer): Boolean; overload;

    ///<summary>
    /// Access an enum property
    /// @param Name is the name of the property
    /// @param Value is the value of the property, only valid if Result is true
    ///              Note: The value must be type cast to the appropriate enum type
    /// @returns true, if the property exists and is an enum property
    ///          false if it either doesn't exist or has a different type </summary>
    function TryGetEnumProperty(const _Name: string; out _Value: Integer): Boolean; overload;
    class function TryGetEnumProperty(_Instance: TObject; const _Name: string; out _Value: Integer): Boolean; overload;
    function GetEnumProperty(const _Name: string): Integer; overload;
    class function GetEnumProperty(_Instance: TObject; const _Name: string): Integer; overload;
    function SetEnumProperty(const _Name: string; _Value: Integer): Boolean; overload;
    class function SetEnumProperty(_Instance: TObject; const _Name: string; _Value: Integer): Boolean; overload;

    ///<summary>
    /// Access an object reference property
    /// @param Name is the name of the property
    /// @param Value is the value of the property, only valid if Result is true
    /// @returns true, if the property exists and is an object reference property
    ///          false if it either doesn't exist or has a different type </summary>
    function TryGetObjectProperty(const _Name: string; out _Value: TObject): Boolean; overload;
    class function TryGetObjectProperty(_Instance: TObject; const _Name: string; out _Value: TObject): Boolean; overload;
    function GetObjectProperty(const _Name: string; _Default: TObject): TObject; overload;
    class function GetObjectProperty(_Instance: TObject; const _Name: string; _Default: TObject): TObject; overload;
    function GetObjectProperty(const _Name: string): TObject; overload;
    class function GetObjectProperty(_Instance: TObject; const _Name: string): TObject; overload;
    class function SetObjectProperty(_Instance: TObject; const _Name: string; _Value: TObject): Boolean;

    ///<summary>
    /// Access an event property
    /// @param Name is the name of the property
    /// @param Value is the value of the property, only valid if Result is true
    /// @returns true, if the property exists and is an event property
    ///          false if it either doesn't exist or has a different type </summary>
    function TryGetEventProperty(const _Name: string; out _Value: TMethod): Boolean;
    function GetEventProperty(const _Name: string; _Default: TMethod): TMethod; overload;
    function GetEventProperty(const _Name: string): TMethod; overload;

    ///<summary>
    /// Sets an event property, returns true on success </summary>
    function SetEventProperty(const _Name: string; _Value: TMethod): Boolean; overload;
    class function SetEventProperty(_Instance: TObject; const _Name: string; _Value: TMethod): Boolean; overload;

    function GetProperties: TStringArray;

    function TryGetEnumValues(const _Name: string; _sl: TStrings): Boolean; overload;
    class function TryGetEnumValues(_Instance: TObject; const _Name: string; _sl: TStrings): Boolean; overload;
    procedure GetEnumValues(const _Name: string; _sl: TStrings); overload;
    class procedure GetEnumValues(_Instance: TObject; const _Name: string; _sl: TStrings); overload;
  end;
{$M-}
{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  u_dzTypInfo;

{ TAdvancedObject }

function TAdvancedObject.GetEventProperty(const _Name: string; _Default: TMethod): TMethod;
begin
  Result := u_dzTypInfo.GetEventProperty(Self, _Name, _Default);
end;

class procedure TAdvancedObject.CheckHasEnumProperty(_Instance: TObject; const _Name: string);
begin
  if not HasEnumProperty(_Instance, _Name) then
    raise EPropertyNotFound.CreateFmt(dzlibGetText('Class "%s" has no published %s property "%s".'),
      [_Instance.ClassName, 'Enum', _Name]);
end;

class procedure TAdvancedObject.CheckHasIntProperty(_Instance: TObject; const _Name: string);
begin
  if not HasIntProperty(_Instance, _Name) then
    raise EPropertyNotFound.CreateFmt(dzlibGetText('Class "%s" has no published %s property "%s".'),
      [_Instance.ClassName, 'Integer', _Name]);
end;

function TAdvancedObject.GetBoolProperty(const _Name: string): Boolean;
begin
  Result := GetBoolProperty(Self, _Name);
end;

class function TAdvancedObject.GetBoolProperty(_Instance: TObject; const _Name: string): Boolean;
begin
  if not TryGetBoolProperty(_Instance, _Name, Result) then
    raise EPropertyNotFound.CreateFmt(dzlibGetText('Class "%s" has no published %s property "%s".'),
      [_Instance.ClassName, 'Enum', _Name]);
end;

class function TAdvancedObject.GetEnumProperty(_Instance: TObject; const _Name: string): Integer;
begin
  if not TryGetEnumProperty(_Instance, _Name, Result) then
    raise EPropertyNotFound.CreateFmt(dzlibGetText('Class "%s" has no published %s property "%s".'),
      [_Instance.ClassName, 'Enum', _Name]);
end;

function TAdvancedObject.GetEnumProperty(const _Name: string): Integer;
begin
  Result := GetEnumProperty(Self, _Name);
end;

function TAdvancedObject.GetEventProperty(const _Name: string): TMethod;
begin
  Result := u_dzTypInfo.GetEventProperty(Self, _Name);
end;

function TAdvancedObject.SetEventProperty(const _Name: string; _Value: TMethod): Boolean;
begin
  Result := SetEventProperty(Self, _Name, _Value);
end;

class function TAdvancedObject.SetBoolProperty(_Instance: TObject; const _Name: string;
  _Value: Boolean): Boolean;
var
  PropInfo: PPropInfo;
  Value: NativeInt;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkEnumeration);
  if Result then begin
    Value := NativeInt(_Value);
    TypInfo.SetOrdProp(_Instance, PropInfo, Value);
  end;
end;

function TAdvancedObject.SetBoolProperty(const _Name: string; _Value: Boolean): Boolean;
begin
  Result := SetBoolProperty(Self, _Name, _Value);
end;

class function TAdvancedObject.SetEnumProperty(_Instance: TObject; const _Name: string;
  _Value: Integer): Boolean;
var
  PropInfo: PPropInfo;
  Value: NativeInt;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkEnumeration);
  if Result then begin
    Value := NativeInt(_Value);
    TypInfo.SetOrdProp(_Instance, PropInfo, Value);
  end;
end;

function TAdvancedObject.SetEnumProperty(const _Name: string; _Value: Integer): Boolean;
begin
  Result := SetEnumProperty(Self, _Name, _Value);
end;

class function TAdvancedObject.SetEventProperty(_Instance: TObject; const _Name: string;
  _Value: TMethod): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkMethod);
  if Result then
    TypInfo.SetMethodProp(_Instance, PropInfo, _Value);
end;

function TAdvancedObject.SetIntProperty(const _Name: string; _Value: Integer): Boolean;
begin
  Result := SetIntProperty(Self, _Name, _Value);
end;

class function TAdvancedObject.SetIntProperty(_Instance: TObject; const _Name: string;
  _Value: Integer): Boolean;
begin
  Result := u_dzTypInfo.TrySetIntProperty(_Instance, _Name, _Value);
end;

{$IFDEF SUPPORTS_EXTENDED}
class function TAdvancedObject.GetFloatProperty(_Instance: TObject; const _Name: string): Extended;
begin
  Result := u_dzTypInfo.GetFloatProperty(_Instance, _Name);
end;

class function TAdvancedObject.GetFloatProperty(_Instance: TObject; const _Name: string; const _Default: Extended): Extended;
begin
  Result := u_dzTypInfo.GetFloatProperty(_Instance, _Name, _Default);
end;

function TAdvancedObject.GetFloatProperty(const _Name: string; const _Default: Extended): Extended;
begin
  Result := u_dzTypInfo.GetFloatProperty(Self, _Name, _Default);
end;

function TAdvancedObject.GetFloatProperty(const _Name: string): Extended;
begin
  Result := u_dzTypInfo.GetFloatProperty(Self, _Name);
end;

class function TAdvancedObject.SetFloatProperty(_Instance: TObject; const _Name: string; const _Value: Extended): Boolean;
begin
  Result := u_dzTypInfo.TrySetFloatProperty(_Instance, _Name, _Value);
end;

{$ELSE}
class function TAdvancedObject.GetFloatProperty(_Instance: TObject; const _Name: string): Double;
begin
  Result := u_dzTypInfo.GetFloatProperty(_Instance, _Name);
end;

class function TAdvancedObject.GetFloatProperty(_Instance: TObject; const _Name: string; const _Default: Double): Double;
begin
  Result := u_dzTypInfo.GetFloatProperty(_Instance, _Name, _Default);
end;

function TAdvancedObject.GetFloatProperty(const _Name: string; const _Default: Double): Double; overload;
begin
  Result := u_dzTypInfo.GetFloatProperty(Self, _Name, _Default);
end;

function TAdvancedObject.GetFloatProperty(const _Name: string): Double; overload;
begin
  Result := u_dzTypInfo.GetFloatProperty(Self, _Name);
end;

class function TAdvancedObject.SetFloatProperty(_Instance: TObject; const _Name: string; const _Value: Double): Boolean;
begin
  Result := u_dzTypInfo.TrySetFloatProperty(_Instance, _Name, _Value);
end;
{$ENDIF}

function TAdvancedObject.GetIntProperty(const _Name: string): Integer;
begin
  Result := GetIntProperty(Self, _Name);
end;

class function TAdvancedObject.GetIntProperty(_Instance: TObject; const _Name: string): Integer;
begin
  if not TryGetIntProperty(_Instance, _Name, Result) then
    raise EPropertyNotFound.CreateFmt(dzlibGetText('Class "%s" has no published %s property "%s".'),
      [_Instance.ClassName, 'Integer', _Name]);
end;

class function TAdvancedObject.GetObjectProperty(_Instance: TObject; const _Name: string;
  _Default: TObject): TObject;
begin
  Result := u_dzTypInfo.GetObjectProperty(_Instance, _Name, _Default);
end;

function TAdvancedObject.GetObjectProperty(const _Name: string; _Default: TObject): TObject;
begin
  Result := GetObjectProperty(Self, _Name, _Default);
end;

class function TAdvancedObject.GetObjectProperty(_Instance: TObject; const _Name: string): TObject;
begin
  Result := u_dzTypInfo.GetObjectProperty(_Instance, _Name);
end;

function TAdvancedObject.GetObjectProperty(const _Name: string): TObject;
begin
  Result := GetObjectProperty(Self, _Name);
end;

class function TAdvancedObject.SetObjectProperty(_Instance: TObject; const _Name: string; _Value: TObject): Boolean;
begin
  Result := u_dzTypInfo.TrySetObjectProperty(_Instance, _Name, _Value);
end;

function TAdvancedObject.GetStringProperty(const _Name, _Default: string): string;
begin
  Result := u_dzTypInfo.GetStringProperty(Self, _Name, _Default);
end;

class function TAdvancedObject.GetStringProperty(_Instance: TObject; const _Name, _Default: string): string;
begin
  if not TryGetStringProperty(_Instance, _Name, Result) then
    Result := _Default;
end;

class function TAdvancedObject.GetStringProperty(_Instance: TObject; const _Name: string): string;
begin
  if not TryGetStringProperty(_Instance, _Name, Result) then
    raise EPropertyNotFound.CreateFmt(dzlibGetText('Class "%s" has no published %s property "%s".'),
      [_Instance.ClassName, 'String', _Name]);
end;

function TAdvancedObject.GetStringProperty(const _Name: string): string;
begin
  Result := u_dzTypInfo.GetStringProperty(Self, _Name);
end;

function TAdvancedObject.HasEventProperty(const _Name: string): Boolean;
begin
  Result := HasEventProperty(Self, _Name);
end;

function TAdvancedObject.HasEnumProperty(const _Name: string): Boolean;
begin
  Result := HasEnumProperty(Self, _Name);
end;

class function TAdvancedObject.HasEnumProperty(_Instance: TObject; const _Name: string): Boolean;
var
  Kind: TTypeKind;
begin
  Result := HasProperty(_Instance, _Name, Kind) and (Kind = tkEnumeration);
end;

class function TAdvancedObject.HasEventProperty(_Instance: TObject; const _Name: string): Boolean;
var
  Kind: TTypeKind;
begin
  Result := HasProperty(_Instance, _Name, Kind) and (Kind = tkMethod);
end;

class function TAdvancedObject.HasFloatProperty(_Instance: TObject; const _Name: string): Boolean;
var
  Kind: TTypeKind;
begin
  Result := HasProperty(_Instance, _Name, Kind) and (Kind in FLOAT_PROPERTY_TYPES);
end;

function TAdvancedObject.HasFloatProperty(const _Name: string): Boolean;
begin
  Result := HasFloatProperty(Self, _Name);
end;

class function TAdvancedObject.HasIntProperty(_Instance: TObject; const _Name: string): Boolean;
var
  Kind: TTypeKind;
begin
  Result := HasProperty(_Instance, _Name, Kind) and (Kind = tkInteger);
end;

function TAdvancedObject.HasIntProperty(const _Name: string): Boolean;
begin
  Result := HasIntProperty(Self, _Name);
end;

class function TAdvancedObject.HasObjectProperty(_Instance: TObject; const _Name: string): Boolean;
var
  Kind: TTypeKind;
begin
  Result := HasProperty(_Instance, _Name, Kind) and (Kind = tkClass);
end;

function TAdvancedObject.HasObjectProperty(const _Name: string): Boolean;
begin
  Result := HasObjectProperty(Self, _Name);
end;

class function TAdvancedObject.HasProperty(_Instance: TObject; const _Name: string;
  out _Type: TTypeKind): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo);
  if Result then
    _Type := PropInfo.PropType^.Kind;
end;

function TAdvancedObject.HasProperty(const _Name: string; out _Type: TTypeKind): Boolean;
begin
  Result := HasProperty(Self, _Name, _Type);
end;

class function TAdvancedObject.HasStringProperty(_Instance: TObject; const _Name: string): Boolean;
var
  Kind: TTypeKind;
begin
  Result := HasProperty(_Instance, _Name, Kind) and (Kind in STRING_PROPERTY_TYPES);
end;

function TAdvancedObject.HasStringProperty(const _Name: string): Boolean;
begin
  Result := HasStringProperty(_Name);
end;

class function TAdvancedObject.SetStringProperty(_Instance: TObject; const _Name, _Value: string): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind in STRING_PROPERTY_TYPES);
  if Result then
    TypInfo.SetStrProp(_Instance, PropInfo, _Value);
end;

function TAdvancedObject.SetStringProperty(const _Name, _Value: string): Boolean;
begin
  Result := SetStringProperty(Self, _Name, _Value);
end;

class function TAdvancedObject.TryGetBoolProperty(_Instance: TObject; const _Name: string;
  out _Value: Boolean): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkEnumeration);
  if Result then
    _Value := GetOrdProp(_Instance, PropInfo) <> 0;
end;

function TAdvancedObject.TryGetBoolProperty(const _Name: string; out _Value: Boolean): Boolean;
begin
  Result := TryGetBoolProperty(Self, _Name, _Value);
end;

class function TAdvancedObject.TryGetEnumProperty(_Instance: TObject; const _Name: string;
  out _Value: Integer): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkEnumeration);
  if Result then
    _Value := GetOrdProp(_Instance, PropInfo);
end;

function TAdvancedObject.TryGetEnumProperty(const _Name: string; out _Value: Integer): Boolean;
begin
  Result := TryGetEnumProperty(Self, _Name, _Value);
end;

function TAdvancedObject.TryGetEventProperty(const _Name: string; out _Value: TMethod): Boolean;
begin
  Result := u_dzTypInfo.TryGetEventProperty(Self, _Name, _Value);
end;

{$IFDEF SUPPORTS_EXTENDED}
function TAdvancedObject.TryGetFloatProperty(const _Name: string; out _Value: Extended): Boolean;
begin
  Result := u_dzTypInfo.TryGetFloatProperty(Self, _Name, _Value);
end;
{$ENDIF}

function TAdvancedObject.TryGetFloatProperty(const _Name: string; out _Value: Double): Boolean;
begin
  Result := u_dzTypInfo.TryGetFloatProperty(Self, _Name, _Value);
end;

function TAdvancedObject.TryGetFloatProperty(const _Name: string; out _Value: Single): Boolean;
begin
  Result := u_dzTypInfo.TryGetFloatProperty(Self, _Name, _Value);
end;

class function TAdvancedObject.TryGetIntProperty(_Instance: TObject; const _Name: string;
  out _Value: Integer): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _Name);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind = tkInteger);
  if Result then
    _Value := GetOrdProp(_Instance, PropInfo);
end;

function TAdvancedObject.TryGetIntProperty(const _Name: string; out _Value: Integer): Boolean;
begin
  Result := TryGetIntProperty(Self, _Name, _Value);
end;

class function TAdvancedObject.TryGetObjectProperty(_Instance: TObject; const _Name: string;
  out _Value: TObject): Boolean;
begin
  Result := u_dzTypInfo.TryGetObjectProperty(_Instance, _Name, _Value);
end;

function TAdvancedObject.TryGetObjectProperty(const _Name: string; out _Value: TObject): Boolean;
begin
  Result := TryGetObjectProperty(Self, _Name, _Value);
end;

class function TAdvancedObject.TryGetStringProperty(_Instance: TObject; const _Name: string;
  out _Value: string): Boolean;
begin
  Result := u_dzTypInfo.TryGetStringProperty(_Instance, _Name, _Value);
end;

function TAdvancedObject.TryGetStringProperty(const _Name: string; out _Value: string): Boolean;
begin
  Result := TryGetStringProperty(Self, _Name, _Value);
end;

function TAdvancedObject.GetProperties: TStringArray;
var
  Props: PPropList;
  cnt: Integer;
  i: Integer;
  PropInfo: PPropInfo;
begin
  cnt := GetPropList(Self, Props);
  try
    SetLength(Result, cnt);
    for i := 0 to cnt - 1 do begin
      PropInfo := Props^[i];
      Result[i] := string(PropInfo.Name);
    end;
  finally
    FreeMem(Props);
  end;
end;

class function TAdvancedObject.TryGetEnumValues(_Instance: TObject; const _Name: string;
  _sl: TStrings): Boolean;
begin
  Result := u_dzTypInfo.TryGetEnumValues(_Instance, _Name, _sl);
end;

function TAdvancedObject.TryGetEnumValues(const _Name: string; _sl: TStrings): Boolean;
begin
  Result := TryGetEnumValues(Self, _Name, _sl);
end;

class procedure TAdvancedObject.GetEnumValues(_Instance: TObject; const _Name: string;
  _sl: TStrings);
begin
  u_dzTypInfo.GetEnumValues(_Instance, _Name, _sl);
end;

procedure TAdvancedObject.GetEnumValues(const _Name: string; _sl: TStrings);
begin
  GetEnumValues(Self, _Name, _sl);
end;

{$ENDIF DELPHI2007_UP}

end.

