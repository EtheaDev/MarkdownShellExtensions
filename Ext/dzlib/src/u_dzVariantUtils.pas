///<summary>
/// several utility functions for Variants </summary>
unit u_dzVariantUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Variants,
  VarUtils,
  u_dzTranslator,
  u_dzTypes,
  u_dzDateUtils; // we need this for $IF Declared(Str2Date)

type
  ///<summary> raised if there is a conversion error in one of the Var2XxxEx functions </summary>
  EVariantConvertError = class(EdzException);
  ///<summary> raised if the variant passed to one of the Var2XxxEx functions is null </summary>
  EVarIsNull = class(EVariantConvertError);
  ///<summary> raised if the variant passed to one of the Var2XxxEx functions is empty </summary>
  EVarIsEmpty = class(EVariantConvertError);

function VarIsNullOrEmpty(_v: Variant): Boolean;

///<summary>
/// converts a variant to its string representation (for debugging / logging) </summary>
function ToString(_v: OleVariant): string; overload; deprecated; // use DumpVariant instead
///<summary>
/// converts a variant to its string representation (for debugging / logging) </summary>
function DumpVariant(_v: OleVariant): string;

///<summary> Checks whether a variant is a type that can be assigned to an integer (signed 32 bit),
///          Note: Excludes longword and Int64, even if the value may be <= MaxLongInt </summary>
function VarIsInteger(_v: Variant): Boolean;

///<summary> Checks whether a variant is of a type that can be assigned to a longword (unsigned 32 bit),
//           Note: Excludes signed integers, even if the value may be positive </summary>
function VarIsLongWord(_v: Variant): Boolean;

///<summary> Checks whether a variant is of a type that can be assigned to an Int64 (signed 64 bit) </summary>
function VarIsInt64(_v: Variant): Boolean;

// Variant to other type conversion functions
// TryVar2Xxx converts from variant to type Xxx, returns false, if
// the variant is NULL.
// Var2Xxx converts from variant to type Xxx and returns the Default if the
// variant is NULL.
// Var2XxxEx converts from variant to type Xxx, but raises an exception if
// variant is NULL, using the Source for the message.

///<summary> Converts a variant to an integer.
///          If v is null or empty, it returns false
///          @param v Variant value to convert
///          @param Value is the variant's integer value, only valid if the function
///                       returns true.
///          @returns true, if the variant could be converted to integer, false if not. </summary>
function TryVar2Int(const _v: Variant; out _Value: Integer): Boolean;
function TryVar2Int64(const _v: Variant; out _Value: Int64): Boolean;

///<summary> Converts a variant to a LongWord.
///          If v is null or empty, it returns false
///          @param v Variant value to convert
///          @param Value is the variant's LongWord value, only valid if the function
///                       returns true.
///          @returns true, if the variant could be converted to integer, false if not. </summary>
function TryVar2LongWord(const _v: Variant; out _Value: LongWord): Boolean;

///<summary> Converts a variant to an integer.
///          If v is null or empty, it returns the Default.
///          @param v Variant value to convert
///          @param Default Value to return if v is empty or null
///          @returns the integer value of v or the Default if v can not be converted </summary>
function Var2Int(const _v: Variant; _Default: Integer): Integer;
function Var2Int64(const _v: Variant; _Default: Int64): Int64;

///<summary> Converts a variant to an integer.
///          Raises an exception if v can not be converted.
///          @param v Variant value to convert
///          @param Source string to include in the exception message
///          @returns the integer value of v
///          @raises EVarIsNull if v is null
///          @raises EVarIsEmpty if v is empty
///          @raises EVariantConvertError if there is some other conversion error </summary>
function Var2IntEx(const _v: Variant; const _Source: string): Integer;

///<summary> tries to convert a variant to a boolean
///          @param b contains the value if the conversion succeeds
///          @returns true on success, false otherwise </summary>
function TryVar2Bool(const _v: Variant; out _b: Boolean): Boolean;

///<summary> Converts a variant to a boolean </summary>
function Var2BoolEx(const _v: Variant; const _Source: string): Boolean;

///<summary> Converts a variant to the string representation of an integer.
///          If v is null or empty, it returns the NullValue.
///          @param v Variant value to convert
///          @param NullValue String value to return if v is empty or null
///          @returns the string representation of the integer value of v or the
///                   NullValue if v can not be converted </summary>
function Var2IntStr(const _v: Variant; const _NullValue: string = '*NULL*'): string;

///<summary> tries to convert a variant to a single
///          If v is null or empty, it returns false.
///          @param v Variant value to convert
///          @param Value is the variant's single value, only valid if the function
///                       returns true.
///          @returns true, if the variant could be converted to single, false if not
///          @raises EVariantConvertError if there is some other conversion error </summary>
function TryVar2Single(const _v: Variant; out _Value: Single): Boolean;

///<summary> tries to convert a variant to a double
///          If v is null or empty, it returns false.
///          @param v Variant value to convert
///          @param Value is the variant's double value, only valid if the function
///                       returns true.
///          @returns true, if the variant could be converted to double, false if not
///          @raises EVariantConvertError if there is some other conversion error </summary>
function TryVar2Dbl(const _v: Variant; out _Value: Double): Boolean;

///<summary> Converts a variant to a double.
///          If v is null or empty, it returns the Default.
///          @param v Variant value to convert
///          @param Default Value to return if v is empty or null
///          @returns the double value of v or the Default if v can not be converted </summary>
function Var2Dbl(const _v: Variant; const _Default: Double): Double;

///<summary> Converts a variant to a double.
///          Raises an exception if v can not be converted.
///          @param v Variant value to convert
///          @param Source string to include in the exception message
///          @returns the double value of v
///          @raises EVarIsNull if v is null
///          @raises EVarIsEmpty if v is empty
///          @raises EVariantConvertError if there is some other conversion error </summary>
function Var2DblEx(const _v: Variant; const _Source: string): Double;

///<summary> Converts a variant to the string representation of a double.
///          If v is null or empty, it returns the Default.
///          It uses Float2Str (not FloatToStr) with a '.' as decimal separator.
///          @param v Variant value to convert
///          @param NullValue String value to return if v is empty or null
///          @returns the string representation of the double value of v or the
///                   NullValue if v can not be converted </summary>
function Var2DblStr(const _v: Variant; const _NullValue: string = '*NULL*'): string;

///<summary> tries to convert a variant to an extended
///          If v is null or empty, it returns false.
///          @param v Variant value to convert
///          @param Value is the variant's extended value, only valid if the function
///                       returns true.
///          @returns true, if the variant could be converted to extended, false if not
///          @raises EVariantConvertError if there is some other conversion error </summary>
function TryVar2Ext(const _v: Variant; out _Value: Extended): Boolean;

///<summary> Converts a variant to an extended.
///          Raises an exception if v can not be converted.
///          @param v Variant value to convert
///          @param Source string to include in the exception message
///          @returns the extended value of v
///          @raises EVarIsNull if v is null
///          @raises EVarIsEmpty if v is empty
///          @raises EVariantConvertError if there is some other conversion error </summary>
function Var2ExtEx(const _v: Variant; const _Source: string): Extended;

///<summary> Converts a variant to an extended.
///          If v is null or empty, it returns the Default.
///          @param v Variant value to convert
///          @param Default Value to return if v is empty or null
///          @returns the extended value of v or the Default if v can not be converted </summary>
function Var2Ext(const _v: Variant; const _Default: Extended): Extended;

{$IF Declared(TryStr2DateTime)}
///<summary> Converts a variant to a TDateTime.
///          Raises an exception if v can not be converted.
///          @param v Variant value to convert
///          @param Source string to include in the exception message
///          @returns the TDateTime value of v
///          @raises EVarIsNull if v is null
///          @raises EVarIsEmpty if v is empty
///          @raises EVariantConvertError if there is some other conversion error </summary>
function Var2DateTimeEx(const _v: Variant; const _Source: string): TDateTime;
{$IFEND}

{$IF Declared(TryStr2DateTime)}
function TryVar2DateTime(const _v: Variant; out _dt: TDateTime): Boolean;
{$IFEND}

///<summary> Converts a variant to an ISO format DateTime string (yyyy-mm-dd hh:mm:ss)
///          @param v Variant value to convert
///          @param NullValue String value to return if v is empty or null
///          @returns an ISO format DateTime string of v or NullValue if v can not be converted </summary>
function Var2DateTimeStr(const _v: Variant; const _NullValue: string = '*NULL*'): string;

///<summary> Converts a variant to an ISO format Date string (yyyy-mm-dd)
///          @param v Variant value to convert
///          @param NullValue String value to return if v is empty or null
///          @returns an ISO format Date string of v or NullValue if v can not be converted </summary>
function Var2DateStr(const _v: Variant; const _NullValue: string = '*NULL*'): string;

{$IF Declared(Str2Date)}
///<summary> Converts a variant to an ISO format Date string (yyyy-mm-dd)
///          @param v Variant value to convert
///          @param Source string to include in the exception message
///          @raises EVarIsNull if v is null
///          @raises EVarIsEmpty if v is empty
///          @raises EVariantConvertError if there is some other conversion error </summary>
///          @returns an ISO format Date string of v or NullValue if v can not be converted </summary>
function Var2DateStrEx(const _v: Variant; const _Source: string): string;
{$IFEND}

///<summary> Converts a variant to a string
///          If v is null or empty, it returns false.
///          @param v Variant value to convert
///          @param Value is the variant's string value, only valid if the function
///                       returns true.
///          @returns true, if the variant could be converted to double, false if not </summary>
function TryVar2Str(const _v: Variant; out _Value: string): Boolean;

///<summary> Converts a variant to a string.
///          If v is null or empty, it returns the Default.
///          @param v Variant value to convert
///          @param Default Value to return if v is empty or null
///          @returns the string value of v or the Default if v can not be converted </summary>
function Var2Str(const _v: Variant; const _Default: string = '*NULL*'): string;

///<summary> Converts a variant to a string.
///          Raises an exception if v can not be converted.
///          @param v Variant value to convert
///          @param Source string to include in the exception message
///          @returns the string value of v
///          @raises EVarIsNull if v is null
///          @raises EVarIsEmpty if v is empty
///          @raises EVariantConvertError if there is some other conversion error </summary>
function Var2StrEx(_v: Variant; const _Source: string): string;

///<summary>
/// Checks whether two variants are exactly the same, that is:
/// * they are the same type    AND
/// * they have the same value
/// Note that VarIsSame('3', 3) will return false, because the variant types are different.
function VarIsSame(_A, _B: Variant): Boolean;

///<summary>
/// converts a TVarRec (the type used in an array of const) to a string for debugging purposes </summary>
function TVarRec_ToString(const _v: TVarRec): string;

///<summary>
/// converts a array of const to a string of the form 'value0, value1, ..., valueN' for debugging purposes </summary>
function ArrayOfConst2String(const _arr: array of const): string;

implementation

uses
  DateUtils,
  u_dzConvertUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

function DumpVariant(_v: OleVariant): string;
var
  vt: TVarType;
  i, j: Integer;
begin
  try
    vt := VarType(_v);
    case vt of
      varEmpty: Result := '<Empty>'; // do not translate
      varNull: Result := '<Null>'; // do not translate
      varSmallint: Result := VarToStr(_v);
      varInteger: Result := VarToStr(_v);
      varSingle: Result := VarToStr(_v);
      varDouble: Result := VarToStr(_v);
      varCurrency: Result := VarToStr(_v);
      varDate: Result := VarToStr(_v);
      varOleStr: Result := VarToStr(_v);
      varDispatch: Result := VarToStr(_v);
      varString: Result := VarToStr(_v);
    else
      if (vt and varArray) = varArray then begin
        if VarArrayDimCount(_v) = 1 then begin
          for i := VarArrayLowBound(_v, 1) to VarArrayHighBound(_v, 1) do begin
            if Result <> '' then
              Result := Result + ' | ';
            Result := Result + DumpVariant(_v[i]);
          end;
        end else if VarArrayDimCount(_v) = 2 then begin
          for i := VarArrayLowBound(_v, 1) to VarArrayHighBound(_v, 1) do
            for j := VarArrayLowBound(_v, 1) to VarArrayHighBound(_v, 1) do
              Result := Result + DumpVariant(_v[i, j]);
        end else
          Result := '3dim-array not supported'; // do not translate
      end else
        Result := '<Unknown Type>'; // do not translate
    end;
    Result := Result + ' (' + VarTypeAsText(VarType(_v)) + ')';
  except
    on ex: Exception do
      Result := Result + '#ERROR: ' + ex.Message; // do not translate
  end;
end;

function ToString(_v: OleVariant): string;
begin
  Result := DumpVariant(_v);
end;

function VarIsNullOrEmpty(_v: Variant): Boolean;
begin
  Result := VarIsNull(_v) or VarIsEmpty(_v);
end;

function VarIsInteger(_v: Variant): Boolean;
begin
  Result := VarIsType(_v, [varSmallint, varInteger, varShortInt, varByte, varWord]);
end;

function VarIsLongWord(_v: Variant): Boolean;
begin
  Result := VarIsType(_v, [varByte, varWord, varLongWord]);
end;

function VarIsInt64(_v: Variant): Boolean;
begin
  Result := VarIsType(_v, [varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64]);
end;

function TryVar2LongWord(const _v: Variant; out _Value: LongWord): Boolean;
begin
  Result := not VarIsNull(_v) and not VarIsEmpty(_v);
  if Result then
    try
      _Value := _v;
    except
      on e: EVariantError do
        Result := False;
    end;
end;

function TryVar2Int(const _v: Variant; out _Value: Integer): Boolean;
begin
  Result := not VarIsNull(_v) and not VarIsEmpty(_v);
  if Result then
    try
      _Value := _v;
    except
      on e: EVariantError do
        Result := False;
    end;
end;

function TryVar2Int64(const _v: Variant; out _Value: Int64): Boolean;
begin
  Result := not VarIsNull(_v) and not VarIsEmpty(_v);
  if Result then
    try
      _Value := _v;
    except
      on e: EVariantError do
        Result := False;
    end;
end;

function Var2Int(const _v: Variant; _Default: Integer): Integer;
begin
  if not TryVar2Int(_v, Result) then
    Result := _Default;
end;

function Var2Int64(const _v: Variant; _Default: Int64): Int64;
begin
  if not TryVar2Int64(_v, Result) then
    Result := _Default;
end;

function Var2IntEx(const _v: Variant; const _Source: string): Integer;
const
  EXPECTED = 'Integer'; // do not translate
begin
  if VarIsNull(_v) then
    raise EVarIsNull.CreateFmt(_('Variant is Null, should be %s: %s'), [EXPECTED, _Source]);
  if VarIsEmpty(_v) then
    raise EVarIsEmpty.CreateFmt(_('Variant is Empty, should be %s: %s'), [EXPECTED, _Source]);
  try
    Result := _v;
  except
    on e: EVariantError do
      raise EVariantConvertError.CreateFmt(_('Variant can not be converted to %s: %s'), [EXPECTED, _Source]);
  end;
end;

function TryVar2Bool(const _v: Variant; out _b: Boolean): Boolean;
begin
  Result := not VarIsNull(_v) and not VarIsEmpty(_v);
  if Result then begin
    try
      _b := _v;
    except
      on e: EVariantError do
        Result := False;
    end;
  end;
end;

function Var2BoolEx(const _v: Variant; const _Source: string): Boolean;
const
  EXPECTED = 'Boolean'; // do not translate
begin
  if VarIsNull(_v) then
    raise EVarIsNull.CreateFmt(_('Variant is Null, should be %s: %s'), [EXPECTED, _Source]);
  if VarIsEmpty(_v) then
    raise EVarIsEmpty.CreateFmt(_('Variant is Empty, should be %s: %s'), [EXPECTED, _Source]);
  try
    Result := _v;
  except
    on e: EVariantError do
      raise EVariantConvertError.CreateFmt(_('Variant can not be converted to %s: %s'), [EXPECTED, _Source]);
  end;
end;

function Var2IntStr(const _v: Variant; const _NullValue: string = '*NULL*'): string;
var
  Value: Integer;
begin
  if TryVar2Int(_v, Value) then
    Result := IntToStr(Value)
  else
    Result := _NullValue;
end;

{$IF Declared(TryStr2DateTime)}
function Var2DateTimeEx(const _v: Variant; const _Source: string): TDateTime;
const
  EXPECTED = 'DateTime'; // do not translate
begin
  if VarIsNull(_v) then
    raise EVarIsNull.CreateFmt(_('Variant is Null, should be %s: %s'), [EXPECTED, _Source]);
  if VarIsEmpty(_v) then
    raise EVarIsEmpty.CreateFmt(_('Variant is Empty, should be %s: %s'), [EXPECTED, _Source]);
  if VarIsStr(_v) then begin
    if not TryStr2DateTime(_v, Result) then
      raise EVariantConvertError.CreateFmt(_('Variant can not be converted to %s: %s'), [EXPECTED, _Source]);
  end else begin
    try
      Result := _v;
    except
      on e: EVariantError do
        raise EVariantConvertError.CreateFmt(_('Variant can not be converted to %s: %s'), [EXPECTED, _Source]);
    end;
  end;
end;
{$IFEND}

{$IF Declared(TryStr2DateTime)}
function TryVar2DateTime(const _v: Variant; out _dt: TDateTime): Boolean;

  // from Variants
  function VarToDoubleCustom(const V: TVarData; out AValue: Double): Boolean;
  var
    LHandler: TCustomVariantType;
    LTemp: TVarData;
  begin
    Result := FindCustomVariantType(V.VType, LHandler);
    if Result then begin
      VariantInit(LTemp);
      LHandler.CastTo(LTemp, V, varDouble);
      AValue := LTemp.VDouble;
    end;
  end;

var
  d: Double;
begin
  Result := VarIsType(_v, varDate);
  if Result then
    _dt := VarToDateTime(_v)
  else if VarIsStr(_v) then
    Result := TryStr2DateTime(_v, _dt)
  else begin
    Result := VarToDoubleCustom(TVarData(_v), d);
    _dt := d;
  end;
end;
{$IFEND}

function Var2DateTimeStr(const _v: Variant; const _NullValue: string = '*NULL*'): string;
var
  Value: TDateTime;
begin
  if VarIsNull(_v) or VarIsEmpty(_v) then
    Result := _NullValue
  else
    try
      Value := _v;
      Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Value); // do not translate
    except
      Result := _NullValue;
    end;
end;

function Var2DateStr(const _v: Variant; const _NullValue: string = '*NULL*'): string;
var
  Value: TDateTime;
begin
  if VarIsNull(_v) or VarIsEmpty(_v) then
    Result := _NullValue
  else
    try
      Value := _v;
      Result := FormatDateTime('yyyy-mm-dd', Value); // do not translate
    except
      Result := _NullValue;
    end;
end;

{$IF Declared(Str2Date)}
function Var2DateStrEx(const _v: Variant; const _Source: string): string;
const
  EXPECTED = 'Date'; // do not translate
var
  Value: TDateTime;
begin
  if VarIsNull(_v) then
    raise EVarIsNull.CreateFmt(_('Variant is Null, should be %s: %s'), [EXPECTED, _Source]);
  if VarIsEmpty(_v) then
    raise EVarIsEmpty.CreateFmt(_('Variant is Empty, should be %s: %s'), [EXPECTED, _Source]);
  if VarIsStr(_v) then begin
    Value := Str2Date(_v);
  end else begin
    try
      Value := _v;
    except
      on e: EVariantError do
        raise EVariantConvertError.CreateFmt(_('Variant can not be converted to %s: %s'), [EXPECTED, _Source]);
    end;
  end;

  Result := FormatDateTime('yyyy-mm-dd', DateOf(Value)); // do not translate
end;
{$IFEND}

function TryVar2Single(const _v: Variant; out _Value: Single): Boolean;
const
  EXPECTED = 'single'; // do not translate
begin
  Result := not VarIsNull(_v) and not VarIsEmpty(_v);
  if Result then
    try
      _Value := _v;
    except
      on e: EVariantError do
        raise EVariantConvertError.CreateFmt(_('Variant can not be converted to %s'), [EXPECTED]);
    end;
end;

function TryVar2Dbl(const _v: Variant; out _Value: Double): Boolean;
const
  EXPECTED = 'double'; // do not translate
begin
  Result := not VarIsNull(_v) and not VarIsEmpty(_v);
  if Result then
    try
      _Value := _v;
    except
      on e: EVariantError do
        raise EVariantConvertError.CreateFmt(_('Variant can not be converted to %s'), [EXPECTED]);
    end;
end;

function TryVar2Ext(const _v: Variant; out _Value: Extended): Boolean;
const
  EXPECTED = 'extended'; // do not translate
begin
  Result := not VarIsNull(_v) and not VarIsEmpty(_v);
  if Result then
    try
      _Value := _v;
    except
      on e: EVariantError do
        raise EVariantConvertError.CreateFmt(_('Variant can not be converted to %s'), [EXPECTED]);
    end;
end;

function Var2Dbl(const _v: Variant; const _Default: Double): Double;
begin
  if not TryVar2Dbl(_v, Result) then
    Result := _Default
end;

function Var2DblEx(const _v: Variant; const _Source: string): Double;
const
  EXPECTED = 'double'; // do not translate
begin
  if VarIsNull(_v) then
    raise EVarIsNull.CreateFmt(_('Variant is Null, should be %s: %s'), [EXPECTED, _Source]);
  if VarIsEmpty(_v) then
    raise EVarIsEmpty.CreateFmt(_('Variant is Empty, should be %s: %s'), [EXPECTED, _Source]);
  try
    Result := _v;
  except
    on e: EVariantError do
      raise EVariantConvertError.CreateFmt(_('Variant can not be converted to %s: %s'), [EXPECTED, _Source]);
  end;
end;

function Var2ExtEx(const _v: Variant; const _Source: string): Extended;
const
  EXPECTED = 'extended'; // do not translate
begin
  if VarIsNull(_v) then
    raise EVarIsNull.CreateFmt(_('Variant is Null, should be %s: %s'), [EXPECTED, _Source]);
  if VarIsEmpty(_v) then
    raise EVarIsEmpty.CreateFmt(_('Variant is Empty, should be %s: %s'), [EXPECTED, _Source]);
  try
    Result := _v;
  except
    on e: EVariantError do
      raise EVariantConvertError.CreateFmt(_('Variant can not be converted to %s: %s'), [EXPECTED, _Source]);
  end;
end;

function Var2Ext(const _v: Variant; const _Default: Extended): Extended;
begin
  if not TryVar2Ext(_v, Result) then
    Result := _Default
end;

function Var2DblStr(const _v: Variant; const _NullValue: string = '*NULL*'): string;
var
  Value: Double;
begin
  if TryVar2Dbl(_v, Value) then
    Result := Float2Str(Value)
  else
    Result := _NullValue;
end;

function TryVar2Str(const _v: Variant; out _Value: string): Boolean;
const
  EXPECTED = 'String'; // do not translate
begin
  Result := not VarIsNull(_v) and not VarIsEmpty(_v);
  if Result then
    try
      _Value := _v;
    except
      on e: EVariantError do
        raise EVariantConvertError.CreateFmt(_('Variant can not be converted to %s'), [EXPECTED]);
    end;
end;

function Var2Str(const _v: Variant; const _Default: string): string;
begin
  if not TryVar2Str(_v, Result) then
    Result := _Default
end;

function Var2StrEx(_v: Variant; const _Source: string): string;
const
  EXPECTED = 'string'; // do not translate
begin
  if VarIsNull(_v) then
    raise EVarIsNull.CreateFmt(_('Variant is Null, should be %s: %s'), [EXPECTED, _Source]);
  if VarIsEmpty(_v) then
    raise EVarIsEmpty.CreateFmt(_('Variant is Empty, should be %s: %s'), [EXPECTED, _Source]);
  try
    Result := _v;
  except
    on e: EVariantError do
      raise EVariantConvertError.CreateFmt(_('Variant can not be converted to %s: %s'), [EXPECTED, _Source]);
  end;
end;

function VarIsSame(_A, _B: Variant): Boolean;
var
  LA, LB: TVarData;
begin
  LA := FindVarData(_A)^;
  LB := FindVarData(_B)^;
  if LA.VType <> LB.VType then
    Result := False
  else
    Result := (_A = _B);
end;

function TVarRec_ToString(const _v: TVarRec): string;

  function Object2Str(_obj: TObject): string;
  begin
    if Assigned(_obj) then
      Result := Format('%s($%p)', [_obj.ClassName, Pointer(_obj)])
    else
      Result := 'TObject(nil)';
  end;

  function Class2Str(_cls: TClass): string;
  begin
    if Assigned(_cls) then
      Result := 'class(' + _cls.ClassName + ')'
    else
      Result := 'class(nil)';
  end;

begin
  try
    case _v.VType of
      vtInteger:
        Result := IntToStr(_v.VInteger);
      vtBoolean: begin
          if _v.VBoolean then
            Result := 'True'
          else
            Result := 'False';
        end;
      vtChar: Result := Char(_v.VChar);
      vtExtended: Result := Format('%g', [_v.VExtended^]);
      vtString: Result := '''' + string(_v.VString^) + '''';
      vtPointer: Result := Format('$%p', [_v.VPointer]);
      vtPChar: Result := string(_v.VPChar);
      vtObject: Result := Object2Str(_v.VObject);
      vtClass: Result := Class2Str(_v.VClass);
{$IF Declared(vtWideChar)}
      vtWideChar: Result := _v.VWideChar;
{$IFEND}
{$IF declared(vtPWideChar)}
      vtPWideChar: Result := string(WideString(_v.VPWideChar));
{$IFEND}
      vtAnsiString: Result := '''' + string(PAnsiString(_v.VAnsiString)) + '''';
{$IF declared(vtCurrency)}
      vtCurrency: Result := CurrToStr(_v.VCurrency^);
{$IFEND}
{$IF declared(vtVariant)}
      vtvariant: Result := Var2Str(_v.VVariant^);
{$IFEND}
{$IF declared(vtInterface)}
      vtInterface: Result := Format('Intf($%p)', [_v.VInterface]);
{$IFEND}
      vtWideString: Result := '''' + string(PWideString(_v.VWideString)) + '''';
      vtInt64: Result := IntToStr(_v.VInt64^);
{$IF declared(vtUnicodeString)}
      vtUnicodeString: Result := '''' + string(_v.VUnicodeString) + '''';
{$IFEND}
    else
      Result := '<unsupported type ' + IntToStr(_v.VType) + '>';
    end;
  except
    on e: Exception do begin
      Result := '<conversion of type ' + IntToStr(_v.VType) + ' failed>';
    end;
  end;
end;

function ArrayOfConst2String(const _arr: array of const): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(_arr) - 1 do begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + TVarRec_ToString(_arr[i]);
  end;
end;

end.

