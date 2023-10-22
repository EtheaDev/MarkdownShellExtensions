{.GXFormatter.config=twm}
///<summary>
/// Implements commonly used functions.
/// This unit implements some commonly used functions.
///<br/>
/// There is also a NotImplemend procedure which should be called
/// whereever some features are left out to be implemented "later"
/// This procedure will not be available when we compile the
/// shipping code (no DEBUG symbol), so the compiler should
/// complain if it is still used by then.
/// <br>
/// Note: String functions have been moved to dzStringUtils
/// Note: Variant functions have been moved to dzVariantUtils
/// @author twm
///</summary>

unit u_dzMiscUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils,
  Registry,
  u_dzTranslator,
  u_dzTypes;

type
  ///<summary> raised by Max([array of const]) and Min([array of const]) if the passed
  ///   paramter is empty </summary>
  EEmptyArray = class(Exception);

  EPathTooLong = class(Exception);

{$IFDEF debug}
  // do not remove the ifdef!!!
  ENotImplemented = class(Exception);
{$ENDIF}

type
  TBooleanNames = array[Boolean] of string;
const
  BOOLEAN_NAMES: TBooleanNames = ('false', 'true'); // do not translate

///<summary> Emulates this infamous Visual Basic function of which nobody actually knows
///          what it does. </summary>
function TwipsPerPixelX(_Handle: hdc): Extended;

///<summary> Emulates this infamous Visual Basic function of which nobody actually knows
///          what it does. </summary>
function TwipsPerPixelY(_Handle: hdc): Extended;

///<summary> Returns the name for the HKey constant. </summary>
function HKeyToString(_HKey: HKey): string;

///<summary> Returns the name for the TRegDataType Value. </summary>
function RegDataTypeToString(_DataType: TRegDataType): string;

///<summary> returns a hex dump of the buffer (no spaces added)
///          @param Buffer is the memory block to dump
///          @param Len is the length of the block
///          @returns a string containing the hex dump of the buffer </summary>
function HexDump(const _Buffer; _Len: Integer): string;

///<summary> hex dumps a double value </summary>
function HexDumpDouble(const _dbl: Double): string;

///<summary> hex dumps an extended value </summary>
function HexDumpExtended(const _ext: Extended): string;

///<summary> returns a hex dump of the string s </summary>
function HexDumpString(const _s: AnsiString): string;

///<summary> converts a hexdump of a double back to a double value </summary>
procedure HexDumpToDbl(const _s: string; var _Value: Double); overload;
{$IFDEF SUPPORTS_UNICODE}
procedure HexDumpToDbl(const _s: AnsiString; var _Value: Double); overload;
{$ENDIF SUPPORTS_UNICODE}

///<summary> converts a hexdump of an extended back to an extended value </summary>
procedure HexDumpToExtended(const _s: string; var _Value: Extended);

{$IFNDEF HAS_INTTOHEX_FUNCTION}
///<summary> converts an integer to a 8 digit hex string </summary>
function IntToHex(_Value: Integer): string; overload;

///<summary> converts an In64 to a 16 digit hex string </summary>
function IntToHex(_Value: Int64): string; overload;
{$ENDIF HAS_INTTOHEX_FUNCTION}

{$IFDEF SUPPORTS_UINT64}
{$IFNDEF HAS_INTTOHEX_FUNCTION_UINT64}
function IntToHex(_Value: UInt64): string; overload;
{$ENDIF HAS_INTTOHEX_FUNCTION_64}
{$ENDIF SUPPORTS_UINT64}

function PtrToHex(_Value: Pointer): string;

///<summary> Converts an integer to a boolean.
///          @param Int is the integer to convert
///          @returns false, if the integer is 0, true otherwise </summary>
function IntToBool(_Int: Integer): Boolean;

///<summary> Converts a boolean to an integer.
///          @param B is the boolean to convert
///          @returns 0 if the boolean is false, 1 if it is true </summary>
function BoolToInt(_B: Boolean): Integer;

///<summary> Uses GetLastError to get the last WinAPI error code, then
///          calls SysErrorMessage to get the corresponding error string,
///          optionally takes a format string.
///          @param Error is the error string, only valid if error code <> 0
///          @param Format The Format string to use. It must have %d and %s in it, to
///                        change the order, use %0:d and %1:s, e.g. 'Error %1:s (%0:d)'
///                        %d is replaced by the error code and %s is replaced by the
///                        error message string.
///                        If no format string is given Error will just contain the
///                        Windows error message.
///                        NOTE: Do not pass a resource string or a string translated
///                              using DxGetText to this function since this
///                              would clear the GetLastError result. Use the
///                              overloaded version that takes the ErrCode
///                              parameter instead.
///          @returns the error code /</summary>
function GetLastOsError(out _Error: string; const _Format: string = ''): DWORD; overload;
function GetLastOsError(_ErrCode: Integer; out _Error: string; const _Format: string = ''): DWORD; overload;

///<summary> Similar to SysUtils.Win32Check, but does not raise an exception. Instead
///          it returns the error message. The function optionally takes a format string.
///          @param RetVal is the return value of a WinAPI function
///          @param ErrorCode is the error code returned by GetLastError
///          @param Error is the error message corresponding to the error code (only valid if result <> 0)
///          @param Format The Format string to use. It must have %d and %s in it, to
///                        change the order, use %0:d and %1:s, e.g. 'Error %1:s (%0:d)'
///                        %d is replaced by the error code and %s is replaced by the
///                        error message string.
///                        If no format string is given Error will just contain the
///                        Windows error message.
///                        NOTE: Do not pass a resource string or a string translated
///                              using DxGetText to this function since this
///                              would clear the GetLastError result.
///          @Returns the error code </summary>
function Win32CheckEx(_RetVal: BOOL; out _ErrorCode: DWORD; out _Error: string; const _Format: string = ''): BOOL;

///<summary> Same as VCL RaiseLastWin32Error but can specify a format.
///          This procedure does the same as the VCL RaiseLastWin32Error but you can
///          specify a format string to use. With this string you can provide some
///          additional information about where the error occured.
///          It calls GetLastError to get the result code of the last Win32 api function.
///          If it returns non 0 the function uses SysErrorMessage to retrieve an error
///          message for the error code and raises raises an EWin32Error exception
///          (to be compatible with the VCL function) with the Error message.
///          NOTE: Do not pass a resource string as format parameter, since loading this
///                string will reset the error code returned by GetLastError, so
///                you always get 0. Use the overloaded Version that takes the error code
///                as parameter and get it before using the resource string if you want that.
///          @param Format The Format string to use. It must have %d and %s in it, to
///                        change the order, use %0:d and %1:s, e.g. 'Error %1:s (%0:d)'
///                        %d is replaced by the error code and %s is replaced by the
///                        error message string. </summary>
procedure RaiseLastOsErrorEx(const _Format: string); overload;

///<summary> Same as VCL RaiseLastWin32Error but can specify a format.
///          This procedure does the same as the VCL RaiseLastWin32Error but you can
///          specify a format string to use. With this string you can provide some
///          additional information about where the error occured.
///          If ErrorCode <> 0 the function uses SysErrorMessage to retrieve an error
///          message for the error code and raises raises an EWin32Error exception
///          (to be compatible with the VCL function) with the Error message.
///          NOTE: If you pass a resource string as format parameter make sure you
///          call GetLastError before referencing the resource string, otherwise
///          loading the string will reset the error code returned by GetLastError, so
///          you always get 0.
///          @param ErrorCode is an error code returned from GetLastWin32Error
///          @param Format The Format string to use. It must have %d and %s in it, to
///                        change the order, use %0:d and %1:s, e.g. 'Error %1:s (%0:d)'
///                        %d is replaced by the error code and %s is replaced by the
///                        error message string. </summary>
procedure RaiseLastOsErrorEx(_ErrorCode: Integer; const _Format: string); overload;

///<summary> Combines WriteLn with Format
///          @param FormatStr string describing the format
///          @param Args constant array with the arguments </summary>
procedure WriteFmtLn(const _FormatStr: string; _Args: array of const);

///<summary> splits a wildcard into its components: The path and the filemask
///          @param Wildcard is a string specifying the wildcard
///          @param Path is a string returning the path part of the wildcard
///          @param Mask is a string returning the Mask part of the wildcard
///          @returns true, if the Path exists, false otherwise </summary>
function SplitWildcard(const _Wildcard: string; out _Path, _Mask: string): Boolean;

{: returns the string's reference counter, pass a string by typecasting it
   to a pointer to avoid an additional increment of the reference counter }
function GetStringRefCount(_s: Pointer): Integer;

procedure InitializeNil(var _Obj1); overload;
procedure InitializeNil(var _Obj1, _Obj2); overload;
procedure InitializeNil(var _Obj1, _Obj2, _Obj3); overload;
procedure InitializeNil(var _Obj1, _Obj2, _Obj3, _Obj4); overload;
procedure InitializeNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5); overload;
procedure InitializeNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6); overload;
procedure InitializeNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6, _Obj7); overload;
procedure InitializeNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6, _Obj7, _Obj8); overload;
procedure InitializeNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6, _Obj7, _Obj8, _Obj9); overload;

procedure FreeAndNil(var _Obj1); overload;
procedure FreeAndNil(var _Obj1, _Obj2); overload;
procedure FreeAndNil(var _Obj1, _Obj2, _Obj3); overload;
procedure FreeAndNil(var _Obj1, _Obj2, _Obj3, _Obj4); overload;
procedure FreeAndNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5); overload;
procedure FreeAndNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6); overload;
procedure FreeAndNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6, _Obj7); overload;
procedure FreeAndNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6, _Obj7, _Obj8); overload;
procedure FreeAndNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6, _Obj7, _Obj8, _Obj9); overload;

{$IFDEF debug}
// do NOT remove, this is a sanity check so we don't ship anything where there are
// missing features.
procedure NotImplemented;
{$ENDIF debug}

implementation

uses
  StrUtils,
  u_dzFileUtils,
  u_dzStringUtils,
  u_dzConvertUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{$IFDEF debug}

procedure NotImplemented;
begin
  if ID_YES <> Windows.MessageBox(0, 'Function not implemented! Continue anyway?', 'Warning', MB_YESNO + MB_ICONWARNING) then
    raise ENotImplemented.Create('Function not implemented');
end;
{$ENDIF debug}

function TwipsPerPixelX(_Handle: hdc): Extended;
var
  Pixels: Integer;
begin
  Pixels := GetDeviceCaps(_Handle, LOGPIXELSX);
  if Pixels = 0 then
    Result := 0
  else
    Result := 1440 / Pixels;
end;

function TwipsPerPixelY(_Handle: hdc): Extended;
var
  Pixels: Integer;
begin
  Pixels := GetDeviceCaps(_Handle, LOGPIXELSY);
  if Pixels = 0 then
    Result := 0
  else
    Result := 1440 / Pixels;
end;

function HKeyToString(_HKey: HKey): string;
begin
  {$IFDEF Win64}
  case _HKey of
    Integer(HKEY_CLASSES_ROOT): Result := 'HKEY_CLASSES_ROOT'; // do not translate
    Integer(HKEY_CURRENT_USER): Result := 'HKEY_CURRENT_USER'; // do not translate
    Integer(HKEY_LOCAL_MACHINE): Result := 'HKEY_LOCAL_MACHINE'; // do not translate
    Integer(HKEY_USERS): Result := 'HKEY_USERS'; // do not translate
    Integer(HKEY_PERFORMANCE_DATA): Result := 'HKEY_PERFORMANCE_DATA'; // do not translate
    Integer(HKEY_CURRENT_CONFIG): Result := 'HKEY_CURRENT_CONFIG'; // do not translate
    Integer(HKEY_DYN_DATA): Result := 'HKEY_DYN_DATA'; // do not translate
  else
    Result := Format(_('unknown Registry Root Key %x'), [_HKey]);
  end;
  {$ELSE}
  case _HKey of
    HKEY_CLASSES_ROOT: Result := 'HKEY_CLASSES_ROOT'; // do not translate
    HKEY_CURRENT_USER: Result := 'HKEY_CURRENT_USER'; // do not translate
    HKEY_LOCAL_MACHINE: Result := 'HKEY_LOCAL_MACHINE'; // do not translate
    HKEY_USERS: Result := 'HKEY_USERS'; // do not translate
    HKEY_PERFORMANCE_DATA: Result := 'HKEY_PERFORMANCE_DATA'; // do not translate
    HKEY_CURRENT_CONFIG: Result := 'HKEY_CURRENT_CONFIG'; // do not translate
    HKEY_DYN_DATA: Result := 'HKEY_DYN_DATA'; // do not translate
  else
    Result := Format(_('unknown Registry Root Key %x'), [_HKey]);
  end;
  {$ENDIF}
end;

function RegDataTypeToString(_DataType: TRegDataType): string;
begin
  case _DataType of
    rdUnknown: Result := 'Unknown'; // do not translate
    rdString: Result := 'String'; // do not translate
    rdExpandString: Result := 'ExpandString'; // do not translate
    rdInteger: Result := 'Integer'; // do not translate
    rdBinary: Result := 'Binary'; // do not translate
  else
    Result := _('unknown RegDataType');
  end;
end;

function IntToBool(_Int: Integer): Boolean;
begin
  Result := (_Int <> 0);
end;

function BoolToInt(_B: Boolean): Integer;
begin
  if _B then
    Result := 1
  else
    Result := 0;
end;

procedure RaiseLastOsErrorEx(const _Format: string);
begin
  RaiseLastOsErrorEx(GetLastError, _Format);
end;

procedure RaiseLastOsErrorEx(_ErrorCode: Integer; const _Format: string); overload;
var
  Error: EOSError;
begin
  if _ErrorCode <> ERROR_SUCCESS then
    Error := EOSError.CreateFmt(_Format, [_ErrorCode, SysErrorMessage(_ErrorCode)])
  else
    Error := EOSError.CreateFmt(_Format, [_ErrorCode, _('unknown OS error')]);
  Error.ErrorCode := _ErrorCode;
  raise Error;
end;

function GetLastOsError(out _Error: string; const _Format: string = ''): DWORD;
begin
  Result := GetLastOsError(GetLastError, _Error, _Format);
end;

function GetLastOsError(_ErrCode: Integer; out _Error: string; const _Format: string = ''): DWORD;
var
  s: string;
begin
  Result := _ErrCode;
  if Result <> ERROR_SUCCESS then
    s := SysErrorMessage(Result)
  else
    s := _('unknown OS error');
  if _Format <> '' then
    try
      _Error := Format(_Format, [Result, s])
    except
      _Error := s;
    end else
    _Error := s;
end;

function Win32CheckEx(_RetVal: BOOL; out _ErrorCode: DWORD; out _Error: string;
  const _Format: string = ''): BOOL;
begin
  Result := _RetVal;
  if not Result then
    _ErrorCode := GetLastOsError(_Error, _Format);
end;

procedure WriteFmtLn(const _FormatStr: string; _Args: array of const);
begin
  WriteLn(Format(_FormatStr, _Args));
end;

function SplitWildcard(const _Wildcard: string; out _Path, _Mask: string): Boolean;
var
  i: Integer;
  MaskFound: Boolean;
begin
  if _Wildcard = '' then begin
    _Path := '.';
    _Mask := '';
    Result := True;
    Exit; //==>
  end;
  MaskFound := False;
  i := Length(_Wildcard);
  while i > 0 do begin
    if CharInSet(_Wildcard[i], ['*', '?']) then
      MaskFound := True;
    if _Wildcard[i] = '\' then begin
      if MaskFound or not TFileSystem.DirExists(_Wildcard) then begin
        // if we had a mask, this is easy, just split the wildcard at position i
        // if there was no mask, and the whole thing is not a directory,
        // split at position i
        _Mask := TailStr(_Wildcard, i + 1);
        _Path := LeftStr(_Wildcard, i - 1);
        Result := TFileSystem.DirExists(_Path);
      end else begin
        // there was no mask and the whole thing is a directory
        Result := True;
        _Path := _Wildcard;
        _Mask := '';
      end;
      Exit; //==>
    end;
    Dec(i);
  end;

  // we found no backslash in the whole thing, so this could either
  // be a file or a subdirectory in the current directory.

  // if there was a mask, or the thing is not a directory, it is a file in
  // the current directory
  if MaskFound or not TFileSystem.DirExists(_Wildcard) then begin
    _Path := '.';
    _Mask := _Wildcard;
    Result := True;
  end else begin
    // otherwise it is a subdirectory
    _Path := _Wildcard;
    _Mask := '';
    Result := True;
  end;
end;

function iif(_Cond: Boolean; _IfTrue: Integer; _IfFalse: Integer): Integer; overload;
begin
  if _Cond then
    Result := _IfTrue
  else
    Result := _IfFalse;
end;

function iif(_Cond: Boolean; const _IfTrue: string; const _IfFalse: string): string; overload;
begin
  if _Cond then
    Result := _IfTrue
  else
    Result := _IfFalse;
end;

function iif(_Cond: Boolean; const _IfTrue: Double; const _IfFalse: Double): Double; overload;
begin
  if _Cond then
    Result := _IfTrue
  else
    Result := _IfFalse;
end;

function iif(_Cond: Boolean; const _IfTrue: Char; const _IfFalse: Char): Char; overload;
begin
  if _Cond then
    Result := _IfTrue
  else
    Result := _IfFalse;
end;

function HexDumpString(const _s: AnsiString): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(_s) do begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + Long2Hex2(Ord(_s[i]));
  end;
end;

function HexDump(const _Buffer; _Len: Integer): string;
type
  PByte = ^Byte;
var
  i: Integer;
  p: PByte;
begin
  p := @_Buffer;
  Result := '';
  for i := 0 to _Len - 1 do begin //FI:W528 - variable is only used for counting
    Result := Result + Long2Hex2(p^);
    Inc(p);
  end;
end;

function HexDumpDouble(const _dbl: Double): string;
begin
  Result := HexDump(_dbl, SizeOf(Double));
end;

function HexDumpExtended(const _ext: Extended): string;
begin
  Result := HexDump(_ext, SizeOf(Extended));
end;

procedure HexDumpToDbl(const _s: string; var _Value: Double);
type
  TBuffer = array[0..SizeOf(_Value)] of Byte;
var
  i: Integer;
  Dec: LongWord;
  p: ^TBuffer;
begin
  Assert(Length(_s) = SizeOf(_Value) * 2);
  p := Pointer(@_Value);
  for i := 0 to SizeOf(_Value) - 1 do begin
    Dec := Hex2Long(Copy(_s, i * 2 + 1, 2));
    p^[i] := Dec;
  end;
end;

{$IFDEF SUPPORTS_UNICODE}
procedure HexDumpToDbl(const _s: AnsiString; var _Value: Double);
begin
  HexDumpToDbl(string(_s), _Value);
end;
{$ENDIF SUPPORTS_UNICODE}

procedure HexDumpToExtended(const _s: string; var _Value: Extended);
type
  TBuffer = array[0..SizeOf(_Value)] of Byte;
var
  i: Integer;
  Dec: LongWord;
  p: ^TBuffer;
begin
  Assert(Length(_s) = SizeOf(_Value) * 2);
  p := Pointer(@_Value);
  for i := 0 to SizeOf(_Value) - 1 do begin
    Dec := Hex2Long(Copy(_s, i * 2 + 1, 2));
    p^[i] := Dec;
  end;
end;

{$IFNDEF HAS_INTTOHEX_FUNCTION}
function IntToHex(_Value: Integer): string;
begin
  Result := IntToHex(_Value, SizeOf(_Value) * 2);
end;

function IntToHex(_Value: Int64): string;
begin
  Result := IntToHex(_Value, SizeOf(_Value) * 2);
end;
{$ENDIF HAS_INTTOHEX_FUNCTION}

{$IFNDEF HAS_INTTOHEX_FUNCTION_UINT32}
function IntToHex(_Value: UInt32): string; overload;
begin
  Result := IntToHex(Int64(_Value), 8);
end;
{$ENDIF HAS_INTTOHEX_FUNCTION_UINT32}

{$IFDEF SUPPORTS_UINT64}
{$IFNDEF HAS_INTTOHEX_FUNCTION_UINT64}
function IntToHex(_Value: UInt64): string; overload;
var
  Buf: PUInt32;
begin
  Buf := PUInt32(UIntPtr(@_Value) + 8);
  Result := IntToHex(Buf^, 8);
  Buf := PUInt32(@_Value);
  Result := Result + IntToHex(Buf^, 8);
end;
{$ENDIF HAS_INTTOHEX_FUNCTION_UINT64}
{$ENDIF SUPPORTS_UINT64}

function PtrToHex(_Value: Pointer): string;
begin
  Result := IntToHex(UIntPtr(_Value));
end;

type
  PStringDescriptor = ^TStringDescriptor;
  TStringDescriptor = record
    RefCount: Integer;
    Size: Integer;
  end;

function GetStringRefCount(_s: Pointer): Integer;
var
  desc: PStringDescriptor;
begin
  if _s <> nil then begin
    desc := Pointer(Integer(_s) - 8);
    Result := desc.RefCount;
  end else
    Result := 0;
end;

procedure InitializeNil(var _Obj1);
begin
  Pointer(_Obj1) := nil;
end;

procedure InitializeNil(var _Obj1, _Obj2);
begin
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
end;

procedure InitializeNil(var _Obj1, _Obj2, _Obj3);
begin
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
end;

procedure InitializeNil(var _Obj1, _Obj2, _Obj3, _Obj4);
begin
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
  Pointer(_Obj4) := nil;
end;

procedure InitializeNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5);
begin
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
  Pointer(_Obj4) := nil;
  Pointer(_Obj5) := nil;
end;

procedure InitializeNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6);
begin
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
  Pointer(_Obj4) := nil;
  Pointer(_Obj5) := nil;
  Pointer(_Obj6) := nil;
end;

procedure InitializeNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6, _Obj7);
begin
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
  Pointer(_Obj4) := nil;
  Pointer(_Obj5) := nil;
  Pointer(_Obj6) := nil;
  Pointer(_Obj7) := nil;
end;

procedure InitializeNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6, _Obj7, _Obj8);
begin
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
  Pointer(_Obj4) := nil;
  Pointer(_Obj5) := nil;
  Pointer(_Obj6) := nil;
  Pointer(_Obj7) := nil;
  Pointer(_Obj8) := nil;
end;

procedure InitializeNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6, _Obj7, _Obj8, _Obj9);
begin
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
  Pointer(_Obj4) := nil;
  Pointer(_Obj5) := nil;
  Pointer(_Obj6) := nil;
  Pointer(_Obj7) := nil;
  Pointer(_Obj8) := nil;
  Pointer(_Obj9) := nil;
end;

procedure FreeAndNil(var _Obj1);
var
  Temp1: TObject;
begin
  Temp1 := TObject(_Obj1);
  Pointer(_Obj1) := nil;
  Temp1.Free;
end;

procedure FreeAndNil(var _Obj1, _Obj2);
var
  Temp1: TObject;
  Temp2: TObject;
begin
  Temp1 := TObject(_Obj1);
  Temp2 := TObject(_Obj2);
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Temp1.Free;
  Temp2.Free;
end;

procedure FreeAndNil(var _Obj1, _Obj2, _Obj3);
var
  Temp1: TObject;
  Temp2: TObject;
  Temp3: TObject;
begin
  Temp1 := TObject(_Obj1);
  Temp2 := TObject(_Obj2);
  Temp3 := TObject(_Obj3);
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
  Temp1.Free;
  Temp2.Free;
  Temp3.Free;
end;

procedure FreeAndNil(var _Obj1, _Obj2, _Obj3, _Obj4);
var
  Temp1: TObject;
  Temp2: TObject;
  Temp3: TObject;
  Temp4: TObject;
begin
  Temp1 := TObject(_Obj1);
  Temp2 := TObject(_Obj2);
  Temp3 := TObject(_Obj3);
  Temp4 := TObject(_Obj4);
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
  Pointer(_Obj4) := nil;
  Temp1.Free;
  Temp2.Free;
  Temp3.Free;
  Temp4.Free;
end;

procedure FreeAndNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5);
var
  Temp1: TObject;
  Temp2: TObject;
  Temp3: TObject;
  Temp4: TObject;
  Temp5: TObject;
begin
  Temp1 := TObject(_Obj1);
  Temp2 := TObject(_Obj2);
  Temp3 := TObject(_Obj3);
  Temp4 := TObject(_Obj4);
  Temp5 := TObject(_Obj5);
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
  Pointer(_Obj4) := nil;
  Pointer(_Obj5) := nil;
  Temp1.Free;
  Temp2.Free;
  Temp3.Free;
  Temp4.Free;
  Temp5.Free;
end;

procedure FreeAndNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6);
var
  Temp1: TObject;
  Temp2: TObject;
  Temp3: TObject;
  Temp4: TObject;
  Temp5: TObject;
  Temp6: TObject;
begin
  Temp1 := TObject(_Obj1);
  Temp2 := TObject(_Obj2);
  Temp3 := TObject(_Obj3);
  Temp4 := TObject(_Obj4);
  Temp5 := TObject(_Obj5);
  Temp6 := TObject(_Obj6);
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
  Pointer(_Obj4) := nil;
  Pointer(_Obj5) := nil;
  Pointer(_Obj6) := nil;
  Temp1.Free;
  Temp2.Free;
  Temp3.Free;
  Temp4.Free;
  Temp5.Free;
  Temp6.Free;
end;

procedure FreeAndNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6, _Obj7);
var
  Temp1: TObject;
  Temp2: TObject;
  Temp3: TObject;
  Temp4: TObject;
  Temp5: TObject;
  Temp6: TObject;
  Temp7: TObject;
begin
  Temp1 := TObject(_Obj1);
  Temp2 := TObject(_Obj2);
  Temp3 := TObject(_Obj3);
  Temp4 := TObject(_Obj4);
  Temp5 := TObject(_Obj5);
  Temp6 := TObject(_Obj6);
  Temp7 := TObject(_Obj7);
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
  Pointer(_Obj4) := nil;
  Pointer(_Obj5) := nil;
  Pointer(_Obj6) := nil;
  Pointer(_Obj7) := nil;
  Temp1.Free;
  Temp2.Free;
  Temp3.Free;
  Temp4.Free;
  Temp5.Free;
  Temp6.Free;
  Temp7.Free;
end;

procedure FreeAndNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6, _Obj7, _Obj8);
var
  Temp1: TObject;
  Temp2: TObject;
  Temp3: TObject;
  Temp4: TObject;
  Temp5: TObject;
  Temp6: TObject;
  Temp7: TObject;
  Temp8: TObject;
begin
  Temp1 := TObject(_Obj1);
  Temp2 := TObject(_Obj2);
  Temp3 := TObject(_Obj3);
  Temp4 := TObject(_Obj4);
  Temp5 := TObject(_Obj5);
  Temp6 := TObject(_Obj6);
  Temp7 := TObject(_Obj7);
  Temp8 := TObject(_Obj8);
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
  Pointer(_Obj4) := nil;
  Pointer(_Obj5) := nil;
  Pointer(_Obj6) := nil;
  Pointer(_Obj7) := nil;
  Pointer(_Obj8) := nil;
  Temp1.Free;
  Temp2.Free;
  Temp3.Free;
  Temp4.Free;
  Temp5.Free;
  Temp6.Free;
  Temp7.Free;
  Temp8.Free;
end;

procedure FreeAndNil(var _Obj1, _Obj2, _Obj3, _Obj4, _Obj5, _Obj6, _Obj7, _Obj8, _Obj9);
var
  Temp1: TObject;
  Temp2: TObject;
  Temp3: TObject;
  Temp4: TObject;
  Temp5: TObject;
  Temp6: TObject;
  Temp7: TObject;
  Temp8: TObject;
  Temp9: TObject;
begin
  Temp1 := TObject(_Obj1);
  Temp2 := TObject(_Obj2);
  Temp3 := TObject(_Obj3);
  Temp4 := TObject(_Obj4);
  Temp5 := TObject(_Obj5);
  Temp6 := TObject(_Obj6);
  Temp7 := TObject(_Obj7);
  Temp8 := TObject(_Obj8);
  Temp9 := TObject(_Obj9);
  Pointer(_Obj1) := nil;
  Pointer(_Obj2) := nil;
  Pointer(_Obj3) := nil;
  Pointer(_Obj4) := nil;
  Pointer(_Obj5) := nil;
  Pointer(_Obj6) := nil;
  Pointer(_Obj7) := nil;
  Pointer(_Obj8) := nil;
  Pointer(_Obj9) := nil;
  Temp1.Free;
  Temp2.Free;
  Temp3.Free;
  Temp4.Free;
  Temp5.Free;
  Temp6.Free;
  Temp7.Free;
  Temp8.Free;
  Temp9.Free;
end;

end.
