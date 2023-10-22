{.GXFormatter.config=twm}
unit u_dzVersionInfo;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes,
  u_dzTypes;

type
  EApplicationInfo = class(Exception);
//  EAIChecksumError = class(EApplicationInfo);
  EAIUnknownProperty = class(EApplicationInfo);
  EAIInvalidVersionInfo = class(EApplicationInfo);

type
  TFileProperty = (FpProductName, FpProductVersion, FpFileDescription, FpFileVersion, FpCopyright,
    FpCompanyName, FpTrademarks, fpInternalName, fpOriginalFilename, fpComments, fpPrivateBuild,
    fpSpecialBuild);
  TFilePropertySet = set of TFileProperty;

type
  TVersionParts = (vpMajor, vpMajorMinor, vpMajorMinorRevision, vpFull);

type
  TFileVersionRec = record
    Major: Integer;
    Minor: Integer;
    Revision: Integer;
    Build: Integer;
    IsValid: Boolean;
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
    procedure CheckValid;
    procedure Init(_Major, _Minor, _Revision, _Build: Integer);
    class operator GreaterThan(_a, _b: TFileVersionRec): Boolean;
    class operator GreaterThanOrEqual(_a, _b: TFileVersionRec): Boolean;
    class operator Equal(_a, _b: TFileVersionRec): Boolean;
    class operator NotEqual(_a, _b: TFileVersionRec): Boolean;
    class operator LessThan(_a, _b: TFileVersionRec): Boolean;
    class operator LessThanOrEqual(_a, _b: TFileVersionRec): Boolean;
{$ENDIF}
  end;

type
  IFileInfo = interface ['{BF3A3600-1E39-4618-BD7A-FBBD6C148C2E}']
    function HasVersionInfo: Boolean;
    procedure SetAllowExceptions(_Value: Boolean);
    ///<summary> If set to false, any exceptions will be ignored and an empty string will
    ///          be returned. </summary>
    property AllowExceptions: Boolean write SetAllowExceptions;
    ///<summary> The file name.</summary>
    function Filename: string;
    ///<summary> The file directory whithout the filename with a terminating backslash </summary>
    function FileDir: string;
    ///<summary> The file description from the version resource </summary>
    function FileDescription: string;
    ///<summary> The file version from the file version resource </summary>
    function FileVersion: string;
    function FileVersionRec: TFileVersionRec;
    function FileVersionStr(_Parts: TVersionParts = vpMajorMinorRevision): string;
    ///<summary> The file's product name from the version resource </summary>
    function ProductName: string;
    ///<summary> The the product version from the version resource </summary>
    function ProductVersion: string;
    ///<summary> The company name from the version resource </summary>
    function Company: string; deprecated; // use CompanyName
    ///<summary> The company name from the version resource </summary>
    function CompanyName: string;
    ///<summary> The LegalCopyRight string from the file version resources </summary>
    function LegalCopyRight: string;
    ///<summary> The LegalTrademark string from the file version resources </summary>
    function LegalTradeMarks: string;
    function InternalName: string;
    function OriginalFilename: string;
    function Comments: string;
    function PrivateBuild: string;
    function SpecialBuild: string;
    procedure GetAllStrings(_Strings: TStrings);
  end;

type
  TEXEVersionData = record
    CompanyName,
      FileDescription,
      FileVersion,
      InternalName,
      LegalCopyRight,
      LegalTradeMarks,
      OriginalFilename,
      ProductName,
      ProductVersion,
      Comments,
      PrivateBuild,
      SpecialBuild: string;
  end;

type
  ///<summary> abstract ancestor, do not instantiate this class, instantiate one of
  ///          the derived classes below </summary>
  TCustomFileInfo = class(TInterfacedObject)
  private
  private
    FAllowExceptions: Boolean;
    FFilename: string;

    FFilePropertiesRead: Boolean;
    FFileProperties: array[TFileProperty] of string;
    function GetFileProperty(_Property: TFileProperty): string; virtual;
    function ReadVersionData: TEXEVersionData;
  protected // implements IFileInfo
    procedure SetAllowExceptions(_Value: Boolean);
    function HasVersionInfo: Boolean;

    function Filename: string;
    function FileDir: string;
    function FileDescription: string;
    function FileVersion: string;
    function FileVersionRec: TFileVersionRec; virtual;
    function FileVersionStr(_Parts: TVersionParts = vpMajorMinorRevision): string;

    function ProductName: string;
    function ProductVersion: string;
    ///<summary> The company name from the version resource </summary>
    function Company: string;
    ///<summary> The company name from the version resource </summary>
    function CompanyName: string;
    ///<summary> The LegalCopyRight string from the file version resources </summary>
    function LegalCopyRight: string;
    ///<summary> The LegalTrademark string from the file version resources </summary>
    function LegalTradeMarks: string;
    function InternalName: string;
    function OriginalFilename: string;
    function Comments: string;
    function PrivateBuild: string;
    function SpecialBuild: string;
    procedure GetAllStrings(_Strings: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    property AllowExceptions: Boolean read FAllowExceptions write SetAllowExceptions;
  end;

type
  ///<summary> Get informations about the given file.</summary>
  TFileInfo = class(TCustomFileInfo, IFileInfo)
  public
    constructor Create(const _Filename: string);
  end;

type
  ///<summary> Get informations about the current executable
  ///          If called from a dll it will return the info about the
  ///          calling executable, if called from an executable, it will return
  ///          info about itself. </summary>
  TApplicationInfo = class(TCustomFileInfo, IFileInfo)
  public
    constructor Create;
  end;

type
  ///<summary> Get informations about the current DLL.
  ///          It will always return info about itself regardless of whether it is
  ///          called from a dll or an executable </summary>
  TDllInfo = class(TCustomFileInfo, IFileInfo)
  public
    constructor Create;
  end;

type
  TDummyFileInfo = class(TCustomFileInfo, IFileInfo)
  protected
    function GetFileProperty(_Property: TFileProperty): string; override;
    function FileVersionRec: TFileVersionRec; override;
  public
    constructor Create;
  end;

implementation

uses
  Windows,
  u_dzTranslator,
  u_dzOsUtils;

function _(const _s: string): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := dzlibGetText(_s);
end;

{ TCustomFileInfo }

constructor TCustomFileInfo.Create;
begin
  inherited;

  FAllowExceptions := True;
  FFilePropertiesRead := False;
end;

function TCustomFileInfo.Filename: string;
begin
  Result := FFilename;
end;

destructor TCustomFileInfo.Destroy;
begin
  inherited;
end;

function TCustomFileInfo.FileDescription: string;
begin
  Result := GetFileProperty(FpFileDescription);
end;

function TCustomFileInfo.FileDir: string;
begin
  Result := ExtractFileDir(Filename);
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

procedure TCustomFileInfo.SetAllowExceptions(_Value: Boolean);
begin
  FAllowExceptions := _Value;
end;

function TCustomFileInfo.SpecialBuild: string;
begin
  Result := GetFileProperty(fpSpecialBuild);
end;

type
  ///<summary>
  /// Represents the organization of data in a file-version resource.
  /// It contains a string that describes a specific aspect of a file, for example, a file's
  /// version, its copyright notices, or its trademarks.
  /// @member wLength The length, in bytes, of this String structure.
  /// @member wValueLength: The size, in words, of the Value member.
  /// @member wType The type of data in the version resource. This member is 1 if the version
  ///               resource contains text data and 0 if the version resource contains binary data.
  /// @member szKey An arbitrary Unicode string.
  /// @member Value A zero-terminated string. See the szKey member description for more information.
  /// @Note This record cannot be read or written as such because it contains a variable size
  ///       field szKey. Call ReadVsVersionInfo to read it
  /// see https://docs.microsoft.com/en-us/windows/win32/menurc/string-str </summary>
  PVS_String = ^TVS_String;
  TVS_String = packed record
    wLength: UInt16;
    wValueLength: UInt16;
    wType: UInt16;
    szKey: WideString;
    // Padding: As many zero words as necessary to align the Children member on a 32-bit boundary.
    Value: WideString;
  end;
  ///<summary>
  /// Represents the organization of data in a file-version resource.
  /// It contains language and code page formatting information for the strings specified by the
  /// Children member. A code page is an ordered character set.
  /// @member wLength The length, in bytes, of this StringTable structure, including all
  ///                 structures indicated by the Children member.
  /// @member wValueLength: This member is always equal to zero.
  /// @member wType The type of data in the version resource. This member is 1 if the version
  ///               resource contains text data and 0 if the version resource contains binary data.
  /// @member szKey An 8-digit hexadecimal number stored as a Unicode string. The four most
  ///               significant digits represent the language identifier. The four least
  ///               significant digits represent the code page for which the data is formatted.
  ///               Each Microsoft Standard Language identifier contains two parts:
  ///               the low-order 10 bits specify the major language,
  ///               and the high-order 6 bits specify the sublanguage.
  ///               For a table of valid identifiers see ???
  ///               (The  reference is missing from the original description.)
  /// @Note This record cannot be read or written as such because it contains a variable size
  ///       field szKey. Call ReadVsVersionInfo to read it
  // see https://docs.microsoft.com/en-us/windows/win32/menurc/stringtable </summary>
  PVS_StringTable = ^TVS_StringTable;
  TVS_StringTable = packed record
    wLength: UInt16;
    wValueLength: UInt16;
    wType: UInt16;
    szKey: WideString;
    // Padding: As many zero words as necessary to align the Children member on a 32-bit boundary.
    Children: array of TVS_String;
  end;
  ///<summary>
  /// Represents the organization of data in a file-version resource.
  /// It contains version information that can be displayed for a particular language and code page.
  /// @member wLength The length, in bytes, of the entire StringFileInfo block, including all
  ///                 structures indicated by the Children member.
  /// @member wValueLength This member is always equal to zero.
  /// @member wType The type of data in the version resource. This member is 1 if the version
  ///               resource contains text data and 0 if the version resource contains binary data.
  /// @member szKey The Unicode string L"StringFileInfo"
  /// @Note This record cannot be read or written as such because it contains a variable size
  ///       field szKey. Call ReadVsVersionInfo to read it
  /// see https://docs.microsoft.com/en-us/windows/win32/menurc/stringfileinfo </summary>
  TVS_StringFileInfo = packed record
    wLength: UInt16;
    wValueLength: UInt16;
    wType: UInt16;
    szKey: WideString;
    // Padding: As many zero words as necessary to align the Children member on a 32-bit boundary.
    Children: array of TVS_StringTable;
  end;

  ///<summary>
  /// Represents the organization of data in a file-version resource. It typically contains a list
  /// of language and code page identifier pairs that the version of the application or DLL supports.
  /// @member wLength The length, in bytes, of the Var structure.
  /// @member wValueLength The length, in bytes, of the Value member.
  /// @member wType The type of data in the version resource. This member is 1 if the version
  ///               resource contains text data and 0 if the version resource contains binary data.
  /// @member szKey The Unicode string L"Translation".
  /// @member Value An array of one or more values that are language and code page identifier pairs.
  ///               If you use the Var structure to list the languages your application or DLL
  ///               supports instead of using multiple version resources, use the Value member to
  ///               contain an array of DWORD values indicating the language and code page combinations
  ///               supported by this file. The low-order word of each DWORD must contain a Microsoft
  ///               language identifier, and the high-order word must contain the IBM code page number.
  ///               Either high-order or low-order word can be zero, indicating that the file is
  ///               language or code page independent. If the Var structure is omitted, the file
  ///               will be interpreted as both language and code page independent.
  /// @Note This record cannot be read or written as such because it contains a variable size
  ///       field szKey. Call ReadVsVersionInfo to read it
  /// see https://docs.microsoft.com/en-us/windows/win32/menurc/var-str </summary>
  TVS_Var = packed record
    wLength: UInt16;
    wValueLength: UInt16;
    wType: UInt16;
    szKey: WideString;
    // Padding: As many zero words as necessary to align the Value member on a 32-bit boundary.
    Value: UInt32;
  end;

  ///<summary>
  /// Represents the organization of data in a file-version resource. It contains version
  /// information not dependent on a particular language and code page combination.
  /// @member wLength The length, in bytes, of the entire VarFileInfo block,
  ///                 including all structures indicated by the Children member.
  /// @member wValueLength This member is always equal to zero.
  /// @member wType The type of data in the version resource. This member is 1 if the version
  ///               resource contains text data and 0 if the version resource contains binary data.
  /// @member szKey The Unicode string L"VarFileInfo"
  /// @member Children Typically contains a list of languages that the application or DLL supports.
  /// @Note This record cannot be read or written as such because it contains a variable size
  ///       field szKey. Call ReadVsVersionInfo to read it
  /// see https://docs.microsoft.com/en-us/windows/win32/menurc/varfileinfo </summary>
  TVS_VarFileInfo = packed record
    wLength: UInt16;
    wValueLength: UInt16;
    wType: UInt16;
    szKey: WideString;
    // Padding: As many zero words as necessary to align the Children member on a 32-bit boundary.
    Children: array of TVS_Var
  end;

  ///<summary>
  /// Represents the organization of data in a file-version resource. It is the root structure
  /// that contains all other file-version information structures.
  /// @member wLength The length, in bytes, of the VS_VERSIONINFO structure. This length does not
  ///                 include any padding that aligns any subsequent version resource data on a
  ///                 32-bit boundary.
  /// @member wValueLength The length, in bytes, of the Value member. This value is zero if there
  ///                      is no Value member associated with the current version structure.
  /// @member wType The type of data in the version resource. This member is 1 if the version
  ///               resource contains text data and 0 if the version resource contains binary data.
  /// @member szKey The Unicode string L"VS_VERSION_INFO".
  /// @member Value Arbitrary data associated with this VS_VERSIONINFO structure.
  ///               The wValueLength member specifies the length of this member;
  ///               if wValueLength is zero, this member does not exist.
  /// @member Children An array of zero or one StringFileInfo structures, and zero or one
  ///                  VarFileInfo structures that are children of the current VS_VERSIONINFO
  ///                  structure.
  // @NOTE This record cannot be read or written as such because it contains a variable size
  //       field szKey and needs padding for aligning the members. Call ReadVsVersionInfo to read it
  /// see https://docs.microsoft.com/en-us/windows/win32/menurc/vs-versioninfo </summary>
  TVsVersionInfo = packed record
    wLength: UInt16;
    wValueLength: UInt16;
    wType: UInt16;
    szKey: WideString;
    // Padding1: Contains as many zero words as necessary to align the Value member on a 32-bit boundary.
    Value: VS_FIXEDFILEINFO;
    // Padding2: As many zero words as necessary to align the Children member on a 32-bit boundary.
    //           These bytes are not included in wValueLength. This member is optional.
    // Children: TBytes;
  end;

function ReadNullTerminatedWideString(const _Buffer: TBytes; _Offset: Integer): WideString;
var
  CharArr: array of WideChar;
  BufLen: Integer;
begin
  BufLen := Length(_Buffer);
  SetLength(CharArr, (BufLen - _Offset) div SizeOf(WideChar) + 1);
  Move(_Buffer[_Offset], CharArr[0], BufLen - _Offset);
  Result := PWideChar(@CharArr[0]);
end;

procedure ReadStringVsString(const _Buffer: TBytes; _Offset: Integer; out _VsString: TVS_String);
var
  szOffset: Integer;
  Offset: Integer;
begin
  szOffset := SizeOf(UInt16) * 3;
  Move(_Buffer[_Offset], _VsString, szOffset);

  _VsString.szKey := ReadNullTerminatedWideString(_Buffer, _Offset + szOffset);

  Offset := (_Offset + szOffset + (Length(_VsString.szKey) + 1) * SizeOf(WideChar) + 3) and (not 3);
  _VsString.Value := ReadNullTerminatedWideString(_Buffer, Offset);
end;

procedure ReadVsStringTable(const _Buffer: TBytes; _Offset: Integer; out _StringTable: TVS_StringTable);
var
  szOffset: Integer;
  Offset: Integer;
  VsString: PVS_String;
  Idx: Integer;
begin
  szOffset := SizeOf(UInt16) * 3;
  Move(_Buffer[_Offset], _StringTable, szOffset);
  if _StringTable.wValueLength <> 0 then
    raise EdzException.CreateFmt(_('StringTable.wValueLength must be 0 but is %d'), [_StringTable.wValueLength]);

  _StringTable.szKey := ReadNullTerminatedWideString(_Buffer, _Offset + szOffset);
  // _StringTable.szKey is the language code

  SetLength(_StringTable.Children, 0);
  Offset := (_Offset + szOffset + (Length(_StringTable.szKey) + 1) * SizeOf(WideChar) + 3) and (not 3);
  repeat
    Idx := Length(_StringTable.Children);
    SetLength(_StringTable.Children, Idx + 1);
    VsString := @_StringTable.Children[Idx];
    ReadStringVsString(_Buffer, Offset, VsString^);
    Offset := (Offset + VsString.wLength + 3) and (not 3);
  until Offset >= _StringTable.wLength + _Offset;
end;

procedure ReadVsStringFileInfo(const _Buffer: TBytes; _Offset: Integer;
  out _StringFileInfo: TVS_StringFileInfo);
var
  szOffset: Integer;
  Offset: Integer;
  StringTable: PVS_StringTable;
  Idx: Integer;
begin
  szOffset := SizeOf(UInt16) * 3;
  Move(_Buffer[_Offset], _StringFileInfo, szOffset);
  if _StringFileInfo.wValueLength <> 0 then
    raise EdzException.CreateFmt(_('StringFileInfo.wValueLength must be 0 but is %d'), [_StringFileInfo.wValueLength]);

  _StringFileInfo.szKey := ReadNullTerminatedWideString(_Buffer, _Offset + szOffset);
  if _StringFileInfo.szKey <> 'StringFileInfo' then
    raise EdzException.CreateFmt(_('StringFileInfo.szKey must be "StringFileInfo" but is "%s"'), [_StringFileInfo.szKey]);

  SetLength(_StringFileInfo.Children, 0);
  Offset := (_Offset + szOffset + (Length(_StringFileInfo.szKey) + 1) * SizeOf(WideChar) + 3) and (not 3);
  repeat
    Idx := Length(_StringFileInfo.Children);
    SetLength(_StringFileInfo.Children, Idx + 1);
    StringTable := @_StringFileInfo.Children[Idx];
    ReadVsStringTable(_Buffer, Offset, StringTable^);
    Offset := (Offset + StringTable.wLength + 3) and (not 3);
  until Offset >= _StringFileInfo.wLength + _Offset;
end;

procedure ReadVsVarFileInfo(const _Buffer: TBytes; _Offset: Integer;
  out _VarFileInfo: TVS_VarFileInfo);
var
  szOffset: Integer;
begin
  szOffset := SizeOf(UInt16) * 3;
  Move(_Buffer[_Offset], _VarFileInfo, szOffset);
  _VarFileInfo.szKey := ReadNullTerminatedWideString(_Buffer, _Offset + szOffset);
end;

procedure ReadVsVersionInfo(const _Buffer: TBytes; out _VerInfo: TVsVersionInfo;
  out _StringFileInfo: TVS_StringFileInfo; out _VarFileInfo: TVS_VarFileInfo);
var
  Offset: Integer;
  szOffset: Integer;
  Key: WideString;
begin
  szOffset := SizeOf(UInt16) * 3;
  Move(_Buffer[0], _VerInfo, szOffset);
  if _VerInfo.wLength = 0 then
    raise EdzException.Create('Version Info is empty');

  _VerInfo.szKey := ReadNullTerminatedWideString(_Buffer, szOffset);
  if _VerInfo.szKey <> 'VS_VERSION_INFO' then
    raise EdzException.CreateFmt(_('VS_VersionInfo.szKey is not "VS_VERSION_INFO" but "%s"'), [_VerInfo.szKey]);

  Offset := (szOffset + (Length(_VerInfo.szKey) + 1) * SizeOf(WideChar) + 3) and (not 3);
  if _VerInfo.wValueLength > 0 then begin
    Move(_Buffer[Offset], _VerInfo.Value, SizeOf(_VerInfo.Value));
    if _VerInfo.Value.dwSignature <> $FEEF04BD then
      raise EdzException.Create('Version Info has wrong signature');
    Offset := Offset + SizeOf(_VerInfo.Value);
  end;

  _StringFileInfo.wLength := 0;
  _VarFileInfo.wLength := 0;

  // now it gets really complicated:
  // Children is zero or one StringFileInfo structures and zero or one VarFileInfo structures
  if Offset = _VerInfo.wLength then begin
    // None of these structures exist
    Exit; //==>
  end;
  // at least one of these structures exists. In order to find out which one it is, we must read
  // the szKey string
  szOffset := Offset + SizeOf(UInt16) * 3;
  Key := ReadNullTerminatedWideString(_Buffer, szOffset);
  if Key = 'StringFileInfo' then begin
    ReadVsStringFileInfo(_Buffer, Offset, _StringFileInfo);
    Offset := Offset + _StringFileInfo.wLength;
    szOffset := Offset + SizeOf(UInt16) * 3;
    Key := ReadNullTerminatedWideString(_Buffer, szOffset);
  end;
  if Key = 'VarFileInfo' then begin
    ReadVsVarFileInfo(_Buffer, Offset, _VarFileInfo);
  end;
end;

procedure TCustomFileInfo.GetAllStrings(_Strings: TStrings);
var
  Dummy: DWORD;
  Len: DWORD;
  Buf: TBytes;
  InfoBase: TVsVersionInfo;
  StringFileInfo: TVS_StringFileInfo;
  VarFileInfo: TVS_VarFileInfo;
  sfi: Integer;
  Lang: WideString;
  st: PVS_StringTable;
  sti: Integer;
begin
  Len := GetFileVersionInfoSize(PChar(Filename), Dummy);
  if Len = 0 then
    RaiseLastOSError;
  SetLength(Buf, Len);

  if not GetFileVersionInfo(PChar(Filename), 0, Len, @Buf[0]) then
    RaiseLastOSError;

  ReadVsVersionInfo(Buf, InfoBase, StringFileInfo, VarFileInfo);
  _Strings.Clear;
  if StringFileInfo.wLength > 0 then begin
    for sfi := 0 to Length(StringFileInfo.Children) - 1 do begin
      st := @StringFileInfo.Children[sfi];
      Lang := st.szKey;
      for sti := 0 to Length(st.Children) - 1 do begin
        _Strings.Values[Lang + '\' + st.Children[sti].szKey] := st.Children[sti].Value;
      end;
    end;
  end;
end;

function TCustomFileInfo.ReadVersionData: TEXEVersionData;
// code taken from http://stackoverflow.com/a/5539411/49925
type
  PLandCodepage = ^TLandCodepage;
  TLandCodepage = record
    wLanguage,
      wCodePage: Word;
  end;
var
  Dummy: DWORD;
  Len: DWORD;
  Buf: TBytes;
  pntr: Pointer;
  Lang: string;
begin
  Len := GetFileVersionInfoSize(PChar(Filename), Dummy);
  if Len = 0 then
    RaiseLastOSError;
  SetLength(Buf, Len);

  if not GetFileVersionInfo(PChar(Filename), 0, Len, @Buf[0]) then
    RaiseLastOSError;
  if not VerQueryValue(Buf, '\VarFileInfo\Translation\', pntr, Len) then
    RaiseLastOSError;
  Lang := Format('%.4x%.4x', [PLandCodepage(pntr)^.wLanguage, PLandCodepage(pntr)^.wCodePage]);

  if VerQueryValue(@Buf[0], PChar('\StringFileInfo\' + Lang + '\CompanyName'), pntr, Len) { and (@len <> nil)} then
    Result.CompanyName := PChar(pntr);
  if VerQueryValue(@Buf[0], PChar('\StringFileInfo\' + Lang + '\FileDescription'), pntr, Len) { and (@len <> nil)} then
    Result.FileDescription := PChar(pntr);
  if VerQueryValue(@Buf[0], PChar('\StringFileInfo\' + Lang + '\FileVersion'), pntr, Len) { and (@len <> nil)} then
    Result.FileVersion := PChar(pntr);
  if VerQueryValue(@Buf[0], PChar('\StringFileInfo\' + Lang + '\InternalName'), pntr, Len) { and (@len <> nil)} then
    Result.InternalName := PChar(pntr);
  if VerQueryValue(@Buf[0], PChar('\StringFileInfo\' + Lang + '\LegalCopyright'), pntr, Len) { and (@len <> nil)} then
    Result.LegalCopyRight := PChar(pntr);
  if VerQueryValue(@Buf[0], PChar('\StringFileInfo\' + Lang + '\LegalTrademarks'), pntr, Len) { and (@len <> nil)} then
    Result.LegalTradeMarks := PChar(pntr);
  if VerQueryValue(@Buf[0], PChar('\StringFileInfo\' + Lang + '\OriginalFileName'), pntr, Len) { and (@len <> nil)} then
    Result.OriginalFilename := PChar(pntr);
  if VerQueryValue(@Buf[0], PChar('\StringFileInfo\' + Lang + '\ProductName'), pntr, Len) { and (@len <> nil)} then
    Result.ProductName := PChar(pntr);
  if VerQueryValue(@Buf[0], PChar('\StringFileInfo\' + Lang + '\ProductVersion'), pntr, Len) { and (@len <> nil)} then
    Result.ProductVersion := PChar(pntr);
  if VerQueryValue(@Buf[0], PChar('\StringFileInfo\' + Lang + '\Comments'), pntr, Len) { and (@len <> nil)} then
    Result.Comments := PChar(pntr);
  if VerQueryValue(@Buf[0], PChar('\StringFileInfo\' + Lang + '\PrivateBuild'), pntr, Len) { and (@len <> nil)} then
    Result.PrivateBuild := PChar(pntr);
  if VerQueryValue(@Buf[0], PChar('\StringFileInfo\' + Lang + '\SpecialBuild'), pntr, Len) { and (@len <> nil)} then
    Result.SpecialBuild := PChar(pntr);
end;

function TCustomFileInfo.HasVersionInfo: Boolean;
var
  Handle: DWORD;
  Size: DWORD;
begin
  Size := GetFileVersionInfoSize(PChar(Filename), Handle);
  Result := Size <> 0;
end;

function TCustomFileInfo.GetFileProperty(_Property: TFileProperty): string;
var
  fi: TEXEVersionData;
begin
  Result := '';

  if not FFilePropertiesRead then begin
    try
      case _Property of
        FpProductName,
          FpProductVersion,
          FpCompanyName,
          FpFileDescription,
          FpFileVersion,
          FpCopyright,
          fpInternalName,
          fpOriginalFilename,
          fpComments,
          fpPrivateBuild,
          fpSpecialBuild: begin
            if not HasVersionInfo then begin
              if FAllowExceptions then
                raise EAIInvalidVersionInfo.CreateFmt(_('File "%s" has no version information.'), [Filename]);
              Exit;
            end;

            fi := Self.ReadVersionData;

            FFileProperties[FpFileVersion] := fi.FileVersion;
            FFileProperties[FpFileDescription] := fi.FileDescription;
            FFileProperties[FpProductName] := fi.ProductName;
            FFileProperties[FpProductVersion] := fi.ProductVersion;
            FFileProperties[FpCopyright] := fi.LegalCopyRight;
            FFileProperties[FpTrademarks] := fi.LegalTradeMarks;
            FFileProperties[FpCompanyName] := fi.CompanyName;
            FFileProperties[fpOriginalFilename] := fi.OriginalFilename;
            FFileProperties[fpInternalName] := fi.InternalName;
            FFileProperties[fpComments] := fi.Comments;
            FFileProperties[fpPrivateBuild] := fi.PrivateBuild;
            FFileProperties[fpSpecialBuild] := fi.SpecialBuild;

            FFilePropertiesRead := True;
          end;
      end;
    except
      if FAllowExceptions then
        raise;
      Exit;
    end;
  end;

  Result := FFileProperties[_Property];
end;

function TCustomFileInfo.InternalName: string;
begin
  Result := GetFileProperty(fpInternalName);
end;

function TCustomFileInfo.Comments: string;
begin
  Result := GetFileProperty(fpComments);
end;

function TCustomFileInfo.Company: string;
begin
  Result := CompanyName;
end;

function TCustomFileInfo.CompanyName: string;
begin
  Result := GetFileProperty(FpCompanyName);
end;

function TCustomFileInfo.LegalCopyRight: string;
begin
  Result := GetFileProperty(FpCopyright);
end;

function TCustomFileInfo.LegalTradeMarks: string;
begin
  Result := GetFileProperty(FpTrademarks);
end;

function TCustomFileInfo.OriginalFilename: string;
begin
  Result := GetFileProperty(fpOriginalFilename);
end;

function TCustomFileInfo.FileVersion: string;
begin
  Result := GetFileProperty(FpFileVersion);
end;

function TCustomFileInfo.FileVersionRec: TFileVersionRec;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.IsValid := GetFileBuildInfo(FFilename, Result.Major, Result.Minor, Result.Revision, Result.Build);
end;

function TCustomFileInfo.FileVersionStr(_Parts: TVersionParts = vpMajorMinorRevision): string;
var
  Rec: TFileVersionRec;
begin
  Rec := FileVersionRec;
  if Rec.IsValid then begin
    case _Parts of
      vpMajor: Result := IntToStr(Rec.Major);
      vpMajorMinor: Result := IntToStr(Rec.Major) + '.' + IntToStr(Rec.Minor);
      vpMajorMinorRevision: Result := IntToStr(Rec.Major) + '.' + IntToStr(Rec.Minor) + '.' + IntToStr(Rec.Revision);
      vpFull: Result := IntToStr(Rec.Major) + '.' + IntToStr(Rec.Minor) + '.' + IntToStr(Rec.Revision) + '.' + IntToStr(Rec.Build)
    else
      raise EAIUnknownProperty.CreateFmt(_('Invalid version part (%d)'), [Ord(_Parts)]);
    end;
  end else
    Result := _('<no version information>');
end;

function TCustomFileInfo.PrivateBuild: string;
begin
  Result := GetFileProperty(fpPrivateBuild);
end;

function TCustomFileInfo.ProductName: string;
begin
  Result := GetFileProperty(FpProductName);
end;

function TCustomFileInfo.ProductVersion: string;
begin
  Result := GetFileProperty(FpProductVersion);
end;

{ TFileInfo }

constructor TFileInfo.Create(const _Filename: string);
begin
  inherited Create;
  FFilename := ExpandFileName(_Filename);
end;

{ TApplicationInfo }

constructor TApplicationInfo.Create;
begin
  inherited Create;
  FFilename := GetModuleFilename(0);
end;

{ TDllInfo }

constructor TDllInfo.Create;
begin
  inherited Create;
  FFilename := GetModuleFilename;
end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
{ TFileVersionRec }

procedure TFileVersionRec.CheckValid;
begin
  if not IsValid then
    raise EAIInvalidVersionInfo.Create(_('Invalid version info'));
end;

class operator TFileVersionRec.Equal(_a, _b: TFileVersionRec): Boolean;
begin
  _a.CheckValid;
  _b.CheckValid;

  Result := (_a.Major = _b.Major) and (_a.Minor = _b.Minor) and (_a.Revision = _b.Revision) and (_a.Build = _b.Build);
end;

class operator TFileVersionRec.GreaterThan(_a, _b: TFileVersionRec): Boolean;
begin
  _a.CheckValid;
  _b.CheckValid;

  Result := _a.Major > _b.Major;
  if not Result and (_a.Major = _b.Major) then begin
    Result := _a.Minor > _b.Minor;
    if not Result and (_a.Minor = _b.Minor) then begin
      Result := _a.Revision > _b.Revision;
      if not Result and (_a.Revision = _b.Revision) then
        Result := _a.Build > _b.Build;
    end;
  end;
end;

class operator TFileVersionRec.GreaterThanOrEqual(_a, _b: TFileVersionRec): Boolean;
begin
  _a.CheckValid;
  _b.CheckValid;

  Result := not (_a < _b);
end;

procedure TFileVersionRec.Init(_Major, _Minor, _Revision, _Build: Integer);
begin
  Major := _Major;
  Minor := _Minor;
  Revision := _Revision;
  Build := _Build;
end;

class operator TFileVersionRec.LessThan(_a, _b: TFileVersionRec): Boolean;
begin
  _a.CheckValid;
  _b.CheckValid;

  Result := _a.Major < _b.Major;
  if not Result and (_a.Major = _b.Major) then begin
    Result := _a.Minor < _b.Minor;
    if not Result and (_a.Minor = _b.Minor) then begin
      Result := _a.Revision < _b.Revision;
      if not Result and (_a.Revision = _b.Revision) then
        Result := _a.Build < _b.Build;
    end;
  end;
end;

class operator TFileVersionRec.LessThanOrEqual(_a, _b: TFileVersionRec): Boolean;
begin
  _a.CheckValid;
  _b.CheckValid;

  Result := not (_a > _b);
end;

class operator TFileVersionRec.NotEqual(_a, _b: TFileVersionRec): Boolean;
begin
  _a.CheckValid;
  _b.CheckValid;

  Result := not (_a = _b);
end;
{$ENDIF}

{ TDummyFileInfo }

constructor TDummyFileInfo.Create;
begin
  inherited Create;
  AllowExceptions := False;
end;

function TDummyFileInfo.FileVersionRec: TFileVersionRec;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.IsValid := False;
end;

function TDummyFileInfo.GetFileProperty(_Property: TFileProperty): string;
begin
  Result := '';
end;

end.

