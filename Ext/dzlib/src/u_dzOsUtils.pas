///<summary> This unit contains operating system dependent functions, at least some of them. </summary>
unit u_dzOsUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzTypes;

type
  EOsFunc = class(EdzException);
  EOFNoFileinfo = class(EOsFunc);
  EPowerRequestFailed = class(EOsFunc)
  public
    ErrorCode: Word;
  end;

///<summary>
/// Determines the computername
/// @param RaiseException: If true, an error raises an EOsError exception, otherwise the function
///                        returns an empty string.
/// @returns a string with the computername, or an empty string if there was an error </summary>
function GetComputerName(_RaiseException: Boolean = False): string;

type
  _COMPUTER_NAME_FORMAT = (ComputerNameNetBIOS,
    ComputerNameDnsHostname, ComputerNameDnsDomain,
    ComputerNameDnsFullyQualified, ComputerNamePhysicalNetBIOS,
    ComputerNamePhysicalDnsHostname, ComputerNamePhysicalDnsDomain,
    ComputerNamePhysicalDnsFullyQualified, ComputerNameMax);
  TComputerNameFormat = _COMPUTER_NAME_FORMAT;
  COMPUTER_NAME_FORMAT = _COMPUTER_NAME_FORMAT;

///<summary>
/// Determines the computername
/// @param NameFormat determines the desired name format, default is ComputerNameNetBIOS
///                   https://msdn.microsoft.com/en-us/library/windows/desktop/ms724301(v=vs.85).aspx
/// @param RaiseException: If true, an error raises an EOsError exception, otherwise the function
///                        returns an empty string.
/// @returns a string with the computername, or an empty string if there was an error </summary>
function GetComputerNameEx(_NameFormat: COMPUTER_NAME_FORMAT = ComputerNameNetBIOS;
  _RaiseException: Boolean = False): string;

///<summary>
/// Determines the name of the user who runs this program.
/// @param RaiseException: If true, an error raises an EOsError exception, otherwise the function
///                        returns an empty string.
/// @returns a string with the user logon name or an empty string if there was an error </summary>
function GetUserName(_RaiseException: Boolean = False): string;

type
  EXTENDED_NAME_FORMAT = (
    NameUnknown = 0, // Unknown name type.
    NameFullyQualifiedDN = 1, // Fully qualified distinguished name
    NameSamCompatible = 2, // Windows NT® 4.0 account name
    NameDisplay = 3, // A "friendly" display name
    NameUniqueId = 6, // GUID string that the IIDFromString function returns
    NameCanonical = 7, // Complete canonical name
    NameUserPrincipal = 8, // User principal name
    NameCanonicalEx = 9,
    NameServicePrincipal = 10, // Generalized service principal name
    DNSDomainName = 11 // DNS domain name, plus the user name
    );

///<summary>
/// Determines the name of the user who runs this program.
/// @param NameFormat determines the desired name format, default is NameSamCompatible
///                   https://msdn.microsoft.com/en-us/library/windows/desktop/ms724435(v=vs.85).aspx
/// @param RaiseException: If true, an error raises an EOsError exception, otherwise the function
///                        returns an empty string.
/// @returns a string with the user logon name or an empty string if there was an error </summary>
function GetUserNameEx(_NameFormat: EXTENDED_NAME_FORMAT = NameSamCompatible;
  _RaiseException: Boolean = False): string;

///<summary> Returns the current user's home directory.
///          Examines the environment variable HOME and if that is not
///          set, it concatenates HOMEDRV and HOMEPATH </summary>
function GetHomeDir: string;

///<summary> Calls the windows function with the same name and returns its result </summary>
function ExpandEnvironmentStrings(const _WithVariables: string): string;
///<summary> Calls the windows API function GetEnvironmentStrings and returns them result
///          in the string list.
///          @param Vars is the string list that contains the environment
///          @returns true, if the function succeeded, false otherwise. </summary>
function GetEnvironmentVars(const _Vars: TStrings): Boolean;

///<summary> Reads an integer value from the registry.
///          @param RootKey is the HK_* constant specifying the registry branch to read
///          @param Key is a string specifying the name of registry key to read
///          @param Name is a string specifying the the name of the registry value to read
///          @param Value returns the integer value read from the registry, only valid if
///                       the function result is true.
///          @returns true, if an integer value could be read, false, if it does not exist
///                      or is not an integer value. </summary>
function GetRegValue(_RootKey: HKEY; const _Key, _Name: string; out _Value: Integer): Boolean; overload;

///<summary> Writes a string value from the registry.
///          @param RootKey is the HK_* constant specifying the registry branch to read
///          @param Key is a string specifying the name of registry key to read
///          @param Name is a string specifying the the name of the registry value to read
///          @param Value is the string value to write to the registry. </summary>
procedure SetRegValue(_RootKey: HKEY; const _Key, _Name, _Value: string); overload;

///<summary> Reads a file's version information and returns the four parts of the version
///          number.
///          @param Filename is a string with the name of the file to check, if empty, the
///                 current program is checked.
///          @param Major is a word returning the major version number
///          @param Minor is a word returning the minor version number
///          @param Revision is a word returning the revision number
///          @param Build is a word returning the build number
///          @returns True, if version information was found,
///                   False if the file does not contain any version information </summary>
///          @note: There is also an overloaded version that returns words.
function GetFileBuildInfo(_Filename: string;
  out _Major, _Minor, _Revision, _Build: Integer): Boolean; overload;

///<summary> Reads a file's version information and returns the four parts of the version
///          number.
///          @param Filename is a string with the name of the file to check, if empty, the
///                 current program is checked.
///          @param Major is an integer returning the major version number
///          @param Minor is an integer returning the minor version number
///          @param Revision is an integer returning the revision number
///          @param Build is an integer returning the build number
///          @returns True, if version information was found,
///                   False if the file does not contain any version information </summary>
///          @note: There is also an overloaded version that returns integers.
function GetFileBuildInfo(const _Filename: string;
  out _Major, _Minor, _Revision, _Build: Word): Boolean; overload;

///<summary> Reads a file's version information
///          @param Filename is the file whose version information should be read,
///                          if empty the current program is checked.
///          @param AllowException is a boolean that determines if missing version information
///                                should cause an exception, if false, 'unknown' is returned
///          @returns a string containing the version number as Major.Minor.Revision.Build
///                   or 'unknown' if it can not be determined. </summary>
function GetFileBuildInfo(const _Filename: string = ''; _AllowException: Boolean = False): string; overload;

///<summary> Reads a file's product information and returns the four parts of the version
///          number.
///          @param Filename is a string with the name of the file to check, if empty, the
///                 current program is checked.
///          @param Major is a word returning the major version number
///          @param Minor is a word returning the minor version number
///          @param Revision is a word returning the revision number
///          @param Build is a word returning the build number
///          @returns True, if version information was found,
///                   False if the file does not contain any version information </summary>
function GetFileProductInfo(_Filename: string;
  out _Major, _Minor, _Revision, _Build: Integer): Boolean; overload;

///<summary> Reads a file's product information
///          @param Filename is the file whose version information should be read,
///                          if empty the current program is checked.
///          @param AllowException is a boolean that determines if missing version information
///                                should cause an exception, if false, 'unknown' is returned
///          @returns a string containing the version number as Major.Minor.Revision.Build
///                   or 'unknown' if it can not be determined. </summary>
function GetFileProductInfo(const _Filename: string; _AllowException: Boolean = False): string; overload;

///<summary> @returns the filename of the current module </summary>
function GetModuleFilename: string; overload;
function GetModuleFilename(const _Module: Cardinal): string; overload;

///<summary> registers an open command for a file extension
///          @param Extension is the file extension to register e.g. '.bla'
///          @param DocumentName is the user friendly name for the file type e.g. 'Bla bla file'
///          @param OpenCommand is the command that must be executed to open that file
///                             e.g. '"c:\program files\My Company\My App\myprog.exe" "%1"'
///                             Don't forget to put quotes around both, the executable name and
///                             the parameter, and also don't forget to pass the parameter.
///          @param ShortDocName is an internal, short name for the file type e.g. 'MyProg.bla'
//                               You should always supply one
procedure RegisterFileAssociation(const _Extension, _DocumentName, _OpenCommand: string); overload; deprecated;
procedure RegisterFileAssociation(const _Extension, _ShortDocName, _DocumentName, _OpenCommand: string); overload;

function OsHasNTSecurity: Boolean;

///<summary> Checks whether the currently logged on user (the one who runs this process) has administrator rights
///          (In Win9x this always returns true, in WinNT+ it checks whether the user is member of the
///          administrators group </summary>
function CurrentUserHasAdminRights: Boolean;

///<summary> tries to open a file with the associated application
///          @param Filename is the name of the file to open
///          @returns true on success, false otherwise </summary>
function OpenFileWithAssociatedApp(const _Filename: string; _ShowAssociateDialog: Boolean = False): Boolean;

///<summary> Calls ShellExecuteEx with the given parameters </summary>
function ShellExecEx(const _Filename: string; const _Parameters: string;
  const _Verb: string; _CmdShow: Integer; _ShowAssociateDialog: Boolean = False): Boolean;

///<summary>
/// Opens Windows Explorer and selects the given file, if it exists. If it doesn't exist, it
/// opens the first directory in that filename that exists.
/// @returns True if the file exists, False if not </summary>
function OpenExplorerAndSelectFile(const _Filename: string): Boolean;

///<summary> Simulates mouse movement, so that the screen saver does not start. This is the only
///          way to prevent the screen saver to start from Vista onwards if password protection
///          is enabled. (according to http://stackoverflow.com/a/1675793/49925)
procedure JiggleMouse;

type
  TPowerRequestType = (
    PowerRequestDisplayRequired = 0,
    PowerRequestSystemRequired = 1,
    PowerRequestAwayModeRequired = 2,
    PowerRequestExecutionRequired = 3);

///<summary>
/// simple interface for the Windows API  PowerCreateRequest / PowerSetRequest / PowerClearRequest
/// Avaiable in Windows 7 and later.
/// @returns an interface which, when released, calls PowerClearRequest
/// See also BlockScreenSaver </summary>
function SetPowerRequest(const _Reason: WideString; _RequestType: TPowerRequestType): IInterface;
function TrySetPowerRequest(const _Reason: WideString; _RequestType: TPowerRequestType; out _Request: IInterface): Boolean;

///<summary>
/// Uses the Windows API PowerCreateRequest and PowerSetRequest (Windows 7 and later) to
/// prevent the screen saver from starting.
/// @param Reason is the reason why the screen saver is blocked
/// @returns an IInterface. While a reference to this interface active the screen saver is blocked </summary>
function BlockScreenSaver(const _Reason: WideString): IInterface;

///<summary> Calls Windows.CharToOem </summary>
function CharToOem(const _s: string): AnsiString;
///<summary> Calls Windows.OemToChar </summary>
function OemToChar(const _s: AnsiString): string;

function GetModifierKeyState: TShiftState;
///<summary>
/// @returns true, if the Alt key is currently pressed </summary>
function IsAltDown: Boolean;
///<summary>
/// @returns true, if the Ctrl key is currently pressed </summary>
function IsCtrlDown: Boolean;
///<summary>
/// @returns true, if the Shift key is currently pressed </summary>
function IsShiftDown: Boolean;

function ShutdownWindows(_Force, _Reboot: Boolean): Boolean;

///<summary>
/// @Returns the volume name of the given drive
/// @raises EOsError if an error occurs (e.g. if the drive does not exist) </summary>
function GetHdVolumeName(_DriveChar: Char; _RaiseException: Boolean = False): string;
function TryGetHdVolumeName(_DriveChar: Char; out _Name: string): Boolean;

///<summary>
/// Returns a human readable string with the OS version, e.g.
/// 'Windows XP', 'Windows 8.1' or 'Windows 10 (1809)'
/// (For Windows 10 this is probably more a guess since there seems to be no official method
///  for getting that version. (Or maybe I haven't looked hard enough.))
/// @Note: Versions older than Windows 2000 (Windows 95/98/ME and Windows NT3.1, 3.5 and 4)
/// did not support the GetVersionEx API call used here. But we don't really care any more. </summary>
function GetOsVersionString: string;

type
  TWinCurrentRec = record
    BuildLab: string;
    BuildLabEx: string;
    CSDBuildNumber: string;
    CSDVersion: string; // Service Pack
    CurrentBuildNumber: string;
    CurrentVersion: string;
    EditionId: string; // e.g. Professional
    ProductName: string;
    ReleaseId: string; // Windows 10 version
    UBR: Integer;
  end;

///<summary>
/// Reads HKLM\Software\Microsoft\Windows NT\CurrentVersion:
/// If one of the values cannot be read, it will be set to an empty string
/// or 0 (UBR)
/// @param Values is a record returning the values read, only valid if Result = True
/// @returns True, if the registry key exists
///          False if not </summary>
function TryGetWindowsVersionFromRegistry(out _Values: TWinCurrentRec): Boolean;

///<summary>
/// Calls TryGetWindowsVersionFromRegistry and builds a string from the result </summary>
function GetWindowsVersionStringFromRegistry: string;

///<summary>
/// Retrieves the product version information from the kernel32.dll.
/// @param on return Major, Minor, Revision and Build are the product version of the kernel32.dll,
///        only valid if Result = True
/// @returns true, if the version information could be read
/// @Note: Since in newer Windows (version 8 and later) the GetVersionEx WinAPI function
/// is lying, we might as well directly call GetKernel32Version instead. </summary>
function GetKernel32Version(out _Major, _Minor, _Revision, _Build: Integer): Boolean;

///<summary>
/// Uses the GetLogicalProcessorInformation WinAPI function to get the size of the processor's cache line </summary>
function GetCpuCacheLineSize: Integer;

///<summary>
/// Uses the GetSystemInfo WinAPI function to get number of logical processors </summary>
function GetCpuLogicalProcessorCount: Integer;

implementation

uses
  Registry,
  ShellApi,
  u_dzMiscUtils,
  u_dzClassUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

function IsShiftDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Shift] and 128) <> 0);
end;

function IsCtrlDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[VK_CONTROL] and 128) <> 0);
end;

function IsAltDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[VK_MENU] and 128) <> 0);
end;

function GetModifierKeyState: TShiftState;
var
  State: TKeyboardState;
begin
  Result := [];
  if GetKeyboardState(State) then begin
    if ((State[vk_Shift] and 128) <> 0) then
      Include(Result, ssShift);
    if ((State[VK_CONTROL] and 128) <> 0) then
      Include(Result, ssCtrl);
    if ((State[VK_MENU] and 128) <> 0) then
      Include(Result, ssAlt);
  end;
end;

const
  // https://msdn.microsoft.com/en-us/library/cc761107.aspx
  CNLEN = 15; // = MAX_COMPUTERNAME_LENGTH
  DNLEN = CNLEN;
  UNLEN = 256;
  MAX_BUFFER_SIZE = CNLEN + UNLEN + 1 + 1;

function GetComputerName(_RaiseException: Boolean): string;
var
  BufSize: DWORD;
begin
  BufSize := MAX_BUFFER_SIZE;
  SetLength(Result, BufSize + 1);
  if Windows.GetComputerName(@Result[1], BufSize) then
    SetLength(Result, BufSize)
  else begin
    if _RaiseException then
      RaiseLastOSError;
    Result := '';
  end;
end;

function GetUserName(_RaiseException: Boolean = False): string;
var
  BufSize: DWORD;
begin
  BufSize := MAX_BUFFER_SIZE;
  SetLength(Result, BufSize + 1);
  if Windows.GetUserName(PChar(Result), BufSize) then begin
    SetLength(Result, BufSize - 1);
  end else begin
    if _RaiseException then
      RaiseLastOSError;
    Result := '';
  end;
end;

function GetComputerNameEx(_NameFormat: COMPUTER_NAME_FORMAT = ComputerNameNetBIOS;
  _RaiseException: Boolean = False): string;
const
{$IF CompilerVersion >= 20} // Delphi 2009 / BDS 6
  ENTRY_POINT_NAME = 'GetComputerNameExW';
{$ELSE}
  ENTRY_POINT_NAME = 'GetComputerNameExA';
{$IFEND}
var
  BufSize: DWORD;
  GetComputerNameExApi: function(NameFormat: DWORD; lpNameBuffer: PChar; var nSize: ULONG): BOOL; stdcall;
begin
  GetComputerNameExApi := GetProcAddress(GetModuleHandle(kernel32), ENTRY_POINT_NAME);
  if not Assigned(GetComputerNameExApi) then
    raise EOSError.CreateFmt(_('Could not get address of entry point %s in module %s'), [ENTRY_POINT_NAME, kernel32]);

  BufSize := MAX_BUFFER_SIZE;
  SetLength(Result, BufSize + 1);
  if GetComputerNameExApi(DWORD(_NameFormat), PChar(Result), BufSize) then
    SetLength(Result, BufSize)
  else begin
    if _RaiseException then
      RaiseLastOSError;
    Result := '';
  end;
end;

function GetUserNameEx(_NameFormat: EXTENDED_NAME_FORMAT = NameSamCompatible;
  _RaiseException: Boolean = False): string;
const
  DLL_NAME = 'secur32.dll';
{$IF CompilerVersion >= 20} // Delphi 2009 / BDS 6
  ENTRY_POINT_NAME = 'GetUserNameExW';
{$ELSE}
  ENTRY_POINT_NAME = 'GetUserNameExA';
{$IFEND}
var
  Len: Cardinal;
  ModuleHandle: THandle;
  LastError: Cardinal;
  GetUserNameExApi: function(NameFormat: DWORD; lpNameBuffer: PChar; var nSize: ULONG): BOOL; stdcall;
begin
  ModuleHandle := SysUtils.SafeLoadLibrary(DLL_NAME);
  try
    GetUserNameExApi := GetProcAddress(ModuleHandle, ENTRY_POINT_NAME);
    if not Assigned(GetUserNameExApi) then
      Result := 'username unknown'
    else begin
      Len := MAX_BUFFER_SIZE;
      SetLength(Result, Len);
      if GetUserNameExApi(DWORD(_NameFormat), PChar(Result), Len) then begin
        SetLength(Result, Len);
      end else begin
        LastError := GetLastError;
        Result := Format('username unknown (%d)', [LastError]);
      end;
    end;
  finally
    FreeLibrary(ModuleHandle);
  end;
end;

function ExpandEnvironmentStrings(const _WithVariables: string): string;
var
  Res: Integer;
  MaxLen: Integer;
  LastError: Cardinal;
begin
  MaxLen := Length(_WithVariables) + 16 * 1024; // 16 KB should be enough for everybody... ;-)
  SetLength(Result, MaxLen);
  Res := Windows.ExpandEnvironmentStrings(PChar(_WithVariables), PChar(Result), MaxLen);
  if Res > MaxLen then begin
    MaxLen := Res + 1;
    SetLength(Result, MaxLen);
    Res := Windows.ExpandEnvironmentStrings(PChar(_WithVariables), PChar(Result), MaxLen);
  end;
  if Res = 0 then begin
    LastError := GetLastError;
    RaiseLastOSErrorEx(LastError, _('Error %1:s (%0:d) calling Windows.ExpandEnvironmentStrings'));
  end;
  SetLength(Result, Res - 1);
end;

function GetEnvironmentVars(const _Vars: TStrings): Boolean;
var
  Vars: PChar;
  p: PChar;
begin
  Result := False;
  _Vars.BeginUpdate;
  try
    _Vars.Clear;
    Vars := Windows.GetEnvironmentStrings;
    if Vars <> nil then begin
      try
        p := Vars;
        while p^ <> #0 do begin
          _Vars.Add(p);
          p := StrEnd(p);
          Inc(p);
        end;
      finally
        Windows.FreeEnvironmentStrings(Vars);
      end;
      Result := True;
    end;
  finally
    _Vars.EndUpdate;
  end;
end;

function GetHomeDir: string;
begin
  Result := GetEnvironmentVariable('HOME');
  if Result = '' then
    Result := GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable('HOMEPATH');
end;

function GetRegValue(_RootKey: HKEY; const _Key, _Name: string; out _Value: Integer): Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := _RootKey;
    if Reg.OpenKeyReadOnly(_Key) then
      try
        try
          _Value := Reg.ReadInteger(_Name);
          Result := True;
        except
          // ignore exceptions, return false
        end; //FI:W501
      finally
        Reg.CloseKey;
      end
  finally
    Reg.Free;
  end;
end;

procedure SetRegValue(_RootKey: HKEY; const _Key, _Name, _Value: string); overload;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := _RootKey;
    if Reg.OpenKey(_Key, True) then
      try
        Reg.WriteString(_Name, _Value);
      finally
        Reg.CloseKey;
      end
  finally
    Reg.Free;
  end;
end;

procedure RegisterFileAssociation(const _Extension, _ShortDocName, _DocumentName, _OpenCommand: string);
begin
  SetRegValue(HKEY_CLASSES_ROOT, _Extension, '', _ShortDocName);
  SetRegValue(HKEY_CLASSES_ROOT, _ShortDocName, '', _DocumentName);
  SetRegValue(HKEY_CLASSES_ROOT, Format('%s\shell\command', [_ShortDocName]), '', _OpenCommand);
end;

procedure RegisterFileAssociation(const _Extension, _DocumentName, _OpenCommand: string);
begin
  RegisterFileAssociation(_Extension, _DocumentName, _DocumentName, _OpenCommand);
end;

function GetModuleFilename(const _Module: Cardinal): string;
var
  Buffer: array[0..260] of Char;
begin
  SetString(Result, Buffer, Windows.GetModuleFilename(_Module, Buffer, SizeOf(Buffer)))
end;

function GetModuleFilename: string;
begin
  Result := GetModuleFilename(HInstance);
end;

function GetFileBuildInfo(_Filename: string;
  out _Major, _Minor, _Revision, _Build: Integer): Boolean;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  if _Filename = '' then
    _Filename := GetModuleFilename;
  VerInfoSize := GetFileVersionInfoSize(PChar(_Filename), Dummy);
  Result := (VerInfoSize <> 0);
  if Result then begin
    GetMem(VerInfo, VerInfoSize);
    try
      GetFileVersionInfo(PChar(_Filename), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      with VerValue^ do begin
        _Major := dwFileVersionMS shr 16;
        _Minor := dwFileVersionMS and $FFFF;
        _Revision := dwFileVersionLS shr 16;
        _Build := dwFileVersionLS and $FFFF;
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  end;
end;

function GetFileProductInfo(_Filename: string;
  out _Major, _Minor, _Revision, _Build: Integer): Boolean;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  if _Filename = '' then
    _Filename := GetModuleFilename;
  VerInfoSize := GetFileVersionInfoSize(PChar(_Filename), Dummy);
  Result := (VerInfoSize <> 0);
  if Result then begin
    GetMem(VerInfo, VerInfoSize);
    try
      GetFileVersionInfo(PChar(_Filename), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      with VerValue^ do begin
        _Major := dwProductVersionMS shr 16;
        _Minor := dwProductVersionMS and $FFFF;
        _Revision := dwProductVersionLS shr 16;
        _Build := dwProductVersionLS and $FFFF;
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  end;
end;

function GetFileBuildInfo(const _Filename: string;
  out _Major, _Minor, _Revision, _Build: Word): Boolean;
var
  Major, Minor, Revision, Build: Integer;
begin
  Result := GetFileBuildInfo(_Filename, Major, Minor, Revision, Build);
  if Result then begin
    _Major := Major;
    _Minor := Minor;
    _Revision := Revision;
    _Build := Build;
  end;
end;

function GetFileBuildInfo(const _Filename: string; _AllowException: Boolean): string;
var
  Major: Integer;
  Minor: Integer;
  Revision: Integer;
  Build: Integer;
begin
  if GetFileBuildInfo(_Filename, Major, Minor, Revision, Build) then
    Result := Format('%d.%d.%d.%d', [Major, Minor, Revision, Build])
  else if _AllowException then
    raise EOFNoFileinfo.CreateFmt(_('No version information available for %s'), [_Filename])
  else
    Result := 'unknown';
end;

function GetFileProductInfo(const _Filename: string; _AllowException: Boolean): string;
var
  Major: Integer;
  Minor: Integer;
  Revision: Integer;
  Build: Integer;
begin
  if GetFileProductInfo(_Filename, Major, Minor, Revision, Build) then
    Result := Format('%d.%d.%d.%d', [Major, Minor, Revision, Build])
  else if _AllowException then
    raise EOFNoFileinfo.CreateFmt(_('No version information available for %s'), [_Filename])
  else
    Result := 'unknown';
end;

function OsHasNTSecurity: Boolean;
var
  vi: TOSVersionInfo;
begin
  FillChar(vi, SizeOf(vi), 0);
  vi.dwOSVersionInfoSize := SizeOf(vi);
  GetVersionEx(vi);
  Result := (vi.dwPlatformId = VER_PLATFORM_WIN32_NT);
end;

const
  SECURITY_NT_AUTHORITY: SID_IDENTIFIER_AUTHORITY = (Value: (0, 0, 0, 0, 0, 5)); // ntifs

  SECURITY_BUILTIN_DOMAIN_RID: DWORD = $00000020;
  DOMAIN_ALIAS_RID_ADMINS: DWORD = $00000220;
  DOMAIN_ALIAS_RID_USERS: DWORD = $00000221;
  DOMAIN_ALIAS_RID_GUESTS: DWORD = $00000222;
  DOMAIN_ALIAS_RID_POWER_: DWORD = $00000223;

function CurrentUserIsInAdminGroup: Boolean;
var
  bSuccess: Boolean;
  psidAdministrators: Pointer;
  X: Integer;
  ptgGroups: PTokenGroups;
  hAccessToken: THandle;
  dwInfoBufferSize: Cardinal;
begin
  Result := False;
  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);
  if not bSuccess then begin
    if GetLastError = ERROR_NO_TOKEN then
      bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);
  end;
  if bSuccess then begin
    try
      GetMem(ptgGroups, 1024);
      try
        bSuccess := GetTokenInformation(hAccessToken, TokenGroups, ptgGroups, 1024, dwInfoBufferSize);
        if bSuccess then begin
          AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdministrators);
          try
{$R-}
            for X := 0 to ptgGroups.GroupCount - 1 do
              if EqualSid(psidAdministrators, ptgGroups.Groups[X].Sid) then begin
                Result := True;
                Break;
              end;
          finally
{$R+}
            FreeSid(psidAdministrators);
          end;
        end;
      finally
        FreeMem(ptgGroups);
      end;
    finally
      CloseHandle(hAccessToken);
    end;
  end;
end;

type
  TCheckTokenMembership = function(TokenHandle: THandle; SidToCheck: PSID; var IsMember: BOOL): BOOL; stdcall;
var
  CheckTokenMembership: TCheckTokenMembership = nil;

function IsUserAdmin: Boolean;
var
  b: BOOL;
  AdministratorsGroup: PSID;
  Hdl: HMODULE;
begin
  {
    This function returns true if you are currently running with admin privileges.
    In Vista and later, if you are non-elevated, this function will return false
    (you are not running with administrative privileges).
    If you *are* running elevated, then IsUserAdmin will return true, as you are
    running with admin privileges.

    Windows provides this similar function in Shell32.IsUserAnAdmin.
    But the function is deprecated, and this code is lifted
    from the docs for CheckTokenMembership:
      http://msdn.microsoft.com/en-us/library/aa376389.aspx
  }

  {
    Routine Description: This routine returns TRUE if the callers
    process is a member of the Administrators local group. Caller is NOT
    expected to be impersonating anyone and is expected to be able to
    open its own process and process token.
      Arguments: None.
      Return Value:
        TRUE - Caller has Administrators local group.
        FALSE - Caller does not have Administrators local group.
  }
  { idea from:
    http://stackoverflow.com/a/8290384/49925
    but heavily modified }
  Result := False;
  if not AllocateAndInitializeSid(
    SECURITY_NT_AUTHORITY,
    2, //2 sub-authorities
    SECURITY_BUILTIN_DOMAIN_RID, //sub-authority 0
    DOMAIN_ALIAS_RID_ADMINS, //sub-authority 1
    0, 0, 0, 0, 0, 0, //sub-authorities 2-7 not passed
    AdministratorsGroup) then
    Exit; //=>
  try
    if Assigned(CheckTokenMembership) then begin
      Hdl := LoadLibrary(advapi32);
      if Hdl = 0 then
        Exit; //=>
      @CheckTokenMembership := GetProcAddress(Hdl, 'CheckTokenMembership');
      if Assigned(CheckTokenMembership) then begin
        FreeLibrary(Hdl);
        Exit; //=>
      end;
    end;
    if CheckTokenMembership(0, AdministratorsGroup, b) then
      Result := b;
  finally
    FreeSid(AdministratorsGroup);
  end;
end;

function CurrentUserHasAdminRights: Boolean;
begin
  if OsHasNTSecurity then
    Result := IsUserAdmin // CurrentUserIsInAdminGroup
  else
    Result := True;
end;

function ShellExecEx(const _Filename: string; const _Parameters: string;
  const _Verb: string; _CmdShow: Integer; _ShowAssociateDialog: Boolean = False): Boolean;
var
  Sei: TShellExecuteInfo;
begin
  FillChar(Sei, SizeOf(Sei), #0);
  Sei.cbSize := SizeOf(Sei);
  Sei.FMask := SEE_MASK_DOENVSUBST;
  if not _ShowAssociateDialog then
    Sei.FMask := Sei.FMask or SEE_MASK_FLAG_NO_UI;
  Sei.lpFile := PChar(_Filename);
  if _Parameters <> '' then
    Sei.lpParameters := PChar(_Parameters)
  else
    Sei.lpParameters := nil;
  if _Verb <> '' then
    Sei.lpVerb := PChar(_Verb)
  else
    Sei.lpVerb := nil;
  Sei.nShow := _CmdShow;
  Result := ShellExecuteEx(@Sei);
end;

function OpenExplorerAndSelectFile(const _Filename: string): Boolean;
var
  fn: string;
begin
  fn := _Filename;
  Result := FileExists(fn);
  if Result then begin
    ShellExecEx('explorer.exe', '/select,"' + fn + '"', '', SW_SHOWNORMAL);
    Exit; //==>
  end;

  while ExtractFileName(fn) <> '' do begin
    fn := ExtractFileDir(fn);
    if (fn <> '') and DirectoryExists(fn) then begin
      ShellExecEx(fn, '', '', SW_SHOWNORMAL);
      Exit; //==>
    end;
  end;
end;

function OpenFileWithAssociatedApp(const _Filename: string; _ShowAssociateDialog: Boolean = False): Boolean;
begin
  Result := ShellExecEx(_Filename, '', 'open', SW_SHOWNORMAL, _ShowAssociateDialog);
end;

procedure JiggleMouse;
var
  Inpt: TInput;
begin
  Inpt.Itype := INPUT_MOUSE;
  Inpt.mi.dx := 0;
  Inpt.mi.dy := 0;
  Inpt.mi.mouseData := 0;
  Inpt.mi.dwFlags := MOUSEEVENTF_MOVE;
  Inpt.mi.Time := 0;
  Inpt.mi.dwExtraInfo := 0;
  SendInput(1, Inpt, SizeOf(Inpt));
end;

{ TScreenSaverBlocker }

const
  POWER_REQUEST_CONTEXT_VERSION = 0;
  POWER_REQUEST_CONTEXT_DETAILED_STRING = 2;
  POWER_REQUEST_CONTEXT_SIMPLE_STRING = 1;
type
  PReasonContext = ^TReasonContext;
  TReasonContext = record
    Version: ULONG;
    Flags: DWORD;
    case Boolean of
      False: (
        SimpleReasonString: PWideChar;
        );
      True: (
        Detailed: record
          LocalizedReasonModule: HMODULE;
          LocalizedReasonId: ULONG;
          ReasonStringCount: ULONG;
          ReasonStrings: PPWideChar;
        end;
        );
  end;

type
  TPowerCreateRequest = function(_Context: PReasonContext): THandle; stdcall;
  TPowerSetRequest = function(_Handle: THandle; _RequestType: TPowerRequestType): LongBool; stdcall;
  TPowerClearRequest = function(_Handle: THandle; _RequestType: TPowerRequestType): LongBool; stdcall;

type
  TPowerRequest = class(TInterfacedObject, IInterface)
  private
    FDllHandle: HMODULE;
    FRequestHandle: THandle;
    PowerCreateRequest: TPowerCreateRequest;
    PowerSetRequest: TPowerSetRequest;
    PowerClearRequest: TPowerClearRequest;
    FContext: TReasonContext;
    FReason: array[0..255] of WideChar;
  public
    constructor Create(const _Reason: WideString; _RequestType: TPowerRequestType);
    destructor Destroy; override;
  end;

constructor TPowerRequest.Create(const _Reason: WideString; _RequestType: TPowerRequestType);
var
  LastError: DWORD;
  Error: EPowerRequestFailed;
begin
  inherited Create;
  FDllHandle := SafeLoadLibrary(kernel32);
  PowerCreateRequest := GetProcAddress(FDllHandle, 'PowerCreateRequest');
  PowerSetRequest := GetProcAddress(FDllHandle, 'PowerSetRequest');
  PowerClearRequest := GetProcAddress(FDllHandle, 'PowerClearRequest');
  if not Assigned(PowerCreateRequest) or not Assigned(PowerSetRequest) or not Assigned(PowerClearRequest) then
    raise EOsFunc.Create(_('Could not initialize the PowerXxxxRequest functions from kernel32.'));

  Assert(Length(_Reason) < 255);
  Move(_Reason[1], FReason[0], Length(_Reason) * SizeOf(WideChar) + 1);

  FContext.Version := POWER_REQUEST_CONTEXT_VERSION;
  FContext.Flags := POWER_REQUEST_CONTEXT_SIMPLE_STRING;
  FContext.SimpleReasonString := @FReason[0];

  FRequestHandle := PowerCreateRequest(@FContext);
  if FRequestHandle = INVALID_HANDLE_VALUE then begin
    LastError := GetLastError;
    if LastError <> 0 then
      Error := EPowerRequestFailed.CreateFmt(_('PowerCreateRequest failed with error code %d. (%s)'),
        [LastError, SysErrorMessage(LastError)])
    else
      Error := EPowerRequestFailed.CreateFmt(_('PowerCreateRequest failed with error code %d. (unknown error)'),
        [LastError]);
    Error.ErrorCode := LastError;
    raise Error;
  end;

  if not PowerSetRequest(FRequestHandle, _RequestType) then begin
    LastError := GetLastError;
    if LastError <> 0 then
      Error := EPowerRequestFailed.CreateFmt(_('PowerCreateRequest failed with error code %d. (%s)'),
        [LastError, SysErrorMessage(LastError)])
    else
      Error := EPowerRequestFailed.CreateFmt(_('PowerCreateRequest failed with error code %d. (unknown error)'),
        [LastError]);
    Error.ErrorCode := LastError;
    raise Error;
  end;
end;

destructor TPowerRequest.Destroy;
begin
  if FRequestHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FRequestHandle);
  if FDllHandle <> 0 then
    FreeLibrary(FDllHandle);
  inherited;
end;

function BlockScreenSaver(const _Reason: WideString): IInterface;
begin
  Result := SetPowerRequest(_Reason, PowerRequestDisplayRequired);
end;

function SetPowerRequest(const _Reason: WideString; _RequestType: TPowerRequestType): IInterface;
begin
  Result := TPowerRequest.Create(_Reason, _RequestType);
end;

function TrySetPowerRequest(const _Reason: WideString; _RequestType: TPowerRequestType; out _Request: IInterface): Boolean;
begin
  try
    _Request := SetPowerRequest(_Reason, _RequestType);
    Result := True;
  except
    Result := False;
    _Request := nil;
  end;
end;

function CharToOem(const _s: string): AnsiString;
begin
  SetLength(Result, Length(_s));
  Windows.CharToOem(PChar(_s), PAnsiChar(Result))
end;

function OemToChar(const _s: AnsiString): string;
begin
  SetLength(Result, Length(_s));
  Windows.OemToChar(PAnsiChar(_s), PChar(Result));
end;

function ShutdownWindows(_Force, _Reboot: Boolean): Boolean;
var
  hToken: THandle;
  tp: TTokenPrivileges;
  h,
    Flag: DWORD;
begin
  // if Windows NT/2000/XP or later, we first need to get the rights
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then begin
    Flag := EWX_POWEROFF;
    OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, hToken);
    LookupPrivilegeValue(nil, 'SeShutdownPrivilege', tp.Privileges[0].Luid);
    tp.PrivilegeCount := 1;
    tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
    h := 0;
    AdjustTokenPrivileges(hToken, False, tp, 0, PTokenPrivileges(nil)^, h);
    CloseHandle(hToken);
  end else
    Flag := EWX_SHUTDOWN; // Win 98 / ME

  if _Force then
    Flag := Flag or EWX_FORCE;
  if _Reboot then
    Flag := Flag or EWX_REBOOT;

  Result := ExitWindowsEx(Flag, 0);
end;

function TryGetHdVolumeName(_DriveChar: Char; out _Name: string): Boolean;
var
  OldErrorMode: Integer;
  NotUsed, VolFlags: DWORD;
  Buf: array[0..MAX_PATH] of Char;
  Res: Boolean;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Res := GetVolumeInformation(PChar(_DriveChar + ':\'), Buf,
      SizeOf(Buf), nil, NotUsed, VolFlags, nil, 0);
    if Res then begin
      _Name := Buf;
      Result := True;
    end else begin
      Result := False;
    end;
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

function GetHdVolumeName(_DriveChar: Char; _RaiseException: Boolean = False): string;
begin
  if not TryGetHdVolumeName(_DriveChar, Result) then begin
    if _RaiseException then
      RaiseLastOSError
    else
      Result := '';
  end;
end;

// Delphi 2007 declares only the GetVersionEx function with a TOsVersionInfo record,
// so we have to add an overloaded version of it for a TOsVersionInfoEx record.
type
  TOsVersionInfoExW = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of WideChar; { Maintenance UnicodeString for PSS usage }
    wServicePackMajor: Word;
    wServicePackMinor: Word;
    wSuiteMask: Word;
    wProductType: Byte;
    wReserved: Byte;
  end;

  TOsVersionInfoExA = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of AnsiChar; { Maintenance AnsiString for PSS usage }
    wServicePackMajor: Word;
    wServicePackMinor: Word;
    wSuiteMask: Word;
    wProductType: Byte;
    wReserved: Byte;
  end;

function GetVersionExA(var lpVersionInformation: TOsVersionInfoExA): BOOL; stdcall; overload; external kernel32 Name 'GetVersionExA';

function GetVersionExW(var lpVersionInformation: TOsVersionInfoExW): BOOL; stdcall; overload; external kernel32 Name 'GetVersionExW';

{$IFDEF unicode}
type
  TOsVersionInfoEx = TOsVersionInfoExW;

function GetVersionEx(var lpVersionInformation: TOsVersionInfoEx): BOOL; stdcall; overload; external kernel32 Name 'GetVersionExW';

{$ELSE}
type
  TOsVersionInfoEx = TOsVersionInfoExA;

function GetVersionEx(var lpVersionInformation: TOsVersionInfoEx): BOOL; stdcall; overload; external kernel32 Name 'GetVersionExA';

{$ENDIF}

function GetKernel32Version(out _Major, _Minor, _Revision, _Build: Integer): Boolean;
var
  Kernel32Handle: DWORD;
  Kernel32Filename: string;
begin
  Kernel32Handle := GetModuleHandle('kernel32.dll');
  Kernel32Filename := u_dzOsUtils.GetModuleFilename(Kernel32Handle);
  Result := GetFileProductInfo(Kernel32Filename, _Major, _Minor, _Revision, _Build);
end;

function OsVersionFromKernel32Version(_IsServer: Boolean): string;
var
  Major: Integer;
  Minor: Integer;
  Revision: Integer;
  Build: Integer;
begin
  // Starting with Windows 8 the GerVersionEx function is lying:
  // > With the release of Windows 8.1, the behavior of the GetVersionEx API has changed
  // in the value it will return for the operating system version. The value returned
  // by the GetVersionEx function now depends on how the application is manifested.
  // Applications not manifested for Windows 8.1 or Windows 10 will return the
  // Windows 8 OS version value (6.2). Once an application is manifested for a given
  // operating system version, GetVersionEx will always return the version that the
  // application is manifested for in future releases. To manifest your applications
  // for Windows 8.1 or Windows 10 <
  // https://docs.microsoft.com/en-us/windows/desktop/api/sysinfoapi/nf-sysinfoapi-getversionexa
  // So, we can only get the correct version, if the Delphi IDE has a manifest telling
  // Windows that it supports the version installed. This of course will not work if
  // the Delphi version is older than the Windows version (e.g. Delphi 2007 won't know
  // about anything newer than Windows XP).
  // Instead we now use GetFileVersionInfo on kernel32.dll.
  // https://docs.microsoft.com/en-us/windows/desktop/sysinfo/getting-the-system-version

  if not GetKernel32Version(Major, Minor, Revision, Build) then begin
    Result := _('Unknown OS version (GetFileProductInfo(kernel32.dll) failed).');
    Exit; //==>
  end;

  // we start with the most likely ones (not that it matters much here, nobody is going
  // to call this function so often that it makes a difference performance wise)
  if (Major = 10) and (Minor = 0) then begin
    // We have reached the "last" Windows version 10, which of course still has
    // new version numbers internally for each major update:
    // For whatever reason, the Windows 10 revision number is often called build number.
    // (e.g. https://en.wikipedia.org/wiki/Windows_10_version_history )
    // But it is really the revision number.
    //
    // As of 2019-04-21 there are the following revisions (newest first):
    // Windows 10 (1903) 10.0.18362
    // Windows 10 (1809) 10.0.17763
    // Windows 10 (1803) 10.0.17134
    // Windows 10 (1709) 10.0.16299
    // Windows 10 (1703) 10.0.15063
    // Windows 10 (1607) 10.0.14393
    // Windows 10 (1511) 10.0.10586
    // Windows 10 (1507) 10.0.10240 (the first release from 2015)

    if _IsServer then
      Result := 'Windows Server 2016'
    else
      Result := 'Windows 10';

    // todo: There should be a way to detect that there has been a new major Windows 10 update ...
    if Revision >= 18362 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1903, Major, Minor, Revision, Build]);
    end else if Revision >= 17763 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1809, Major, Minor, Revision, Build]);
    end else if Revision >= 17134 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1803, Major, Minor, Revision, Build]);
    end else if Revision >= 16299 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1709, Major, Minor, Revision, Build]);
    end else if Revision >= 15063 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1703, Major, Minor, Revision, Build]);
    end else if Revision >= 14393 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1607, Major, Minor, Revision, Build]);
    end else if Revision >= 10586 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1511, Major, Minor, Revision, Build]);
    end else if Revision >= 10240 then begin
      Result := Format('%s (%d) (kernel %d.%d.%d build %d)', [Result, 1507, Major, Minor, Revision, Build]);
    end else
      Result := Format(_('Windows %d.%d rev %d build %d (from kernel32)'), [Major, Minor, Revision, Build]);
  end else if Major = 6 then begin
    if Minor = 3 then begin
      if _IsServer then
        Result := 'Windows Server 2012 R2'
      else
        Result := 'Windows 8.1';
    end else
      Result := Format(_('Unknown Windows %d.%d rev %d build %d (from kernel32)'), [Major, Minor, Revision, Build]);
  end else
    Result := Format(_('Unknown Windows %d.%d rev %d build %d (from kernel32)'), [Major, Minor, Revision, Build]);
end;

function GetOsVersionString: string;
const
  VER_NT_WORKSTATION = $0000001;
  VER_NT_DOMAIN_CONTROLLER = $0000002;
  VER_NT_SERVER = $0000003;
var
  VerInfo: TOsVersionInfoEx;
  IsServer: Boolean;
begin
  ZeroMemory(@VerInfo, SizeOf(VerInfo));
  VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);
  if not u_dzOsUtils.GetVersionEx(VerInfo) then begin
    Result := _('Unknown OS version (GetVersionInfoEx failed).');
    Exit; //==>
  end;

  // todo: This might not be correct. MSDN tells us to use GetSystemMetrics for some
  //       server OSs, but since I don't have any way to test it, I might as well
  //       just not go through the trouble of implementing it.
  IsServer := (VerInfo.wProductType <> VER_NT_WORKSTATION);

  // we start with the most likely ones
  if (VerInfo.dwMajorVersion = 10) and (VerInfo.dwMinorVersion = 0) then begin
    // Windows 10 or Windows Server 2016
    // But this only works if the manifest tells Windows not to lie.
    // Since we don't know the version number ("Windows 10" is the product, it's not Windows
    // version 10), we need to check further anyway.
    Result := OsVersionFromKernel32Version(IsServer);
  end else if VerInfo.dwMajorVersion = 6 then begin
    if VerInfo.dwMinorVersion = 2 then begin
      // Windows 8
      // In theory at least, but Windows might be lying so we need to check further
      Result := OsVersionFromKernel32Version(IsServer);
    end else if VerInfo.dwMinorVersion = 3 then begin
      // Windows 8.1
      // But this only works if the manifest tells Windows not to lie
      // otherwise all versions newer than Windows 8 will be reported as Windows 8
      if IsServer then
        Result := 'Windows Server 2012 R2'
      else
        Result := 'Windows 8.1';
    end else if VerInfo.dwMinorVersion = 1 then begin
      // Windows 7
      if IsServer then
        Result := 'Windows Server 2008 R2'
      else
        Result := 'Windows 7';
    end else if VerInfo.dwMinorVersion = 0 then begin
      // Windows Vista
      if IsServer then
        Result := 'Windows Server 2008'
      else
        Result := 'Windows Vista';
    end else
      Result := Format(_('Unknown Windows version %d.%d build %d'),
        [VerInfo.dwMajorVersion, VerInfo.dwMinorVersion, VerInfo.dwBuildNumber]);
  end else if VerInfo.dwMajorVersion = 5 then begin
    if VerInfo.dwMinorVersion = 2 then begin
      Result := 'Windows XP Professional x64 Edition'
      // or Windows Server 2003, Windows Home Server or Windows Server 2003 R2
    end else if VerInfo.dwMinorVersion = 1 then begin
      Result := 'Windows XP'
    end else if VerInfo.dwMinorVersion = 0 then begin
      Result := 'Windows 2000'
    end else
      Result := Format(_('Unknown Windows version %d.%d build %d'),
        [VerInfo.dwMajorVersion, VerInfo.dwMinorVersion, VerInfo.dwBuildNumber]);
  end else
    Result := Format(_('Unknown Windows version %d.%d build %d'),
      [VerInfo.dwMajorVersion, VerInfo.dwMinorVersion, VerInfo.dwBuildNumber]);
end;

function TryGetWindowsVersionFromRegistry(out _Values: TWinCurrentRec): Boolean;
{$IF not Declared(KEY_WOW64_64KEY)}
const
  KEY_WOW64_64KEY = $0100;
{$IFEND}
var
  Reg: TRegistry;
begin
  // We need to read the real registry, not the 32 bit view, because some of the entries
  // don't exist there.
  Reg := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Result := Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows NT\CurrentVersion');
    if not Result then
      Exit; //==>

    _Values.BuildLab := TRegistry_ReadString(Reg, 'BuildLab');
    _Values.BuildLabEx := TRegistry_ReadString(Reg, 'BuildLabEx');
    _Values.CSDBuildNumber := TRegistry_ReadString(Reg, 'CSDBuildNumber');
    _Values.CSDVersion := TRegistry_ReadString(Reg, 'CSDVersion');
    _Values.CurrentBuildNumber := TRegistry_ReadString(Reg, 'CurrentBuildNumber');
    _Values.CurrentVersion := TRegistry_ReadString(Reg, 'CurrentVersion');
    _Values.EditionId := TRegistry_ReadString(Reg, 'EditionId');
    _Values.ProductName := TRegistry_ReadString(Reg, 'ProductName');
    _Values.ReleaseId := TRegistry_ReadString(Reg, 'ReleaseId');
    _Values.UBR := TRegistry_ReadInteger(Reg, 'UBR');
  finally
    FreeAndNil(Reg);
  end;
end;

function GetWindowsVersionStringFromRegistry: string;
var
  Values: TWinCurrentRec;
begin
  if not TryGetWindowsVersionFromRegistry(Values) then begin
    Result := _('Unknown Windows 10 version (registry key does not exist)');
    Exit; //==>
  end;

  if Values.ProductName = '' then begin
    Result := _('Unknown Windows version (ProductName entry cannot be read)');
    Exit; //==>
  end;
  Result := Values.ProductName;
  if Values.ReleaseId <> '' then
    Result := Result + ' Version ' + Values.ReleaseId;
  if Values.CurrentBuildNumber <> '' then begin
    Result := Result + ' (OS Build ' + Values.CurrentBuildNumber;
    if Values.UBR <> 0 then
      Result := Result + '.' + IntToStr(Values.UBR);
    Result := Result + ')';
  end;
  if Values.CSDVersion <> '' then
    Result := Result + ' ' + Values.CSDVersion;
end;

{$IF not declared(PSystemLogicalProcessorInformation)}
{$ALIGN ON}
{$MINENUMSIZE 4}

{$IF not declared(ULONG_PTR)}
type
  ULONG_PTR = LongWord;
{$IFEND}

{$IF not declared(ULONGLONG)}
  ULONGLONG = UInt64;
{$IFEND}

type
  _PROCESSOR_CACHE_TYPE = (CacheUnified { = 0}, CacheInstruction { = 1}, CacheData { = 2}, CacheTrace { = 3});
  PROCESSOR_CACHE_TYPE = _PROCESSOR_CACHE_TYPE;
  TProcessorCacheType = PROCESSOR_CACHE_TYPE;
type
  TCacheDescriptor = record
    Level: Byte;
    Associativity: Byte;
    LineSize: Word;
    Size: DWORD;
    _Type: PROCESSOR_CACHE_TYPE;
  end;

type
  TLogicalProcessorRelationship = (RelationProcessorCore { = 0},
    RelationNumaNode { = 1},
    RelationCache { = 2},
    RelationProcessorPackage { = 3},
    RelationGroup { = 4}, RelationAll = $FFFF);

type
  TSystemLogicalProcessorInformation = record
    ProcessorMask: ULONG_PTR;
    Relationship: TLogicalProcessorRelationship;
    case Integer of
      0: (Flags: Byte); // ProcessorCore
      1: (NodeNumber: DWORD); // NumaNode
      2: (Cache: TCacheDescriptor); //Cache
      3: (Reserved: array[0..1] of ULONGLONG);
  end;
  PSystemLogicalProcessorInformation = ^TSystemLogicalProcessorInformation;

function GetLogicalProcessorInformation(Buffer: PSystemLogicalProcessorInformation; var ReturnedLength: DWORD): BOOL; stdcall;
  external kernel32 Name 'GetLogicalProcessorInformation';
{$IFEND}

function GetCpuCacheLineSize: Integer;
var
  ProcInfo: PSystemLogicalProcessorInformation;
  CurInfo: PSystemLogicalProcessorInformation;
  Len: DWORD;
  Err: DWORD;
begin
  Result := 64;

  Len := 0;
  if not GetLogicalProcessorInformation(nil, Len) then begin
    Err := GetLastError;
    if Err = ERROR_INSUFFICIENT_BUFFER then begin
      GetMem(ProcInfo, Len);
      try
        if GetLogicalProcessorInformation(ProcInfo, Len) then begin
          // it should not be possible that the second call still returns false, but ...
          CurInfo := ProcInfo;
          while Len > 0 do begin
            if (CurInfo.Relationship = RelationCache) and (CurInfo.Cache.Level = 1) then begin
              Result := CurInfo.Cache.LineSize;
              Exit;
            end;
            Inc(CurInfo);
            Dec(Len, SizeOf(CurInfo^));
          end;
        end;
      finally
        FreeMem(ProcInfo);
      end;
    end;
  end;
end;

{$IF Declared(CpuCount)}
function GetCpuLogicalProcessorCount: Integer;
begin
  Result := CPUCount;
end;

{$ELSE}
function GetCpuLogicalProcessorCount: Integer;
var
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  Result := SysInfo.dwNumberOfProcessors;
end;
{$IFEND}

end.
