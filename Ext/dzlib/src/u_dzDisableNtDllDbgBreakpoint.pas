unit u_dzDisableNtDllDbgBreakpoint;

interface

implementation

uses
  Windows,
  SysUtils;

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

// mostly taken from http://codeverge.com/embarcadero.delphi.ide/stack-overflow-from-ide-only/1047900
procedure PatchINT3;
var
  NOP: Byte;
  BytesWritten: DWORD;
  NtDll: THandle;
  P: Pointer;
  VerInfo: TOsVersionInfoEx;
begin
  ZeroMemory(@VerInfo, SizeOf(VerInfo));
  VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);
  if not GetVersionEx(VerInfo) then begin
    // we don't know the OS version
    Exit; //==>
  end;

  if VerInfo.dwMajorVersion <> 5 then begin
    // it's neither Windows XP nor 2000 -> do nothing
    Exit; //==>
  end;

  NtDll := GetModuleHandle('NTDLL.DLL');
  if NtDll = 0 then
    Exit;
  P := GetProcAddress(NtDll, 'DbgBreakPoint');
  if P = nil then
    Exit;
  try
    if Byte(P^) <> $CC then
      Exit;
    NOP := $90;
    if WriteProcessMemory(GetCurrentProcess, P, @NOP, 1, BytesWritten) and
      (BytesWritten = 1) then
      FlushInstructionCache(GetCurrentProcess, P, 1);
  except
    on EAccessViolation do
      ;
  else
    raise;
  end;
end;

initialization
  PatchINT3;
end.

