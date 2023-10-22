{.GXFormatter.config=twm}
///<summary> declares TdzDllLoader class to make handling of DLLs easier </summary>
unit u_dzDllLoader;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils,
  u_dzTranslator,
  u_dzVersionInfo,
  u_dzCustomDllLoader;

type
  ///<summary> wrapper for the Load/FreeLibrary and GetProcAddress API calls </summary>
  TdzDllLoader = class(TdzCustomDllLoader)
  protected
    ///<summary> module handle of dll as returned by LoadLibrary </summary>
    FDllHandle: HMODULE;
    ///<summary> Version info of dll </summary>
    FDllVersion: IFileInfo;
    procedure LoadDll; override;
    procedure UnloadDll; override;
    ///<symmary> Called from within the constructor to initialize entry points, does nothing
    ///          here but should be overridden by descendants. </summary>
    procedure InitEntryPoints; virtual;
  public
    ///<summary>
    /// Calls GetProcAddress for the given EntryPoint, returns true, if it exists
    /// @param EntryPoint is the name of the entry point to search for
    /// @param Address returns the entry point address if it can be found
    /// @returns true, if the entry point was found, false if not </summary>
    function TryGetProcAddress(const _EntryPoint: string; out _Address: FARPROC): Boolean; overload;
    ///<summary> calls GetProcAddress and raises ENoEntryPoint if it returns nil
    ///          @param EntryPoint is the name of the entry point to get
    ///          @param DefaultFunc is a function pointer to assign if the entry point cannot be found
    ///                             if it is nil, an ENoEntryPoint exception will be raised in that case.
    ///                             Note: This function pointer must match the calling convention of
    ///                             the entry point and unless the calling convention is cdecl
    ///                             it must also match number of parameters of the entry point.
    ///                             See also the NotSupportedN functions in u_dzCustomDllLoader.
    ///          @returns a pointer to the entry pointer
    ///          @raises ENoEntryPoint on failure </summary>
    function GetProcAddressEx(const _EntryPoint: string; _DefaultFunc: FARPROC = nil): FARPROC; overload; override;
    ///<summary> calls GetProcAddress for MSC mangled entry points and raises ENoEntryPoint if it returns nil
    ///          @param EntryPoint is the name of the entry point to get
    ///          @param DWordParams is the number of DWord parameters of the entry point, used to
    ///                             generate the actual name of the entry point
    ///          @param DefaultFunc is a function pointer to assign if the entry point cannot be found
    ///                             if it is nil, an ENoEntryPoint exception will be raised in that case.
    ///                             Note: This function pointer must match the calling convention of
    ///                             the entry point and unless the calling convention is cdecl
    ///                             it must also match number of parameters of the entry point.
    ///                             See also the NotSupportedN functions in u_dzCustomDllLoader.
    ///          @returns a pointer to the entry pointer
    ///          @raises ENoEntryPoint on failure </summary>
    function GetProcAddressEx(const _EntryPoint: string; _DWordParams: Integer;
      _DefaultFunc: FARPROC = nil): FARPROC; overload; override;
  public
    ///<summary> calls LoadLibrary, returns true, if it succeeds and the handle in DllHandle
    ///                             returns false, if it fails and the error message </summary>
    class function TryLoadDll(const _DllName: string; out _DllHandle: HMODULE; out _Error: string): Boolean;
{$IFDEF SUPPORTS_STATIC}
     static;
{$ENDIF}
    ///<summary> calls GetProcAddress and raises ENoEntryPoint if it returns nil
    ///          @param DllHandle is the handle of the DLL from which the entry point should be determined
    ///          @param EntryPoint is the name of the entry point to get
    ///          @param DefaultFunc is a function pointer to assign if the entry point cannot be found
    ///                             if it is nil, an ENoEntryPoint exception will be raise in that case.
    ///                             Note: This function pointer must match the calling convention of
    ///                             the entry point and unless the calling convention is cdecl
    ///                             it must also match number of parameters of the entry point.
    ///                             See also the NotSupportedN functions in u_dzCustomDllLoader.
    ///          @returns a pointer to the entry pointer
    ///          @raises ENoEntryPoint on failure </summary>
    class function GetProcAddressEx(_DllHandle: HMODULE; const _DllName: string; const _EntryPoint: string;
      _DefaultFunc: FARPROC = nil): FARPROC; overload;
    ///<summary>
    /// Calls GetProcAddress for the given EntryPoint, returns true, if it exists
    /// @param DllHandle is the HModule of the DLL (returned by LoadLibrary or TryLoadDll)
    /// @param EntryPoint is the name of the entry point to search for
    /// @param Address returns the entry point address if it can be found
    /// @returns true, if the entry point was found, false if not </summary>
    class function TryGetProcAddress(_DllHandle: HMODULE; const _EntryPoint: string;
      out _Address: FARPROC): Boolean; overload;
    ///<summary> assumes that the dll has already been loaded and uses the given DllHandle,
    ///          NOTE: The destructor will call FreeLibrary anyway, so make sure you don't
    ///                store the dll handle anywhere else! </summary>
    constructor Create(const _DllName: string; _DllHandle: HMODULE); overload;
    constructor Create(const _DllName: string); overload;
    ///<summary> Generates a TVersionInfo object on demand and returns it </summary>
    function DllVersion: IFileInfo; override;
    ///<summary> returns the full path of the dll that has been loaded </summary>
    function DllFilename: string; override;
  end;

implementation

uses
  u_dzMiscUtils,
  u_dzOsUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TdzDllLoader }

class function TdzDllLoader.TryLoadDll(const _DllName: string;
  out _DllHandle: HMODULE; out _Error: string): Boolean;
var
  ErrorCode: DWORD;
begin
  _DllHandle := SafeLoadLibrary(_DllName);
  Result := (_DllHandle <> 0);
  if not Result then begin
    ErrorCode := GetLastError;
    _Error := SysErrorMessage(ErrorCode);
  end;
end;

constructor TdzDllLoader.Create(const _DllName: string; _DllHandle: HMODULE);
begin
  inherited Create;
  FDllName := _DllName;
  FDllHandle := _DllHandle;

  InitEntryPoints;
end;

constructor TdzDllLoader.Create(const _DllName: string);
begin
  inherited Create(_DllName);
  InitEntryPoints;
end;

function TdzDllLoader.DllFilename: string;
begin
  Result := GetModuleFilename(FDllHandle);
end;

function TdzDllLoader.DllVersion: IFileInfo;
begin
  if not Assigned(FDllVersion) then begin
    FDllVersion := TFileInfo.Create(DllFilename);
    FDllVersion.AllowExceptions := False;
  end;
  Result := FDllVersion;
end;

procedure TdzDllLoader.InitEntryPoints;
begin
  // does nothing
end;

procedure TdzDllLoader.LoadDll;
var
  Err: DWORD;
begin
  FDllHandle := SafeLoadLibrary(FDllName);
  if FDllHandle = 0 then begin
    Err := GetLastError;
    RaiseLastOsErrorEx(Err, Format(_('Could not load %s.'), [FDllName]) + ' ' + _('Error %1:s (%0:d)'));
  end;
end;

function TdzDllLoader.GetProcAddressEx(const _EntryPoint: string; _DWordParams: Integer;
  _DefaultFunc: FARPROC): FARPROC;
var
  EntryPoint: string;
begin
  EntryPoint := '_' + _EntryPoint + '@' + IntToStr(_DWordParams);
  Result := GetProcAddressEx(EntryPoint, _DefaultFunc);
end;

function TdzDllLoader.TryGetProcAddress(const _EntryPoint: string; out _Address: FARPROC): Boolean;
begin
  Result := TryGetProcAddress(FDllHandle, _EntryPoint, _Address);
end;

class function TdzDllLoader.TryGetProcAddress(_DllHandle: HMODULE; const _EntryPoint: string;
  out _Address: FARPROC): Boolean;
begin
  _Address := GetProcAddress(_DllHandle, PChar(_EntryPoint));
  Result := Assigned(_Address);
end;

class function TdzDllLoader.GetProcAddressEx(_DllHandle: HMODULE; const _DllName: string;
  const _EntryPoint: string; _DefaultFunc: FARPROC = nil): FARPROC;
var
  ErrCode: Integer;
begin
  if not TryGetProcAddress(_DllHandle, _EntryPoint, Result) then begin
    if Assigned(_DefaultFunc) then
      Result := _DefaultFunc
    else begin
      ErrCode := GetLastError;
      RaiseLastOsErrorEx(ErrCode,
        Format(_('Could not find entry point %s in %s'#13#10'ERROR= %%d, %%s'), [_EntryPoint, _DllName]));
    end;
  end;
end;

function TdzDllLoader.GetProcAddressEx(const _EntryPoint: string; _DefaultFunc: FARPROC = nil): FARPROC;
begin
  Result := GetProcAddressEx(FDllHandle, FDllName, _EntryPoint, _DefaultFunc);
end;

procedure TdzDllLoader.UnloadDll;
begin
  if FDllHandle <> 0 then
    FreeLibrary(FDllHandle);
  FDllHandle := 0;
end;

end.
