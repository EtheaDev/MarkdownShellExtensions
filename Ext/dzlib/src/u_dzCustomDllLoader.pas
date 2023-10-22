{.GXFormatter.config=twm}
unit u_dzCustomDllLoader;

interface

uses
  SysUtils,
  u_dzTranslator,
  u_dzVersionInfo,
  u_dzTypes;

type
  ///<summary> parent exception class for all exceptions raised in u_dzDllLoader </summary>
  EdzDllLoader = class(EdzException);
  ///<summary> raised if LoadLibrary fails </summary>
  EDllLoadError = class(EdzDllLoader);
  ///<summary> raised if GetProcAddress fails </summary>
  ENoEntryPoint = class(EdzDllLoader);

type
  ///<summary> wrapper for the Load/FreeLibrary and GetProcAddress API calls </summary>
  TdzCustomDllLoader = class(TInterfacedObject)
  protected
    ///<summary> name of wrapped dll </summary>
    FDllName: string;
    procedure LoadDll; virtual; abstract;
    procedure UnloadDll; virtual; abstract;
  public
    ///<summary> tries to load the given dll and raises EDllLoadError if it fails
    ///          @param DllName is the name of the dll to load, can contain absolute path
    ///          @raises EDllLoadError on failure </summary>
    constructor Create(const _DllName: string); overload;
    ///<summary> Calls UnloadDll and free's the object</summary>
    destructor Destroy; override;
    ///<summary> calls GetProcAddress and raises ENoEntryPoint if it returns nil
    ///          @param EntryPoint is the name of the entry point to get
    ///          @param DefaultFunc is a function pointer to assign if the entry point cannot be found
    ///                             if it is nil, an ENoEntryPoint exception will be raise in that case.
    ///                             Note: This function pointer must match the calling convention of
    ///                             the entry point and unless the calling convention is cdecl
    ///                             it must also match number of parameters of the entry point.
    ///                             See also the NotSupportedN functions in this unit.
    ///          @returns a pointer to the entry pointer
    ///          @raises ENoEntryPoint on failure </summary>
    function GetProcAddressEx(const _EntryPoint: string; _DefaultFunc: Pointer = nil): Pointer; overload; virtual; abstract;
    ///<summary> calls GetProcAddress for MSC mangled entry points and raises ENoEntryPoint if it returns nil
    ///          @param EntryPoint is the name of the entry point to get
    ///          @param DWordParams is the number of DWord parameters of the entry point, used to
    ///                             generate the actual name of the entry point
    ///          @param DefaultFunc is a function pointer to assign if the entry point cannot be found
    ///                             if it is nil, an ENoEntryPoint exception will be raised in that case.
    ///                             Note: This function pointer must match the calling convention of
    ///                             the entry point and unless the calling convention is cdecl
    ///                             it must also match number of parameters of the entry point.
    ///                             See also the NotSupportedN functions in u_dzDllLoader.
    ///          @returns a pointer to the entry pointer
    ///          @raises ENoEntryPoint on failure </summary>
    function GetProcAddressEx(const _EntryPoint: string; _DWordParams: Integer; _DefaultFunc: Pointer = nil): Pointer; overload; virtual; abstract;
    ///<summary> returns the full path of the dll that has been loaded </summary>
    function DllFilename: string; virtual; abstract;
    ///<summary> Generates a TVersionInfo object on demand and returns it </summary>
    function DllVersion: IFileInfo; virtual; abstract;
    ///<summary> returns the dll name as passed to the constructor </summary>
    property DllName: string read FDllName;
  end;

///<summary> dummy implementation for unsupported functions with 1 long parameter </summary>
function NotSupported1(one: Integer): Integer; stdcall;

///<summary> dummy implementation for unsupported functions with 2 long parameters </summary>
function NotSupported2(one, two: Integer): Integer; stdcall;

///<summary> dummy implementation for unsupported functions with 3 long parameters </summary>
function NotSupported3(one, two, three: Integer): Integer; stdcall;

///<summary> dummy implementation for unsupported functions with 4 long parameters </summary>
function NotSupported4(one, two, three, four: Integer): Integer; stdcall;

///<summary> dummy implementation for unsupported functions with 5 long parameters </summary>
function NotSupported5(one, two, three, four, five: Integer): Integer; stdcall;

///<summary> dummy implementation for unsupported functions with 6 long parameters </summary>
function NotSupported6(one, two, three, four, five, six: Integer): Integer; stdcall;

///<summary> since for cdecl calling convention, the caller cleans up the stack, we only
///          need one dummy function </summary>
function CdeclNotSupported(): Integer; cdecl;

///<summary> since for cdecl calling convention, the caller cleans up the stack, we only
///          need one dummy function </summary>
function CdeclReturn0: Integer; cdecl;

implementation

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TdzCustomDllLoader }

constructor TdzCustomDllLoader.Create(const _DllName: string);
begin
  inherited Create;
  FDllName := _DllName;
  LoadDll;
end;

destructor TdzCustomDllLoader.Destroy;
begin
  UnloadDll;
  inherited;
end;

// Dummy functions for assigning as default entry points

function NotSupported1(one: Integer): Integer; stdcall;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function NotSupported2(one, two: Integer): Integer; stdcall;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function NotSupported3(one, two, three: Integer): Integer; stdcall;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function NotSupported4(one, two, three, four: Integer): Integer; stdcall;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function NotSupported5(one, two, three, four, five: Integer): Integer; stdcall;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function NotSupported6(one, two, three, four, five, six: Integer): Integer; stdcall;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function CdeclNotSupported(): Integer; cdecl;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function CdeclReturn0: Integer; cdecl;
begin
  Result := 0;
end;

end.

