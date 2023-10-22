unit u_dzDllApi;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  u_dzCustomDllLoader;

type
  TDzDllApi = class
  private
    function TryLoadDllFromResource(const _DllName: string; var _DllLoader: TdzCustomDllLoader): boolean;
    ///<summary> Checks the environment variable 'dzDllApiForceFile'. If it exists, it might contain
    ///          a semicolon separated list of dll names (without extensions and paths) that
    ///          are not to be taken from the resource.
    ///          @returns true, if the DllName was found in the list. </summary>
    function CheckEnvironment(const _DllName: string): Boolean;
  protected
    FDllLoader: TdzCustomDllLoader;
    ///<summary> Called by the constructor after the dll has been loaded. Override
    ///          it to initialize any entry points </summary>
    procedure InitEntryPoints; virtual;
  public
    constructor Create(_DllLoader: TdzCustomDllLoader); overload;
    constructor Create(const _DllName: string; _TryResFirst: boolean = true); overload;
    destructor Destroy; override;
  end;

implementation

uses
  StrUtils,
  u_dzResourceDllLoader,
  u_dzDllLoader,
  u_dzStringUtils;

{ TDzDllApi }

constructor TDzDllApi.Create(_DllLoader: TdzCustomDllLoader);
begin
  inherited Create;
  FDllLoader := _DllLoader;
  InitEntryPoints;
end;

constructor TDzDllApi.Create(const _DllName: string; _TryResFirst: boolean);
var
  Loader: TdzCustomDllLoader;
begin
  if _TryResFirst and CheckEnvironment(_DllName) then
    _TryResFirst := false;
  if not _TryResFirst or not TryLoadDllFromResource(_DllName, Loader) then
    Loader := TdzDllLoader.Create(_DllName);
  Create(Loader);
end;

destructor TDzDllApi.Destroy;
begin
  FreeAndNil(FDllLoader);
  inherited;
end;

procedure TDzDllApi.InitEntryPoints;
begin
  // override in descendants
end;

function TDzDllApi.CheckEnvironment(const _DllName: string): Boolean;
var
  p: Integer;
  s: string;
  Basename: string;
begin
  Result := false;
  s := LowerCase(GetEnvironmentVariable('dzDllApiForceFile'));
  Basename := LowerCase(ChangeFileExt(ExtractFileName(_DllName), ''));
  p := Pos(Basename, s);
  while p > 0 do begin
    if (p = 1) or (s[p - 1] = ';') then
      if (p + Length(Basename) > Length(s)) or (s[p + Length(Basename)] = ';') then begin
        Result := true;
        exit;
      end;
    p := PosEx(Basename, s, p + 1);
  end;
end;

function TDzDllApi.TryLoadDllFromResource(const _DllName: string; var _DllLoader: TdzCustomDllLoader): boolean;
begin
  Result := false;
  try
    _DllLoader := TdzResourceDllLoader.Create(_DllName);
    Result := true;
  except
    _DllLoader := nil;
  end;
end;

end.

