unit u_dzConfigUtils;

interface

uses
  SysUtils;

type
  TdzConfig = class
  private
    FCompany: string;
    FProductName: string;
    FVersion: string;
    FExeName: string;
    FExtension: string;
    FForcedUserCfg: string;
    FForcedCommonCfg: string;
    FHModule: Cardinal;
    function BuildCfgFilename(const _Base: string): string;
    procedure GetCompanyAndProductFromPath(_ModuleName: string);
    function ExpandVariables(const _s: string): string;
    procedure SetForcedCommonCfg(const _Value: string);
    procedure SetForcedUserCfg(const _Value: string);
  public
    ///<summary> @param HModule is the module handle of the file to use for version information
    ///                         and for the default RedirIni filename, defaults to 0 (the
    ///                         main executable), set to HInstance if you want to use a dll rather
    ///                         than the executable that loaded the dll.
    ///          @param RedirIni is the name of an .ini file to read ForcedUserCfg and ForcedCommonCfg
    ///                          from. Defaults to ChangeFileExt(GetModuleName(HModule), '.ini').
    ///          @param RedirSection is the name of the section in the RedirIni file to read.
    ///                              defaults to 'global', set to '' if you do not want redirection. </summary>
    constructor Create(_HModule: Cardinal = 0; _RedirIni: string = ''; _RedirSection: string = 'global');
    ///<summary> Determine the name of the User-Config file
    ///          @param Roaming determines whether to use 'Application Data' (true) or
    ///                         'Local Application Data' (false), defaults to true.
    ///          @returns the User-Ini file built from
    ///          <ApplicationData>\<Company>\<ProductName>\<Version>\<ExeName>.<Extension>
    ///          where <ApplicationData> is either the value for CSIDL_APPDATA
    ///          (e.g. C:\Documents and Settings\<user>\Application Data) or CSIDL_LOCAL_APPDATA
    ///          (e.g. C:\Documents and Settings\<user>\Local Settings\Application Data)
    ///          depending on the value passed for the Roaming parameter
    ///</summary>
    function GetUserCfgFile(_Roaming: boolean = true): string;
    ///<summary> Determine the name of the Common-Ini file
    ///          @returns the User-Ini file built from
    ///          <ApplicationData>\<Company>\<ProductName>\<Version>\<ExeName>.<Extension>
    ///          where <ApplicationData> is the value for CSIDL_COMMON_APPDATA
    ///          (e.g. C:\Documents and Settings\All Users\Application Data) or CSIDL_LOCAL_APPDATA
    ///          Note that this file usually is read only to non admin users.
    ///</summary>
    function GetCommonCfgFile: string;
    ///<summary> Company is taken from the version information, if it exists.
    ///          Otherwise we try to guess it from the executable's directory, assuming
    ///          <ProgramFiles>\<Company>\<ProductName>.
    ///          If that fails (e.g. there is only one level below <ProgramFiles>) it will
    ///          be empty. </summary>
    property Company: string read FCompany write FCompany;
    ///<summary> ProductName is taken from the version information, if it exists.
    ///          Otherwise we try to guess them from the executable's directory, assuming
    ///          <ProgramFiles>\<Company>\<ProductName>.
    ///          If that fails (e.g. there is only one level below <ProgramFiles>) it will
    ///          be set to the parent directory of the executable. </summary>
    property ProductName: string read FProductName write FProductName;
    ///<summary> Version is generated from <FileVersionMajor> and <FileVersionMinor) taken from
    ///          the version information, if it exists. <FileVersionMinor> will always be
    ///          two digits long, prepended with a zero if necessary
    ///          (e.g. Major = 2 and Minor = 1 will become '2.01')
    ///          If no version information exists it will be empty. </summary>
    property Version: string read FVersion write FVersion;
    ///<summary> ExeName is the InternalName taken from the version information, failing that
    ///          the filename of the executable (without path and extension) is used. </summary>
    property ExeName: string read FExeName write FExeName;
    ///<summary> Extension defaults to '.ini'
    property Extension: string read FExtension write FExtension;
    ///<summary> If set, this will always be returned by GetUserCfgFile,
    ///          Initialized from <ExeNameBase>.ini [global]usercfg
    ///          When set, variable expansion takes place. </summary>
    property ForcedUserCfg: string read FForcedUserCfg write SetForcedUserCfg;
    ///<summary> If set, this will always be returned by GetCommonCfgFile
    ///          When set, variable expansion takes place. </summary>
    property ForcedCommonCfg: string read FForcedCommonCfg write SetForcedCommonCfg;

  end;

implementation

uses
  StrUtils,
  u_dzOsUtils,
  u_dzFileUtils,
  u_dzClassutils,
  u_dzShellApiUtils,
  u_dzVersionInfo;

{ TdzConfig }

constructor TdzConfig.Create(_HModule: Cardinal = 0; _RedirIni: string = ''; _RedirSection: string = 'global');
var
  VerInfo: IFileInfo;
  ModuleName: string;
  s: string;
begin
  inherited Create;
  FHModule := _HModule;
  ModuleName := GetModuleFilename(FHModule);

  if _RedirIni = '' then
    _RedirIni := ChangeFileExt(ModuleName, '.ini');
  if _RedirSection <> '' then begin
    if TFileSystem.FileExists(_RedirIni) then begin
      s := TIniFile_ReadString(_RedirIni, _RedirSection, 'usercfg', '');
      SetForcedUserCfg(s);
      s := TIniFile_ReadString(_RedirIni, _RedirSection, 'commoncfg', '');
      SetForcedCommonCfg(s);
    end;
  end;

  VerInfo := TFileInfo.Create(ModuleName);
  VerInfo.AllowExceptions := false;
  FCompany := VerInfo.CompanyName;
  FProductName := VerInfo.ProductName;
  if VerInfo.FileVersionRec.IsValid then begin
    FVersion := Format('%d.%.2d', [VerInfo.FileVersionRec.Major, VerInfo.FileVersionRec.Minor]);
  end;
  FExeName := VerInfo.InternalName;
  FExtension := '.ini';

  if (FCompany = '') and (FProductName = '') then
    GetCompanyAndProductFromPath(ModuleName);

  if FExeName = '' then
    FExeName := ChangeFileExt(ExtractFileName(ModuleName), '');
end;

{$IF not Declared(ReplaceText)}
function ReplaceText(const AText, AFromText, AToText: string): string;
begin
  Result := AnsiReplaceText(AText, AFromText, AToText)
end;
{$IFEND}

function TdzConfig.ExpandVariables(const _s: string): string;
begin
  // the following variables are supported:
  // * AppData
  // * LocalAppData
  // * CommonAppData
  // * ExeName
  // * ExeDir
  // they must be enclosed in curly braces "{" and "}"
  Result := _s;
  if Pos('{', Result) <> 0 then
    Result := ReplaceText(Result, '{AppData}', TWindowsShell.GetAppDataDir);
  if Pos('{', Result) <> 0 then
    Result := ReplaceText(Result, '{LocalAppData}', TWindowsShell.GetLocalAppDataDir);
  if Pos('{', Result) <> 0 then
    Result := ReplaceText(Result, '{CommonAppData}', TWindowsShell.GetCommonAppDataDir);
  if Pos('{', Result) <> 0 then
    Result := ReplaceText(Result, '{ExeName}', ExtractFileName(GetModuleFilename(FHModule)));
  if Pos('{', Result) <> 0 then
    Result := ReplaceText(Result, '{ExeDir}', ExtractFileDir(GetModuleFilename(FHModule)));
end;

procedure TdzConfig.GetCompanyAndProductFromPath(_ModuleName: string);
var
  Programfiles: string;
  Path: string;
begin
  FCompany := '';
  FProductName := '';
  Path := etpd(ExtractFileDir(_ModuleName));
  Programfiles := etpd(TWindowsShell.GetProgramFilesDir);
  if SameText(Path, Programfiles) or (Length(Path) = 2) and (Path[2] = ':') then
    exit;
  FProductName := ExtractFileName(Path);
  Path := etpd(ExtractFileDir(Path));
  if SameText(Path, Programfiles) or (Length(Path) = 2) and (Path[2] = ':') then
    exit;
  FCompany := ExtractFileName(Path);
end;

function TdzConfig.BuildCfgFilename(const _Base: string): string;

  procedure AppendIfGiven(const _Value: string);
  begin
    if _Value <> '' then
      Result := Result + itpd(_Value);
  end;

begin
  Result := '';
  AppendIfGiven(_Base);
  AppendIfGiven(FCompany);
  AppendIfGiven(FProductName);
  AppendIfGiven(FVersion);
  Result := Result + ChangeFileExt(FExeName, FExtension);
end;

function TdzConfig.GetCommonCfgFile: string;
begin
  if FForcedCommonCfg <> '' then
    Result := FForcedCommonCfg
  else
    Result := BuildCfgFilename(TWindowsShell.GetCommonAppDataDir);
end;

function TdzConfig.GetUserCfgFile(_Roaming: boolean = true): string;
var
  Base: string;
begin
  if FForcedUserCfg <> '' then
    Result := FForcedUserCfg
  else begin
    if _Roaming then
      Base := TWindowsShell.GetAppDataDir
    else
      Base := TWindowsShell.GetLocalAppDataDir;
    Result := BuildCfgFilename(Base);
  end;
end;

procedure TdzConfig.SetForcedCommonCfg(const _Value: string);
begin
  FForcedCommonCfg := ExpandVariables(_Value);
end;

procedure TdzConfig.SetForcedUserCfg(const _Value: string);
begin
  FForcedUserCfg := ExpandVariables(_Value);
end;

end.

