unit u_dzJclUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes,
  Forms,
  JclFileUtils;

type
  TAbstractFileEnumHandler = class
  public
    procedure HandleFile(const _Directory: string; const _FileInfo: TSearchRec); virtual; abstract;
  end;

type
  TFileEnumCollector = class(TAbstractFileEnumHandler)
  private
    FList: TStrings;
  public
    constructor Create(_List: TStrings);
    procedure HandleFile(const _Directory: string; const _FileInfo: TSearchRec); override;
  end;

type
  TDirEnumHandler = class
  private
    FFileHandler: TAbstractFileEnumHandler;
    FOnHandleDirectory: TFileHandler;
    procedure doOnHandleDirectory(const _DirName: string);
  public
    constructor Create(_FileHandler: TAbstractFileEnumHandler);
    procedure HandleDirectory(const _DirName: string);
    property OnHandleDirectory: TFileHandler read FOnHandleDirectory write FOnHandleDirectory;
  end;

procedure dzEnumDirectories(const _Root: string; const _HandleDirectory: TFileHandler;
  const _IncludeHiddenDirectories: Boolean = false; const _SubDirectoriesMask: string = '');

///<summary> appends ' - [fileversion projectversion] to the form's caption
///          @param Caption is used instead of the original caption, if not empty </summary>
procedure TForm_AppendVersion(_frm: TForm; _Caption: string = ''); deprecated; // use u_dzVclUtils.TForm_AppendVersion

///<summary> gets the file version from the executables version information </summary>
function TApplication_GetFileVersion: string; deprecated; // use u_dzVclUtils.TApplication_GetFileVersion

///<summary> gets the product name from the executables version information </summary>
function TApplication_GetProductName: string;

///<summary> gets the product version from the executables version information </summary>
function TApplication_GetProductVersion: string;

implementation

uses
  JclStrings;

constructor TFileEnumCollector.Create(_List: TStrings);
begin
  inherited Create;
  FList := _List;
end;

procedure TFileEnumCollector.HandleFile(const _Directory: string; const _FileInfo: TSearchRec);
begin
  FList.Append(_Directory + _FileInfo.Name);
end;

{ TEnumDirRecursiveHandler }

constructor TDirEnumHandler.Create(_FileHandler: TAbstractFileEnumHandler);
begin
  inherited Create;
  FFileHandler := _FileHandler;
end;

procedure TDirEnumHandler.doOnHandleDirectory(const _DirName: string);
begin
  if Assigned(FOnHandleDirectory) then
    FOnHandleDirectory(_DirName);
end;

procedure TDirEnumHandler.HandleDirectory(const _DirName: string);
begin
  doOnHandleDirectory(_DirName);
  JclFileUtils.EnumFiles(_DirName + '*.*', FFileHandler.HandleFile);
end;

// copied from JclFileUtils and modified

procedure dzEnumDirectories(const _Root: string; const _HandleDirectory: TFileHandler;
  const _IncludeHiddenDirectories: Boolean = false; const _SubDirectoriesMask: string = '');

  function CanonicalizedSearchPath(const _Directory: string): string;
  begin
// twm: removed, because it strips one backslash:  Result := PathCanonicalize(_Directory);
    Result := _Directory;
  // avoid changing "X:" (current directory on drive X:) into "X:\" (root dir.)
    if Result[Length(Result)] <> ':' then
      Result := PathAddSeparator(Result);
  // strip leading "./" resp. ".\"
    if Pos('.' + DirDelimiter, Result) = 1 then
      Result := Copy(Result, 3, Length(Result) - 2);
  end;

var
  RootDir: string;
  Attr: Integer;

  procedure Process(const _Directory: string);
  var
    DirInfo: TSearchRec;
    SubDir: string;
    Found: Boolean;
  begin
    _HandleDirectory(_Directory);
    Found := SysUtils.FindFirst(_Directory + '*', Attr, DirInfo) = 0;
    try

      while Found do begin
        if (DirInfo.Name <> '.') and (DirInfo.Name <> '..') and
          (DirInfo.Attr and faDirectory <> 0) then begin
          SubDir := _Directory + DirInfo.Name + DirDelimiter;
          if (_SubDirectoriesMask = '') or StrMatches(_SubDirectoriesMask, SubDir, Length(RootDir)) then
            Process(SubDir);
        end;
        Found := FindNext(DirInfo) = 0;
      end;
    finally
      FindClose(DirInfo);
    end;
  end;

begin
  Assert(Assigned(_HandleDirectory));
  RootDir := CanonicalizedSearchPath(_Root);

  if _IncludeHiddenDirectories then
    Attr := faDirectory + faHidden // no effect on Linux
  else
    Attr := faDirectory;

  Process(RootDir);
end;

procedure TForm_AppendVersion(_frm: TForm; _Caption: string = '');
var
  VersionInfo: TJclFileVersionInfo;
  Version: string;
begin
  VersionInfo := TJclFileVersionInfo.Create(Application.ExeName);
  try
    Version := VersionInfo.FileVersion + ' ' + VersionInfo.ProductVersion;
  finally
    FreeAndNil(VersionInfo);
  end;

  if _Caption = '' then
    _Caption := _frm.Caption;

  _frm.Caption := _Caption + ' - [' + Version + ']';
end;

function TApplication_GetFileVersion: string;
var
  VersionInfo: TJclFileVersionInfo;
begin
  try
    VersionInfo := TJclFileVersionInfo.Create(Application.ExeName);
    try
      Result := VersionInfo.FileVersion;
    finally
      FreeAndNil(VersionInfo);
    end;
  except
    Result := '<no version>';
  end;
end;

function TApplication_GetProductName: string;
var
  VersionInfo: TJclFileVersionInfo;
begin
  try
    VersionInfo := TJclFileVersionInfo.Create(Application.ExeName);
    try
      Result := VersionInfo.ProductName;
    finally
      FreeAndNil(VersionInfo);
    end;
  except
    Result := '<no product>';
  end;
end;

function TApplication_GetProductVersion: string;
var
  VersionInfo: TJclFileVersionInfo;
begin
  try
    VersionInfo := TJclFileVersionInfo.Create(Application.ExeName);
    try
      Result := VersionInfo.ProductVersion;
    finally
      FreeAndNil(VersionInfo);
    end;
  except
    Result := '<no product version>';
  end;
end;

end.

