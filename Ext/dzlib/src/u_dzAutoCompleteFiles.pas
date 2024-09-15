unit u_dzAutoCompleteFiles;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  StdCtrls,
  ActiveX,
  u_dzfileutils,
  i_dzAutoComplete;

type
  TOnGetBaseFileEvent = procedure(_Sender: TObject; out _Base: string) of object;
  TOnFilterFileCallback = procedure(_Sender: TObject; const _Filename: string; var _Accept: Boolean) of object;

type
  ///<summary>
  /// Implementaition of IEnumString for files </summary>
  TEnumStringFiles = class(TEnumStringAbstract, IEnumString)
  private
    FOnGetBase: TOnGetBaseFileEvent;
    FDirEnum: TSimpleDirEnumerator;
    FFileEnum: TSimpleDirEnumerator;
    FBase: string;
    FFilter: string;
    FOnFilterFileCallback: TOnFilterFileCallback;
    procedure doOnGetBase(out _Base: string);
    function FilterFile(const _Filename: string): Boolean;
    function FindNextDirOrFile(out _DirOrFilename: WideString): Boolean;
  protected
    // IEnumString
    function Next(celt: LongInt; out elt; pceltFetched: PLongint): HResult; override;
    function Skip(celt: LongInt): HResult; override;
    function Reset: HResult; override;
  public
    constructor Create(_OnGetBase: TOnGetBaseFileEvent; const _Filter: string;
      _OnFilterFile: TOnFilterFileCallback);
    destructor Destroy; override;
  end;

///<summary>
/// @param Filter is a filter string for filenames passed on to FindFirst </summary>
procedure TEdit_ActivateAutoCompleteFiles(_ed: TCustomEdit; const _Filter: string); overload;
///<summary>
/// @param OnFilterFile is a callback method which is called for each file found. </summary>
procedure TEdit_ActivateAutoCompleteFiles(_ed: TCustomEdit; _OnFilterFile: TOnFilterFileCallback); overload;
///<summary>
/// @param Masks is an open array of strings containing masks passed on to Masks.MatchesMask to filter the files. </summary>
procedure TEdit_ActivateAutoCompleteFiles(_ed: TCustomEdit; const _Masks: array of string); overload;

implementation

uses
  Masks,
  ComObj;

{ TEnumStringFiles }

constructor TEnumStringFiles.Create(_OnGetBase: TOnGetBaseFileEvent; const _Filter: string;
  _OnFilterFile: TOnFilterFileCallback);
begin
  inherited Create;
  FFilter := _Filter;
  if FFilter = '' then
    FFilter := '*';
  FOnGetBase := _OnGetBase;
  FOnFilterFileCallback := _OnFilterFile;
end;

destructor TEnumStringFiles.Destroy;
begin
  FreeAndNil(FDirEnum);
  FreeAndNil(FFileEnum);
  inherited;
end;

function TEnumStringFiles.FilterFile(const _Filename: string): Boolean;
begin
  Result := True;
  if Assigned(FOnFilterFileCallback) then
    FOnFilterFileCallback(Self, _Filename, Result);
end;

procedure TEnumStringFiles.doOnGetBase(out _Base: string);
begin
  if Assigned(FOnGetBase) then
    FOnGetBase(Self, _Base);
end;

function TEnumStringFiles.FindNextDirOrFile(out _DirOrFilename: WideString): Boolean;
var
  fn: string;
begin
  if Assigned(FDirEnum) then begin
    Result := FDirEnum.FindNext(fn, True);
    if Result then begin
      _DirOrFilename := fn;
      Exit; //==>
    end;
  end;

  if Assigned(FFileEnum) then begin
    while FFileEnum.FindNext(fn, True) do begin
      if FilterFile(fn) then begin
        _DirOrFilename := fn;
        Result := True;
        Exit; //==>
      end;
    end;
  end;
  Result := False;
end;

function TEnumStringFiles.Next(celt: Integer; out elt; pceltFetched: PLongint): HResult;
type
  // avoid bug of Classes.pas declaration TPointerList = array of Pointer;
  // see https://blog.dummzeuch.de/2019/06/02/fix-for-access-violation-in-u_dzautocompletestrings/
  // and https://stackoverflow.com/a/50082133/49925
  TPointerList = array[0..0] of Pointer;
var
  i: Integer;
  wStr: WideString;
begin
  i := 0;
  while (i < celt) and FindNextDirOrFile(wStr) do begin
    TPointerList(elt)[i] := StringToLPOLESTR(wStr);
    Pointer(wStr) := nil;
    Inc(i);
  end;
  if pceltFetched <> nil then
    pceltFetched^ := i;
  if i = celt then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TEnumStringFiles.Reset: HResult;
begin
  doOnGetBase(FBase);
  FreeAndNil(FDirEnum);
  FreeAndNil(FFileEnum);
  FDirEnum := TSimpleDirEnumerator.CreateForDirsOnly(FBase + '*');
  FFileEnum := TSimpleDirEnumerator.Create(FBase + FFilter, [dfaArchive]);
  Result := S_OK;
end;

function TEnumStringFiles.Skip(celt: Integer): HResult;
var
  i: Integer;
  wStr: WideString;
begin
  i := 0;
  while FindNextDirOrFile(wStr) do begin
    Inc(i);
    if i < celt then begin
      Result := S_OK;
      Exit; //==>
    end;
  end;
  Result := S_FALSE;
end;

type
  TAutoCompleteHelperFiles = class(TAutoCompleteHelper)
  private
    FFilter: string;
    FOnFilterFile: TOnFilterFileCallback;
    FMasks: array of string;
    procedure HandleOnGetBase(_Sender: TObject; out _Base: string);
    procedure HandleFilterFile(_Sender: TObject; const _Filename: string; var _Accept: Boolean);
  protected
    function CreateEnumStringInt: IEnumString; override;
  public
    constructor Create(_ed: TCustomEdit; const _Filter: string); overload;
    constructor Create(_ed: TCustomEdit; const _OnFilterFile: TOnFilterFileCallback); overload;
    constructor Create(_ed: TCustomEdit; const _Masks: array of string); overload;
  end;

procedure TEdit_ActivateAutoCompleteFiles(_ed: TCustomEdit; const _Filter: string);
begin
  TAutoCompleteHelperFiles.Create(_ed, _Filter);
end;

procedure TEdit_ActivateAutoCompleteFiles(_ed: TCustomEdit; _OnFilterFile: TOnFilterFileCallback);
begin
  TAutoCompleteHelperFiles.Create(_ed, _OnFilterFile);
end;

procedure TEdit_ActivateAutoCompleteFiles(_ed: TCustomEdit; const _Masks: array of string); overload;
begin
  TAutoCompleteHelperFiles.Create(_ed, _Masks);
end;

{ TAutoCompleteHelperFiles }

constructor TAutoCompleteHelperFiles.Create(_ed: TCustomEdit; const _Filter: string);
begin
  FFilter := _Filter;
  inherited Create(_ed);
end;

constructor TAutoCompleteHelperFiles.Create(_ed: TCustomEdit;
  const _OnFilterFile: TOnFilterFileCallback);
begin
  FOnFilterFile := _OnFilterFile;
  inherited Create(_ed);
end;

constructor TAutoCompleteHelperFiles.Create(_ed: TCustomEdit; const _Masks: array of string);
var
  i: Integer;
begin
  SetLength(FMasks, Length(_Masks));
  for i := 0 to Length(_Masks) - 1 do
    FMasks[i] := _Masks[i];
  inherited Create(_ed);
end;

function TAutoCompleteHelperFiles.CreateEnumStringInt: IEnumString;
begin
  if Length(FMasks) > 0 then
    Result := TEnumStringFiles.Create(HandleOnGetBase, FFilter, HandleFilterFile)
  else
    Result := TEnumStringFiles.Create(HandleOnGetBase, FFilter, FOnFilterFile);
end;

procedure TAutoCompleteHelperFiles.HandleFilterFile(_Sender: TObject; const _Filename: string;
  var _Accept: Boolean);
var
  fno: string;
  i: Integer;
begin
  fno := ExtractFileName(_Filename);
  for i := 0 to Length(FMasks) - 1 do begin
    if MatchesMask(fno, FMasks[i]) then begin
      _Accept := True;
      Exit; //==>
    end;
  end;
  _Accept := False;
end;

procedure TAutoCompleteHelperFiles.HandleOnGetBase(_Sender: TObject; out _Base: string);
begin
  _Base := (FCtrl as TCustomEdit).Text;
end;

end.
