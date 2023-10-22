unit u_dzAutoCompleteDirs;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  StdCtrls,
  ActiveX,
  u_dzFileUtils,
  i_dzAutoComplete;

type
  TOnGetBaseDirEvent = procedure(_Sender: TObject; out _Base: string) of object;

type
  ///<summary>
  /// Implementaition of IEnumString for directories </summary>
  TEnumStringDirectories = class(TEnumStringAbstract, IEnumString)
  private
    FOnGetBase: TOnGetBaseDirEvent;
    FEnum: TSimpleDirEnumerator;
    FBase: string;
    procedure doOnGetBase(out _Base: string);
    function FindNextDir(out _DirName: WideString): Boolean;
  protected
    // IEnumString
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; override;
    function Skip(celt: Longint): HResult; override;
    function Reset: HResult; override;
  public
    constructor Create(_OnGetBase: TOnGetBaseDirEvent);
    destructor Destroy; override;
  end;

///<summary>
/// Adds autocompletion to the edit by using the text as a directory name.
/// Similar to u_dzVclUtils.TEdit_SetAutocomplete with _Source =acsFileSystem,
/// but will not autocomplete file names. </summary>
procedure TEdit_ActivateAutoCompleteDirectories(_ed: TCustomEdit);

implementation

uses
  ComObj,
  Controls,
  u_dzVclUtils;

{ TEnumStringDirectories }

constructor TEnumStringDirectories.Create(_OnGetBase: TOnGetBaseDirEvent);
begin
  inherited Create;
  FOnGetBase := _OnGetBase;
end;

destructor TEnumStringDirectories.Destroy;
begin
  FreeAndNil(FEnum);
  inherited;
end;

procedure TEnumStringDirectories.doOnGetBase(out _Base: string);
begin
  if Assigned(FOnGetBase) then
    FOnGetBase(Self, _Base);
end;

function TEnumStringDirectories.FindNextDir(out _DirName: WideString): Boolean;
var
  fn: string;
begin
  Result := Assigned(FEnum) and FEnum.FindNext(fn, True);
  if Result then
    _DirName := fn;
end;

function TEnumStringDirectories.Next(celt: Integer; out elt; pceltFetched: PLongint): HResult;
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
  while (i < celt) and FindNextDir(wStr) do begin
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

function TEnumStringDirectories.Reset: HResult;
begin
  doOnGetBase(FBase);
  FreeAndNil(FEnum);
  FEnum := TSimpleDirEnumerator.CreateForDirsOnly(FBase + '*');
  FEnum.MustHaveAttr := [dfaDirectory];
  Result := S_OK;
end;

function TEnumStringDirectories.Skip(celt: Integer): HResult;
var
  i: Integer;
  wStr: WideString;
begin
  i := 0;
  while FindNextDir(wStr) do begin
    Inc(i);
    if i < celt then begin
      Result := S_OK;
      Exit; //==>
    end;
  end;
  Result := S_FALSE;
end;

type
  TAutoCompleteHelperDirectories = class(TAutoCompleteHelper)
  private
    procedure HandleOnGetBase(_Sender: TObject; out _Base: string);
  protected
    function CreateEnumStringInt: IEnumString; override;
  end;

procedure TEdit_ActivateAutoCompleteDirectories(_ed: TCustomEdit);
begin
  TAutoCompleteHelperDirectories.Create(_ed);
end;

{ TAutoCompleteHelperDirectories }

function TAutoCompleteHelperDirectories.CreateEnumStringInt: IEnumString;
begin
  Result := TEnumStringDirectories.Create(HandleOnGetBase);
end;

procedure TAutoCompleteHelperDirectories.HandleOnGetBase(_Sender: TObject; out _Base: string);
begin
  _Base := (FCtrl as TCustomEdit).Text;
end;

end.
