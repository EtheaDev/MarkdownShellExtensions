unit u_dzAutoCompletePath;

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
  TOnGetBasePathEvent = procedure(_Sender: TObject; out _Base: string) of object;

type
  ///<summary>
  /// Implementaition of IEnumString for directories </summary>
  TEnumStringPath = class(TEnumStringAbstract, IEnumString)
  private
    FOnGetBase: TOnGetBasePathEvent;
    FEnum: TSimpleDirEnumerator;
    FBase: string;
    FPrefix: string;
    procedure doOnGetBase(out _Base: string);
    function FindNextDir(out _DirName: WideString): Boolean;
  protected
    // IEnumString
    function Next(celt: LongInt; out elt;
      pceltFetched: PLongint): HResult; override;
    function Skip(celt: LongInt): HResult; override;
    function Reset: HResult; override;
  public
    constructor Create(_OnGetBase: TOnGetBasePathEvent);
    destructor Destroy; override;
  end;

///<summary>
/// Adds autocompletion to the edit by using the text as a path with directories separated by
/// semicolon. Autocompletion will always use the last entry in the path. </summary>
procedure TEdit_ActivateAutoCompletePath(_ed: TCustomEdit);

implementation

uses
  ComObj,
  Controls,
  u_dzVclUtils,
  u_dzStringUtils;

{ TEnumStringPath }

constructor TEnumStringPath.Create(_OnGetBase: TOnGetBasePathEvent);
begin
  inherited Create;
  FOnGetBase := _OnGetBase;
end;

destructor TEnumStringPath.Destroy;
begin
  FreeAndNil(FEnum);
  inherited;
end;

procedure TEnumStringPath.doOnGetBase(out _Base: string);
begin
  if Assigned(FOnGetBase) then
    FOnGetBase(Self, _Base);
end;

function TEnumStringPath.FindNextDir(out _DirName: WideString): Boolean;
var
  fn: string;
begin
  Result := Assigned(FEnum) and FEnum.FindNext(fn, False);
  if Result then begin
    _DirName := FPrefix + fn;
  end;
end;

function TEnumStringPath.Next(celt: Integer; out elt; pceltFetched: PLongint): HResult;
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

function TEnumStringPath.Reset: HResult;
var
  Mask: string;
  p: Integer;
begin
  doOnGetBase(FBase);
  FreeAndNil(FEnum);
  p := LastDelimiter(';', FBase);
  if p > 0 then begin
    FPrefix := Copy(FBase, 1, p - 1);
    Mask := Copy(FBase, p + 1);
    p := LastDelimiter('\', FBase);
    if (p > 0) and (p > Length(FPrefix)) then
      FPrefix := Copy(FBase, 1, p);
  end else begin
    Mask := FBase;
    FPrefix := FBase;
    p := LastDelimiter('\', FBase);
    if p > 0 then
      FPrefix := Copy(FBase, 1, p);
  end;

  FEnum := TSimpleDirEnumerator.CreateForDirsOnly(Mask + '*');
  FEnum.MustHaveAttr := [dfaDirectory];
  Result := S_OK;
end;

function TEnumStringPath.Skip(celt: Integer): HResult;
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
  TAutoCompleteHelperPath = class(TAutoCompleteHelper)
  private
    procedure HandleOnGetBase(_Sender: TObject; out _Base: string);
  protected
    function CreateEnumStringInt: IEnumString; override;
  end;

procedure TEdit_ActivateAutoCompletePath(_ed: TCustomEdit);
begin
  TAutoCompleteHelperPath.Create(_ed);
end;

{ TAutoCompleteHelperPath }

function TAutoCompleteHelperPath.CreateEnumStringInt: IEnumString;
begin
  Result := TEnumStringPath.Create(HandleOnGetBase);
end;

procedure TAutoCompleteHelperPath.HandleOnGetBase(_Sender: TObject; out _Base: string);
begin
  _Base := (FCtrl as TCustomEdit).Text;
end;

end.
