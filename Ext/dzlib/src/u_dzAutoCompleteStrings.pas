unit u_dzAutoCompleteStrings;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ActiveX,
  StdCtrls,
  i_dzAutoComplete;

type
  ///<summary>
  /// Implementaition of IEnumString for reading from a StringList
  /// based on Ken White's answer on Stack Overflow:
  /// https://stackoverflow.com/a/5465826/49925 </summary>
  TEnumStringStringList = class(TEnumStringAbstract, IEnumString)
  private
    FStrings: TStringList;
    FCurrIndex: Integer;
    procedure SetStrings(const _Value: TStringList);
  protected
    //IEnumString
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult; override;
    function Skip(celt: Longint): HResult; override;
    function Reset: HResult; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Strings: TStringList read FStrings write SetStrings;
  end;

///<summary>
/// Adds autocompletion to the edit based on the given string list </summary>
procedure TEdit_SetAutoCompleteStringList(_ed: TCustomEdit; _Strings: TStringList);

implementation

{ TEnumStringStringList }

constructor TEnumStringStringList.Create;
begin
  inherited Create;
  FStrings := TStringList.Create;
  FCurrIndex := 0;
end;

destructor TEnumStringStringList.Destroy;
begin
  FStrings.Free;
  inherited;
end;

function TEnumStringStringList.Next(celt: Integer; out elt;
  pceltFetched: PLongint): HResult;
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
  while (i < celt) and (FCurrIndex < FStrings.Count) do begin
    wStr := FStrings[FCurrIndex];

    TPointerList(elt)[I] := StringToLPOLESTR(wStr);
    Pointer(wStr) := nil;
    // I found a different implementation of the two preceding lines:
    //
    // TPointerList(elt)[i] := CoTaskMemAlloc(2 * (Length(wStr) + 1));
    // StringToWideChar(wStr, TPointerList(elt)[i], 2 * (Length(wStr) + 1));
    //
    // But I am not sure whether it is necessary because WideString is an OLEString
    // and the reference counter here is kept at 1 due to typecasting it as pointer.

    Inc(i);
    Inc(FCurrIndex);
  end;
  if pceltFetched <> nil then
    pceltFetched^ := i;
  if i = celt then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TEnumStringStringList.Reset: HResult;
begin
  FCurrIndex := 0;
  Result := S_OK;
end;

procedure TEnumStringStringList.SetStrings(const _Value: TStringList);
begin
  FStrings.Assign(_Value);
end;

function TEnumStringStringList.Skip(celt: Integer): HResult;
begin
  if (FCurrIndex + celt) <= FStrings.Count then begin
    Inc(FCurrIndex, celt);
    Result := S_OK;
  end else begin
    FCurrIndex := FStrings.Count;
    Result := S_FALSE;
  end;
end;

type
  TAutoCompleteHelperStringList = class(TAutoCompleteHelper)
  private
    FStrings: TStringList;
  protected
    function CreateEnumStringInt: IEnumString; override;
  public
    constructor Create(_ed: TCustomEdit; _Strings: TStringList);
    destructor Destroy; override;
  end;

procedure TEdit_SetAutoCompleteStringList(_ed: TCustomEdit; _Strings: TStringList);
begin
  TAutoCompleteHelperStringList.Create(_ed, _Strings);
end;

{ TAutoCompleteHelperStringList }

constructor TAutoCompleteHelperStringList.Create(_ed: TCustomEdit; _Strings: TStringList);
begin
  // must be initalized before calling inherited Create because it calls CreateNumStringInt
  // which needs FStrings
  FStrings := _Strings;
  inherited Create(_ed);
end;

function TAutoCompleteHelperStringList.CreateEnumStringInt: IEnumString;
var
  essl: TEnumStringStringList;
begin
  essl := TEnumStringStringList.Create;
  essl.Strings.Assign(FStrings);
  Result := essl;
end;

destructor TAutoCompleteHelperStringList.Destroy;
begin
  // do NOT Free FStrings
  inherited;
end;

end.

