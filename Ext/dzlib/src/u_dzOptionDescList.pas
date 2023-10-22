unit u_dzOptionDescList;

interface

uses
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzQuicksort;

type
  EOptionDesc = class(Exception);
  EOptionName = class(EOptionDesc);

type
  TOptionDesc = class
  private
    FIsHidden: boolean;
    function CreateDescription(const _OptionName: string): string;
    procedure AssertValidOptionName(const _Name: ansistring);
  protected
    FPrimaryName: string;
    FDescription: string;
    FHasValue: boolean;
    FNames: TStringList;
  public
    constructor Create(const _Names: array of string; const _Description: string;
      _HasValue: boolean = false; _IsHidden: boolean = false);
    destructor Destroy; override;
    function GetDescription(_Indent: integer): string;
    property HasValue: boolean read FHasValue;
    property PrimaryName: string read FPrimaryName;
    property isHidden: boolean read FIsHidden write FIsHidden;
  end;

{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TOptionDesc;
  _KEY_TYPE_ = string;
{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

type
  ///<summary> List for storing TOptionDesc items sorted by String </summary>
  TOptionDescList = class(_DZ_SORTED_OBJECT_LIST_TEMPLATE_)
  protected
    ///<summary> return the key of an item for comparison </summary>
    function KeyOf(const _Item: TOptionDesc): string; override;
    ///<summary> compare the keys of two items, must return a value
    ///          < 0 if Key1 < Key2, = 0 if Key1 = Key2 and > 0 if Key1 > Key2 </summary>
    function Compare(const _Key1, _Key2: string): integer; override;
  public
    function NonHiddenCount: integer;
  end;

implementation

uses
  StrUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

function TOptionDescList.KeyOf(const _Item: TOptionDesc): string;
begin
  Result := _Item.PrimaryName;
end;

function TOptionDescList.NonHiddenCount: integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to Count - 1 do
    if not Items[i].isHidden then
      Inc(Result);
end;

function TOptionDescList.Compare(const _Key1, _Key2: string): integer;
begin
  Result := CompareText(_Key1, _Key2);
end;

constructor TOptionDesc.Create(const _Names: array of string; const _Description: string;
  _HasValue: boolean = false; _IsHidden: boolean = false);
var
  i: integer;
  s: string;
begin
  inherited Create;
  FDescription := _Description;
  FHasValue := _HasValue;
  FIsHidden := _IsHidden;
  Assert(Length(_Names) > 0);
  FPrimaryName := _Names[0];
  AssertValidOptionName(AnsiString(FPrimaryName));
  FNames := TStringList.Create;
  for i := 0 to high(_Names) do begin
    s := _Names[i];
    AssertValidOptionName(AnsiString(s));
    fNames.Add(s);
  end;
end;

destructor TOptionDesc.Destroy;
begin
  FreeAndNil(FNames);
  inherited;
end;

procedure TOptionDesc.AssertValidOptionName(const _Name: ansistring);
var
  i: integer;
begin
  if _Name = '' then
    raise EOptionName.Create(_('Option name cannot be empty.'));
  { TODO -otwm : Maybe '$', '#' and some other special chars should be allowed }
  if not (_Name[1] in ['a'..'z', 'A'..'Z', '0'..'9', '?']) then
    raise EOptionName.Create(_('Option name must start with an alphanumeric character.'));
  for i := 2 to Length(_Name) do
    if not (_Name[i] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) then
      raise EOptionName.CreateFmt(_('Option name contains invalid character "%s" at position %d.'), [_Name[i], Ord(_Name[i])]);
end;

function TOptionDesc.CreateDescription(const _OptionName: string): string;
begin
  case Length(_OptionName) of
    0: Result := ''; // should never happen
    1: begin
        Result := '-' + _OptionName;
        if FHasValue then
          Result := Result + ' ' + _('value');
      end;
  else begin
      Result := '--' + _OptionName;
      if FHasValue then
        Result := Result + '=' + _('value');
    end
  end;
end;

function TOptionDesc.GetDescription(_Indent: integer): string;
var
  i: integer;
  Len: integer;
  s: string;
begin
  Len := 0;
  Result := '';
  for i := 0 to fNames.Count - 1 do begin
    s := CreateDescription(FNames[i]);
    Len := Length(s);
    if Result <> '' then
      Result := Result + #13#10;
    Result := Result + s;
  end;
  Result := Result + StringOfChar(' ', _Indent - Len - 3) + ' : ' + fDescription;
end;

end.

