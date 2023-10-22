unit u_dzUnsortedNameValueList;

interface

uses
  SysUtils,
  Classes,
  u_dzNameValueList;

{$DEFINE __DZ_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TNameValue;
  _KEY_TYPE_ = string;
{$INCLUDE 't_dzObjectListTemplate.tpl'}

type
  /// <summary>
  /// unsorted list for storing TNameValue items </summary>
  TUnsortedNameValueList = class(_DZ_OBJECT_LIST_TEMPLATE_)
  private
    function GetByName(const _Name: string): string;
    procedure SetByName(const _Name, _Value: string);
  public
    function Add(const _Name, _Value: string): Integer; reintroduce; overload;
    function Add(const _NameValue: string): Integer; reintroduce; overload;
    function ByNameDef(const _Name, _Default: string): string;
    function Find(const _Name: string; out _Value: string): Boolean; overload;
    function Find(const _Name: string; out _Idx: Integer): Boolean; overload;
    procedure LoadFrom(_Strings: TStrings);
    procedure SaveTo(_Strings: TStrings);
    ///<summary> Checks for duplicate names and empty values </summary>
    function IsValid(var _Error: string): Boolean;
    property ByName[const _Name: string]: string read GetByName write SetByName;
  end;

implementation

uses
  u_dzTranslator,
  u_dzStringUtils;

function _(const _s: string): string; inline;
begin
  Result := dzlibGetText(_s);
end;

{$INCLUDE 't_dzObjectListTemplate.tpl'}

{ TUnsortedNameValueList }

function TUnsortedNameValueList.Add(const _Name, _Value: string): Integer;
begin
  Result := Add(TNameValue.Create(_Name, _Value));
end;

function TUnsortedNameValueList.Add(const _NameValue: string): Integer;
var
  p: Integer;
begin
  p := Pos('=', _NameValue);
  if p = 0 then
    raise Exception.CreateFmt(_('NameValue "%s" does not contain a "=".'), [_NameValue]);
  Result := Add(Copy(_NameValue, 1, p - 1), Copy(_NameValue, p + 1));
end;

function TUnsortedNameValueList.Find(const _Name: string; out _Idx: Integer): Boolean;
var
  i: Integer;
  nv: TNameValue;
begin
  for i := 0 to Count - 1 do begin
    nv := Items[i];
    if SameText(_Name, nv.Name) then begin
      _Idx := i;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TUnsortedNameValueList.Find(const _Name: string; out _Value: string): Boolean;
var
  Idx: Integer;
begin
  Result := Find(_Name, Idx);
  if Result then
    _Value := Items[Idx].Value;
end;

function TUnsortedNameValueList.GetByName(const _Name: string): string;
begin
  if not Find(_Name, Result) then
    raise ENameNotFound.CreateFmt(_('Entry "%s" not found.'), [_Name]);
end;

function TUnsortedNameValueList.IsValid(var _Error: string): Boolean;
var
  i: Integer;
  j: Integer;
  Item: TNameValue;
begin
  Result := False;
  for i := 0 to Count - 1 do begin
    Item := Items[i];
    for j := i + 1 to Count - 1 do
      if SameText(Item.Name, Items[j].Name) then begin
        _Error := Format(_('Duplicate value name "%s".'), [Item.Name]);
        Exit;
      end;
    if Item.Value = '' then begin
      _Error := Format(_('Empty value for "%s".'), [Item.Name]);
      Exit;
    end;
  end;
  Result := True;
end;

procedure TUnsortedNameValueList.LoadFrom(_Strings: TStrings);
var
  i: Integer;
  Name: string;
begin
  Clear;
  for i := 0 to _Strings.Count - 1 do begin
    Name := _Strings.Names[i];
    Add(Name, _Strings.Values[Name]);
  end;
end;

procedure TUnsortedNameValueList.SaveTo(_Strings: TStrings);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    _Strings.Values[Items[i].Name] := Items[i].Value;
end;

procedure TUnsortedNameValueList.SetByName(const _Name, _Value: string);
var
  Idx: Integer;
begin
  if Find(_Name, Idx) then
    Items[Idx].Value := _Value
  else
    Add(TNameValue.Create(_Name, _Value));
end;

function TUnsortedNameValueList.ByNameDef(const _Name, _Default: string): string;
begin
  if not Find(_Name, Result) then
    Result := _Default;
end;

end.

