unit u_dzNameBoolList;

interface

uses
  Classes,
  u_dzQuicksort;

type
  TNameBool = class
  private
    FName: string;
    FBool: boolean;
  public
    constructor Create(const _Name: string; _Bool: boolean);
    property Name: string read FName;
    property Bool: boolean read FBool write FBool;
  end;

{$DEFINE __DZ_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TNameBool;
{$INCLUDE 't_dzObjectListTemplate.tpl'}

type
  {: Sorted list for storing TNameBool items }
  TNameBoolList = class(_DZ_OBJECT_LIST_TEMPLATE_)
  private
    function FindName(const _Name: string; out _Idx: integer): boolean;
    function GetValues(const _Name: string): boolean;
    procedure SetValues(const _Name: string; _Value: boolean);
  public
    property Values[const _Name: string]: boolean read GetValues write SetValues;
  end;

implementation

uses
  SysUtils;

{$INCLUDE 't_dzObjectListTemplate.tpl'}

{ TNameBool }

constructor TNameBool.Create(const _Name: string; _Bool: boolean);
begin
  inherited Create;
  FName := _Name;
  FBool := _Bool;
end;

{ TNameBoolList }

function TNameBoolList.FindName(const _Name: string; out _Idx: integer): boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, _Name) then begin
      Result := true;
      _Idx := i;
      exit;
    end;
  Result := false;
end;

function TNameBoolList.GetValues(const _Name: string): boolean;
var
  Idx: integer;
begin
  if not FindName(_Name, Idx) then
    Result := false
  else
    Result := Items[Idx].Bool;
end;

procedure TNameBoolList.SetValues(const _Name: string; _Value: boolean);
var
  Idx: integer;
begin
  if FindName(_Name, Idx) then
    Items[Idx].Bool := _Value
  else
    Add(TNameBool.Create(_Name, _Value))
end;

end.

