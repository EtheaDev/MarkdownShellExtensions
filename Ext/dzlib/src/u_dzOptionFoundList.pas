unit u_dzOptionFoundList;

interface

uses
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzQuicksort,
  u_dzOptionDescList;

type
  TOptionFound = class
  private
    FOption: TOptionDesc;
    FName: string;
    FValue: string;
  public
    constructor Create(_Option: TOptionDesc; const _Name, _Value: string);
    function GetPrimaryName: string;
    property Name: string read FName;
    property Value: string read FValue;
  end;

{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TOptionFound;
  _KEY_TYPE_ = string;
{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

type
  ///<summary> List for storing TOptionFound items sorted by String </summary>
  TOptionFoundList = class(_DZ_SORTED_OBJECT_LIST_TEMPLATE_)
  protected
     ///<summary> return the key of an item for comparison </summary>
    function KeyOf(const _Item: TOptionFound): string; override;
     ///<summary> compare the keys of two items, must return a value
     ///          < 0 if Key1 < Key2, = 0 if Key1 = Key2 and > 0 if Key1 > Key2 </summary>
    function Compare(const _Key1, _Key2: string): integer; override;
  end;

implementation

{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

function TOptionFoundList.KeyOf(const _Item: TOptionFound): string;
begin
  Result := _Item.GetPrimaryName;
end;

function TOptionFoundList.Compare(const _Key1, _Key2: string): integer;
begin
  Result := CompareText(_Key1, _Key2);
end;

constructor TOptionFound.Create(_Option: TOptionDesc; const _Name, _Value: string);
begin
  inherited Create;
  FOption := _Option;
  FName := _Name;
  FValue := _Value;
end;

function TOptionFound.GetPrimaryName: string;
begin
  Result := FOption.PrimaryName;
end;

end.

