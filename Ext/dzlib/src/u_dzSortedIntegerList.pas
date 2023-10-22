unit u_dzSortedIntegerList;

interface

uses
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzQuicksort,
  u_dzList;

{$DEFINE __DZ_SORTED_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _LIST_CONTAINER_ = TdzList;
  _LIST_CONTAINER_ITEM_TYPE_ = pointer;
  _ITEM_TYPE_ = Integer;
  _KEY_TYPE_ = Integer;
{$DEFINE  __DZ_SORTED_LIST_TEMPLATE_ITEM_TYPE_IS_INTEGER__}
{$INCLUDE 't_dzSortedListTemplate.tpl'}

type
  {: Sorted list for storing Integer items sorted by Integer }
  TSortedIntegerList = class(_DZ_SORTED_LIST_TEMPLATE_)
  protected
    {: return the key of an item for comparison }
    function KeyOf(const _Item: Integer): Integer; override;
    {: compare the keys of two items, must return a value
       < 0 if Key1 < Key2, = 0 if Key1 = Key2 and > 0 if Key1 > Key2 }
    function Compare(const _Key1, _Key2: Integer): integer; override;
    {: Frees a Integer }
    procedure FreeItem(_Item: Integer); override;
  end;

implementation

{$INCLUDE 't_dzSortedListTemplate.tpl'}

function TSortedIntegerList.KeyOf(const _Item: Integer): Integer;
begin
  Result := _Item;
end;

function TSortedIntegerList.Compare(const _Key1, _Key2: Integer): integer;
begin
  Result := _Key1 - _Key2;
end;

procedure TSortedIntegerList.FreeItem(_Item: Integer);
begin
  { Integers don't need to be freed }
end;

end.

