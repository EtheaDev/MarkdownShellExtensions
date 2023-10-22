unit u_dzIntegerList;

interface

uses
  Classes;

{$DEFINE __DZ_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _LIST_CONTAINER_ = TList;
  _LIST_CONTAINER_ITEM_TYPE_ = pointer; 
  _ITEM_TYPE_ = Integer;
{$INCLUDE 't_dzListTemplate.tpl'}

type
  {: List for storing Integer items }
  TDzIntegerList = class(_DZ_LIST_TEMPLATE_)
  protected
  end;

implementation

{$INCLUDE 't_dzListTemplate.tpl'}

end.
