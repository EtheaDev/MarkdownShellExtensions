unit u_PCharQueue;

interface

uses
  SysUtils,
  Classes;

{$DEFINE __QUEUE_TEMPLATE__}
type
  _QUEUE_ANCESTOR_ = TObject;
  _QUEUE_CONTAINER_TYPE_ = TList;
  _LIST_CONTAINER_ITEM_TYPE_ = Pointer;
  _QUEUE_ITEM_ = PChar;
{$INCLUDE 't_dzQueueTemplate.tpl'}

type
  {: Queue for storing PChar items }
  TPCharQueue = class(_QUEUE_TEMPLATE_)
  protected
  public
    destructor Destroy; override;
  end;

implementation

{$INCLUDE 't_dzQueueTemplate.tpl'}

{ TPCharQueue }

destructor TPCharQueue.Destroy;
begin
  while not IsEmpty do
    StrDispose(Dequeue);
  inherited;
end;

end.

