unit u_dzErrorThreadList;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils,
  Classes,
  u_dzErrorThread;

{$DEFINE __DZ_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TErrorThread;
{$INCLUDE 't_dzObjectListTemplate.tpl'}

type
  {: List for storing TErrorThread items }
  TErrorThreadList = class(_DZ_OBJECT_LIST_TEMPLATE_)
  public
    function StillRunning: Integer;
    procedure GetErrors(_sl: TStrings);
  end;

implementation

{$INCLUDE 't_dzObjectListTemplate.tpl'}

{ TErrorThreadList }

procedure TErrorThreadList.GetErrors(_sl: TStrings);
var
  i: Integer;
  s: string;
begin
  for i := 0 to Count - 1 do begin
    s := Items[i].ErrorMessage;
    if s <> '' then begin
      UniqueString(s);
      _sl.Add(s);
    end;
  end;
end;

function TErrorThreadList.StillRunning: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if not Items[i].HasFinished then
      Inc(Result);
end;

end.
