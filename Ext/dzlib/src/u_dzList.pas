unit u_dzList;

interface

uses
  SysUtils,
  Classes;

type
  TdzList = class(TList)
  protected
    procedure Grow; override;
  public
    procedure ShrinkToMinimum;
  end;

implementation

{ TdzList }

procedure TdzList.Grow;
var
  CurCapacity: Integer;
  Delta: Integer;
begin
  CurCapacity := Capacity;
  if CurCapacity > 1024 * 1024 then
    // we can't grwo large lists by 1/4 of the capacity because this will quickly run out of memory
    // so we only grow by 1024
    Delta := 1024
  else if CurCapacity > 64 then
    Delta := CurCapacity div 4
  else if CurCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(CurCapacity + Delta);
end;

procedure TdzList.ShrinkToMinimum;
begin
  SetCapacity(Count);
end;

end.

