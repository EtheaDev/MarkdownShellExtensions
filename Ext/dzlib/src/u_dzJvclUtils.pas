unit u_dzJvclUtils;

interface

uses
  JvSpin;

procedure TJvSpinEdit_SetValueNoChange(_sed: TJvSpinEdit; _Value: extended);

implementation

uses
  Classes;

procedure TJvSpinEdit_SetValueNoChange(_sed: TJvSpinEdit; _Value: extended);
var
  Event: TNotifyEvent;
begin
  Event := _sed.OnChange;
  _sed.OnChange := nil;
  try
    _sed.Value := _Value;
  finally
    _sed.OnChange := Event;
  end;
end;

end.
