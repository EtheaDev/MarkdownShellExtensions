unit u_dzInputValidatorCheckListbox deprecated; // use u_dzInputValidator instead

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  CheckLst,
  u_dzInputValidator;

type
  IdzChecklistboxValidator = interface ['{D8A290D5-308D-40BB-A9DB-24CD36E4E5AD}']
    function AtLeastOneChecked: Boolean;
  end;

function ControlValidator(_clb: TCheckListBox; _iv: IdzInputValidator): IdzChecklistboxValidator; overload;

implementation

uses
  u_dzVclUtils;

type
  TChecklistboxValidator = class(TControlValidator, IdzChecklistboxValidator)
  private
    function CheckListBox: TCheckListBox;
  private
    function AtLeastOneChecked: Boolean;
  public
    constructor Create(_clb: TCheckListBox; _iv: IdzInputValidator);
  end;

function ControlValidator(_clb: TCheckListBox; _iv: IdzInputValidator): IdzChecklistboxValidator;
begin
  Result := TChecklistboxValidator.Create(_clb, _iv);
end;

{ TChecklistboxValidator }

constructor TChecklistboxValidator.Create(_clb: TCheckListBox; _iv: IdzInputValidator);
begin
  inherited Create(_clb, _iv);
  SendMessage(_clb.Handle, WM_PRINTCLIENT, 0, 0);
end;

function TChecklistboxValidator.CheckListBox: TCheckListBox;
begin
  Result := FControl as TCheckListBox;
end;

function TChecklistboxValidator.AtLeastOneChecked: Boolean;
begin
  Result := (TCheckListBox_GetCheckedCount(CheckListBox) > 0);
  SetColor(Result);
end;

end.

