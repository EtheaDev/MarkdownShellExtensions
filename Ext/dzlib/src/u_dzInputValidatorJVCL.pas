unit u_dzInputValidatorJVCL;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  JvToolEdit,
  u_dzInputValidator;

type
  IdzJvDateEditValidator = interface ['{7F573119-AA32-4D34-9433-29E3BC81D8AC}']
    function ValidateHasValue: Boolean;
  end;

function ControlValidator(_ed: TJvDateEdit; _iv: IdzInputValidator): IdzJvDateEditValidator; overload;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

type
  TJvDateEditValidator = class(TControlValidator, IdzJvDateEditValidator)
  private
    function DateEdit: TJvDateEdit;
  private
    function ValidateHasValue: Boolean;
  end;

{ TJvDateEditValidator }

function TJvDateEditValidator.DateEdit: TJvDateEdit;
begin
  Result := FControl as TJvDateEdit;
end;

function TJvDateEditValidator.ValidateHasValue: Boolean;
begin
  Result := (DateEdit.Date <> 0);
end;

function ControlValidator(_ed: TJvDateEdit; _iv: IdzInputValidator): IdzJvDateEditValidator;
begin
  Result := TJvDateEditValidator.Create(_ed, _iv);
end;

{$ENDIF DELPHI2007_UP}

end.

