///<summary>
/// Patches TWinControl.SetFocus to avoid the "Cannot focus a xxx control"
/// exception.
/// Source: https://stackoverflow.com/a/41017504/49925 </summary>
unit u_dzSetFocusFix;

interface

implementation

uses
  Windows,
  SysUtils,
  Controls,
  Forms;

type
  TWinControlHack = class(TWinControl)
  public
    procedure SetFocus; override;
  end;

procedure TWinControlHack.SetFocus;
var
  Parent: TCustomForm;
begin
  if not CanFocus then
    Exit;

  Parent := GetParentForm(Self);
  if Parent <> nil then
    Parent.FocusControl(Self)
  else if ParentWindow <> 0 then
    Windows.SetFocus(Handle)
  else
    ValidParentForm(Self);
end;

procedure RedirectFunction(OrgProc, NewProc: Pointer);
type
  TJmpBuffer = packed record
    Jmp: Byte;
    Offset: Integer;
  end;
var
  n: UINT_PTR;
  JmpBuffer: TJmpBuffer;
begin
  JmpBuffer.Jmp := $E9;
  JmpBuffer.Offset := NativeInt(NewProc) - (NativeInt(OrgProc) + 5);

  // theoretically this Move should do the same as the WriteProcessMemory call,
  // but it results in an Access Violation.
//  Move(JmpBuffer, OrgProc^, SizeOf(JmpBuffer));
  if not WriteProcessMemory(GetCurrentProcess, OrgProc, @JmpBuffer, SizeOf(JmpBuffer), n) then
    RaiseLastOSError;
end;

initialization
  RedirectFunction(@TWinControl.SetFocus, @TWinControlHack.SetFocus);

end.

