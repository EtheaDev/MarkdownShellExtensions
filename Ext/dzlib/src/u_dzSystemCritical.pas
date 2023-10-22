///<summary>
// http://delphidabbler.com/tips/127
// @Author: Shlomo Abuisak </summary>
unit u_dzSystemCritical;

interface

uses
  Windows;

type
  TSystemCritical = class
  private
    FIsCritical: Boolean;
    procedure SetIsCritical(const Value: Boolean);
  protected
    procedure UpdateCritical(Value: Boolean); virtual;
  public
    constructor Create;
    property IsCritical: Boolean read FIsCritical write SetIsCritical;
  end;

var
  SystemCritical: TSystemCritical;

implementation

{ TSystemCritical }
// REF: http://msdn.microsoft.com/en-us/library/aa373208.aspx
type
  EXECUTION_STATE = DWORD;

const
  ES_SYSTEM_REQUIRED = $00000001;
  ES_DISPLAY_REQUIRED = $00000002;
  ES_USER_PRESENT = $00000004;
  ES_AWAYMODE_REQUIRED = $00000040;
  ES_CONTINUOUS = $80000000;

{
  SetThreadExecutionState Function
  Enables an application to inform the system that it is in use,
  thereby preventing the system from entering sleep or turning off the
  display while the application is running.
}

procedure SetThreadExecutionState(ESFlags: EXECUTION_STATE);
  stdcall; external kernel32 name 'SetThreadExecutionState';

constructor TSystemCritical.Create;
begin
  inherited;
  FIsCritical := False;
end;

procedure TSystemCritical.SetIsCritical(const Value: Boolean);
begin
  if FIsCritical = Value then
    Exit;
  FIsCritical := Value;
  UpdateCritical(FIsCritical);
end;

procedure TSystemCritical.UpdateCritical(Value: Boolean);
begin
  if Value then
    // Prevent the sleep idle time-out and Power off.
    SetThreadExecutionState(ES_SYSTEM_REQUIRED or ES_CONTINUOUS)
  else
    // Clear EXECUTION_STATE flags to disable away mode and allow the
    // system to idle to sleep normally.
    SetThreadExecutionState(ES_CONTINUOUS);
end;

initialization
  SystemCritical := TSystemCritical.Create;
finalization
  SystemCritical.IsCritical := False;
  SystemCritical.Free;
end.
