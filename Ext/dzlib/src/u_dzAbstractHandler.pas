///<summary>
/// This unit implements an AbstractErrorHandler which raises the exception where
/// the abstract method was called, so it is actually possible to find which bl**dy
/// abstract method has caused the problem.
/// Owner: TM (curtesy of Allan Mertner from Peregrine Systems)
/// Usage: Include in the application, preferable as the first unit.
///</summary>
unit u_dzAbstractHandler;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  SysConst;

implementation

procedure XAbstractErrorHandler;
var
  p: Pointer;
begin
  asm
    mov eax,[ebp+8]
    mov p,eax
  end;
  raise EAbstractError.CreateResFmt(PResStringRec(@SAbstractError), [''])at p;
end;

initialization
  AbstractErrorProc := XAbstractErrorHandler;
end.

