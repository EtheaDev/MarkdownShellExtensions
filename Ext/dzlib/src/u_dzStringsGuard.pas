unit u_dzStringsGuard;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
{$IFDEF HAS_UNIT_WIDESTRINGS}
  WideStrings,
{$ENDIF}
  Classes;

type
  IStringsGuard = interface ['{FE9A8BF7-A8CA-4D87-968A-0662C0B20C0A}']
    function List: TStrings;
  end;

function Guard(_List: TStrings): IStringsGuard; overload;

{$IFDEF HAS_UNIT_WIDESTRINGS}
type
  IWideStringsGuard = interface ['{B76D2FE5-9E4A-4B0F-A1D3-4CD94B6C03C6}']
    function List: TWideStrings;
  end;

function Guard(_List: TWideStrings): IWideStringsGuard; overload;
{$ENDIF}

implementation

uses
  u_dzObjectGuard;

type
  TStringsGuard = class(TObjectGuard, IStringsGuard)
  public
    function List: TStrings;
  end;

{ TStringsGuard }

function TStringsGuard.List: TStrings;
begin
  Result := GuardedObject as TStrings;
end;

function Guard(_List: TStrings): IStringsGuard;
begin
  Result := TStringsGuard.Create(_List);
end;

{$IFDEF HAS_UNIT_WIDESTRINGS}
type
  TWideStringsGuard = class(TObjectGuard, IWideStringsGuard)
  public
    function List: TWideStrings;
  end;

{ TWideStringsGuard }

function TWideStringsGuard.List: TWideStrings;
begin
  Result := GuardedObject as TWideStrings;
end;

function Guard(_List: TWideStrings): IWideStringsGuard;
begin
  Result := TWideStringsGuard.Create(_List);
end;
{$ENDIF}

end.

