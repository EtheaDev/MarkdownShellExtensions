unit u_dzAssertTrace;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes;

var
  gblAssertTraceOn: Boolean = False;

function WriteTrace(const _fn: string): Boolean;
function ClearTrace: Boolean;

implementation

{$IFDEF ASSERT_TRACING}

{$MESSAGE warn 'Assert tracing is turned on, this will impact performance!'}

var
  Trace: TStringList = nil;

procedure DebugAssertLine(const _Message, _Filename: string; _LineNumber: Integer; _ErrorAddr: Pointer);
var
  Line: string;
begin
  if gblAssertTraceOn then begin
    if not Assigned(Trace) then
      Trace := TStringList.Create;
    Line := ChangeFileExt(ExtractFileName(_Filename), '') + ':' + IntToStr(_LineNumber) + ' ' + _Message;
//    SendDebug(Line);
    Trace.Add(Line);
  end;
end;
{$ENDIF}

function WriteTrace(const _fn: string): Boolean;
begin
{$IF Declared(Trace)}
  Result := (_fn <> '') and Assigned(Trace);
  if Result then begin
    Trace.SaveToFile(_fn);
  end;
{$ELSE}
  Result := False;
{$IFEND}
end;

function ClearTrace: Boolean;
begin
{$IF Declared(Trace)}
  if Assigned(Trace) then
    Trace.Clear;
  Result := True;
{$ELSE}
  Result := True;
{$IFEND}
end;

procedure dummy;
begin
  if ClearTrace then
    WriteTrace('');
end;

{$IFDEF ASSERT_TRACING}
initialization
  dummy;
  AssertErrorProc := DebugAssertLine;
finalization
  FreeAndNil(Trace);
{$ENDIF}
end.
