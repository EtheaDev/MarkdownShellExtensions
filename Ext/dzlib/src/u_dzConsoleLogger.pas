unit u_dzConsoleLogger;

interface

uses
  u_dzLogging;

type
  TConsoleLogger = class(TAbstractLogger, ILogger)
  protected
    procedure Log(const _s: string; _Level: TLogLevel = llDump); override;
{$WARNINGS OFF}
    // We get lots of deprecated warnings here for inherited methods that implement
    // the ILogger interface. They can be safely ignored.
    // For whatever reason {$WARN SYMBOL_DEPRECATED OFF} doesn't work here.
  end;

implementation

{ TConsoleLogger }

procedure TConsoleLogger.Log(const _s: string; _Level: TLogLevel);
begin
  if IsConsole then
    WriteLn(LogLevel2Str(_Level) + ': ' + _s);
end;

end.

