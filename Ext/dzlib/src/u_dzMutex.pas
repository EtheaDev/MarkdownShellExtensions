unit u_dzMutex;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SyncObjs,
  u_dzTranslator;

{$IF not Declared(TMutex)}
{MESSAGE HINT 'this unit requires TMutex'}
{$ELSE}

type
  TdzMutex = class(TMutex)
  public
    ///<summary> Calls WaitFor with and returns true, if that returns
    ///          wrSignaled or wrAbandoned. Returns false if WaitFor
    ///          returns wrTimeout or wrError.
    ///          If ExceptionOnError is set to true, wrError will raise an
    ///          exception.
    function TryAcquire(_Timeout: LongWord = 0; _ExceptionOnError: boolean = true): boolean;
  end;
{$IFEND}

implementation

{$IF Declared(TMutex)}
uses
  u_dzMiscUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TdzMutex }

function TdzMutex.TryAcquire(_Timeout: LongWord = 0; _ExceptionOnError: boolean = true): boolean;
var
  Res: TWaitResult;
begin
  Res := WaitFor(_Timeout);
  Result := False;
  case Res of
    wrSignaled, wrAbandoned: begin
        Result := true;
      end;
    wrTimeout: begin
        Result := False;
      end;
    wrError: begin
        if _ExceptionOnError then
          RaiseLastOsErrorEx(self.LastError, _('Error "%1:s (%0:d)" while trying to acquire mutex.'));
        Result := False;
      end;
  end;
end;

{$IFEND}

end.

