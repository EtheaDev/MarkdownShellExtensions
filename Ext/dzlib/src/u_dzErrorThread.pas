unit u_dzErrorThread;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils,
  Classes,
  u_dzNamedThread;

type
  ///<summary>
  /// A thread that handles exceptions and provides the error message as well as has a HasFinished
  /// property.
  /// Do not override Execute, override doExecute instead. </summary>
  TErrorThread = class(TNamedThread)
  private
    FExceptionClass: ExceptClass;
    FErrorMessage: string;
    FHasFinished: Boolean;
{$IF not Declared(SyncEvent)}
    // In Delphi 6 these are private in TThread so we can't simply call them but must implement
    // them ourselves.
    procedure CheckThreadError(ErrCode: Integer); overload;
    procedure CheckThreadError(Success: Boolean); overload;
{$IFEND}
  protected
    ///<summary>
    /// Calls inherited to set the thread name and then the doExecute method.
    /// Any Exceptions are caught and their Message stored in ErrorMessage.
    /// After the doExecute method has finished the HasFinished property is set to true. </summary>
    procedure Execute; override;
    ///<summary>
    /// empty method that must be overriden by descendants to do anything </summary>
    procedure doExecute; virtual;
  public
    ///<summary>
    /// Overloaded version of TThread.WaitFor which uses a timeout.
    /// @param TimeoutMsecs is the desired timeout in milliseconds
    /// @param ReturnValue is the result of the thread procecedure (the ReturnValue property
    ///                    of TThread). Only valid if Result = True.
    /// @returns True, if the thread has terminated, False otherwise. </summary>
    function WaitFor(_TimeoutMsecs: DWORD; out _ReturnValue: DWORD): Boolean; overload;
    function WaitFor(_TimeoutMsecs: DWORD): Boolean; overload;
    ///<summary>
    /// Checks whether the thread has terminated and if yes, checks whether an exception was the cause
    /// for this and raises this exception. This method is meant to be called from the main thread
    /// To make sure that an exception in a background thread does not go unnoticed. </summary>
    procedure RaiseErrorException;
    ///<summary>
    /// Calls Windows.TerminateThread to kill the thread without freeing resources.
    /// Read the documentation first!
    /// https://docs.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-terminatethread
    /// @ExitCode is the exit code to return to GetExitCodeThread calls.
    ///           NOTE: ExitCode must *not* be STILL_ACTIVE (259)
    /// @returns True, if TerminateThread was called
    ///          False if not (e.g. if the thread was already finished or never started) </summary>
    function Kill_YesIHaveReadTheTerminateThreadApiDocumentation(_ExitCode: DWORD): Boolean;
    ///<summary>
    /// Is true, when the thread has finished executing </summary>
    property HasFinished: Boolean read FHasFinished;
    ///<summary>
    /// If an unhandled exception occurred in the callback, its message is stored in ErrorMessage.
    /// Only valid after the thread has finished executing (that is HasFinished = true).
    /// see also TThread.FatalException (which is never set if you are using TErrorThread!) </summary>
    property ErrorMessage: string read FErrorMessage;
    ///<summary>
    /// Class of exception whose message was stored in ErrorMessage </summary>
    property ExceptionClass: ExceptClass read FExceptionClass;
  end;

implementation

{$IF not Declared(SyncEvent)}
uses
  RTLConsts;

{ TErrorThread }

procedure TErrorThread.CheckThreadError(ErrCode: Integer);
begin
  if ErrCode <> 0 then
    raise EThread.CreateFmt(SThreadError, [SysErrorMessage(ErrCode), ErrCode]);
end;

procedure TErrorThread.CheckThreadError(Success: Boolean);
begin
  if not Success then
    CheckThreadError(GetLastError);
end;
{$IFEND}

procedure TErrorThread.doExecute;
begin
  // does nothing
end;

procedure TErrorThread.Execute;
begin
  try
    try
      inherited;
      doExecute;
    except
      on e: Exception do begin
        FExceptionClass := ExceptClass(e.ClassType);
        FErrorMessage := e.Message;
        UniqueString(FErrorMessage);
      end;
    end;
  finally
    FHasFinished := True;
  end;
end;

function TErrorThread.Kill_YesIHaveReadTheTerminateThreadApiDocumentation(_ExitCode: DWORD): Boolean;
begin
  Result := not FHasFinished and (Handle <> 0);
  if Result then begin
    Win32Check(Windows.TerminateThread(Handle, 5));
  end;
end;

procedure TErrorThread.RaiseErrorException;
begin
  if HasFinished then begin
    if Assigned(FExceptionClass) then
      raise FExceptionClass.Create(Self.FErrorMessage + ' in ' + Self.Classname);
  end;
end;

function TErrorThread.WaitFor(_TimeoutMsecs: DWORD): Boolean;
var
  Dummy: DWORD;
begin
  Result := WaitFor(_TimeoutMsecs, Dummy);
end;

{$IF Declared(SyncEvent)}

function TErrorThread.WaitFor(_TimeoutMsecs: DWORD; out _ReturnValue: DWORD): Boolean;
var
  h: array[0..1] of THandle;
  WaitResult: Cardinal;
  Msg: TMsg;
begin
  h[0] := Handle;
  if GetCurrentThreadID = MainThreadID then begin
    WaitResult := 0;
    h[1] := SyncEvent;
    repeat
      { This prevents a potential deadlock if the background thread
        does a SendMessage to the foreground thread }
      if WaitResult = WAIT_OBJECT_0 + 2 then
        PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
      if _TimeoutMsecs = INFINITE then begin
        WaitResult := MsgWaitForMultipleObjects(2, h, False, 1000, QS_SENDMESSAGE);
      end else begin
        WaitResult := MsgWaitForMultipleObjects(2, h, False, _TimeoutMsecs, QS_SENDMESSAGE);
      end;
      CheckThreadError(WaitResult <> WAIT_FAILED);
      if WaitResult = WAIT_OBJECT_0 + 1 then
        CheckSynchronize;
      Result := (WaitResult = WAIT_OBJECT_0);
    until Result or (_TimeoutMsecs <> INFINITE);
  end else begin
    WaitResult := WaitForSingleObject(h[0], _TimeoutMsecs);
    if WaitResult = WAIT_FAILED then
      RaiseLastOSError;
    Result := (WaitResult <> WAIT_TIMEOUT);
  end;
  if Result then
    CheckThreadError(GetExitCodeThread(h[0], _ReturnValue));
end;

{$ELSE}
// Delphi 6 did not have a SyncEvent variable (later versions declare it in Classes)

function TErrorThread.WaitFor(_TimeoutMsecs: DWORD; out _ReturnValue: DWORD): Boolean;
var
  h: THandle;
  WaitResult: Cardinal;
  Msg: TMsg;
begin
  h := Handle;
  if GetCurrentThreadID = MainThreadID then begin
    WaitResult := 0;
    repeat
      { This prevents a potential deadlock if the background thread
        does a SendMessage to the foreground thread }
      if WaitResult = WAIT_OBJECT_0 + 1 then
        PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
      if _TimeoutMsecs = INFINITE then begin
        WaitResult := MsgWaitForMultipleObjects(2, h, False, 1000, QS_SENDMESSAGE)
      end else begin
        WaitResult := MsgWaitForMultipleObjects(1, h, False, _TimeoutMsecs, QS_SENDMESSAGE);
      end;
      CheckThreadError(WaitResult <> WAIT_FAILED);
      if WaitResult = WAIT_OBJECT_0 + 1 then
        CheckSynchronize;
      Result := (WaitResult = WAIT_OBJECT_0);
    until Result or (_TimeoutMsecs <> INFINITE);
  end else begin
    WaitResult := WaitForSingleObject(h, _TimeoutMsecs);
    if WaitResult = WAIT_FAILED then
      RaiseLastOSError;
    Result := (WaitResult <> WAIT_TIMEOUT);
  end;
  if Result then
    CheckThreadError(GetExitCodeThread(h, _ReturnValue));
end;
{$IFEND}

end.
