///<summary>
/// If this unit is included in a program, it will set the name for the main thread to 'Main'.
/// Also it declares a TNamedThread type that uses its class name for the thread
/// name. Make sure to override this if there are multiple instances of this class.
/// If you do not want to derive from TNamedThread, just call SetThreadName from
/// your thread's execute procedure. </summary>
unit u_dzNamedThread;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Classes,
  Windows;

type
  ///<summary> This record must be filled to set the name of a thread </summary>
  TThreadNameInfo = record
    FType: LongWord; // must be 0x1000
    FName: PAnsiChar; // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord; // reserved for future use, must be zero
  end;

///<summary>
/// Set the name (displayed in the debugger) for the current thread
/// @param Name is a string with the name to use </summary>
procedure SetThreadName(const _Name: AnsiString);

type
  /// <summary>
  /// A TThread that sets its name to its class name. Make sure you call
  /// inherited Execute in descendants! </summary>
  TNamedThread = class(TThread)
  protected
    FThreadName: string;
    ///<summary>
    /// Calls SetName with FThreadName </summary>
    procedure SetName; overload; virtual;
    ///<summary>
    /// @note: Do *not* make this a class method! </summary>
    procedure SetName(const _Name: string); overload; virtual;
    ///<summary>
    /// Calls SetName </summary>
    procedure Execute; override;
    function GetThreadName: string;
  public
    ///<summary>
    /// @param CreateSuspended (see TThread.Create)
    /// @param Name is the name to set for the thread, if empty, the ClassName will be used </summary>
    constructor Create(_CreateSuspended: Boolean; const _Name: string = '');
    ///<summary>
    /// This is not a class method so it only works for this thread object.
    /// That's on purpose: The class methode introduced in later Delphi versions (it does exist
    /// in Delphi 10.2) should always be called as TThread.CheckTerminates to prevent confusion
    /// which thread is being checked.
    /// @returns The value of the protected Perminated property </summary>
    function CheckTerminated: Boolean;
  end;

implementation

{$IFDEF SUPPORTS_UNSAFE_WARNINGS}
{$WARN UNSAFE_CODE OFF}
{$ENDIF}

procedure SetThreadName(const _Name: AnsiString);
var
  ThreadNameInfo: TThreadNameInfo;
begin
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := PAnsiChar(_Name);
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;
  try
    RaiseException($406D1388, 0, SizeOf(ThreadNameInfo) div SizeOf(LongWord), Pointer(@ThreadNameInfo));
  except
    // ignore
  end; //FI:W501 Empty EXCEPT block
end;

{ TNamedThread }

constructor TNamedThread.Create(_CreateSuspended: Boolean; const _Name: string = '');
begin
  if _Name <> '' then
    FThreadName := _Name
  else
    FThreadName := ClassName;

  inherited Create(_CreateSuspended);
end;

function TNamedThread.CheckTerminated: Boolean;
begin
  Result := Terminated;
end;

procedure TNamedThread.Execute;
begin
  SetName;
end;

function TNamedThread.GetThreadName: string;
begin
  Result := FThreadName;
end;

procedure TNamedThread.SetName(const _Name: string);
begin
  SetThreadName(AnsiString(_Name));
end;

procedure TNamedThread.SetName;
begin
  SetName(FThreadName);
end;

initialization
  // set the name for the main thread to 'Main'
  SetThreadName('Main');
end.
