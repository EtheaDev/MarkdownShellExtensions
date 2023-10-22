unit u_dzCallbackThread;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes,
  u_dzNamedThread,
  u_dzErrorThread;

type
  ///<summary>
  /// A thread that calls the given callback method, catches exceptions and allows to check
  /// whether it has finished. </summary>
  TdzCallbackThread = class(TErrorThread)
  private
    FCallback: TNotifyEvent;
  protected
    ///<summary>
    /// Calls the callback method given in the constructor. </summary>
    procedure doExecute; override;
  public
    ///<summary>
    /// @param CreateSuspended (see TThread.Create)
    /// @param Callback is TNotifyEvent that will be executed inside the thread
    /// @param Name is the name to set for this thread for the debugger to display </summary>
    constructor Create(_CreateSuspended: Boolean; _Callback: TNotifyEvent; const _Name: string);
    property Terminated;
  end;

implementation

{ TdzCallbackThread }

constructor TdzCallbackThread.Create(_CreateSuspended: Boolean; _Callback: TNotifyEvent; const _Name: string);
begin
  Assert(Assigned(_Callback), 'Callback must be assigned');
  FCallback := _Callback;
  inherited Create(_CreateSuspended, _Name);
end;

procedure TdzCallbackThread.doExecute;
begin
  FCallback(Self);
end;

end.
