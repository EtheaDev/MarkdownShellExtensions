{.GXFormatter.config=twm}
///<summary>
/// Implements a wrapper object around the Win32 API CreateProcess and
/// related functions.
/// @author twm </summary>
unit u_dzAsyncExec;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzCallbackThread;

type
  TdzPipe = class
  private
    FReadHandle: THandle;
    FWriteHandle: THandle;
    FPipeName: string;
  public
    ///<summary>
    /// @param PipeName is the name of the pipe, only for information / debugging
    /// @param InheritableRead determines whether the pipe's read handle is inheritable
    /// @param InheritableRead determines whether the pipe's write handle is inheritable</summary>
    constructor Create(const _PipeName: string; _InheritableRead, _InheritableWrite: Boolean);
    destructor Destroy; override;
    property PipeName: string read FPipeName;
    property ReadHandle: THandle read FReadHandle;
    property WriteHandle: THandle read FWriteHandle;
  end;

type
  ///<summary>
  /// Pipe to be used to redirect StdOut or StdErr </summary>
  TdzOutPipe = class(TdzPipe)
  public
    type
      TReadCallback = procedure(_Sender: TObject; _Byte: Byte) of object;
  private
    FReadThread: TdzCallbackThread;
    FReadCallback: TReadCallback;
    procedure ReadCallback(_Sender: TObject);
  public
    constructor Create(_ReadCallback: TReadCallback; const _PipeName: string = 'StdOut');
    destructor Destroy; override;
  end;

type
  ///<summary>
  /// Pipe to be used to redirect StdIn </summary>
  TdzInPipe = class(TdzPipe)
  public
    type
      TWriteCallback = procedure(_Sender: TObject; out _Bytes: TBytes) of object;
  private
    FWriteCallback: TWriteCallback;
    FWriteThread: TdzCallbackThread;
    procedure WriteCallback(_Sender: TObject);
  public
    constructor Create(_WriteCallback: TWriteCallback; const _PipeName: string = 'StdIn');
    destructor Destroy; override;
  end;

type
  ///<sumamary>
  /// ancestor to all exceptions raised in this unit </summary>
  EAsyncExec = class(Exception);
  ///<sumamary>
  /// raised, when calling GetExitCode while Status is esInvalid </summary>
  ENoProcess = class(EAsyncExec);
  ///<sumamary>
  /// raised, when calling GetStdOut or GetStdErr while Status is esRunning </summary>
  EProcessRunning = class(EAsyncExec);
  ///<sumamary>
  /// raised, when trying to destroy the object while a process with I/O redirection
  /// is still running. </summary>
  ERedirectedProcess = class(EProcessRunning);
  ///<sumamary>
  /// raised when calling TAsyncExec methods which do not work when the process
  /// has already terminated. </summary>
  EProcessTerminated = class(EAsyncExec);
  ///<sumamary>
  /// raised when either StdOut or StdErr is accessed when RedirectStdXxx is not true </summary>
  ENotRedirected = class(EAsyncExec);

type
  ///<sumamary>
  //// Status of the TAsyncExec object
  //// * esInvalid = Process has not yet been started
  //// * esRunning = Process is running
  //// * esTerminated = Process has terminated </summary>
  TAsyncExecStatus = (esInvalid, esRunning, esTerminated);
  TAsyncExecStatusSet = set of TAsyncExecStatus;

type
  ///<summary>
  /// Class wrapper around the Win32 functions for starting child processes.
  /// It allows to start a process, redirect stdIn/Out/Err, wait for it to finish,
  /// get the exit code and terminate it.
  /// Usage:<code>
  ///   AExec := TAsyncExec.Create;
  ///   AExec.WorkingDir := 's:\';
  ///   AExec.Commandline := 'cvs -n -q update';
  ///   AExec.Execute;
  ///   AExec.Wait;
  ///   MessageDlg(Format('StdOut: %s'#13#10+
  ///                     'StdErr: %s', [AExec.StdOut, AExec.StdErr]),
  ///             mtInformation, [mbOK], 0);
  ///   AExec.Free;
  /// </code> </summary>
  TAsyncExec = class
  public
    type
      TOnOuputLine = procedure(_Sender: TObject; const _Line: string) of object;
  private
    FLastError: Integer;
    FStdOutBuffer: string;
    FStdErrBuffer: string;
    FOnStdOutLine: TOnOuputLine;
    FOnStdErrLine: TOnOuputLine;
    FStdOutLine: string;
    FStdErrLine: string;
    procedure StdOutReadCallback(_Sender: TObject; _Byte: Byte);
    procedure StdErrReadCallback(_Sender: TObject; _Byte: Byte);
    procedure StdInWriteCallback(_Sender: TObject; out _Bytes: TBytes);
    procedure SetOnStdErrLine(_Value: TOnOuputLine);
    procedure SetOnStdOutLine(_Value: TOnOuputLine);
  protected
    FExeName: string;
    FCommandline: string;
    FEnvironment: TStringList;
    FWorkingDir: string;
    FIsStdOutRedirected: Boolean;
    FIsStdErrRedirected: Boolean;
    FStdIn: AnsiString;
    FExitCode: DWORD;
    FVisible: Boolean;
    ///<summary>
    /// pipe to be used as standard input for the process </summary>
    FStdInPipe: TdzInPipe;
    ///<summary>
    /// pipe to be used as standard output for the process </summary>
    FStdOutPipe: TdzOutPipe;
    ///<summary>
    /// pipe to be used as standard error for the process </summary>
    FStdErrPipe: TdzOutPipe;
    FProcessInfo: TProcessInformation;
    FStatus: TAsyncExecStatus;
    ///<summary>
    /// gets the process handle, raises an ENoProcess exception if status is esInvalid
    /// @returns the process handle value </summary>
    function GetProcessHandle: THandle;
    ///<summary>
    /// gets the thread handle, raises an ENoProcess exception if status is esInvalid
    /// @returns the process main thread handle </summary>
    function GetThreadHandle: THandle;
    ///<summary>
    /// gets the process exit code, raises an ENoProcess exception if status is esInvalid
    /// @returns the process exit code </summary>
    function GetExitCode: DWORD;
    ///<summary>
    /// reads the given stream into a string </summary>
    function ReadTextFile(_st: TStream): string;
    ///<summary>
    /// gets the Status property which is esInvalid when no process has been started,
    /// esRunning while the process is still running and esTerminated when the
    /// process has terminated. </summary>
    function GetStatus: TAsyncExecStatus;
    ///<summary>
    /// checks whether the current status is in the set passed and raises one of
    /// ENoProcess, EProcessRunning, EProcessTerminated if not.
    /// This method is called by various other methods to assert that their
    /// functions are allowed in the current Status.
    /// @param ValidStatusSet is a set of valid Status values for the check. </summary>
    procedure AssertStatus(_ValidStatusSet: TAsyncExecStatusSet; const _Method: string = '');
  public
    ///<summary>
    /// Creates a TAsyncExec object }
    constructor Create;
    ///<summary>
    /// Destroys a TAsyncExec object. If the Ststus is esRunning and the process
    /// uses I/O redirection, this raises an ERedirectedProcess exception </summary>
    destructor Destroy; override;
    ///<summary>
    /// Does everything necessary to start the given executable with the
    /// given parameters without waiting for it to finish. </summary>
    class procedure Execute(const _Executable, _Parameters: string); overload;
    ///<summary>
    /// Waits for the process to terminate. Raises ENoProcess if Status is
    /// esInvalid. If Status is esTerminated this funtion returns immediately.
    /// @param Timeout is a DWORD giving the time to wait in milliseconds
    /// @returns the result of the call to WaitForSingleObject </summary>
    function Wait(_Timeout: DWORD = INFINITE): DWORD;
    ///<summary>
    /// Kills a running process, avoid using this since it may not free all
    /// resources.
    /// @returns true on success, false otherwise </summary>
    function Kill: Boolean;
    ///<summary>
    /// Searches for the executable given in the ExeName property using the
    /// system search path. If found the ExeName is set to the fully qualified
    /// name of the executable.
    /// @returns true if found, false otherwise </summary>
    function FindExecutable(_ExeName: string = ''): Boolean;
    ///<summary>
    /// Executes the program given either by ExeName or as the first parameter
    /// in Commandline.
    /// @param RaiseException determines whether to raise an exception if CreateProcess
    ///                       fails or simply return false. If false Execute will still
    ///                       call GetLastError to retrieve the error code and it will
    ///                       be available in the LastError property
    ///  @returns the result of the call to CreateProcess </summary>
    function doExecute(_RaiseException: Boolean = True): Boolean;
    function Execute(_RaiseException: Boolean = True): Boolean; overload; deprecated; // use doExecute instead
    property LastError: Integer read FLastError;
    ///<summary>
    /// This is a quick hack to allow freeing the object without waiting for the program
    /// to terminate </summary>
    procedure ResetStatus;
    ///<summary>
    /// Fully qualified name of the executable to start. FindExecutable can be
    /// used if only the filename is known. Alternatively this can be left
    /// empty in which case the first parameter in CommanLine is used as
    /// executable name. </summary>
    property Exename: string read FExeName write FExeName;
    ///<summary>
    /// Commandline to pass to the process. If ExeName is empty the first
    /// parameter is used as executable name. </summary>
    property Commandline: string read FCommandline write FCommandline;
    property Environment: TStringList read FEnvironment;
    ///<summary>
    /// determines whether the process is started visible or not. If set
    /// to true, starting a commandline program form a GUI app will open
    /// a console window. </summary>
    property Visible: Boolean read FVisible write FVisible;
    ///<summary>
    /// @returns True if StdIn is redirected </summary>
    function IsStdInRedirected: Boolean;
    ///<summary>
    /// Set to true to redirect the process' standard output. This will result
    /// in having all text the process writes to its standard output handle
    /// copied to the StdOut property. </summary>
    property IsStdOutRedirected: Boolean read FIsStdOutRedirected write FIsStdOutRedirected;
    ///<summary>
    /// Set to true to redirect the process' standard output. This will result
    /// in having all text the process writes to its standard output handle
    /// copied to the StdOut property. </summary>
    property IsStdErrRedirected: Boolean read FIsStdErrRedirected write FIsStdErrRedirected;
    ///<summary>
    /// Set this to the standard input you want to supply to the process. </summary>
    property StdIn: AnsiString read FStdIn write FStdIn;
    ///<summary>
    /// After the process has terminated this contains the standard output.
    /// @raises either ENoProcess or EProcessRunning when accessed before the process has
    ///         terminated /summary>
    function StdOut: string;
    ///<summary>
    /// After the process has terminated this contains the standard error.
    /// @raises either ENoProcess or EProcessRunning when accessed before the process has
    ///         terminated /summary>
    function StdErr: string;
    ///<summary>
    /// Contains the exit code of the process.
    /// @raises an ENoProcess exception if accessed while Status is esInvalid
    /// While the process is still running ExitCode is STILL_ACTIVE. </summary>
    property ExitCode: DWORD read GetExitCode;
    ///<summary>
    /// Contains the handle of the process
    /// @raises ENoProcess while status is esInvalid. </summary>
    property ProcessHandle: THandle read GetProcessHandle;
    ///<summary>
    /// Contains the handle of the process' main thread
    /// @raises ENoProcess while status is esInvalid. </summary>
    property ThreadHandle: THandle read GetThreadHandle;
    ///<summary>
    /// Contains the status of a TAsyncExec which is
    /// * esInvalid when no process has been started,
    /// * esRunning while the process is still running and
    /// * esTerminated when the process has terminated. </summary>
    property Status: TAsyncExecStatus read GetStatus;
    ///<summary>
    /// Contains the working directory the process should be started in. If
    /// left empty the process will start in the current directory. </summary>
    property WorkingDir: string read FWorkingDir write FWorkingDir;
    property OnStdOutLine: TOnOuputLine read FOnStdOutLine write SetOnStdOutLine;
    property OnStdErrLine: TOnOuputLine read FOnStdErrLine write SetOnStdErrLine;
  end;

implementation

uses
  u_dzOsUtils,
  u_dzMiscUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TdzPipe }

constructor TdzPipe.Create(const _PipeName: string; _InheritableRead, _InheritableWrite: Boolean);
var
  PipeAttrs: SECURITY_ATTRIBUTES;
  LastError: DWORD;
begin
  inherited Create;
  FPipeName := _PipeName;
  FReadHandle := INVALID_HANDLE_VALUE;
  FWriteHandle := INVALID_HANDLE_VALUE;

  PipeAttrs.nLength := SizeOf(PipeAttrs);
  PipeAttrs.lpSecurityDescriptor := nil;
  PipeAttrs.bInheritHandle := _InheritableRead or _InheritableWrite;
  if not CreatePipe(FReadHandle, FWriteHandle, @PipeAttrs, 0) then begin
    LastError := GetLastError;
    // Keep the duplicate % signs here!
    RaiseLastOsErrorEx(LastError,
      Format(_('Error %%1:s (%%0:d) calling CreatePipe for pipe %s'), [FPipeName]));
  end;

  if _InheritableRead xor _InheritableWrite then begin
    // if the inheritable flags for the read and write handle are different
    if not _InheritableRead then
      if not SetHandleInformation(FReadHandle, HANDLE_FLAG_INHERIT, 0) then begin
        LastError := GetLastError;
        // Keep the duplicate % signs here!
        RaiseLastOsErrorEx(LastError,
          Format(_('Error %%1:s (%%0:d) calling SetHandleInformation for read handle of pipe %s'), [FPipeName]));
      end;
    if not _InheritableWrite then
      if not SetHandleInformation(FWriteHandle, HANDLE_FLAG_INHERIT, 0) then begin
        LastError := GetLastError;
        // Keep the duplicate % signs here!
        RaiseLastOsErrorEx(LastError,
          Format(_('Error %%1:s (%%0:d) calling SetHandleInformation for write handle of pipe %s'), [FPipeName]));
      end;
  end;
end;

destructor TdzPipe.Destroy;
begin
  if FReadHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FReadHandle);
  if FWriteHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FWriteHandle);
  inherited;
end;

{ TdzOutPipe }

constructor TdzOutPipe.Create(_ReadCallback: TReadCallback;
  const _PipeName: string = 'StdOut');
begin
  Assert(Assigned(_ReadCallback));

  inherited Create(_PipeName, False, True);
  FReadCallback := _ReadCallback;
  FReadThread := TdzCallbackThread.Create(True, ReadCallback, FPipeName + 'ReadThread');
{$IFDEF TTHREAD_HAS_START}
  FReadThread.Start;
{$ELSE}
  FReadThread.Resume;
{$ENDIF}
end;

destructor TdzOutPipe.Destroy;
begin
  // first, close the WriteHandle which will sooner or later cause the reader thread to exit
  if FWriteHandle <> INVALID_HANDLE_VALUE then begin
    FileClose(FWriteHandle);
    FWriteHandle := INVALID_HANDLE_VALUE;
  end;

  // wait for the reader thread to finish and free it
  FReadThread.Free;
  // only then set the variable to nil
  FReadThread := nil;

  // then call inherited, which will also close the ReadHandle
  inherited;
end;

procedure TdzOutPipe.ReadCallback(_Sender: TObject);
var
  OK: Boolean;
  TheByte: Byte;
  BytesRead: DWORD;
begin
  while not FReadThread.Terminated do begin
    OK := ReadFile(ReadHandle, TheByte, SizeOf(TheByte), BytesRead, nil);
    if not OK or (BytesRead = 0) then begin
      // the pipe's WriteHandle has been closed, we are done
      Exit; //==>
    end;
    FReadCallback(Self, TheByte);
  end;
end;

{ TdzInPipe }

constructor TdzInPipe.Create(_WriteCallback: TWriteCallback; const _PipeName: string = 'StdIn');
begin
  Assert(Assigned(_WriteCallback));

  inherited Create(_PipeName, True, False);
  FWriteCallback := _WriteCallback;
  FWriteThread := TdzCallbackThread.Create(True, WriteCallback, FPipeName + 'WriteThread');
{$IFDEF TTHREAD_HAS_START}
  FWriteThread.Start;
{$ELSE}
  FWriteThread.Resume;
{$ENDIF}
end;

destructor TdzInPipe.Destroy;
begin
  // first, close the ReadHandle which will sooner or later cause the writer thread to exit
  if FReadHandle <> INVALID_HANDLE_VALUE then begin
    FileClose(FReadHandle);
    FReadHandle := INVALID_HANDLE_VALUE;
  end;

  // wait for the writer thread to finish and free it
  FWriteThread.Free;
  // only then set the variable to nil
  FWriteThread := nil;

  // then call inherited, which will also close the WriteHandle
  inherited;
end;

procedure TdzInPipe.WriteCallback(_Sender: TObject);
var
  Bytes: TBytes;
  BytesWritten: DWORD;
  Len: Integer;
  OK: Boolean;
  LastError: DWORD;
begin
  while not FWriteThread.Terminated do begin
    SetLength(Bytes, 0);
    FWriteCallback(Self, Bytes);
    Len := Length(Bytes);
    if Len > 0 then begin
      OK := WriteFile(WriteHandle, Bytes[0], Len, BytesWritten, nil);
      if not OK then begin
        LastError := GetLastError;
        if LastError = ERROR_BROKEN_PIPE then begin
          // the pipe's HeadHandle has been closed, we are done
          Exit; //==>
        end;
        // todo: Handle other errors
      end;
    end;
  end;
end;

// Auteur Thaddy de Koning
// adapted to unicode by twm
type
  TEnvironmentBlockReader = class(TStringList)
  public
    constructor Create;
  end;

  TEnvironmentBlockWriter = class(TStringStream)
  private
    FClosed: Boolean;
    function GetBlockPtr: PChar;
  protected
    procedure Close;
  public
    procedure Add(const AValue: string); overload;
    procedure Add(const aToken, AValue: string); overload;
    property Block: PChar read GetBlockPtr;
  end;

{ TAsyncExec }

constructor TAsyncExec.Create;
begin
  inherited;
  FStatus := esInvalid;
  FIsStdOutRedirected := False;
  FIsStdErrRedirected := False;
  FEnvironment := TEnvironmentBlockReader.Create;
  FVisible := True;
  ZeroMemory(@FProcessInfo, SizeOf(FProcessInfo));
  FWorkingDir := 'c:\';
end;

destructor TAsyncExec.Destroy;
begin
  inherited;
  if (Status = esRunning) and
    (IsStdInRedirected or IsStdOutRedirected or IsStdErrRedirected) then
    raise ERedirectedProcess.Create(_('Can not free an TAsyncExec instanced while a process using redirection is still running.'));
  FStdInPipe.Free;
  FStdOutPipe.Free;
  FStdErrPipe.Free;
  FEnvironment.Free;
  //  Kernel objects, like the process and the files we created in this case,
  //  are maintained by a usage count.
  //  So, for cleaning up purposes we have to close the handles
  //  to inform the system that we don't need the objects anymore
  if FProcessInfo.hThread <> 0 then
    CloseHandle(FProcessInfo.hThread);
  if FProcessInfo.hProcess <> 0 then
    CloseHandle(FProcessInfo.hProcess);
end;

function TAsyncExec.FindExecutable(_ExeName: string): Boolean;
var
  SearchPath: string;
  Found: string;
begin
  if _ExeName = '' then
    _ExeName := FExeName;
  Result := _ExeName <> '';
  if Result then begin
    SearchPath := GetEnvironmentVariable('path');
    Found := FileSearch(_ExeName, SearchPath);
    Result := Found <> '';
    if Result then
      FExeName := Found;
  end;
end;

procedure TAsyncExec.AssertStatus(_ValidStatusSet: TAsyncExecStatusSet; const _Method: string);
var
  Stat: TAsyncExecStatus;
begin
  Stat := GetStatus;
  if not (Stat in _ValidStatusSet) then
    case Stat of
      esInvalid: raise ENoProcess.CreateFmt(_('Process has not yet been started.'#13#10'%s'), [_Method]);
      esRunning: raise EProcessRunning.CreateFmt(_('Process is still running.'#13#10'%s'), [_Method]);
      esTerminated: raise EProcessTerminated.CreateFmt(_('Process has terminated.'#13#10'%s'), [_Method]);
    end;
end;

function TAsyncExec.GetStatus: TAsyncExecStatus;
begin
  if FStatus = esRunning then begin
    Win32Check(GetExitCodeProcess(FProcessInfo.hProcess, FExitCode));
    if FExitCode = STILL_ACTIVE then
      FStatus := esRunning
    else
      FStatus := esTerminated;
  end;
  Result := FStatus;
end;

function TAsyncExec.GetExitCode: DWORD;
begin
  AssertStatus([esRunning, esTerminated]);
  Result := FExitCode;
end;

function TAsyncExec.Wait(_Timeout: DWORD): DWORD;
begin
  AssertStatus([esRunning, esTerminated]);
  Result := WaitforSingleObject(ProcessHandle, _Timeout);
end;

function TAsyncExec.Kill: Boolean;
begin
  case GetStatus of
    esInvalid: raise ENoProcess.Create(_('Process has not yet been started'));
    esRunning: Result := TerminateProcess(FProcessInfo.hProcess, $FFFFFFFF);
  else
    Result := True;
  end;
end;

function TAsyncExec.GetProcessHandle: THandle;
begin
  AssertStatus([esRunning, esTerminated]);
  Result := FProcessInfo.hProcess;
end;

function TAsyncExec.GetThreadHandle: THandle;
begin
  AssertStatus([esRunning, esTerminated]);
  Result := FProcessInfo.hThread;
end;

function TAsyncExec.IsStdInRedirected: Boolean;
begin
  Result := FStdIn <> '';
end;

procedure TAsyncExec.StdErrReadCallback(_Sender: TObject; _Byte: Byte);
var
  c: Char;
begin
  c := Char(AnsiChar(_Byte));
  FStdErrBuffer := FStdErrBuffer + c;
  case _Byte of
    10: begin
        // ignore LF
      end;
    13: begin
        // on CR send a new line
        if Assigned(FOnStdErrLine) then
          FOnStdErrLine(Self, FStdErrLine);
        FStdErrLine := '';
      end;
  else
    FStdErrLine := FStdErrLine + c;
  end;
end;

procedure TAsyncExec.StdOutReadCallback(_Sender: TObject; _Byte: Byte);
var
  c: Char;
begin
  c := Char(AnsiChar(_Byte));
  FStdOutBuffer := FStdOutBuffer + c;
  case _Byte of
    10: begin
        // ignore LF
      end;
    13: begin
        // on CR send a new line
        if Assigned(FOnStdOutLine) then
          FOnStdOutLine(Self, FStdOutLine);
        FStdOutLine := '';
      end;
  else
    FStdOutLine := FStdOutLine + c;
  end;
end;

procedure TAsyncExec.StdInWriteCallback(_Sender: TObject; out _Bytes: TBytes);
var
  i: Integer;
  Len: Integer;
begin
  // todo: this is probably not the best approach
  Len := Length(FStdIn);
  SetLength(_Bytes, Len);
  for i := 1 to Length(FStdIn) do
    _Bytes[i - 1] := Ord(FStdIn[i]);
end;

function TAsyncExec.doExecute(_RaiseException: Boolean): Boolean;
var
  StartupInfo: TStartupInfo;
  SecurityAttributes: TSecurityAttributes;
  Cmdline: string;
  env: TEnvironmentBlockWriter;
  i: Integer;
  pCmdLine: PChar;
  pEnv: PChar;
  PExeName: PChar;
  CreationFlags: DWORD;
begin
  if FStatus <> esInvalid then
    raise EAsyncExec.Create(_('Process has already been started.'));

  // prepare SecurityAttributes for files, set InheritHandle to true
  ZeroMemory(@SecurityAttributes, SizeOf(SecurityAttributes));
  SecurityAttributes.nLength := SizeOf(SecurityAttributes);
  SecurityAttributes.lpSecurityDescriptor := nil;
  SecurityAttributes.bInheritHandle := True;

  // prepare StartupInfo structure
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.lpDesktop := nil;
  StartupInfo.hStdOutput := 0;
  StartupInfo.hStdInput := 0;
  StartupInfo.hStdError := 0;
  StartupInfo.dwFlags := 0;

  if IsStdInRedirected then begin
    FStdInPipe := TdzInPipe.Create(StdInWriteCallback);
    StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESTDHANDLES;
    StartupInfo.hStdInput := FStdInPipe.ReadHandle;
  end;

  if IsStdOutRedirected then begin
    FStdOutPipe := TdzOutPipe.Create(StdOutReadCallback);
    StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESTDHANDLES;
    StartupInfo.hStdOutput := FStdOutPipe.WriteHandle;
  end;

  if IsStdErrRedirected then begin
    FStdErrPipe := TdzOutPipe.Create(StdErrReadCallback, 'StdErr');
    StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESTDHANDLES;
    StartupInfo.hStdError := FStdErrPipe.WriteHandle;
  end;

  if not FVisible then begin
    StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_HIDE;
  end else begin
    StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_SHOWNORMAL;
  end;

  if FExeName <> '' then begin
    Cmdline := Format('"%s" %s', [FExeName, FCommandline]);
    PExeName := PChar(FExeName);
  end else begin
    PExeName := nil;
    Cmdline := FCommandline;
  end;

  env := nil;
  pCmdLine := StrNew(PChar(Cmdline));
  try
    env := TEnvironmentBlockWriter.Create('');
    for i := 0 to Environment.Count - 1 do begin
      env.Add(Environment[i]);
    end;
    pEnv := env.Block;

    CreationFlags := NORMAL_PRIORITY_CLASS;
{$IFDEF SUPPORTS_UNICODE_STRING}
    CreationFlags := CreationFlags or CREATE_UNICODE_ENVIRONMENT;
{$ENDIF SUPPORTS_UNICODE_STRING}

    Result := CreateProcess(
      PExeName, // pointer to the executable (or nil)
      pCmdLine, // pointer to command line string (or nil)
      nil, // pointer to process security attributes
      nil, // pointer to thread security attributes
      True, // handle inheritance flag
      CreationFlags, // creation flags
      pEnv, // pointer to new environment block
      PChar(FWorkingDir), // pointer to current directory name
      StartupInfo, // pointer to STARTUPINFO
      FProcessInfo); // pointer to PROCESS_INF
    if Result then begin
      FStatus := esRunning;
    end else begin
      FLastError := GetLastError;
      if _RaiseException then begin
        RaiseLastOsErrorEx(FLastError, Format(_('%%1:s (%%0:d) in CreateProcess("%s", "%s")'), [FExeName, Cmdline]));
      end;
    end;
  finally
    StrDispose(pCmdLine);
    FreeAndNil(env);
  end;
end; // TAsyncExec.doExecute

class procedure TAsyncExec.Execute(const _Executable, _Parameters: string);
var
  Exe: TAsyncExec;
begin
  Exe := TAsyncExec.Create;
  try
    Exe.Exename := _Executable;
    Exe.Commandline := _Parameters;
    Exe.doExecute(True);
  finally
    FreeAndNil(Exe);
  end;
end;

function TAsyncExec.Execute(_RaiseException: Boolean = True): Boolean;
begin
  Result := doExecute(_RaiseException);
end;

procedure TAsyncExec.ResetStatus;
begin
  FStatus := esTerminated;
end;

function TAsyncExec.ReadTextFile(_st: TStream): string;
var
  sl: TStringList;
begin
  _st.Seek(0, soFromBeginning);
  sl := TStringList.Create;
  try
    sl.LoadFromStream(_st);
    Result := sl.Text;
  finally
    FreeAndNil(sl);
  end;
end;

procedure TAsyncExec.SetOnStdErrLine(_Value: TOnOuputLine);
begin
  FOnStdErrLine := _Value;
  IsStdErrRedirected := True;
end;

procedure TAsyncExec.SetOnStdOutLine(_Value: TOnOuputLine);
begin
  FOnStdOutLine := _Value;
  IsStdOutRedirected := True;
end;

function TAsyncExec.StdErr: string;
begin
  AssertStatus([esTerminated]);
  if not IsStdErrRedirected then
    raise ENotRedirected.Create(_('StdErr was not redirected'));

  FreeAndNil(FStdErrPipe);
  Result := FStdErrBuffer;
end;

function TAsyncExec.StdOut: string;
begin
  AssertStatus([esTerminated]);
  if not IsStdOutRedirected then
    raise ENotRedirected.Create(_('StdOut was not redirected'));

  FreeAndNil(FStdOutPipe);
  Result := FStdOutBuffer;
end;

// Auteur Thaddy de Koning
// adapted to unicode by twm

const
  // Terminator gedefinieerd als typed const
  // (Zoadat het een memory reference heeft)
  NullChar: Char = #0;

{ TEnvironmentBlockWriter }

procedure TEnvironmentBlockWriter.Add(const AValue: string);
// Is het blok gesloten?
// Zoja, ga een positie terug
// Schrijf de string
// Schrijf terminating #0
begin
  if FClosed then begin
    Seek(-1, soFromEnd);
    FClosed := False;
  end;
  WriteString(AValue);
  Write(NullChar, 1);
end;

procedure TEnvironmentBlockWriter.Add(const aToken, AValue: string);
begin
  Add(aToken + '=' + AValue);
end;

// Block afsluiten door een extra #0 te schrijven, als gespecificeerd

procedure TEnvironmentBlockWriter.Close;
begin
  if not FClosed then begin
    Write(NullChar, 1);
    FClosed := True;
  end;
end;

// Als we het block uitlezen nemen we aan dat het gesloten is!

function TEnvironmentBlockWriter.GetBlockPtr: PChar;
begin
  Close;
  Result := PChar(DataString);
end;

{ TEnvironmentBlockReader }

constructor TEnvironmentBlockReader.Create;
var
  FBlock: PChar;
  StrPtr: PChar;
begin
  inherited;
  FBlock := GetEnvironmentStrings;
  if FBlock <> nil then
    try
      StrPtr := FBlock;
      repeat
        Add(StrPtr);
        Inc(StrPtr, Succ(StrLen(StrPtr)));
      until StrPtr^ = #0;
    finally
      FreeEnvironmentStrings(FBlock);
    end;
end;

end.

