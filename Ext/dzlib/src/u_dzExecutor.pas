{.GXFormatter.config=twm}
{: Implements a wrapper object around the Win32 API CreateProcess and
   related functions.
   @author twm
}
unit u_dzExecutor deprecated;  // use u_dzAsyncExec for newer programs

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzFileStreams;

type
  {: ancestor to all exceptions raised in this unit }
  EExecutor = class(Exception);
  {: raised, when calling GetExitCode while Status is esInvalid }
  ENoProcess = class(EExecutor);
  {: raised, when calling GetStdOut or GetStdErr while Status is esRunning }
  EProcessRunning = class(EExecutor);
  {: raised, when trying to destroy the object while a process with I/O redirection
     is still running. }
  ERedirectedProcess = class(EProcessRunning);
  {: raised when calling TExecutor methods which do not work when the process
     has already terminated. }
  EProcessTerminated = class(EExecutor);
  {: raised when either StdOut or StdErr is accessed when RedirectStdXxx is not
     true }
  ENotRedirected = class(EExecutor);

type
  {: Status of the TExecutor object
     esInvalid = Process has not yet been started
     esRunning = Process is running
     esTerminated = Process has terminated }
  TExecutorStatus = (esInvalid, esRunning, esTerminated);
  TExecutorStatusSet = set of TExecutorStatus;

type
  {: Class wrapper around the Win32 functions for starting child processes.
     It allows to start a process, redirect stdIn/Out/Err, wait for it to finish,
     get the exit code and terminate it.
     Usage:<code>
       Executor := TExecutor.Create;
       Executor.WorkingDir := 's:\';
       Executor.Commandline := 'cvs -n -q update';
       Executor.Execute;
       Executor.Wait;
       MessageDlg(Format('StdOut: %s'#13#10+
                         'StdErr: %s', [Executor.StdOut, Executor.StdErr]),
                  mtInformation, [mbOK], 0);
       Executor.Free;
     </code>
  }
  TExecutor = class
  private
    FLastError: Integer;
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
    /// temporary file stream to be used as standard input for the process </summary>
    FInputFile: TdzTempFile;
    ///<summary>
    /// temporary file stream to be used as standard output for the process </summary>
    FOutputFile: TdzTempFile;
    ///<summary>
    /// temporary file stream to be used as standard error for the process </summary>
    FErrorFile: TdzTempFile;
    FProcessInfo: TProcessInformation;
    FStatus: TExecutorStatus;
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
    function GetStatus: TExecutorStatus;
    ///<summary>
    /// checks whether the current status is in the set passed and raises one of
    /// ENoProcess, EProcessRunning, EProcessTerminated if not.
    /// This method is called by various other methods to assert that their
    /// functions are allowed in the current Status.
    /// @param ValidStatusSet is a set of valid Status values for the check. </summary>
    procedure AssertStatus(_ValidStatusSet: TExecutorStatusSet; const _Method: string = '');
  public
    ///<summary>
    /// Creates a TExecutor object }
    constructor Create;
    ///<summary>
    /// Destroys a TExecutor object. If the Ststus is esRunning and the process
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
    procedure GetStdOut(_sl: TStrings);
    procedure GetStdErr(_sl: TStrings);
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
    /// @retruns True if StdIn is redirected </summary>
    function RedirectStdIn: Boolean; deprecated; // use IsStdInRedirected
    function IsStdInRedirected: Boolean;
    ///<summary>
    /// Set to true to redirect the process' standard output. This will result
    /// in having all text the process writes to its standard output handle
    /// copied to the StdOut property. </summary>
    property RedirectStdOut: Boolean read FIsStdOutRedirected write FIsStdOutRedirected; // deprecated, use IsStdOutRedirected
    property IsStdOutRedirected: Boolean read FIsStdOutRedirected write FIsStdOutRedirected;
    ///<summary>
    /// Set to true to redirect the process' standard output. This will result
    /// in having all text the process writes to its standard output handle
    /// copied to the StdOut property. </summary>
    property RedirectStdErr: Boolean read FIsStdErrRedirected write FIsStdErrRedirected; // deprecated, use IsStdErrRedirected
    property IsStdErrRedirected: Boolean read FIsStdErrRedirected write FIsStdErrRedirected;
    ///<summary>
    /// Set this to the standard input you want to supply to the process. </summary>
    property StdIn: AnsiString read FStdIn write FStdIn;
    ///<summary>
    /// After the process has terminated this contains the standard output.
    /// @raises either ENoProcess or EProcessRunning when accessed before the process has
    ///         terminated /summary>
    function StdOut: string;
    ///<summary> gets the StdOut stream </summary>
    function StdOutStream: TStream;
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
    /// Contains the status of a TExecutor which is
    /// * esInvalid when no process has been started,
    /// * esRunning while the process is still running and
    /// * esTerminated when the process has terminated. </summary>
    property Status: TExecutorStatus read GetStatus;
    ///<summary>
    /// Contains the working directory the process should be started in. If
    /// left empty the process will start in the current directory. </summary>
    property WorkingDir: string read FWorkingDir write FWorkingDir;
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

{ TExecutor }

constructor TExecutor.Create;
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

destructor TExecutor.Destroy;
begin
  inherited;
  if (Status = esRunning) and
    (IsStdInRedirected or IsStdOutRedirected or IsStdErrRedirected) then
    raise ERedirectedProcess.Create(_('Can not free Executor while a process using redirection is still running.'));
  FInputFile.Free;
  FOutputFile.Free;
  FErrorFile.Free;
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

function TExecutor.FindExecutable(_ExeName: string): Boolean;
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

procedure TExecutor.AssertStatus(_ValidStatusSet: TExecutorStatusSet; const _Method: string);
var
  Stat: TExecutorStatus;
begin
  Stat := GetStatus;
  if not (Stat in _ValidStatusSet) then
    case Stat of
      esInvalid: raise ENoProcess.CreateFmt(_('Process has not yet been started.'#13#10'%s'), [_Method]);
      esRunning: raise EProcessRunning.CreateFmt(_('Process is still running.'#13#10'%s'), [_Method]);
      esTerminated: raise EProcessTerminated.CreateFmt(_('Process has terminated.'#13#10'%s'), [_Method]);
    end;
end;

function TExecutor.GetStatus: TExecutorStatus;
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

function TExecutor.GetExitCode: DWORD;
begin
  AssertStatus([esRunning, esTerminated]);
  Result := FExitCode;
end;

function TExecutor.Wait(_Timeout: DWORD): DWORD;
begin
  AssertStatus([esRunning, esTerminated]);
  Result := WaitforSingleObject(ProcessHandle, _Timeout);
end;

function TExecutor.Kill: Boolean;
begin
  case GetStatus of
    esInvalid: raise ENoProcess.Create(_('Process has not yet been started'));
    esRunning: Result := TerminateProcess(FProcessInfo.hProcess, $FFFFFFFF);
  else
    Result := True;
  end;
end;

function TExecutor.GetProcessHandle: THandle;
begin
  AssertStatus([esRunning, esTerminated]);
  Result := FProcessInfo.hProcess;
end;

function TExecutor.GetThreadHandle: THandle;
begin
  AssertStatus([esRunning, esTerminated]);
  Result := FProcessInfo.hThread;
end;

function TExecutor.IsStdInRedirected: Boolean;
begin
  Result := FStdIn <> '';
end;

function TExecutor.RedirectStdIn: Boolean;
begin
  Result := IsStdInRedirected;
end;

function TExecutor.doExecute(_RaiseException: Boolean): Boolean;
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
    raise Exception.Create(_('Process has already been started.'));

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
    FInputFile := TdzTempFile.Create;
    try
      FInputFile.SecurityAttributes := @SecurityAttributes;
      FInputFile.Open;
      FInputFile.WriteBuffer(FStdIn[1], Length(FStdIn));
      FInputFile.Seek(0, soFromBeginning);
      StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESTDHANDLES;
      StartupInfo.hStdInput := FInputFile.Handle;
    except
      FInputFile.Free;
      FInputFile := nil;
      raise;
    end;
  end;

  if IsStdOutRedirected then begin
    FOutputFile := TdzTempFile.Create;
    FOutputFile.SecurityAttributes := @SecurityAttributes;
    FOutputFile.Open;
    StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESTDHANDLES;
    StartupInfo.hStdOutput := FOutputFile.Handle;
  end;

  if IsStdErrRedirected then begin
    FErrorFile := TdzTempFile.Create;
    FErrorFile.SecurityAttributes := @SecurityAttributes;
    FErrorFile.Open;
    StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESTDHANDLES;
    StartupInfo.hStdError := FErrorFile.Handle;
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
        RaiseLastOSErrorEx(FLastError, Format(_('%%1:s (%%0:d) in CreateProcess("%s", "%s")'), [FExeName, Cmdline]));
      end;
    end;
  finally
    StrDispose(pCmdLine);
    FreeAndNil(env);
  end;
end; // TExecutor.doExecute

class procedure TExecutor.Execute(const _Executable, _Parameters: string);
var
  Exe: TExecutor;
begin
  Exe := TExecutor.Create;
  try
    Exe.Exename := _Executable;
    Exe.Commandline := _Parameters;
    Exe.doExecute(True);
  finally
    FreeAndNil(Exe);
  end;
end;

function TExecutor.Execute(_RaiseException: Boolean = True): Boolean;
begin
  Result := doExecute(_RaiseException);
end;

procedure TExecutor.ResetStatus;
begin
  FStatus := esTerminated;
end;

function TExecutor.ReadTextFile(_st: TStream): string;
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

function TExecutor.StdErr: string;
begin
  AssertStatus([esTerminated]);
  if not IsStdErrRedirected then
    raise ENotRedirected.Create(_('StdErr was not redirected'));

  Result := ReadTextFile(FErrorFile);
end;

procedure TExecutor.GetStdErr(_sl: TStrings);
begin
  AssertStatus([esTerminated]);
  if not IsStdErrRedirected then
    raise ENotRedirected.Create(_('StdErr was not redirected'));

  FErrorFile.Seek(0, soFromBeginning);
  _sl.LoadFromStream(FErrorFile);
end;

function TExecutor.StdOut: string;
begin
  AssertStatus([esTerminated]);
  if not IsStdOutRedirected then
    raise ENotRedirected.Create(_('StdOut was not redirected'));

  Result := ReadTextFile(FOutputFile);
end;

procedure TExecutor.GetStdOut(_sl: TStrings);
begin
  AssertStatus([esTerminated]);
  if not IsStdOutRedirected then
    raise ENotRedirected.Create(_('StdOut was not redirected'));

  FOutputFile.Seek(0, soFromBeginning);
  _sl.LoadFromStream(FOutputFile);
end;

function TExecutor.StdOutStream: TStream;
begin
  AssertStatus([esTerminated]);
  if not IsStdOutRedirected then
    raise ENotRedirected.Create(_('StdOut was not redirected'));
  Result := FOutputFile;
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

