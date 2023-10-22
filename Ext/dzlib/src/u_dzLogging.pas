{.GXFormatter.config=twm}
{: Implements a logging interface which can be used in a procedural or OO way }
unit u_dzLogging;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes;

{$WARN SYMBOL_DEPRECATED OFF}

type
  {: supported log levels }
  TLogLevel = (llDump, llTrace, llDebug, llInfo, llWarning, llError);

type
  {: used for callbacks }
  TOnLog = procedure(const _s: string; _Level: TLogLevel) of object;

type
  ILogTrackMethod = interface ['{296AA9F4-599D-4338-849A-E648B1CD8E1E}']
    procedure Log(const _s: string; _Level: TLogLevel = llDump); // alternative to LogLine
    procedure Error(const _s: string); // alternative to LogError
    procedure Warning(const _s: string); // alternative to LogWarning
    procedure Info(const _s: string); // alternative to LogInfo
    procedure Debug(const _s: string); // alternative to LogDebug
    procedure Dump(const _s: string); // alternative to LogDump
  end;

type
  ILogger = interface ['{BE5D915E-C384-4648-9272-621981A48AED}']
    function LogLevel2Str(_Level: TLogLevel): string;
    procedure LogLine(const _s: string; _Level: TLogLevel = llDump); deprecated; // use Log method instead
    procedure LogException(_e: Exception; const _Where: string = ''; _IncludeCallstack: Boolean = True);
    procedure LogCallstack(_Level: TLogLevel);
    procedure LogError(const _s: string); deprecated; // use Error method instead
    procedure LogWarning(const _s: string); deprecated; // use Warning method instead
    procedure LogInfo(const _s: string); deprecated; // use Info method instead
    procedure LogDebug(const _s: string); deprecated; // use Debug method instead
    procedure LogTrace(const _s: string); deprecated; // use Trace method instead
    procedure LogDump(const _s: string); deprecated; // use Dump method instaed
    procedure LogProcEnter(const _Proc: string = ''); deprecated;
    procedure LogProcExit(const _Proc: string = ''); deprecated;
    procedure SetCallback(_Callback: TOnLog);
    // the following methods are now preferred because they are easier to use with code completion:
    procedure Log(const _s: string; _Level: TLogLevel = llDump); // alternative to LogLine
    procedure Error(const _s: string); // alternative to LogError
    procedure Warning(const _s: string); // alternative to LogWarning
    procedure Info(const _s: string); // alternative to LogInfo
    procedure Debug(const _s: string); // alternative to LogDebug
    procedure Trace(const _s: string); // alternative to LogTrace
    procedure Dump(const _s: string); // alternative to LogDump
    procedure MethodEnter(const _s: string = ''); // alternative to LogProcEnter
    procedure MethodLeave(const _s: string = ''); // alternative to LogProcExit
    function MethodTrack(const _Proc: string = ''): ILogTrackMethod;
    procedure ProcessEnter(const _s: string);
    procedure ProcessLeave(const _s: string);
    procedure ThreadEnter(const _s: string);
    procedure ThreadLeave(const _s: string);

    function CreateSession(const _SessionName: string): ILogger;
  end;

type
  {: An abstract ancestor that can be used as an ancestor for classes implementing
     the ILogger interface. Descendants then only need to override the Log method }
  TAbstractLogger = class(TInterfacedObject, ILogger)
  protected
    FCallback: TOnLog;
    procedure doCallback(const _s: string; _Level: TLogLevel);
  protected // implements ILogger
    function LogLevel2Str(_Level: TLogLevel): string;
    // the following methods are now preferred because they are easier to use with code completion:
    ///<summary> This is the main logging method, all other methods eventually call Log
    ///          In this implementation it does nothing but call doCallback which calls the registered
    ///          callback event.
    ///          Descendant classes should write to the log first and then call inherited. </summary>
    procedure Log(const _s: string; _Level: TLogLevel = llDump); virtual;
    procedure Error(const _s: string);
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
    procedure Warning(const _s: string);
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
    procedure Info(const _s: string);
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
    procedure Debug(const _s: string);
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
    procedure Trace(const _s: string);
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
    procedure Dump(const _s: string);
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
    procedure MethodEnter(const _s: string = ''); virtual;
    procedure MethodLeave(const _s: string = ''); virtual;

    {: Logs a line
       @param s is the string to log
       @param Level is the log level, defaults to llDump }
    procedure LogLine(const _s: string; _Level: TLogLevel = llDump); virtual; deprecated; // use Log method instead
    procedure LogException(_e: Exception; const _Where: string = ''; _IncludeCallstack: Boolean = True);
    procedure LogCallstack(_Level: TLogLevel);
    {: Logs a string at error level }
    procedure LogError(const _s: string); deprecated; // use Error method instead
    {: Logs a string at warning level }
    procedure LogWarning(const _s: string); deprecated; // use Warning method instead
    {: Logs a string at info level }
    procedure LogInfo(const _s: string); deprecated; // use Info method instead
    {: Logs a string at debug level }
    procedure LogDebug(const _s: string); deprecated; // use Debug method instead
    {: Logs a string at trace level }
    procedure LogTrace(const _s: string); deprecated; // use Trace method instead
    {: Logs a string at dump level }
    procedure LogDump(const _s: string); deprecated; // use Dump method instead
    {: Logs the entry into a procedure }
    procedure LogProcEnter(const _Proc: string = ''); deprecated; // use MethodEnter method instead (or MethodTrack)
    {: Logs the exit from a procedure }
    procedure LogProcExit(const _Proc: string = ''); deprecated; // use MethodLeave method instead (or MethodTrack)

    function MethodTrack(const _Proc: string = ''): ILogTrackMethod; virtual;

    procedure ProcessEnter(const _s: string); virtual;
    procedure ProcessLeave(const _s: string); virtual;

    procedure ThreadEnter(const _s: string); virtual;
    procedure ThreadLeave(const _s: string); virtual;

    procedure SetCallback(_Callback: TOnLog); virtual;
    function CreateSession(const _SessionName: string): ILogger; virtual;
  end;

type
  {: Implements the ILogger interface by sending all logging calls to an event handler }
  TEventLogger = class(TAbstractLogger, ILogger)
  private
    {: OnLogLine event, called from the LogLine method }
    FOnLog: TOnLog;
    {: used internally when Disconnect is called }
    procedure LogIgnore(const _s: string; _Level: TLogLevel);
  protected
    {: calls the FOnLogLine event }
    procedure Log(const _s: string; _Level: TLogLevel = llDump); override;
  public
    {: Default constructor, creates a TEventLogger instance and sets the
       FOnLog field }
    constructor Create(_OnLog: TOnLog);
    {: disconnects the event by changing the FOnLogLine field to the LogLineIgnore method }
    procedure Disconnect;
  end;

type
  {: Dummy implementation for the ILogger interface that ignores everything written to it
     An instance of this class is automatically created and assigned to the gblLogger
     variable. }
  TNoLogging = class(TAbstractLogger, ILogger)
    // This is just an alias for TAbstractLogger, it does not do any logging but calls
    // the callback method.
  end;

type
  TFileLogger = class(TAbstractLogger, ILogger)
  private
    FFilename: string;
    FFileOpen: Boolean;
    FFile: TextFile;
    function OpenFile(const _Filename: string): Boolean;
  protected
    procedure Log(const _s: string; _Level: TLogLevel = llDump); override;
    function CreateSession(const _SessionName: string): ILogger; override;
  public
    constructor Create(const _Filename: string);
    destructor Destroy; override;
  end;

{: procedureal interface for calling the gblLogger.LogLine method }
procedure LogLine(const _s: string; _Level: TLogLevel = llDump);
{: procedureal interface for calling the gblLogger.LogError method }
procedure LogError(const _s: string);
{: procedureal interface for calling the gblLogger.LogException method }
procedure LogException(_e: Exception; const _Where: string = ''; _IncludeCallstack: Boolean = True);
{: procedureal interface for calling the gblLogger.LogWarning method }
procedure LogWarning(const _s: string);
{: procedureal interface for calling the gblLogger.LogInfo method }
procedure LogInfo(const _s: string);
{: procedureal interface for calling the gblLogger.LogDebug method }
procedure LogDebug(const _s: string);
{: procedureal interface for calling the gblLogger.LogTrace method }
procedure LogTrace(const _s: string);
{: procedureal interface for calling the gblLogger.LogDump method }
procedure LogDump(const _s: string);
{: procedureal interface for calling the gblLogger.LogProcEnter method }
procedure LogProcEnter(const _Proc: string = '');
{: procedureal interface for calling the gblLogger.LogProcExit method }
procedure LogProcExit(const _Proc: string = '');
{: procedureal interface for calling the gblLogger.MethodTrack method }
function MethodTrack(const _Proc: string = ''): ILogTrackMethod;

function GetGlobalLogger: ILogger;
procedure SetGlobalLogger(_Logger: ILogger);

type
  TOnGetCallstack = procedure(_sl: TStrings);
const
  gblOnGetCallstack: TOnGetCallstack = nil;
  gblOnGetCurrentCallstack: TOnGetCallstack = nil;

implementation

uses
  Windows,
  u_dzFileUtils;

var
  {: Global ILogger interface, automtically initialized to a TNoLogging class }
  gblLogger: ILogger = nil;

procedure SetGlobalLogger(_Logger: ILogger);
begin
  if _Logger = nil then
    gblLogger := TNoLogging.Create
  else
    gblLogger := _Logger;
end;

function GetGlobalLogger: ILogger;
begin
  Result := gblLogger; // TRedirectToGlobalLogger.Create;
end;

procedure LogLine(const _s: string; _Level: TLogLevel = llDump);
begin
  gblLogger.LogLine(_s, _Level);
end;

procedure LogError(const _s: string);
begin
  gblLogger.LogError(_s);
end;

procedure LogException(_e: Exception; const _Where: string = ''; _IncludeCallstack: Boolean = True);
begin
  gblLogger.LogException(_e, _Where, _IncludeCallstack);
end;

procedure LogWarning(const _s: string);
begin
  gblLogger.LogWarning(_s);
end;

procedure LogInfo(const _s: string);
begin
  gblLogger.LogInfo(_s);
end;

procedure LogDebug(const _s: string);
begin
  gblLogger.LogDebug(_s);
end;

procedure LogTrace(const _s: string);
begin
  gblLogger.LogTrace(_s);
end;

procedure LogDump(const _s: string);
begin
  gblLogger.LogDump(_s);
end;

procedure LogProcEnter(const _Proc: string = '');
begin
  gblLogger.MethodEnter(_Proc);
end;

procedure LogProcExit(const _Proc: string = '');
begin
  gblLogger.MethodLeave(_Proc);
end;

function MethodTrack(const _Proc: string = ''): ILogTrackMethod;
begin
  Result := gblLogger.MethodTrack(_Proc);
end;

type
  TLogProcEnterExit = class(TInterfacedObject, ILogTrackMethod)
  private
    FProc: string;
    FLogger: ILogger;
  private // ILogTrackMethod
    procedure Log(const _s: string; _Level: TLogLevel = llDump); // alternative to LogLine
    procedure Error(const _s: string); // alternative to LogError
    procedure Warning(const _s: string); // alternative to LogWarning
    procedure Info(const _s: string); // alternative to LogInfo
    procedure Debug(const _s: string); // alternative to LogDebug
    procedure Dump(const _s: string); // alternative to LogDump
  public
    constructor Create(_Logger: ILogger; const _Proc: string);
    destructor Destroy; override;
  end;

{ TLogProcEnterExit }

constructor TLogProcEnterExit.Create(_Logger: ILogger; const _Proc: string);
begin
  inherited Create;
  FLogger := _Logger;
  FProc := _Proc;
end;

destructor TLogProcEnterExit.Destroy;
begin
  FLogger.MethodLeave(FProc);
  inherited;
end;

procedure TLogProcEnterExit.Log(const _s: string; _Level: TLogLevel);
begin
  FLogger.Log(FProc + ': ' + _s, _Level);
end;

procedure TLogProcEnterExit.Debug(const _s: string);
begin
  Log(_s, llDebug);
end;

procedure TLogProcEnterExit.Dump(const _s: string);
begin
  Log(_s, llDump);
end;

procedure TLogProcEnterExit.Error(const _s: string);
begin
  Log(_s, llError);
end;

procedure TLogProcEnterExit.Info(const _s: string);
begin
  Log(_s, llInfo);
end;

procedure TLogProcEnterExit.Warning(const _s: string);
begin
  Log(_s, llWarning);
end;

{ TAbstractLogger }

procedure TAbstractLogger.Log(const _s: string; _Level: TLogLevel);
begin
  doCallback(_s, _Level);
end;

procedure TAbstractLogger.LogDebug(const _s: string);
begin
  LogLine(_s, llDebug);
end;

procedure TAbstractLogger.LogDump(const _s: string);
begin
  LogLine(_s, llDump);
end;

procedure TAbstractLogger.LogError(const _s: string);
begin
  LogLine(_s, llError);
end;

procedure TAbstractLogger.LogException(_e: Exception; const _Where: string; _IncludeCallstack: Boolean);
var
  s: string;
  sl: TStringList;
  i: Integer;
begin
  if _Where <> '' then
    s := _Where + ': '
  else
    s := '';
  LogError(s + _e.ClassName + ': ' + _e.Message);
  if _IncludeCallstack and Assigned(gblOnGetCallstack) then begin
    sl := TStringList.Create;
    try
      LogDebug('<begin call stack>');
      gblOnGetCallstack(sl);
      for i := 0 to sl.Count - 1 do
        LogDebug(sl[i]);
      LogDebug('<end call stack>');
    finally
      sl.Free;
    end;
  end;
end;

procedure TAbstractLogger.LogCallstack(_Level: TLogLevel);
var
  sl: TStringList;
  i: Integer;
begin
  if Assigned(gblOnGetCurrentCallstack) then begin
    sl := TStringList.Create;
    try
      Log('<begin call stack>', _Level);
      gblOnGetCurrentCallstack(sl);
      for i := 0 to sl.Count - 1 do
        Log(sl[i], _Level);
      Log('<end call stack>', _Level);
    finally
      sl.Free;
    end;
  end;
end;

function TAbstractLogger.CreateSession(const _SessionName: string): ILogger;
begin
  // The default implementation does not distinguish between sessions
  // override this, to create a different ILogger instance for each session
  Result := Self;
end;

procedure TAbstractLogger.Debug(const _s: string);
begin
  Self.Log(_s, llDebug);
end;

procedure TAbstractLogger.doCallback(const _s: string; _Level: TLogLevel);
begin
  if Assigned(FCallback) then
    FCallback(_s, _Level);
end;

procedure TAbstractLogger.Dump(const _s: string);
begin
  Self.Log(_s, llDump);
end;

procedure TAbstractLogger.Error(const _s: string);
begin
  Self.Log(_s, llError);
end;

procedure TAbstractLogger.Info(const _s: string);
begin
  Self.Log(_s, llInfo);
end;

procedure TAbstractLogger.LogInfo(const _s: string);
begin
  LogLine(_s, llInfo);
end;

function TAbstractLogger.LogLevel2Str(_Level: TLogLevel): string;
begin
  case _Level of
    llDump: Result := 'Dump';
    llTrace: Result := 'Trace';
    llDebug: Result := 'Debug';
    llInfo: Result := 'Info';
    llWarning: Result := 'Warning';
    llError: Result := 'Error';
  end;
end;

procedure TAbstractLogger.LogLine(const _s: string; _Level: TLogLevel);
begin
  Self.Log(_s, _Level);
end;

procedure TAbstractLogger.LogProcEnter(const _Proc: string);
begin
  MethodEnter(_Proc);
end;

procedure TAbstractLogger.LogProcExit(const _Proc: string);
begin
  MethodLeave(_Proc);
end;

procedure TAbstractLogger.LogTrace(const _s: string);
begin
  LogLine(_s, llTrace);
end;

procedure TAbstractLogger.LogWarning(const _s: string);
begin
  LogLine(_s, llWarning);
end;

procedure TAbstractLogger.MethodEnter(const _s: string);
begin
  LogLine('>>' + _s, llTrace);
end;

procedure TAbstractLogger.MethodLeave(const _s: string);
begin
  LogLine('<<' + _s, llTrace);
end;

function TAbstractLogger.MethodTrack(const _Proc: string): ILogTrackMethod;
begin
  MethodEnter(_Proc);
  Result := TLogProcEnterExit.Create(Self as ILogger, _Proc);
end;

procedure TAbstractLogger.ProcessEnter(const _s: string);
begin
  Log('EnterProcess: ' + _s);
end;

procedure TAbstractLogger.ProcessLeave(const _s: string);
begin
  Log('ExitProcess: ' + _s);
end;

procedure TAbstractLogger.SetCallback(_Callback: TOnLog);
begin
  FCallback := _Callback;
end;

procedure TAbstractLogger.ThreadEnter(const _s: string);
begin
  Log('EnterThread: ' + _s);
end;

procedure TAbstractLogger.ThreadLeave(const _s: string);
begin
  Log('ExitThread: ' + _s);
end;

procedure TAbstractLogger.Trace(const _s: string);
begin
  Self.Log(_s, llTrace);
end;

procedure TAbstractLogger.Warning(const _s: string);
begin
  Self.Log(_s, llWarning);
end;

{ TEventLogger }

constructor TEventLogger.Create(_OnLog: TOnLog);
begin
  inherited Create;
  FOnLog := _OnLog;
end;

procedure TEventLogger.Disconnect;
begin
  FOnLog := LogIgnore;
end;

procedure TEventLogger.Log(const _s: string; _Level: TLogLevel);
begin
  // FOnLog is always assigned since disconnect sets it to LogIngore rather than nil
  FOnLog(_s, _Level);
end;

procedure TEventLogger.LogIgnore(const _s: string; _Level: TLogLevel);
begin //FI:W519
  // ignore
end;

{ TFileLogger }

constructor TFileLogger.Create(const _Filename: string);
var
  Filename: string;
begin
  inherited Create;
  Filename := _Filename;
  FFileOpen := OpenFile(Filename);
  if not FFileOpen then begin
    // alternatively try to write to %TEMP%
    Filename := ExtractFileName(Filename);
    FFileOpen := OpenFile(itpd(TFileSystem.GetTempPath) + Filename);
    // if even that fails, give up, we just run without logging
  end;
end;

destructor TFileLogger.Destroy;
begin
  if FFileOpen then
    CloseFile(FFile);
  inherited;
end;

procedure TFileLogger.Log(const _s: string; _Level: TLogLevel);
var
  SystemTime: TSystemTime;
  s: string;
begin
  if FFileOpen then begin
    GetLocalTime(SystemTime);
    with SystemTime do
      s := Format('%.2d:%.2d:%.2d:%.3d', [wHour, wMinute, wSecond, wMilliSeconds]);
    WriteLn(FFile, s + #9 + IntToStr(Ord(_Level)) + #9 + _s);
    Flush(FFile);
  end;
end;

function TFileLogger.OpenFile(const _Filename: string): Boolean;
var
  dir: string;
begin
  try
    dir := ExcludeTrailingPathDelimiter(ExtractFilePath(_Filename));
    if (dir <> '') then
      ForceDirectories(dir);

    AssignFile(FFile, _Filename);
    if FileExists(_Filename) then
      Append(FFile)
    else
      Rewrite(FFile);
    Result := True;
    FFilename := _Filename;
  except
    Result := False;
  end;
end;

function TFileLogger.CreateSession(const _SessionName: string): ILogger;
var
  Ext: string;
  Filename: string;
begin
  Ext := ExtractFileExt(FFilename);
  Filename := ChangeFileExt(FFilename, '');
  Filename := Filename + '_' + _SessionName;
  Filename := ChangeFileExt(Filename, Ext);
  Info('Opening log session "' + _SessionName + '" to file "' + Filename + '"');
  Result := TFileLogger.Create(Filename);
end;

initialization
  gblLogger := TNoLogging.Create;
end.

