unit u_dzLogConsole;

interface

uses
  Windows,
  SysUtils,
  Classes,
  u_dzLogging,
  u_dzStdOut;

type
  TdzCustomLogConsole = class(TComponent)
  private
    FWarningLinesCount: Integer;
    FErrorLinesCount: Integer;
  protected
    FAutoShow: Boolean;
    FVisible: Boolean;
    FOwnConsole: Boolean;
    procedure SetVisible(const _Visible: Boolean);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    procedure LogCallback(const _s: string; _Level: TLogLevel);
    function ShowConsole: Boolean;
    procedure HideConsole;
    property AutoShow: Boolean read FAutoShow write FAutoShow default True;
    property Visible: Boolean read FVisible write SetVisible default False;
    property ErrorLinesCount: Integer read FErrorLinesCount;
    property WarningLinesCount: Integer read FWarningLinesCount;
  end;

  TdzLogConsole = class(TdzCustomLogConsole)
  published
    property AutoShow;
    property Visible;
  end;

implementation

uses
  u_dzDateUtils;

const
  ATTACH_PARENT_PROCESS = DWORD(-1);

function AttachConsole(ProcessId: DWORD): BOOL; stdcall; external 'kernel32.dll' name 'AttachConsole';

{ TdzCustomLogConsole }

constructor TdzCustomLogConsole.Create(_Owner: TComponent);
begin
  inherited;
  FVisible := IsConsole;
  FOwnConsole := False;
  FAutoShow := True;
end;

destructor TdzCustomLogConsole.Destroy;
begin
  try
    HideConsole;
  except
    // ignore exceptions
  end;
  inherited;
end;

function TdzCustomLogConsole.ShowConsole: Boolean;
begin
  if not FVisible then begin
    FVisible := AttachConsole(ATTACH_PARENT_PROCESS);
    if not FVisible then begin
      FVisible := AllocConsole;
      if FVisible then
        FOwnConsole := True;
    end;
    StdOut := TStdOut.Create;
  end;
  Result := FVisible;
end;

procedure TdzCustomLogConsole.LogCallback(const _s: string; _Level: TLogLevel);
var
  TextOutput: TStdOut.TColoredText;
  Marker: string;
begin
  if not FVisible then
    if FAutoShow then
      if not ShowConsole then
        Exit;
  if not FVisible then
    Exit;

  case _Level of
    llError: begin
        Marker := '!';
        TextOutput := StdOut.Error;
        Inc(FErrorLinesCount);
      end;
    llWarning: begin
        Marker := '+';
        TextOutput := StdOut.Warning;
        Inc(FWarningLinesCount);
      end;
    llInfo: begin
        Marker := '-';
        TextOutput := StdOut.Hint;
      end;
  else
    TextOutput := StdOut.default;
    Marker := ' ';
  end;
  TextOutput.WriteLn('%s%s %s', [Marker, DateTime2Iso(Now, True), _s]);
end;

procedure TdzCustomLogConsole.SetVisible(const _Visible: Boolean);
begin
  if FVisible = _Visible then
    Exit;
  if _Visible then
    ShowConsole
  else
    HideConsole;
end;

procedure TdzCustomLogConsole.HideConsole;
begin
  if not (FOwnConsole and FVisible) then
    Exit;
  FreeConsole;
  FreeAndNil(StdOut);
  FVisible := False;
  FOwnConsole := False;
end;

end.
