unit u_dzCmdLineParserStates;

{$INCLUDE 'dzlib.inc'}

interface

uses
  u_dzCmdLineParser,
  u_dzTranslator;

type
  TEngineStateAbstract = class(TInterfacedObject)
  private
    function GetClassName: string;
  end;

type
  TEngineStateError = class(TEngineStateAbstract, IEngineState)
  private
    FError: string;
    function Execute(const _Context: IEngineContext): IEngineState;
  public
    constructor Create(const _Error: string);
  end;

type
  TEngineStateStart = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateSpace = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateSpaceNoOptions = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateDash = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateDoubleDash = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateLongOption = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateShortOption = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateShortSwitch = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateShortParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateQuotedShortParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateLongParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateQuotedLongParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateExe = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateQuotedExe = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateQuotedParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

implementation

uses
  SysUtils,
  u_dzStringUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TEngineStateAbstract }

function TEngineStateAbstract.GetClassName: string;
begin
  Result := ClassName;
end;

{ TEngineStateError }

constructor TEngineStateError.Create(const _Error: string);
begin
  inherited Create;
  FError := _Error;
end;

function TEngineStateError.Execute(const _Context: IEngineContext): IEngineState;
begin
  raise EStateEngineError.Create(FError);
end;

{ TEngineStateStart }

function TEngineStateStart.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '-':
      Result := TEngineStateDash.Create;
    #0:
      Result := nil; // end state
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedExe.Create;
      end;
    ' ':
      Result := Self;
  else
    _Context.AddToParameter(c);
    Result := TEngineStateExe.Create;
  end;
end;

{ TEngineStateSpace }

function TEngineStateSpace.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '-':
      Result := TEngineStateDash.Create;
    #0:
      Result := nil; // end state
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedParam.Create;
      end;
    ' ':
      Result := Self;
  else
    _Context.AddToParameter(c);
    Result := TEngineStateParam.Create;
  end;
end;

{ TEngineStateSpaceNoOptions }

function TEngineStateSpaceNoOptions.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  // The difference to TEngineStateSpace is that TEngineStateSpaceNoOptions does not allow any
  // options. Everything must be a parameter.
  c := _Context.GetNextChar;
  case c of
    #0:
      Result := nil; // end state
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedParam.Create;
      end;
    ' ':
      Result := Self;
  else
    _Context.AddToParameter(c);
    Result := TEngineStateParam.Create;
  end;
end;

{ TEngineStateExe }

function TEngineStateExe.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedExe.Create;
      end;
    #0, ' ': begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
  else
    _Context.AddToParameter(c);
    Result := Self;
  end;
end;

{ TEngineStateQuotedExe }

function TEngineStateQuotedExe.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateExe.Create;
      end;
    #0:
      Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  else
    _Context.AddToParameter(c);
    Result := Self;
  end;
end;

{ TEngineStateParam }

function TEngineStateParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedParam.Create;
      end;
    #0, ' ': begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpaceNoOptions.Create;
      end;
  else
    _Context.AddToParameter(c);
    Result := Self;
  end;
end;

{ TEngineStateQuotedParam }

function TEngineStateQuotedParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateParam.Create;
      end;
    #0:
      Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  else
    _Context.AddToParameter(c);
    Result := Self;
  end;
end;

{ TEngineStateDash }

function TEngineStateDash.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  if CharInSet(c, ALPHANUMERIC_CHARS + ['?']) then begin
    _Context.AddToOption(c);
    Result := TEngineStateShortOption.Create;
  end else if c = '-' then
    Result := TEngineStateDoubleDash.Create
  else if c = ' ' then
    Result := TEngineStateSpaceNoOptions.Create
  else
    Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
end;

{ TEngineStateDoubleDash }

function TEngineStateDoubleDash.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  if CharInSet(c, ALPHANUMERIC_CHARS) then begin
    _Context.AddToOption(c);
    Result := TEngineStateLongOption.Create;
  end else
    Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
end;

{ TEngineStateShortOption }

function TEngineStateShortOption.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    ' ': begin
        Result := TEngineStateShortParam.Create;
      end;
    '-', '+': begin
        _Context.AddToParameter(c);
        Result := TEngineStateShortSwitch.Create;
      end;
    #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
  else
    Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  end;
end;

{ TEngineStateShortSwitch }

function TEngineStateShortSwitch.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    ' ', #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end else
    Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  end;
end;

{ TEngineStateShortParam }

function TEngineStateShortParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    ' ', #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedShortParam.Create;
      end;
    '-': begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateDash.Create;
      end;
  else
    _Context.AddToParameter(c);
    Result := Self;
  end;
end;

{ TEngineStateQuotedShortParam }

function TEngineStateQuotedShortParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateShortParam.Create;
      end;
    #0:
      Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  else
    _Context.AddToParameter(c);
    Result := Self;
  end;
end;

{ TEngineStateLongOption }

function TEngineStateLongOption.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '=':
      Result := TEngineStateLongParam.Create;
    ' ', #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
    '"', '''':
      Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  else
    _Context.AddToOption(c);
    Result := TEngineStateLongOption.Create;
  end;
end;

{ TEngineStateLongParam }

function TEngineStateLongParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedLongParam.Create;
      end;
    ' ', #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
  else
    _Context.AddToParameter(c);
    Result := TEngineStateLongParam.Create;
  end;
end;

{ TEngineStateQuotedLongParam }

function TEngineStateQuotedLongParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateLongParam.Create;
      end;
    #0:
      Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  else
    _Context.AddToParameter(c);
    Result := TEngineStateQuotedLongParam.Create;
  end;
end;

end.

