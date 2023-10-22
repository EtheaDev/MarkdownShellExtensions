unit u_dzStdOut;

// If set, the StdOut variable will not automatically be initialized.
// Use this for programs that are not compiled as a console program but
// call AllocConsole to create a console window. Don't forget to initialize
// the variable in that case. Note that the variable will always be freed
// in the unit finalization.
{.$DEFINE NO_STDOUT_AUTO_INIT}

interface

uses
  Windows,
  SysUtils,
  u_dzTranslator;

type
  TConsoleColors = (
    ccBlack, ccBlue, ccGreen, ccCyan, ccRed, ccMagenta, ccBrown, ccLightGray,
    ccDarkGray, ccLightBlue, ccLightGreen, ccLightCyan, ccLightRed, ccLightMagenta, ccYellow, ccWhite);

type
  TStdOut = class
  public
    type
      TColoredText = record
      private
        FStdOut: TStdOut;
        FTextColor: TConsoleColors;
        procedure Init(_StdOut: TStdOut; _TextColor: TConsoleColors);
      public
        procedure Write(const _Text: string); overload;
        procedure Write(const _Format: string; const _Params: array of const); overload;
        procedure WriteLn(const _Text: string); overload;
        procedure WriteLn(const _Format: string; const _Params: array of const); overload;
        procedure Pause(const _Msg: string);
      end;
  private
    FConsoleTextAttr: Byte;
  public
    Error: TColoredText;
    Warning: TColoredText;
    Hint: TColoredText;
    Success: TColoredText;
    Default: TColoredText;
    constructor Create(_ErrorColor: TConsoleColors = ccLightRed;
      _WarningColor: TConsoleColors = ccYellow;
      _HintColor: TConsoleColors = ccWhite;
      _SuccessColor: TConsoleColors = ccLightGreen;
      _DefaultColor: TConsoleColors = ccLightGray);
    destructor Destroy; override;
    ///</summary>
    /// @returns the file handle of the OUTPUT file </summary>
    function StdOutHandle: Integer;
    ///<summary>
    /// @returns the current text attributes </summary>
    function GetTextAttr: Byte;
    ///<summary>
    /// Sets the text atrributes and returns the previous value </summary>
    function SetTextAttr(_NewAttr: Byte): Byte;
    ///<summary>
    /// @returns the current text color </summary>
    function GetTextColor: TConsoleColors;
    ///<summary>
    /// Sets the text color and returns the previous value </summary>
    function SetTextColor(_Value: TConsoleColors): TConsoleColors;
    ///<summary>
    /// @returns the current background color </summary>
    function GetBackgroundColor: TConsoleColors;
    ///<summary>
    /// Sets the background color and returns the previous value </summary>
    function SetBackgroundColor(_Value: TConsoleColors): TConsoleColors;
    ///<summary>
    /// Sets text and background colors and returns the previous TextAttr value.
    /// The result can be passed to SetTextAttr to reset the colors to the previous value. </summary>
    function SetColors(_TextColor, _BackgroundColor: TConsoleColors): Byte;
    ///<summary>
    /// Changes the colors back to the defaults:
    /// Text color = DefaultColor (as given in the constructor which in turn defaults to ccLightGray)
    /// Background color = ccBlack </summary>
    procedure ResetColors;

    ///<summary>
    /// Writes text with the currently selected colors </summary>
    procedure Write(const _Text: string); overload;
    ///<summary>
    /// short for Write(Format(_Format, _Parms)) </summary>
    procedure Write(const _Format: string; const _Params: array of const); overload;
    ///<summary>
    /// Writes text with the given text color and the currently selected background color. </summary>
    procedure Write(_TextColor: TConsoleColors; const _Text: string); overload;
    ///<summary>
    /// short for Write(_TextColor, Format(_Format, _Parms)) </summary>
    procedure Write(_TextColor: TConsoleColors; const _Format: string; const _Params: array of const); overload;
    ///<summary>
    /// Writes text with the given colors. </summary>
    procedure Write(_TextColor, _BackgroundColor: TConsoleColors; const _Text: string); overload;
    ///<summary>
    /// short for Write(_TextColor, _BackgroundColor, Format(_Format, _Parms)) </summary>
    procedure Write(_TextColor: TConsoleColors; _BackgroundColor: TConsoleColors;
      const _Format: string; const _Params: array of const); overload;

    ///<summary>
    /// Writes text follwed by CR/LF with the currently selected colors </summary>
    procedure WriteLn(const _Text: string); overload;
    ///<summary>
    /// short for WriteLn(Format(_Format, _Parms)) </summary>
    procedure WriteLn(const _Format: string; const _Params: array of const); overload;
    ///<summary>
    /// Writes text follwed by CR/LF with the given color and the currently selected background color. </summary>
    procedure WriteLn(_TextColor: TConsoleColors; const _Text: string); overload;
    ///<summary>
    /// short for WriteLn(_TextColor, Format(_Format, _Parms)) </summary>
    procedure WriteLn(_TextColor: TConsoleColors; const _Format: string; const _Params: array of const); overload;
    ///<summary>
    /// Writes text follwed by CR/LF with the given colors. </summary>
    procedure WriteLn(_TextColor, _BackgroundColor: TConsoleColors; const _Text: string); overload;
    ///<summary>
    /// short for WriteLn(_TextColor, _BackgroundColor, Format(_Format, _Parms)) </summary>
    procedure WriteLn(_TextColor, _BackgroundColor: TConsoleColors;
      const _Format: string; const _Params: array of const); overload;
    ///<summary>
    /// Write the given message, followed by 'Press <enter>' and waits for the user to press enter </summary>
    procedure Pause(const _Msg: string = ''); overload;
    procedure Pause(_TextColor: TConsoleColors; const _Msg: string = ''); overload;
    procedure Pause(_TextColor, _BackgroundColor: TConsoleColors; const _Msg: string = ''); overload;
  end;

var
  StdOut: TStdOut = nil;

implementation

uses
  u_dzMiscUtils;

function _(_s: string): string;
begin
  Result := dzlibGetText(_s);
end;

{ TStdOut }

constructor TStdOut.Create(_ErrorColor: TConsoleColors = ccLightRed;
  _WarningColor: TConsoleColors = ccYellow;
  _HintColor: TConsoleColors = ccWhite;
  _SuccessColor: TConsoleColors = ccLightGreen;
  _DefaultColor: TConsoleColors = ccLightGray);
var
  BufferInfo: TConsoleScreenBufferInfo;
  LastError: DWORD;
begin
  inherited Create;

  Rewrite(Output);
  if not GetConsoleScreenBufferInfo(StdOutHandle, BufferInfo) then begin
    LastError := GetLastError;
    RaiseLastOsErrorEx(LastError, _('%1:s (%0:d) calling GetConsoleScreenBuffer'));
  end;
//  FConsoleTextAttr := BufferInfo.wAttributes and $FF;
  SetColors(_DefaultColor, ccBlack);

  Error.Init(Self, _ErrorColor);
  Warning.Init(Self, _WarningColor);
  Hint.Init(Self, _HintColor);
  Success.Init(Self, _SuccessColor);
  Default.Init(Self, _DefaultColor);
end;

destructor TStdOut.Destroy;
begin
  inherited;
end;

function TStdOut.GetBackgroundColor: TConsoleColors;
begin
  Result := TConsoleColors((FConsoleTextAttr and $F0) shr 4);
end;

function TStdOut.GetTextAttr: Byte;
begin
  Result := FConsoleTextAttr;
end;

function TStdOut.GetTextColor: TConsoleColors;
begin
  Result := TConsoleColors(FConsoleTextAttr and $0F);
end;

procedure TStdOut.Pause(_TextColor: TConsoleColors; const _Msg: string);
begin
  if _Msg <> '' then
    Self.Write(_TextColor, _Msg + ' ');
  Self.Write(_TextColor, 'Press <Enter>');
  System.Readln;
end;

procedure TStdOut.Pause(_TextColor, _BackgroundColor: TConsoleColors; const _Msg: string);
begin
  if _Msg <> '' then
    Self.Write(_TextColor, _BackgroundColor, _Msg + ' ');
  Self.Write(_TextColor, _BackgroundColor, 'Press <Enter>');
  System.Readln;
end;

procedure TStdOut.ResetColors;
begin
  SetColors(Default.FTextColor, ccBlack);
end;

procedure TStdOut.Pause(const _Msg: string = '');
begin
  if _Msg <> '' then
    Write(_Msg + ' ');
  Self.Write('Press <Enter>');
  System.Readln;
end;

function TStdOut.SetBackgroundColor(_Value: TConsoleColors): TConsoleColors;
begin
  Result := GetBackgroundColor;
  SetColors(GetTextColor, _Value);
end;

function TStdOut.SetColors(_TextColor, _BackgroundColor: TConsoleColors): Byte;
begin
  Result := SetTextAttr((Byte(Ord(_BackgroundColor)) shl 4) or (Byte(Ord(_TextColor) and $0F)));
end;

function TStdOut.SetTextAttr(_NewAttr: Byte): Byte;
begin
  Result := FConsoleTextAttr;
  FConsoleTextAttr := _NewAttr;
  SetConsoleTextAttribute(StdOutHandle, FConsoleTextAttr);
end;

function TStdOut.SetTextColor(_Value: TConsoleColors): TConsoleColors;
begin
  Result := GetTextColor;
  SetColors(_Value, GetBackgroundColor);
end;

function TStdOut.StdOutHandle: Integer;
begin
  Result := TTextRec(Output).Handle;
end;

procedure TStdOut.Write(const _Text: string);
begin
  System.Write(Output, _Text);
end;

procedure TStdOut.Write(const _Format: string; const _Params: array of const);
begin
  Self.Write(Format(_Format, _Params));
end;

procedure TStdOut.Write(_TextColor: TConsoleColors; _BackgroundColor: TConsoleColors;
  const _Text: string);
var
  PrevAttr: Byte;
begin
  PrevAttr := SetColors(_TextColor, _BackgroundColor);
  Self.Write(_Text);
  SetTextAttr(PrevAttr);
end;

procedure TStdOut.Write(_TextColor: TConsoleColors; _BackgroundColor: TConsoleColors;
  const _Format: string; const _Params: array of const);
begin
  Self.Write(_TextColor, _BackgroundColor, Format(_Format, _Params));
end;

procedure TStdOut.Write(_TextColor: TConsoleColors; const _Text: string);
begin
  Self.Write(_TextColor, GetBackgroundColor, _Text);
end;

procedure TStdOut.Write(_TextColor: TConsoleColors; const _Format: string; const _Params: array of const);
begin
  Self.Write(_TextColor, Format(_Format, _Params));
end;

procedure TStdOut.WriteLn(_TextColor, _BackgroundColor: TConsoleColors; const _Text: string);
var
  PrevAttr: Byte;
begin
  PrevAttr := SetColors(_TextColor, _BackgroundColor);
  Self.WriteLn(_Text);
  SetTextAttr(PrevAttr);
end;

procedure TStdOut.WriteLn(_TextColor, _BackgroundColor: TConsoleColors;
  const _Format: string; const _Params: array of const);
begin
  Self.WriteLn(_TextColor, _BackgroundColor, Format(_Format, _Params));
end;

procedure TStdOut.WriteLn(const _Text: string);
begin
  System.WriteLn(Output, _Text);
end;

procedure TStdOut.WriteLn(const _Format: string; const _Params: array of const);
begin
  Self.WriteLn(Format(_Format, _Params));
end;

procedure TStdOut.WriteLn(_TextColor: TConsoleColors; const _Text: string);
var
  LastColor: TConsoleColors;
begin
  LastColor := SetTextColor(_TextColor);
  Self.WriteLn(_Text);
  SetTextColor(LastColor);
end;

procedure TStdOut.WriteLn(_TextColor: TConsoleColors; const _Format: string; const _Params: array of const);
begin
  Self.WriteLn(_TextColor, Format(_Format, _Params));
end;

{ TStdOut.TColoredText }

procedure TStdOut.TColoredText.Init(_StdOut: TStdOut; _TextColor: TConsoleColors);
begin
  FStdOut := _StdOut;
  FTextColor := _TextColor;
end;

procedure TStdOut.TColoredText.Pause(const _Msg: string);
begin
  FStdOut.Pause(FTextColor, ccBlack, _Msg);
end;

procedure TStdOut.TColoredText.Write(const _Format: string; const _Params: array of const);
begin
  FStdOut.Write(FTextColor, ccBlack, _Format, _Params);
end;

procedure TStdOut.TColoredText.Write(const _Text: string);
begin
  FStdOut.Write(FTextColor, ccBlack, _Text);
end;

procedure TStdOut.TColoredText.WriteLn(const _Text: string);
begin
  FStdOut.WriteLn(FTextColor, ccBlack, _Text);
end;

procedure TStdOut.TColoredText.WriteLn(const _Format: string; const _Params: array of const);
begin
  FStdOut.WriteLn(FTextColor, ccBlack, _Format, _Params);
end;

initialization
{$IFNDEF NO_STDOUT_AUTO_INIT}
  StdOut := TStdOut.Create;
{$ENDIF}
finalization
  FreeAndNil(StdOut);
end.
