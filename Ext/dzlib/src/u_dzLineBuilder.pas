unit u_dzLineBuilder;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes;

type
  ///<summary> Helper class for building a text line </summary>
  TLineBuilder = class
  private
    FListSeparator: string;
    FContent: string;
{$IF Declared(TFormatSettings)}
    FFormatSettings: TFormatSettings;
{$ELSE}
    FDecimalSeparator: Char;
{$IFEND}
    FQuoteChar: Char;
    FColumnCount: Integer;
    FForceQuoted: Boolean;
    function GetDecimalSeparator: Char;
    procedure SetDecimalSeparator(_Value: Char);
  public
    ///<summary> Creates a TLineBuilder instance with the given separator
    ///          @param ListSeparator is the separator string to use, defaults to TAB (#9)
    ///          @param DecimalSeparator is the decimal separator to use for floating point
    ///                                  values, defaults to a dot (.). </summary>
    constructor Create(const _ListSeparator: string = #9; const _DecimalSeparator: Char = '.');
    ///<summary> Assigns the contents of another TLineBuilder instance </summary>
    procedure Assign(_Source: TLineBuilder);
    ///<summary> Adds a string column </summary>
    procedure Add(const _Column: string); overload;
    ///<summary> Adds a string column, putting it in quotes </summary>
    procedure AddQuoted(const _Column: string);
    ///<summary> Adds an integer value column </summary>
    procedure Add(_IntValue: Integer); overload;
    ///<summary> Adds a word value column </summary>
    procedure Add(_WordValue: Word); overload;
    ///<summary> Adds an integer value column </summary>
    procedure Add(_ShortIntValue: Shortint); overload;
    ///<summary> Adds a floating point value column</summary>
    procedure Add(_FloatValue: Extended); overload;
    ///<summary> Adds a floating point value column with the given number of decimals </summary>
    procedure Add(_FloatValue: Extended; _Decimals: Integer); overload;
    ///<summary> Adds a floating point value column with the given number of integer digits
    ///          and the given number of fractional digits </summary>
    procedure Add(_FloatValue: Extended; _IntDigits, _FracDigits: Integer); overload;
    ///<summary> Adds a column with a time in hh:mm:ss format </summary>
    procedure Add(_Hours, _Minutes, _Seconds: Integer); overload;
    ///<summary> Adds a column with a time in hh:mm:ss:tt format </summary>
    procedure Add(_Hours, _Minutes, _Seconds, _Hundredth: Integer); overload;
    ///<summary> Adds a boolean column, with 'Y' for true and 'N' for false </summary>
    procedure Add(_b: Boolean); overload;
    ///<summary> Adds a variant column, if it is a float, converts it to string using the configured DecimalSeparator </summary>
    procedure Add(_v: Variant); overload;
    ///<summary> Clears the line </summary>
    procedure Clear;
    ///<summary> Appends the contents of the given line </summary>
    procedure Append(_Line: TLineBuilder);
    ///<summary> Prepends the contents of the given line </summary>
    procedure Prepend(_Line: TLineBuilder);
    ///<summary> Extracts the first column from the line, returns false when empty </summary>
    function ExtractFirst(out _Column: string): Boolean;
    ///<summary> @returns the length of Content </summary>
    function Length: Integer;
    ///<summary> allows read access to the content that has been built </summary>
    property Content: string read FContent;
    ///<summary> Number of columns that have been added to this line </summary>
    property ColumnCount: Integer read FColumnCount;
    property DecimalSeparator: Char read GetDecimalSeparator write SetDecimalSeparator default '.';
    property ListSeparator: string read FListSeparator write FListSeparator;
    ///<summary> If set to true, every column will be enclosed in quotes </summary>
    property ForceQuoted: Boolean read FForceQuoted write FForceQuoted;
    property QuoteChar: Char read FQuoteChar write FQuoteChar;
{$IF Declared(TFormatSettings)}
    property FormatSettings: TFormatSettings read FFormatSettings;
{$IFEND}
  end;

implementation

uses
  StrUtils,
  Variants,
  u_dzVariantUtils,
  u_dzStringUtils;

{ TLineBuilder }

constructor TLineBuilder.Create(const _ListSeparator: string = #9; const _DecimalSeparator: Char = '.');
begin
  inherited Create;
  FListSeparator := _ListSeparator;
{$IF Declared(TFormatSettings)}
  FFormatSettings := GetUserDefaultLocaleSettings;
  FFormatSettings.DecimalSeparator := _DecimalSeparator;
  FFormatSettings.ThousandSeparator := #0;
{$ELSE}
  FDecimalSeparator := _DecimalSeparator;
{$IFEND}
  FQuoteChar := '"';
  FColumnCount := 0;
end;

function TLineBuilder.GetDecimalSeparator: Char;
begin
{$IF Declared(TFormatSettings)}
  Result := FFormatSettings.DecimalSeparator;
{$ELSE}
  Result := FDecimalSeparator;
{$IFEND}
end;

function TLineBuilder.Length: Integer;
begin
  Result := System.Length(FContent);
end;

procedure TLineBuilder.SetDecimalSeparator(_Value: Char);
begin
{$IF Declared(TFormatSettings)}
  FFormatSettings.DecimalSeparator := _Value;
{$ELSE}
  FDecimalSeparator := _Value;
{$IFEND}
end;

procedure TLineBuilder.Add(_IntValue: Integer);
begin
  Add(IntToStr(_IntValue));
end;

procedure TLineBuilder.Add(_WordValue: Word);
begin
  Add(IntToStr(_WordValue));
end;

procedure TLineBuilder.Add(_ShortIntValue: Shortint);
begin
  Add(IntToStr(_ShortIntValue));
end;

procedure TLineBuilder.Add(_FloatValue: Extended; _Decimals: Integer);
{$IF Declared(TFormatSettings)}
begin
  Add(FloatToStrF(_FloatValue, fffixed, 18, _Decimals, FFormatSettings));
end;
{$ELSE}
var
  SysDecimalSep: Char;
  SysThousandSep: Char;
begin
  SysDecimalSep := SysUtils.DecimalSeparator;
  SysThousandSep := SysUtils.ThousandSeparator;
  try
    Add(FloatToStrF(_FloatValue, fffixed, 18, _Decimals));
  finally
    SysUtils.DecimalSeparator := SysDecimalSep;
    SysUtils.ThousandSeparator := SysThousandSep;
  end;
end;
{$IFEND}

procedure TLineBuilder.Add(_FloatValue: Extended);
{$IF Declared(TFormatSettings)}
begin
  Add(FloatToStr(_FloatValue, FFormatSettings));
end;
{$ELSE}
var
  SysDecimalSep: Char;
  SysThousandSep: Char;
begin
  SysDecimalSep := SysUtils.DecimalSeparator;
  SysThousandSep := SysUtils.ThousandSeparator;
  try
    Add(FloatToStr(_FloatValue));
  finally
    SysUtils.DecimalSeparator := SysDecimalSep;
    SysUtils.ThousandSeparator := SysThousandSep;
  end;
end;
{$IFEND}

procedure TLineBuilder.Add(_FloatValue: Extended; _IntDigits, _FracDigits: Integer);
{$IF Declared(TFormatSettings)}
begin
  Add(Format('%*.*f', [_IntDigits, _FracDigits, _FloatValue], FFormatSettings));
end;
{$ELSE}
var
  SysDecimalSep: Char;
  SysThousandSep: Char;
begin
  SysDecimalSep := SysUtils.DecimalSeparator;
  SysThousandSep := SysUtils.ThousandSeparator;
  try
    Add(Format('%*.*f', [_IntDigits, _FracDigits, _FloatValue]));
  finally
    SysUtils.DecimalSeparator := SysDecimalSep;
    SysUtils.ThousandSeparator := SysThousandSep;
  end;
end;
{$IFEND}

procedure TLineBuilder.Add(const _Column: string);
var
  s: string;
begin
  if FColumnCount > 0 then
    FContent := FContent + FListSeparator;
  if FForceQuoted then
    s := FQuoteChar + _Column + FQuoteChar
  else
    s := _Column;
  FContent := FContent + s;
  Inc(FColumnCount);
end;

function ZeroPadLeft(_Value: Integer; _Len: Integer): string;
var
  s: AnsiString;
begin
  Str(_Value, s);
  Result := string(s);
  while Length(Result) < _Len do
    Result := '0' + Result;
end;

procedure TLineBuilder.Add(_Hours, _Minutes, _Seconds: Integer);
begin
  Add(ZeroPadLeft(_Hours, 2) + ':' + ZeroPadLeft(_Minutes, 2) + ':' + ZeroPadLeft(_Seconds, 2));
end;

procedure TLineBuilder.Add(_Hours, _Minutes, _Seconds, _Hundredth: Integer);
begin
  Add(ZeroPadLeft(_Hours, 2) + ':' + ZeroPadLeft(_Minutes, 2) + ':' + ZeroPadLeft(_Seconds, 2)
    + ':' + ZeroPadLeft(_Hundredth, 2));
end;

procedure TLineBuilder.Add(_b: Boolean);
begin
  Add(IfThen(_b, 'Y', 'N'));
end;

procedure TLineBuilder.Add(_v: Variant);
begin
  if VarIsFloat(_v) then
    Add(Var2ExtEx(_v, 'TLineBuilder.Add'))
  else
    Add(Var2Str(_v, ''));
end;

procedure TLineBuilder.AddQuoted(const _Column: string);
begin
  Add(FQuoteChar + _Column + FQuoteChar);
end;

procedure TLineBuilder.Append(_Line: TLineBuilder);
var
  s: string;
begin
  s := _Line.Content;
  if FColumnCount > 0 then
    FContent := FContent + FListSeparator + s
  else
    FContent := s;
  FColumnCount := FColumnCount + _Line.ColumnCount;
end;

procedure TLineBuilder.Assign(_Source: TLineBuilder);
begin
  FContent := _Source.Content;
  FColumnCount := _Source.ColumnCount;
end;

procedure TLineBuilder.Clear;
begin
  FContent := '';
  FColumnCount := 0;
end;

function TLineBuilder.ExtractFirst(out _Column: string): Boolean;
var
  p: Integer;
begin
  p := Pos(FListSeparator, FContent);
  Result := p <> 0;
  if Result then begin
    _Column := LeftStr(FContent, p - 1);
    FContent := TailStr(FContent, p + 1);
    Dec(FColumnCount);
  end;
end;

procedure TLineBuilder.Prepend(_Line: TLineBuilder);
var
  s: string;
begin
  s := _Line.Content;
  if FColumnCount > 0 then
    FContent := s + FListSeparator + FContent
  else
    FContent := s;
  FColumnCount := FColumnCount + _Line.ColumnCount;
end;

end.
