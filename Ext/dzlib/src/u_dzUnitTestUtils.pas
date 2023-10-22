/// <summary>
/// implements an extended TTextCase class
/// </summary>
unit u_dzUnitTestUtils;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2006_UP}
{$IFNDEF NO_DELPHI2006UP_HINT}
{$MESSAGE HINT 'Delphi <2006 not supported'}
{$ENDIF}
{$ELSE}

uses
  SysUtils,
{$IFDEF fpc}
  fpcunit,
  testutils,
  testregistry,
{$ENDIF}
{$IFDEF delphi}
  TestFramework,
{$ENDIF}
  Classes;

type
  ETestInfo = class(ETestFailure)
  end;

{$M+}
type
  ///<summary>
  /// Adds an option to display Info messages by calling the new method Info
  /// (requires using the u_dzGuiTestRunner instead of GuiTestRunner).
  /// Implements some additional CheckXxx procedures </summary>
  TdzTestCase = class(TTestcase, ITest)
  protected
    procedure RunTest(testResult: TTestResult); override;
  public
    ///<summary>
    /// Adds a non-error "failure" to the failure list </summary>
    procedure Info(const _Message: string);
    procedure CheckEqualsSingle(_Expected, _Actual: single; _Msg: string = '');
    procedure CheckEqualsDouble(_Expected, _Actual: double; _Msg: string = '');
    procedure CheckEqualsExtended(_Expected, _Actual: extended; _Msg: string = '');
    procedure CheckEqualsStringArray(const _Expected, _Actual: array of string);
    /// <summary>
    /// Checks whether the value is a variant of a floating point type (includes integer)
    /// </summary>
    procedure CheckVariantIsFloat(_Value: variant; _Msg: string = '');
    /// <summary>
    /// Checks whether the value is a variant of an (signed) integer type (excludes LongWord and Int64)
    /// </summary>
    procedure CheckVariantIsInteger(_Value: variant; _Msg: string = '');
    /// <summary>
    /// Checks whether the value is a variant of an (signed) int64 type (includes LongWord and Int64)
    /// </summary>
    procedure CheckVariantIsInt64(_Value: variant; _Msg: string = '');
    /// <summary>
    /// Checks whether the value is a variant of an (unsigned) integer type (excludes all signed integer types)
    /// </summary>
    procedure CheckVariantIsLongWord(_Value: variant; _Msg: string = '');
    /// <summary>
    /// Checks whether the value is a non Null variant
    /// </summary>
    procedure CheckVariantIsNotNull(_Value: variant; _Msg: string = '');
    /// <summary>
    /// Checks whether the value is a Null variant
    /// </summary>
    procedure CheckVariantIsNull(_Value: variant; _Msg: string = '');
    /// <summary>
    /// Checks whether the value is a variant of a string type
    /// </summary>
    procedure CheckVariantIsString(_Value: variant; _Msg: string = '');
    /// <summary>
    /// Called by the CheckVAriantIsXxxx functions to show errors
    /// </summary>
    procedure FailNotVarType(const _Expected, _Actual, _Msg: string; _ErrorAddr: pointer);
    /// <summary>
    /// Called by the FailNotVarType function to generate the error message
    /// </summary>
    function NotVarTypeErrorMessage(const _Expected: string; const _Actual: string; _Msg: string): string;
    /// <summary>
    /// Checks whether the date part of two TDateTime values is equal
    /// </summary>
    procedure CheckEqualsDate(_Expected, _Actual: TDateTime; const _Message: string = '');
    /// <summary>
    /// Checks whether two TDateTime values are equal
    /// </summary>
    procedure CheckEqualsDateTime(_Expected, _Actual: TDateTime; const _Message: string = '');
    /// <summary>
    /// Checks whether the time part of two TDateTime values is equal
    /// </summary>
    procedure CheckEqualsTime(_Expected, _Actual: TDateTime; const _Message: string = '');
    /// <summary>
    /// Checks multiline strings for equality
    /// </summary>
    procedure CheckEqualsMultiline(_Expected, _Actual: string; const _Message: string = '');
    /// <summary>
    /// Checks a multiline string against the content of a file
    /// </summary>
    procedure CheckEqualsFile(const _Filename: string; _Actual: string; const _Message: string = '');
    /// <summary>
    /// checks whether two files have got the same contents, content is treated as a multiline string
    /// </summary>
    procedure CheckEqualsFiles(const _ExpectedFile, _ActualFile: string; const _Message: string = '');
  end;
{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2006_UP}

uses
  Math,
  Variants,
  DateUtils,
  u_dzVariantUtils;

  { TdzTestCase }

{$IFNDEF DelphiXE2_up}
function ReturnAddress: pointer;
begin
  Result := CallerAddr;
end;
{$ENDIF DelphiXE2up}

procedure TdzTestCase.CheckVariantIsInt64(_Value: variant; _Msg: string);
begin
  if not VarIsInt64(_Value) then
    FailNotVarType('int64', VarTypeAsText(VarType(_Value)), _Msg, ReturnAddress);
end;

procedure TdzTestCase.CheckVariantIsInteger(_Value: variant; _Msg: string);
begin
  if not VarIsInteger(_Value) then
    FailNotVarType('integer', VarTypeAsText(VarType(_Value)), _Msg, ReturnAddress);
end;

procedure TdzTestCase.CheckVariantIsLongWord(_Value: variant; _Msg: string);
begin
  if not VarIsLongWord(_Value) then
    FailNotVarType('longword', VarTypeAsText(VarType(_Value)), _Msg, ReturnAddress);
end;

procedure TdzTestCase.CheckEqualsSingle(_Expected, _Actual: single; _Msg: string);
begin
  if not Math.SameValue(_Expected, _Actual) then
    FailNotEquals(FloatToStr(_Expected), FloatToStr(_Actual), _Msg, ReturnAddress);
end;

procedure TdzTestCase.CheckEqualsDouble(_Expected, _Actual: double; _Msg: string);
begin
  if not Math.SameValue(_Expected, _Actual) then
    FailNotEquals(FloatToStr(_Expected), FloatToStr(_Actual), _Msg, ReturnAddress);
end;

procedure TdzTestCase.CheckEqualsExtended(_Expected, _Actual: extended; _Msg: string);
begin
  if not Math.SameValue(_Expected, _Actual) then
    FailNotEquals(FloatToStr(_Expected), FloatToStr(_Actual), _Msg, ReturnAddress);
end;

procedure TdzTestCase.CheckEqualsDate(_Expected, _Actual: TDateTime; const _Message: string);
begin
  if not DateUtils.SameDate(_Expected, _Actual) then
    FailNotEquals(FloatToStr(_Expected), FloatToStr(_Actual), _Message, ReturnAddress);
end;

procedure TdzTestCase.CheckEqualsDateTime(_Expected, _Actual: TDateTime;
  const _Message: string);
begin
  if not DateUtils.SameDateTime(_Expected, _Actual) then
    FailNotEquals(FloatToStr(_Expected), FloatToStr(_Actual), _Message, ReturnAddress);
end;

procedure TdzTestCase.CheckEqualsStringArray(const _Expected, _Actual: array of string);
var
  i: Integer;
begin
  CheckEquals(Length(_Expected), Length(_Actual), 'length');
  for i := 0 to Length(_Expected) - 1 do
    CheckEquals(_Expected[i], _Actual[i], 'Index ' + IntToStr(i));
end;

procedure TdzTestCase.CheckEqualsFile(const _Filename: string; _Actual: string; const _Message: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(_Filename);
    CheckEqualsMultiline(sl.Text, _Actual, _Message);
  finally
    sl.Free;
  end;
end;

procedure TdzTestCase.CheckEqualsFiles(const _ExpectedFile, _ActualFile, _Message: string);
var
  sl: TStringList;
  Soll: string;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(_ExpectedFile);
    Soll := sl.Text;
    sl.Clear;
    sl.LoadFromFile(_ActualFile);
    CheckEqualsMultiline(Soll, sl.Text);
  finally
    sl.Free;
  end;
end;

procedure TdzTestCase.CheckEqualsMultiline(_Expected, _Actual: string; const _Message: string = '');
var
  ExpStrings: TStringList;
  ActStrings: TStringList;
  i: Integer;
begin
  ActStrings := nil;
  ExpStrings := TStringList.Create;
  try
    ActStrings := TStringList.Create;
    ExpStrings.Text := _Expected;
    ActStrings.Text := _Actual;
    CheckEquals(ExpStrings.Count, ActStrings.Count, _Message + ' (no. of lines does not match)');
    for i := 0 to ExpStrings.Count - 1 do
      CheckEquals(ExpStrings[i], ActStrings[i], _Message + Format(' (line %d does not match)', [i]));
  finally
    ActStrings.Free;
    ExpStrings.Free;
  end;
end;

procedure TdzTestCase.CheckEqualsTime(_Expected, _Actual: TDateTime;
  const _Message: string);
begin
  if not DateUtils.SameTime(_Expected, _Actual) then
    FailNotEquals(FloatToStr(_Expected), FloatToStr(_Actual), _Message, ReturnAddress);
end;

procedure TdzTestCase.CheckVariantIsFloat(_Value: variant; _Msg: string);
begin
  if not VarIsFloat(_Value) then
    FailNotVarType('float', VarTypeAsText(VarType(_Value)), _Msg, ReturnAddress);
end;

procedure TdzTestCase.CheckVariantIsNotNull(_Value: variant; _Msg: string);
begin
  if VarIsNull(_Value) then
    FailNotVarType('not null', 'null', _Msg, ReturnAddress);
end;

procedure TdzTestCase.CheckVariantIsNull(_Value: variant; _Msg: string);
begin
  if not VarIsNull(_Value) then
    FailNotVarType('null', VarTypeAsText(VarType(_Value)), _Msg, ReturnAddress);
end;

procedure TdzTestCase.CheckVariantIsString(_Value: variant; _Msg: string);
begin
  if not VarIsStr(_Value) then
    FailNotVarType('string', VarTypeAsText(VarType(_Value)), _Msg, ReturnAddress);
end;

procedure TdzTestCase.FailNotVarType(const _Expected, _Actual, _Msg: string; _ErrorAddr: pointer);
begin
  Fail(NotVarTypeErrorMessage(_Expected, _Actual, _Msg), _ErrorAddr);
end;

function TdzTestCase.NotVarTypeErrorMessage(const _Expected: string; const _Actual: string; _Msg: string): string;
begin
  if (_Msg <> '') then
    _Msg := _Msg + ', ';
  Result := Format('%sexpected variant type: <%s> but was: <%s>', [_Msg, _Expected, _Actual])
end;

procedure TdzTestCase.Info(const _Message: string);
begin
  raise ETestInfo.Create(_Message);
end;

procedure TdzTestCase.RunTest(testResult: TTestResult);
begin
  try
    inherited RunTest(testResult);
  except
    on e: ETestInfo do begin
      testResult.AddFailure(Self, e, ExceptAddr);
    end;
  end;
end;

{$ENDIF DELPHI2007_UP}

end.
