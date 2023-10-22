unit u_dzTextTestRunner;

interface

uses
  DUnitConsts,
  TestFramework,
  TextTestRunner,
  u_dzUnitTestUtils;

type
  ///<summary>
  /// extends TGUITestRunner so it can display info messages in addition to failures </summary>
  TdzTextTestListener = class(TTextTestListener, ITestListener)
  public
    procedure AddFailure(failure: TTestFailure); override;
  end;

function RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;

implementation

function RunTest(suite: ITest; exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;
begin
  Result := TestFramework.RunTest(suite, [TdzTextTestListener.Create]);
  case exitBehavior of
    rxbPause:
      try
        writeln(sPressReturn);
        readln
      except
      end;
    rxbHaltOnFailures:
      with Result do begin
        if not WasSuccessful then
          System.Halt(ErrorCount + FailureCount);
      end
  end;
end;

function RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;
begin
  Result := RunTest(registeredTests, exitBehavior);
end;

{ TdzTextTestListener }

procedure TdzTextTestListener.AddFailure(failure: TTestFailure);
begin
  write('I');
end;

end.
