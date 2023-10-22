unit u_dzGuiTestRunner;

interface

uses
  TestFramework,
  GUITestRunner,
  u_dzUnitTestUtils;

type
  ///<summary>
  /// extends TGUITestRunner so it can display info messages in addition to failures </summary>
  TdzGuiTestRunner = class(TGUITestRunner, ITestListener)
  private
    procedure AddFailure(failure: TTestFailure);
  end;

procedure RunRegisteredTests;

implementation

uses
  ComCtrls,
  Forms;

procedure RunTest(test: ITest);
var
  Runner: TdzGuiTestRunner;
begin
  Runner := TdzGuiTestRunner.Create(nil);
  try
    Runner.Suite := test;
    Runner.ShowModal;
  finally
    Runner.Free;
  end;
end;

procedure RunRegisteredTests;
begin
  RunTest(registeredTests)
end;

type
  TProgressBarCrack = class(TProgressBar);

procedure TdzGuiTestRunner.AddFailure(failure: TTestFailure);
var
  ListItem: TListItem;
begin
  ListItem := AddFailureItem(failure);
  if failure.ThrownExceptionClass.InheritsFrom(ETestInfo) then
    ListItem.ImageIndex := imgRUN
  else begin
    ListItem.ImageIndex := imgERROR;
    if TestResult.ErrorCount = 0 then begin
      TProgressBarCrack(ScoreBar).Color := clFAILURE;
      TProgressBarCrack(ScoreBar).RecreateWnd;
    end;
    SetTreeNodeImage(TestToNode(failure.failedTest), imgFAILED);
    UpdateStatus(False);
  end;
end;

end.
