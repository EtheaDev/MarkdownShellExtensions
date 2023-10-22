unit u_dzParallelSort;

interface

uses
  SysUtils,
  Classes,
  SyncObjs,
  u_dzSortUtils,
  u_dzTypes,
  u_dzTranslator;

type
  EStackCapacityExhausted = class(EdzException);

procedure ParallelQuicksortPlus(_Left, _Right: Integer;
  _CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth;
  _Cutoff: Integer = 15; _MaxThreads: Integer = 4); overload;

procedure RegisterUnitTests;

implementation

uses
  TestFramework,
  u_dzUnitTestUtils,
  u_dzErrorThread,
  u_dzErrorThreadList,
  u_dzMiscUtils,
  u_dzCriticalSection,
  u_dzInsertionSort;

function _(const _s: string): string; inline;
begin
  Result := dzlibGetText(_s);
end;

type
  TWorkPackagesStack = class
  private
    FItems: array of record
      FLeft, FRight: Integer;
    end;
    FCritSect: TdzCriticalSection;
    FStackPtr: Integer;
  public
    constructor Create(_Capacity: Integer);
    destructor Destroy; override;
    function Depth: Integer;
    procedure Push(_Left, _Right: Integer);
    function TryPop(out _Left, _Right: Integer): Boolean;
    procedure Lock;
    procedure Unlock;
  end;

type
  TWorkerThread = class(TErrorThread)
  private
    FCompareMeth: TCompareItemsMeth;
    FSwapMeth: TSwapItemsMeth;
    FWorkPackages: TWorkPackagesStack;
    FCutoff: Integer;
    FIsBusy: Boolean;
  protected
    procedure doQuicksort(_Left, _Right: Integer);
    procedure doExecute; override;
  public
    constructor Create(_CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth;
      _Cutoff: Integer; _WorkPackages: TWorkPackagesStack);
    property IsBusy: Boolean read FIsBusy;
  end;

type
  TWorkerThreadList = class(TErrorThreadList)
  public
    function AtLeastOneBusy: Boolean;
    procedure TerminateAll;
  end;

procedure ParallelQuicksortPlus(_Left, _Right: Integer;
  _CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth;
  _Cutoff: Integer = 15; _MaxThreads: Integer = 4);
var
  WorkerThreads: TWorkerThreadList;
  WorkPackages: TWorkPackagesStack;
  i: Integer;
begin
  if _Left >= _Right then
    Exit; //==>

  InitializeNil(WorkPackages, WorkerThreads);
  try
    WorkPackages := TWorkPackagesStack.Create(50);
    WorkPackages.Push(_Left, _Right);
    WorkerThreads := TWorkerThreadList.Create;
    for i := 1 to _MaxThreads do
      WorkerThreads.Add(TWorkerThread.Create(_CompareMeth, _SwapMeth, _Cutoff, WorkPackages));
    while (WorkPackages.Depth > 0) or WorkerThreads.AtLeastOneBusy do
      Sleep(1);
    WorkerThreads.TerminateAll;
    while WorkerThreads.StillRunning > 0 do
      Sleep(1);
  finally
    FreeAndNil(WorkerThreads, WorkPackages);
  end;
end;

{ TWorkPackagesList }

constructor TWorkPackagesStack.Create(_Capacity: Integer);
begin
  inherited Create;
  FCritSect := TdzCriticalSection.Create;
  FCritSect.Enter;
  try
    Setlength(FItems, _Capacity);
  finally
    FCritSect.Leave;
  end;
  FStackPtr := 0;
end;

destructor TWorkPackagesStack.Destroy;
begin
  FreeAndNil(FCritSect);
  inherited;
end;

procedure TWorkPackagesStack.Push(_Left, _Right: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FItems);
  if FStackPtr = Capacity then
    Setlength(FItems, Capacity + 10);
  FItems[FStackPtr].FLeft := _Left;
  FItems[FStackPtr].FRight := _Right;
  Inc(FStackPtr);
end;

function TWorkPackagesStack.Depth: Integer;
begin
  Result := FStackPtr;
end;

function TWorkPackagesStack.TryPop(out _Left, _Right: Integer): Boolean;
begin
  Lock;
  try
    Result := (FStackPtr > 0);
    if Result then begin
      Dec(FStackPtr);
      _Left := FItems[FStackPtr].FLeft;
      _Right := FItems[FStackPtr].FRight;
    end;
  finally
    Unlock;
  end;
end;

procedure TWorkPackagesStack.Lock;
begin
  FCritSect.Enter;
end;

procedure TWorkPackagesStack.Unlock;
begin
  FCritSect.Leave;
end;

{ TWorkerThread }

constructor TWorkerThread.Create(_CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth;
  _Cutoff: Integer; _WorkPackages: TWorkPackagesStack);
begin
  FCompareMeth := _CompareMeth;
  FSwapMeth := _SwapMeth;
  FWorkPackages := _WorkPackages;
  FCutoff := _Cutoff;
  inherited Create(False);
end;

procedure TWorkerThread.doExecute;
var
  Left: Integer;
  Right: Integer;
begin
  FIsBusy := FWorkPackages.TryPop(Left, Right);
  while FIsBusy or not Terminated do begin
    if FIsBusy then
      doQuicksort(Left, Right)
    else
      Sleep(1);
    FIsBusy := FWorkPackages.TryPop(Left, Right);
  end;
end;

procedure TWorkerThread.doQuicksort(_Left, _Right: Integer);
var
  i: Integer;
  j: Integer;
  p: Integer;
begin
  if _Right - _Left < FCutoff then begin
    InsertionSort(_Left, _Right, FCompareMeth, FSwapMeth);
  end else begin
    i := _Left;
    j := _Right;
    p := GetPivot(i, j, FCompareMeth);
    repeat
      while FCompareMeth(i, p) < 0 do
        Inc(i);
      while FCompareMeth(j, p) > 0 do
        Dec(j);
      if i <= j then begin
        if i < j then
          FSwapMeth(i, j);
        if p = i then
          p := j
        else if p = j then
          p := i;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    FWorkPackages.Lock;
    try
      if _Left < j then
        FWorkPackages.Push(_Left, j);
      if i < _Right then
        FWorkPackages.Push(i, _Right);
    finally
      FWorkPackages.Unlock;
    end;
  end;
end;

type
  TestWorkPackagesRing = class(TdzTestCase)
  private
    FRing: TWorkPackagesStack;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmpty;
    procedure TestPushPop;
    procedure TestPushError;
  end;

procedure RegisterUnitTests;
begin
  RegisterTest('dzParallelSort', TestWorkPackagesRing.Suite);
end;

{ TestWorkPackagesRing }

procedure TestWorkPackagesRing.SetUp;
begin
  FRing := TWorkPackagesStack.Create(5);
end;

procedure TestWorkPackagesRing.TearDown;
begin
  FreeAndNil(FRing);
end;

procedure TestWorkPackagesRing.TestPushError;
begin
  FRing.Push(0, 1);
  CheckEquals(1, FRing.Depth, 'Depth');
  FRing.Push(2, 3);
  CheckEquals(2, FRing.Depth, 'Depth');
  FRing.Push(4, 5);
  CheckEquals(3, FRing.Depth, 'Depth');
  FRing.Push(6, 7);
  CheckEquals(4, FRing.Depth, 'Depth');
  FRing.Push(8, 9);
  CheckEquals(5, FRing.Depth, 'Depth');
  StartExpectingException(EStackCapacityExhausted);
  FRing.Push(10, 11);
  StopExpectingException;
  CheckEquals(6, FRing.Depth, 'Depth');
end;

procedure TestWorkPackagesRing.TestPushPop;
var
  Left: Integer;
  Right: Integer;
begin
  FRing.Push(0, 1);
  CheckEquals(1, FRing.Depth, 'Depth');
  CheckEquals(True, FRing.TryPop(Left, Right), 'TryGet');
  CheckEquals(0, Left, 'Left');
  CheckEquals(1, Right, 'Right');
  CheckEquals(0, FRing.Depth, 'Depth');
end;

procedure TestWorkPackagesRing.TestEmpty;
var
  Left: Integer;
  Right: Integer;
begin
  CheckEquals(0, FRing.Depth, 'Depth');
  CheckEquals(False, FRing.TryPop(Left, Right), 'TryGet');
end;

{ TWorkerThreadList }

function TWorkerThreadList.AtLeastOneBusy: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do begin
    if (Items[i] as TWorkerThread).IsBusy then
      Exit;
  end;
  Result := False;
end;

procedure TWorkerThreadList.TerminateAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Terminate;
end;

end.
