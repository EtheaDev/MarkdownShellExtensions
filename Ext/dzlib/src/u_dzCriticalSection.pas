unit u_dzCriticalSection;

{$INCLUDE 'dzlib.inc'}

{.$DEFINE DEBUG_CRIT_SECT}

{$IFNDEF DEBUG}
{$UNDEF DEBUG_CRIT_SECT}
{$ENDIF}

interface

uses
  Windows,
  SyncObjs,
  u_dzTranslator;

type
  TdzCriticalSection = class(TCriticalSection)
  private
{$IFDEF DEBUG_CRIT_SECT}
    FLockCount: Integer;
    FOwner: Integer;
{$ENDIF DEBUG_CRIT_SECT}
  public
    class function NewInstance: TObject; override;
{$IFDEF DEBUG_CRIT_SECT}
    procedure Acquire; override;
    procedure Release; override;
{$ENDIF DEBUG_CRIT_SECT}
  end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
type
{$ALIGN ON}
  PdzRTLCriticalSection = ^TdzRTLCriticalSection;
  TdzRTLCriticalSection = record
  private
    DebugInfo: PRTLCriticalSectionDebug;
    LockCount: Longint;
    RecursionCount: Longint;
    OwningThread: THandle;
    LockSemaphore: THandle;
    // Apparently the following field contains the SpinCount value (passed or dynamically generated)
    // ORed with one of the RTL_CRITICAL_SECTION_FLAG_xxxx flags (see below).
    // Note that there is no official documentation on it.
    Reserved: DWORD;
  public
    ///<summary>
    /// Allocates a TdzRtlCriticalSection on the heap and initializes it
    /// @param SpinCount gives the maximum number of spin loops before the thread waits.
    ///                  default is 0 which uses the system default (which apparently is 2000)
    /// @param WithDebugInfo determines whether a critical section is created with
    ///                      debug info. Default is True which is also the system default </summary>
    class function AllocAndInit(_SpinCount: DWORD; _WithDebugInfo: Boolean): PdzRTLCriticalSection; overload; static;
    class function AllocAndInit(_SpinCount: DWORD = 0): PdzRTLCriticalSection; overload; static;
    ///<summary>
    /// DeInits a cricital section that has been allocated on the using AllocAndInit
    /// and frees the memory. </summary>
    procedure DeInitAndFree;
    ///<summary>
    /// calls InitializeCriticalSection or InitializeCriticalSectionEx for this TdzRTLCriticalSection
    /// @param SpinCount gives the maximum number of spin loops before the thread waits.
    ///                  default is 0 which uses the system default.
    /// @param WithDebugInfo determines whether a critical section is created with
    ///                      debug info. If not given, the system default is used, which apparently
    ///                      is True for Windows 7 and False for Windows 10 (don't know about
    ///                      Windows 8) </summary>
    procedure Init(_SpinCount: DWORD; _WithDebugInfo: Boolean); overload;
    procedure Init(_SpinCount: DWORD = 0); overload;
    ///<summary>
    /// calls FreeCriticalSection for this TdzRTLCriticalSection </summary>
    procedure DeInit;
    ///<summary>
    /// calls SetCriticalSectionSpinCount for this TdzRTLCriticalSection
    /// @param SpinCount is the new spin count
    /// @returns the previous SpinCount of this TdzRTLCriticalSection
    /// NOTE: Even though there is no official way to read the spin count, it is possible to get
    ///       it by calling this method twice:
    ///       1. set the SpinCount to an arbitrary value
    ///       2. call it again to set it back to the old value returned by the first call </summary>
    function SetSpinCount(_SpinCount: DWORD): DWORD;
    ///<summary>
    /// calls TryEnterCriticalSection for this TdzRTLCriticalSection </summary>
    function TryEnter: Boolean;
    ///<summary>
    /// calls EnterCriticalSection for this TdzRTLCriticalSection </summary>
    procedure Enter;
    ///<summary>
    /// calls LeaveCriticalSection for this TdzRTLCriticalSection </summary>
    procedure Leave;
{$IFDEF DEBUG_CRIT_SECT}
    type
      TCritSectFlags = (csfNoDebugInfo, csfDynamicSpin, csfStaticInit, csfResourceType, csfForceDebugInfo);
      TCritSectFlagSet = set of TCritSectFlags;
    function GetSpinCountFromReserved: DWORD;
    function GetFlagsFromReserved: TCritSectFlagSet;
{$ENDIF}
  end;
{$ENDIF SUPPORTS_ENHANCED_RECORDS}

const
{$IF not declared(CRITICAL_SECTION_NO_DEBUG_INFO)}
  CRITICAL_SECTION_NO_DEBUG_INFO = $01000000;
{$IFEND}
  // These flags are not documented but defined in winnt.h and can be passed to InitializeCriticalSectionEx.
  // Also, apparently Windows 10 was changed to default to CRITICAL_SECTION_NO_DEBUG_INFO
  // source: https://stackoverflow.com/a/53089288/49925
  RTL_CRITICAL_SECTION_FLAG_DYNAMIC_SPIN = $02000000;
  RTL_CRITICAL_SECTION_FLAG_STATIC_INIT = $04000000;
  RTL_CRITICAL_SECTION_FLAG_RESOURCE_TPE = $08000000;
  RTL_CRITICAL_SECTION_FLAG_FORCE_DEBUG_INFO = $10000000;

implementation

uses
  u_dzMiscUtils,
  u_dzOsUtils,
  u_dzTypes;

var
  CacheLineSize: Integer;

{ TdzCriticalSection }

class function TdzCriticalSection.NewInstance: TObject;
// see
// http://delphitools.info/2011/11/30/fixing-tcriticalsection/
// for an explanation why this could speed up execution on multi core systems
// NOTE: I can't see any positive effect in my tests (see tests\SpinLockTest).
//       On the contrary there seems to be a negative effect on some CPUs.
//       It's probably not worth it.
var
  InstSize: Integer;
begin
  InstSize := InstanceSize;
  if InstSize < CacheLineSize then
    InstSize := CacheLineSize;
  Result := InitInstance(GetMemory(InstSize));
end;

{$IFDEF DEBUG_CRIT_SECT}

procedure TdzCriticalSection.Acquire;
begin
  InterlockedIncrement(FLockCount);
  inherited;
  FOwner := GetCurrentThreadId;
end;

procedure TdzCriticalSection.Release;
begin
  inherited;
  if InterlockedDecrement(FLockCount) < 0 then
    Assert(FLockCount < 10);
end;
{$ENDIF DEBUG_CRIT_SECT}

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
{ TdzRTLCriticalSection }

type
  TInitializeCriticalSectionEx = function(lpCriticalSection: PRTLCriticalSection; _SpinCount: DWORD;
    _Flags: DWORD): BOOL; stdcall;

var
  InitializeCriticalSectionEx: TInitializeCriticalSectionEx = nil;

procedure TdzRTLCriticalSection.Init(_SpinCount: DWORD; _WithDebugInfo: Boolean);
var
  Flags: DWORD;
begin
  if _WithDebugInfo or (_SpinCount <> 0) then begin
    if _WithDebugInfo then
      Flags := RTL_CRITICAL_SECTION_FLAG_FORCE_DEBUG_INFO
    else
      Flags := CRITICAL_SECTION_NO_DEBUG_INFO;
    if Assigned(InitializeCriticalSectionEx) then
      InitializeCriticalSectionEx(PRTLCriticalSection(@Self), _SpinCount, Flags)
    else
      InitializeCriticalSectionAndSpinCount(TRTLCriticalSection(Self), _SpinCount)
  end else
    InitializeCriticalSection(TRTLCriticalSection(Self));
end;

procedure TdzRTLCriticalSection.Init(_SpinCount: DWORD);
begin
  InitializeCriticalSectionAndSpinCount(TRTLCriticalSection(Self), _SpinCount)
end;

class function TdzRTLCriticalSection.AllocAndInit(_SpinCount: DWORD;
  _WithDebugInfo: Boolean): PdzRTLCriticalSection;
begin
  GetMem(Result, SizeOf(TdzCriticalSection));
  Result.Init(_SpinCount, _WithDebugInfo);
end;

class function TdzRTLCriticalSection.AllocAndInit(_SpinCount: DWORD): PdzRTLCriticalSection;
begin
  GetMem(Result, SizeOf(TdzCriticalSection));
  Result.Init(_SpinCount);
end;

procedure TdzRTLCriticalSection.DeInitAndFree;
var
  SelfPtr: PdzRTLCriticalSection;
begin
  DeInit;
  SelfPtr := @Self;
  FreeMem(SelfPtr);
end;

procedure TdzRTLCriticalSection.DeInit;
begin
  DeleteCriticalSection(TRTLCriticalSection(Self));
end;

procedure TdzRTLCriticalSection.Enter;
begin
  EnterCriticalSection(TRTLCriticalSection(Self));
end;

procedure TdzRTLCriticalSection.Leave;
begin
  LeaveCriticalSection(TRTLCriticalSection(Self));
end;

function TdzRTLCriticalSection.SetSpinCount(_SpinCount: DWORD): DWORD;
begin
  Result := SetCriticalSectionSpinCount(TRTLCriticalSection(Self), _SpinCount);
end;

function TdzRTLCriticalSection.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(TRTLCriticalSection(Self));
end;

procedure TryInitInitializeCriticalSectionEx;
var
  HKernel32: HModule;
begin
  HKernel32 := GetModuleHandle(kernel32);
  InitializeCriticalSectionEx := GetProcAddress(HKernel32, 'InitializeCriticalSectionEx');
end;

{$IFDEF DEBUG_CRIT_SECT}
function TdzRTLCriticalSection.GetFlagsFromReserved: TCritSectFlagSet;
begin
  Result := [];
  if (Reserved and CRITICAL_SECTION_NO_DEBUG_INFO) <> 0 then
    Include(Result, csfNoDebugInfo);
  if (Reserved and RTL_CRITICAL_SECTION_FLAG_DYNAMIC_SPIN) <> 0 then
    Include(Result, csfDynamicSpin);
  if (Reserved and RTL_CRITICAL_SECTION_FLAG_STATIC_INIT) <> 0 then
    Include(Result, csfStaticInit);
  if (Reserved and RTL_CRITICAL_SECTION_FLAG_RESOURCE_TPE) <> 0 then
    Include(Result, csfResourceType);
  if (Reserved and RTL_CRITICAL_SECTION_FLAG_FORCE_DEBUG_INFO) <> 0 then
    Include(Result, csfForceDebugInfo);
end;

function TdzRTLCriticalSection.GetSpinCountFromReserved: DWORD;
begin
  Result := Reserved and $00FFFFFF;
end;

procedure TestTdzRtlCriticalSection;
var
  cs: TdzRTLCriticalSection;
  Flags: TdzRTLCriticalSection.TCritSectFlagSet;
  Spin: DWORD;
begin
  cs.Init(5);
  try
    Assert(cs.GetSpinCountFromReserved = 5);
    Flags := cs.GetFlagsFromReserved;
    Assert(Flags = []);

    // This seems to disable spinning
    Spin := cs.SetSpinCount(0);
    Assert(Spin = 5);
    Assert(cs.GetSpinCountFromReserved = 0);
    Flags := cs.GetFlagsFromReserved;
    Assert(Flags = []);
  finally
    cs.DeInit;
  end;

// this does not work under Windows XP
//  cs.Init;
//  try
//    Assert(cs.GetSpinCountFromReserved = 2000);
//    Flags := cs.GetFlagsFromReserved;
//    Assert(Flags = [csfDynamicSpin]);

    // Apparently setting the spin count does not clear the csfDynamicSpin flag,
    // so it's questionable whether it is possible whether setting the spin count
    // actually has any effect at all.
//    Spin := cs.SetSpinCount(5);
//    Assert(Spin = 2000);
//    Assert(cs.GetSpinCountFromReserved = 5);
//    Flags := cs.GetFlagsFromReserved;
//    Assert(Flags = [csfDynamicSpin]);
//
//    Spin := cs.SetSpinCount(0);
//    Assert(Spin = 5);
//    Assert(cs.GetSpinCountFromReserved = 0);
//    Flags := cs.GetFlagsFromReserved;
//    Assert(Flags = [csfDynamicSpin]);
//  finally
//    cs.DeInit;
//  end;

  cs.Init(5, True);
  try
    Assert(cs.GetSpinCountFromReserved = 5);
    Assert(Uintptr(cs.DebugInfo) <> $FFFFFFFF);
    Flags := cs.GetFlagsFromReserved;
    Assert(Flags = []);
  finally
    cs.DeInit;
  end;

// this does not work under Windows XP
//  cs.Init(0, True);
//  try
//    Assert(cs.GetSpinCountFromReserved = 2000);
//    Assert(Uintptr(cs.DebugInfo) <> $FFFFFFFF);
//    Flags := cs.GetFlagsFromReserved;
//    Assert(Flags = [csfDynamicSpin]);
//  finally
//    cs.DeInit;
//  end;
end;
{$ENDIF}

// This procedure only exists to prevent compiler warnings for the private fields.
// It should be eliminated automatically by the linker.
procedure DummyAccessToCsFields;
var
  cs: TdzRTLCriticalSection;
begin
  cs.DebugInfo := nil;
  cs.LockCount := 0;
  cs.RecursionCount := 0;
  cs.OwningThread := 0;
  cs.LockSemaphore := 0;
  cs.Reserved := 0;
end;
{$ENDIF SUPPORTS_ENHANCED_RECORDS}

initialization
  CacheLineSize := GetCpuCacheLineSize;
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
  Assert(SizeOf(TdzRTLCriticalSection) = SizeOf(TRTLCriticalSection));
  TryInitInitializeCriticalSectionEx;
{$IFDEF DEBUG_CRIT_SECT}
  TestTdzRtlCriticalSection;
{$ENDIF}
{$ENDIF SUPPORTS_ENHANCED_RECORDS}
end.
