unit u_dzSpinLock;

interface

uses
  Windows;

///<summary>
/// simple spin lock function, Lk must have been initialized with 0 before first use </summary>
procedure doLock(var _Lk: Integer);

///<summary>
/// simple spin unlock function, Lk must have been initialized with 0 before first use </summary>
procedure doUnLock(var _Lk: Integer);

// simple spinlock functions are taken from
// https://vitaliburkov.wordpress.com/2011/10/28/parallel-programming-with-delphi-part-ii-resolving-race-conditions/

///<summary>
/// simple spin trylock function, Lk must have been initialized with 0 before first use
/// @returns True if the lock could be acquired, False otherwise </summary>
function doTryLock(var _Lk: Integer): Boolean;

{$IF not declared(YieldProcessor)}
procedure YieldProcessor;
{$IFEND}

{$IF not declared(AtomicCmpExchange)}
{$DEFINE NEEDS_ATOMICCMPEXCHANGE}
///<summary>
/// Compares the contents of the Target to a given value (Comparand) and, only if they are the same,
/// modifies the contents of Target to the new value.
/// This function always returns the original value of Target.
/// If a Succeeded parameter is provided, Succeeded becomes True if there is a value exchange
/// (even if both Target and NewValue are the same); it becomes False otherwise.
function AtomicCmpExchange(var Target: Integer; NewValue: Integer; Comparand: Integer; out Succeeded: Boolean): Integer; overload;
function AtomicCmpExchange(var Target: Integer; NewValue: Integer; Comparand: Integer): Integer; overload;
{$IFEND}

type
  ///<summary>
  /// non class based version of a spin lock.
  /// @NOTE: You must initialize the record to 0, e.g. like this:
  ///        rec := TSpinLockRec.Create;
  /// @NOTE: You must take care of calling AcquireXxx and Release in the proper order </summary>
  TSpinLockRec = record
    Flk: Integer;
    ///<summary>
    /// Returns a properly initialized TSpinLockRec record. </summary>
    class function Create: TSpinLockRec; static;
    ///<summary>
    /// Acquire the lock, loop until successful (burns CPU) </summary>
    procedure AcquireLoop;
    ///<summary>
    /// Acquire the lock, call Sleep after every successful attempt (yields CPU) </summary>
    procedure AcquireSleep;
    ///<summary>
    /// Release the lock </summary>
    procedure Release;
    ///<summary>
    /// alias for AcquireSleep </summary>
    procedure Enter; inline;
    ///<summary>
    /// alias for Release </summary>
    procedure Leave; inline;
  end;

type
  /// <summary>
  /// This is a very simple thread synchronisation class that allows to limit
  /// access to a resource. It uses InterlockedIncrement/Decrement.
  /// @NOTE: This is much slower than TSpinLocRec and TSpinLock2. So unless you have a very
  /// good reason, you will want to use one of these instead.
  /// @NOTE: You must take care of calling AcquireXxx and Release in the proper order </summary>
  TSpinLock = class
  private
    FLock: Integer;
  public
    constructor Create;
    /// Acquire the lock, loop until successful (burns CPU) </summary>
    procedure AcquireLoop;
    /// Acquire the lock, call Sleep after every successful attempt (yields CPU) </summary>
    procedure AcquireSleep;
    /// <summary> try to aquire the lock and return false if it could not be acquired </summary>
    function TryAcquire: Boolean;
    /// <summary> release the lock  </summary>
    procedure Release;
    ///<summary>
    /// alias for AcquireSleep </summary>
    procedure Enter; inline;
    ///<summary>
    /// alias for Release </summary>
    procedure Leave; inline;
  end;

type
  /// <summary>
  /// This is a very simple thread synchronisation class that allows to limit
  /// access to a resource. It uses the assmbler code instruction 'lock cmpxchg'.
  /// @NOTE: You must take care of calling AcquireXxx and Release in the proper order </summary>
  TSpinLock2 = class
  private
    FLock: Integer;
  public
    constructor Create;
    /// Acquire the lock, loop until successful (burns CPU) </summary>
    procedure AcquireLoop;
    /// Acquire the lock, call Sleep after every successful attempt (yields CPU) </summary>
    procedure AcquireSleep;
    /// <summary> try to aquire the lock and return false if it could not be acquired </summary>
    function TryAcquire: Boolean;
    /// <summary> release the lock  </summary>
    procedure Release;
    ///<summary>
    /// alias for AcquireSleep </summary>
    procedure Enter; inline;
    ///<summary>
    /// alias for Release </summary>
    procedure Leave; inline;
  end;

implementation

const
  // not 0, for the reason see https://devblogs.microsoft.com/oldnewthing/20051004-09/?p=33923
  SLEEP_INTERVAL = 1;

{ TSpinLock }

constructor TSpinLock.Create;
begin
  inherited Create;
  FLock := -1;
end;

procedure TSpinLock.Enter;
begin
  AcquireSleep;
end;

procedure TSpinLock.Leave;
begin
  Release;
end;

procedure TSpinLock.AcquireLoop;
begin
  while not TryAcquire do
    ;
end;

procedure TSpinLock.AcquireSleep;
begin
  while not TryAcquire do
    Sleep(SLEEP_INTERVAL);
end;

function TSpinLock.TryAcquire: Boolean;
begin
  Result := (InterlockedIncrement(FLock) = 0);
  if not Result then
    InterlockedDecrement(FLock);
end;

procedure TSpinLock.Release;
begin
  InterlockedDecrement(FLock);
end;

function doTryLock(var _Lk: Integer): Boolean;
asm
  mov edx, eax
  mov ecx, 1
  mov eax, 0
  lock cmpxchg dword ptr [edx], ecx
  setz al
end;

procedure doLock(var _Lk: Integer);
asm
  mov edx, eax
  mov ecx, 1
@Loop:
  xor eax,eax
  lock cmpxchg dword ptr [edx], ecx
  jnz @Loop
end;

procedure doUnLock(var _Lk: Integer);
asm
  lock dec dword ptr[eax];
end;

{ TSpinLockRec }

class function TSpinLockRec.Create: TSpinLockRec;
begin
  Result.Flk := 0;
end;

procedure TSpinLockRec.Enter;
begin
  AcquireSleep;
end;

procedure TSpinLockRec.Leave;
begin
  Release;
end;

procedure TSpinLockRec.AcquireSleep;
begin
  while not doTryLock(Flk) do
    Sleep(SLEEP_INTERVAL);
end;

procedure TSpinLockRec.AcquireLoop;
// while AtomicCmpExchange(Flk, 1, 0) do begin
//   YieldProcessor;
// end;
asm
  // eax = self
  push ebx

  mov ebx,eax
  jmp @while

@Loop:
  pause

@while:
  xor eax,eax
  mov edx,1
  // cmpxchg DEST, SRC
  // // Accumulator = AL, AX, EAX, or RAX depending on whether a byte, word, doubleword, or quadword comparison is being performed
  // TEMP := DEST
  // IF accumulator = TEMP THEN
  //   ZF := 1;
  //   DEST := SRC;
  // ELSE
  //   ZF := 0;
  //   accumulator := TEMP;
  //   DEST := TEMP;
  // FI;
  lock cmpxchg [ebx],edx
  jnz @Loop

  pop ebx
end;

procedure TSpinLockRec.Release;
// AtomicExchange(Flk, 0);
asm
  // eax = self
  xor edx, edx
  lock xchg[eax], edx
end;

{ TSpinLock2 }

procedure TSpinLock2.AcquireLoop;
begin
  doLock(FLock);
end;

procedure TSpinLock2.AcquireSleep;
begin
  while not doTryLock(FLock) do
    Sleep(SLEEP_INTERVAL);
end;

constructor TSpinLock2.Create;
begin
  FLock := 0;
end;

procedure TSpinLock2.Enter;
begin
  AcquireSleep;
end;

procedure TSpinLock2.Leave;
begin
  Release;
end;

procedure TSpinLock2.Release;
begin
  doUnLock(FLock);
end;

function TSpinLock2.TryAcquire: Boolean;
begin
  Result := doTryLock(FLock);
end;

procedure YieldProcessor;
asm
  PAUSE
end;

{$IFDEF NEEDS_ATOMICCMPEXCHANGE}
function AtomicCmpExchange(var Target: Integer; NewValue: Integer; Comparand: Integer): Integer;
asm
  push ebx
  mov ebx, eax
  mov eax, ecx
  lock cmpxchg[ebx], edx
  pop ebx
end;

function AtomicCmpExchange(var Target: Integer; NewValue: Integer; Comparand: Integer; out Succeeded: Boolean): Integer;
asm
//  push ebp
//  mov ebp,esp
  push ecx

  mov [ebp-$04],ecx
  mov ecx,eax
  mov eax,[ebp-$04]
  lock cmpxchg [ecx],edx
  mov edx,[ebp+$08]
  setz byte ptr [edx]

  pop ecx
//  pop ebp
end;
{$ENDIF NEEDS_ATOMICCMPEXCHANGE}

procedure TestAtomicCmpExchange1;
var
  Target: Integer;
  NewValue: Integer;
  Comparand: Integer;
  Return: Integer;
  Succeeded: Boolean;
begin
  Target := 1;
  NewValue := 7;
  Comparand := 5;

  Return := AtomicCmpExchange(Target, NewValue, Comparand, Succeeded);
  Assert(Return = 1);
  Assert(Target = 1);
  Assert(Succeeded = False);

  Comparand := Target;
  Return := AtomicCmpExchange(Target, NewValue, Comparand, Succeeded);
  Assert(Return = 1);
  Assert(Target = NewValue);
  Assert(Succeeded = True);
end;

procedure TestAtomicCmpExchange2;
var
  Target: Integer;
  NewValue: Integer;
  Comparand: Integer;
  Return: Integer;
begin
  Target := 1;
  NewValue := 7;
  Comparand := 5;

  Return := AtomicCmpExchange(Target, NewValue, Comparand);
  Assert(Return = 1);
  Assert(Target = 1);

  Comparand := Target;
  Return := AtomicCmpExchange(Target, NewValue, Comparand);
  Assert(Return = 1);
  Assert(Target = NewValue);
end;

//initialization
//  TestAtomicCmpExchange1;
//  TestAtomicCmpExchange2;
end.
