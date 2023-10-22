unit u_dzRingBuffer;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils,
  u_dzTranslator;

type
  EdzRingBuffer = class(Exception);
  EBufferFull = class(EdzRingBuffer);
  EBufferEmpty = class(EdzRingBuffer);
  EIndexOutOfBounds = class(EdzRingBuffer);

type
  TdzCustomRingBuffer = class
  private
    procedure CheckIndex(_Idx: Integer);
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  protected
    ///<summary> size of one element stored in this buffer </summary>
    FElementSize: Integer;
    ///<summary> length of the buffer </summary>
    FLength: Integer;
    ///<summary> pointer to a memory block that stores the buffer </summary>
    FBuffer: PByte;
    ///<summary> position of the first element stored in the buffer,
    ///          if fFirstUsed = fFirstFree the buffer is empty </summary>
    FFirstUsed: Integer;
    ///<summary> position of the next element bo be stored in the buffer
    ///          if fFirstUsed = fFirstFree the buffer is empty </summary>
    FFirstFree: Integer;
    ///<summary> number of elements stored in the buffer </summary>
    FElementCount: Integer;
    ///<summary> is called by the destructor to do any finalization that might be
    ///          necessary for the elements stored in the buffer. Does nothing here. </summary>
    procedure FinalizeElements; virtual;
    ///<summary> inserts the given Element in front of the buffer </summary>
    procedure InsertFront(const _Element); virtual;
    ///<summary> inserts the given Element at the end of the buffer </summary>
    procedure InsertEnd(const _Element); virtual;
    ///<summary> extracts the first Element from the buffer </summary>
    procedure ExtractFront(var _Element); virtual;
    ///<summary> extracts the last Element from the buffer </summary>
    procedure ExtractEnd(var _Element); virtual;
    ///<summary> gets the Element with the index Idx </summary>
    procedure GetElement(_Idx: Integer; var _Element); virtual;
    ///<summary> sets the Element with the index Idx, note: No finalization is done
    ///          for the element previously stored at Idx. </summary>
    procedure SetElement(_Idx: Integer; const _Element); virtual;
    ///<summary> gets the first Element from the buffer </summary>
    procedure GetFirst(var _Element); virtual;
    ///<summary> gets the last Element from the buffer </summary>
    procedure GetLast(var _Element); virtual;
    ///<summary> checks whether there is enough space for another element in the buffer </summary>
    procedure CheckFull; virtual;
    ///<summary> checks whether there are any elements in the buffer </summary>
    procedure CheckEmpty; virtual;
    ///<summary> returns true, if the buffer is full </summary>
    function IsFull: Boolean; virtual;
    ///<summary> returns true, if the buffer is empty </summary>
    function IsEmpty: Boolean; virtual;
    ///<summary> returns the number of elements stored in the buffer </summary>
    function GetCount: Integer; virtual;
    ///<summary> deletes all elements from the buffer </summary>
    procedure Clear; virtual;
  public
    ///<summary> creates a ringbuffer with enough space to store Length elements of
    ///          ElementSize bytes size </summary>
    constructor Create(_ElementSize: Integer; _Length: Integer);
    ///<summary> calls FinalizeElements and frees the memory allocated for the buffer </summary>
    destructor Destroy; override;
  end;

type
  ///<summary> publishes all methods of TdzCustomRingBuffer </summary>
  TdzRingBuffer = class(TdzCustomRingBuffer)
  public
    ///<summary> inserts the given Element in front of the buffer </summary>
    procedure InsertFront(const _Element); override;
    ///<summary> inserts the given Element at the end of the buffer </summary>
    procedure InsertEnd(const _Element); override;
    ///<summary> extracts the first Element from the buffer </summary>
    procedure ExtractFront(var _Element); override;
    ///<summary> extracts the last Element from the buffer </summary>
    procedure ExtractEnd(var _Element); override;
    ///<summary> gets the Element with the index Idx </summary>
    procedure GetElement(_Idx: Integer; var _Element); override;
    ///<summary> sets the Element with the index Idx, note: No finalization is done
    ///          for the element previously stored at Idx. </summary>
    procedure SetElement(_Idx: Integer; const _Element); override;
    ///<summary> gets the first Element from the buffer </summary>
    procedure GetFirst(var _Element); override;
    ///<summary> gets the last Element from the buffer </summary>
    procedure GetLast(var _Element); override;
    ///<summary> checks whether there is enough space for another element in the buffer </summary>
    procedure CheckFull; override;
    ///<summary> checks whether there are any elements in the buffer </summary>
    procedure CheckEmpty; override;
    ///<summary> returns true, if the buffer is full </summary>
    function IsFull: Boolean; override;
    ///<summary> returns true, if the buffer is empty </summary>
    function IsEmpty: Boolean; override;
    ///<summary> returns the number of elements stored in the buffer </summary>
    function GetCount: Integer; override;
    ///<summary> deletes all elements from the buffer </summary>
    procedure Clear; override;
  end;

type
  ///<summary> makes only those methods public that are useful for a stack
  ///          (Yes, it doesn't really make much sense implementing a stack
  ///          as a ring buffer.) </summary>
  TdzRingStack = class(TdzCustomRingBuffer)
  public
    ///<summary> inserts the given Element at the end of the buffer </summary>
    procedure InsertEnd(const _Element); override;
    ///<summary> extracts the last Element from the buffer </summary>
    procedure ExtractEnd(var _Element); override;
    ///<summary> gets the Element with the index Idx </summary>
    procedure GetElement(_Idx: Integer; var _Element); override;
    ///<summary> sets the Element with the index Idx, note: No finalization is done
    ///          for the element previously stored at Idx. <summary>
    procedure SetElement(_Idx: Integer; const _Element); override;
    ///<summary> gets the last Element from the buffer </summary>
    procedure GetLast(var _Element); override;
    ///<summary> returns true, if the buffer is full </summary>
    function IsFull: Boolean; override;
    ///<summary> returns true, if the buffer is empty </summary>
    function IsEmpty: Boolean; override;
    ///<summary> returns the number of elements stored in the buffer </summary>
    function GetCount: Integer; override;
    ///<summary> deletes all elements from the buffer </summary>
    procedure Clear; override;
  end;

type
  ///<summary> makes only those methods public that are useful for a queue </summary>
  TdzRingQueue = class(TdzCustomRingBuffer)
    ///<summary> inserts the given Element at the end of the buffer </summary>
    procedure InsertEnd(const _Element); override;
    ///<summary> extracts the first Element from the buffer </summary>
    procedure ExtractFront(var _Element); override;
    function TryExtractFront(var _Element): Boolean;
    ///<summary> gets the Element with the index Idx </summary>
    procedure GetElement(_Idx: Integer; var _Element); override;
    ///<summary> sets the Element with the index Idx, note: No finalization is done
    ///          for the element previously stored at Idx. </summary>
    procedure SetElement(_Idx: Integer; const _Element); override;
    ///<summary> gets the first Element from the buffer </summary>
    procedure GetFirst(var _Element); override;
    function TryGetFirst(var _Element): Boolean;
    ///<summary> returns true, if the buffer is full </summary>
    function IsFull: Boolean; override;
    ///<summary> returns true, if the buffer is empty </summary>
    function IsEmpty: Boolean; override;
    ///<summary> returns the number of elements stored in the buffer </summary>
    function GetCount: Integer; override;
    ///<summary> deletes all elements from the buffer </summary>
    procedure Clear; override;
  end;

implementation

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TdzCustomRingBuffer }

constructor TdzCustomRingBuffer.Create(_ElementSize, _Length: Integer);
begin
  inherited Create;
  FLength := _Length;
  FElementSize := _ElementSize;
  FFirstUsed := 0;
  FFirstFree := 0;
  GetMem(FBuffer, FLength * FElementSize);
end;

destructor TdzCustomRingBuffer.Destroy;
begin
  if Assigned(FBuffer) and (FLength > 0) then begin
    FinalizeElements;
    FreeMem(FBuffer);
  end;
  inherited;
end;

procedure TdzCustomRingBuffer.FinalizeElements;
begin
  // does nothing, override if elements need finalization
end;

procedure TdzCustomRingBuffer.InsertFront(const _Element);
var
  p: PByte;
begin
  CheckFull;
  FFirstUsed := (FFirstUsed - 1) mod FLength;
  p := FBuffer;
  Inc(p, FFirstUsed * FElementSize);
  Move(_Element, p^, FElementSize);
  Inc(FElementCount);
end;

procedure TdzCustomRingBuffer.InsertEnd(const _Element);
var
  p: PByte;
begin
  CheckFull;
  p := FBuffer;
  Inc(p, FFirstFree * FElementSize);
  Move(_Element, p^, FElementSize);
  FFirstFree := (FFirstFree + 1) mod FLength;
  Inc(FElementCount);
end;

procedure TdzCustomRingBuffer.ExtractFront(var _Element);
var
  p: PByte;
begin
  CheckEmpty;
  p := FBuffer;
  Inc(p, FFirstUsed * FElementSize);
  Move(p^, _Element, FElementSize);
  FFirstUsed := (FFirstUsed + 1) mod FLength;
  Dec(FElementCount);
end;

procedure TdzCustomRingBuffer.ExtractEnd(var _Element);
var
  p: PByte;
begin
  CheckEmpty;
  FFirstFree := (FFirstFree - 1) mod FLength;
  p := FBuffer;
  Inc(p, FFirstFree * FElementSize);
  Move(p^, _Element, FElementSize);
  Dec(FElementCount);
end;

procedure TdzCustomRingBuffer.CheckIndex(_Idx: Integer);
begin
  if IsFull then begin
    if (_Idx < 0) or (_Idx >= FLength) then
      raise EIndexOutOfBounds.CreateFmt(_('Index %d out of bounds.'), [_Idx]);
  end else begin
    if (FFirstUsed + _Idx) mod FLength >= FFirstFree then
      raise EIndexOutOfBounds.CreateFmt(_('Index %d out of bounds.'), [_Idx]);
  end;
end;

procedure TdzCustomRingBuffer.GetElement(_Idx: Integer; var _Element);
var
  p: PByte;
begin
  CheckIndex(_Idx);
  p := FBuffer;
  Inc(p, (FFirstUsed + _Idx) * FElementSize);
  Move(p^, _Element, FElementSize);
end;

procedure TdzCustomRingBuffer.SetElement(_Idx: Integer; const _Element);
var
  p: PByte;
begin
  CheckIndex(_Idx);
  p := FBuffer;
  Inc(p, (FFirstUsed + _Idx) * FElementSize);
  Move(_Element, p^, FElementSize);
end;

procedure TdzCustomRingBuffer.GetFirst(var _Element);
begin
  GetElement(0, _Element);
end;

procedure TdzCustomRingBuffer.GetLast(var _Element);
begin
  GetElement(GetCount - 1, _Element);
end;

function TdzCustomRingBuffer.IsFull: Boolean;
begin
  Result := FElementCount >= FLength;
end;

procedure TdzCustomRingBuffer.CheckFull;
begin
  if IsFull then
    raise EBufferFull.Create('Buffer is full');
end;

function TdzCustomRingBuffer.IsEmpty: Boolean;
begin
  Result := FElementCount = 0;
end;

procedure TdzCustomRingBuffer.CheckEmpty;
begin
  if IsEmpty then
    raise EBufferEmpty.Create('Buffer is empty');
end;

function TdzCustomRingBuffer.GetCount: Integer;
begin
  Result := FElementCount;
end;

procedure TdzCustomRingBuffer.Clear;
begin
  FinalizeElements;
  FFirstUsed := 0;
  FFirstFree := 0;
  FElementCount := 0;
end;

{ TdzRingBuffer }

procedure TdzRingBuffer.CheckEmpty;
begin
  inherited;
end;

procedure TdzRingBuffer.CheckFull;
begin
  inherited;
end;

procedure TdzRingBuffer.Clear;
begin
  inherited;
end;

procedure TdzRingBuffer.ExtractEnd(var _Element);
begin
  inherited;
end;

procedure TdzRingBuffer.ExtractFront(var _Element);
begin
  inherited;
end;

function TdzRingBuffer.GetCount: Integer;
begin
  Result := inherited GetCount;
end;

procedure TdzRingBuffer.GetElement(_Idx: Integer; var _Element);
begin
  inherited;
end;

procedure TdzRingBuffer.GetFirst(var _Element);
begin
  inherited;
end;

procedure TdzRingBuffer.GetLast(var _Element);
begin
  inherited;
end;

procedure TdzRingBuffer.InsertEnd(const _Element);
begin
  inherited;
end;

procedure TdzRingBuffer.InsertFront(const _Element);
begin
  inherited;
end;

function TdzRingBuffer.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty;
end;

function TdzRingBuffer.IsFull: Boolean;
begin
  Result := inherited IsFull;
end;

procedure TdzRingBuffer.SetElement(_Idx: Integer; const _Element);
begin
  inherited;
end;

{ TdzRingStack }

procedure TdzRingStack.Clear;
begin
  inherited;
end;

procedure TdzRingStack.ExtractEnd(var _Element);
begin
  inherited;
end;

function TdzRingStack.GetCount: Integer;
begin
  Result := inherited GetCount;
end;

procedure TdzRingStack.GetElement(_Idx: Integer; var _Element);
begin
  inherited;
end;

procedure TdzRingStack.GetLast(var _Element);
begin
  inherited;
end;

procedure TdzRingStack.InsertEnd(const _Element);
begin
  inherited;
end;

function TdzRingStack.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty;
end;

function TdzRingStack.IsFull: Boolean;
begin
  Result := inherited IsFull;
end;

procedure TdzRingStack.SetElement(_Idx: Integer; const _Element);
begin
  inherited;
end;

{ TdzRingQueue }

procedure TdzRingQueue.Clear;
begin
  inherited;
end;

procedure TdzRingQueue.ExtractFront(var _Element);
begin
  inherited;
end;

function TdzRingQueue.GetCount: Integer;
begin
  Result := inherited GetCount;
end;

procedure TdzRingQueue.GetElement(_Idx: Integer; var _Element);
begin
  inherited;
end;

procedure TdzRingQueue.GetFirst(var _Element);
begin
  inherited;
end;

function TdzRingQueue.TryExtractFront(var _Element): Boolean;
begin
  Result := not IsEmpty;
  if Result then
    ExtractFront(_Element);
end;

function TdzRingQueue.TryGetFirst(var _Element): Boolean;
begin
  Result := not IsEmpty;
  if Result then
    GetFirst(_Element);
end;

procedure TdzRingQueue.InsertEnd(const _Element);
begin
  inherited;
end;

function TdzRingQueue.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty;
end;

function TdzRingQueue.IsFull: Boolean;
begin
  Result := inherited IsFull;
end;

procedure TdzRingQueue.SetElement(_Idx: Integer; const _Element);
begin
  inherited;
end;

end.
