unit u_StringQueue;

interface

uses
  SysUtils,
  u_PCharQueue;

type
  TStringQueue = class
  private
    FQueue: TPCharQueue;
  public
    /// Creates a new TStringQueue instance
    constructor Create;
    destructor Destroy; override;
    /// adds an item to the end of the queue
    procedure Enqueue(const _Item: string);
    /// removes the first item from the queue and returns it
    function Dequeue: string; overload;
    /// removes the first item from the queue
    /// @param Item contains the dequeued item, only valid if result is true
    /// @returns true, if an item was dequeued, false otherwise
    function Dequeue(out _Item: string): boolean; overload;
    /// returns the first item in the queue without removing it
    function Peek: string; overload;
    /// returns the first item in the queue without removing it
    /// @param Item is the first item, only valid if result is true
    /// @returns true, if the queue was not emtpy, false otherwise
    function Peek(out _Item: string): boolean; overload;
    /// returns true, if the queue is emtpy, false otherwise
    function IsEmpty: boolean;
    /// returns the length of the queue
    function Count: integer;
  end;

implementation

{ TStringQueue }

constructor TStringQueue.Create;
begin
  inherited Create;
  FQueue := TPCharQueue.Create;
end;

destructor TStringQueue.Destroy;
begin
  FQueue.Free;
  inherited;
end;

function TStringQueue.Count: integer;
begin
  Result := FQueue.Count;
end;

function TStringQueue.Dequeue(out _Item: string): boolean;
var
  p: PChar;
begin
  Result := FQueue.Dequeue(p);
  if Result then begin
    _Item := p;
    StrDispose(p);
  end;
end;

function TStringQueue.Dequeue: string;
var
  p: PChar;
begin
  p := FQueue.Dequeue;
  Result := p;
  StrDispose(p);
end;

procedure TStringQueue.Enqueue(const _Item: string);
begin
  FQueue.Enqueue(StrNew(PChar(_Item)));
end;

function TStringQueue.IsEmpty: boolean;
begin
  Result := FQueue.IsEmpty;
end;

function TStringQueue.Peek: string;
var
  p: PChar;
begin
  p := FQueue.Peek;
  Result := p;
  StrDispose(p);
end;

function TStringQueue.Peek(out _Item: string): boolean;
var
  p: PChar;
begin
  Result := FQueue.Peek(p);
  if Result then begin
    _Item := p;
    StrDispose(p);
  end;
end;

end.

