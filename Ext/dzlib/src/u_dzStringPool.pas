unit u_dzStringPool;

interface

uses
  SysUtils,
  Classes;

type
  ///<summary>
  /// This class maintains a sorted list of all strings that are ever passed to its Intern function
  /// and always returns a string that refers to a single memory area containing the string thus
  /// resulting in greatly reduced memory requirements if an application uses many duplicate strings.
  /// (see string interning)
  /// This is a rather trivial implemenation internally using a sorted TStringList.
  /// Note: An instance of this class is not thread safe when used by several threads but you can
  ///       create one instance per thread which will eliminate duplicates whithin this thread.
  /// Note: There is also an InternString function that internally uses a global instance of this
  ///       class. As noted above this is not thread safe. </summary>
  TStringPool = class
{$IFDEF DebugStringPool}
  protected
{$ELSE}
  private
{$ENDIF}
    FList: TStringList;
    FMakeStringsUnique: Boolean;
  public
    ///<summary>
    /// @param MakeStringsUnique is a boolean determining whether strings are made unique (by
    ///                          calling UniqueString) before adding them to the list. This should
    ///                          not be necessary in most applications. </summary>
    constructor Create(_MakeStringsUnique: Boolean = False);
    destructor Destroy; override;
    ///<summary> Intern the given string, that is: Consolidate duplicates. </summary>
    procedure Intern(var _s: string);
    function GetStatistics: string;
    function SaveToFile(const _s: string): Integer;
  end;

procedure InternString(var _s: string);
function InternStr(const _s: string): string;

function GetStringPoolStatistics: string;
function StringPoolSaveToFile(const _s: string): Integer;

implementation

var
  StringPool: TStringPool = nil;

procedure InternString(var _s: string);
begin
  if not Assigned(StringPool) then
    StringPool := TStringPool.Create();
  StringPool.Intern(_s);
end;

function InternStr(const _s: string): string;
begin
  Result := _s;
  InternString(Result);
end;

function GetStringPoolStatistics: string;
begin
  if Assigned(StringPool) then
    Result := StringPool.GetStatistics
  else
    Result := 'stringpool is not in use';
end;

function StringPoolSaveToFile(const _s: string): Integer;
begin
  if Assigned(StringPool) then
    Result := StringPool.SaveToFile(_s)
  else
    Result := 0;
end;

{ TStringPool }

constructor TStringPool.Create(_MakeStringsUnique: Boolean = False);
begin
  inherited Create;
  FList := TStringList.Create;
  FList.CaseSensitive := True;
  FList.Sorted := True;
  FMakeStringsUnique := _MakeStringsUnique;
end;

function GetStringRefcount(const s: string): Cardinal;
asm
  or eax, eax
  jz @done
  sub eax, 8
  mov eax, dword ptr [eax]
@done:
end;

destructor TStringPool.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TStringPool.GetStatistics: string;
var
  MinCnt: Integer;
  MaxCnt: Integer;
  RefCnt: Integer;
  cnt: Int64;
  i: Integer;
  Avg: Double;
  s: string;
begin
  MinCnt := MaxInt;
  MaxCnt := 0;
  cnt := 0;
  for i := 0 to FList.Count - 1 do begin
    s := FList[i];
    RefCnt := GetStringRefcount(s);
    if s <> '' then begin
      // RefCnt is always 2 or more:
      // s
      // FList[i]
      // We don't care about these two references, so we subtract them.
      Dec(RefCnt, 2);
    end;
    cnt := cnt + RefCnt;
    if RefCnt > MaxCnt then
      MaxCnt := RefCnt;
    if s <> '' then begin
      if RefCnt < MinCnt then
        MinCnt := RefCnt;
    end;
  end;
  if FList.Count > 0 then
    Avg := cnt / FList.Count
  else
    Avg := 0;
  Result := Format('%d strings, Avg RefCount: %.2f, Min RefCnt: %d, MaxRefCnt: %d',
    [FList.Count, Avg, MinCnt, MaxCnt]);
end;

procedure TStringPool.Intern(var _s: string);
var
  idx: Integer;
begin
  if FList.Find(_s, idx) then
    _s := FList[idx]
  else begin
    if FMakeStringsUnique then
      UniqueString(_s);
    FList.Add(_s);
  end;
end;

function TStringPool.SaveToFile(const _s: string): Integer;
var
  i: Integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    for i := 0 to FList.Count - 1 do begin
      sl.Add(Format('"%s" %d', [FList[i], GetStringRefcount(FList[i])]));
    end;
    sl.SaveToFile(_s);
  finally
    FreeAndNil(sl);
  end;
  Result := FList.Count;
end;

procedure TestStringPool;

var
  s: string;
  s2: string;
begin
  s := 'bla';
  InternString(s);
  s2 := s;
  s := '';
  s := s2;
  GetStringPoolStatistics;
end;

initialization
//  TestStringPool;
finalization
  FreeAndNil(StringPool);
end.

