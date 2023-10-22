unit u_dzSortProvider;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils,
  Classes,
  u_dzTranslator;

type
  TdzAbstractSortProvider = class
  private
    FCount: Integer;
    FSorted: array of Integer;
  protected
    function doCompare(_IndexA, _IndexB: Integer): Integer; virtual; abstract;
    procedure doSwap(_IndexA, _IndexB: Integer);
  public
    constructor Create(_Count: Integer);
    procedure Update; overload;
    procedure Update(_Count: Integer); overload;
    function GetRealPos(_Pos: Integer): Integer;
    property Count: Integer read FCount;
  end;

type
  TDummyIntegerArray = array[0..0] of Integer;

type
  TdzIntegerArraySortProvider = class(TdzAbstractSortProvider)
  private
  private
    FOriginal: ^TDummyIntegerArray;
  protected
    function doCompare(_IndexA, _IndexB: Integer): Integer; override;
  public
    constructor Create(const _Original: array of Integer);
  end;

type
  TSortCompareCallback = function(_IndexA: Integer; _IndexB: Integer): Integer of object;
type
  ///<summary>
  /// The SortProvider provides sorting to a static array or list without changing that list.
  /// Example:
  ///     Arr[0..Count - 1]: unsorted data
  ///
  ///   Creating a sort provider:
  ///     sp := TdzSortProvider.Create(Length(Array), CompareItems);
  ///     // where CompareItems is a function that cmpares the entries Arr[IndexA] to Arr[IndexB]
  ///     // an returns
  ///     // 1, if Arr[_IndexA] > Arr[_IndexB]
  ///     // 0, if Arr[_IndexA] = Arr[_IndexB]
  ///     // -1, if Arr[_IndexA] < Arr[_IndexB]
  ///
  ///     sp.GetRealPos(Idx) is the real position (in Arr) of the sorted Idx'th entry
  ///
  ///   Output the array in the unsorted original order:
  ///     for i := 0 to Count - 1 do
  ///       doOutput(Arr[i]);
  ///
  ///   Output of the array in sorted order:
  ///     for i := 0 to Count - 1 do
  ///       doOutput(Arr[sp.GetRealPos(i)]);
  /// Credits: I got the idea from Allan Mertner who implemented it at fPrint UK Ltd.
  ///          some time in the 90ies. Since then I have re-implemented it several times
  ///          for various jobs until putting it into dzlib now.
  ///</summary>
  TdzSortProvider = class(TdzAbstractSortProvider)
  public
  protected
    FOnCompare: TSortCompareCallback;
    function doCompare(_IndexA, _IndexB: Integer): Integer; override;
  public
    constructor Create(_Count: Integer; _OnCompare: TSortCompareCallback);
    procedure Update(_OnCompare: TSortCompareCallback); overload;
    procedure Update(_Count: Integer; _OnCompare: TSortCompareCallback); overload;
  end;

implementation

uses
  Math,
  u_dzQuicksort;

{ TdzAbstractSortProvider }

constructor TdzAbstractSortProvider.Create(_Count: Integer);
begin
  inherited Create();
  Update(_Count);
end;

procedure TdzAbstractSortProvider.Update(_Count: Integer);
var
  i: Integer;
begin
  FCount := _Count;
  SetLength(FSorted, FCount);
  for i := 0 to FCount - 1 do
    FSorted[i] := i;
  QuickSort(0, _Count - 1, doCompare, doSwap);
end;

procedure TdzAbstractSortProvider.Update;
begin
  Update(FCount);
end;

procedure TdzAbstractSortProvider.doSwap(_IndexA, _IndexB: Integer);
var
  temp: Integer;
begin
  temp := FSorted[_IndexA];
  FSorted[_IndexA] := FSorted[_IndexB];
  FSorted[_IndexB] := temp;
end;

function TdzAbstractSortProvider.GetRealPos(_Pos: Integer): Integer;
begin
  Result := FSorted[_Pos];
end;

{ TdzIntegerArraySortProvider }

constructor TdzIntegerArraySortProvider.Create(const _Original: array of Integer);
begin
  FOriginal := Pointer(@_Original[0]);
  inherited Create(Length(_Original));
end;

function TdzIntegerArraySortProvider.doCompare(_IndexA, _IndexB: Integer): Integer;
begin
  Result := CompareValue(FOriginal^[_IndexA], FOriginal^[_IndexB]);
end;

{ TdzSortProvider }

constructor TdzSortProvider.Create(_Count: Integer; _OnCompare: TSortCompareCallback);
begin
  FOnCompare := _OnCompare;
  inherited Create(_Count);
end;

procedure TdzSortProvider.Update(_Count: Integer; _OnCompare: TSortCompareCallback);
begin
  FOnCompare := _OnCompare;
  Update(_Count);
end;

procedure TdzSortProvider.Update(_OnCompare: TSortCompareCallback);
begin
  Update(FCount, _OnCompare);
end;

function TdzSortProvider.doCompare(_IndexA, _IndexB: Integer): Integer;
begin
  Assert(Assigned(FOnCompare));

  Result := FOnCompare(_IndexA, _IndexB);
end;

end.

