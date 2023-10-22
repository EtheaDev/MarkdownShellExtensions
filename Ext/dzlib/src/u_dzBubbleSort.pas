unit u_dzBubbleSort;

interface

uses
  u_dzSortUtils;

// Why do I use Bubblesort instead of Insertionsort?
// * It has roughly the same efficiency as Insertionsort (according to Wikipedia)
//   which is good enough for small data sets.
// * It requires the same methods as Quicksort: Compare and Swap
//   (Insertionsort would need a Move method that is not as simple to implement independently
//    of the data type.)

// https://en.wikipedia.org/wiki/Bubble_sort
procedure BubbleSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth); overload;
procedure BubbleSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler); overload;

implementation

procedure BubbleSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth);
var
  Max: Integer;
  NewMax: Integer;
  i: Integer;
begin
  Max := _Right;
  repeat
    NewMax := 0;
    for i := 1 to Max do begin
      if _CompareMeth(i - 1, i) > 0 then begin
        _SwapMeth(i - 1, i);
        NewMax := i;
      end;
    end;
    Max := NewMax;
  until Max = 0;
end;

procedure BubbleSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler);
var
  Max: Integer;
  NewMax: Integer;
  i: Integer;
begin
  Max := _Right;
  repeat
    NewMax := 0;
    for i := 1 to Max do begin
      if _DataHandler.Compare(i - 1, i) > 0 then begin
        _DataHandler.Swap(i - 1, i);
        NewMax := i;
      end;
    end;
    Max := NewMax;
  until Max = 0;
end;

end.
