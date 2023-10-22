unit u_dzInsertionSort;

interface

uses
  u_dzSortUtils;

// https://en.wikipedia.org/wiki/Insertion_sort
procedure InsertionSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth); overload;
procedure InsertionSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler); overload;

implementation

procedure InsertionSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth);
var
  i: Integer;
  j: Integer;
begin
  i := _Left + 1;
  while i <= _Right do begin
    j := i;
    while (j > _Left) and (_CompareMeth(j - 1, j) > 0) do begin
      _SwapMeth(j, j - 1);
      Dec(j);
    end;
    Inc(i);
  end;
end;

procedure InsertionSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler); overload;
var
  i: Integer;
  j: Integer;
begin
  i := _Left + 1;
  while i <= _Right do begin
    j := i;
    while (j > _Left) and (_DataHandler.Compare(j - 1, j) > 0) do begin
      _DataHandler.Swap(j, j - 1);
      Dec(j);
    end;
    Inc(i);
  end;
end;

end.
