unit u_dzCombSort;

interface

uses
  u_dzSortUtils;

// https://en.wikipedia.org/wiki/Comb_sort
procedure CombSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth); overload;
procedure CombSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler); overload;

implementation

procedure CombSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth);
var
  i: Integer;
  gap: Integer;
  Sorted: Boolean;
begin
  gap := _Right - _Left + 1;
  Sorted := False;
  while (gap > 1) or not Sorted do begin
    gap := trunc(gap / 1.3);
    if gap < 1 then
      gap := 1;
    Sorted := True;
    for i := _Left to _Right - gap do
      if _CompareMeth(i, i + gap) > 0 then begin
        _SwapMeth(i, i + gap);
        Sorted := False;
      end;
  end;
end;

procedure CombSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler);
var
  i: Integer;
  gap: Integer;
  Sorted: Boolean;
begin
  gap := _Right - _Left + 1;
  Sorted := False;
  while (gap > 1) or not Sorted do begin
    gap := trunc(gap / 1.3);
    if gap < 1 then
      gap := 1;
    Sorted := True;
    for i := _Left to _Right - gap do
      if _DataHandler.Compare(i, i + gap) > 0 then begin
        _DataHandler.Swap(i, i + gap);
        Sorted := False;
      end;
  end;
end;

end.
