unit u_dzCocktailSort;

interface

uses
  u_dzSortUtils;

// https://en.wikipedia.org/wiki/Cocktail_shaker_sort
procedure CocktailSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth); overload;
procedure CocktailSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler); overload;

implementation

procedure CocktailSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth);
var
  k: Integer;
  l: Integer;
  r: Integer;
  j: Integer;
begin
  l := _Left + 1;
  r := _Right;
  k := _Right;
  repeat
    for j := r downto l do begin
      if _CompareMeth(j - 1, j) > 0 then begin
        _SwapMeth(j - 1, j);
        k := j;
      end;
    end;
    l := k + 1;
    for j := l to r do begin
      if _CompareMeth(j - 1, j) > 0 then begin
        _SwapMeth(j - 1, j);
        k := j;
      end;
    end;
    r := k - 1;
  until l > r;
end;

procedure CocktailSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler); overload;
var
  k: Integer;
  l: Integer;
  r: Integer;
  j: Integer;
begin
  l := _Left + 1;
  r := _Right;
  k := _Right;
  repeat
    for j := r downto l do begin
      if _DataHandler.Compare(j - 1, j) > 0 then begin
        _DataHandler.Swap(j - 1, j);
        k := j;
      end;
    end;
    l := k + 1;
    for j := l to r do begin
      if _DataHandler.Compare(j - 1, j) > 0 then begin
        _DataHandler.Swap(j - 1, j);
        k := j;
      end;
    end;
    r := k - 1;
  until l > r;
end;

end.
