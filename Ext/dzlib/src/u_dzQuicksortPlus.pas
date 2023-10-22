unit u_dzQuicksortPlus;

interface

uses
  u_dzSortUtils;

///<summary>
/// Optimized Quicksort which
/// * choses a pivot by selecting the median between elements Left, Right and (Left + Right) / 2
/// * uses InsertionSort for small sets, if Right - Left < Cutoff
/// https://en.wikipedia.org/wiki/Quicksort#Optimizations
/// @param Left is the leftmost index into the data
/// @param Right is the rightmost index into the data
/// @param CompareMeth is a method for comparing the data at the given indexes
/// @param SwapMeth is a method for swapping the data at the givven indexes </summary>
procedure QuicksortPlus(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth;
  _Cutoff: Integer = 15); overload;

///<summary>
/// Optimized Quicksort which
/// * choses a pivot by selecting the median between elements Left, Right and (Left + Right) / 2
/// * uses InsertionSort for small sets, if Right - Left < Cutoff
/// https://en.wikipedia.org/wiki/Quicksort#Optimizations
/// @param Left is the leftmost index into the data
/// @param Right is the rightmost index into the data
/// @param DataHandler implements the IQsDataHandler interface which supplies methods for
///                    comparing and swapping data
/// @param SwapMeth is a method for swapping the data at the givven indexes </summary>
procedure QuicksortPlus(_Left, _Right: Integer; _DataHandler: ISortDataHandler; _Cutoff: Integer = 15); overload;

implementation

uses
  u_dzInsertionSort;

procedure QuicksortPlus(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth;
  _Cutoff: Integer = 15);
var
  i, j, P: Integer;
begin
  if _Left >= _Right then
    Exit; //==>

  if _Right - _Left < _Cutoff then begin
  // I also tried CocktailSort and CombSort.
  // InsertionSort was the winner with the others being close behind:
  //                               sorted     reverse        half      random
  //CocktailSort:
  // QuickSort+(12)(10000000):    0,63257     0,64771     3,60685     1,72447
  // QuickSort+(15)(10000000):    0,61002     0,63970     3,54100     1,75077
  // QuickSort+(17)(10000000):    0,60974     0,63940     3,52119     1,76208
  // QuickSort+(20)(10000000):    0,57250     0,60263     3,54340     1,80887
  //CombSort:
  // QuickSort+(12)(10000000):    0,78495     0,81840     3,69471     1,86440
  // QuickSort+(15)(10000000):    0,78195     0,81936     3,75795     1,86730
  // QuickSort+(17)(10000000):    0,78141     0,81444     3,66137     1,83700
  // QuickSort+(20)(10000000):    0,77811     0,81060     3,73538     1,82990
  //InsertionSort:
  // QuickSort+(12)(10000000):    0,62492     0,65038     3,54591     1,71980
  // QuickSort+(15)(10000000):    0,64694     0,64918     3,52348     1,68722
  // QuickSort+(17)(10000000):    0,61865     0,65471     3,57409     1,70130
  // QuickSort+(20)(10000000):    0,57436     0,60499     3,51615     1,70527
    InsertionSort(_Left, _Right, _CompareMeth, _SwapMeth);
  end else begin
    repeat
      i := _Left;
      j := _Right;
      P := GetPivot(i, j, _CompareMeth);
      repeat
        while _CompareMeth(i, P) < 0 do
          Inc(i);
        while _CompareMeth(j, P) > 0 do
          Dec(j);
        if i <= j then begin
          if i < j then
            _SwapMeth(i, j);
          if P = i then
            P := j
          else if P = j then
            P := i;
          Inc(i);
          Dec(j);
        end;
      until i > j;
      if _Left < j then
        QuicksortPlus(_Left, j, _CompareMeth, _SwapMeth, _Cutoff);
      _Left := i;
      if (_Right - _Left < _Cutoff) and (_Left < _Right) then begin
        InsertionSort(_Left, _Right, _CompareMeth, _SwapMeth);
        Exit; //==>
      end;
    until i >= _Right;
  end;
end;

procedure QuicksortPlus(_Left, _Right: Integer; _DataHandler: ISortDataHandler; _Cutoff: Integer = 15);
var
  i, j, P: Integer;
begin
  if _Left >= _Right then
    Exit; //==>

  if _Right - _Left < _Cutoff then begin
    InsertionSort(_Left, _Right, _DataHandler);
  end else begin
    repeat
      i := _Left;
      j := _Right;
      P := GetPivot(i, j, _DataHandler);
      repeat
        while _DataHandler.Compare(i, P) < 0 do
          Inc(i);
        while _DataHandler.Compare(j, P) > 0 do
          Dec(j);
        if i <= j then begin
          if i < j then
            _DataHandler.Swap(i, j);
          if P = i then
            P := j
          else if P = j then
            P := i;
          Inc(i);
          Dec(j);
        end;
      until i > j;
      if _Left < j then
        QuicksortPlus(_Left, j, _DataHandler, _Cutoff);
      _Left := i;
      if (_Right - _Left < _Cutoff) and (_Left < _Right) then begin
        InsertionSort(_Left, _Right, _DataHandler);
        Exit; //==>
      end;
    until i >= _Right;
  end;
end;

end.
