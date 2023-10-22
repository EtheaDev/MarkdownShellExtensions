{: This unit implements a Quicksort procedure that can
   be used to sort anything as well as a binary sarch
   function.
   @author(Thomas Mueller http://www.dummzeuch.de)
}
unit u_dzQuicksort;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Classes,
  SysUtils,
  u_dzSortUtils;

type
  ///<summary>
  /// raised in the sorted list templates instead of EListError so it is possible to
  /// specifically ignore these exceptions in the debugger if they are handled in code </summary>
  EdzListError = class(EListError)
  end;

type
  ///<sumary>
  /// Method pointer for passing to BinarySearch
  /// @param Key is the key to compare to as an untyped const parameter
  /// @param Index is the index of te item in the list to compare this key to.
  /// @returns  0 if Key = Item[Idx].Key
  ///           1 if Key > Item[Idx].Key
  ///          -1 if Key < Item[Idx].Key</summary>
  TCompareToItemMeth1 = function(const _Key; _Idx: Integer): Integer of object;

  ///<sumary>
  /// Method pointer for passing to BinarySearch
  /// @param Key is a pointer to the key to compare to
  /// @param Index is the index of te item in the list to compare this key to.
  /// @returns  0 if Key = Item[Idx].Key
  ///           1 if Key > Item[Idx].Key
  ///          -1 if Key < Item[Idx].Key</summary>
  TCompareToItemMeth2 = function(_Key: Pointer; _Idx: Integer): Integer of object;

///<summary>
/// Quicksort with two method pointers for comparing and swapping two elements.
/// The method pointer signatures are declared in u_dzSortUtils.
/// @longcode(##
///   Quicksort(0, Count-1, self.CompareItems, self.SwapItems);
/// ##) </summary>
procedure QuickSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth;
  _SwapMeth: TSwapItemsMeth); overload;

///<summary>
/// Quicksort with an ISortDataHandler interface that does the comparison and swapping
/// The ISortDataHandler interface are declared in u_dzSortUtils.
/// @longcode(##
///   Quicksort(0, Count-1, CompareAndSwapInterface);
/// ##) </summary>
procedure QuickSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler); overload;

///<summary>
/// Quicksort with an ISortDataHandlerEx interface that does the comparison and swapping
/// and also give the number of items to sort.
/// The ISortDataHandlerEx interface are declared in u_dzSortUtils.
/// @longcode(##
///   Quicksort(0, Count-1, CountCompareAndSwapInterface);
/// ##) </summary>
procedure QuickSort(_DataHandler: ISortDataHandlerEx); overload;

///<summary>
/// BinarySearch with a method pointer that compares an index to the Item sought.
/// @param Index contains the index, where the item was found (if Result = True),
///              or where it would be inserted (if Result = False)
/// @param Duplicates determines whether to check for duplicates in the list
///                   If True, Index will always be that of first item that matches the key.
///                   If False, Index will be any item that matches the key.
/// @returns True, if the item was found, False otherwise
/// @longcode(##
///   Found := BinarySearch(0, Count - 1, Idx, Key, Self.CompareToKey);
/// ##) </summary>
function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  const _Key; _CompareMeth: TCompareToItemMeth1;
  _Duplicates: Boolean = False): Boolean; overload;

///<summary>
/// BinarySearch with a method pointer that compares an index to the Item sought.
/// @param Index contains the index, where the item was found (if Result = True),
///              or where it would be inserted (if Result = False)
/// @param Duplicates determines whether to check for duplicates in the list
///                   If True, Index will always be that of first item that matches the key.
///                   If False, Index will be any item that matches the key.
/// @returns True, if the item was found, False otherwise
/// @longcode(##
///   Found := BinarySearch(0, Count - 1, Idx, Key, Self.CompareToKey);
/// ##) </summary>
function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  _Key: Pointer; _CompareMeth: TCompareToItemMeth2;
  _Duplicates: Boolean = False): Boolean; overload;

type
  ICompareToKey = interface ['{CEB61050-D71F-4F67-B9BC-FD496A079F75}']
    ///<summary>
    /// Compares the key (stored in the interface) to the the item with the given index.
    /// @param Index is the index of te item in the list to compare this key to.
    /// @returns  0 if Key = Item[Idx].Key
    ///           1 if Key > Item[Idx].Key
    ///          -1 if Key < Item[Idx].Key</summary>
    function CompareTo(_Idx: Integer): Integer;
  end;

type
  ICompareToKeyEx = interface(ICompareToKey)['{10673750-EE58-40E3-A144-CA9EF517EBCA}']
    function Count: Integer;
  end;

///<summary>
/// BinarySearch with an ICompareToKey interface that compares an index to the Item sought.
/// @param Index contains the index, where the item was found (if Result = True),
///              or where it would be inserted (if Result = False)
/// @param Duplicates determines whether to check for duplicates in the list
///                   If True, Index will always be that of first item that matches the key.
///                   If False, Index will be any item that matches the key.
/// @returns True, if the item was found, False otherwise
/// @longcode(##
///   Found := BinarySearch(0, Count - 1, Idx, Key, CompareToInt);
/// ##) } </summary>
function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  _CompareInt: ICompareToKey; _Duplicates: Boolean = False): Boolean; overload;

function BinarySearch(out _Index: Integer;
  _CompareExInt: ICompareToKeyEx; _Duplicates: Boolean = False): Boolean; overload;

implementation

procedure QuickSort(_DataHandler: ISortDataHandlerEx); overload;
begin
  QuickSort(0, _DataHandler.Count - 1, _DataHandler);
end;

procedure QuickSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler); overload;
var
  I, J, P: Integer;
begin
  if _Left >= _Right then
    Exit; //==>
  repeat
    I := _Left;
    J := _Right;
//    P := (_Left + _Right) shr 1;
    // Chosing the pivot element can make a big difference:
    // In my unit tests its factor 100 for the TestPartSortedPartReverse test
    P := GetPivot(I, J, _DataHandler);
    repeat
      while _DataHandler.Compare(I, P) < 0 do
        Inc(I);
      while _DataHandler.Compare(J, P) > 0 do
        Dec(J);
      if I <= J then begin
        if I < J then
          _DataHandler.Swap(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if _Left < J then
      QuickSort(_Left, J, _DataHandler);
    _Left := I;
  until I >= _Right;
end;

procedure QuickSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth;
  _SwapMeth: TSwapItemsMeth);
var
  I, J, P: Integer;
begin
  if _Left >= _Right then
    Exit; //==>
  repeat
    I := _Left;
    J := _Right;
//    P := (_Left + _Right) shr 1;
    // Chosing the pivot element can make a big difference:
    // In my unit tests its factor 100 for the TestPartSortedPartReverse test
    P := GetPivot(I, J, _CompareMeth);
    repeat
      while _CompareMeth(I, P) < 0 do
        Inc(I);
      while _CompareMeth(J, P) > 0 do
        Dec(J);
      if I <= J then begin
        if I < J then
          _SwapMeth(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if _Left < J then
      QuickSort(_Left, J, _CompareMeth, _SwapMeth);
    _Left := I;
  until I >= _Right;
end;

function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  const _Key; _CompareMeth: TCompareToItemMeth1;
  _Duplicates: Boolean = False): Boolean;
var
  p, c: LongInt;
begin
  Result := False;
  while _Left <= _Right do begin
    p := (_Left + _Right) shr 1;
    c := _CompareMeth(_Key, p);
    if c > 0 then
      _Left := p + 1
    else begin
      _Right := p - 1;
      if c = 0 then begin
        Result := True;
        if not _Duplicates then
          _Left := p;
      end;
    end;
  end;
  _Index := _Left;
end;

function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  _Key: pointer; _CompareMeth: TCompareToItemMeth2;
  _Duplicates: Boolean = False): Boolean;
var
  p, c: LongInt;
begin
  Result := False;
  while _Left <= _Right do begin
    p := (_Left + _Right) shr 1;
    c := _CompareMeth(_Key, p);
    if c > 0 then
      _Left := p + 1
    else begin
      _Right := p - 1;
      if c = 0 then begin
        Result := True;
        if not _Duplicates then
          _Left := p;
      end;
    end;
  end;
  _Index := _Left;
end;

function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  _CompareInt: ICompareToKey; _Duplicates: Boolean = False): Boolean;
var
  p, c: LongInt;
begin
  Result := False;
  while _Left <= _Right do begin
    p := (_Left + _Right) shr 1;
    c := _CompareInt.CompareTo(p);
    if c > 0 then
      _Left := p + 1
    else begin
      _Right := p - 1;
      if c = 0 then begin
        Result := True;
        if not _Duplicates then
          _Left := p;
      end;
    end;
  end;
  _Index := _Left;
end;

function BinarySearch(out _Index: Integer;
  _CompareExInt: ICompareToKeyEx; _Duplicates: Boolean = False): Boolean;
begin
  Result := BinarySearch(0, _CompareExInt.Count - 1, _Index, _CompareExInt, _Duplicates);
end;

end.

