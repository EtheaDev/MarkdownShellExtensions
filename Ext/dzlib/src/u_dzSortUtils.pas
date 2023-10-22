unit u_dzSortUtils;

{$INCLUDE 'dzlib.inc'}

interface

// To use any of the sorting algorithms, you need to supply either the two callbacks
// TCompareItemsMeth and TSwapItemsMeth or pass a class implementing IQSDataHandler
// ("QS" stands for "QuickSort" from the time when there was only one sorting algorithm)

type
  ///<summary>
  /// Compare items at the given indexes and return values similar to the standard Delphi
  /// Compare functions e.g. CompareStr or CompareText
  /// @returns 0 if they are equal
  ///          <0 if Item[Idx1] < Item[Idx2]
  ///          >0 if Item[Idx1] > Item[Idx2] </summary>
  TCompareItemsMeth = function(_Idx1, _Idx2: Integer): Integer of object;
  ///<summary>
  /// Swap the items at the given indexes </summary>
  TSwapItemsMeth = procedure(_Idx1, _Idx2: Integer) of object;

type
  ///<summary>
  /// Interface to be implemented for using the sorting algorithms </summary>
  ISortDataHandler = interface ['{C7B22837-F9C0-4228-A2E3-DC8BBF27DBA9}']
    /// Compare items at the given indexes and return values similar to the standard Delphi
    /// Compare functions e.g. CompareStr or CompareText
    /// @returns 0 if they are equal
    ///          <0 if Item[Idx1] < Item[Idx2]
    ///          >0 if Item[Idx1] > Item[Idx2] </summary>
    function Compare(_Idx1, _Idx2: Integer): Integer;
    ///<summary>
    /// Swap the items at the given indexes </summary>
    procedure Swap(_Idx1, _Idx2: Integer);
  end;

type
  ///<summary>
  /// Adds a Count method to the ISortDataHander interface </summary>
  ISortDataHandlerEx = interface(ISortDataHandler)['{C2DD7397-C0C3-4519-AA24-7FB2EF559DAD}']
    function Count: Integer;
  end;

type
  IQSDataHandler = ISortDataHandler
{$IFDEF SUPPORTS_DEPRECATED_TYPES}
  deprecated // use ISortDataHandler instead
{$ENDIF}
  ;

///<summary>
/// Try to optimize the pivot by chosing the median of Left, Right and ((Left + Right) shr 1) </summary>
function GetPivot(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth): Integer; {$IFDEF SupportsInline} inline; {$ENDIF} overload;
///<summary>
/// Try to optimize the pivot by chosing the median of Left, Right and ((Left + Right) shr 1) </summary>
function GetPivot(_Left, _Right: Integer; _DataHandler: ISortDataHandler): Integer; {$IFDEF SupportsInline} inline; {$ENDIF} overload;

implementation

function GetPivot(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth): Integer;
begin
  Result := (_Left + _Right) shr 1;
  // try to optimize the pivot by chosing the
  // median of Left, Right and Result:
  if _CompareMeth(_Left, Result) > 0 then begin
    if _CompareMeth(Result, _Right) > 0 then begin
      // Result is already the median
    end else if _CompareMeth(_Right, _Left) > 0 then begin
      Result := _Left;
    end else
      Result := _Right;
  end else begin
    if _CompareMeth(_Right, Result) > 0 then begin
      // Result is already the median
    end else if _CompareMeth(_Left, _Right) > 0 then begin
      Result := _Left;
    end else begin
      Result := _Right;
    end;
  end;
end;

function GetPivot(_Left, _Right: Integer; _DataHandler: ISortDataHandler): Integer;
begin
  Result := (_Left + _Right) shr 1;
  // try to optimize the pivot by chosing the
  // median of Left, Right and Result:
  if _DataHandler.Compare(_Left, Result) > 0 then begin
    if _DataHandler.Compare(Result, _Right) > 0 then begin
      // Result is already the median
    end else if _DataHandler.Compare(_Right, _Left) > 0 then begin
      Result := _Left;
    end else
      Result := _Right;
  end else begin
    if _DataHandler.Compare(_Right, Result) > 0 then begin
      // Result is already the median
    end else if _DataHandler.Compare(_Left, _Right) > 0 then begin
      Result := _Left;
    end else begin
      Result := _Right;
    end;
  end;
end;

end.
