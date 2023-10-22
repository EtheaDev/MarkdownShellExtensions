unit u_dzStringArrayUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes,
  u_dzTypes;

function StringArrayOf(const _arr: array of string): TStringArray;

function StringArrayCombine(_arr: TStringArray; const _Separator: string): string;
procedure StringArraySort(var _arr: TStringArray);

function TStringArray_Concat(const _Arr1, _Arr2: array of string): TStringArray;

procedure TStringArray_Append(var _arr: TStringArray; const _Value: string);

///<summary>
/// Deletes Count entries from Arr starting from Index.
/// @Note: It is allowed for Index > Length(Arr) and also Index+Count > Length(Arr)
/// @raises ERangeCheck if Index or Count < 0 </summary>
procedure TStringArray_Delete(var _arr: TStringArray; _Index: Integer; _Count: Integer);

function TStringArray_Contains(const _arr: TStringArray; const _s: string; out _Idx: Integer): Boolean; overload;
function TStringArray_Contains(const _arr: TStringArray; const _s: string): Boolean; overload;

function TStringArray_AsCsv(const _arr: TStringArray; const _Separator: string = ','): string;

function TStringArray_FromStrings(_sl: TStrings): TStringArray;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

function TStrings_AsStringArray(_st: TStrings): TStringArray;
procedure TStrings_AssignStringArray(_st: TStrings; _arr: TStringArray);
procedure TStrings_AppendStringArray(_st: TStrings; _arr: TStringArray);

///<summary>
/// Concatenate strings from index FromIdx to ToIdx in the array to a string, using the given separator
/// @param arr is the source array
/// @param Separator is a string which will used to separate the array items
/// @param FromIdx is the starting index, default is 0
/// @param ToIdx is the end index, can be negative meaning Length - ToIdx, so -1 means Length-1
/// @returns the concatenated string </summary>
//function TStringArray_ToString(const _arr: TStringArray; const _Separator: string;
//  _FromIdx: Integer = 0; _ToIdx: Integer = -1): string;

implementation

uses
  SysConst;

function StringArrayOf(const _arr: array of string): TStringArray;
var
  i: Integer;
  len: Integer;
begin
  len := Length(_arr);
  SetLength(Result, len);
  for i := 0 to len - 1 do
    Result[i] := _arr[i];
end;

function StringArrayCombine(_arr: TStringArray; const _Separator: string): string;
var
  i: Integer;
  len: Integer;
begin
  len := Length(_arr);
  if len = 0 then begin
    Result := '';
  end else begin
    Result := _arr[0];
    for i := 1 to len - 1 do begin
      Result := Result + _Separator + _arr[i];
    end;
  end;
end;

function TStrings_AsStringArray(_st: TStrings): TStringArray;
var
  cnt: Integer;
  i: Integer;
begin
  cnt := _st.Count;
  SetLength(Result, cnt);
  for i := 0 to cnt - 1 do
    Result[i] := _st[i];
end;

function TStringArray_FromStrings(_sl: TStrings): TStringArray;
begin
  Result := TStrings_AsStringArray(_sl);
end;

procedure TStrings_AssignStringArray(_st: TStrings; _arr: TStringArray);
begin
  _st.Clear;
  TStrings_AppendStringArray(_st, _arr);
end;

procedure TStrings_AppendStringArray(_st: TStrings; _arr: TStringArray);
var
  i: Integer;
begin
  for i := 0 to Length(_arr) - 1 do
    _st.Add(_arr[i]);
end;

procedure StringArraySort(var _arr: TStringArray);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    TStrings_AssignStringArray(sl, _arr);
    sl.Sort;
    _arr := TStrings_AsStringArray(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TStringArray_Delete(var _arr: TStringArray; _Index: Integer; _Count: Integer); overload;
var
  len: Integer;
  i: Integer;
begin
  if _Index < 0 then
    raise ERangeError.CreateRes(PResStringRec(@SRangeError));
  if _Count < 0 then
    raise ERangeError.CreateRes(PResStringRec(@SRangeError));

  len := Length(_arr);
  if _Index > len - 1 then begin
    // after the end of the array -> nothing to do
    Exit; //==>
  end;

  if _Index >= len - _Count then begin
    // delete from the end
    SetLength(_arr, _Index);
    Exit; //==>
  end;

  for i := _Index to len - _Count - 1 do begin
    _arr[i] := _arr[i + _Count];
  end;
  SetLength(_arr, len - _Count);
end;

function TStringArray_Contains(const _arr: TStringArray; const _s: string; out _Idx: Integer): Boolean;
var
  i: Integer;
begin
  for i := Low(_arr) to High(_arr) do begin
    if _arr[i] = _s then begin
      Result := True;
      Exit; //==>
    end;
  end;
  Result := False;
end;

function TStringArray_Contains(const _arr: TStringArray; const _s: string): Boolean;
var
  Idx: Integer;
begin
  Result := TStringArray_Contains(_arr, _s, Idx);
end;

function TStringArray_AsCsv(const _arr: TStringArray; const _Separator: string = ','): string;
var
  i: Integer;
  len: Integer;
begin
  len := Length(_arr);
  case len of
    0: Result := '';
    1: Result := _arr[0];
  else
    Result := _arr[0];
    for i := 1 to len - 1 do begin
      Result := Result + _Separator + _arr[i];
    end;
  end;
end;

function TStringArray_Concat(const _Arr1, _Arr2: array of string): TStringArray;
var
  Len1: Integer;
  Len2: Integer;
  i: Integer;
begin
  Len1 := Length(_Arr1);
  Len2 := Length(_Arr2);
  SetLength(Result, Len1 + Len2);
  for i := 0 to Len1 - 1 do
    Result[i] := _Arr1[i];
  for i := 0 to Len2 - 1 do
    Result[i + Len1] := _Arr2[i];
end;

procedure TStringArray_Append(var _arr: TStringArray; const _Value: string);
var
  len: Integer;
begin
  len := Length(_arr);
  SetLength(_arr, len + 1);
  _arr[len] := _Value;
end;

end.
