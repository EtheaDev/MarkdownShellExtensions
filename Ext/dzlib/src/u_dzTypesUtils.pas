unit u_dzTypesUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Types,
  SysUtils;

///<summary> Returns the Rect's width </summary>
function TRect_Width(const _Rect: TRect): Integer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

procedure TRect_SetWidth(var _Rect: TRect; _Value: Integer);
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary> Returns the Rect's height </summary>
function TRect_Height(const _Rect: TRect): Integer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

procedure TRect_SetHeight(var _Rect: TRect; _Value: Integer);
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary>
/// @returns a TRect generated from Left, Top, Width and Height </summary>
function TRect_FromLTWH(_l, _t, _w, _h: Integer): TRect;

procedure TRect_SetOffset(var _Rect: TRect; const _x, _y: Integer);
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary> returns the center point of the Rect </summary>
function TRect_Center(const _Rect: TRect): TPoint;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary>
/// Check whether a TRect contains a TPoint </summary>
function TRect_Contains(const _Rect: TRect; const _Pnt: TPoint): Boolean;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
overload;

///<summary>
/// Check whether a TRect contains a point with the given coordinates </summary>
function TRect_Contains(const _Rect: TRect; _x, _y: Integer): Boolean;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
overload;

///<summary>
/// Quicksort for the Array between Lo and Hi </summary>
procedure TSingleDynArray_Sort(var _Arr: TSingleDynArray; _Lo, _Hi: Integer);

///<summary>
/// Quicksort for the Array between Lo and Hi </summary>
procedure TDoubleDynArray_Sort(var _Arr: TDoubleDynArray; _Lo, _Hi: Integer);

implementation

function TRect_Width(const _Rect: TRect): Integer;
begin
  Result := _Rect.Right - _Rect.Left;
end;

procedure TRect_SetWidth(var _Rect: TRect; _Value: Integer);
begin
  _Rect.Right := _Rect.Left + _Value;
end;

function TRect_Height(const _Rect: TRect): Integer;
begin
  Result := _Rect.Bottom - _Rect.Top;
end;

procedure TRect_SetHeight(var _Rect: TRect; _Value: Integer);
begin
  _Rect.Bottom := _Rect.Top + _Value;
end;

function TRect_FromLTWH(_l, _t, _w, _h: Integer): TRect;
begin
  Result := Rect(_l, _t, _l + _w, _t + _h);
end;

procedure TRect_SetOffset(var _Rect: TRect; const _x, _y: Integer);
begin
  Inc(_Rect.Left, _x);
  Inc(_Rect.Right, _x);
  Inc(_Rect.Top, _y);
  Inc(_Rect.Bottom, _y);
end;

function TRect_Center(const _Rect: TRect): TPoint;
begin
  // in theory this can lead to an integer overflow, if the rect-coordinates are very large.
  Result := Point((_Rect.Left + _Rect.Right) div 2, (_Rect.Top + _Rect.Bottom) div 2);
end;

function TRect_Contains(const _Rect: TRect; const _Pnt: TPoint): Boolean;
begin
  Result := TRect_Contains(_Rect, _Pnt.x, _Pnt.y);
end;

function TRect_Contains(const _Rect: TRect; _x, _y: Integer): Boolean;
begin
  Result := (_Rect.Left <= _x) and (_Rect.Right >= _x)
    and (_Rect.Top <= _y) and (_Rect.Bottom >= _y);
end;

procedure TSingleDynArray_Sort(var _Arr: TSingleDynArray; _Lo, _Hi: Integer);
var
  Lo, Hi: Integer;
  Pivot, t: Single;
begin
  Lo := _Lo;
  Hi := _Hi;
  if Lo > Hi then
    Exit; //==>
  Pivot := _Arr[(Lo + Hi) div 2];
  repeat
    while _Arr[Lo] < Pivot do
      Inc(Lo);
    while _Arr[Hi] > Pivot do
      Dec(Hi);
    if Lo <= Hi then begin
      t := _Arr[Lo];
      _Arr[Lo] := _Arr[Hi];
      _Arr[Hi] := t;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if Hi > _Lo then
    TSingleDynArray_Sort(_Arr, _Lo, Hi);
  if Lo < _Hi then
    TSingleDynArray_Sort(_Arr, Lo, _Hi);
end;

procedure TDoubleDynArray_Sort(var _Arr: TDoubleDynArray; _Lo, _Hi: Integer);
var
  Lo, Hi: Integer;
  Pivot, t: Single;
begin
  Lo := _Lo;
  Hi := _Hi;
  if Lo > Hi then
    Exit; //==>
  Pivot := _Arr[(Lo + Hi) div 2];
  repeat
    while _Arr[Lo] < Pivot do
      Inc(Lo);
    while _Arr[Hi] > Pivot do
      Dec(Hi);
    if Lo <= Hi then begin
      t := _Arr[Lo];
      _Arr[Lo] := _Arr[Hi];
      _Arr[Hi] := t;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if Hi > _Lo then
    TDoubleDynArray_Sort(_Arr, _Lo, Hi);
  if Lo < _Hi then
    TDoubleDynArray_Sort(_Arr, Lo, _Hi);
end;

end.
