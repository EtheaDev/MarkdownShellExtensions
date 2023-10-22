unit u_dzTypes;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes,
  Types; // for $IF Declared(TBytes) and TStringDynArray

type
  // Fixed size signed and unsigned integer types
{$IF not declared(Int8)}
  Int8 = Shortint;
{$IFEND}
{$IF not declared(UInt8)}
  UInt8 = Byte;
{$IFEND}
{$IF not declared(Int16)}
  Int16 = Smallint;
{$IFEND}
{$IF not declared(UInt16)}
  UInt16 = Word;
{$IFEND}
{$IF not declared(Int32)}
  Int32 = Integer;
{$IFEND}
{$IF not declared(UInt32)}
  UInt32 = Cardinal;
{$IFEND}
  // Int64 is predefined
{$IF not declared(UInt64)}
  UInt64 = Int64;
{$IFEND}
  // UInt64 is predefined

{$IF not declared(PInt8)}
  PInt8 = ^Int8;
{$IFEND}
{$IF not declared(PUInt8)}
  PUInt8 = ^UInt8;
{$IFEND}
{$IF not declared(PInt16)}
  PInt16 = ^Int16;
{$IFEND}
{$IF not declared(PUInt16)}
  PUInt16 = ^UInt16;
{$IFEND}
{$IF not declared(PInt32)}
  PInt32 = ^Int32;
{$IFEND}
{$IF not declared(PUInt32)}
  PUInt32 = ^UInt32;
{$IFEND}
{$IF not declared(PInt64)}
  PInt64 = ^Int64;
{$IFEND}
{$IF not declared(PUInt64)}
  PUInt64 = ^UInt64;
{$IFEND}
{$IF SizeOf(Pointer) <> SizeOf(NativeInt)}
// In Delphi 2007 and older, the NativeInt declaration is wrong. It should always have the same size
// as a pointer.
type
  NativeInt = Int32;
{$IFEND}
{$IF not declared(NativeUInt) or (SizeOf(Pointer) <> SizeOf(NativeUInt))}
  NativeUInt = UInt32;
{$IFEND}
{$IF not declared(IntPtr))}
{$IF SizeOf(Pointer)=4}
  IntPtr = Int32;
{$ELSE}
  IntPtr = Int64;
{$IFEND}
{$IFEND}
{$IF not declared(UIntPtr)}
{$IF SizeOf(Pointer)=4}
  UIntPtr = UInt32;
{$ELSE}
  UIntPtr = UInt64;
{$IFEND}
{$IFEND}

{$IF not declared(MaxUInt32)}
const
  MaxUInt32 = $FFFFFFFF;
{$IFEND}

{$IF not declared(MaxInt32)}
const
  MaxInt32 = $7FFFFFFF;
{$IFEND}

{$IF not declared(MinInt64)}
const
{$IFDEF DELPHI2005_UP}
  MinInt64 = -$8000000000000000;
{$ELSE}
  // for Delphi 6 and 7 we need to increment it by one:
  MinInt64 = -$7FFFFFFFFFFFFFFF;
{$ENDIF}
{$IFEND}

{$IF not declared(MaxInt64)}
const
  MaxInt64 = $7FFFFFFFFFFFFFFF;
{$IFEND}

{$IF not declared(MinInt32)}
const
{$IFDEF DELPHI2005_UP}
// Delphi 7 does not like this: "Overflow in conversion or arithmetic operation"
// Delphi 6 seems to cope, but I wouldn't bet on it, so we leave it out
  MinInt32 = -$80000000;
{$ELSE}
  MinInt32 = -$7FFFFFFF;
{$ENDIF}
{$IFEND}

{$IF not declared(MaxUInt16)}
const
  MaxUInt16 = $FFFF;
{$IFEND}

{$IF not declared(MaxInt16)}
const
  MaxInt16 = $7FFF;
{$IFEND}

{$IF not declared(MinInt16)}
const
  MinInt16 = -$8000;
{$IFEND}

{$IF not declared(MaxUInt8)}
const
  MaxUInt8 = $FF;
{$IFEND}

{$IF not declared(MaxInt8)}
const
  MaxInt8 = $7F;
{$IFEND}

{$IF not declared(MinInt8)}
const
  MinInt8 = -$80;
{$IFEND}

type
  EdzException = class(Exception)
  end;

type
  TErrorHandlingEnum = (ehReturnFalse, ehRaiseException);

{$IF not Declared(RawByteString)}
type
  RawByteString = AnsiString;
{$IFEND}

type
{$IF not declared(TStringDynArray)}
  TStringDynArray = array of string;
{$IFEND}
  TStringArray = TStringDynArray;
  TRawByteStringArray = array of RawByteString;
  TIntegerArray = array of Integer;
  TSingleArray = array of Single;
  TDoubleArray = array of Double;
  TExtendedArray = array of Extended;
  TExtendedDynArray = TExtendedArray;
{$IF not Declared(TBytes)}
  TBytes = array of Byte;
{$IFEND}
  TUInt16Array = array of UInt16;

type
  TByteMatrix = array of array of Byte;
  TBitMatrix = array of array of Boolean;

  TSingleMatrix = array of array of Single;
  TDoubleMatrix = array of array of Double;

type
  TMethodPointer = procedure of object;

type
  TRectLTWH = record
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
  private
    function GetTopLeft: TPoint;
    procedure SetTopLeft(_TopLeft: TPoint);
    function GetBottomLeft: TPoint;
    procedure SetBottomLeft(const _BottomLeft: TPoint);
  public
{$ENDIF}
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
    procedure Assign(_Left, _Top, _Width, _Height: Integer); overload;
    procedure Assign(_a: TRect); overload;
    procedure AssignTLRB(_Left, _Top, _Right, _Bottom: Integer);
    procedure AssignTo(out _Left, _Top, _Width, _Height: Integer); overload;
    procedure AssignTo(out _a: TRect); overload;
    ///<summary>
    /// Gets and sets the top left coordinates keeping the size </summary>
    property TopLeft: TPoint read GetTopLeft write SetTopLeft;
    ///<summary>
    /// Gets and sets the bottom left coordinates keeping the size </summary>
    property BottomLeft: TPoint read GetBottomLeft write SetBottomLeft;
    function GetCenter: TPoint;
    function Right: Integer;
    function Bottom: Integer;
    class operator Implicit(_a: TRect): TRectLTWH;
    class operator Implicit(_a: TRectLTWH): TRect;
    class function FromLTWH(_Left, _Top, _Width, _Height: Integer): TRectLTWH; static;
{$ENDIF}
  end;

procedure TRectLTWH_Assign(var _LTWH: TRectLTWH; _Left, _Top, _Width, _Height: Integer); overload;
procedure TRectLTWH_Assign(var _LTWH: TRectLTWH; _a: TRect); overload;
procedure TRectLTWH_AssignTLRB(var _LTWH: TRectLTWH; _Left, _Top, _Right, _Bottom: Integer);

implementation

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
{ TRectLTWH }

procedure TRectLTWH.Assign(_Left, _Top, _Width, _Height: Integer);
begin
  Left := _Left;
  Top := _Top;
  Width := _Width;
  Height := _Height;
end;

procedure TRectLTWH.Assign(_a: TRect);
begin
  AssignTLRB(_a.Left, _a.Top, _a.Right, _a.Bottom);
end;

procedure TRectLTWH.AssignTLRB(_Left, _Top, _Right, _Bottom: Integer);
begin
  Assign(_Left, _Top, _Right - _Left, _Bottom - _Top);
end;

procedure TRectLTWH.AssignTo(out _Left, _Top, _Width, _Height: Integer);
begin
  _Left := Left;
  _Top := Top;
  _Width := Width;
  _Height := Height;
end;

procedure TRectLTWH.AssignTo(out _a: TRect);
begin
  _a := Rect(Left, Top, Left + Width, Top + Height);
end;

class function TRectLTWH.FromLTWH(_Left, _Top, _Width, _Height: Integer): TRectLTWH;
begin
  Result.Assign(_Left, _Top, _Width, _Height);
end;

function TRectLTWH.Right: Integer;
begin
  Result := Left + Width;
end;

function TRectLTWH.Bottom: Integer;
begin
  Result := Top + Height;
end;

function TRectLTWH.GetBottomLeft: TPoint;
begin
  Result := Point(Left, Top + Height);
end;

function TRectLTWH.GetCenter: TPoint;
begin
  Result := Point(Left + Width div 2, Top + Height div 2);
end;

procedure TRectLTWH.SetBottomLeft(const _BottomLeft: TPoint);
begin
  Left := _BottomLeft.X;
  Top := _BottomLeft.Y - Height;
end;

function TRectLTWH.GetTopLeft: TPoint;
begin
  Result.X := Left;
  Result.Y := Top;
end;

procedure TRectLTWH.SetTopLeft(_TopLeft: TPoint);
begin
  Left := _TopLeft.X;
  Top := _TopLeft.Y;
end;

class operator TRectLTWH.Implicit(_a: TRectLTWH): TRect;
begin
  Result := Rect(_a.Left, _a.Top, _a.Left + _a.Width, _a.Top + _a.Height);
end;

class operator TRectLTWH.Implicit(_a: TRect): TRectLTWH;
begin
  Result.Assign(_a);
end;
{$ENDIF}

procedure TRectLTWH_Assign(var _LTWH: TRectLTWH; _Left, _Top, _Width, _Height: Integer);
begin
  _LTWH.Left := _Left;
  _LTWH.Top := _Top;
  _LTWH.Width := _Width;
  _LTWH.Height := _Height;
end;

procedure TRectLTWH_Assign(var _LTWH: TRectLTWH; _a: TRect);
begin
  TRectLTWH_AssignTLRB(_LTWH, _a.Left, _a.Top, _a.Right, _a.Bottom);
end;

procedure TRectLTWH_AssignTLRB(var _LTWH: TRectLTWH; _Left, _Top, _Right, _Bottom: Integer);
begin
  TRectLTWH_Assign(_LTWH, _Left, _Top, _Right - _Left, _Bottom - _Top);
end;

end.
