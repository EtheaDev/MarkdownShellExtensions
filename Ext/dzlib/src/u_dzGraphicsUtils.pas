unit u_dzGraphicsUtils;

{$INCLUDE 'dzlib.inc'}

{.$UNDEF OPTIMIZE_DZ_GRAPHIC_UTILS}
{.$UNDEF SUPPORTS_INLINE}

{$IFDEF DELPHI2005}
// the Delphi 2005 compiler crashes if this is compiled with typed @ operator
// turned on
{$TYPEDADDRESS OFF}
{$ENDIF}

{$IFDEF OPTIMIZE_DZ_GRAPHIC_UTILS}
{$OPTIMIZATION ON}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
{$ENDIF}

{$IFOPT O-} // Optimization
{$IFNDEF NO_OPTIMIZE_DZ_GRAPHIC_UTILS_HINT}
{$MESSAGE HINT 'optimization is off, consider turning it on for significantly better performance'}
{$ENDIF}
{$ELSE}
// If optimization is on, we turn off assertions, just in case the programmer forgot.
// The reason for this is that we have some assertions below that might significantly impact
// performance.
{$C-} // this is the short form for $ASSERTIONS OFF
{$ENDIF}

{$IFNDEF NO_OPTIMIZE_DZ_GRAPHIC_UTILS_HINT}
{$IFOPT Q+}
{$MESSAGE WARN 'Overflow checking is on, consider turning it off for significantly better performance'}
{$ENDIF}

{$IFOPT R+}
{$MESSAGE WARN 'Range checking is on, consider turning it off for significantly better performance'}
{$ENDIF}
{$ENDIF}

{.$DEFINE dzUseGraphics32}

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  Graphics,
{$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
{$ENDIF}
{$IFDEF dzUseGraphics32}
  GR32, // libs\graphics32\src
{$ENDIF}
  u_dzTranslator,
  u_dzTypes,
  u_dzConvertUtils,
  u_dzTypesUtils;

type
  EdzPixelFormatNotSupported = class(EdzException)
    constructor Create(_PixelFormat: TPixelFormat); overload;
  end;

type
  TRgbBrightnessChannelEnum = (rcbAverage, rcbFastLuminance, rcbRed, rcbGreen, rcbBlue, rcbLuminance);

type
  TVisualizationMethod = (tvmRainbow, tvmGrayscale);

const
  // Constant from GraphUtil (implementation section)
  HLSMAX = 240; // H,L, and S vary over 0-HLSMAX

type
{$IFDEF dzUseGraphics32}
  THlsValueType = Single; // 0..1
{$ELSE}
  THlsValueType = Word; // 0..HLSMAX (240)
{$ENDIF}
  THlsRec = record
    Hue: THlsValueType;
    Luminance: THlsValueType;
    Saturation: THlsValueType;
  end;

type
  TValueIdxTriple = (vitBlue, vitGreen, vitRed);

type
  PdzRgbTripleValues = ^TdzRgbTripleValues;
  TdzRgbTripleValues = packed array[TValueIdxTriple] of Byte;
  PdzRgbTriple = ^TdzRgbTriple;
  TdzRgbTriple = packed record
    // do not change the order of the fields, do not add any fields
    Blue: Byte;
    Green: Byte;
    Red: Byte;
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
    function GetValues(_Idx: TValueIdxTriple): Byte; inline;
    procedure SetValues(_Idx: TValueIdxTriple; _Value: Byte); overload; inline;
    procedure SetValues(_Red, _Green, _Blue: Byte); overload;
    function GetColor: TColor;
    ///<summary>
    /// Sets Blue, Green and Red for the given Color, supporting system colors in addition to RGB colors
    /// @Note this is marginally slower than SetRgbColor. </summary>
    procedure SetColor(_Color: TColor);
    ///<summary>
    /// Sets Blue, Green and Red for the given Color assuming that it is an RGB color and not a system color.
    /// @Note this is marginally faster than SetColor. </summary>
    procedure SetRgbColor(_Color: TColor);
    procedure SetGray(_Value: Byte);
    function GetLuminance: Byte;
    function GetFastLuminance: Byte; overload;
    class function GetFastLuminance(_Red, _Green, _Blue: Byte): Byte; overload; static; inline;
    function GetBrightness(_Channel: TRgbBrightnessChannelEnum): Byte;
    procedure SetBrightness(_Value: Byte); deprecated; //use SetGray
    procedure GetHls(out _Hls: THlsRec);
    procedure SetHls(const _Hls: THlsRec);
    property Values[_Idx: TValueIdxTriple]: Byte read GetValues write SetValues;
{$ENDIF}
  end;

function GetRgbBrightness(_Red, _Green, _Blue: Byte; _Channel: TRgbBrightnessChannelEnum): Byte;

function CalcBytesPerPixel(_PixelFormat: TPixelFormat): Integer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function CalcBytesPerLine(_Width, _BytesPerPixel: Integer): Integer; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
function CalcBytesPerLine(_Width: Integer; _PixelFormat: TPixelFormat): Integer; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
function CalcBytesPerLine(_Width: Integer; _bmp: TBitmap): Integer; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

procedure IncPtr(var _Ptr: Pointer; _Offset: IntPtr);
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function AddToPtr(const _Ptr: Pointer; _Offset: IntPtr): Pointer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function PtrDiff(const _Ptr1, _Ptr2: Pointer): IntPtr;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function TdzRgbTriple_GetFastLuminance(const _Triple: TdzRgbTriple): Byte;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

procedure TdzRgbTriple_SetColor(var _Triple: TdzRgbTriple; _Color: TColor);
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function GetFastLuminance(const _Red, _Green, _Blue: Byte): Byte;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

type
  TdzRgbTripleArray = packed array[0..MaxInt div SizeOf(TdzRgbTriple) - 1] of TdzRgbTriple;
  PdzRgbTripleArray = ^TdzRgbTripleArray;

type
  PdzRgbQuad = ^TdzRgbQuad;
  TdzRgbQuad = packed record
    // do not change the order of the fields, do not add any fields
    Blue: Byte;
    Green: Byte;
    Red: Byte;
    Reserved: Byte;
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
    function GetColor: TColor;
    procedure SetColor(_Color: TColor);
    procedure SetGray(_Value: Byte);
    function GetLuminance: Word;
    function GetFastLuminance: Word;
    function GetBrightness(_Channel: TRgbBrightnessChannelEnum): Word;
    procedure SetBrightness(_Value: Byte); deprecated; //use SetGray
    procedure GetHls(out _Hue, _Luminance, _Saturation: Word);
    procedure SetHls(_Hue, _Luminance, _Saturation: Word);
{$ENDIF}
  end;

type
  TdzRgbQuadArray = packed array[0..MaxInt div SizeOf(TdzRgbQuad) - 1] of TdzRgbQuad;
  PdzRgbQuadArray = ^TdzRgbQuadArray;

///<summary> Returns the bounding box of the active clipping region </summary>
function TCanvas_GetClipRect(_Canvas: TCanvas): TRect;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary> Sets a clipping rect, returns true, if the region is not empty, false if it is empty </summary>
function TCanvas_SetClipRect(_Canvas: TCanvas; _Rect: TRect): Boolean;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

type
  TDrawTextFlags = (
    dtfLeft, dtfRight, dtfCenter, // horizontal alignment
    dtfTopSingle, dtfBottomSingle, dtfVCenterSingle, // vertical alignment, only if dtfSingleLine is given
    dtfSingleLine, // only print as single line (ignore line breaks).
                   // Note that this is is not the same as not specifying dtfWordBreak because
                   // it will also ignore existing line breaks.
    dtfWordBreak, // Breaks words. Lines are automatically broken between words if a word would
                  // extend past the edge of the rectangle specified by the lpRect parameter.
                  // A carriage return-line feed sequence also breaks the line.
                  // If this is not specified, output is on one line.
    dtfCalcRect, // Determines the width and height of the rectangle. If there are multiple lines
                 // of text, DrawText uses the width of the rectangle pointed to by the lpRect
                 // parameter and extends the base of the rectangle to bound the last line of text.
                 // If the largest word is wider than the rectangle, the width is expanded.
                 // If the text is less than the width of the rectangle, the width is reduced.
                 // If there is only one line of text, DrawText modifies the right side of the
                 // rectangle so that it bounds the last character in the line. In either case,
                 // DrawText returns the height of the formatted text but does not draw the text.
    dtfPathEllipsis, // replace characters in the middle of the string with ellipses ('...') so that
                     // the result fits in the specified rectangle. If the string contains backslash
                     // (\) characters, preserves as much as possible of the text after the last backslash.
    dtfEndEllipsis, // if the end of a string does not fit in the rectangle, it is truncated and
                    // ellipses ('...') are added. If a word that is not at the end of the string
                    // goes beyond the limits of the rectangle, it is truncated without ellipses.
                    // (Unless dtfWordEllipsis is also specified.)
    dtfWordEllipsis, // Truncates any word that does not fit in the rectangle and adds ellipses ('...').
    dtfNoClip); // draw without clipping (slightly faster)
// not implemented:
//    dtfModifyStringEllipsis, // if given, together with one of the dtfXxxEllipsis flags, the
                             // string is modified to matcht the output.
//    dtfEditControl,
//    dtfExpandTabs, dtfExternalLeading, dtfHidePrefix, dtfInternal,
//    dtfNoFullWidthCharBreak, dtfNoPrefix,
//    dtfPrefixOnly, dtRtlReading, dtfTabStop,
  TDrawTextFlagSet = set of TDrawTextFlags;

  TDrawTextHorizontalAlignment = (dthaLeft, dthaRight, dthaCenter);
  TDrawTextVerticalAlignment = (dtvaTop, dtvaBottom, dtvaCenter);
  TDrawTextFlagsNoAlign = dtfCalcRect..dtfNoClip;
  TDrawTextFlagSetNoAlign = set of TDrawTextFlagsNoAlign;

///<summary>
/// Calculates the Rect necessary for drawing the text.
/// @returns the calculated height </summary>
function TCanvas_DrawText(_Canvas: TCanvas; const _Text: string; var _Rect: TRect; _Flags: TDrawTextFlagSet): Integer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function TCanvas_DrawTextSingleLine(_Canvas: TCanvas; const _Text: string; var _Rect: TRect;
  _HAlign: TDrawTextHorizontalAlignment; _VAlign: TDrawTextVerticalAlignment;
  _Flags: TDrawTextFlagSetNoAlign): Integer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

procedure TCanvas_DrawLine(_cnv: TCanvas; _x1, _y1, _x2, _y2: Integer); overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

procedure TCanvas_DrawLine(_cnv: TCanvas; _pnt1, _pnt2: TPoint); overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

procedure TCanvas_DrawHorizontalLine(_cnv: TCanvas; _x1, _x2, _y: Integer);
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

procedure TCanvas_DrawVerticalLine(_cnv: TCanvas; _x, _y1, _y2: Integer);
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary> calls Windows.SaveDC and returns an interface which will automatically call
///          Windows.RestoreDC when destroyed </summary>
function TCanvas_SaveDC(_Canvas: TCanvas): IInterface;

procedure TCanvas_DrawArrow(_Canvas: TCanvas; _From, _To: TPoint; _ArrowHeadLength: Integer = 15);

///<summary>
/// Draws an isoceles right triangle. The tip is either on the top or bottom, depending
/// on the sign of the Height parameter.
/// @param Canvas is the canvas to draw on
/// @param Tip is the coordinates of the vertex point
/// @param Height is the height of the triangle, if negative, the triangle is painted upside down </summary>
procedure TCanvas_DrawTriangle(_Canvas: TCanvas; _Tip: TPoint; _Height: Integer);
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function TCanvas_BitBlt(_Canvas: TCanvas; _DestRect: TRect; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Src: TBitmap; _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary> abbreviation for StretchBlt that takes TCanvas and TPoint values. </summary>
function dzStretchBlt(_DestCnv: TCanvas; _DestTopLeft: TPoint; _DestSize: TPoint;
  _SrcCnv: TCanvas; _SrcTopLeft: TPoint; _SrcSize: TPoint; _Rop: DWORD = SRCCOPY): BOOL; overload;
{$IFDEF SUPPORTS_INLINE}inline;
{$ENDIF}

function dzStretchBlt(_DestCnv: TCanvas; _DestLeft, _DestTop: Integer; _DestSize: TPoint;
  _SrcCnv: TCanvas; _SrcTopLeft: TPoint; _SrcSize: TPoint; _Rop: DWORD = SRCCOPY): BOOL; overload;
{$IFDEF SUPPORTS_INLINE}inline;
{$ENDIF}

///<summary> abbreviation for StretchBlt that takes TCanvas, TBitmap and TPoint values. </summary>
function dzStretchBlt(_DestCnv: TCanvas; _DestTopLeft: TPoint; _DestSize: TPoint;
  _SrcBmp: TBitmap; _SrcTopLeft: TPoint; _SrcSize: TPoint; _Rop: DWORD = SRCCOPY): BOOL; overload;
{$IFDEF SUPPORTS_INLINE}inline;
{$ENDIF}

///<summary> abbreviation for StretchBlt that takes TCanvas, TBitmap and TPoint values. </summary>
function dzStretchBlt(_DestCnv: TCanvas; _DestLeft, _DestTop: Integer; _DestSize: TPoint;
  _SrcBmp: TBitmap; _SrcTopLeft: TPoint; _SrcSize: TPoint; _Rop: DWORD = SRCCOPY): BOOL; overload;
{$IFDEF SUPPORTS_INLINE}inline;
{$ENDIF}

///<summary> abbreviation for StretchBlt that takes two TBitmap and TPoint values. </summary>
function dzStretchBlt(_DestBmp: TBitmap; _DestTopLeft: TPoint; _DestSize: TPoint;
  _SrcBmp: TBitmap; _SrcTopLeft: TPoint; _SrcSize: TPoint; _Rop: DWORD = SRCCOPY): BOOL; overload;
{$IFDEF SUPPORTS_INLINE}inline;
{$ENDIF}

///<summary> abbreviation for StretchBlt that takes TRect </summary>
function dzStretchBlt(_DestHandle: Hdc; _DestRect: TRect;
  _SrcHandle: Hdc; _SrcRect: TRect; _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary> abbreviation for StretchBlt that takes TCanvas and TRect </summary>
function dzStretchBlt(_DestCnv: TCanvas; _DestRect: TRect;
  _SrcHandle: Hdc; _SrcRect: TRect; _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary> abbreviation for StretchBlt that takes TRect and TBitmap </summary>
function dzStretchBlt(_DestHandle: Hdc; _DestRect: TRect;
  _Src: TBitmap; _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary> abbreviation for StretchBlt that takes TCanvas, TRect and TBitmap </summary>
function dzStretchBlt(_DestCnv: TCanvas; _DestRect: TRect;
  _Src: TBitmap; _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary>
/// Abbreviation for StretchBlt that takes two TBitmap, resizes and keeps the spect ratio,
/// using stretchmode HALFTONE (which usually gives the best quality but is a bit slower).
/// The original stretchmode and the brush origin are preserved.
/// https://msdn.microsoft.com/en-us/library/windows/desktop/dd145089(v=vs.85).aspx </summary>
function dzStretchBlt(_DestBmp, _SrcBmp: TBitmap; _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary> abbreviation for BitBlt that takes TPoint / TRect and TBitmap parameters </summary>
function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function dzBitBlt(_DestHandle: Hdc; _DestRect: TRect; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Src: TBitmap;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

procedure TBitmap_SetSize(_bmp: TBitmap; _Width, _Height: Integer);
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
function TBitmap_BitBlt(_DestBmp: TBitmap; _DestRect: TRect; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Src: TBitmap;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary> load a jpeg file and assign it to the bitmap </summary>
procedure TBitmap_LoadJpg(_bmp: TBitmap; const _JpgFn: string); overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
{$IF Declared(TBitmap32)}
procedure TBitmap_LoadJpg(_bmp: TBitmap32; const _JpgFn: string); overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
{$IFEND}

///<summary> save a bitmap as a jpeg file </summary>
procedure TBitmap_SaveJpg(_bmp: TBitmap; const _JpgFn: string);
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary>
/// Assign a buffer containg a bitmap in BGR 8 format to the TBitmap </summary>
procedure TBitmap_AssignBgr8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);

///<summary>
/// Assign a buffer containg a bitmap in RGB 8 format to the TBitmap
/// @NOTE: This is much slower than TBitmap_AssignBgr8, so if you have got the choice,
///        go with BGR 8 format. </summary>
procedure TBitmap_AssignRgb8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);

///<summary>
/// Assign a buffer containg a bitmap in Mono 8 format to the TBitmap with 24 bit colors </summary>
procedure TBitmap_AssignMono824(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);

///<summary>
/// Assign a buffer containing a bitmap in Mono 8 format to a 8 bit gray scale TBitmap </summary>
procedure TBitmap_AssignMono8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean); overload;
procedure TBitmap_AssignMono8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean; _RowStride: Int64); overload;

type
  ///<summary>
  /// Converts the value at the given position to a Byte and increments BufPtr to point to the
  /// next value </summary>
  TBufferBitsToMono8Func = function(var _BufPtr: Pointer): Byte;
  TBufferBitsToMono8Meth = function(var _BufPtr: Pointer): Byte of object;

///<summary>
/// Converts a 12 bit value at the given position to a Byte and increments BufPtr by 2 </summary>
function BufferBits12ToMono8(var _BufPtr: Pointer): Byte;

///<summary>
/// Assign a buffer containing a bitmap in Monochrome format to a 8 bit gray scale TBitmap
/// @param BufferBitsToMono8Func is a callback function that converts the value at a given
///                              position to a Byte and increments the position to point to
///                              the next value.
/// @param RowStride (optional) is the number of bytes in Buffer for one row. If 0 it is assumed
///                  that BufferBitsToMono8 will increment Buffer correctly </summary>
procedure TBitmap_AssignToMono8(_BufferBitsToMono8Func: TBufferBitsToMono8Func;
  _Buffer: Pointer; _bmp: TBitmap; _YIsReversed: Boolean; _RowStride: Int64 = 0); overload;
procedure TBitmap_AssignToMono8(_BufferBitsToMono8Meth: TBufferBitsToMono8Meth;
  _Buffer: Pointer; _bmp: TBitmap; _YIsReversed: Boolean; _RowStride: Int64 = 0); overload;

///<summary>
/// converts a pf24bit or pf32bit monochrome bitmap to a pf8bit monochrome bitmap </summary>
function TBitmap_MonoToMono8(_bmp: TBitmap): TBitmap; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
procedure TBitmap_MonoToMono8(_InBmp, _OutBmp: TBitmap); overload;

///<summary>
/// Makes the given bitmap pf8Bit grayscale </summary>
procedure TBitmap_MakeMono8(_bmp: TBitmap);
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary>
/// Create an empty Mono8 TBitmap </summary>
function TBitmap_CreateMono8: TBitmap;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary>
/// Calculates the positive y coordinate for the given x coordinate for an ellipse
/// with horizontal and vertical axes, centered on the coordinate origin (0/0).
/// @param a, b are horizontal and vertical radius values
/// @param x is the x coordinate to calculate the coordinate for
/// @param y returns the y coordinate if it can be calculated
/// @returns true if the x coordinate was inside the ellipse, false if not </summary>
function TryCalcEllipsePoint(_a, _b, _x: Extended; out _y: Extended): Boolean;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary>
/// Calculates both y coordinates for the given x coordinate for an ellipse
/// with horizontal and vertical axes and the given center.
/// @aram x0, y0 are the coordinates of the ellipsis center
/// @param a, b are horizontal and vertical radius values
/// @param x is the x coordinate to calculate the coordinate for
/// @param y1, y2 return the y coordinates if they can be calculated
/// @returns true if the x coordinate was inside the ellipse, false if not </summary>
function TryCalcEllipsePoints(_x0, _y0, _a, _b, _x: Extended; out _y1, _y2: Extended): Boolean;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary>
/// Blurs a rectangular area in the given bitmap.
/// @param bmp is the bitmap to process
/// @param Minx, MaxX, MinY, MaxY describe the area to blur
/// @param passes is the number of blurring passes </summary>
procedure TBitmap_BlurRect(_bmp: TBitmap; _Left, _Top, _Right, _Bottom: Integer; _Passes: Integer);

///<summary>
/// Blurs an elliptic area in the given bitmap.
/// @param bmp is the bitmap to process
/// @param Minx, MaxX, MinY, MaxY describe the area to blur
/// @param passes is the number of blurring passes </summary>
procedure TBitmap_BlurEllipse(_bmp: TBitmap; _Left, _Top, _Right, _Bottom: Integer; _Passes: Integer);

///<summary>
/// Sharpens a bitmap, pixelformat must be pf24bit
/// @param SrcBmp is the input
/// @param DstBmp is the output (the sharpened picture)
/// @param Alpha is the sharpen factor, must be >=0 and <=5
/// source: https://www.swissdelphicenter.ch/en/showcode.php?id=1948
///         but changed alpha interval from ]1..6[ to [0..5]  </summary>
procedure TBitmap8_Sharpen(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single); overload;
procedure TBitmap24_Sharpen(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single); overload;
procedure TBitmap_Sharpen(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single); overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary>
/// Sharpens a bitmap, pixelformat must be pf24bit
/// @param SrcBmp is the input
/// @param DstBmp is the output (the sharpened picture)
/// @param Alpha is a matrix of sharpen factors for each pixel
///        Dimensions must match the bitmap, *remember* *that* *y* *is* *reversed*.
///        Values must be >= 0 and <=5 </summary>
procedure TBitmap8_Sharpen(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix); overload;
procedure TBitmap24_Sharpen(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix); overload;
procedure TBitmap_Sharpen(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix); overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary>
/// Balances the brightness of the SrcBmp bitmap and returns the result in the DstBmp bitmap
/// If possible, the contrast will also be maximized using histogram stretching.
/// @param SrcBmp ist the input bitmap which will remain unchanged
/// @param DstBmp will be filled with the result. The Result will be an 8 bit grayscal bitmap.
/// @param Offset gives the radius of the area used to calculage the average brightness for each
///               pixel. The default is 3*64 (=192)
/// @NOTE: This currently leaves an outer frame of Offset pixels unchanged. A future version will
///        probably also process that area. </summary>
procedure TBitmap8_BalanceBrightness(_SrcBmp, _DstBmp: TBitmap; _Offset: Word = 3 * 64); // 192

///<summary>
/// Calculates the average brightness of a bitmap with PixelFormat = pf8Bit
/// @param bmp is the bitmap to process
/// @param LowCutoff is the lower brightness limit for pixels to be included in the calculation
/// @param HighCutoff is the upper brightness limit for pixels to be included in the calculation
/// @param Average returns the calculated average, only valid if Result = True
/// @returns True, if at least on pixel was in the desired interval
///          False, if not </summary>
function TBitmap8_TryCalcAverage(_bmp: TBitmap; _LowCutoff, _HighCutoff: Byte;
  out _Average: Byte): Boolean;

///<summary>
/// Calculates the average brightness of a bitmap with PixelFormat = pf24Bit
/// @param bmp is the bitmap to process
/// @param LowCutoff is the lower brightness limit for pixels to be included in the calculation
/// @param HighCutoff is the upper brightness limit for pixels to be included in the calculation
/// @param Channel determines how to calculate the brightness
/// @param Average returns the calculated average, only valid if Result = True
/// @returns True, if at least on pixel was in the desired interval
///          False, if not </summary>
function TBitmap24_TryCalcAverage(_bmp: TBitmap; _LowCutoff, _HighCutoff: Byte;
  _Channel: TRgbBrightnessChannelEnum;
  out _Average: Byte): Boolean;

///<summary>
/// Calculates the average brightness of a bitmap with PixelFormat = pf24Bit thereby only
/// using the blue channel.
/// @param bmp is the bitmap to process
/// @param LowCutoff is the lower brightness limit for pixels to be included in the calculation
/// @param HighCutoff is the upper brightness limit for pixels to be included in the calculation
/// @param Average returns the calculated average, only valid if Result = True
/// @returns True, if at least on pixel was in the desired interval
///          False, if not </summary>
function TBitmap24_TryCalcAverageBlue(_bmp: TBitmap; _LowCutoff, _HighCutoff: Byte;
  out _Average: Byte): Boolean;

type
  TUInt32Array256 = array[0..255] of UInt32;
  TUInt64Array256 = array[0..255] of UInt64;

///<summary>
/// Calculate the histogram for a bitmap with PixelFormat = pf24 for the given channel
/// @param bmp is the bitmap to process
/// @param Channel determines how to calculate the brightness
/// @returns a TUInt64Array256 containing the histogram </summary>
function TBitmap24_GetHistogram(_bmp: TBitmap; _Channel: TRgbBrightnessChannelEnum): TUInt64Array256;
///<summary>
/// Calculate the histograms for red, green and blue for a bitmap with PixelFormat = pf24
/// @param bmp is the bitmap to process
/// @param Red returns the histogram for the red channel
/// @param Green returns the histogram for the green channel
/// @param Blue returns the histogram for the blue channel </summary>
procedure TBitmap24_GetHistograms(_bmp: TBitmap; out _Red, _Green, _Blue: TUInt64Array256); overload;
///<summary>
/// Calculate the histograms for red, green, blue and brightness for a bitmap with PixelFormat = pf24
/// @param bmp is the bitmap to process
/// @param Channel determines how to calculate the brightness
/// @param Red returns the histogram for the red channel
/// @param Green returns the histogram for the green channel
/// @param Blue returns the histogram for the blue channel
/// @param Blue returns the histogram for the selected brightness channel </summary>
procedure TBitmap24_GetHistograms(_bmp: TBitmap; _BrightnessChannel: TRgbBrightnessChannelEnum;
  out _Red, _Green, _Blue, _Brightness: TUInt64Array256); overload;

///<summary>
/// Calcluates the histogram of a grayscale 8, 24 or 32 bit bitmap </summary>
function TBitmapMono_GetHistogram(_bmp: TBitmap): TUInt64Array256; overload;
procedure TBitmapMono_GetHistogram(_bmp: TBitmap; out _Histogram: TUInt64Array256; out _Average: UInt8); overload;

function TBitmap8_GetHistogram(_bmp: TBitmap): TUInt64Array256; overload; deprecated; // use TBitmapMono_GetHistogram
///<summary>
/// Calculates the histogram for an 8 bit grayscale bitmap
/// @param bmp is the bitmap to analyze
/// @param Histogram is an array which will be filled with the histogram
/// @param Average is set to the average brightness of the bitmap
/// todo: This should maybe also (or instead) return the Median </summary>
procedure TBitmap8_GetHistogram(_bmp: TBitmap; out _Histogram: TUInt64Array256;
  out _Average: UInt8); overload; deprecated; // use TBitmapMono_GetHistogram

type
  // Note: The bitmap is stored upside down, so the y coordinates are reversed!
  TPixel24FilterCallback = procedure(_x, _y: Integer; var _Pixel: TdzRgbTriple) of object;
  TPixel8FilterCallback = procedure(_x, _y: Integer; var _Pixel: Byte) of object;

///<summary>
/// Calls the given callback procedure for each pixel of the bitmap </summary>
procedure TBitmap24_FilterPixels(_SrcBmp, _DstBmp: TBitmap; _Callback: TPixel24FilterCallback);
procedure TBitmap8_FilterPixels(_SrcBmp, _DstBmp: TBitmap; _Callback: TPixel8FilterCallback);

type
  TGammaCurve = array[0..255] of Byte;

///</summary>
/// Apply the given gamma curves to the respctive colors of the bitmap </summary>
procedure TBitmap24_ApplyGamma(_SrcBmp, _DstBmp: TBitmap; const _GammaRed, _GammaGreen, _GammaBlue: TGammaCurve); overload;

///</summary>
/// Apply the given gamma curve to all colors of the bitmap </summary>
procedure TBitmap24_ApplyGamma(_SrcBmp, _DstBmp: TBitmap; const _Gamma: TGammaCurve); overload;

///</summary>
/// Apply the given gamma curve to an 8 bit gray scale bitmap </summary>
procedure TBitmap8_ApplyGamma(_SrcBmp, _DstBmp: TBitmap; const _Gamma: TGammaCurve);

///</summary>
/// Apply the given gamma curve to a 24 bit gray scale bitmap and convert it to 8 bit
/// in the process.
/// @param SrcBmp is the source bitmap, must have PixelFormat pf24bit and be grayscale
///               The algoritm only takes the blue channel for the conversion.
/// @param DstBmp is the destinateion bitmap, it will be set to the same size as the source
///               and to PixelFormat pf8bit and a gray scale palette.
/// @param Gamma is an array representing the gamma curve to apply. </summary>
procedure TBitmap24_ApplyGammaTo8(_SrcBmp, _DstBmp: TBitmap; const _Gamma: TGammaCurve);

///</summary>
/// Sequentially Apply the given gamma curves to an 8 bit gray scale bitmap </summary>
procedure TBitmap8_ApplyMultiGamma(_SrcBmp, _DstBmp: TBitmap; const _GammaArr: array of TGammaCurve);

type
  ///<summary>
  /// PixelFilter for cutting off R, G and B values at the given CutOff value
  /// This reduches brightness of white pixels by cutting of brightness at a given value.
  /// Assumes a gray scale bitmap where white means R=255, G=255 and B=255. </summary>
  TPixelFilterCutoff = class
  private
    FCutOff: Byte;
  public
    constructor Create(_CutOff: Byte);
    // Note: The bitmap is stored upside down, so the y coordinates are reversed!
    procedure FilterCallback(_x, _y: Integer; var _Pixel: TdzRgbTriple); overload;
    procedure FilterCallback(_x, _y: Integer; var _Pixel: Byte); overload;
  end;

type
  TPixelFilterStretch = class
  private
    FLowCutOff: Byte;
    FHighCutOff: Byte;
    FDivisor: Integer;
    procedure StretchColor(var _Color: Byte);
{$IFDEF SUPPORTS_INLINE}
    inline;
{$ENDIF}
  public
    constructor Create(_LowCutoff, _HighCutoff: Byte);
    procedure FilterCallback(_x, _y: Integer; var _Pixel: TdzRgbTriple); overload;
    procedure FilterCallback(_x, _y: Integer; var _Pixel: Byte); overload;
  end;

type
  TPixelFilterMove = class
  private
    FMoveBy: Integer;
    procedure MoveColor(var _Color: Byte);
{$IFDEF SUPPORTS_INLINE}
    inline;
{$ENDIF}
  public
    constructor Create(_MoveBy: Integer);
    procedure FilterCallback(_x, _y: Integer; var _Pixel: TdzRgbTriple); overload;
    procedure FilterCallback(_x, _y: Integer; var _Pixel: Byte); overload;
  end;

type
  TNumColors = 1..256;

function MakeGrayPalette(_NumColors: TNumColors = 256): HPALETTE;

///<summary>
// Calculates the (perceived) brightness of an RGB color value (luminance) </summary>
function ColorBrightness(_Red, _Green, _Blue: Byte): Byte; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
///<summary>
// Calculates the (perceived) brightness of a TColor value (luminance) </summary>
function ColorBrightness(_Color: TColor): Byte; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

///<summary>
/// @returns clWhite or clBlack depending on the brightness (luminance) of the color </summary>
function BestForegroundForColor(_Red, _Green, _Blue: Byte): TColor; overload;
///<summary>
/// @returns clWhite or clBlack depending on the brightness (luminance) of the color </summary>
function BestForegroundForColor(_Color: TColor): TColor; overload;

///<summary>
/// @param Hue is a value between 0 and 1 </summary>
function RainbowColor(_Hue: Double): TColor; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
///<summary>
/// @param Hue is a value between 0 and 1 </summary>
procedure RainbowColor(_Hue: Double; out _Color: TdzRgbTriple); overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
{$ENDIF}
///<summary>
/// @param Brightness is a grayscale value </summary>
function RainbowColor(_Brightness: Byte): TColor; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
procedure RainbowColor(_Brightness: Byte; out _Red, _Green, _Blue: Byte); overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
procedure RainbowColor(_Brightness: Byte; out _Pixel: TdzRgbTriple); overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
function RainbowColor(_MinHue, _MaxHue, _Hue: Integer): TColor; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

function TryStr2Color(const _s: string; out _Color: TColor): Boolean;

function TPicture_TryLoadMatchingFile(_pic: TPicture; const _FileMask: string): Boolean;

function TPicture_TryLoadFromResource(_pic: TPicture; const _ResName: string): Boolean;

implementation

uses
  Math,
  TypInfo,
  jpeg, // if you get a compile error here you might need to add Vcl.Imaging to the unit scope names
{$IFDEF HAS_UNIT_PNGIMAGE}
  pngimage, // support for TImage.LoadGraphics for PNG files
{$ENDIF}
  GraphUtil,
  u_dzFileUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

function dzStretchBlt(_DestCnv: TCanvas; _DestTopLeft: TPoint; _DestSize: TPoint;
  _SrcCnv: TCanvas; _SrcTopLeft: TPoint; _SrcSize: TPoint; _Rop: DWORD): BOOL;
begin
  Result := StretchBlt(_DestCnv.Handle,
    _DestTopLeft.x, _DestTopLeft.y,
    _DestSize.x, _DestSize.y,
    _SrcCnv.Handle,
    _SrcTopLeft.x, _SrcTopLeft.y,
    _SrcSize.x, _SrcSize.y, _Rop);
end;

function dzStretchBlt(_DestCnv: TCanvas; _DestLeft, _DestTop: Integer; _DestSize: TPoint;
  _SrcCnv: TCanvas; _SrcTopLeft: TPoint; _SrcSize: TPoint; _Rop: DWORD = SRCCOPY): BOOL;
begin
  Result := StretchBlt(_DestCnv.Handle,
    _DestLeft, _DestTop,
    _DestSize.x, _DestSize.y,
    _SrcCnv.Handle,
    _SrcTopLeft.x, _SrcTopLeft.y,
    _SrcSize.x, _SrcSize.y, _Rop);
end;

function dzStretchBlt(_DestCnv: TCanvas; _DestTopLeft: TPoint; _DestSize: TPoint;
  _SrcBmp: TBitmap; _SrcTopLeft: TPoint; _SrcSize: TPoint; _Rop: DWORD = SRCCOPY): BOOL;
begin
  Result := dzStretchBlt(_DestCnv, _DestTopLeft, _DestSize,
    _SrcBmp.Canvas, _SrcTopLeft, _SrcSize, _Rop);
end;

function dzStretchBlt(_DestCnv: TCanvas; _DestLeft, _DestTop: Integer; _DestSize: TPoint;
  _SrcBmp: TBitmap; _SrcTopLeft: TPoint; _SrcSize: TPoint; _Rop: DWORD = SRCCOPY): BOOL;
begin
  Result := dzStretchBlt(_DestCnv, _DestLeft, _DestTop, _DestSize,
    _SrcBmp.Canvas, _SrcTopLeft, _SrcSize, _Rop);
end;

function dzStretchBlt(_DestBmp: TBitmap; _DestTopLeft: TPoint; _DestSize: TPoint;
  _SrcBmp: TBitmap; _SrcTopLeft: TPoint; _SrcSize: TPoint; _Rop: DWORD = SRCCOPY): BOOL; overload;
begin
  Result := dzStretchBlt(_DestBmp.Canvas, _DestTopLeft, _DestSize,
    _SrcBmp.Canvas, _SrcTopLeft, _SrcSize, _Rop);
end;

function dzStretchBlt(_DestHandle: Hdc; _DestRect: TRect; _SrcHandle: Hdc; _SrcRect: TRect; _Rop: DWORD): LongBool;
begin
  Result := StretchBlt(_DestHandle, _DestRect.Left, _DestRect.Top, TRect_Width(_DestRect), TRect_Height(_DestRect),
    _SrcHandle, _SrcRect.Left, _SrcRect.Top, TRect_Width(_SrcRect), TRect_Height(_SrcRect), _Rop);
end;

function dzStretchBlt(_DestCnv: TCanvas; _DestRect: TRect; _SrcHandle: Hdc; _SrcRect: TRect; _Rop: DWORD): LongBool;
begin
  Result := dzStretchBlt(_DestCnv.Handle, _DestRect, _SrcHandle, _SrcRect, _Rop);
end;

function dzStretchBlt(_DestHandle: Hdc; _DestRect: TRect; _Src: TBitmap; _Rop: DWORD): LongBool;
begin
  Result := StretchBlt(_DestHandle, _DestRect.Left, _DestRect.Top, TRect_Width(_DestRect), TRect_Height(_DestRect),
    _Src.Canvas.Handle, 0, 0, _Src.Width, _Src.Height, _Rop);
end;

function dzStretchBlt(_DestCnv: TCanvas; _DestRect: TRect; _Src: TBitmap; _Rop: DWORD): LongBool;
begin
  Result := dzStretchBlt(_DestCnv.Handle, _DestRect, _Src, _Rop);
end;

function dzStretchBlt(_DestBmp, _SrcBmp: TBitmap; _Rop: DWORD = SRCCOPY): LongBool;
var
  DstHandle: Hdc;
  OrigBltMode: Integer;
  OrigBrushOrigin: TPoint;
  wSrc: Integer;
  hSrc: Integer;
  x: Int64;
  y: Integer;
  wDst: Integer;
  hDst: Integer;
begin
  wDst := _DestBmp.Width;
  hDst := _DestBmp.Height;
  if wDst = 0 then
    raise Exception.Create(_('Destination bitmap width must not be 0.'));
  if hDst = 0 then
    raise Exception.Create(_('Destination bitmap height must not be 0.'));

  DstHandle := _DestBmp.Canvas.Handle;
  OrigBltMode := GetStretchBltMode(DstHandle);
  try
    SetBrushOrgEx(DstHandle, 0, 0, @OrigBrushOrigin);
    SetStretchBltMode(DstHandle, HALFTONE);
    wSrc := _SrcBmp.Width;
    hSrc := _SrcBmp.Height;
    if (hSrc = 0) or (wSrc = 0) then begin
      // SrcBmp is empty, nothing to do
      // todo: Should this clear DestBmp?
      Result := False;
    end else begin
      if SameValue(wSrc / hSrc, wDst / hDst) then begin
        x := 0;
        y := 0;
      end else begin
        if hSrc > wSrc then begin
          x := Round((wDst * (hSrc - wSrc)) / 2 / hSrc);
          y := 0;
          wDst := Round(wDst * wSrc / hSrc);
        end else begin
          x := 0;
          y := Round((hDst * (wSrc - hSrc)) / 2 / wSrc);
          hDst := Round(hDst * hSrc / wSrc);
        end;
      end;
      Result := dzStretchBlt(DstHandle, Rect(x, y, x + wDst - 1, y + hDst - 1), _SrcBmp);
    end;
  finally
    SetStretchBltMode(DstHandle, OrigBltMode);
    SetBrushOrgEx(DstHandle, OrigBrushOrigin.x, OrigBrushOrigin.y, nil);
  end;
end;

function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD): LongBool;
begin
  Result := Windows.BitBlt(
    _DestHandle,
    _DestPos.x, _DestPos.y,
    _Size.x, _Size.y,
    _Src.Canvas.Handle,
    _SrcPos.x, _SrcPos.y, _Rop);
end;

function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap;
  _Rop: DWORD = SRCCOPY): LongBool;
begin
  Result := Windows.BitBlt(
    _DestHandle,
    _DestPos.x, _DestPos.y,
    _Size.x, _Size.y,
    _Src.Canvas.Handle,
    0, 0, _Rop);
end;

function dzBitBlt(_DestHandle: Hdc; _DestRect: TRect; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD): LongBool;
begin
  Result := Windows.BitBlt(
    _DestHandle,
    _DestRect.Left, _DestRect.Top,
    _DestRect.Right - _DestRect.Left + 1, _DestRect.Bottom - _DestRect.Top + 1,
    _Src.Canvas.Handle,
    _SrcPos.x, _SrcPos.y, _Rop);
end;

function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD): LongBool;
begin
  Result := Windows.BitBlt(
    _DestHandle,
    _DestPos.x, _DestPos.y,
    _Src.Width, _Src.Height,
    _Src.Canvas.Handle,
    _SrcPos.x, _SrcPos.y,
    _Rop);
end;

function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Src: TBitmap; _Rop: DWORD): LongBool; overload;
begin
  Result := Windows.BitBlt(
    _DestHandle,
    _DestPos.x, _DestPos.y,
    _Src.Width, _Src.Height,
    _Src.Canvas.Handle,
    0, 0,
    _Rop);
end;

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool;
begin
  Result := dzBitBlt(
    _Canvas.Handle,
    _DestPos,
    _Size,
    _Src,
    _SrcPos,
    _Rop);
end;

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap;
  _Rop: DWORD = SRCCOPY): LongBool;
begin
  Result := dzBitBlt(_Canvas.Handle,
    _DestPos,
    _Size,
    _Src,
    _Rop);
end;

function TCanvas_BitBlt(_Canvas: TCanvas; _DestRect: TRect; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool;
begin
  Result := dzBitBlt(
    _Canvas.Handle,
    _DestRect,
    _Src,
    _SrcPos,
    _Rop);
end;

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool;
begin
  Result := dzBitBlt(
    _Canvas.Handle,
    _DestPos,
    _Src,
    _SrcPos,
    _Rop);
end;

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Src: TBitmap; _Rop: DWORD = SRCCOPY): LongBool;
begin
  Result := dzBitBlt(
    _Canvas.Handle,
    _DestPos,
    _Src,
    Point(0, 0),
    _Rop);
end;

function TCanvas_GetClipRect(_Canvas: TCanvas): TRect;
var
  RGN: THandle;
  Res: Integer;
begin
  RGN := CreateRectRgn(0, 0, 0, 0);
  if RGN = 0 then
    raise Exception.Create(_('CreateRectRgn failed'));
  try
    Res := GetClipRgn(_Canvas.Handle, RGN);
    if Res = -1 then
      raise Exception.Create(_('GetClipRgn failed'));
    GetRgnBox(RGN, Result);
  finally
    DeleteObject(RGN);
  end;
end;

function TCanvas_SetClipRect(_Canvas: TCanvas; _Rect: TRect): Boolean;
var
  RGN: THandle;
  Res: Integer;
begin
{$IFNDEF DELPHIX_TOKYO_UP}
  // Older Delphis think the function result is not assigned if we don't assign it here.
  // Newer Delphis warn that this assignment is not necessary.
  // So we have a bloody IFDEF here to make all compilers shut up.
  Result := False;
{$ENDIF}
  RGN := CreateRectRgn(_Rect.Left, _Rect.Top, _Rect.Right, _Rect.Bottom);
  if RGN = 0 then
    raise Exception.Create(_('CreateRectRgn failed'));

  try
    Res := SelectClipRgn(_Canvas.Handle, RGN);
    if Res = Error then
      raise Exception.Create(_('SelectClipRgn failed'));
    Result := (Res <> NULLREGION);
  finally
    DeleteObject(RGN);
  end;
end;

function TCanvas_DrawText(_Canvas: TCanvas; const _Text: string; var _Rect: TRect; _Flags: TDrawTextFlagSet): Integer;
var
  Flags: LongWord;
begin
  Flags := 0;

  // horizontal alignment
  if dtfLeft in _Flags then
    Flags := Flags or DT_LEFT;
  if dtfRight in _Flags then
    Flags := Flags or DT_RIGHT;
  if dtfCenter in _Flags then
    Flags := Flags or DT_CENTER;

  // vertical alignment (for single lines only)
  if dtfTopSingle in _Flags then
    Flags := Flags or DT_TOP;
  if dtfBottomSingle in _Flags then
    Flags := Flags or DT_BOTTOM;
  if dtfVCenterSingle in _Flags then
    Flags := Flags or DT_VCENTER;

  if dtfWordBreak in _Flags then
    Flags := Flags or DT_WORDBREAK;
  if dtfSingleLine in _Flags then
    Flags := Flags or DT_SINGLELINE;

  // adding ellipsis '...'
  if dtfPathEllipsis in _Flags then
    Flags := Flags or DT_PATH_ELLIPSIS;
  if dtfEndEllipsis in _Flags then
    Flags := Flags or DT_END_ELLIPSIS;
  if dtfWordEllipsis in _Flags then
    Flags := Flags or DT_WORD_ELLIPSIS;

  if dtfNoClip in _Flags then
    Flags := Flags or DT_NOCLIP;

  if dtfCalcRect in _Flags then
    Flags := Flags or DT_CALCRECT;

  Result := Windows.DrawText(_Canvas.Handle, PChar(_Text), -1, _Rect, Flags);
end;

function TCanvas_DrawTextSingleLine(_Canvas: TCanvas; const _Text: string; var _Rect: TRect;
  _HAlign: TDrawTextHorizontalAlignment; _VAlign: TDrawTextVerticalAlignment;
  _Flags: TDrawTextFlagSetNoAlign): Integer;
var
  Flags: TDrawTextFlagSet;
begin
  Flags := _Flags;

  case _HAlign of
    dthaRight: Include(Flags, dtfRight);
    dthaCenter: Include(Flags, dtfCenter);
  else // dthaLeft:
    Include(Flags, dtfLeft);
  end;

  case _VAlign of
    dtvaBottom: Include(Flags, dtfBottomSingle);
    dtvaCenter: Include(Flags, dtfVCenterSingle);
  else // dtvaTop:
    Include(Flags, dtfTopSingle);
  end;

  Include(Flags, dtfSingleLine);

  Result := TCanvas_DrawText(_Canvas, _Text, _Rect, Flags)
end;

procedure TCanvas_DrawVerticalLine(_cnv: TCanvas; _x, _y1, _y2: Integer);
begin
  _cnv.MoveTo(_x, _y1);
  _cnv.LineTo(_x, _y2);
end;

procedure TCanvas_DrawHorizontalLine(_cnv: TCanvas; _x1, _x2, _y: Integer);
begin
  _cnv.MoveTo(_x1, _y);
  _cnv.LineTo(_x2, _y);
end;

procedure TCanvas_DrawLine(_cnv: TCanvas; _x1, _y1, _x2, _y2: Integer);
begin
  _cnv.MoveTo(_x1, _y1);
  _cnv.LineTo(_x2, _y2);
end;

procedure TCanvas_DrawLine(_cnv: TCanvas; _pnt1, _pnt2: TPoint);
begin
  TCanvas_DrawLine(_cnv, _pnt1.x, _pnt1.y, _pnt2.x, _pnt2.y);
end;

type
  TCanvasSaveDC = class(TInterfacedObject)
  private
    FCanvas: TCanvas;
    FSavedDC: Integer;
  public
    constructor Create(_Canvas: TCanvas; _SavedDC: Integer);
    destructor Destroy; override;
  end;

{ TCanvasSaveDC }

constructor TCanvasSaveDC.Create(_Canvas: TCanvas; _SavedDC: Integer);
begin
  inherited Create;
  FCanvas := _Canvas;
  FSavedDC := _SavedDC;
end;

destructor TCanvasSaveDC.Destroy;
begin
  Windows.RestoreDC(FCanvas.Handle, FSavedDC);
  inherited;
end;

function TCanvas_SaveDC(_Canvas: TCanvas): IInterface;
var
  SavedDC: Integer;
begin
  SavedDC := Windows.SaveDC(_Canvas.Handle);
  Result := TCanvasSaveDC.Create(_Canvas, SavedDC);
end;

procedure TCanvas_DrawArrow(_Canvas: TCanvas; _From, _To: TPoint; _ArrowHeadLength: Integer = 15);
// taken from: http://www.efg2.com/Lab/Library/Delphi/Graphics/Arrow.Txt
var
  xbase: Integer;
  xLineDelta: Integer;
  xLineUnitDelta: Double;
  xNormalDelta: Integer;
  xNormalUnitDelta: Double;
  ybase: Integer;
  yLineDelta: Integer;
  yLineUnitDelta: Double;
  yNormalDelta: Integer;
  yNormalUnitDelta: Double;
begin
  _Canvas.MoveTo(_From.x, _From.y);
  _Canvas.LineTo(_To.x, _To.y);

  xLineDelta := _To.x - _From.x;
  yLineDelta := _To.y - _From.y;

  xLineUnitDelta := xLineDelta / Sqrt(Sqr(xLineDelta) + Sqr(yLineDelta));
  yLineUnitDelta := yLineDelta / Sqrt(Sqr(xLineDelta) + Sqr(yLineDelta));

  // (xBase,yBase) is where arrow line is perpendicular to base of triangle.
  xbase := _To.x - Round(_ArrowHeadLength * xLineUnitDelta);
  ybase := _To.y - Round(_ArrowHeadLength * yLineUnitDelta);

  xNormalDelta := yLineDelta;
  yNormalDelta := -xLineDelta;
  xNormalUnitDelta := xNormalDelta / Sqrt(Sqr(xNormalDelta) + Sqr(yNormalDelta));
  yNormalUnitDelta := yNormalDelta / Sqrt(Sqr(xNormalDelta) + Sqr(yNormalDelta));

  // Draw the arrow tip
  _Canvas.Polygon([_To,
      Point(xbase + Round(_ArrowHeadLength * xNormalUnitDelta),
        ybase + Round(_ArrowHeadLength * yNormalUnitDelta)),
      Point(xbase - Round(_ArrowHeadLength * xNormalUnitDelta),
        ybase - Round(_ArrowHeadLength * yNormalUnitDelta))]);
end;

procedure TCanvas_DrawTriangle(_Canvas: TCanvas; _Tip: TPoint; _Height: Integer);
var
  BaselineY: Integer;
  BaselineLeft: Integer;
  BaselineRight: Integer;
begin
  BaselineY := _Tip.y + _Height;
  BaselineLeft := _Tip.x - Abs(_Height);
  BaselineRight := _Tip.x + Abs(_Height);
  _Canvas.Polygon([_Tip, Point(BaselineLeft, BaselineY), Point(BaselineRight, BaselineY)]);
end;

// Inlined method must be implemented before it is called
function GetFastLuminance(const _Red, _Green, _Blue: Byte): Byte;
begin
  Result := Round(0.299 * _Red + 0.587 * _Green + 0.114 * _Blue);
end;

function TdzRgbTriple_GetFastLuminance(const _Triple: TdzRgbTriple): Byte;
begin
  Result := GetFastLuminance(_Triple.Red, _Triple.Green, _Triple.Blue);
end;

procedure TdzRgbTriple_SetColor(var _Triple: TdzRgbTriple; _Color: TColor);
begin
  _Color := ColorToRGB(_Color);
  _Triple.Red := GetRValue(_Color);
  _Triple.Green := GetGValue(_Color);
  _Triple.Blue := GetBValue(_Color);
end;

procedure GetRgbHls(_Red, _Green, _Blue: Byte; out _Hls: THlsRec);
begin
{$IFDEF dzUseGraphics32}
  GR32.RGBtoHSL(GR32.Color32(RGB(_Red, _Green, _Blue)), _Hls.Hue, _Hls.Saturation, _Hls.Luminance);
{$ELSE}
  ColorRGBToHLS(RGB(_Red, _Green, _Blue), _Hls.Hue, _Hls.Luminance, _Hls.Saturation);
{$ENDIF}
end;

function GetRgbLuminance(_Red, _Green, _Blue: Byte): Byte;
var
  Hls: THlsRec;
begin
  GetRgbHls(_Red, _Green, _Blue, Hls);
{$IFDEF dzUseGraphics32}
  Result := Round(Hls.Luminance * HLSMAX);
{$ELSE}
  Result := Hls.Luminance;
{$ENDIF}
end;

function GetRgbBrightness(_Red, _Green, _Blue: Byte; _Channel: TRgbBrightnessChannelEnum): Byte;
begin
  case _Channel of
    rcbAverage: Result := Round((_Red + _Green + _Blue) / 3);
    rcbFastLuminance: Result := GetFastLuminance(_Red, _Green, _Blue);
    rcbRed: Result := _Red;
    rcbGreen: Result := _Green;
    rcbBlue: Result := _Blue;
  else //  rcbLuminance: ;
{$IFDEF dzUseGraphics32}
    Result := Round(GetRgbLuminance(_Red, _Green, _Blue) * HLSMAX);
{$ELSE}
    Result := GetRgbLuminance(_Red, _Green, _Blue);
{$ENDIF}
  end;
end;

function CalcBytesPerPixel(_PixelFormat: TPixelFormat): Integer;
begin
  case _PixelFormat of
    pf8bit: Result := SizeOf(Byte);
    pf24bit: Result := SizeOf(TdzRgbTriple);
    pf32bit: Result := SizeOf(TdzRgbQuad);
  else
    raise EdzPixelFormatNotSupported.Create(_PixelFormat);
  end;
end;

function CalcBytesPerLine(_Width, _BytesPerPixel: Integer): Integer;
begin
  Result := ((_Width * 8 * _BytesPerPixel + 31) and not 31) div 8;
end;

function CalcBytesPerLine(_Width: Integer; _PixelFormat: TPixelFormat): Integer;
begin
  Result := CalcBytesPerLine(_Width, CalcBytesPerPixel(_PixelFormat));
end;

function CalcBytesPerLine(_Width: Integer; _bmp: TBitmap): Integer;
begin
  Result := CalcBytesPerLine(_Width, _bmp.PixelFormat);
end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
{ TdzRgbTriple }

// Inlined method must be iomplemented before it is called
function TdzRgbTriple.GetFastLuminance: Byte;
begin
  Result := Round(0.299 * Red + 0.587 * Green + 0.114 * Blue);
end;

function TdzRgbTriple.GetBrightness(_Channel: TRgbBrightnessChannelEnum): Byte;
begin
  case _Channel of
    rcbAverage: Result := Round((Red + Green + Blue) / 3);
    rcbFastLuminance: Result := GetFastLuminance;
    rcbRed: Result := Red;
    rcbGreen: Result := Green;
    rcbBlue: Result := Blue;
  else //  rcbLuminance: ;
{$IFDEF dzUseGraphics32}
    Result := Round(GetLuminance * HLSMAX);
{$ELSE}
    Result := GetLuminance;
{$ENDIF}
  end;
end;

function TdzRgbTriple.GetColor: TColor;
begin
  Result := RGB(Red, Green, Blue);
end;

procedure TdzRgbTriple.SetRgbColor(_Color: TColor);
begin
  Red := GetRValue(_Color);
  Green := GetGValue(_Color);
  Blue := GetBValue(_Color);
end;

procedure TdzRgbTriple.SetColor(_Color: TColor);
begin
  _Color := ColorToRGB(_Color);
  Red := GetRValue(_Color);
  Green := GetGValue(_Color);
  Blue := GetBValue(_Color);
end;

function TdzRgbTriple.GetValues(_Idx: TValueIdxTriple): Byte;
begin
  Result := TdzRgbTripleValues(Self)[_Idx];
end;

procedure TdzRgbTriple.SetValues(_Idx: TValueIdxTriple; _Value: Byte);
begin
  TdzRgbTripleValues(Self)[_Idx] := _Value;
end;

procedure TdzRgbTriple.SetValues(_Red, _Green, _Blue: Byte);
begin
  Red := _Red;
  Green := _Green;
  Blue := _Blue;
end;

procedure TdzRgbTriple.SetBrightness(_Value: Byte);
begin
  Red := _Value;
  Green := _Value;
  Blue := _Value;
end;

class function TdzRgbTriple.GetFastLuminance(_Red, _Green, _Blue: Byte): Byte;
begin
  Result := Round(0.299 * _Red + 0.587 * _Green + 0.114 * _Blue);
end;

{$IFDEF dzUseGraphics32}

procedure TdzRgbTriple.GetHls(out _Hls: THlsRec);
begin
  GR32.RGBtoHSL(GR32.Color32(GetColor), _Hls.Hue, _Hls.Saturation, _Hls.Luminance);
end;

procedure TdzRgbTriple.SetHls(const _Hls: THlsRec);
begin
  SetColor(GR32.WinColor(GR32.HSLtoRGB(_Hls.Hue, _Hls.Saturation, _Hls.Luminance)));
end;

{$ELSE}

procedure TdzRgbTriple.GetHls(out _Hls: THlsRec);
begin
  ColorRGBToHLS(GetColor, _Hls.Hue, _Hls.Luminance, _Hls.Saturation);
end;

procedure TdzRgbTriple.SetHls(const _Hls: THlsRec);
begin
  SetColor(ColorHLSToRGB(_Hls.Hue, _Hls.Luminance, _Hls.Saturation));
end;

{$ENDIF}

// untested from http://www.swissdelphicenter.ch/en/showcode.php?id=2349
//function RGB2HSV (R,G,B : Byte) : THSV;
//var
//  Min_, Max_, Delta : Double;
//  H , S , V : Double ;
//begin
//  H := 0.0 ;
//  Min_ := Min (Min( R,G ), B);
//  Max_ := Max (Max( R,G ), B);
//  Delta := ( Max_ - Min_ );
//  V := Max_ ;
//  If ( Max_ <> 0.0 ) then
//    S := 255.0 * Delta / Max_
//  else
//    S := 0.0 ;
//  If (S <> 0.0) then
//    begin
//      If R = Max_ then
//        H := (G - B) / Delta
//      else
//        If G = Max_ then
//          H := 2.0 + (B - R) / Delta
//        else
//          If B = Max_ then
//            H := 4.0 + (R - G) / Delta
//    End
//  else
//    H := -1.0 ;
//  H := H * 60 ;
//  If H < 0.0 then H := H + 360.0;
//  with Result Do
//    begin
//      Hue := H ;             // Hue -> 0..360
//      Sat := S * 100 / 255; // Saturation -> 0..100 %
//      Val := V * 100 / 255; // Value - > 0..100 %
//    end;
//end;

//procedure Swap(var _a, _b: Byte);
//var
//  t: Byte;
//begin
//  t := _a;
//  _a := _b;
//  _b := t;
//end;

// untested: Delphi implmementations of
// http://lolengine.net/blog/2013/01/13/fast-rgb-to-hsv
//procedure RGB2HSV(_r, _g, _b: Byte; out _h, _s, _v: Single);
//var
//  k: Single;
//  chroma: Single;
//begin
//  k := 0;
//
//  if _g < _b then begin
//    Swap(_g, _b);
//    k := -1;
//  end;
//
//  if _r < _g then begin
//    Swap(_r, _g);
//    k := -2 / 6 - k;
//  end;
//
//  chroma := _r - min(_g, _b);
//  _h := Abs(k + (_g - _b) / (6 * chroma + 1e-20));
//  _s := chroma / (_r + 1e-20);
//  _v := _r;
//end;

//procedure RGBtoHSL(_r, _g, _b: Byte; out _h, _s, _l: Single);
//var
//  k: Single;
//  lightness: Integer;
//  chroma: Integer;
//begin
//  k := 0.0;
//  if (_g < _b) then begin
//    Swap(_g, _b);
//    k := 6.0;
//  end;
//  if (_r < _g) then begin
//
//    Swap(_r, _g);
//    k := 2.0 - k;
//  end;
//  lightness := _r + min(_g, _b);
//  chroma := _r - min(_g, _b);
//  if (chroma <> 0) then begin
//    _h := Abs((_g - _b) / chroma - k) * 1.0 / 6.0;
//    _s := chroma / (255 - Abs(lightness - 255));
//  end else begin
//    _h := 0.0;
//    _s := 0.0;
//  end;
//  _l = lightness * 1.0 / 510.0;
//end;

function TdzRgbTriple.GetLuminance: Byte;
var
  Hls: THlsRec;
begin
  GetHls(Hls);
{$IFDEF dzUseGraphics32}
  Result := Round(Hls.Luminance * HLSMAX);
{$ELSE}
  Result := Hls.Luminance;
{$ENDIF}
end;

procedure TdzRgbTriple.SetGray(_Value: Byte);
begin
  Red := _Value;
  Green := _Value;
  Blue := _Value;
end;

{ tdzRgbQuad }

function TdzRgbQuad.GetBrightness(_Channel: TRgbBrightnessChannelEnum): Word;
begin
  case _Channel of
    rcbAverage: Result := Round((Red + Green + Blue) / 3);
    rcbFastLuminance: Result := GetFastLuminance;
    rcbRed: Result := Red;
    rcbGreen: Result := Green;
    rcbBlue: Result := Blue;
  else //  rcbLuminance: ;
    Result := GetLuminance;
  end;
end;

function TdzRgbQuad.GetColor: TColor;
begin
  Result := RGB(Red, Green, Blue);
end;

function TdzRgbQuad.GetFastLuminance: Word;
begin
  Result := Round(0.299 * Red + 0.587 * Green + 0.114 * Blue);
end;

procedure TdzRgbQuad.GetHls(out _Hue, _Luminance, _Saturation: Word);
begin
  ColorRGBToHLS(GetColor, _Hue, _Luminance, _Saturation)
end;

function TdzRgbQuad.GetLuminance: Word;
var
  Hue: Word;
  Saturation: Word;
begin
  Result := 0;

  GetHls(Hue, Result, Saturation);
end;

procedure TdzRgbQuad.SetBrightness(_Value: Byte);
begin
  Red := _Value;
  Green := _Value;
  Blue := _Value;
end;

procedure TdzRgbQuad.SetColor(_Color: TColor);
begin
  _Color := ColorToRGB(_Color);
  Red := GetRValue(_Color);
  Green := GetGValue(_Color);
  Blue := GetBValue(_Color);
end;

procedure TdzRgbQuad.SetGray(_Value: Byte);
begin
  Red := _Value;
  Green := _Value;
  Blue := _Value;
  // According to the Win32 API documenation this member is reserved and must be zero,
  // but apparently it can be non-zero and contain an alpha value.
  Reserved := 0;
end;

procedure TdzRgbQuad.SetHls(_Hue, _Luminance, _Saturation: Word);
begin
  SetColor(ColorHLSToRGB(_Hue, _Luminance, _Saturation));
end;
{$ENDIF}

procedure TBitmap_SetSize(_bmp: TBitmap; _Width, _Height: Integer);
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
{$IFDEF SUPPPORTS_BITMAP_SETSIZE}
  _bmp.SetSize(_Width, _Height);
{$ELSE}
  _bmp.Width := _Width;
  _bmp.Height := _Height;
{$ENDIF}
end;

{$IF Declared(TBitmap32)}

procedure TBitmap_LoadJpg(_bmp: TBitmap32; const _JpgFn: string);
var
  jpg: TJPEGImage;
begin
  jpg := TJPEGImage.Create;
  try
    jpg.LoadFromFile(_JpgFn);
    _bmp.Assign(jpg);
  finally
    FreeAndNil(jpg);
  end;
end;
{$IFEND}

procedure TBitmap_LoadJpg(_bmp: TBitmap; const _JpgFn: string);
var
  jpg: TJPEGImage;
begin
  jpg := TJPEGImage.Create;
  try
    jpg.LoadFromFile(_JpgFn);
    _bmp.Assign(jpg);
  finally
    FreeAndNil(jpg);
  end;
end;

procedure TBitmap_SaveJpg(_bmp: TBitmap; const _JpgFn: string);
var
  jpg: TJPEGImage;
begin
  jpg := TJPEGImage.Create;
  try
    jpg.Assign(_bmp);
    jpg.SaveToFile(_JpgFn);
  finally
    FreeAndNil(jpg);
  end;
end;

var
  // global structure for the most common case of 256 colors, initialized on demand
  GreyPalette256: TMaxLogPalette = (
    palVersion: 0; // 0 => has not been initialized
    );

procedure InternalMakeGrayPalette(_NumColors: TNumColors; out lp: TMaxLogPalette);
var
  i: Integer;
  Grey: Byte;
  MaxValue: Integer;
begin
  lp.palNumEntries := _NumColors;
  MaxValue := _NumColors - 1;
  for i := 0 to _NumColors - 1 do begin
    Grey := i * 255 div MaxValue;
    lp.palPalEntry[i].peRed := Grey;
    lp.palPalEntry[i].peGreen := Grey;
    lp.palPalEntry[i].peBlue := Grey;
    lp.palPalEntry[i].peFlags := PC_RESERVED;
  end;
  // set it last, so we don't create a race condition with multithreaded access
  lp.palVersion := $300;
end;

function MakeGrayPalette(_NumColors: TNumColors): HPALETTE;
var
  lp: TMaxLogPalette;
  plp: PMaxLogPalette;
begin
  if _NumColors = 256 then begin
    if GreyPalette256.palVersion = 0 then
      InternalMakeGrayPalette(_NumColors, GreyPalette256);
    plp := @GreyPalette256;
  end else begin
    InternalMakeGrayPalette(_NumColors, lp);
    plp := @lp;
  end;
  Result := CreatePalette(pLogPalette(plp)^);
end;

///<sumamry>
/// Calls to this function are meant to be enclosed into Assert() so the compiler creates code for
/// it only if assertions are enabled </summary>
function AssertPixelFormat(_bmp: TBitmap; _Expected: TPixelFormat): Boolean; overload;
var
  ActualName: string;
  ExpectedName: string;
begin
  Assert(Assigned(_bmp), 'bitmap is not assigned');
  Result := (_bmp.PixelFormat = _Expected);
  if not Result then begin
    ActualName := GetEnumName(TypeInfo(TPixelFormat), Ord(_bmp.PixelFormat));
    ExpectedName := GetEnumName(TypeInfo(TPixelFormat), Ord(_Expected));
    Assert(False, 'unexpected PixelFormat ' + ActualName + ' (expected ' + ExpectedName + ')');
  end;
end;

type
  TPixelFormatSet = set of TPixelFormat;

///<sumamry>
/// Calls to this function are meant to be enclosed into Assert() so the compiler creates code for
/// it only if assertions are enabled </summary>
function AssertPixelFormat(_bmp: TBitmap; _Expected: TPixelFormatSet): Boolean; overload;
var
  ActualName: string;
  ExpectedNames: string;
  pf: TPixelFormat;
begin
  Assert(Assigned(_bmp), 'bitmap is not assigned');
  Result := (_bmp.PixelFormat in _Expected);
  if not Result then begin
    ActualName := GetEnumName(TypeInfo(TPixelFormat), Ord(_bmp.PixelFormat));
    ExpectedNames := '';
    for pf := Low(TPixelFormat) to High(TPixelFormat) do begin
      ExpectedNames := ExpectedNames + ',' + GetEnumName(TypeInfo(TPixelFormat), Ord(pf));
    end;
    ExpectedNames := Copy(ExpectedNames, 2, 255);
    Assert(False, 'unexpected PixelFormat ' + ActualName + ' (expected one of' + ExpectedNames + ')');
  end;
end;

procedure TBitmap_AssignBgr8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);
const
  BytesPerPixel = 3;
var
  y: Integer;
  ScanLine: PdzRgbTripleArray;
  BufferBytesPerLine: Integer;
  BitmapBytesPerLine: Integer;
  h: Integer;
  w: Integer;
//  ms: TMemoryStream;
//  bfh: TBitmapFileHeader;
//  bih: TBitmapInfoHeader;
begin
  Assert(AssertPixelFormat(_bmp, pf24bit));

  h := _bmp.Height;
  w := _bmp.Width;

  BufferBytesPerLine := BytesPerPixel * w;

  BitmapBytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BitmapBytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

//  bfh.bfType := $4D42; // 'BM'
//  bfh.bfSize := BytesPerLine * _Bmp.Height;
//  bfh.bfReserved1 := 0;
//  bfh.bfReserved2 := 0;
//  bfh.bfOffBits := SizeOf(bfh);

//  bih.biSize := SizeOf(bih);
//  bih.biWidth := _Bmp.Width;
//  bih.biHeight := -_Bmp.Height; // origin is upper left corner -> negative
//  bih.biPlanes := 1;
//  bih.biBitCount := 24;
//  bih.biCompression := BI_RGB;
//  bih.biSizeImage := 0; // The size, in bytes, of the image. This may be set to zero for BI_RGB bitmaps.
//  bih.biXPelsPerMeter := 1000;
//  bih.biYPelsPerMeter := 1000;
//  bih.biClrUsed := 0; // The number of color indexes in the color table that are actually used by the bitmap. If this value is zero, the bitmap uses the maximum number of colors corresponding to the value of the biBitCount member for the compression mode specified by biCompression.
//  bih.biClrImportant := 0;

//  ms := TMemoryStream.Create;
//  ms.WriteBuffer(bfh, SizeOf(bfh));
//  ms.WriteBuffer(_Buffer^, bfh.bfSize);

  // Unfortunately the y coordinates of TBitmap are reversed (the picture is upside down).
  // So we can only copy the whole picture in one go, if the buffer is also upside down
  // (many cameras have this feature). If not, we have to copy it one line at a time.
  if _YIsReversed then begin
    ScanLine := _bmp.ScanLine[h - 1];
    Move(_Buffer^, ScanLine^, h * BufferBytesPerLine);
  end else begin
    // At least with GBR8 the bytes have the right order so we can copy the whole line in one go
    ScanLine := _bmp.ScanLine[0];
    for y := 0 to h - 1 do begin
      Move(_Buffer^, ScanLine^, BufferBytesPerLine);
      Inc(_Buffer, BufferBytesPerLine);
      Dec(PByte(ScanLine), BitmapBytesPerLine);
    end;
  end;
end;

procedure TBitmap_AssignRgb8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);
var
  y: Integer;
  x: Integer;
  ScanLine: PdzRgbTripleArray;
  h: Integer;
begin
  Assert(AssertPixelFormat(_bmp, pf24bit));

  h := _bmp.Height;
  for y := 0 to h - 1 do begin
    if _YIsReversed then begin
      ScanLine := _bmp.ScanLine[h - y - 1];
    end else begin
      ScanLine := _bmp.ScanLine[y];
    end;
    for x := 0 to _bmp.Width - 1 do begin
      // unfortunately the bytes in the buffer have a different order (RGB) than in the
      // Bitmap (BGR) so we must copy each byte separately
      ScanLine[x].Red := _Buffer^;
      Inc(_Buffer);
      ScanLine[x].Green := _Buffer^;
      Inc(_Buffer);
      ScanLine[x].Blue := _Buffer^;
      Inc(_Buffer);
    end;
  end;
end;

procedure TBitmap_AssignMono824(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);
var
  y: Integer;
  x: Integer;
  ScanLine: PdzRgbTripleArray;
  h: Integer;
  Value: Byte;
begin
  Assert(AssertPixelFormat(_bmp, pf24bit));
  h := _bmp.Height;
  for y := 0 to h - 1 do begin
    if _YIsReversed then begin
      ScanLine := _bmp.ScanLine[h - y - 1];
    end else begin
      ScanLine := _bmp.ScanLine[y];
    end;
    for x := 0 to _bmp.Width - 1 do begin
      // gray scale: Set all colours to the same value
      Value := _Buffer^;
      ScanLine[x].Red := Value;
      ScanLine[x].Green := Value;
      ScanLine[x].Blue := Value;
      Inc(_Buffer);
    end;
  end;
end;

procedure TBitmap_AssignMono8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);
var
  w: Integer;
begin
  Assert(AssertPixelFormat(_bmp, pf8bit));
  w := _bmp.Width;
  TBitmap_AssignMono8(_Buffer, _bmp, _YIsReversed, w);
end;

procedure TBitmap_AssignMono8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean; _RowStride: Int64);
var
  w: Integer;
  h: Integer;
  y: Integer;
  ScanLine: PByte;
begin
  Assert(AssertPixelFormat(_bmp, pf8bit));
  w := _bmp.Width;
  h := _bmp.Height;

  Assert(_RowStride >= w);

  // Unfortunately the y coordinates of TBitmap are reversed (the picture is upside down).
  // So we can only copy the whole picture in one go, if the buffer is also upside down
  // (many cameras have this feature). If not, we have to copy it one line at a time.
  if _YIsReversed and (_RowStride = w) then begin
    ScanLine := _bmp.ScanLine[h - 1];
    Move(_Buffer^, ScanLine^, h * w);
  end else begin
    for y := 0 to h - 1 do begin
      ScanLine := _bmp.ScanLine[y];
      Move(_Buffer^, ScanLine^, w);
      Inc(_Buffer, _RowStride);
    end;
  end;
end;

function BufferBits12ToMono8(var _BufPtr: Pointer): Byte;
begin
  Result := MulDiv(PUInt16(_BufPtr)^, 255, 1 shl 12 - 1);
  IncPtr(_BufPtr, 2);
end;

procedure TBitmap_AssignToMono8(_BufferBitsToMono8Func: TBufferBitsToMono8Func;
  _Buffer: Pointer; _bmp: TBitmap; _YIsReversed: Boolean; _RowStride: Int64); overload;
var
  y: Integer;
  x: Integer;
  w: Integer;
  h: Integer;
  ScanLine: PByte;
  Buf: Pointer;
begin
  Assert(AssertPixelFormat(_bmp, pf8bit));

  w := _bmp.Width;
  h := _bmp.Height;

  Assert((_RowStride = 0) or (_RowStride >= w));

  for y := 0 to _bmp.Height - 1 do begin
    if _YIsReversed then begin
      ScanLine := _bmp.ScanLine[h - 1];
    end else begin
      ScanLine := _bmp.ScanLine[y];
    end;
    Buf := _Buffer;
    for x := 0 to w - 1 do begin
      ScanLine^ := _BufferBitsToMono8Func(Buf);
      Inc(ScanLine);
    end;
    if _RowStride > 0 then begin
      IncPtr(_Buffer, _RowStride);
    end else begin
      // we assume that BufferBitsToMono8Func inrements the buffer correctly
      _Buffer := Buf;
    end;
  end;
end;

procedure TBitmap_AssignToMono8(_BufferBitsToMono8Meth: TBufferBitsToMono8Meth;
  _Buffer: Pointer; _bmp: TBitmap; _YIsReversed: Boolean; _RowStride: Int64 = 0); overload;
var
  y: Integer;
  x: Integer;
  w: Integer;
  h: Integer;
  ScanLine: PByte;
  Buf: Pointer;
begin
  Assert(AssertPixelFormat(_bmp, pf8bit));

  w := _bmp.Width;
  h := _bmp.Height;

  Assert((_RowStride = 0) or (_RowStride >= w));

  for y := 0 to _bmp.Height - 1 do begin
    if _YIsReversed then begin
      ScanLine := _bmp.ScanLine[h - 1];
    end else begin
      ScanLine := _bmp.ScanLine[y];
    end;
    Buf := _Buffer;
    for x := 0 to w - 1 do begin
      ScanLine^ := _BufferBitsToMono8Meth(Buf);
      Inc(ScanLine);
    end;
    if _RowStride > 0 then begin
      IncPtr(_Buffer, _RowStride);
    end else begin
      // we assume that _BufferBitsToMono8Meth inrements the buffer correctly
      _Buffer := Buf;
    end;
  end;
end;

type
  PByteArray = SysUtils.PByteArray;
  TCopyScanline = procedure(_Width: Integer; _SrcLine: Pointer; _DestLine: Pointer);

procedure Copy8Bit(_Width: Integer; _SrcLine: Pointer; _DestLine: Pointer);
begin
  Move(PByte(_SrcLine)^, PByte(_DestLine)^, _Width);
end;

procedure Copy24Bit(_Width: Integer; _SrcLine: Pointer; _DestLine: Pointer);
var
  x: Integer;
  SrcPixel: PdzRgbTriple;
  DstPixel: PByte;
begin
  SrcPixel := _SrcLine;
  DstPixel := _DestLine;
  for x := 0 to _Width - 1 do begin
    DstPixel^ := SrcPixel.Blue;
    Inc(SrcPixel);
    Inc(DstPixel);
  end;
end;

procedure Copy32Bit(_Width: Integer; _SrcLine: Pointer; _DestLine: Pointer);
var
  x: Integer;
  SrcPixel: PdzRgbQuad;
  DstPixel: PByte;
begin
  SrcPixel := _SrcLine;
  DstPixel := _DestLine;
  for x := 0 to _Width - 1 do begin
    DstPixel^ := SrcPixel.Blue;
    Inc(SrcPixel);
    Inc(DstPixel);
  end;
end;

procedure TBitmap_MonoToMono8(_InBmp, _OutBmp: TBitmap);
const
  DstBytesPerPixel = 1;
var
  CopyScanLine: TCopyScanline;
  SrcBytesPerPixel: Integer;
  SrcBytesPerLine: Integer;
  DstBytesPerLine: Integer;
  w: Integer;
  h: Integer;
  y: Integer;
  SrcLine: PByte;
  DstLine: PByte;
begin
  Assert(AssertPixelFormat(_InBmp, [pf8bit, pf24bit, pf32bit]));

  case _InBmp.PixelFormat of
    pf8bit: begin
        CopyScanLine := Copy8Bit;
        SrcBytesPerPixel := 1;
      end;
    pf24bit: begin
        CopyScanLine := Copy24Bit;
        SrcBytesPerPixel := 3;
      end;
    pf32bit: begin
        CopyScanLine := Copy32Bit;
        SrcBytesPerPixel := 4;
      end;
  else
    raise EdzPixelFormatNotSupported.Create(_InBmp.PixelFormat)
  end;

  w := _InBmp.Width;
  h := _InBmp.Height;

  _OutBmp.PixelFormat := pf8bit;
  _OutBmp.Palette := MakeGrayPalette();

  _OutBmp.Width := w;
  _OutBmp.Height := h;

  SrcBytesPerLine := ((w * 8 * SrcBytesPerPixel + 31) and not 31) div 8;
  Assert(SrcBytesPerLine = Graphics.BytesPerScanline(w, SrcBytesPerPixel * 8, 32));
  DstBytesPerLine := ((w * 8 * DstBytesPerPixel + 31) and not 31) div 8;
  Assert(DstBytesPerLine = Graphics.BytesPerScanline(w, DstBytesPerPixel * 8, 32));

  SrcLine := _InBmp.ScanLine[0];
  DstLine := _OutBmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    CopyScanLine(w, SrcLine, DstLine);
    Dec(SrcLine, SrcBytesPerLine);
    Dec(DstLine, DstBytesPerLine);
  end;
end;

function TBitmap_MonoToMono8(_bmp: TBitmap): TBitmap;
begin
  Result := TBitmap.Create;
  TBitmap_MonoToMono8(_bmp, Result);
end;

procedure TBitmap_MakeMono8(_bmp: TBitmap);
begin
  _bmp.PixelFormat := pf8bit;
  _bmp.Palette := MakeGrayPalette();
end;

function TBitmap_CreateMono8: TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf8bit;
  Result.Palette := MakeGrayPalette();
end;

// original source: http://www.delphigeist.com/2009/09/blur-bitmap-algorithm.html
// but heavily modified

procedure BlurBuffer(const _In: TByteMatrix; out _out: TByteMatrix);
var
  w: Integer;
  h: Integer;
  x: Integer;
  y: Integer;
begin
  h := Length(_In);
  w := Length(_In[0]);

  SetLength(_out, h);
  // Yes, this could be moved inside the loop for setting the first and last line.
  // but it would bomb out there when w = 0. So we either have to check for w=0 all the time
  // or we make it a separate loop.
  for y := 0 to h - 1 do begin
    SetLength(_out[y], w);
  end;

  if (w = 0) or (h = 0) then begin
    asm nop end;
    Exit;
  end;

  // copy first and last line without changes
  for y := 0 to h - 1 do begin
    _out[y][0] := _In[y][0];
    _out[y][w - 1] := _In[y][w - 1];
  end;

  // copy first and last column without changes
  for x := 0 to w - 1 do begin
    _out[0][x] := _In[0][x];
    _out[h - 1][x] := _In[h - 1][x];
  end;

  // blur everything else
  for y := 1 to h - 2 do begin
    for x := 1 to w - 2 do begin
      _out[y][x] := (
        _In[y - 1][x - 1]
        + _In[y - 1][x]
        + _In[y - 1][x + 1]
        + _In[y][x - 1]
        // todo: Check whether the point itself should actually be used, maybe
        //       the blurring effect is better without (and if you remove it,
        //       remember change the divisor to 8)
        + _In[y][x]
        + _In[y][x + 1]
        + _In[y + 1][x - 1]
        + _In[y + 1][x]
        + _In[y + 1][x + 1]) div 9;
    end;
  end;
end;

procedure BlurBufferMask(const _In: TByteMatrix; const _Mask: TBitMatrix; out _out: TByteMatrix);
var
  w: Integer;
  h: Integer;
  x: Integer;
  y: Integer;
begin
  h := Length(_In);
  w := Length(_In[0]);

  Assert(Length(_Mask) = h);
  Assert(Length(_Mask[0]) = w);

  SetLength(_out, h);
  // copy first and last line without changes
  for y := 0 to h - 1 do begin
    SetLength(_out[y], w);
    _out[y][0] := _In[y][0];
    _out[y][w - 1] := _In[y][w - 1];
  end;

  // copy first and last column without changes
  for x := 0 to w - 1 do begin
    _out[0][x] := _In[0][x];
    _out[h - 1][x] := _In[h - 1][x];
  end;

  // blur everything else
  for y := 1 to h - 2 do begin
    for x := 1 to w - 2 do begin
      if _Mask[y][x] then begin
        // if the mask is true for this bit -> blur the point
        _out[y][x] := (
          _In[y - 1][x - 1]
          + _In[y - 1][x]
          + _In[y - 1][x + 1]
          + _In[y][x - 1]
          // todo: Check whether the point itself should actually be used, maybe
          //       the blurring effect is better without (and if you remove it,
          //       remember change the divisor to 8)
          + _In[y][x]
          + _In[y][x + 1]
          + _In[y + 1][x - 1]
          + _In[y + 1][x]
          + _In[y + 1][x + 1]) div 9;
      end else begin
        // if not, copy the point unchanged
        _out[y][x] := _In[y][x];
      end;
    end;
  end;
end;

procedure TBitmap_BlurRect(_bmp: TBitmap; _Left, _Top, _Right, _Bottom: Integer; _Passes: Integer);
var
  x, y: Integer;
  Line: PdzRgbTripleArray;
  i: Integer;
  BufW: Integer;
  BufH: Integer;
  Buffer1: TByteMatrix;
  Buffer2: TByteMatrix;
begin
  Assert(Assigned(_bmp));

  if _Left < 1 then
    _Left := 1;
  if _Top < 1 then
    _Top := 1;
  if _Right > _bmp.Width - 2 then
    _Right := _bmp.Width - 2;
  if _Bottom > _bmp.Height - 2 then
    _Bottom := _bmp.Height - 2;

  _bmp.PixelFormat := pf24bit;

  // prepare the working buffer for blurring
  BufH := _Bottom - _Top + 1;
  BufW := _Right - _Left + 1;
  SetLength(Buffer1, BufH);
  for i := 0 to BufH - 1 do
    SetLength(Buffer1[i], BufW);

  // blur blue
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Buffer1[y - _Top][x - _Left] := Line^[x].Blue;
  end;
  for i := 1 to Ceil(_Passes / 2) do begin
    BlurBuffer(Buffer1, Buffer2);
    BlurBuffer(Buffer2, Buffer1);
  end;
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Line^[x].Blue := Buffer1[y - _Top][x - _Left];
  end;

  // blur red
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Buffer1[y - _Top][x - _Left] := Line^[x].Red;
  end;
  for i := 1 to Ceil(_Passes / 2) do begin
    BlurBuffer(Buffer1, Buffer2);
    BlurBuffer(Buffer2, Buffer1);
  end;
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Line^[x].Red := Buffer1[y - _Top][x - _Left];
  end;

  // blur green
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Buffer1[y - _Top][x - _Left] := Line^[x].Green;
  end;
  for i := 1 to Ceil(_Passes / 2) do begin
    BlurBuffer(Buffer1, Buffer2);
    BlurBuffer(Buffer2, Buffer1);
  end;
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Line^[x].Green := Buffer1[y - _Top][x - _Left];
  end;
end;

// Inlined method must be implemented before it is used
function TryCalcEllipsePoint(_a, _b, _x: Extended; out _y: Extended): Boolean;
var
  sq: Extended;
begin
  sq := 1 - Sqr(_x / _a);
  Result := (CompareValue(sq, 0) = GreaterThanValue);
  if Result then
    _y := _b * Sqrt(sq);
end;

// Inlined method must be implemented before it is used
function TryCalcEllipsePoints(_x0, _y0, _a, _b, _x: Extended; out _y1, _y2: Extended): Boolean;
var
  y: Extended;
begin
  Result := TryCalcEllipsePoint(_a, _b, _x - _x0, y);
  if Result then begin
    _y1 := -y + _y0;
    _y2 := y + _y0;
  end;
end;

procedure TBitmap_BlurEllipse(_bmp: TBitmap; _Left, _Top, _Right, _Bottom: Integer; _Passes: Integer);
var
  x, y: Integer;
  Line: PdzRgbTripleArray;
  i: Integer;
  BufW: Integer;
  BufH: Integer;
  Buffer1: TByteMatrix;
  Buffer2: TByteMatrix;
  Mask: TBitMatrix;
  x0: Extended;
  y0: Extended;
  a: Extended;
  b: Extended;
  Y1, Y2: Extended;
begin
  Assert(Assigned(_bmp));

  if _Left < 1 then
    _Left := 1;
  if _Top < 1 then
    _Top := 1;
  if _Right > _bmp.Width - 2 then
    _Right := _bmp.Width - 2;
  if _Bottom > _bmp.Height - 2 then
    _Bottom := _bmp.Height - 2;

  // prepare the working buffer for blurring and an empty mask
  BufH := _Bottom - _Top + 1;
  BufW := _Right - _Left + 1;

  if (BufH <= 1) or (BufW <= 1) then
    Exit; //==>

  _bmp.PixelFormat := pf24bit;

  SetLength(Buffer1, BufH);
  SetLength(Mask, BufH);
  for y := 0 to BufH - 1 do begin
    SetLength(Buffer1[y], BufW);
    SetLength(Mask[y], BufW);
    for x := 0 to BufW - 1 do begin
      Mask[y][x] := False;
    end;
  end;

  // create an eliptic mask
  x0 := (_Left + _Right) / 2;
  y0 := (_Top + _Bottom) / 2;
  a := (_Right - _Left) / 2;
  b := (_Bottom - _Top) / 2;
  for x := _Left to _Right do begin
    if TryCalcEllipsePoints(x0, y0, a, b, x, Y1, Y2) then begin
      for y := Round(Y1) to Round(Y2) do begin
        Mask[y - _Top][x - _Left] := True;
      end;
    end;
  end;

  // blur blue
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Buffer1[y - _Top][x - _Left] := Line^[x].Blue;
  end;
  for i := 1 to Ceil(_Passes / 2) do begin
    BlurBufferMask(Buffer1, Mask, Buffer2);
    BlurBufferMask(Buffer2, Mask, Buffer1);
  end;
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Line^[x].Blue := Buffer1[y - _Top][x - _Left];
  end;

  // blur red
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Buffer1[y - _Top][x - _Left] := Line^[x].Red;
  end;
  for i := 1 to Ceil(_Passes / 2) do begin
    BlurBufferMask(Buffer1, Mask, Buffer2);
    BlurBufferMask(Buffer2, Mask, Buffer1);
  end;
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Line^[x].Red := Buffer1[y - _Top][x - _Left];
  end;

  // blur green
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Buffer1[y - _Top][x - _Left] := Line^[x].Green;
  end;
  for i := 1 to Ceil(_Passes / 2) do begin
    BlurBufferMask(Buffer1, Mask, Buffer2);
    BlurBufferMask(Buffer2, Mask, Buffer1);
  end;
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Line^[x].Green := Buffer1[y - _Top][x - _Left];
  end;
end;

procedure TBitmap24_FilterPixels(_SrcBmp, _DstBmp: TBitmap; _Callback: TPixel24FilterCallback);
const
  BytesPerPixel = 3;
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
  SrcLine: PByte;
  DstLine: PByte;
  SrcPixel: PByte;
  DstPixel: PByte;
  BytesPerLine: Integer;
begin
  Assert(Assigned(_SrcBmp));

  _SrcBmp.PixelFormat := pf24bit;
  _DstBmp.PixelFormat := pf24bit;
  w := _SrcBmp.Width;
  h := _SrcBmp.Height;
  TBitmap_SetSize(_DstBmp, w, h);

  if h = 0 then
    Exit; //==>

  BytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

  SrcLine := _SrcBmp.ScanLine[0];
  DstLine := _DstBmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(SrcLine = _SrcBmp.ScanLine[y]);
    Assert(DstLine = _DstBmp.ScanLine[y]);
    SrcPixel := SrcLine;
    DstPixel := DstLine;
    for x := 0 to w - 1 do begin
      PdzRgbTriple(DstPixel)^ := PdzRgbTriple(SrcPixel)^;
      _Callback(x, y, PdzRgbTriple(DstPixel)^);
      Inc(SrcPixel, BytesPerPixel);
      Inc(DstPixel, BytesPerPixel);
    end;
    Dec(SrcLine, BytesPerLine);
    Dec(DstLine, BytesPerLine);
  end;
end;

procedure TBitmap8_FilterPixels(_SrcBmp, _DstBmp: TBitmap; _Callback: TPixel8FilterCallback);
const
  BytesPerPixel = 1;
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
  SrcLine: PByte;
  DstLine: PByte;
  SrcPixel: PByte;
  DstPixel: PByte;
  BytesPerLine: Integer;
begin
  Assert(Assigned(_SrcBmp));

  _SrcBmp.PixelFormat := pf8bit;
  _DstBmp.Assign(nil);
  _DstBmp.PixelFormat := pf8bit;
  w := _SrcBmp.Width;
  h := _SrcBmp.Height;
  _DstBmp.Palette := MakeGrayPalette;
  TBitmap_SetSize(_DstBmp, w, h);

  if h = 0 then
    Exit; //==>

  BytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

  SrcLine := _SrcBmp.ScanLine[0];
  DstLine := _DstBmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(SrcLine = _SrcBmp.ScanLine[y]);
    Assert(DstLine = _DstBmp.ScanLine[y]);
    SrcPixel := SrcLine;
    DstPixel := DstLine;
    for x := 0 to w - 1 do begin
      DstPixel^ := SrcPixel^;
      _Callback(x, y, DstPixel^);
      Inc(SrcPixel, BytesPerPixel);
      Inc(DstPixel, BytesPerPixel);
    end;
    Dec(SrcLine, BytesPerLine);
    Dec(DstLine, BytesPerLine);
  end;
end;

procedure TBitmap24_ApplyGamma(_SrcBmp, _DstBmp: TBitmap; const _GammaRed, _GammaGreen, _GammaBlue: TGammaCurve);
const
  BytesPerPixel = 3;
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
  SrcLine: PByte;
  DstLine: PByte;
  SrcPixel: PByte;
  DstPixel: PByte;
  DstTriple: PdzRgbTriple;
  SrcTriple: PdzRgbTriple;
  BytesPerLine: Integer;
begin
  Assert(AssertPixelFormat(_SrcBmp, pf24bit));

  _DstBmp.PixelFormat := pf24bit;
  w := _SrcBmp.Width;
  h := _SrcBmp.Height;
  TBitmap_SetSize(_DstBmp, w, h);

  if h = 0 then
    Exit; //==>

  BytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

  SrcLine := _SrcBmp.ScanLine[0];
  DstLine := _DstBmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(SrcLine = _SrcBmp.ScanLine[y]);
    Assert(DstLine = _DstBmp.ScanLine[y]);
    SrcPixel := SrcLine;
    DstPixel := DstLine;
    for x := 0 to w - 1 do begin
      SrcTriple := PdzRgbTriple(SrcPixel);
      DstTriple := PdzRgbTriple(DstPixel);
      DstTriple.Red := _GammaRed[SrcTriple.Red];
      DstTriple.Green := _GammaGreen[SrcTriple.Green];
      DstTriple.Blue := _GammaBlue[SrcTriple.Blue];
      Inc(SrcPixel, BytesPerPixel);
      Inc(DstPixel, BytesPerPixel);
    end;
    Dec(SrcLine, BytesPerLine);
    Dec(DstLine, BytesPerLine);
  end;
end;

procedure TBitmap24_ApplyGamma(_SrcBmp, _DstBmp: TBitmap; const _Gamma: TGammaCurve);
const
  BytesPerPixel = 3;
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
  SrcLine: PByte;
  DstLine: PByte;
  SrcPixel: PByte;
  DstPixel: PByte;
  DstTriple: PdzRgbTriple;
  SrcTriple: PdzRgbTriple;
  BytesPerLine: Integer;
begin
  Assert(AssertPixelFormat(_SrcBmp, pf24bit));

  _DstBmp.PixelFormat := pf24bit;
  w := _SrcBmp.Width;
  h := _SrcBmp.Height;
  TBitmap_SetSize(_DstBmp, w, h);

  if h = 0 then
    Exit; //==>

  BytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

  SrcLine := _SrcBmp.ScanLine[0];
  DstLine := _DstBmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(SrcLine = _SrcBmp.ScanLine[y]);
    Assert(DstLine = _DstBmp.ScanLine[y]);
    SrcPixel := SrcLine;
    DstPixel := DstLine;
    for x := 0 to w - 1 do begin
      SrcTriple := PdzRgbTriple(SrcPixel);
      DstTriple := PdzRgbTriple(DstPixel);
      DstTriple.Red := _Gamma[SrcTriple.Red];
      DstTriple.Green := _Gamma[SrcTriple.Green];
      DstTriple.Blue := _Gamma[SrcTriple.Blue];
      Inc(SrcPixel, BytesPerPixel);
      Inc(DstPixel, BytesPerPixel);
    end;
    Dec(SrcLine, BytesPerLine);
    Dec(DstLine, BytesPerLine);
  end;
end;

procedure TBitmap24_ApplyGammaTo8(_SrcBmp, _DstBmp: TBitmap; const _Gamma: TGammaCurve); overload;
const
  BytesPerPixelInput = 3;
  BytesPerPixelOutput = 1;
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
  SrcLine: PByte;
  DstLine: PByte;
  SrcPixel: PByte;
  DstPixel: PByte;
  BytesPerLineInput: Integer;
  BytesPerLineOutput: Integer;
begin
  Assert(AssertPixelFormat(_SrcBmp, pf24bit));

  w := _SrcBmp.Width;
  h := _SrcBmp.Height;
  _DstBmp.Assign(nil);
  TBitmap_MakeMono8(_DstBmp);
  TBitmap_SetSize(_DstBmp, w, h);

  if h = 0 then
    Exit; //==>

  BytesPerLineInput := ((w * 8 * BytesPerPixelInput + 31) and not 31) div 8;
  Assert(BytesPerLineInput = Graphics.BytesPerScanline(w, BytesPerPixelInput * 8, 32));

  BytesPerLineOutput := ((w * 8 * BytesPerPixelOutput + 31) and not 31) div 8;
  Assert(BytesPerLineOutput = Graphics.BytesPerScanline(w, BytesPerPixelOutput * 8, 32));

  SrcLine := _SrcBmp.ScanLine[0];
  DstLine := _DstBmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(SrcLine = _SrcBmp.ScanLine[y]);
    Assert(DstLine = _DstBmp.ScanLine[y]);
    SrcPixel := SrcLine;
    DstPixel := DstLine;
    for x := 0 to w - 1 do begin
      DstPixel^ := _Gamma[SrcPixel^];
      Inc(SrcPixel, BytesPerPixelInput);
      Inc(DstPixel, BytesPerPixelOutput);
    end;
    Dec(SrcLine, BytesPerLineInput);
    Dec(DstLine, BytesPerLineOutput);
  end;
end;

procedure TBitmap8_ApplyGamma(_SrcBmp, _DstBmp: TBitmap; const _Gamma: TGammaCurve);
const
  BytesPerPixel = 1;
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
  SrcLine: PByte;
  DstLine: PByte;
  SrcPixel: PByte;
  DstPixel: PByte;
  BytesPerLine: Integer;
begin
  Assert(AssertPixelFormat(_SrcBmp, pf8bit));

  _DstBmp.Assign(nil);
  _DstBmp.PixelFormat := pf8bit;
  w := _SrcBmp.Width;
  h := _SrcBmp.Height;
  _DstBmp.Palette := MakeGrayPalette;
  TBitmap_SetSize(_DstBmp, w, h);

  if h = 0 then
    Exit; //==>

  BytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

  SrcLine := _SrcBmp.ScanLine[0];
  DstLine := _DstBmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(SrcLine = _SrcBmp.ScanLine[y]);
    Assert(DstLine = _DstBmp.ScanLine[y]);
    SrcPixel := SrcLine;
    DstPixel := DstLine;
    for x := 0 to w - 1 do begin
      DstPixel^ := _Gamma[SrcPixel^];
      Inc(SrcPixel, BytesPerPixel);
      Inc(DstPixel, BytesPerPixel);
    end;
    Dec(SrcLine, BytesPerLine);
    Dec(DstLine, BytesPerLine);
  end;
end;

procedure TBitmap8_ApplyMultiGamma(_SrcBmp, _DstBmp: TBitmap; const _GammaArr: array of TGammaCurve);
const
  BytesPerPixel = 1;
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
  SrcLine: PByte;
  DstLine: PByte;
  SrcPixel: PByte;
  DstPixel: PByte;
  BytesPerLine: Integer;
  i: Integer;
  Value: Byte;
begin
  Assert(AssertPixelFormat(_SrcBmp, pf8bit));

  _DstBmp.Assign(nil);
  _DstBmp.PixelFormat := pf8bit;
  w := _SrcBmp.Width;
  h := _SrcBmp.Height;
  _DstBmp.Palette := MakeGrayPalette;
  TBitmap_SetSize(_DstBmp, w, h);

  if h = 0 then
    Exit; //==>

  BytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

  SrcLine := _SrcBmp.ScanLine[0];
  DstLine := _DstBmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(SrcLine = _SrcBmp.ScanLine[y]);
    Assert(DstLine = _DstBmp.ScanLine[y]);
    SrcPixel := SrcLine;
    DstPixel := DstLine;
    for x := 0 to w - 1 do begin
      Value := SrcPixel^;
      for i := Low(_GammaArr) to High(_GammaArr) do
        Value := _GammaArr[i][Value];
      DstPixel^ := Value;
      Inc(SrcPixel, BytesPerPixel);
      Inc(DstPixel, BytesPerPixel);
    end;
    Dec(SrcLine, BytesPerLine);
    Dec(DstLine, BytesPerLine);
  end;
end;

// source: https://www.swissdelphicenter.ch/en/showcode.php?id=1948

procedure TBitmap_Sharpen(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single);
begin
  case _SrcBmp.PixelFormat of
    pf8bit: TBitmap8_Sharpen(_SrcBmp, _DstBmp, _Alpha);
  else
    TBitmap24_Sharpen(_SrcBmp, _DstBmp, _Alpha);
  end;
end;

procedure TBitmap8_Sharpen(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single);
type
  PPixel = PByte;
const
  BytesPerPixel = 1;
  ForcedPixelFormat = pf8bit;
var
  Row, Column: Integer;
  SrcPixelTop: PPixel;
  SrcPixelLeft: PPixel;
  SrcPixelCenter: PPixel;
  SrcPixelRight: PPixel;
  SrcPixelBottom: PPixel;
  SrcRow: PByte;
  DstRow: PByte;
  DstPixel: PPixel;
  Beta: Single;
  IntAlpha, IntBeta: Integer;
  WorkAreaHeight, WorkAreaWidth: Integer;
  CenterBrightness: Integer;
  AvgBrightness: Integer;
  BytesPerLine: Integer;
begin
  // sharpening is blending of the current pixel
  // with the average of the surrounding ones,
  // but with a negative weight for the average
  Assert((_SrcBmp.Width > 2) and (_SrcBmp.Height > 2), 'Bitmap must be at least 3x3');
  Assert((_Alpha >= 0) and (_Alpha <= 5), 'Alpha must be >=0 and <=5');

  // since 0 <= Alpha <= 5 we can be sure that 0 <= Beta <= 1
  Beta := _Alpha / 5;
  _Alpha := _Alpha + 1;

  // integer scaled alpha and beta calculated only once
  IntBeta := Round(Beta * $10000);
  IntAlpha := Round(_Alpha * $10000);

  _SrcBmp.PixelFormat := pf8bit;
  _DstBmp.PixelFormat := pf8bit;
  _DstBmp.Palette := MakeGrayPalette;
  TBitmap_SetSize(_DstBmp, _SrcBmp.Width, _SrcBmp.Height);

  WorkAreaWidth := _SrcBmp.Width - 2;
  WorkAreaHeight := _SrcBmp.Height - 2;
  BytesPerLine := (((WorkAreaWidth + 2) * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(WorkAreaWidth + 2, BytesPerPixel * 8, 32));

  // Copy first row unchanged
  SrcRow := _SrcBmp.ScanLine[0];
  DstRow := _DstBmp.ScanLine[0];
  SrcPixelCenter := PPixel(SrcRow);
  DstPixel := PPixel(DstRow);
  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixelCenter^;
    Inc(DstPixel);
    Inc(SrcPixelCenter);
  end;

  for Row := 1 to WorkAreaHeight do begin
    // ScanLine[0] is the line with the highest memory address, so we decrement it by
    // BytesPerLine to get the next line (which would be ScanLine[1])
    // We could of course call _SrcBmp.Scanline[], but that would affect the efficiency
    // of the code due the the function call and some overhead in that code.
    Dec(SrcRow, BytesPerLine);
    Dec(DstRow, BytesPerLine);

    SrcPixelCenter := PPixel(SrcRow);
    DstPixel := PPixel(DstRow);

    // 1st column unchanged
    DstPixel^ := SrcPixelCenter^;

    Inc(DstPixel);

    // remember: Bitmaps are stored upside down, so for the previous line, we must add BytesPerLine
    //           and for the next line we must subtract BytesPerLine
    SrcPixelLeft := SrcPixelCenter;
    Dec(SrcPixelLeft);
    SrcPixelRight := SrcPixelCenter;
    Inc(SrcPixelRight);
    SrcPixelTop := AddToPtr(SrcPixelCenter, +BytesPerLine);
    SrcPixelBottom := AddToPtr(SrcPixelCenter, -BytesPerLine);
    for Column := 1 to WorkAreaWidth do begin
      CenterBrightness := SrcPixelCenter^;

      // calculate the average brightness weighted by -beta
      AvgBrightness := SrcPixelTop^;
      AvgBrightness := AvgBrightness + SrcPixelLeft^;
      AvgBrightness := AvgBrightness + CenterBrightness;
      AvgBrightness := AvgBrightness + SrcPixelRight^;
      AvgBrightness := AvgBrightness + SrcPixelBottom^;
      AvgBrightness := (IntBeta * AvgBrightness + $7FFF) shr 16;

      // add center pixel weighted by alpha
      AvgBrightness := (IntAlpha * CenterBrightness + $7FFF) shr 16 - AvgBrightness;

      DstPixel^ := ReduceToByte(AvgBrightness);

      Inc(DstPixel);

      Inc(SrcPixelTop);
      Inc(SrcPixelLeft);
      Inc(SrcPixelCenter);
      Inc(SrcPixelRight);
      Inc(SrcPixelBottom);
    end;

    // copy Last column unchanged
    DstPixel^ := SrcPixelCenter^;
  end;
  Dec(SrcRow, BytesPerLine);

  Dec(DstRow, BytesPerLine);
  DstPixel := PPixel(DstRow);

  // copy last row unchanged
  SrcPixelCenter := PPixel(SrcRow);

  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixelCenter^;
    Inc(DstPixel);
    Inc(SrcPixelCenter);
  end;
end;

procedure TBitmap24_Sharpen(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single);
type
  PPixel = PRGBTriple;
const
  BytesPerPixel = 3;
var
  Row, Column, k: Integer;
  SrcRows: array[0..2] of PByte;
  SrcPixels: array[0..4] of PPixel;
  FirstSrcRow: PByte;
  SrcPixel: PPixel;
  DstRow: PByte;
  DstPixel: PPixel;
  p: PPixel;
  Beta: Single;
  IntAlpha, IntBeta: Integer;
  WorkAreaHeight, WorkAreaWidth: Integer;
  AvgRed, AvgGreen, AvgBlue: Integer;
  BytesPerLine: Integer;
begin
  // sharpening is blending of the current pixel
  // with the average of the surrounding ones,
  // but with a negative weight for the average
  Assert((_SrcBmp.Width > 2) and (_SrcBmp.Height > 2), 'Bitmap must be at least 3x3');
  Assert((_Alpha >= 0) and (_Alpha <= 5), 'Alpha must be >=0 and <=5');

  // since 0 <= Alpha <= 5 we can be sure that 0 <= Beta <= 1
  Beta := _Alpha / 5;
  _Alpha := _Alpha + 1;

  // integer scaled alpha and beta calculated only once
  IntBeta := Round(Beta * $10000);
  IntAlpha := Round(_Alpha * $10000);

  _SrcBmp.PixelFormat := pf24bit;
  _DstBmp.PixelFormat := pf24bit;
  TBitmap_SetSize(_DstBmp, _SrcBmp.Width, _SrcBmp.Height);

  WorkAreaWidth := _SrcBmp.Width - 2;
  WorkAreaHeight := _SrcBmp.Height - 2;
  // There is Graphics.BytesPerScanline() which we could call instead of doing the
  // calculation here
  BytesPerLine := (((WorkAreaWidth + 2) * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(WorkAreaWidth + 2, BytesPerPixel * 8, 32));

  DstRow := _DstBmp.ScanLine[0];
  DstPixel := PPixel(DstRow);

  // Copy first row unchanged
  FirstSrcRow := _SrcBmp.ScanLine[0];
  SrcPixel := PPixel(FirstSrcRow);
  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixel^;
    Inc(DstPixel);
    Inc(SrcPixel);
  end;

  // ScanLine[0] is the line with the highest memory address, so we decrement it by
  // BytesPerLine to get the next line (which would be ScanLine[1])
  // We could of course call _SrcBmp.Scanline[], but that would affect the efficiency
  // of the code due the the function call and some overhead in that code.
  SrcRows[0] := FirstSrcRow;
  SrcRows[1] := AddToPtr(FirstSrcRow, -BytesPerLine);
  SrcRows[2] := AddToPtr(SrcRows[1], -BytesPerLine);
  for Row := 1 to WorkAreaHeight do begin
    Dec(DstRow, BytesPerLine);
    DstPixel := PPixel(DstRow);
    SrcPixels[0] := AddToPtr(SrcRows[0], +1 * BytesPerPixel); //top
    SrcPixels[1] := PPixel(SrcRows[1]); //left
    SrcPixels[2] := AddToPtr(SrcRows[1], +1 * BytesPerPixel); //center
    SrcPixels[3] := AddToPtr(SrcRows[1], +2 * BytesPerPixel); //right
    SrcPixels[4] := AddToPtr(SrcRows[2], +1 * BytesPerPixel); //bottom
    DstPixel^ := SrcPixels[1]^; //1st col unchanged
    for Column := 1 to WorkAreaWidth do begin
      // calculate average weighted by -beta for each color
      AvgRed := 0;
      AvgGreen := 0;
      AvgBlue := 0;
      for k := 0 to 4 do begin
        AvgRed := AvgRed + SrcPixels[k]^.rgbtRed;
        AvgGreen := AvgGreen + SrcPixels[k]^.rgbtGreen;
        AvgBlue := AvgBlue + SrcPixels[k]^.rgbtBlue;
        Inc(SrcPixels[k]);
      end;
      AvgRed := (IntBeta * AvgRed + $7FFF) shr 16;
      AvgGreen := (IntBeta * AvgGreen + $7FFF) shr 16;
      AvgBlue := (IntBeta * AvgBlue + $7FFF) shr 16;

      // add center pixel weighted by alpha
      p := PPixel(SrcPixels[1]); // after inc, st[1] is at center
      AvgRed := (IntAlpha * p^.rgbtRed + $7FFF) shr 16 - AvgRed;
      AvgGreen := (IntAlpha * p^.rgbtGreen + $7FFF) shr 16 - AvgGreen;
      AvgBlue := (IntAlpha * p^.rgbtBlue + $7FFF) shr 16 - AvgBlue;

      Inc(DstPixel);
      DstPixel^.rgbtRed := ReduceToByte(AvgRed);
      DstPixel^.rgbtGreen := ReduceToByte(AvgGreen);
      DstPixel^.rgbtBlue := ReduceToByte(AvgBlue);
    end;
    Inc(DstPixel);
    Inc(SrcPixels[1]);

    // copy Last column unchanged
    DstPixel^ := SrcPixels[1]^;

    // prepare for next loop
    SrcRows[0] := SrcRows[1];
    SrcRows[1] := SrcRows[2];
    Dec(SrcRows[2], BytesPerLine);
  end;
  Dec(DstRow, BytesPerLine);
  DstPixel := PPixel(DstRow);

  // copy last row unchanged
  SrcPixel := PPixel(SrcRows[1]);

  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixel^;
    Inc(DstPixel);
    Inc(SrcPixel);
  end;
end;

procedure TBitmap_Sharpen(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix);
begin
  case _SrcBmp.PixelFormat of
    pf8bit: TBitmap8_Sharpen(_SrcBmp, _DstBmp, _AlphaMap);
  else
    TBitmap24_Sharpen(_SrcBmp, _DstBmp, _AlphaMap);
  end;
end;

procedure IncPtr(var _Ptr: Pointer; _Offset: IntPtr);
begin
  _Ptr := Pointer(IntPtr(_Ptr) + _Offset);
end;

// Inlined method must be implemented before it is called
function AddToPtr(const _Ptr: Pointer; _Offset: IntPtr): Pointer;
begin
  Result := Pointer(IntPtr(_Ptr) + _Offset);
end;

// Inlined method must be implemented before it is called
function PtrDiff(const _Ptr1, _Ptr2: Pointer): IntPtr;
begin
  Result := IntPtr(_Ptr1) - IntPtr(_Ptr2);
end;

procedure TBitmap8_Sharpen(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix);
type
  PPixel = PByte;
const
  BytesPerPixel = 1;
  ForcedPixelFormat = pf8bit;
var
  Row, Column: Integer;
  SrcPixelTop: PPixel;
  SrcPixelLeft: PPixel;
  SrcPixelCenter: PPixel;
  SrcPixelRight: PPixel;
  SrcPixelBottom: PPixel;
  SrcRow: PByte;
  DstRow: PByte;
  DstPixel: PPixel;
  Alpha: Single;
  Beta: Single;
  IntAlpha, IntBeta: Integer;
  WorkAreaHeight, WorkAreaWidth: Integer;
  CenterBrightness: Integer;
  AvgBrightness: Integer;
  BytesPerLine: Integer;
  AlphaEntrySize: IntPtr;
  AlphaPtr: PSingle;
begin
  // sharpening is blending of the current pixel
  // with the average of the surrounding ones,
  // but with a negative weight for the average
  Assert((_SrcBmp.Width > 2) and (_SrcBmp.Height > 2), 'Bitmap must be at least 3x3');
  Assert((Length(_AlphaMap) = _SrcBmp.Height),
    Format('Number of lines in AlphaMap (%d) must match bitmap height (%d)', [Length(_AlphaMap), _SrcBmp.Height]));

  _SrcBmp.PixelFormat := pf8bit;
  _DstBmp.PixelFormat := pf8bit;
  _DstBmp.Palette := MakeGrayPalette;
  TBitmap_SetSize(_DstBmp, _SrcBmp.Width, _SrcBmp.Height);

  WorkAreaWidth := _SrcBmp.Width - 2;
  WorkAreaHeight := _SrcBmp.Height - 2;
  BytesPerLine := (((WorkAreaWidth + 2) * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(WorkAreaWidth + 2, BytesPerPixel * 8, 32));

  AlphaEntrySize := PtrDiff(@(_AlphaMap[0][1]), @(_AlphaMap[0][0]));

  SrcRow := _SrcBmp.ScanLine[0];
  DstRow := _DstBmp.ScanLine[0];
  SrcPixelCenter := PPixel(SrcRow);
  DstPixel := PPixel(DstRow);

  // Copy first row unchanged
  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixelCenter^;
    Inc(DstPixel);
    Inc(SrcPixelCenter);
  end;

  for Row := 1 to WorkAreaHeight do begin
    Assert(Length(_AlphaMap[Row]) = _SrcBmp.Width,
      Format('Number of values in AlphaMap[%d] (%d) must match bitmap width (%d)',
      [Row, Length(_AlphaMap[Row]), _SrcBmp.Width]));

    // ScanLine[0] is the line with the highest memory address, so we decrement it by
    // BytesPerLine to get the next line (which would be ScanLine[1])
    // We could of course call _SrcBmp.Scanline[], but that would affect the efficiency
    // of the code due the the function call and some overhead in that code.
    Dec(SrcRow, BytesPerLine);
    Dec(DstRow, BytesPerLine);
    SrcPixelCenter := PPixel(SrcRow);
    DstPixel := PPixel(DstRow);

    // 1st column unchanged
    DstPixel^ := SrcPixelCenter^;
    Inc(DstPixel);

    // remember: Bitmaps are stored upside down, so for the previous line, we must add BytesPerLine
    //           and for the next line we must subtract BytesPerLine
    SrcPixelLeft := SrcPixelCenter;
    Dec(SrcPixelLeft);
    SrcPixelRight := SrcPixelCenter;
    Inc(SrcPixelRight);
    SrcPixelTop := AddToPtr(SrcPixelCenter, +BytesPerLine);
    SrcPixelBottom := AddToPtr(SrcPixelCenter, -BytesPerLine);

    AlphaPtr := @(_AlphaMap[Row][1]);
    for Column := 1 to WorkAreaWidth do begin
      CenterBrightness := SrcPixelCenter^;

      Assert(AlphaPtr = @(_AlphaMap[Row][Column]));
      Alpha := AlphaPtr^;
      Assert((Alpha >= 0) and (Alpha <= 5), Format('Alpha[%d][%d] must be >=1 and <=5', [Row, Column]));
      // since 0 <= Alpha <= 5 we can be sure that 0 <= Beta <= 1
      Beta := Alpha / 5;
      // the original algorithm used 1 < Alpha < 6 so we need to add 1 here
      Alpha := Alpha + 1;
      // integer scaled alpha and beta calculated only once
      IntBeta := Round(Beta * $10000);
      IntAlpha := Round(Alpha * $10000);

      // calculate the average brightness weighted by -beta
      AvgBrightness := SrcPixelTop^;
      AvgBrightness := AvgBrightness + SrcPixelLeft^;
      AvgBrightness := AvgBrightness + CenterBrightness;
      AvgBrightness := AvgBrightness + SrcPixelRight^;
      AvgBrightness := AvgBrightness + SrcPixelBottom^;
      AvgBrightness := (IntBeta * AvgBrightness + $7FFF) shr 16;

      // add center pixel weighted by alpha
      AvgBrightness := (IntAlpha * CenterBrightness + $7FFF) shr 16 - AvgBrightness;

      // write into the target pixel
      DstPixel^ := ReduceToByte(AvgBrightness);

      Inc(DstPixel);

      Inc(SrcPixelTop);
      Inc(SrcPixelLeft);
      Inc(SrcPixelCenter);
      Inc(SrcPixelRight);
      Inc(SrcPixelBottom);

      IncPtr(Pointer(AlphaPtr), AlphaEntrySize);
    end;

    // copy Last column unchanged
    DstPixel^ := SrcPixelCenter^;
  end;
  Dec(SrcRow, BytesPerLine);

  Dec(DstRow, BytesPerLine);
  DstPixel := PPixel(DstRow);

  // copy last row unchanged
  SrcPixelCenter := PPixel(SrcRow);

  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixelCenter^;
    Inc(DstPixel);
    Inc(SrcPixelCenter);
  end;
end;

procedure TBitmap24_Sharpen(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix);
type
  PPixel = PRGBTriple;
const
  BytesPerPixel = 3;
var
  Row, Column, k: Integer;
  SrcRows: array[0..2] of PByte;
  SrcPixels: array[0..4] of PPixel;
  FirstSrcRow: PByte;
  SrcPixel: PPixel;
  DstRow: PByte;
  DstPixel: PPixel;
  p: PPixel;
  Alpha: Single;
  Beta: Single;
  IntAlpha, IntBeta: Integer;
  WorkAreaHeight, WorkAreaWidth: Integer;
  AvgRed, AvgGreen, AvgBlue: Integer;
  BytesPerLine: Integer;
begin
  // sharpening is blending of the current pixel
  // with the average of the surrounding ones,
  // but with a negative weight for the average
  Assert((_SrcBmp.Width > 2) and (_SrcBmp.Height > 2), 'Bitmap must be at least 3x3');
  Assert((Length(_AlphaMap) = _SrcBmp.Height),
    Format('Number of lines in AlphaMap (%d) must match bitmap height (%d)', [Length(_AlphaMap), _SrcBmp.Height]));

  _SrcBmp.PixelFormat := pf24bit;
  _DstBmp.PixelFormat := pf24bit;
  TBitmap_SetSize(_DstBmp, _SrcBmp.Width, _SrcBmp.Height);

  WorkAreaWidth := _SrcBmp.Width - 2;
  WorkAreaHeight := _SrcBmp.Height - 2;
  // There is Graphics.BytesPerScanline() which we could call instead of doing the
  // calculation here
  BytesPerLine := (((WorkAreaWidth + 2) * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(WorkAreaWidth + 2, BytesPerPixel * 8, 32));

  DstRow := _DstBmp.ScanLine[0];
  DstPixel := PPixel(DstRow);

  // Copy first row unchanged
  FirstSrcRow := _SrcBmp.ScanLine[0];
  SrcPixel := PPixel(FirstSrcRow);
  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixel^;
    Inc(DstPixel);
    Inc(SrcPixel);
  end;

  // ScanLine[0] is the line with the highest memory address, so we decrement it by
  // BytesPerLine to get the next line (which would be ScanLine[1])
  // We could of course call _SrcBmp.Scanline[], but that would affect the efficiency
  // of the code due the the function call and some overhead in that code.
  SrcRows[0] := FirstSrcRow;
  SrcRows[1] := AddToPtr(FirstSrcRow, -BytesPerLine);
  SrcRows[2] := AddToPtr(SrcRows[1], -BytesPerLine);
  for Row := 1 to WorkAreaHeight do begin
    Assert(Length(_AlphaMap[Row]) = _SrcBmp.Width,
      Format('Number of values in AlphaMap[%d] (%d) must match bitmap width (%d)',
      [Row, Length(_AlphaMap[Row]), _SrcBmp.Width]));

    Dec(DstRow, BytesPerLine);
    DstPixel := PPixel(DstRow);
    SrcPixels[0] := AddToPtr(SrcRows[0], +1 * BytesPerPixel); //top
    SrcPixels[1] := PPixel(SrcRows[1]); //left
    SrcPixels[2] := AddToPtr(SrcRows[1], +1 * BytesPerPixel); //center
    SrcPixels[3] := AddToPtr(SrcRows[1], +2 * BytesPerPixel); //right
    SrcPixels[4] := AddToPtr(SrcRows[2], +1 * BytesPerPixel); //bottom
    // copy 1st col unchanged ("[1]" is not the pixel index!)
    DstPixel^ := SrcPixels[1]^;
    for Column := 1 to WorkAreaWidth do begin
      Alpha := _AlphaMap[Row][Column];
      Assert((Alpha >= 0) and (Alpha <= 5), Format('Alpha[%d][%d] must be >=1 and <=5', [Row, Column]));
      // since 0 <= Alpha <= 5 we can be sure that 0 <= Beta <= 1
      Beta := Alpha / 5;
      // the original algorithm used 1 < Alpha < 6 so we need to add 1 here
      Alpha := Alpha + 1;
      // integer scaled alpha and beta calculated only once
      IntBeta := Round(Beta * $10000);
      IntAlpha := Round(Alpha * $10000);

      // calculate average weighted by -beta for each color
      AvgRed := 0;
      AvgGreen := 0;
      AvgBlue := 0;
      for k := 0 to 4 do begin
        AvgRed := AvgRed + SrcPixels[k]^.rgbtRed;
        AvgGreen := AvgGreen + SrcPixels[k]^.rgbtGreen;
        AvgBlue := AvgBlue + SrcPixels[k]^.rgbtBlue;
        Inc(SrcPixels[k]);
      end;

      AvgRed := (IntBeta * AvgRed + $7FFF) shr 16;
      AvgGreen := (IntBeta * AvgGreen + $7FFF) shr 16;
      AvgBlue := (IntBeta * AvgBlue + $7FFF) shr 16;

      // add center pixel weighted by alpha
      p := PPixel(SrcPixels[1]); // after inc, SrcPixels[1] is at center
      AvgRed := (IntAlpha * p^.rgbtRed + $7FFF) shr 16 - AvgRed;
      AvgGreen := (IntAlpha * p^.rgbtGreen + $7FFF) shr 16 - AvgGreen;
      AvgBlue := (IntAlpha * p^.rgbtBlue + $7FFF) shr 16 - AvgBlue;

      // write into the target pixel
      Inc(DstPixel);
      DstPixel^.rgbtRed := ReduceToByte(AvgRed);
      DstPixel^.rgbtGreen := ReduceToByte(AvgGreen);
      DstPixel^.rgbtBlue := ReduceToByte(AvgBlue);
    end;
    Inc(DstPixel);
    Inc(SrcPixels[1]);

    // copy Last column unchanged
    DstPixel^ := SrcPixels[1]^;

    // prepare for next loop
    SrcRows[0] := SrcRows[1];
    SrcRows[1] := SrcRows[2];
    Dec(SrcRows[2], BytesPerLine);
  end;

  // copy last row unchanged
  Dec(DstRow, BytesPerLine);
  DstPixel := PPixel(DstRow);
  SrcPixel := PPixel(SrcRows[1]);
  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixel^;
    Inc(DstPixel);
    Inc(SrcPixel);
  end;
end;

// this calculates the average of the given value array using the median
function CalcAverageBrightness(_Arr: array of Integer): Integer;
const
  Left = 0;
var
  Len: Integer;
  Right: Integer;
  i: Integer;
  j: Integer;
  tmp: Integer;
  Mid: Integer;
begin
  Len := Length(_Arr);
  Assert(Len > 0);
  Right := Len - 1;
  i := Left + 1;
  while i <= Right do begin
    j := i;
    while (j > Left) and (_Arr[j - 1] > _Arr[j]) do begin
      tmp := _Arr[j];
      _Arr[j] := _Arr[j - 1];
      _Arr[j - 1] := tmp;
      Dec(j);
    end;
    Inc(i);
  end;
  Mid := Left + Len div 2;
  if Odd(Len) then
    Result := _Arr[Mid]
  else
    Result := (_Arr[Mid] + _Arr[Mid - 1]) div 2;
end;

procedure TBitmap8_BalanceBrightness(_SrcBmp, _DstBmp: TBitmap; _Offset: Word);
type
  PPixel = PByte;
const
  BytesPerPixel = 1;
  ForcedPixelFormat = pf8bit;
var
  Offset: Integer;
  Row: Integer;
  Column: Integer;
  SrcPixel: PPixel;
  SrcPixel0: PPixel;
  SrcPixel1: PPixel;
  SrcPixel2: PPixel;
  SrcPixel3: PPixel;
  SrcPixel4: PPixel;
  SrcPixel5: PPixel;
  SrcPixel6: PPixel;
  SrcPixel7: PPixel;
  SrcPixel8: PPixel;
  SrcPixel9: PPixel;
  SrcRow: PByte;
  DstRow: PByte;
  DstPixel: PPixel;
  BmpWidth: Integer;
  BmpHeight: Integer;
  WorkAreaHeight: Integer;
  WorkAreaWidth: Integer;
  WorkBuffer: array of array of Integer;
  CenterBrightness: Integer;
  AvgBrightness: Integer;
  MinBrightness: Integer;
  MaxBrightness: Integer;
  BytesPerLine: Integer;
  SrcBottomRow: PByte;
  DstBottomRow: PByte;
  SrcOffset0: Integer;
  SrcOffset1: Integer;
  SrcOffset2: Integer;
  SrcOffset3: Integer;
  SrcOffset4: Integer;
  SrcOffset5: Integer;
  SrcOffset6: Integer;
  SrcOffset7: Integer;
  SrcOffset8: Integer;
  SrcOffset9: Integer;
//  TotalBrightness: Int64;
//  TotalAvgBrightness: Integer;
begin
  Assert((_SrcBmp.Width > 2 * _Offset) and (_SrcBmp.Height > 2 * _Offset), Format('Bitmap must be at least %dx%d', [2 * _Offset, 2 * _Offset]));
  Offset := _Offset;

  _SrcBmp.PixelFormat := ForcedPixelFormat;
  _DstBmp.PixelFormat := ForcedPixelFormat;
  _DstBmp.Palette := MakeGrayPalette;
  BmpWidth := _SrcBmp.Width;
  BmpHeight := _SrcBmp.Height;
  TBitmap_SetSize(_DstBmp, BmpWidth, BmpHeight);

  SetLength(WorkBuffer, BmpHeight);
  for Row := 0 to BmpHeight - 1 do
    SetLength(WorkBuffer[Row], BmpWidth);

  BytesPerLine := (((BmpWidth) * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(BmpWidth, BytesPerPixel * 8, 32));

  WorkAreaWidth := BmpWidth - 2 * Offset;
  WorkAreaHeight := BmpHeight - 2 * Offset;

  MinBrightness := 255;
  MaxBrightness := 0;

  // ScanLine[0] is the line with the highest memory address, so we decrement it by
  // BytesPerLine to get the next line (which would be ScanLine[1])
  // We could of course call _SrcBmp.Scanline[], but that would affect the efficiency
  // of the code due the the function call and some overhead in that code.

  SrcBottomRow := _SrcBmp.ScanLine[0];
  DstBottomRow := _DstBmp.ScanLine[0];

  // Calculate offsets for pixels around the SrcPixel in these positions:
  //
  //          0
  //        -   1
  //      9       -
  //    -           2
  //  8       *       3
  //    7           -
  //      -       4
  //        6   -
  //          5
  //
  // The distance between SrcPixel and the pixels 0, 4 5 and 8 is Offset.
  // This is designed not to be too symmetric.
  // I tried using only 0, 3, 5 and 8 but it generated artifacts.
  SrcOffset0 := +BytesPerLine * Offset;
  SrcOffset1 := +BytesPerLine * Offset * 2 div 3 + Offset * 1 div 3;
  SrcOffset2 := +BytesPerLine * Offset * 1 div 3 + Offset * 2 div 3;
  SrcOffset3 := +Offset;
  SrcOffset4 := -BytesPerLine * Offset * 1 div 2 + Offset * 1 div 2;
  SrcOffset5 := -BytesPerLine * Offset;
  SrcOffset6 := -BytesPerLine * Offset * 2 div 3 - Offset * 1 div 3;
  SrcOffset7 := -BytesPerLine * Offset * 1 div 3 - Offset * 2 div 3;
  SrcOffset8 := -Offset;
  SrcOffset9 := +BytesPerLine * Offset * 1 div 2 - Offset * 1 div 2;

  // Experimental code: Calculate the total average brightness.
  // This was originally meant to adjust the total brightness of the picture back to the
  // original average. This is no longer done since we use the minimum and maximum values to
  // stretch the histogram at the end to get the best possible contrast.
//  TotalBrightness := 0;
//  SrcRow := SrcBottomRow;
//  for Row := 0 to BmpHeight - 1 do begin
//    SrcPixel := PPixel(SrcRow);
//    for Column := 0 to BmpWidth - 1 do begin
//      CenterBrightness := SrcPixel^;
//      Inc(TotalBrightness, CenterBrightness);
//      Inc(SrcPixel);
//    end;
//    Dec(SrcRow, BytesPerLine);
//  end;
//  TotalAvgBrightness := TotalBrightness div BmpWidth div BmpHeight;

  // Copy bottom row(s) unchanged
  SrcRow := SrcBottomRow;
  DstRow := DstBottomRow;
  for Row := 0 to Offset - 1 do begin
    SrcPixel := PPixel(SrcRow);
    DstPixel := PPixel(DstRow);
    for Column := 0 to BmpWidth - 1 do begin
      CenterBrightness := SrcPixel^;
      WorkBuffer[Row, Column] := CenterBrightness;
      DstPixel^ := CenterBrightness;
      Inc(SrcPixel);
      Inc(DstPixel);
    end;
    Dec(SrcRow, BytesPerLine);
    Dec(DstRow, BytesPerLine);
  end;

  for Row := Offset to Offset + WorkAreaHeight - 1 do begin
    SrcPixel := PPixel(SrcRow);
    DstPixel := PPixel(DstRow);

    // copy the leftmost column(s) unchanged
    for Column := 0 to Offset - 1 do begin
      CenterBrightness := SrcPixel^;
      WorkBuffer[Row, Column] := CenterBrightness;
      DstPixel^ := CenterBrightness;
      Inc(SrcPixel);
      Inc(DstPixel);
    end;

    // remember: Bitmaps are stored upside down, so for the previous line, we must add BytesPerLine
    //           and for the next line we must subtract BytesPerLine

    SrcPixel0 := AddToPtr(SrcPixel, SrcOffset0);
    SrcPixel1 := AddToPtr(SrcPixel, SrcOffset1);
    SrcPixel2 := AddToPtr(SrcPixel, SrcOffset2);
    SrcPixel3 := AddToPtr(SrcPixel, SrcOffset3);
    SrcPixel4 := AddToPtr(SrcPixel, SrcOffset4);
    SrcPixel5 := AddToPtr(SrcPixel, SrcOffset5);
    SrcPixel6 := AddToPtr(SrcPixel, SrcOffset6);
    SrcPixel7 := AddToPtr(SrcPixel, SrcOffset7);
    SrcPixel8 := AddToPtr(SrcPixel, SrcOffset8);
    SrcPixel9 := AddToPtr(SrcPixel, SrcOffset9);

    for Column := Offset to Offset + WorkAreaWidth - 1 do begin
      CenterBrightness := SrcPixel^;
      AvgBrightness := CalcAverageBrightness([SrcPixel0^, SrcPixel1^, SrcPixel2^, SrcPixel3^,
          SrcPixel4^, SrcPixel5^, SrcPixel6^, SrcPixel7^, SrcPixel8^, SrcPixel9^]);
      CenterBrightness := (CenterBrightness - AvgBrightness);
      WorkBuffer[Row, Column] := CenterBrightness;
      if CenterBrightness < MinBrightness then
        MinBrightness := CenterBrightness;
      if CenterBrightness > MaxBrightness then
        MaxBrightness := CenterBrightness;

      Inc(SrcPixel0);
      Inc(SrcPixel1);
      Inc(SrcPixel2);
      Inc(SrcPixel3);
      Inc(SrcPixel4);
      Inc(SrcPixel5);
      Inc(SrcPixel6);
      Inc(SrcPixel7);
      Inc(SrcPixel8);
      Inc(SrcPixel9);
      Inc(SrcPixel);
      Inc(DstPixel);
    end;

    // copy rightmost column(s) unchanged
    for Column := Offset + WorkAreaWidth to BmpWidth - 1 do begin
      CenterBrightness := SrcPixel^;
      WorkBuffer[Row, Column] := CenterBrightness;
      DstPixel^ := CenterBrightness;
      Inc(SrcPixel);
      Inc(DstPixel);
    end;

    Dec(SrcRow, BytesPerLine);
    Dec(DstRow, BytesPerLine);
  end;

  // copy top row(s) unchanged
  for Row := Offset + WorkAreaHeight to BmpHeight - 1 do begin
    DstPixel := PPixel(DstRow);
    SrcPixel := PPixel(SrcRow);
    for Column := 0 to BmpWidth - 1 do begin
      CenterBrightness := SrcPixel^;
      WorkBuffer[Row, Column] := CenterBrightness;
      DstPixel^ := CenterBrightness;
      Inc(SrcPixel);
      Inc(DstPixel);
    end;
    Dec(SrcRow, BytesPerLine);
    Dec(DstRow, BytesPerLine);
  end;

  DstRow := DstBottomRow;
  for Row := 0 to BmpHeight - 1 do begin
    DstPixel := PPixel(DstRow);
    for Column := 0 to BmpWidth - 1 do begin
      CenterBrightness := WorkBuffer[Row, Column];
      if (Row >= Offset) and (Row < Offset + WorkAreaHeight) and (Column >= Offset) and (Column < Offset + WorkAreaWidth) then
        CenterBrightness := (CenterBrightness - MinBrightness) * 255 * 100 div (MaxBrightness - MinBrightness) div 100;
      DstPixel^ := ReduceToByte(CenterBrightness);
      Inc(DstPixel);
    end;
    Dec(DstRow, BytesPerLine);
  end;
end;

function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool;
begin
  Result := TCanvas_BitBlt(
    _DestBmp.Canvas,
    _DestPos,
    _Size,
    _Src,
    _SrcPos,
    _Rop);
end;

function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap;
  _Rop: DWORD = SRCCOPY): LongBool;
begin
  Result := TCanvas_BitBlt(_DestBmp.Canvas, _DestPos, _Size, _Src, _Rop);
end;

function TBitmap_BitBlt(_DestBmp: TBitmap; _DestRect: TRect; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool;
begin
  Result := TCanvas_BitBlt(
    _DestBmp.Canvas,
    _DestRect,
    _Src,
    _SrcPos,
    _Rop);
end;

function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool;
begin
  Result := TCanvas_BitBlt(
    _DestBmp.Canvas,
    _DestPos,
    _Src,
    _SrcPos,
    _Rop);
end;

function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Src: TBitmap; _Rop: DWORD): LongBool;
begin
  Result := TCanvas_BitBlt(
    _DestBmp.Canvas,
    _DestPos,
    _Src,
    Point(0, 0),
    _Rop);
end;

function TBitmap8_TryCalcAverage(_bmp: TBitmap; _LowCutoff, _HighCutoff: Byte;
  out _Average: Byte): Boolean;
const
  BytesPerPixel = 1;
var
  w: Integer;
  h: Integer;
  x: Integer;
  y: Integer;
  ScanLine: PByte;
  Pixel: PByte;
  Value: Byte;
  Sum: Integer;
  cnt: Integer;
  BytesPerLine: Integer;
begin
  Assert(AssertPixelFormat(_bmp, pf8bit));

  h := _bmp.Height;
  if h = 0 then begin
    Result := False;
    Exit; //==>
  end;

  w := _bmp.Width;

  BytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

  Sum := 0;
  cnt := 0;
  ScanLine := _bmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(ScanLine = _bmp.ScanLine[y]);
    Pixel := ScanLine;
    for x := 0 to w - 1 do begin
      Value := Pixel^;
      if (Value >= _LowCutoff) and (Value <= _HighCutoff) then begin
        Sum := Sum + Value;
        Inc(cnt);
      end;
      Inc(Pixel, BytesPerPixel);
    end;
    Dec(ScanLine, BytesPerLine);
  end;
  Result := (cnt > 0);
  if Result then
    _Average := Sum div cnt;
end;

function TBitmap24_TryCalcAverage(_bmp: TBitmap; _LowCutoff, _HighCutoff: Byte;
  _Channel: TRgbBrightnessChannelEnum;
  out _Average: Byte): Boolean;
const
  BytesPerPixel = SizeOf(TdzRgbTriple);
var
  w: Integer;
  h: Integer;
  x: Integer;
  y: Integer;
  ScanLine: PByte;
  Pixel: PByte;
  Value: Byte;
  Sum: Integer;
  cnt: Integer;
  BytesPerLine: Integer;
begin
  Assert(AssertPixelFormat(_bmp, pf24bit));

  h := _bmp.Height;

  if h = 0 then begin
    Result := False;
    Exit; //==>
  end;

  w := _bmp.Width;

  BytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

  Sum := 0;
  cnt := 0;
  ScanLine := _bmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(ScanLine = _bmp.ScanLine[y]);
    Pixel := ScanLine;
    for x := 0 to w - 1 do begin
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
      Value := PdzRgbTriple(Pixel).GetBrightness(_Channel);
{$ELSE}
      Value := GetRgbBrightness(PdzRgbTriple(Pixel).Red, PdzRgbTriple(Pixel).Green, PdzRgbTriple(Pixel).Blue, _Channel);
{$ENDIF}
      if (Value >= _LowCutoff) and (Value <= _HighCutoff) then begin
        Sum := Sum + Value;
        Inc(cnt);
      end;
      Inc(Pixel, BytesPerPixel);
    end;
    Dec(ScanLine, BytesPerLine);
  end;
  Result := (cnt > 0);
  if Result then
    _Average := Sum div cnt;
end;

function TBitmap24_TryCalcAverageBlue(_bmp: TBitmap; _LowCutoff, _HighCutoff: Byte;
  out _Average: Byte): Boolean;
const
  BytesPerPixel = SizeOf(TdzRgbTriple);
var
  w: Integer;
  h: Integer;
  x: Integer;
  y: Integer;
  ScanLine: PByte;
  Pixel: PByte;
  Value: Byte;
  Sum: Integer;
  cnt: Integer;
  BytesPerLine: Integer;
begin
  Assert(AssertPixelFormat(_bmp, pf24bit));

  h := _bmp.Height;

  if h = 0 then begin
    Result := False;
    Exit; //==>
  end;

  w := _bmp.Width;

  BytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

  Sum := 0;
  cnt := 0;
  ScanLine := _bmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(ScanLine = _bmp.ScanLine[y]);
    Pixel := ScanLine;
    for x := 0 to w - 1 do begin
      // the first byte of the triple is the blue value
      Value := Pixel^;
      if (Value >= _LowCutoff) and (Value <= _HighCutoff) then begin
        Sum := Sum + Value;
        Inc(cnt);
      end;
      Inc(Pixel, BytesPerPixel);
    end;
    Dec(ScanLine, BytesPerLine);
  end;
  Result := (cnt > 0);
  if Result then
    _Average := Sum div cnt;
end;

function ColorBrightness(_Red, _Green, _Blue: Byte): Byte;
begin
  Result := GetFastLuminance(_Red, _Green, _Blue);
end;

function ColorBrightness(_Color: TColor): Byte;
var
  RGB: TdzRgbTriple;
begin
  TdzRgbTriple_SetColor(RGB, _Color);
  Result := TdzRgbTriple_GetFastLuminance(RGB);
end;

function BestForegroundForColor(_Red, _Green, _Blue: Byte): TColor;
begin
  if ColorBrightness(_Red, _Green, _Blue) < 123 then
    Result := clWhite
  else
    Result := clBlack;
end;

function BestForegroundForColor(_Color: TColor): TColor;
begin
  if ColorBrightness(_Color) < 123 then
    Result := clWhite
  else
    Result := clBlack;
end;

function TryStr2Color(const _s: string; out _Color: TColor): Boolean;
var
  c: Integer;
begin
  Result := IdentToColor(_s, c);
  if not Result then
    Result := TryStr2Int(_s, c);
  if Result then
    _Color := c;
end;

{ TPixelFilterCutoff }

constructor TPixelFilterCutoff.Create(_CutOff: Byte);
begin
  inherited Create;
  FCutOff := _CutOff;
end;

procedure TPixelFilterCutoff.FilterCallback(_x, _y: Integer; var _Pixel: TdzRgbTriple);
begin
  if _Pixel.Blue > FCutOff then
    _Pixel.Blue := FCutOff;
  if _Pixel.Red > FCutOff then
    _Pixel.Red := FCutOff;
  if _Pixel.Green > FCutOff then
    _Pixel.Green := FCutOff;
end;

procedure TPixelFilterCutoff.FilterCallback(_x, _y: Integer; var _Pixel: Byte);
begin
  if _Pixel > FCutOff then
    _Pixel := FCutOff;
end;

{ TPixelFilterStretch }

constructor TPixelFilterStretch.Create(_LowCutoff, _HighCutoff: Byte);
begin
  inherited Create;
  FLowCutOff := _LowCutoff;
  FHighCutOff := _HighCutoff;
  FDivisor := _HighCutoff - _LowCutoff;
end;

procedure TPixelFilterStretch.StretchColor(var _Color: Byte);
var
  Value: Integer;
begin
  Value := _Color;
  if (Value > FLowCutOff) and (Value < FHighCutOff) then begin
    Value := ((Value - FLowCutOff) * 256) div FDivisor;
    _Color := ReduceToByte(Value);
  end;
end;

procedure TPixelFilterStretch.FilterCallback(_x, _y: Integer; var _Pixel: TdzRgbTriple);
begin
  StretchColor(_Pixel.Blue);
  StretchColor(_Pixel.Red);
  StretchColor(_Pixel.Green);
end;

procedure TPixelFilterStretch.FilterCallback(_x, _y: Integer; var _Pixel: Byte);
begin
  StretchColor(_Pixel);
end;

{ TPixelFilterMove }

constructor TPixelFilterMove.Create(_MoveBy: Integer);
begin
  inherited Create;
  if _MoveBy > 200 then
    raise Exception.CreateFmt(_('MoveBy parameter (%d) must not be > 200'), [_MoveBy]);
  if _MoveBy < -200 then
    raise Exception.CreateFmt(_('MoveBy parameter (%d) must not be < -200'), [_MoveBy]);
  FMoveBy := _MoveBy;
end;

procedure TPixelFilterMove.MoveColor(var _Color: Byte);
var
  Value: Integer;
begin
  Value := _Color;
  Value := Value + FMoveBy;
  _Color := ReduceToByte(Value);
end;

procedure TPixelFilterMove.FilterCallback(_x, _y: Integer; var _Pixel: TdzRgbTriple);
begin
  MoveColor(_Pixel.Blue);
  MoveColor(_Pixel.Red);
  MoveColor(_Pixel.Green);
end;

procedure TPixelFilterMove.FilterCallback(_x, _y: Integer; var _Pixel: Byte);
begin
  MoveColor(_Pixel);
end;

function RainbowColor(_Hue: Double): TColor;
// taken from https://stackoverflow.com/a/19719171/49925
var
  Value: Double;
  IntValue: Integer;
begin
  Value := EnsureRange(_Hue, 0, 1) * 6;
  IntValue := Round(Frac(Value) * 255);
  case Trunc(Value) of
    0: Result := RGB(255, IntValue, 0);
    1: Result := RGB(255 - IntValue, 255, 0);
    2: Result := RGB(0, 255, IntValue);
    3: Result := RGB(0, 255 - IntValue, 255);
    4: Result := RGB(IntValue, 0, 255);
  else // 5
    Result := RGB(255, 0, 255 - IntValue);
  end;
end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
procedure RainbowColor(_Hue: Double; out _Color: TdzRgbTriple);
var
  Value: Double;
  IntValue: Integer;
begin
  Value := EnsureRange(_Hue, 0, 1) * 6;
  IntValue := Round(Frac(Value) * 255);
  case Trunc(Value) of
    0: _Color.SetValues(255, IntValue, 0);
    1: _Color.SetValues(255 - IntValue, 255, 0);
    2: _Color.SetValues(0, 255, IntValue);
    3: _Color.SetValues(0, 255 - IntValue, 255);
    4: _Color.SetValues(IntValue, 0, 255);
  else // 5
    _Color.SetValues(255, 0, 255 - IntValue);
  end;
end;
{$ENDIF}

procedure RainbowColor(_Brightness: Byte; out _Red, _Green, _Blue: Byte);
var
  Brightness: Integer;
  TruncValue: Word;
  FracValue: Word;
begin
  // This is supposed to be a faster version of the overloaded function that takes a double parameter
  // because it uses only integer arithmethic.
  // Note that I have not timed it, but it "feels" a bit faster. The result is the same.
  // -- 2022-03-17 twm
  Brightness := Integer(_Brightness) * 6;
  DivMod(Brightness, 255, TruncValue, FracValue);
  case TruncValue of
    0: begin
        _Red := 255;
        _Green := FracValue;
        _Blue := 0;
      end;
    1: begin
        _Red := 255 - FracValue;
        _Green := 255;
        _Blue := 0;
      end;
    2: begin
        _Red := 0;
        _Green := 255;
        _Blue := FracValue;
      end;
    3: begin
        _Red := 0;
        _Green := 255 - FracValue;
        _Blue := 255;
      end;
    4: begin
        _Red := FracValue;
        _Green := 0;
        _Blue := 255;
      end;
  else // 5
    _Red := 255;
    _Green := 0;
    _Blue := 255 - FracValue;
  end;
end;

procedure RainbowColor(_Brightness: Byte; out _Pixel: TdzRgbTriple);
begin
  RainbowColor(_Brightness, _Pixel.Red, _Pixel.Green, _Pixel.Blue);
end;

function RainbowColor(_Brightness: Byte): TColor;
var
  Red: Byte;
  Green: Byte;
  Blue: Byte;
begin
  RainbowColor(_Brightness, Red, Green, Blue);
  Result := RGB(Red, Green, Blue);
//  Assert(Result = RainbowColor(_Brightness / 255));
end;

function RainbowColor(_MinHue, _MaxHue, _Hue: Integer): TColor;
// taken from https://stackoverflow.com/a/19719171/49925
begin
  Result := RainbowColor((_Hue - _MinHue) / (_MaxHue - _MinHue + 1));
end;

function TBitmap8_GetHistogram(_bmp: TBitmap): TUInt64Array256;
var
  Average: UInt8;
begin
  TBitmapMono_GetHistogram(_bmp, Result, Average);
end;

procedure TBitmap8_GetHistogram(_bmp: TBitmap; out _Histogram: TUInt64Array256; out _Average: UInt8);
begin
  TBitmapMono_GetHistogram(_bmp, _Histogram, _Average);
end;

function TBitmapMono_GetHistogram(_bmp: TBitmap): TUInt64Array256;
var
  Average: UInt8;
begin
  TBitmapMono_GetHistogram(_bmp, Result, Average);
end;

procedure TBitmapMono_GetHistogram(_bmp: TBitmap; out _Histogram: TUInt64Array256; out _Average: UInt8);
var
  BytesPerPixel: Integer;
  w: Integer;
  h: Integer;
  x: Integer;
  y: Integer;
  ScanLine: PByte;
  Pixel: PByte;
  BytesPerLine: Integer;
  Sum: Int64;
begin
  Assert(AssertPixelFormat(_bmp, [pf8bit, pf24bit, pf32bit]));
  case _bmp.PixelFormat of
    pf8bit: BytesPerPixel := 1;
    pf24bit: BytesPerPixel := 3;
    pf32bit: BytesPerPixel := 4;
  else
    raise EdzPixelFormatNotSupported.Create(_bmp.PixelFormat)
  end;
  _Average := 0;
  for x := Low(_Histogram) to High(_Histogram) do
    _Histogram[x] := 0;

  h := _bmp.Height;
  if h = 0 then
    Exit; //==>

  w := _bmp.Width;
  if w = 0 then
    Exit; //==>

  BytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

  Sum := 0;
  ScanLine := _bmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(ScanLine = _bmp.ScanLine[y]);
    Pixel := ScanLine;
    for x := 0 to w - 1 do begin
      Inc(_Histogram[Pixel^]);
      Inc(Sum, Pixel^);
      Inc(Pixel, BytesPerPixel);
    end;
    Dec(ScanLine, BytesPerLine);
  end;
  _Average := (Sum div w) div h;
end;

function TBitmap24_GetHistogram(_bmp: TBitmap; _Channel: TRgbBrightnessChannelEnum): TUInt64Array256; overload;
const
  BytesPerPixel = SizeOf(TdzRgbTriple);
var
  w: Integer;
  h: Integer;
  x: Integer;
  y: Integer;
  ScanLine: PByte;
  Pixel: PByte;
  BytesPerLine: Integer;
begin
  Assert(AssertPixelFormat(_bmp, pf24bit));

  for x := Low(Result) to High(Result) do
    Result[x] := 0;

  h := _bmp.Height;
  if h = 0 then begin
    Exit; //==>
  end;

  w := _bmp.Width;

  BytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

  ScanLine := _bmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(ScanLine = _bmp.ScanLine[y]);
    Pixel := ScanLine;
    for x := 0 to w - 1 do begin
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
      Inc(Result[PdzRgbTriple(Pixel).GetBrightness(_Channel)]);
{$ELSE}
      Inc(Result[GetRgbBrightness(PdzRgbTriple(Pixel).Red, PdzRgbTriple(Pixel).Green, PdzRgbTriple(Pixel).Blue, _Channel)]);
{$ENDIF}
      Inc(Pixel, BytesPerPixel);
    end;
    Dec(ScanLine, BytesPerLine);
  end;
end;

procedure TBitmap24_GetHistograms(_bmp: TBitmap; out _Red, _Green, _Blue: TUInt64Array256); overload;
const
  BytesPerPixel = SizeOf(TdzRgbTriple);
var
  w: Integer;
  h: Integer;
  x: Integer;
  y: Integer;
  ScanLine: PByte;
  Pixel: PByte;
  BytesPerLine: Integer;
begin
  Assert(AssertPixelFormat(_bmp, pf24bit));

  for x := Low(_Red) to High(_Red) do begin
    _Red[x] := 0;
    _Green[x] := 0;
    _Blue[x] := 0;
  end;

  h := _bmp.Height;
  if h = 0 then begin
    Exit; //==>
  end;

  w := _bmp.Width;

  BytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

  ScanLine := _bmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(ScanLine = _bmp.ScanLine[y]);
    Pixel := ScanLine;
    for x := 0 to w - 1 do begin
      Inc(_Red[PdzRgbTriple(Pixel).Red]);
      Inc(_Green[PdzRgbTriple(Pixel).Green]);
      Inc(_Blue[PdzRgbTriple(Pixel).Blue]);
      Inc(Pixel, BytesPerPixel);
    end;
    Dec(ScanLine, BytesPerLine);
  end;
end;

procedure TBitmap24_GetHistograms(_bmp: TBitmap; _BrightnessChannel: TRgbBrightnessChannelEnum;
  out _Red, _Green, _Blue, _Brightness: TUInt64Array256); overload;
const
  BytesPerPixel = SizeOf(TdzRgbTriple);
var
  w: Integer;
  h: Integer;
  x: Integer;
  y: Integer;
  ScanLine: PByte;
  Pixel: PByte;
  BytesPerLine: Integer;
begin
  for x := Low(_Red) to High(_Red) do begin
    _Red[x] := 0;
    _Green[x] := 0;
    _Blue[x] := 0;
    _Brightness[x] := 0;
  end;

  h := _bmp.Height;
  if h = 0 then begin
    Exit; //==>
  end;

  w := _bmp.Width;

  BytesPerLine := ((w * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(w, BytesPerPixel * 8, 32));

  ScanLine := _bmp.ScanLine[0];
  for y := 0 to h - 1 do begin
    Assert(ScanLine = _bmp.ScanLine[y]);
    Pixel := ScanLine;
    for x := 0 to w - 1 do begin
      Inc(_Red[PdzRgbTriple(Pixel).Red]);
      Inc(_Green[PdzRgbTriple(Pixel).Green]);
      Inc(_Blue[PdzRgbTriple(Pixel).Blue]);
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
      Inc(_Brightness[PdzRgbTriple(Pixel).GetBrightness(_BrightnessChannel)]);
{$ELSE}
      Inc(_Brightness[GetRgbBrightness(PdzRgbTriple(Pixel).Red, PdzRgbTriple(Pixel).Green, PdzRgbTriple(Pixel).Blue, _BrightnessChannel)]);
{$ENDIF}
      Inc(Pixel, BytesPerPixel);
    end;
    Dec(ScanLine, BytesPerLine);
  end;
end;

function TPicture_TryLoadMatchingFile(_pic: TPicture; const _FileMask: string): Boolean;
var
  fn: string;
begin
  Result := False;
  if TFileSystem.FindMatchingFile(_FileMask, fn) = mfFile then begin
    try
      _pic.LoadFromFile(fn);
      Result := True;
    except
    end;
  end;
end;

{$IFNDEF PICTURE_HAS_PUBLIC_LOADFROMSTREAM}
function TPicture_TryLoadFromJpgStream(_pic: TPicture; _st: TStream): Boolean;
var
  jpg: TJPEGImage;
begin
  jpg := TJPEGImage.Create;
  try
    try
      _st.Position := 0;
      jpg.LoadFromStream(_st);
      _pic.Bitmap.Assign(jpg);
      Result := True;
    except
      Result := False;
    end;
  finally
    FreeAndNil(jpg);
  end;
end;

function TPicture_TryLoadFromBmpStream(_pic: TPicture; _st: TStream): Boolean;
begin
  try
    _st.Position := 0;
    _pic.Bitmap.LoadFromStream(_st);
    Result := True;
  except
    Result := False;
  end;
end;
{$ENDIF}

function TPicture_TryLoadFromResource(_pic: TPicture; const _ResName: string): Boolean;
var
  ResStream: TResourceStream;
begin
  Assert(Assigned(_pic));
  try
    ResStream := TResourceStream.Create(HInstance, _ResName, RT_rcdata);
    try
      ResStream.Position := 0;
{$IFDEF PICTURE_HAS_PUBLIC_LOADFROMSTREAM}
      _pic.LoadFromStream(ResStream);
      Result := True;
{$ELSE}
      Result := TPicture_TryLoadFromJpgStream(_pic, ResStream);
      if not Result then
        Result := TPicture_TryLoadFromBmpStream(_pic, ResStream);
{$ENDIF}
    finally
      FreeAndNil(ResStream);
    end;
  except
    Result := False;
  end;
end;

{ EdzPixelFormatNotSupported }

constructor EdzPixelFormatNotSupported.Create(_PixelFormat: TPixelFormat);
var
  PixelFormatName: string;
begin
  PixelFormatName := GetEnumName(TypeInfo(TPixelFormat), Ord(_PixelFormat));
  CreateFmt(_('PixelFormat %s not supported'), [PixelFormatName]);
end;

end.
// Here, Delphi 2007 sometimes throws a [DCC Error] F2084 Internal Error: AV06FA6FD9-R00000D1A-0
// Usually it helps to do a full rebuild or delete the DCU output directory contents
// In one case the problem went away when I changed the order of units in the project file

