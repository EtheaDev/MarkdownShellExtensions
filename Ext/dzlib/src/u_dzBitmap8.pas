unit u_dzBitmap8;

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
{$UNDEF SUPPORTS_INLINE}
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

interface

uses
  Windows,
  SysUtils,
  Types,
  Classes,
  Graphics,
{$IFDEF dzUseGraphics32}
  GR32, // libs\graphics32\src
{$ENDIF dzUseGraphics32}
  u_dzTypes,
  u_dzGraphicsUtils;

type
  ///<summary>
  /// This is a simple wrapper around a buffer containing an 8 bit grayscale bitmap.
  /// It is supposed to remove the drawbacks of using TBitmap (in particular in multithreaded
  /// code.
  TdzBitmap8 = class
  private
    FWidth: Integer;
    FHeight: Integer;
    ///<summary>
    /// Points to the buffer for storing the data. It has the same  structure as that of a TBitmap
    /// with PixelFormat pf8Bit, so the lines are stored bottom to top. </summary>
    FBuffer: PByte;
    FBytesPerLine: Integer;
  public
    destructor Destroy; override;
    procedure SetSize(_Width, _Height: Integer);
    procedure Assign(_Src: TdzBitmap8);
    procedure AssignBmp(_Bitmap: TBitmap);
    ///<summary>
    /// Assigns the TdzBitmap8 to the given bitmap.
    /// The PixelFormat of the bitmap will be set to pf8Bit and the palette set to grayscale
    /// and the Size to the Width and Height of the TdzBitmap8. </summary>
    procedure AssignToBmp8(_Bitmap: TBitmap);
    ///<summary>
    /// Assigns the TdzBitmap8 to the given bitmap.
    /// The PixelFormat of the bitmap will be set to pf24Bit
    /// and the Size to the Width and Height of the TdzBitmap8. </summary>
    procedure AssignToBmp24(_Bitmap: TBitmap);
    ///<summary>
    /// Assigns the TdzBitmap8 to the given bitmap.
    /// The PixelFormat of the bitmap will be set to pf32Bit
    /// and the Size to the Width and Height of the TdzBitmap8. </summary>
    procedure AssignToBmp32(_Bitmap: TBitmap); overload;
{$IFDEF dzUseGraphics32}
    ///<summary>
    /// Assigns the TdzBitmap8 to the given bitmap.
    /// The PixelFormat of the bitmap will be set to pf32Bit
    /// and the Size to the Width and Height of the TdzBitmap8. </summary>
    procedure AssignToBmp32(_Bitmap: TBitmap32); overload;
{$ENDIF dzUseGraphics32}
    ///<summary>
    /// Assigns the TdzBitmap8 to the given bitmap with PixelFormat = pf24Bit
    /// @NOTE: The caller must ensure that given bitmap has the correct size and PixelFormat. </summary>
    procedure AssignToScanlines24(_TopScanLine: Pointer; _YIsReversed: Boolean);
    ///<summary>
    /// Assigns the TdzBitmap8 to the given bitmap with PixelFormat = pf32Bit
    /// @NOTE: The caller must ensure that given bitmap has the correct size and PixelFormat.
    /// @NOTE: This can be used to assign to a TBitmap32 (from Graphics32).
    ///        But remember that these bitmaps are not(!) stored upside down </summary>
    procedure AssignToScanlines32(_TopScanLine: Pointer; _YIsReversed: Boolean);
    ///<summary>
    /// assign the given area of the content as a 8 bit Bitmap </summary>
    procedure AssignAreaToBmp8(_Bitmap: TBitmap; _Left, _Top, _Width, _Height: Integer); overload;
    procedure AssignAreaToBmp8(_Bitmap: TBitmap; _Left, _Top: Integer); overload;
    procedure AssignAreaToBmp8(_Bitmap: TBitmap; _Rect: TRect); overload;
    ///<summary>
    /// assign the given area of the content as a 24 bit Bitmap </summary>
    procedure AssignAreaToBmp24(_Bitmap: TBitmap; _Left, _Top, _Width, _Height: Integer);
    ///<summary>
    /// assign the given area of the content as a 32 bit Bitmap </summary>
    procedure AssignAreaToBmp32(_Bitmap: TBitmap; _Left, _Top, _Width, _Height: Integer);
    ///<summary>
    /// assign the given area of the content as a 8 bit Bitmap
    /// @NOTE: The caller must ensure that given bitmap has the correct size, PixelFormat and palette. </summary>
    procedure AssignAreaToScanlines8(_TopScanLine: Pointer; _Left, _Top, _Width, _Height: Integer;
      _YIsReversed: Boolean);
    ///<summary>
    /// assign the given area of the content as a 24 bit Bitmap
    /// @NOTE: The caller must ensure that given bitmap has the correct size and PixelFormat. </summary>
    procedure AssignAreaToScanlines24(_TopScanLine: Pointer; _Left, _Top, _Width, _Height: Integer;
      _YIsReversed: Boolean);
    ///<summary>
    /// assign the given area of the content as a 32 bit Bitmap
    /// @NOTE: The caller must ensure that given bitmap has the correct size and PixelFormat.
    /// @NOTE: This can be used to assign to a TBitmap32 (from Graphics32).
    ///        But remember that these bitmaps are not(!) stored upside down </summary>
    procedure AssignAreaToScanlines32(_TopScanLine: Pointer; _Left, _Top, _Width, _Height: Integer;
      _YIsReversed: Boolean);
    function CalcScanline(_y: Integer): PByte;
    function CalcPixel(_y, _x: Integer): PByte;
    ///<summary>
    /// Calculates the average brightness
    /// @param LowCutoff is the lower brightness limit for pixels to be included in the calculation
    /// @param HighCutoff is the upper brightness limit for pixels to be included in the calculation
    /// @param Average returns the calculated average, only valid if Result = True
    /// @returns True, if at least on pixel was in the desired interval
    ///          False, if not </summary>
    function TryCalcAverage(_LowCutoff, _HighCutoff: Byte; out _Average: Byte): Boolean;
    ///</summary>
    /// Apply the given gamma curve </summary>
    procedure ApplyGamma(_DstBmp: TdzBitmap8; const _Gamma: TGammaCurve);
    ///<summary>
    /// Sharpens the bitmap
    /// @param DstBmp is the output (the sharpened picture)
    /// @param Alpha is a matrix of sharpen factors for each pixel
    ///        Dimensions must match the bitmap, *remember* *that* *y* *is* *reversed*.
    ///        Values must be >= 0 and <=5 </summary>
    procedure Sharpen(_DstBmp: TdzBitmap8; const _AlphaMap: TSingleMatrix);
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property BytesPerLine: Integer read FBytesPerLine;
    property Buffer: PByte read FBuffer;
  end;

implementation

uses
  Math,
  u_dzTypesUtils;

{ TdzBitmap8 }

destructor TdzBitmap8.Destroy;
begin
  FreeMem(FBuffer);
  inherited;
end;

procedure TdzBitmap8.SetSize(_Width, _Height: Integer);
const
  BytesPerPixel = 1;
begin
  FWidth := _Width;
  FHeight := _Height;
  FBytesPerLine := ((FWidth * 8 * BytesPerPixel + 31) and not 31) div 8;
  ReallocMem(FBuffer, FHeight * FBytesPerLine);
end;

procedure TdzBitmap8.Sharpen(_DstBmp: TdzBitmap8; const _AlphaMap: TSingleMatrix);
type
  PPixel = PByte;
const
  BytesPerPixel = 1;
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
  AlphaEntrySize: IntPtr;
  AlphaPtr: PSingle;
begin
  // sharpening is blending of the current pixel
  // with the average of the surrounding ones,
  // but with a negative weight for the average
  Assert((FWidth > 2) and (FHeight > 2), 'Bitmap must be at least 3x3');
  Assert((Length(_AlphaMap) = FHeight),
    Format('Number of lines in AlphaMap (%d) must match bitmap height (%d)', [Length(_AlphaMap), FHeight]));

  _DstBmp.SetSize(FWidth, FHeight);

  WorkAreaWidth := FWidth - 2;
  WorkAreaHeight := FHeight - 2;

  AlphaEntrySize := PtrDiff(@(_AlphaMap[0][1]), @(_AlphaMap[0][0]));

  SrcRow := CalcScanline(0);
  DstRow := _DstBmp.CalcScanline(0);
  SrcPixelCenter := PPixel(SrcRow);
  DstPixel := PPixel(DstRow);

  // Copy first row unchanged
  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixelCenter^;
    Inc(DstPixel);
    Inc(SrcPixelCenter);
  end;

  for Row := 1 to WorkAreaHeight do begin
    Assert(Length(_AlphaMap[Row]) = FWidth,
      Format('Number of values in AlphaMap[%d] (%d) must match bitmap width (%d)',
      [Row, Length(_AlphaMap[Row]), FWidth]));

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
      DstPixel^ := EnsureRange(AvgBrightness, 0, MaxUInt8);

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

function TdzBitmap8.TryCalcAverage(_LowCutoff, _HighCutoff: Byte; out _Average: Byte): Boolean;
const
  BytesPerPixel = 1;
var
  x: Integer;
  y: Integer;
  ScanLine: PByte;
  Pixel: PByte;
  Value: Byte;
  Sum: Integer;
  cnt: Integer;
begin
  Sum := 0;
  cnt := 0;
  ScanLine := CalcScanline(0);
  for y := 0 to Height - 1 do begin
    Pixel := ScanLine;
    for x := 0 to Width - 1 do begin
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

procedure TdzBitmap8.ApplyGamma(_DstBmp: TdzBitmap8; const _Gamma: TGammaCurve);
const
  BytesPerPixel = 1;
var
  x: Integer;
  y: Integer;
  SrcLine: PByte;
  DstLine: PByte;
  SrcPixel: PByte;
  DstPixel: PByte;
begin
  _DstBmp.SetSize(FWidth, FHeight);

  if FHeight = 0 then
    Exit; //==>

  SrcLine := CalcScanline(0);
  DstLine := _DstBmp.CalcScanline(0);
  for y := 0 to FHeight - 1 do begin
    Assert(SrcLine = CalcScanline(y));
    Assert(DstLine = _DstBmp.CalcScanline(y));
    SrcPixel := SrcLine;
    DstPixel := DstLine;
    for x := 0 to FWidth - 1 do begin
      DstPixel^ := _Gamma[SrcPixel^];
      Inc(SrcPixel, BytesPerPixel);
      Inc(DstPixel, BytesPerPixel);
    end;
    Dec(SrcLine, BytesPerLine);
    Dec(DstLine, BytesPerLine);
  end;
end;

procedure TdzBitmap8.Assign(_Src: TdzBitmap8);
begin
  SetSize(_Src.Width, _Src.Height);
  Move(_Src.Buffer^, FBuffer^, FBytesPerLine * FHeight);
end;

procedure TdzBitmap8.AssignBmp(_Bitmap: TBitmap);
var
  y: Integer;
  x: Integer;
  BmpPixel: PByte;
  BufPixel: PByte;
  BmpBytesPerPixel: Integer;
  BmpBytesPerLine: Integer;
  BmpScanline: PByte;
  BufScanline: PByte;
begin
  case _Bitmap.PixelFormat of
    pf8bit: BmpBytesPerPixel := 1;
    pf24bit: BmpBytesPerPixel := 3;
    pf32bit: BmpBytesPerPixel := 4;
  else
    raise EdzPixelFormatNotSupported.Create(_Bitmap.PixelFormat)
  end;

  SetSize(_Bitmap.Width, _Bitmap.Height);

  BmpBytesPerLine := ((FWidth * 8 * BmpBytesPerPixel + 31) and not 31) div 8;

  BmpScanline := _Bitmap.ScanLine[FHeight - 1];
  BufScanline := FBuffer;
  if BmpBytesPerPixel = 1 then begin
    // This is the simplest and fastest case: Copy the whole buffer in one go
    Move(BmpScanline^, BufScanline^, FHeight * BmpBytesPerLine);
  end else begin
    // This is much slower since we need to copy one byte for each pixel. Since we are dealing with
    // grayscale bitmaps, we can simply take the first byte of each pixel.
    for y := 0 to FHeight - 1 do begin
      BmpPixel := BmpScanline;
      BufPixel := BufScanline;
      for x := 0 to FWidth - 1 do begin
        BufPixel^ := BmpPixel^;
        Inc(BufPixel);
        Inc(BmpPixel, BmpBytesPerPixel);
      end;
      Inc(BmpScanline, BmpBytesPerLine);
      Inc(BufScanline, BytesPerLine);
    end;
  end;
end;

procedure TdzBitmap8.AssignAreaToBmp8(_Bitmap: TBitmap; _Left, _Top, _Width, _Height: Integer);
begin
  Assert(_Width > 0);
  Assert(_Height > 0);
  Assert(_Left + _Width <= FWidth);
  Assert(_Top + _Height <= FHeight);

  _Bitmap.SetSize(0, 0);
  TBitmap_MakeMono8(_Bitmap);
  _Bitmap.SetSize(_Width, _Height);

  AssignAreaToScanlines8(_Bitmap.ScanLine[0], _Left, _Top, _Width, _Height, True);
end;

procedure TdzBitmap8.AssignAreaToBmp8(_Bitmap: TBitmap; _Left, _Top: Integer);
begin
  AssignAreaToBmp8(_Bitmap, _Left, _Top, _Bitmap.Width, _Bitmap.Height);
end;

procedure TdzBitmap8.AssignAreaToBmp8(_Bitmap: TBitmap; _Rect: TRect);
begin
  AssignAreaToBmp8(_Bitmap, _Rect.Left, _Rect.Top, TRect_Width(_Rect), TRect_Height(_Rect));
end;

procedure TdzBitmap8.AssignAreaToScanlines8(_TopScanLine: Pointer; _Left, _Top, _Width, _Height: Integer;
  _YIsReversed: Boolean);
const
  BmpBytesPerPixel = 1;
var
  y: Integer;
  BmpPixel: PByte;
  BufPixel: PByte;
  BmpBytesPerLine: Integer;
  BmpScanline: Pointer;
  BufScanline: PByte;
begin
  Assert(_Width > 0);
  Assert(_Height > 0);
  Assert(_Left + _Width <= FWidth);
  Assert(_Top + _Height <= FHeight);

  BmpBytesPerLine := ((_Width * 8 * BmpBytesPerPixel + 31) and not 31) div 8;
  if _YIsReversed then
    BmpBytesPerLine := -BmpBytesPerLine;

  BmpScanline := _TopScanLine;
  BufScanline := CalcScanline(_Top);
  Inc(BufScanline, _Left);
  y := _Height;
  while y > 0 do begin
    BufPixel := BufScanline;
    BmpPixel := BmpScanline;
    Move(BufPixel^, BmpPixel^, _Width);
    IncPtr(BmpScanline, BmpBytesPerLine);
    Dec(BufScanline, BytesPerLine);
    Dec(y);
  end;
end;

procedure TdzBitmap8.AssignAreaToBmp24(_Bitmap: TBitmap; _Left, _Top, _Width, _Height: Integer);
begin
  Assert(_Width > 0);
  Assert(_Height > 0);
  Assert(_Left + _Width <= FWidth);
  Assert(_Top + _Height <= FHeight);

  _Bitmap.SetSize(0, 0);
  _Bitmap.PixelFormat := pf24bit;
  _Bitmap.SetSize(_Width, _Height);
  AssignAreaToScanlines24(_Bitmap.ScanLine[0], _Left, _Top, _Width, _Height, True);
end;

procedure TdzBitmap8.AssignAreaToScanlines24(_TopScanLine: Pointer; _Left, _Top, _Width, _Height: Integer;
  _YIsReversed: Boolean);
const
  BmpBytesPerPixel = 3;
var
  y: Integer;
  x: Integer;
  BmpPixel: PdzRgbTriple;
  BufPixel: PByte;
  BmpBytesPerLine: Integer;
  BmpScanline: Pointer;
  BufScanline: PByte;
  Value: Byte;
begin
  Assert(_Width > 0);
  Assert(_Height > 0);
  Assert(_Left + _Width <= FWidth);
  Assert(_Top + _Height <= FHeight);

  BmpBytesPerLine := ((_Width * 8 * BmpBytesPerPixel + 31) and not 31) div 8;
  if _YIsReversed then
    BmpBytesPerLine := -BmpBytesPerLine;

  BmpScanline := _TopScanLine;
  BufScanline := CalcScanline(_Top);
  Inc(BufScanline, _Left);
  y := _Height;
  while y > 0 do begin
    BmpPixel := BmpScanline;
    BufPixel := BufScanline;
    x := _Width;
    while x > 0 do begin
      Value := BufPixel^;
      BmpPixel.SetGray(Value);
      Inc(BufPixel);
      Inc(BmpPixel);
      Dec(x);
    end;
    IncPtr(BmpScanline, BmpBytesPerLine);
    Dec(BufScanline, BytesPerLine);
    Dec(y);
  end;
end;

procedure TdzBitmap8.AssignAreaToBmp32(_Bitmap: TBitmap; _Left, _Top, _Width, _Height: Integer);
begin
  Assert(_Width > 0);
  Assert(_Height > 0);
  Assert(_Left + _Width <= FWidth);
  Assert(_Top + _Height <= FHeight);

  _Bitmap.SetSize(0, 0);
  _Bitmap.PixelFormat := pf32bit;
  _Bitmap.SetSize(_Width, _Height);

  AssignAreaToScanlines32(_Bitmap.ScanLine[0], _Left, _Top, _Width, _Height, True);
end;

procedure TdzBitmap8.AssignAreaToScanlines32(_TopScanLine: Pointer; _Left, _Top, _Width, _Height: Integer;
  _YIsReversed: Boolean);
const
  BmpBytesPerPixel = 4;
var
  y: Integer;
  x: Integer;
  BmpPixel: PdzRgbQuad;
  BufPixel: PByte;
  BmpBytesPerLine: Integer;
  BmpScanline: Pointer;
  BufScanline: PByte;
  Value: Byte;
begin
  Assert(_Width > 0);
  Assert(_Height > 0);
  Assert(_Left + _Width <= FWidth);
  Assert(_Top + _Height <= FHeight);

  BmpBytesPerLine := ((_Width * 8 * BmpBytesPerPixel + 31) and not 31) div 8;
  if _YIsReversed then
    BmpBytesPerLine := -BmpBytesPerLine;

  BmpScanline := _TopScanLine;
  BufScanline := CalcScanline(_Top);
  Inc(BufScanline, _Left);
  y := _Height;
  while y > 0 do begin
    BmpPixel := BmpScanline;
    BufPixel := BufScanline;
    x := _Width;
    while x > 0 do begin
      Value := BufPixel^;
      BmpPixel.SetGray(Value);
      Inc(BufPixel);
      Inc(BmpPixel);
      Dec(x);
    end;
    IncPtr(BmpScanline, BmpBytesPerLine);
    Dec(BufScanline, BytesPerLine);
    Dec(y);
  end;
end;

procedure TdzBitmap8.AssignToBmp8(_Bitmap: TBitmap);
var
  BmpScanline: PByte;
begin
  _Bitmap.SetSize(0, 0);
  TBitmap_MakeMono8(_Bitmap);
  _Bitmap.SetSize(FWidth, FHeight);

  // Since the size and structure of the buffers are identical, we can copy the whole buffer in one go
  BmpScanline := _Bitmap.ScanLine[FHeight - 1];
  Move(FBuffer^, BmpScanline^, FHeight * BytesPerLine);
end;

procedure TdzBitmap8.AssignToBmp24(_Bitmap: TBitmap);
begin
  _Bitmap.SetSize(0, 0);
  _Bitmap.PixelFormat := pf24bit;
  _Bitmap.SetSize(FWidth, FHeight);

  AssignToScanlines24(_Bitmap.ScanLine[0], True);
end;

procedure TdzBitmap8.AssignToScanlines24(_TopScanLine: Pointer; _YIsReversed: Boolean);
const
  BmpBytesPerPixel = 3;
var
  y: Integer;
  x: Integer;
  BmpPixel: PdzRgbTriple;
  BufPixel: PByte;
  BmpBytesPerLine: Integer;
  BmpScanline: Pointer;
  BufScanline: PByte;
  Value: Byte;
begin
  BmpBytesPerLine := ((FWidth * 8 * BmpBytesPerPixel + 31) and not 31) div 8;
  if _YIsReversed then
    BmpBytesPerLine := -BmpBytesPerLine;

  BmpScanline := _TopScanLine;
  BufScanline := CalcScanline(0);
  y := FHeight;
  while y > 0 do begin
    BmpPixel := BmpScanline;
    BufPixel := BufScanline;
    x := FWidth;
    while x > 0 do begin
      Value := BufPixel^;
      BmpPixel.Blue := Value;
      BmpPixel.Green := Value;
      BmpPixel.Red := Value;
      Inc(BufPixel);
      Inc(BmpPixel);
      Dec(x);
    end;
    IncPtr(BmpScanline, BmpBytesPerLine);
    Dec(BufScanline, BytesPerLine);
    Dec(y);
  end;
end;

procedure TdzBitmap8.AssignToBmp32(_Bitmap: TBitmap);
begin
  _Bitmap.SetSize(0, 0);
  _Bitmap.PixelFormat := pf32bit;
  _Bitmap.SetSize(FWidth, FHeight);

  AssignToScanlines32(_Bitmap.ScanLine[0], True);
end;

procedure TdzBitmap8.AssignToScanlines32(_TopScanLine: Pointer; _YIsReversed: Boolean);
const
  BmpBytesPerPixel = 4;
var
  y: Integer;
  x: Integer;
  BmpPixel: PdzRgbQuad;
  BufPixel: PByte;
  BmpBytesPerLine: Integer;
  BmpScanline: Pointer;
  BufScanline: PByte;
  Value: Byte;
begin
  BmpBytesPerLine := ((FWidth * 8 * BmpBytesPerPixel + 31) and not 31) div 8;
  if _YIsReversed then
    BmpBytesPerLine := -BmpBytesPerLine;

  BmpScanline := _TopScanLine;
  BufScanline := CalcScanline(0);
  y := FHeight;
  while y > 0 do begin
    BmpPixel := BmpScanline;
    BufPixel := BufScanline;
    x := FWidth;
    while x > 0 do begin
      Value := BufPixel^;
      BmpPixel.Blue := Value;
      BmpPixel.Green := Value;
      BmpPixel.Red := Value;
      BmpPixel.Reserved := Value;
      Inc(BufPixel);
      Inc(BmpPixel);
      Dec(x);
    end;
    IncPtr(BmpScanline, BmpBytesPerLine);
    Dec(BufScanline, BytesPerLine);
    Dec(y);
  end;
end;

{$IFDEF dzUseGraphics32}
procedure TdzBitmap8.AssignToBmp32(_Bitmap: TBitmap32);
begin
  _Bitmap.SetSize(0, 0);
  _Bitmap.SetSize(FWidth, FHeight);
  AssignToScanlines32(_Bitmap.ScanLine[0], False);
end;
{$ENDIF dzUseGraphics32}

function TdzBitmap8.CalcPixel(_y, _x: Integer): PByte;
begin
  Result := AddToPtr(CalcScanline(_y), _x);
end;

function TdzBitmap8.CalcScanline(_y: Integer): PByte;
begin
  Result := AddToPtr(FBuffer, (FHeight - 1 - _y) * BytesPerLine);
end;

end.
