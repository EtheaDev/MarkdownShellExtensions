///<summary>
/// This unit contains older versions of TBitmap8_Sharpen for timing purposes.
/// The current version of these procedures is in u_dzGraphicsUtils. </summary>
unit u_dzSharpen8;

{$INCLUDE 'dzlib.inc'}

{$IFDEF OPTIMIZE_DZ_GRAPHIC_UTILS}
{$OPTIMIZATION ON}
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

{$IFNDEF NO_DZSHARPEN8_HINT}
{$MESSAGE HINT 'The procedures in this unit are old versions of TBitmap8_Sharpen.'}
{$ENDIF}

interface

uses
  Windows,
  Types,
  SysUtils,
  Graphics,
  u_dzTranslator,
  u_dzGraphicsUtils,
  u_dzTypes;

///<summary>
/// Sharpens a bitmap, pixelformat must be pf24bit
/// @param SrcBmp is the input
/// @param DstBmp is the output (the sharpened picture)
/// @param Alpha is the sharpen factor, must be >1
/// source: https://www.swissdelphicenter.ch/en/showcode.php?id=1948
/// but modified for 8 bit grayscale bitmaps  </summary>
procedure TBitmap8_SharpenOrig(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single); overload;
procedure TBitmap8_SharpenOrig(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix); overload;

procedure TBitmap8_SharpenWithRecord(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single);

implementation

uses
  Math,
  GraphUtil,
  jpeg,
  u_dzConvertUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

// This is the original code taken from
// https://www.swissdelphicenter.ch/en/showcode.php?id=1948
// but modified for readability and 8 bit grayscale bitmaps </summary>

procedure TBitmap8_SharpenOrig(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single);
type
  PPixel = PByte;
const
  BytesPerPixel = 1;
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
  AvgBrightness: Integer;
  BytesPerLine: Integer;
begin
  // sharpening is blending of the current pixel
  // with the average of the surrounding ones,
  // but with a negative weight for the average
  Assert((_SrcBmp.Width > 2) and (_SrcBmp.Height > 2), 'Bitmap must be at least 3x3');
  Assert((_Alpha >= 1) and (_Alpha < 6), 'Alpha must be >=1 and <6');

  // since Alpha > 1 we can be sure that Beta < 1
  Beta := (_Alpha - 1) / 5;

  // integer scaled alpha and beta calculated only once
  IntBeta := Round(Beta * $10000);
  IntAlpha := Round(_Alpha * $10000);

  _SrcBmp.PixelFormat := pf8bit;
  _DstBmp.PixelFormat := pf8bit;
  _DstBmp.Palette := MakeGrayPalette;
  _DstBmp.SetSize(_SrcBmp.Width, _SrcBmp.Height);

  WorkAreaWidth := _SrcBmp.Width - 2;
  WorkAreaHeight := _SrcBmp.Height - 2;
  // There is Graphics.BytesPerScanline() which we could call instead of doing the
  // calculation here
  BytesPerLine := (((WorkAreaWidth + 2) * 8 * BytesPerPixel + 31) and not 31) div 8;

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
  // BytesPerScanline to get the next line (which would be ScanLine[1])
  // We could of course call _SrcBmp.Scanline[], but that would affect the efficiency
  // of the code due the the function call and some overhead in that code.
  SrcRows[0] := FirstSrcRow;
  SrcRows[1] := PByte(Integer(FirstSrcRow) - BytesPerLine);
  SrcRows[2] := PByte(Integer(SrcRows[1]) - BytesPerLine);
  for Row := 1 to WorkAreaHeight do begin
    Dec(DstRow, BytesPerLine);
    DstPixel := PPixel(DstRow);
    SrcPixels[0] := PPixel(Integer(SrcRows[0]) + 1 * BytesPerPixel); //top
    SrcPixels[1] := PPixel(SrcRows[1]); //left
    SrcPixels[2] := PPixel(Integer(SrcRows[1]) + 1 * BytesPerPixel); //center
    SrcPixels[3] := PPixel(Integer(SrcRows[1]) + 2 * BytesPerPixel); //right
    SrcPixels[4] := PPixel(Integer(SrcRows[2]) + 1 * BytesPerPixel); //bottom
    DstPixel^ := SrcPixels[1]^; //1st col unchanged
    for Column := 1 to WorkAreaWidth do begin
      // calculate average weighted by -beta for each color
      AvgBrightness := 0;
      for k := 0 to 4 do begin
        AvgBrightness := AvgBrightness + SrcPixels[k]^;
        Inc(SrcPixels[k]);
      end;
      AvgBrightness := (IntBeta * AvgBrightness + $7FFF) shr 16;

      // add center pixel weighted by alpha
      p := PPixel(SrcPixels[1]); // after inc, st[1] is at center
      AvgBrightness := (IntAlpha * p^ + $7FFF) shr 16 - AvgBrightness;

      // ensure range (this looks stupid, but avoids function calls)
      if AvgBrightness < 0 then
        AvgBrightness := 0
      else if AvgBrightness > 255 then
        AvgBrightness := 255;

      Inc(DstPixel);
      DstPixel^ := AvgBrightness;
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

procedure TBitmap8_SharpenWithRecord(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single);
type
  PPixel = PByte;
  TSurroundingPixels = record
    Top: PPixel;
    Left: PPixel;
    Center: PPixel;
    Right: PPixel;
    Bottom: PPixel;
  end;
const
  BytesPerPixel = 1;
  ForcedPixelFormat = pf8bit;
var
  Row, Column: Integer;
  SrcPixels: TSurroundingPixels;
  SrcRow: PByte;
  SrcPixel: PPixel;
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
  Assert((_Alpha > 1) and (_Alpha < 6), 'Alpha must be >1 and <6');

  // since Alpha > 1 we can be sure that Beta < 1
  Beta := (_Alpha - 1) / 5;

  // integer scaled alpha and beta calculated only once
  IntBeta := Round(Beta * $10000);
  IntAlpha := Round(_Alpha * $10000);

  _SrcBmp.PixelFormat := pf8bit;
  _DstBmp.PixelFormat := pf8bit;
  _DstBmp.Palette := MakeGrayPalette;
  _DstBmp.SetSize(_SrcBmp.Width, _SrcBmp.Height);

  WorkAreaWidth := _SrcBmp.Width - 2;
  WorkAreaHeight := _SrcBmp.Height - 2;
  BytesPerLine := (((WorkAreaWidth + 2) * 8 * BytesPerPixel + 31) and not 31) div 8;
//  Assert(BytesPerLine = Graphics.BytesPerScanline(WorkAreaWidth + 2, BytesPerPixel * 8, 32));

  DstRow := _DstBmp.ScanLine[0];
  DstPixel := PPixel(DstRow);

  // Copy first row unchanged
  SrcRow := _SrcBmp.ScanLine[0];
  SrcPixel := PPixel(SrcRow);
  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixel^;
    Inc(DstPixel);
    Inc(SrcPixel);
  end;

  // ScanLine[0] is the line with the highest memory address, so we decrement it by
  // BytesPerLine to get the next line (which would be ScanLine[1])
  // We could of course call _SrcBmp.Scanline[], but that would affect the efficiency
  // of the code due the the function call and some overhead in that code.
  for Row := 1 to WorkAreaHeight do begin
    Dec(SrcRow, BytesPerLine);
    SrcPixel := PPixel(SrcRow);

    Dec(DstRow, BytesPerLine);
    DstPixel := PPixel(DstRow);

    // 1st column unchanged
    DstPixel^ := SrcPixel^;

    // remember: Bitmaps are stored upside down, so for the previous line, we must add BytesPerLine
    //           and for the next line we must subtract BytesPerLine
    SrcPixels.Left := SrcPixel;
    Inc(SrcPixel);
    SrcPixels.Center := SrcPixel;
    Inc(SrcPixel);
    SrcPixels.Right := SrcPixel;
    SrcPixels.Top := PPixel(Integer(SrcPixels.Center) + BytesPerLine);
    SrcPixels.Bottom := PPixel(Integer(SrcPixels.Center) - BytesPerLine);
    for Column := 1 to WorkAreaWidth do begin
      CenterBrightness := SrcPixels.Center^;

      // calculate the average brightness weighted by -beta
      AvgBrightness := SrcPixels.Top^;
      AvgBrightness := AvgBrightness + SrcPixels.Left^;
      AvgBrightness := AvgBrightness + CenterBrightness;
      AvgBrightness := AvgBrightness + SrcPixels.Right^;
      AvgBrightness := AvgBrightness + SrcPixels.Bottom^;
      AvgBrightness := (IntBeta * AvgBrightness + $7FFF) shr 16;

      // add center pixel weighted by alpha
      AvgBrightness := (IntAlpha * CenterBrightness + $7FFF) shr 16 - AvgBrightness;

      Inc(SrcPixels.Top);
      Inc(SrcPixels.Left);
      Inc(SrcPixels.Center);
      Inc(SrcPixels.Right);
      Inc(SrcPixels.Bottom);

      // ensure range
      if AvgBrightness < 0 then
        AvgBrightness := 0
      else if AvgBrightness > 255 then
        AvgBrightness := 255;

      Inc(DstPixel);
      DstPixel^ := AvgBrightness;
    end;
    Inc(DstPixel);
    Inc(SrcPixels.Left);

    // copy Last column unchanged
    DstPixel^ := SrcPixels.Center^;
  end;
  Dec(SrcRow, BytesPerLine);

  Dec(DstRow, BytesPerLine);
  DstPixel := PPixel(DstRow);

  // copy last row unchanged
  SrcPixel := PPixel(SrcRow);

  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixel^;
    Inc(DstPixel);
    Inc(SrcPixel);
  end;
end;

procedure TBitmap8_SharpenOrig(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix);
type
  PPixel = PByte;
const
  BytesPerPixel = 1;
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
  AvgBrightness: Integer;
  BytesPerLine: Integer;
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
  _DstBmp.SetSize(_SrcBmp.Width, _SrcBmp.Height);

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
  SrcRows[1] := PByte(Integer(FirstSrcRow) - BytesPerLine);
  SrcRows[2] := PByte(Integer(SrcRows[1]) - BytesPerLine);
  for Row := 1 to WorkAreaHeight do begin
    Assert(Length(_AlphaMap[Row]) = _SrcBmp.Width,
      Format('Number of values in AlphaMap[%d] (%d) must match bitmap width (%d)',
      [Row, Length(_AlphaMap[Row]), _SrcBmp.Width]));

    Dec(DstRow, BytesPerLine);
    DstPixel := PPixel(DstRow);
    SrcPixels[0] := PPixel(Integer(SrcRows[0]) + 1 * BytesPerPixel); //top
    SrcPixels[1] := PPixel(SrcRows[1]); //left
    SrcPixels[2] := PPixel(Integer(SrcRows[1]) + 1 * BytesPerPixel); //center
    SrcPixels[3] := PPixel(Integer(SrcRows[1]) + 2 * BytesPerPixel); //right
    SrcPixels[4] := PPixel(Integer(SrcRows[2]) + 1 * BytesPerPixel); //bottom
    // copy 1st col unchanged ("[1]" is not the pixel index!)
    DstPixel^ := SrcPixels[1]^;
    for Column := 1 to WorkAreaWidth do begin
      Alpha := _AlphaMap[Row][Column];
      if not IsZero(Alpha) then begin
        Assert((Alpha > 1) and (Alpha < 6), Format('Alpha[%d][%d] must be >1 and <6', [Row, Column]));
        // since Alpha > 1 we can be sure that Beta < 1
        Beta := (Alpha - 1) / 5;
        // integer scaled alpha and beta calculated only once
        IntBeta := Round(Beta * $10000);
        IntAlpha := Round(Alpha * $10000);

        // calculate average weighted by -beta for each color
        AvgBrightness := 0;
        for k := 0 to 4 do begin
          AvgBrightness := AvgBrightness + SrcPixels[k]^;
          Inc(SrcPixels[k]);
        end;

        AvgBrightness := (IntBeta * AvgBrightness + $7FFF) shr 16;

        // add center pixel weighted by alpha
        p := PPixel(SrcPixels[1]); // after inc, SrcPixels[1] is at center
        AvgBrightness := (IntAlpha * p^ + $7FFF) shr 16 - AvgBrightness;

        // ensure range (this looks stupid, but avoids function calls)
        if AvgBrightness < 0 then
          AvgBrightness := 0
        else if AvgBrightness > 255 then
          AvgBrightness := 255;

        // write into the target pixel
        Inc(DstPixel);
        DstPixel^ := AvgBrightness;
      end else begin
        // Alpha for this pixel is 0 -> copy it unchanged (and increment all pointers for the next one)
        for k := 0 to 4 do begin
          Inc(SrcPixels[k]);
        end;
        Inc(DstPixel);
        DstPixel^ := SrcPixels[1]^;
      end;
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

end.
