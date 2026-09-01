// -------------------------------------------------------------------------------------------------
// This file contains class for HTML to PDF conversion using great components HTMLVIEW and SynPdf.
// Thanks for authors of these components: HTMLVIEW - L. David Baldwin and SynPdf - Arnaud Bouchez.
// The orignal class was written by Pawel Stroinski on 2010/07/07 and is public domain.
// Revisioned by Muzio Valerio on 2019/07/01 and is public domain.
// -------------------------------------------------------------------------------------------------

// -------------------------------------------------------------------------------------------------
// New implementation 2021/04/24
// TvmHtmlToPdfGDI
// a class derived from TPdfDocumentGDI
// is a public domain.
// -------------------------------------------------------------------------------------------------
{$WARN IMPLICIT_STRING_CAST OFF}
unit vmHtmlToPdf;

interface

uses
  System.Classes,System.SysUtils, Winapi.Windows, Vcl.Printers, Vcl.Forms, Vcl.Graphics, SynPdf, HTMLView;

const
  CPointsPerInch = 72;
  CCentimetersPerInch = 2.54;

type
  TPositionPrint = (ppTop, ppBottom);

  //NB: the non-GDI variant TvmHtmlToPdf (based on TPdfDocument) used to be
  //declared here. It was never instantiated in either project - both use
  //TvmHtmlToPdfGDI - and it was a ~95% copy of it, with the additional defect
  //that its Execute had no try/finally: an exception inside the loop leaked
  //the TList and every TMetafile in it.

  TvmHtmlToPdfGDI = class(TPdfDocumentGDI)
  strict private
    // PDF Parameters
    FPrintOrientation: TPrinterOrientation;
    FPDFMarginTop: Double;
    FPDFMarginLeft: Double;
    FPDFMarginRight: Double;
    FPDFMarginBottom: Double;
    FScaleToFit: Boolean;
    // ----------------

    // Source Viewer HtmlViewer
    FSrcViewer: THtmlViewer;
    // ------------------------

    // Optional parameters for print Page Number
    FPrintPageNumber: Boolean;
    FTextPageNumber: string;
    FPageNumberPositionPrint: TPositionPrint;
    // -----------------------------------------

    // Utility function
    function Centimeters2Points(const aCentimeters: Double): Single;
    function DefinePointsWidth: Single;
    function Points2Pixels(const aPoints: Single): Integer;
    function DefinePointsHeight: Single;
    // -----------------
  private
    function GetPrintOrientation: TPrinterOrientation;
    procedure SetDefaultPaperSize(const Value: TPDFPaperSize);
    procedure SetPageNumberPostionPrint(const Value: TPositionPrint);
    procedure SetPDFMarginBottom(const Value: Double);
    procedure SetPDFMarginLeft(const Value: Double);
    procedure SetPDFMarginRight(const Value: Double);
    procedure SetPDFMarginTop(const Value: Double);
    procedure SetPrintOrientation(const Value: TPrinterOrientation);
    procedure SetPrintPageNumber(const Value: Boolean);
    procedure SetScaleToFit(const Value: Boolean);
    procedure SetSrcViewer(const Value: THTMLViewer);
    procedure SetTextPageNumber(const Value: string);
  public
    property PDFMarginTop: Double write SetPDFMarginTop;
    property PDFMarginLeft: Double write SetPDFMarginLeft;
    property PDFMarginRight: Double write SetPDFMarginRight;
    property PDFMarginBottom: Double write SetPDFMarginBottom;
    property PDFScaleToFit: Boolean write SetScaleToFit;
    property PrintOrientation: TPrinterOrientation read GetPrintOrientation write SetPrintOrientation;
    property DefaultPaperSize: TPDFPaperSize write SetDefaultPaperSize;

    property SrcViewer: THTMLViewer write SetSrcViewer;

    property PrintPageNumber: Boolean write SetPrintPageNumber;
    property TextPageNumber: string write SetTextPageNumber;
    property PageNumberPositionPrint: TPositionPrint write SetPageNumberPostionPrint;

    procedure Execute;
  end;


implementation


{ TvmHtmlToPdfGDI }

function TvmHtmlToPdfGDI.Centimeters2Points(const aCentimeters: Double): Single;
begin
  Result := ACentimeters / CCentimetersPerInch * CPointsPerInch;
end;

function TvmHtmlToPdfGDI.DefinePointsHeight: Single;
begin
  Result := (DefaultPageHeight - Centimeters2Points(FPDFMarginTop + FPDFMarginBottom));
end;

function TvmHtmlToPdfGDI.DefinePointsWidth: Single;
begin
  Result := DefaultPageWidth - Centimeters2Points(FPDFMarginLeft + FPDFMarginRight);
end;

procedure TvmHtmlToPdfGDI.Execute;
var
  lFormatWidth, lWidth, lHeight, I: Integer;
  lMarginL, lMarginR, lPointsWidth,  lPointsHeight, lMarginTop: Single;
  lPages: TList;
  lMFPage: TMetafile;
  lScale: Single;
  lPageText: string;
begin
  try
    lPointsWidth := DefinePointsWidth;
    lFormatWidth := Points2Pixels(lPointsWidth);
    lWidth := FSrcViewer.FullDisplaySize(lFormatWidth).cx;

    lScale := 1;
    if FScaleToFit and (lWidth > lFormatWidth) and (lFormatWidth > 0) then
      lScale := lFormatWidth / lWidth;
    lPointsHeight := DefinePointsHeight / lScale;
    lHeight := Points2Pixels(lPointsHeight);

    lPages := FSrcViewer.MakePagedMetaFiles(lWidth, lHeight);

    lMarginL := Centimeters2Points(FPDFMarginLeft);
    lMarginR := Centimeters2Points(FPDFMarginRight);
    lMarginTop := Centimeters2Points(FPDFMarginTop);

    ScreenLogPixels := Screen.PixelsPerInch;
    for I := 0 to LPages.Count - 1 do
    begin
      lMFPage := TMetafile(lPages[I]);
      Self.AddPage;

      VCLCanvas.Draw(Trunc(lMarginL), Trunc(lMarginTop), LMFPage);

      if FPrintPageNumber then
      begin
        if FTextPageNumber = '' then
          lPageText := 'Page %d/%d'
        else
          lPageText := FTextPageNumber;
        lPageText := Format(lPageText, [I + 1, LPages.Count]);
        VCLCanvas.Font.Name := 'Segoe UI';
        VCLCanvas.Font.Size := 10;
        VCLCanvas.Font.Color := clRed;
        case FPageNumberPositionPrint of
          ppTop:
          begin
            VCLCanvas.TextOut(Trunc(lMarginR + (lPointsWidth - VCLCanvas.TextWidth(PDFString(lPageText)))/2),
              Trunc(0 + VCLCanvas.Font.Size), PDFString(lPageText));
          end;
          ppBottom:
          begin
            VCLCanvas.TextOut(Trunc(lMarginR + (lPointsWidth - VCLCanvas.TextWidth(PDFString(lPageText)))/2),
              lHeight + (VCLCanvas.Font.Size * 2), PDFString(lPageText));
          end;
        end;
      end;
    end;
    FreeAndNil(LMFPage);
  finally
    FreeAndNil(lPages);
  end;
end;

function TvmHtmlToPdfGDI.GetPrintOrientation: TPrinterOrientation;
begin
  Result := FPrintOrientation;
end;

function TvmHtmlToPdfGDI.Points2Pixels(const aPoints: Single): Integer;
begin
  Result := Round(aPoints / CPointsPerInch * Screen.PixelsPerInch);
end;

procedure TvmHtmlToPdfGDI.SetDefaultPaperSize(const Value: TPDFPaperSize);
var
  lPrintOrientation: TPrinterOrientation;
begin
  lPrintOrientation := FPrintOrientation;
  FDefaultPaperSize := Value;
  inherited;
  FPrintOrientation := lPrintOrientation;
end;

procedure TvmHtmlToPdfGDI.SetPageNumberPostionPrint(const Value: TPositionPrint);
begin
  FPageNumberPositionPrint := Value;
end;

procedure TvmHtmlToPdfGDI.SetPDFMarginBottom(const Value: Double);
begin
  FPDFMarginBottom := Value;
end;

procedure TvmHtmlToPdfGDI.SetPDFMarginLeft(const Value: Double);
begin
  FPDFMarginLeft := Value;
end;

procedure TvmHtmlToPdfGDI.SetPDFMarginRight(const Value: Double);
begin
  FPDFMarginRight := Value;
end;

procedure TvmHtmlToPdfGDI.SetPDFMarginTop(const Value: Double);
begin
  FPDFMarginTop := Value;
end;

procedure TvmHtmlToPdfGDI.SetPrintOrientation(const Value: TPrinterOrientation);
begin
  FPrintOrientation := Value;
end;

procedure TvmHtmlToPdfGDI.SetPrintPageNumber(const Value: Boolean);
begin
  FPrintPageNumber := Value;
end;

procedure TvmHtmlToPdfGDI.SetScaleToFit(const Value: Boolean);
begin
  FScaleToFit := Value;
end;

procedure TvmHtmlToPdfGDI.SetSrcViewer(const Value: THTMLViewer);
begin
  FSrcViewer := Value;
end;

procedure TvmHtmlToPdfGDI.SetTextPageNumber(const Value: string);
begin
  FTextPageNumber := Value;
end;

end.

