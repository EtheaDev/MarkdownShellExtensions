unit u_dzProgressBarText;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Messages,
  Graphics,
  ComCtrls;

type
  ///<summary>
  /// extends TProgressBar with a text (e.g. 'xxx%') </summary>
  TProgressBarText = class(ComCtrls.TProgressBar)
  private
    FDC: THandle;
    FCanvas: TCanvas;
    FText: string;
    procedure SetText(const _Value: string);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var _Msg: TMessage); override;
    property Text: string read FText write SetText;
  end;

implementation

uses
  Controls;

constructor TProgressBarText.Create(_Owner: TComponent);
begin
  inherited;
  Parent := _Owner as TWinControl;
end;

destructor TProgressBarText.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited;
end;

procedure TProgressBarText.SetText(const _Value: string);
begin
  FText := _Value;
  Self.Invalidate;
end;

procedure TProgressBarText.WndProc(var _Msg: TMessage);
var
  XPos, YPos: Integer;
begin
  inherited;
  if _Msg.Msg = WM_NCCREATE then begin
    if not Assigned(FCanvas) then
      FCanvas := TCanvas.Create;
    FDC := GetDC(Self.Handle);
    FCanvas.Handle := FDC;
  end else if _Msg.Msg = WM_NCDESTROY then begin
    if Assigned(FCanvas) then begin
      FreeAndNil(FCanvas);
      ReleaseDC(0, FDC);
    end;
  end else if _Msg.Msg = WM_PAINT then begin
    if Assigned(FCanvas) then begin
      FCanvas.Brush.Style := bsClear;
      XPos := Self.Width div 2 - FCanvas.TextWidth(FText) div 2;
      YPos := 0;
      FCanvas.Font.Color := clBlack;
      FCanvas.Font.Style := [];
      FCanvas.TextOut(XPos, YPos, FText);
    end;
  end;
end;

end.
