unit u_dzSpeedBitBtn;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  Messages,
  Classes,
  SysUtils,
  Types,
  Buttons,
  Controls,
  Graphics,
  u_dzVclUtils;

type
  ///<summary>
  /// A helper component that turns a TBitBtn into a button that works similar to a TSpeedButton
  /// but can receive the focus. It allows to either set a caption and a Glyp and even supports
  /// word wrapping and setting a margin.
  /// Clicking the button will first set its Tag property to 0 (up) or down (1) and then call the
  /// original OnClick method.
  /// To use it create it with TdzSpeedBitBtn.Create(BitBtn) where BitBtn is an already existing
  /// TBitBtn component. TdzSpeedBitBtn will be automatically destroyed when the associated BitBtn
  /// is destroyed, so don't free it yourself.
  /// Note: Actions do not work.</summary>
  TdzSpeedBitBtn = class(TWindowProcHook)
  private
    FCaption: string;
    FOrigBmp: TBitmap;
    FOrigOnClick: TNotifyEvent;
    FUpBmp: TBitmap;
    FDownBmp: TBitmap;
    FData: Pointer;
    FNeedsNewGlyphs: Boolean;
    FOrigMargin: Integer;
    FOrigSpacing: Integer;
    procedure doOnClick(_Sender: TObject);
    procedure HandleOnClick(_Sender: TObject);
    function GetDown: Boolean;
    procedure SetDown(const Value: Boolean);
    procedure UpdateGlyph;
    function GetBitBtn: TBitBtn;
    procedure PrepareBmp(_w, _h: Integer; _Color: TColor; _Edge: UINT; out _bmp: TBitmap);
    procedure PrepareBmps;
  protected
    procedure NewWindowProc(var _Msg: TMessage); override;
  public
    constructor Create(_btn: TWinControl);
    destructor Destroy; override;
    property Down: Boolean read GetDown write SetDown;
    property BitBtn: TBitBtn read GetBitBtn;
    property Data: Pointer read FData write FData;
  end;

type
  ///<summary>
  /// Emulates a TRadioGroup, but with BitBtns
  /// Add any number of BitBtns to this object (internally it will create TdzSpeedBitBtns for these)
  /// Note: The BitBtn's OnClick event will be used internally. </summary>
  TdzSpeedBitBtnGroup = class
  private
    FOnClick: TNotifyEvent;
    FList: TList;
    FAllowAllUp: Boolean;
    procedure HandleClick(_Sender: TObject);
    procedure doOnClick;
    function TryGetSelectedSb(out _Idx: Integer; out _sb: TdzSpeedBitBtn): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(_btn: TBitBtn): TdzSpeedBitBtn; overload;
    function Add(_btn: TBitBtn; _Data: Pointer): TdzSpeedBitBtn; overload;
    function Add(_btn: TBitBtn; _Data: Integer): TdzSpeedBitBtn; overload;
    ///<summary>
    /// Sets the given button's down state to False, if allowed
    /// @param Idx is the index of the button to change
    /// @returns True, if the button could be set to Down=False, which is only possible if
    ///                * AllowAllUp is true or
    ///                * There are only two buttons, in which case the other button was set to
    ///                  Down = True
    ///          False, otherwise </summary>
    function SetUp(_Idx: Integer): Boolean; overload;
    function SetUp(_btn: TBitBtn): Boolean; overload;
    procedure SetDown(_Idx: Integer; _CallClick: Boolean = False); overload;
    procedure SetDown(_btn: TBitBtn; _CallClick: Boolean = False); overload;
    ///<summary>
    /// Note: This only works, if all Data values are different. Otherwise
    ///       all buttons matching Data will be set to down. </summary>
    procedure SetDown(_Data: Pointer; _CallClick: Boolean = False); overload;
    function isDown(_Idx: Integer): Boolean; overload;
    function isDown(_btn: TBitBtn): Boolean; overload;
    ///<summary>
    /// Note: This only works, if all Data values are different. Otherwise
    ///       all buttons matching Data will be set to down. </summary>
    function isDown(_Data: Pointer): Boolean; overload;
    function TryGetSelected(out _Idx: Integer): Boolean; overload;
    function TryGetSelected(out _btn: TBitBtn): Boolean; overload;
    function TryGetSelected(out _Data: Pointer): Boolean; overload;
    property AllowAllUp: Boolean read FAllowAllUp write FAllowAllUp;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

implementation

uses
  Math,
  Forms,
  u_dzGraphicsUtils;

{ TdzSpeedBitBtn }

constructor TdzSpeedBitBtn.Create(_btn: TWinControl);
begin
  inherited Create(_btn);
  FOrigOnClick := BitBtn.OnClick;
  FCaption := BitBtn.Caption;

  FOrigBmp := TBitmap.Create;
  FOrigBmp.Assign(BitBtn.Glyph);
  FOrigBmp.Transparent := True;

  BitBtn.Caption := '';

  FUpBmp := TBitmap.Create;
  FDownBmp := TBitmap.Create;

  PrepareBmps;

  BitBtn.OnClick := HandleOnClick;

  FOrigMargin := BitBtn.Margin;
  FOrigSpacing := BitBtn.Spacing;
  BitBtn.Margin := -1;
  BitBtn.Spacing := 0;

  UpdateGlyph;
end;

destructor TdzSpeedBitBtn.Destroy;
begin
  // If we get here, either the constructor failed (which automatically calls the destructor)
  // or BitBtn was already destroyed, so we must not access it at all.
  FUpBmp.Free;
  FDownBmp.Free;
  FOrigBmp.Free;
  inherited;
end;

procedure TdzSpeedBitBtn.PrepareBmps;
var
  w: Integer;
  h: Integer;
  ColBack1: TColor;
  ColBack2: TColor;
begin
  w := BitBtn.ClientWidth;
  h := BitBtn.ClientHeight;

  ColBack1 := RGB(240, 240, 240); // clBtnFace;
  ColBack2 := RGB(245, 245, 245); // a bit lighter than clBtnFace;

  PrepareBmp(w, h, ColBack1, EDGE_RAISED, FUpBmp);
  PrepareBmp(w, h, ColBack2, EDGE_SUNKEN, FDownBmp);
end;

procedure TdzSpeedBitBtn.PrepareBmp(_w, _h: Integer; _Color: TColor; _Edge: UINT; out _bmp: TBitmap);
var
  cnv: TCanvas;

  procedure HandleBmpOnly;
  var
    X: Integer;
    Y: Integer;
  begin
    X := FOrigMargin;
    Y := (_h - FOrigBmp.Height) div 2;
    if X = -1 then begin
      // center image in the button
      X := (_w - FOrigBmp.Width) div 2;
    end else begin
      // left align image
    end;
    cnv.Draw(X, Y, FOrigBmp);
  end;

  procedure HandleTextOnlySingleLine;
  var
    X: Integer;
    r: TRect;
    HorizontalAlignment: TDrawTextHorizontalAlignment;
  begin
    X := FOrigMargin;
    if X = -1 then begin
      HorizontalAlignment := dthaCenter;
      r := Rect(2, 0, _w - 3, _h);
    end else begin
      HorizontalAlignment := dthaLeft;
      r := Rect(X + 2, 0, _w - 3, _h);
    end;
    TCanvas_DrawTextSingleLine(cnv, FCaption, r, HorizontalAlignment, dtvaCenter, []);
  end;

  procedure HandleTextOnlyMultiLine;
  var
    X: Integer;
    r: TRect;
    TextWidth: Integer;
    TextHeight: Integer;
  begin
    X := FOrigMargin;
    if X = -1 then begin
        // center
      r := Rect(2, 0, _w - 3, _h - 4);
      TCanvas_DrawText(cnv, FCaption, r, [dtfCalcRect, dtfCenter, dtfWordBreak]);
      TextHeight := r.Bottom - r.Top;
      r.Left := 2;
      r.Top := Max(0, (_h - TextHeight) div 2);
      r.Right := _w - 3;
      r.Bottom := Min(_h - 4, r.Top + TextHeight);
      TCanvas_DrawText(cnv, FCaption, r, [dtfCenter, dtfWordBreak]);
    end else begin
      // left align the centered text
      // Yes, that doesn't make much sense, but TBitBtn works that way.
      // Actually it's even worse: TBitBtn draws the text centered on the possible button width
      // and then moves it to the right which clips the text if it is too wide.
      // We don't make that mistake here but still center the text and then move it.
      r := Rect(X + 2, 0, _w - 3, _h - 4);
      TCanvas_DrawText(cnv, FCaption, r, [dtfCalcRect, dtfCenter, dtfWordBreak]);
      TextWidth := r.Right - r.Left;
      TextHeight := r.Bottom - r.Top;
      r.Left := X + 2;
      r.Top := Max(0, (_h - TextHeight) div 2);
      r.Right := Min(_w - 3, X + 2 + TextWidth);
      r.Bottom := Min(_h - 4, r.Top + TextHeight);
      TCanvas_DrawText(cnv, FCaption, r, [dtfCenter, dtfWordBreak]);
    end;
  end;

  procedure HandleTextOnly;
  begin
{$IFDEF HAS_BITBTN_WORDWRAP}
    if BitBtn.WordWrap then begin
      HandleTextOnlyMultiLine;
    end else
{$ENDIF}begin
      HandleTextOnlySingleLine;
    end;
  end;

  procedure HandleBmpAndSingleLineText;
  var
    TextSize: TSize;
    RequiredWidth: Integer;
    r: TRect;
    X: Integer;
  begin
    TextSize := cnv.TextExtent(FCaption);
    if FOrigMargin = -1 then begin
      // center image and text on the button
      RequiredWidth := FOrigBmp.Width + FOrigSpacing + TextSize.cx;
      X := (_w - RequiredWidth) div 2;
      cnv.Draw(X, (_h - FOrigBmp.Width) div 2, FOrigBmp);
      r.Left := X + FOrigMargin + FOrigSpacing + FOrigBmp.Width;
      r.Top := (_h - TextSize.cy) div 2;
      r.Right := r.Left + TextSize.cx;
      r.Bottom := r.Top + TextSize.cy;
      TCanvas_DrawText(cnv, FCaption, r, [dtfLeft, dtfTopSingle, dtfSingleLine, dtfNoClip]);
    end else begin
      // left align image and text
      cnv.Draw(FOrigMargin, (_h - FOrigBmp.Height) div 2, FOrigBmp);
      r.Left := FOrigMargin + FOrigSpacing + FOrigBmp.Width;
      r.Top := (_h - TextSize.cy) div 2;
      r.Right := r.Left + TextSize.cx;
      r.Bottom := r.Top + TextSize.cy;
      TCanvas_DrawText(cnv, FCaption, r, [dtfLeft, dtfTopSingle, dtfSingleLine, dtfNoClip]);
    end;
  end;

  procedure HandleBmpAndMultilineText;
  var
    r: TRect;
    TextWidth: Integer;
    TextHeight: Integer;
    RequiredWidth: Integer;
    X: Integer;
  begin
    if FOrigMargin = -1 then begin
      // center image and text on the button
      r := Rect(0, 0, _w - FOrigBmp.Width - 1 - FOrigSpacing, _h - 2);
      TCanvas_DrawText(cnv, FCaption, r, [dtfCalcRect, dtfCenter, dtfWordBreak]);
      TextWidth := r.Right - r.Left;
      TextHeight := r.Bottom - r.Top;
      RequiredWidth := FOrigBmp.Width + FOrigSpacing + TextWidth;
      X := (_w - RequiredWidth) div 2;
      cnv.Draw(X, (_h - FOrigBmp.Height) div 2, FOrigBmp);

      r.Left := X + FOrigBmp.Width + FOrigSpacing;
      r.Top := (_h - TextHeight) div 2;
      r.Right := r.Left + TextWidth;
      r.Bottom := r.Top + TextHeight;
      TCanvas_DrawText(cnv, FCaption, r, [dtfCenter, dtfWordBreak]);
    end else begin
      // left align image and text
      r := Rect(0, 0, _w - FOrigMargin - FOrigBmp.Width - 1 - FOrigSpacing, _h - 2);
      TCanvas_DrawText(cnv, FCaption, r, [dtfCalcRect, dtfCenter, dtfWordBreak]);
      TextWidth := r.Right - r.Left;
      TextHeight := r.Bottom - r.Top;

      cnv.Draw(FOrigMargin, (_h - FOrigBmp.Width) div 2, FOrigBmp);

      r.Left := FOrigMargin + FOrigBmp.Width + FOrigSpacing;
      r.Top := (_h - TextWidth) div 2;
      r.Right := r.Left + TextWidth;
      r.Bottom := r.Top + TextHeight;
      TCanvas_DrawText(cnv, FCaption, r, [dtfCenter, dtfWordBreak]);
    end;
  end;

  procedure HandleBmpAndText;
  begin
      // This is complicated. For now we will only support buttons with
      // Layout=blGlyphLeft
{$IFDEF HAS_BITBTN_WORDWRAP}
    if BitBtn.WordWrap then begin
      HandleBmpAndMultilineText;
    end else
{$ENDIF}begin
      HandleBmpAndSingleLineText;
    end;
  end;

var
  r: TRect;
begin
  _bmp.Width := _w;
  _bmp.Height := _h;
  _bmp.TransparentColor := clFuchsia;

  cnv := _bmp.Canvas;

  cnv.Brush.Color := _Color;
  cnv.Brush.Style := bsSolid;
  cnv.FillRect(Rect(0, 0, _w, _h));

  r := Rect(0, 0, _w - 1, _h - 2);
  DrawEdge(cnv.Handle, r, _Edge, BF_RECT);

  cnv.Brush.Style := bsClear;
  cnv.Font := BitBtn.Font;

  if FCaption <> '' then begin
    if (FOrigBmp.Width <> 0) and (FOrigBmp.Height <> 0) then begin
      HandleBmpAndText;
    end else begin
      // text only
      HandleTextOnly;
    end;
  end else begin
    HandleBmpOnly;
  end;
end;

procedure TdzSpeedBitBtn.doOnClick(_Sender: TObject);
begin
  if Assigned(FOrigOnClick) then
    FOrigOnClick(_Sender);
end;

procedure TdzSpeedBitBtn.HandleOnClick(_Sender: TObject);
begin
  Down := not Down;
  doOnClick(_Sender);
end;

procedure TdzSpeedBitBtn.NewWindowProc(var _Msg: TMessage);
begin
  if _Msg.Msg = CM_DIALOGKEY then begin
    if IsAccel(TCMDialogChar(_Msg).CharCode, FCaption) and BitBtn.CanFocus then begin
      BitBtn.Click;
      _Msg.Result := 1;
    end else
      inherited;
  end else if (_Msg.Msg = CM_FONTCHANGED)
{$IF Declared(WM_DPICHANGED_AFTERPARENT)}
  or (_Msg.Msg = WM_DPICHANGED_AFTERPARENT)
{$IFEND}
  or (_Msg.Msg = WM_SIZE) then begin
    FNeedsNewGlyphs := True;
    inherited;
  end else if _Msg.Msg = WM_PAINT then begin
    if FNeedsNewGlyphs then begin
      PrepareBmps;
      UpdateGlyph;
      FNeedsNewGlyphs := False;
    end;
    inherited;
  end else
    inherited;
end;

function TdzSpeedBitBtn.GetBitBtn: TBitBtn;
begin
  Result := Self.FCtrl as TBitBtn;
end;

function TdzSpeedBitBtn.GetDown: Boolean;
begin
  Result := (BitBtn.Tag <> 0);
end;

procedure TdzSpeedBitBtn.SetDown(const Value: Boolean);
begin
  if Value then
    BitBtn.Tag := 1
  else
    BitBtn.Tag := 0;
  UpdateGlyph;
end;

procedure TdzSpeedBitBtn.UpdateGlyph;
begin
  if BitBtn.Tag <> 0 then
    BitBtn.Glyph := FDownBmp
  else
    BitBtn.Glyph := FUpBmp;

  // Setting Glyph may change the NumGlyph property (if the Width to Height ration of the bitmap
  // is 4, 3 or 2 to 1). We don't want that, so we change it back. (Bloody computer trying to
  // be clever :-(.)
  BitBtn.NumGlyphs := 1;
end;

{ TdzSpeedBitBtnGroup }

constructor TdzSpeedBitBtnGroup.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TdzSpeedBitBtnGroup.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TdzSpeedBitBtnGroup.Add(_btn: TBitBtn; _Data: Pointer): TdzSpeedBitBtn;
begin
  _btn.OnClick := Self.HandleClick;
  Result := TdzSpeedBitBtn.Create(_btn);
  Result.Data := _Data;
  FList.Add(Result);
end;

function TdzSpeedBitBtnGroup.Add(_btn: TBitBtn): TdzSpeedBitBtn;
begin
  Result := Add(_btn, nil);
end;

function TdzSpeedBitBtnGroup.Add(_btn: TBitBtn; _Data: Integer): TdzSpeedBitBtn;
begin
  Result := Add(_btn, Pointer(_Data)); //FI:W541 Casting from Integer to Pointer type (or vice versa)
end;

procedure TdzSpeedBitBtnGroup.doOnClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TdzSpeedBitBtnGroup.HandleClick(_Sender: TObject);
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  for i := 0 to FList.Count - 1 do begin
    sb := TdzSpeedBitBtn(FList[i]);
    if sb.BitBtn = _Sender then begin
      if FAllowAllUp then begin
        // the button has already been marked as down, so the logic here is inverted
        if sb.Down then
          SetDown(i)
        else
          SetUp(i);
      end else
        SetDown(i);
      Break; //==>
    end;
  end;
  doOnClick;
end;

function TdzSpeedBitBtnGroup.isDown(_Idx: Integer): Boolean;
var
  sb: TdzSpeedBitBtn;
begin
  sb := TdzSpeedBitBtn(FList[_Idx]);
  Result := sb.Down;
end;

function TdzSpeedBitBtnGroup.isDown(_btn: TBitBtn): Boolean;
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  for i := 0 to FList.Count - 1 do begin
    sb := TdzSpeedBitBtn(FList[i]);
    if sb.BitBtn = _btn then begin
      Result := sb.Down;
      Exit; //==>
    end;
  end;
  Result := False;
end;

function TdzSpeedBitBtnGroup.isDown(_Data: Pointer): Boolean;
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  for i := 0 to FList.Count - 1 do begin
    sb := TdzSpeedBitBtn(FList[i]);
    if sb.Data = _Data then begin
      Result := sb.Down;
      Exit; //==>
    end;
  end;
  Result := False;
end;

procedure TdzSpeedBitBtnGroup.SetDown(_Idx: Integer; _CallClick: Boolean = False);
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  for i := 0 to FList.Count - 1 do begin
    sb := TdzSpeedBitBtn(FList[i]);
    if i = _Idx then
      sb.Down := True
    else
      sb.Down := False;
  end;
  if _CallClick then
    doOnClick;
end;

procedure TdzSpeedBitBtnGroup.SetDown(_btn: TBitBtn; _CallClick: Boolean = False);
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  for i := 0 to FList.Count - 1 do begin
    sb := TdzSpeedBitBtn(FList[i]);
    if sb.BitBtn = _btn then
      sb.Down := True
    else
      sb.Down := False;
  end;
  if _CallClick then
    doOnClick;
end;

procedure TdzSpeedBitBtnGroup.SetDown(_Data: Pointer; _CallClick: Boolean = False);
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  for i := 0 to FList.Count - 1 do begin
    sb := TdzSpeedBitBtn(FList[i]);
    if sb.Data = _Data then
      sb.Down := True
    else
      sb.Down := False;
  end;
  if _CallClick then
    doOnClick;
end;

function TdzSpeedBitBtnGroup.SetUp(_btn: TBitBtn): Boolean;
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  if FAllowAllUp then begin
    for i := 0 to FList.Count - 1 do begin
      sb := TdzSpeedBitBtn(FList[i]);
      if sb.BitBtn = _btn then
        sb.Down := False;
    end;
    Result := True;
  end else if FList.Count = 2 then begin
    for i := 0 to FList.Count - 1 do begin
      sb := TdzSpeedBitBtn(FList[i]);
      if sb.BitBtn = _btn then
        sb.Down := False
      else
        sb.Down := True;
    end;
    Result := True;
  end else
    Result := False;
end;

function TdzSpeedBitBtnGroup.SetUp(_Idx: Integer): Boolean;
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  if FAllowAllUp then begin
    for i := 0 to FList.Count - 1 do begin
      sb := TdzSpeedBitBtn(FList[i]);
      if i = _Idx then
        sb.Down := False;
    end;
    Result := True;
  end else if FList.Count = 2 then begin
    for i := 0 to FList.Count - 1 do begin
      sb := TdzSpeedBitBtn(FList[i]);
      if i = _Idx then
        sb.Down := False
      else
        sb.Down := True;
    end;
    Result := True;
  end else
    Result := False;
end;

function TdzSpeedBitBtnGroup.TryGetSelectedSb(out _Idx: Integer; out _sb: TdzSpeedBitBtn): Boolean;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    _sb := TdzSpeedBitBtn(FList[i]);
    if _sb.Down then begin
      Result := True;
      _Idx := i;
      Exit; //==>
    end;
  end;
  Result := False;
end;

function TdzSpeedBitBtnGroup.TryGetSelected(out _btn: TBitBtn): Boolean;
var
  Idx: Integer;
  sb: TdzSpeedBitBtn;
begin
  Result := TryGetSelectedSb(Idx, sb);
  if Result then
    _btn := sb.BitBtn;
end;

function TdzSpeedBitBtnGroup.TryGetSelected(out _Data: Pointer): Boolean;
var
  Idx: Integer;
  sb: TdzSpeedBitBtn;
begin
  Result := TryGetSelectedSb(Idx, sb);
  if Result then
    _Data := sb.Data;
end;

function TdzSpeedBitBtnGroup.TryGetSelected(out _Idx: Integer): Boolean;
var
  sb: TdzSpeedBitBtn;
begin
  Result := TryGetSelectedSb(_Idx, sb);
end;

end.
