///<summary> Firemonkey utils (some equivalents of u_dzVclUtils) </summary>
unit u_dzFmxUtils;

interface

uses
  FMX.Forms,
  System.Classes;

type
  TdzWindowPositions = (dwpTop, dwpBottom, dwpLeft, dwpRight, dwpTopLeft, dwpTopRight, dwpBottomLeft, dwpBottomRight);

procedure TForm_MoveTo(_frm: TCustomForm; _Position: TdzWindowPositions);

function TForm_ActivatePositioning(_Form: TForm; _Modifier: TShiftState = [ssCtrl, ssAlt]): TObject;

implementation

uses
  System.Types,
  FMX.Types,
  FMX.Controls,
  System.UITypes;

procedure TForm_MoveTo(_frm: TCustomForm; _Position: TdzWindowPositions);

  procedure ToTop(var _Re: TRect; _MinHeight, _MaxHeight: Integer);
  begin
    _Re.Bottom := _Re.Top + _Re.Height div 2;
    if _Re.Height < _MinHeight then
      _Re.Bottom := _Re.Top + _MinHeight;
    if (_MaxHeight > 0) and (_Re.Height > _MaxHeight) then
      _Re.Bottom := _Re.Top + _MaxHeight;
  end;

  procedure ToBottom(var _Re: TRect; _MinHeight, _MaxHeight: Integer);
  begin
    _Re.Top := _Re.Top + _Re.Height div 2;
    if _Re.Height < _MinHeight then
      _Re.Top := _Re.Bottom - _MinHeight;
    if (_MaxHeight > 0) and (_Re.Height > _MaxHeight) then
      _Re.Top := _Re.Bottom - _MaxHeight;
  end;

  procedure ToLeft(var _Re: TRect; _MinWidth, _MaxWidth: Integer);
  begin
    _Re.Right := _Re.Left + _Re.Width div 2;
    if _Re.Width < _MinWidth then
      _Re.Right := _Re.Left + _MinWidth;
    if (_MaxWidth > 0) and (_Re.Width > _MaxWidth) then
      _Re.Right := _Re.Left + _MaxWidth;
  end;

  procedure ToRight(var _Re: TRect; _MinWidth, _MaxWidth: Integer);
  begin
    _Re.Left := _Re.Left + _Re.Width div 2;
    if _Re.Width < _MinWidth then
      _Re.Left := _Re.Right - _MinWidth;
    if (_MaxWidth > 0) and (_Re.Width > _MaxWidth) then
      _Re.Left := _Re.Right - _MaxWidth;
  end;

  function TryMonitorFromPoint(_pnt: TPoint; out _Display: TDisplay): boolean;
  var
    i: Integer;
    Display: TDisplay;
  begin
    Result := False;
    for i := 0 to Screen.DisplayCount - 1 do begin
      Display := Screen.Displays[i];
      Result := Display.WorkArea.Contains(_pnt);
      if Result then begin
        _Display := Display;
        Exit;
      end;
    end;
  end;

type
  TDummyConstraints = record
    MinWidth, MaxWidth: Integer;
    MinHeight, MaxHeight: Integer;
  end;
var
  re: TRect;
  Bounds: TRect;
  NewMonitor: TDisplay;
  Constraints: TDummyConstraints;
begin
  re := Screen.DisplayFromForm(_frm).WorkareaRect;
  Bounds := _frm.Bounds;
  Constraints.MinWidth := 0;
  Constraints.MaxWidth := 0;
  Constraints.MinHeight := 0;
  Constraints.MaxHeight := 0;
  case _Position of
    dwpTop: begin
        ToTop(re, Constraints.MinHeight, Constraints.MaxHeight);
        if re = Bounds then begin
          if TryMonitorFromPoint(Point((re.Left + re.Right) div 2, re.Top - re.Height div 2), NewMonitor) then begin
            re := NewMonitor.WorkareaRect;
            ToBottom(re, Constraints.MinHeight, Constraints.MaxHeight);
          end;
        end;
      end;
    dwpBottom: begin
        ToBottom(re, Constraints.MinHeight, Constraints.MaxHeight);
        if re = Bounds then begin
          if TryMonitorFromPoint(Point((re.Left + re.Right) div 2, re.Bottom + re.Height div 2), NewMonitor) then begin
            re := NewMonitor.WorkareaRect;
            ToTop(re, Constraints.MinHeight, Constraints.MaxHeight);
          end;
        end;
      end;
    dwpLeft: begin
        ToLeft(re, Constraints.MinWidth, Constraints.MaxWidth);
        if re = Bounds then begin
          if TryMonitorFromPoint(Point(re.Left - re.Width div 2, (re.Top + re.Bottom) div 2), NewMonitor) then begin
            re := NewMonitor.WorkareaRect;
            ToRight(re, Constraints.MinWidth, Constraints.MaxWidth);
          end;
        end;
      end;
    dwpRight: begin
        ToRight(re, Constraints.MinWidth, Constraints.MaxWidth);
        if re = Bounds then begin
          if TryMonitorFromPoint(Point(re.Right + re.Width div 2, (re.Top + re.Bottom) div 2), NewMonitor) then begin
            re := NewMonitor.WorkareaRect;
            ToLeft(re, Constraints.MinWidth, Constraints.MaxWidth);
          end;
        end;
      end;
    dwpTopLeft: begin
        ToTop(re, Constraints.MinHeight, Constraints.MaxHeight);
        ToLeft(re, Constraints.MinWidth, Constraints.MaxWidth);
      end;
    dwpTopRight: begin
        ToTop(re, Constraints.MinHeight, Constraints.MaxHeight);
        ToRight(re, Constraints.MinWidth, Constraints.MaxWidth);
      end;
    dwpBottomLeft: begin
        ToBottom(re, Constraints.MinHeight, Constraints.MaxHeight);
        ToLeft(re, Constraints.MinWidth, Constraints.MaxWidth);
      end;
    dwpBottomRight: begin
        ToBottom(re, Constraints.MinHeight, Constraints.MaxHeight);
        ToRight(re, Constraints.MinWidth, Constraints.MaxWidth);
      end;
  end;
  _frm.Bounds := re;
end;

type
  TFormPositioningActivator = class(TControl)
  private
    FModifier: TShiftState;
  protected
    procedure DialogKey(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(_Form: TCustomForm; _Modifier: TShiftState); reintroduce;
  end;

{ TFormHookChild }

constructor TFormPositioningActivator.Create(_Form: TCustomForm; _Modifier: TShiftState);
begin
  inherited Create(_Form);
  FModifier := _Modifier;
  Parent := _Form;
end;

procedure TFormPositioningActivator.DialogKey(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Shift = FModifier then begin
    case Key of
      vkLeft: TForm_MoveTo(self.Parent as TForm, dwpLeft);
      vkRight: TForm_MoveTo(self.Parent as TForm, dwpRight);
      vkUp: TForm_MoveTo(self.Parent as TForm, dwpTop);
      vkDown: TForm_MoveTo(self.Parent as TForm, dwpBottom);
      vkHome: TForm_MoveTo(self.Parent as TForm, dwpTopLeft);
      vkEnd: TForm_MoveTo(self.Parent as TForm, dwpBottomLeft);
      vkPrior: TForm_MoveTo(self.Parent as TForm, dwpTopRight);
      vkNext: TForm_MoveTo(self.Parent as TForm, dwpBottomRight);
    else
      Exit; // so Key doesn't get set to 0
    end;
    Key := 0;
  end;

end;

function TForm_ActivatePositioning(_Form: TForm; _Modifier: TShiftState = [ssCtrl, ssAlt]): TObject;
begin
  Result := TFormPositioningActivator.Create(_Form, _Modifier);
end;

end.

