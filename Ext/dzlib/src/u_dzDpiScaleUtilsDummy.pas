unit u_dzDpiScaleUtilsDummy;

{$I 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes,
  Types,
  Controls,
  Forms;

const
  USER_DEFAULT_SCREEN_DPI = 96;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
type
  TDpiScaler = record
  private
    FDesignDpi: Integer;
    FCurrentDpi: Integer;
  public
    procedure Init(_frm: TCustomForm); overload;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure Init(_Dpi: Integer); overload;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure Init(_DesignDpi, _CurrentDpi: Integer); overload;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetCurrentDpi(_frm: TCustomForm); overload;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure SetCurrentDpi(_Dpi: Integer); overload;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function Calc(_Value: Integer): Integer; overload;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function Calc(const _Value: TRect): TRect; overload;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function ScaleFactorPercent: Integer;
  end;
{$ENDIF}

type
  PScaledImagesRec = ^TScaledImagesRec;
  TScaledImagesRec = record
    FDpi: Integer;
    FImages: TImageList;
  end;

type
  TImageListScaler = class(TComponent)
  private
    FOriginal: TImageList;
  public
    constructor Create(_Owner: TComponent; _Original: TImageList); reintroduce;
    function GetScaledList(_Dpi: Integer): TImageList;
  end;

type
  TFormDpiScaler = class
  public
    constructor Create(_Form: TForm);
    function Calc(_Value: Integer): Integer;
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
  end;

implementation

{ TFormDpiScaler }

constructor TFormDpiScaler.Create(_Form: TForm);
begin
  inherited Create;
end;

procedure TFormDpiScaler.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
begin
  // do nothing
end;

function TFormDpiScaler.Calc(_Value: Integer): Integer;
begin
  Result := _Value;
end;

{ TImageListScaler }

constructor TImageListScaler.Create(_Owner: TComponent; _Original: TImageList);
begin
  inherited Create(_Owner);
  FOriginal := _Original;
end;

function TImageListScaler.GetScaledList(_Dpi: Integer): TImageList;
begin
  Result := FOriginal;
end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
{ TDpiScaler }

function TDpiScaler.Calc(_Value: Integer): Integer;
begin
  Result := _Value;
end;

function TDpiScaler.Calc(const _Value: TRect): TRect;
begin
  Result := _Value;
end;

procedure TDpiScaler.Init(_frm: TCustomForm);
begin
  FDesignDpi := 96;
  FCurrentDpi := 96;
end;

procedure TDpiScaler.Init(_DesignDpi, _CurrentDpi: Integer);
begin
  FDesignDpi := _DesignDpi;
  FCurrentDpi := _CurrentDpi;
end;

procedure TDpiScaler.Init(_Dpi: Integer);
begin
  FCurrentDpi := _Dpi;
  FDesignDpi := _Dpi;
end;

function TDpiScaler.ScaleFactorPercent: Integer;
begin
  Result := 100;
end;

procedure TDpiScaler.SetCurrentDpi(_Dpi: Integer);
begin
  FCurrentDpi := _Dpi;
end;

procedure TDpiScaler.SetCurrentDpi(_frm: TCustomForm);
begin
  FCurrentDpi := 96;
end;
{$ENDIF}

end.

