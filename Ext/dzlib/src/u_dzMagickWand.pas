unit u_dzMagickWand;

{.$DEFINE SUPPORT_GRAPHICS32}

interface

uses
  SysUtils,
  Classes,
  Graphics,
  u_dzTypes,
  u_dzTranslator,
  u_dzDllLoader,
{$IFDEF SUPPORT_GRAPHICS32}
  GR32,
{$ENDIF}
  ImageMagick,
  magick_wand,
  ctypes;

type
  EMagickWand = class(EdzException)

  end;

type
  DistortMethod = (
    UndefinedDistortion,
    AffineDistortion, AffineProjectionDistortion, ScaleRotateTranslateDistortion,
    PerspectiveDistortion, PerspectiveProjectionDistortion,
    BilinearForwardDistortion,
    BilinearDistortion = BilinearForwardDistortion,
    BilinearReverseDistortion, PolynomialDistortion, ArcDistortion, PolarDistortion,
    DePolarDistortion, Cylinder2PlaneDistortion, Plane2CylinderDistortion, BarrelDistortion,
    BarrelInverseDistortion, ShepardsDistortion, ResizeDistortion, SentinelDistortion);

type
  TMagickWandDll = class(TdzDllLoader)
  private
    type
      TMagickWandGenesis = procedure; cdecl;
      TMagickWandTerminus = procedure; cdecl;
      TMagickGetException = function(wand: PMagickWand; severity: PExceptionType): PAnsiChar; cdecl;
      TNewMagickWand = function: PMagickWand; cdecl;
      TDestroyMagickWand = function(wand: PMagickWand): PMagickWand; cdecl;
      TMagickRelinquishMemory = function(resource: Pointer): Pointer; cdecl;
      TMagickResetIterator = procedure(wand: PMagickWand); cdecl;
      TMagickNextImage = function(wand: PMagickWand): MagickBooleanType; cdecl;
      TMagickReadImage = function(wand: PMagickWand; const filename: PAnsiChar): MagickBooleanType; cdecl;
      TMagickResizeImage = function(wand: PMagickWand;
        const columns, rows: culong; const filter: FilterTypes;
        const blur: Double): MagickBooleanType; cdecl;
      TMagickWriteImage = function(wand: PMagickWand;
        const filename: PAnsiChar): MagickBooleanType; cdecl;
      TMagickWriteImages = function(wand: PMagickWand;
        const filename: PAnsiChar; const adjoin: MagickBooleanType): MagickBooleanType; cdecl;
      TMagickCompositeImage = function(wand: PMagickWand;
        const composite_wand: PMagickWand; const compose: CompositeOperator;
        const x, y: clong): MagickBooleanType; cdecl;
      TMagickDistortImage = function(wand: PMagickWand; Method: DistortMethod; NumOfArgs: Integer;
        arguments: PDouble; bestfit: MagickBooleanType): MagickBooleanType; cdecl;
      TMagickDrawImage = function(wand: PMagickWand;
        const drawing_wand: PDrawingWand): MagickBooleanType; cdecl;
      TMagickGetImage = function(wand: PMagickWand): PMagickWand; cdecl;
      TMagickGetImageBlob = function(wand: PMagickWand; length: Pcsize_t): PByte; cdecl;
      TMagickGetImageWidth = function(wand: PMagickWand): culong; cdecl;
      TMagickGetImageRegion = function(wand: PMagickWand; const width, height: culong;
        const x, y: clong): PMagickWand; cdecl;
      TMagickGetImageHeight = function(wand: PMagickWand): culong; cdecl;
      TMagickReadImageBlob = function(wand: PMagickWand;
        const blob: Pointer; const length: clong): MagickBooleanType; cdecl;
      TMagickSetImageFormat = function(wand: PMagickWand; const format: PAnsiChar): MagickBooleanType; cdecl;
      TMagickSetImageProperty = function(wand: PMagickWand;
        Key: PAnsiChar; Value: PAnsiChar): MagickBooleanType; cdecl;

      TNewDrawingWand = function: PDrawingWand; cdecl;
      TDestroyDrawingWand = function(wand: PDrawingWand): PDrawingWand; cdecl;
      TDrawGetException = function(const wand: PDrawingWand; severity: PExceptionType): PAnsiChar; cdecl;
      TDrawPolygon = procedure(wand: PDrawingWand; const number_coordinates: culong;
        const coordinates: PPointInfo); cdecl;
      TDrawSetFillColor = procedure(wand: PDrawingWand; const fill_wand: PPixelWand); cdecl;
      TDrawSetStrokeColor = procedure(wand: PDrawingWand; const stroke_wand: PPixelWand); cdecl;

      TNewPixelWand = function: PPixelWand; cdecl;
      TDestroyPixelWand = function(wand: PPixelWand): PPixelWand; cdecl;
      TPixelSetColor = function(wand: PPixelWand; const color: PAnsiChar): MagickBooleanType; cdecl;
  public
    MagickWandGenesis: TMagickWandGenesis;
    MagickWandTerminus: TMagickWandTerminus;
    MagickGetException: TMagickGetException;
    NewMagickWand: TNewMagickWand;
    DestroyMagickWand: TDestroyMagickWand;
    MagickRelinquishMemory: TMagickRelinquishMemory;
    MagickResetIterator: TMagickResetIterator;
    MagickNextImage: TMagickNextImage;
    MagickReadImage: TMagickReadImage;
    MagickResizeImage: TMagickResizeImage;
    MagickWriteImage: TMagickWriteImage;
    MagickWriteImages: TMagickWriteImages;
    MagickCompositeImage: TMagickCompositeImage;
    MagickDistortImage: TMagickDistortImage;
    MagickDrawImage: TMagickDrawImage;
    MagickGetImage: TMagickGetImage;
    MagickGetImageBlob: TMagickGetImageBlob;
    MagickGetImageHeight: TMagickGetImageHeight;
    MagickGetImageRegion: TMagickGetImageRegion;
    MagickGetImageWidth: TMagickGetImageWidth;
    MagickReadImageBlob: TMagickReadImageBlob;
    MagickSetImageFormat: TMagickSetImageFormat;
    MagickSetImageProperty: TMagickSetImageProperty;

    NewDrawingWand: TNewDrawingWand;
    DestroyDrawingWand: TDestroyDrawingWand;
    DrawGetException: TDrawGetException;
    DrawPolygon: TDrawPolygon;
    DrawSetFillColor: TDrawSetFillColor;
    DrawSetStrokeColor: TDrawSetStrokeColor;

    NewPixelWand: TNewPixelWand;
    DestroyPixelWand: TDestroyPixelWand;
    PixelSetColor: TPixelSetColor;
  protected
    procedure InitEntryPoints; override;
  public
    constructor Create;
  end;

type
  TWandClass = class
  protected
    FDll: TMagickWandDll;
    procedure ThrowWandException; virtual; abstract;
  public
    constructor Create(_Dll: TMagickWandDll);
  end;

type
  TPixelWandClass = class(TWandClass)
  private
    FWand: PPixelWand;
  protected
    procedure ThrowWandException; override;
  public
    constructor Create(_Dll: TMagickWandDll);
    destructor Destroy; override;
    procedure SetColor(const _Color: string);
  end;

type
  TPointInfoArr = array of PointInfo;

type
  TDrawingWandClass = class(TWandClass)
  private
    FWand: PDrawingWand;
  protected
    procedure ThrowWandException; override;
  public
    constructor Create(_Dll: TMagickWandDll);
    destructor Destroy; override;
    procedure DrawPolygon(const _Coords: TPointInfoArr);

    procedure SetFillColor(_Fill: TPixelWandClass);
    procedure SetStrokeColor(_Stroke: TPixelWandClass);
  end;

type
  TdzMagickBlob = class(TCustomMemoryStream)
  private
    FDll: TMagickWandDll;
  public
    constructor Create(_Dll: TMagickWandDll; _Memory: Pointer; _Size: Cardinal);
    destructor Destroy; override;
    ///<summary>
    /// This function is not implemented! </summary>
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

type
  TMagickWandClass = class(TWandClass)
  private
    FWand: PMagickWand;
  protected
    procedure ThrowWandException; override;
  public
    constructor Create(_Dll: TMagickWandDll); overload;
    constructor Create(_Dll: TMagickWandDll; _Wand: PMagickWand); overload;
    destructor Destroy; override;
    procedure DistortImage(_Method: DistortMethod; const _Arguments: TDoubleArray; _BestFit: Boolean); overload;
    ///<summary>
    /// Not all distort methods can be called with this overload </summary>
    procedure DistortImage(_Method: DistortMethod; const _Points: TPointInfoArr; _BestFit: Boolean); overload;
    procedure DrawImage(_Drawing: TDrawingWandClass);

    ///<summary>
    /// returns a copy of the image </summary>
    function GetImage: TMagickWandClass;
    function GetImageBlob: TdzMagickBlob;
    function GetImageRegion(const _Width, _Height: culong; const _X, _Y: clong): TMagickWandClass;
    procedure ReadImageBlob(_Blob: TMemoryStream);
    procedure SetImageFormat(const _ImageFormat: string);
    procedure SetImageProperty(const _Key, _Value: string);

    procedure ReadImage(const _Filename: string);
    procedure WriteImage(const _Filename: string);

    function GetImageWidth: Integer;
    function GetImageHeight: culong;

    procedure GetBitmap(_bmp: TBitmap); overload;
    procedure SetBitmap(_bmp: TBitmap); overload;
{$IFDEF SUPPORT_GRAPHICS32}
    procedure GetBitmap(_bmp: TBitmap32); overload;
    procedure SetBitmap(_bmp: TBitmap32); overload;
{$ENDIF}
  end;

implementation

uses
  Math;

{ TMagickWandDll }

constructor TMagickWandDll.Create;
begin
  inherited Create('CORE_RL_MagickWand_.dll');
end;

procedure TMagickWandDll.InitEntryPoints;
begin
  inherited;
  MagickWandGenesis := GetProcAddressEx('MagickWandGenesis');
  MagickWandTerminus := GetProcAddressEx('MagickWandTerminus');
  MagickGetException := GetProcAddressEx('MagickGetException');
  NewMagickWand := GetProcAddressEx('NewMagickWand');
  DestroyMagickWand := GetProcAddressEx('DestroyMagickWand');
  MagickRelinquishMemory := GetProcAddressEx('MagickRelinquishMemory');
  MagickResetIterator := GetProcAddressEx('MagickResetIterator');
  MagickNextImage := GetProcAddressEx('MagickNextImage');
  MagickReadImage := GetProcAddressEx('MagickReadImage');
  MagickResizeImage := GetProcAddressEx('MagickResizeImage');
  MagickWriteImage := GetProcAddressEx('MagickWriteImage');
  MagickWriteImages := GetProcAddressEx('MagickWriteImages');
  MagickCompositeImage := GetProcAddressEx('MagickCompositeImage');
  MagickDistortImage := GetProcAddressEx('MagickDistortImage');
  MagickDrawImage := GetProcAddressEx('MagickDrawImage');
  MagickGetImage := GetProcAddressEx('MagickGetImage');
  MagickGetImageBlob := GetProcAddressEx('MagickGetImageBlob');
  MagickGetImageHeight := GetProcAddressEx('MagickGetImageHeight');
  MagickGetImageRegion := GetProcAddressEx('MagickGetImageRegion');
  MagickGetImageWidth := GetProcAddressEx('MagickGetImageWidth');
  MagickReadImageBlob := GetProcAddressEx('MagickReadImageBlob');
  MagickSetImageFormat := GetProcAddressEx('MagickSetImageFormat');
  MagickSetImageProperty := GetProcAddressEx('MagickSetImageProperty');

  NewDrawingWand := GetProcAddressEx('NewDrawingWand');
  DestroyDrawingWand := GetProcAddressEx('DestroyDrawingWand');
  DrawGetException := GetProcAddressEx('DrawGetException');
  DrawPolygon := GetProcAddressEx('DrawPolygon');
  DrawSetFillColor := GetProcAddressEx('DrawSetFillColor');
  DrawSetStrokeColor := GetProcAddressEx('DrawSetStrokeColor');

  NewPixelWand := GetProcAddressEx('NewPixelWand');
  DestroyPixelWand := GetProcAddressEx('DestroyPixelWand');
  PixelSetColor := GetProcAddressEx('PixelSetColor');
end;

{ TPixelWandClass }

constructor TPixelWandClass.Create(_Dll: TMagickWandDll);
begin
  inherited Create(_Dll);
  FWand := FDll.NewPixelWand;
end;

destructor TPixelWandClass.Destroy;
begin
  FDll.DestroyPixelWand(FWand);
  inherited;
end;

procedure TPixelWandClass.ThrowWandException;
begin
  // todo: How do we get the error code and text?
  raise EMagickWand.Create('unknown PixelWand exception');
end;

procedure TPixelWandClass.SetColor(const _Color: string);
var
  Status: MagickBooleanType;
begin
  Status := FDll.PixelSetColor(FWand, PAnsiChar(_Color));
  if Status <> MagickTrue then
    ThrowWandException;
end;

{ TDrawingWandClass }

constructor TDrawingWandClass.Create(_Dll: TMagickWandDll);
begin
  inherited Create(_Dll);
  FWand := FDll.NewDrawingWand;
end;

destructor TDrawingWandClass.Destroy;
begin
  FDll.DestroyDrawingWand(FWand);
  inherited;
end;

procedure TDrawingWandClass.DrawPolygon(const _Coords: TPointInfoArr);
begin
  FDll.DrawPolygon(FWand, length(_Coords), @_Coords[0]);
end;

procedure TDrawingWandClass.SetFillColor(_Fill: TPixelWandClass);
begin
  FDll.DrawSetFillColor(FWand, _Fill.FWand);
end;

procedure TDrawingWandClass.SetStrokeColor(_Stroke: TPixelWandClass);
begin
  FDll.DrawSetStrokeColor(FWand, _Stroke.FWand);
end;

procedure TDrawingWandClass.ThrowWandException;
var
  Description: PAnsiChar;
  severity: ExceptionType;
  Desc: string;
begin
  Description := FDll.DrawGetException(FWand, @severity);
  try
    Desc := Description;
  finally
    FDll.MagickRelinquishMemory(Description);
  end;
  raise EMagickWand.CreateFmt(_('DrawingWand error: %s'), [Desc]);
end;

{ TMagickWandClass }

constructor TMagickWandClass.Create(_Dll: TMagickWandDll);
begin
  Create(_Dll, _Dll.NewMagickWand);
end;

constructor TMagickWandClass.Create(_Dll: TMagickWandDll; _Wand: PMagickWand);
begin
  inherited Create(_Dll);
  FWand := _Wand;
end;

destructor TMagickWandClass.Destroy;
begin
  FDll.DestroyMagickWand(FWand);
  inherited;
end;

procedure TMagickWandClass.ThrowWandException;
var
  Description: PAnsiChar;
  severity: ExceptionType;
  Desc: string;
begin
  Description := FDll.MagickGetException(FWand, @severity);
  try
    Desc := Description;
  finally
    FDll.MagickRelinquishMemory(Description);
  end;
  raise EMagickWand.CreateFmt(_('MagickWand error: %s'), [Desc]);
end;

procedure TMagickWandClass.DistortImage(_Method: DistortMethod; const _Arguments: TDoubleArray;
  _BestFit: Boolean);
var
  Status: MagickBooleanType;
  bestfit: MagickBooleanType;
begin
  bestfit := IfThen(_BestFit, MagickTrue, MagickFalse);
  Status := FDll.MagickDistortImage(FWand, _Method, length(_Arguments), @_Arguments[0], bestfit);
  if Status <> MagickTrue then
    ThrowWandException;
end;

procedure TMagickWandClass.DistortImage(_Method: DistortMethod; const _Points: TPointInfoArr;
  _BestFit: Boolean);
var
  Status: MagickBooleanType;
  bestfit: MagickBooleanType;
begin
  bestfit := IfThen(_BestFit, MagickTrue, MagickFalse);
  Status := FDll.MagickDistortImage(FWand, _Method, length(_Points) * 2, @_Points[0], bestfit);
  if Status <> MagickTrue then
    ThrowWandException;
end;

procedure TMagickWandClass.DrawImage(_Drawing: TDrawingWandClass);
var
  Status: MagickBooleanType;
begin
  Status := FDll.MagickDrawImage(FWand, _Drawing.FWand);
  if Status <> MagickTrue then
    ThrowWandException;
end;

procedure TMagickWandClass.GetBitmap(_bmp: TBitmap);
var
  blob: TdzMagickBlob;
begin
  SetImageFormat('BMP');
  blob := GetImageBlob;
  try
    blob.Position := 0;
    _bmp.LoadFromStream(blob);
  finally
    FreeAndNil(blob);
  end;
end;

{$IFDEF SUPPORT_GRAPHICS32}

procedure TMagickWandClass.GetBitmap(_bmp: TBitmap32);
var
  blob: TdzMagickBlob;
begin
  SetImageFormat('BMP');
  blob := GetImageBlob;
  try
    blob.Position := 0;
    _bmp.LoadFromStream(blob);
  finally
    FreeAndNil(blob);
  end;
end;
{$ENDIF}

function TMagickWandClass.GetImage: TMagickWandClass;
var
  NewWand: PMagickWand;
begin
  NewWand := FDll.MagickGetImage(FWand);
  if not Assigned(NewWand) then
    ThrowWandException;
  Result := TMagickWandClass.Create(FDll, NewWand);
end;

function TMagickWandClass.GetImageBlob: TdzMagickBlob;
var
  Size: Cardinal;
  blob: Pointer;
begin
  blob := FDll.MagickGetImageBlob(FWand, @Size);
  if not Assigned(blob) then
    ThrowWandException;
  Result := TdzMagickBlob.Create(FDll, blob, Size);
end;

function TMagickWandClass.GetImageHeight: culong;
begin
  Result := FDll.MagickGetImageHeight(FWand);
end;

function TMagickWandClass.GetImageRegion(const _Width, _Height: culong;
  const _X, _Y: clong): TMagickWandClass;
var
  NewWand: PMagickWand;
begin
  NewWand := FDll.MagickGetImageRegion(FWand, _Width, _Height, _X, _Y);
  if not Assigned(NewWand) then
    ThrowWandException;
  Result := TMagickWandClass.Create(FDll, NewWand);
end;

function TMagickWandClass.GetImageWidth: Integer;
begin
  Result := FDll.MagickGetImageWidth(FWand);
end;

procedure TMagickWandClass.ReadImage(const _Filename: string);
var
  Status: MagickBooleanType;
begin
  Status := FDll.MagickReadImage(FWand, PAnsiChar(_Filename));
  if Status <> MagickTrue then
    ThrowWandException;
end;

procedure TMagickWandClass.ReadImageBlob(_Blob: TMemoryStream);
var
  Status: MagickBooleanType;
begin
  _Blob.Position := 0;
  Status := FDll.MagickReadImageBlob(FWand, _Blob.Memory, _Blob.Size);
  if Status <> MagickTrue then
    ThrowWandException;
end;

procedure TMagickWandClass.SetBitmap(_bmp: TBitmap);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    _bmp.SaveToStream(ms);
    ReadImageBlob(ms);
  finally
    FreeAndNil(ms);
  end;
end;

{$IFDEF SUPPORT_GRAPHICS32}

procedure TMagickWandClass.SetBitmap(_bmp: TBitmap32);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    _bmp.SaveToStream(ms);
    ReadImageBlob(ms);
  finally
    FreeAndNil(ms);
  end;
end;
{$ENDIF}

procedure TMagickWandClass.SetImageFormat(const _ImageFormat: string);
var
  Status: MagickBooleanType;
begin
  Status := FDll.MagickSetImageFormat(FWand, PAnsiChar(_ImageFormat));
  if Status <> MagickTrue then
    ThrowWandException;
end;

procedure TMagickWandClass.SetImageProperty(const _Key, _Value: string);
var
  Status: MagickBooleanType;
begin
  Status := FDll.MagickSetImageProperty(FWand, PAnsiChar(_Key), PAnsiChar(_Value));
  if Status <> MagickTrue then
    ThrowWandException;
end;

procedure TMagickWandClass.WriteImage(const _Filename: string);
var
  Status: MagickBooleanType;
begin
  Status := FDll.MagickWriteImage(FWand, PAnsiChar(_Filename));
  if Status <> MagickTrue then
    ThrowWandException;
end;

{ TWandClass }

constructor TWandClass.Create(_Dll: TMagickWandDll);
begin
  inherited Create;
  FDll := _Dll;
end;

{ TdzMagickBlob }

constructor TdzMagickBlob.Create(_Dll: TMagickWandDll; _Memory: Pointer; _Size: Cardinal);
begin
  inherited Create;
  FDll := _Dll;
  SetPointer(_Memory, _Size);
end;

destructor TdzMagickBlob.Destroy;
begin
  if Size <> 0 then
    FDll.MagickRelinquishMemory(Memory);
  inherited;
end;

function TdzMagickBlob.Write(const Buffer; Count: Integer): Longint;
begin
  raise EMagickWand.Create(_('Programmer error: TdzMagickBlob.Write is not implemented'));
end;

end.
