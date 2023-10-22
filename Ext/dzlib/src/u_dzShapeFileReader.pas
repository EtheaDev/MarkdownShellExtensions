unit u_dzShapeFileReader;

// Shape File description taken from
// http://www.esri.com/library/whitepapers/pdfs/shapefile.pdf

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzNullableDouble,
  u_dzShapeFileConsts;

type
  TShapeFileReader = class
  public
    type
      TShpBoundingBox = packed record
        XMin: Double;
        YMin: Double;
        XMax: Double;
        YMax: Double;
      end;
      TShpBoundingBoxM = packed record
        XMin: Double;
        YMin: Double;
        FMMin: Double;
        XMax: Double;
        YMax: Double;
        FMMax: Double;
        function MMin: TNullableDouble;
        function MMax: TNullableDouble;
      end;
      TShpBoundingBoxZ = packed record
        XMin: Double;
        YMin: Double;
        ZMin: Double;
        FMMin: Double;
        XMax: Double;
        YMax: Double;
        ZMax: Double;
        FMMax: Double;
        function MMin: TNullableDouble;
        function MMax: TNullableDouble;
      end;
    type
      TShpPointZ = packed record
        X: Double;
        Y: Double;
        Z: Double;
        FM: Double; // measure, note that this can have the "no data" value (that is < –1E38)
        ///<summary> interprets FM < -1E38 as invalid, returns all other values as they are </summary>
        function M: TNullableDouble;
      end;
    type
      TShpPointM = packed record
        X: Double;
        Y: Double;
        FM: Double; // measure, note that this can have the "no data" value (that is < –1E38)
        ///<summary> interprets FM < -1E38 as invalid, returns all other values as they are </summary>
        function M: TNullableDouble;
      end;
    type
      TShpMinMax = packed record
        FMin: Double;
        FMax: Double;
      end;
    type
      TShpPointArr = array of TShpPoint;
      TShpPartArr = array of TShpPointArr;
      TShpPointMArr = array of TShpPointM;
      TShpPartMArr = array of TShpPointMArr;
      TShpPointZArr = array of TShpPointZ;
      TShpPartZArr = array of TShpPointZArr;
    type
      TOnFileHeader = procedure(_Sender: TObject;
        _FileCode: LongWord; _FileLength: Int64;
        _Version: LongWord; _ShapeType: TShapeTypeEnum;
        _XMin: Double; _YMin: Double; _ZMin: TNullableDouble; _MMin: TNullableDouble;
        _XMax: Double; _YMax: Double; _ZMax: TNullableDouble; _MMax: TNullableDouble) of object;
      TOnRecordHeader = procedure(_Sender: TObject;
        _RecordNo, _ContentLength: LongWord) of object;
      TOnNullShape = procedure(_Sender: TObject;
        _RecordNo: LongWord) of object;
      TOnPointShape = procedure(_Sender: TObject;
        _RecordNo: LongWord;
        const _Point: TShpPoint) of object;
      TOnPointMShape = procedure(_Sender: TObject;
        _RecordNo: LongWord;
        const _Point: TShpPointM) of object;
      TOnPointZShape = procedure(_Sender: TObject;
        _RecordNo: LongWord;
        const _Point: TShpPointZ) of object;
      TOnPolygonShape = procedure(_Sender: TObject;
        _RecordNo: LongWord;
        const _BoundingBox: TShpBoundingBox;
        const _Parts: TShpPartArr) of object;
      TOnPolygonMShape = procedure(_Sender: TObject;
        _RecordNo: LongWord;
        const _BoundingBox: TShpBoundingBoxM;
        const _Parts: TShpPartMArr) of object;
      TOnPolygonZShape = procedure(_Sender: TObject;
        _RecordNo: LongWord;
        const _BoundingBox: TShpBoundingBoxZ;
        const _Parts: TShpPartZArr) of object;
      TOnPolyLineShape = procedure(_Sender: TObject;
        _RecordNo: LongWord;
        const _BoundingBox: TShpBoundingBox;
        const _Parts: TShpPartArr) of object;
      TOnPolyLineMShape = procedure(_Sender: TObject;
        _RecordNo: LongWord;
        const _BoundingBox: TShpBoundingBoxM;
        const _Parts: TShpPartMArr) of object;
      TOnPolyLineZShape = procedure(_Sender: TObject;
        _RecordNo: LongWord;
        const _BoundingBox: TShpBoundingBoxZ;
        const _Parts: TShpPartZArr) of object;
      TOnMultiPointShape = procedure(_Sender: TObject;
        _RecordNo: LongWord;
        const _BoundingBox: TShpBoundingBox;
        const _Points: TShpPointArr) of object;
      TOnMultiPointMShape = procedure(_Sender: TObject;
        _RecordNo: LongWord;
        const _BoundingBox: TShpBoundingBoxM;
        const _Points: TShpPointMArr) of object;
      TOnMultiPointZShape = procedure(_Sender: TObject;
        _RecordNo: LongWord;
        const _BoundingBox: TShpBoundingBoxZ;
        const _Points: TShpPointZArr) of object;
  private
    type
      TShpMainFileHeader = packed record
        FFileCode: LongWord; // Big Endian, use FileCode function to get it in Little Endian
        Unused: array[1..5] of Integer; // Big Endian
        FFileLength: LongWord; // Big Endian, use FileLength function to get it in Little Endian
        Version: LongWord; // Little Endian
        FShapeType: LongWord; // Little Endian, use ShapeType to get it as TShapeTypeEnum
        XMin: Double; // Little Endian
        YMin: Double; // Little Endian
        XMax: Double; // Little Endian
        YMax: Double; // Little Endian
        ZMin: Double; // Little Endian
        ZMax: Double; // Little Endian
        FMMin: Double; // Little Endian
        FMMax: Double; // Little Endian
        function ShapeType: TShapeTypeEnum;
        function FileLength: Int64;
        function FileCode: LongWord;
        function MMin: TNullableDouble;
        function MMax: TNullableDouble;
      end;
    type
      TShpRecordHeader = packed record
        ///<sumamry> Record number in Big Endian format, corresponds to record number in dbf file </summary>
        FRecordNo: LongWord;
        ///<summary> Length of record content in Big Endian format in 16 bit words,
        ///          excluding the header size</summary>
        FContentLength: LongWord; // Big Endian
        ///<summary> Returns the record number</summary>
        function RecordNo: LongWord;
        ///<summary> Length of record content in bytes, excluding the header size </summary>
        function ContentLength: Int64;
      end;
  private
    FOnFileHeader: TOnFileHeader;
    FOnRecordHeader: TOnRecordHeader;
    FOnNullShape: TOnNullShape;
    FOnPointShape: TOnPointShape;
    FOnPointMShape: TOnPointMShape;
    FOnPointZShape: TOnPointZShape;
    FOnPolygonShape: TOnPolygonShape;
    FOnPolygonMShape: TOnPolygonMShape;
    FOnPolygonZShape: TOnPolygonZShape;
    FOnPolyLineShape: TOnPolyLineShape;
    FOnPolyLineMShape: TOnPolyLineMShape;
    FOnPolyLineZShape: TOnPolyLineZShape;
    FOnMultiPointShape: TOnMultiPointShape;
    FOnMultiPointMShape: TOnMultiPointMShape;
    FOnMultiPointZShape: TOnMultiPointZShape;
    procedure doOnFileHeader(_FileCode: Integer; _FileLength: Integer;
      _Version: Integer; _ShapeType: TShapeTypeEnum;
      _XMin: Double; _YMin: Double; _ZMin: TNullableDouble; _MMin: TNullableDouble;
      _XMax: Double; _YMax: Double; _ZMax: TNullableDouble; _MMax: TNullableDouble);
    procedure doOnRecordHeader(_RecordNo, _ContentLength: LongWord);
    procedure doOnNullShape(_RecordNo: LongWord);
    procedure doOnPointShape(_RecordNo: LongWord;
      const _Point: TShpPoint);
    procedure doOnPointMShape(_RecordNo: LongWord;
      const _Point: TShpPointM);
    procedure doOnPointZShape(_RecordNo: LongWord;
      const _Point: TShpPointZ);
    procedure doOnPolygonShape(_RecordNo: LongWord;
      const _BoundingBox: TShpBoundingBox;
      const _Parts: TShpPartArr);
    procedure doOnPolygonMShape(_RecordNo: LongWord;
      const _BoundingBox: TShpBoundingBoxM;
      const _Parts: TShpPartMArr);
    procedure doOnPolygonZShape(_RecordNo: LongWord;
      const _BoundingBox: TShpBoundingBoxZ;
      const _Parts: TShpPartZArr);
    procedure doOnPolyLineShape(_RecordNo: LongWord;
      const _BoundingBox: TShpBoundingBox;
      const _Parts: TShpPartArr);
    procedure doOnPolyLineMShape(_RecordNo: LongWord;
      const _BoundingBox: TShpBoundingBoxM;
      const _Parts: TShpPartMArr);
    procedure doOnPolyLineZShape(_RecordNo: LongWord;
      const _BoundingBox: TShpBoundingBoxZ;
      const _Parts: TShpPartZArr);
    procedure doOnMultiPointShape(_RecordNo: LongWord;
      const _BoundingBox: TShpBoundingBox;
      _Points: TShpPointArr);
    procedure doOnMultiPointMShape(_RecordNo: LongWord;
      const _BoundingBox: TShpBoundingBoxM;
      _Points: TShpPointMArr);
    procedure doOnMultiPointZShape(_RecordNo: LongWord;
      const _BoundingBox: TShpBoundingBoxZ;
      _Points: TShpPointZArr);
    procedure ReadMultiPartShape(_Stream: TStream;
      _RecordHeader: TShpRecordHeader;
      var _Box: TShpBoundingBox;
      var _Parts: TShpPartArr);
    procedure ReadMultiPartMShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
      var _Box: TShpBoundingBoxM; var _PartsM: TShpPartMArr);
    procedure ReadMultiPartZShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
      var _Box: TShpBoundingBoxZ; var _PartsZ: TShpPartZArr);
    procedure doReadMultiPartShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
      var _Box: TShpBoundingBox; var _Parts: TShpPartArr;
      out _NumParts: LongWord; out _NumPoints: LongWord);
    procedure ReadMultiPointZShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
      var _Box: TShpBoundingBoxZ; var _PointsZ: TShpPointZArr);
    procedure AssertStreamPosition(_Position, _StartPos: Int64; _ContentLength: LongWord);
    procedure ReadMultiPointMShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
      var _Box: TShpBoundingBoxM; var _Points: TShpPointMArr);
    procedure ReadMultiPointShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
      var _Box: TShpBoundingBox; var _Points: TShpPointArr);
    procedure doReadMultiPointShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
      var _Box: TShpBoundingBox; var _Points: TShpPointArr; out _NumPoints: LongWord);
    ///<summary> returns true, if MValue is < -1E38 </summary>
    class function IsMNoValue(const _MValue: Double): Boolean;
    ///<summary> converts an M value to a TNullableDouble, recognizing "no value" values </summary>
    class function MValueToNullableDouble(const _MValue: Double): TNullableDouble;
  public
    class function ShapeTypeToString(_ShapeType: TShapeTypeEnum): string;
    constructor Create;
    destructor Destroy; override;
//    function CheckFile(const _Filename: string): TShpFileInfo;
    procedure ReadFile(const _Filename: string);
    property OnFileHeader: TOnFileHeader read FOnFileHeader write FOnFileHeader;
    property OnRecordHeader: TOnRecordHeader read FOnRecordHeader write FOnRecordHeader;
    property OnNullShape: TOnNullShape read FOnNullShape write FOnNullShape;
    property OnPointShape: TOnPointShape read FOnPointShape write FOnPointShape;
    property OnPointMShape: TOnPointMShape read FOnPointMShape write FOnPointMShape;
    property OnPointZShape: TOnPointZShape read FOnPointZShape write FOnPointZShape;
    property OnPolygonShape: TOnPolygonShape read FOnPolygonShape write FOnPolygonShape;
    property OnPolygonMShape: TOnPolygonMShape read FOnPolygonMShape write FOnPolygonMShape;
    property OnPolygonZShape: TOnPolygonZShape read FOnPolygonZShape write FOnPolygonZShape;
    property OnPolyLineShape: TOnPolyLineShape read FOnPolyLineShape write FOnPolyLineShape;
    property OnPolyLineMShape: TOnPolyLineMShape read FOnPolyLineMShape write FOnPolyLineMShape;
    property OnPolyLineZShape: TOnPolyLineZShape read FOnPolyLineZShape write FOnPolyLineZShape;
    property OnMultiPointShape: TOnMultiPointShape read FOnMultiPointShape write FOnMultiPointShape;
    property OnMultiPointMShape: TOnMultiPointMShape read FOnMultiPointMShape write FOnMultiPointMShape;
    property OnMultiPointZShape: TOnMultiPointZShape read FOnMultiPointZShape write FOnMultiPointZShape;
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  Math,
  u_dzConvertUtils,
  u_dzFileStreams;

{ TShapeFileReader }

constructor TShapeFileReader.Create;
begin
  inherited Create;
end;

destructor TShapeFileReader.Destroy;
begin
  inherited Destroy;
end;

procedure TShapeFileReader.doOnFileHeader(_FileCode: Integer; _FileLength: Integer;
  _Version: Integer; _ShapeType: TShapeTypeEnum;
  _XMin: Double; _YMin: Double; _ZMin: TNullableDouble; _MMin: TNullableDouble;
  _XMax: Double; _YMax: Double; _ZMax: TNullableDouble; _MMax: TNullableDouble);
begin
  if Assigned(FOnFileHeader) then
    FOnFileHeader(Self,
      _FileCode, _FileLength,
      _Version, _ShapeType,
      _XMin, _YMin, _ZMin, _MMin,
      _XMax, _YMax, _ZMax, _MMax);
end;

procedure TShapeFileReader.doOnRecordHeader(_RecordNo, _ContentLength: LongWord);
begin
  if Assigned(FOnRecordHeader) then
    FOnRecordHeader(Self, _RecordNo, _ContentLength);
end;

class function TShapeFileReader.IsMNoValue(const _MValue: Double): Boolean;
begin
  Result := IsNan(_MValue) or (_MValue < -1E38);
end;

class function TShapeFileReader.MValueToNullableDouble(const _MValue: Double): TNullableDouble;
begin
  if IsMNoValue(_MValue) then
    Result.Invalidate
  else
    Result := _MValue;
end;

procedure TShapeFileReader.doOnNullShape(_RecordNo: LongWord);
begin
  if Assigned(FOnNullShape) then
    FOnNullShape(Self, _RecordNo);
end;

procedure TShapeFileReader.doOnPointShape(_RecordNo: LongWord; const _Point: TShpPoint);
begin
  if Assigned(FOnPointShape) then
    FOnPointShape(Self, _RecordNo, _Point);
end;

procedure TShapeFileReader.doOnPointMShape(_RecordNo: LongWord; const _Point: TShpPointM);
begin
  if Assigned(FOnPointMShape) then
    FOnPointMShape(Self, _RecordNo, _Point);
end;

procedure TShapeFileReader.doOnPointZShape(_RecordNo: LongWord; const _Point: TShpPointZ);
begin
  if Assigned(FOnPointZShape) then
    FOnPointZShape(Self, _RecordNo, _Point);
end;

procedure TShapeFileReader.doOnPolyLineShape(_RecordNo: LongWord;
  const _BoundingBox: TShpBoundingBox; const _Parts: TShpPartArr);
begin
  if Assigned(FOnPolyLineShape) then
    FOnPolyLineShape(Self, _RecordNo, _BoundingBox, _Parts);
end;

procedure TShapeFileReader.doOnPolyLineMShape(_RecordNo: LongWord;
  const _BoundingBox: TShpBoundingBoxM; const _Parts: TShpPartMArr);
begin
  if Assigned(FOnPolyLineMShape) then
    FOnPolyLineMShape(Self, _RecordNo, _BoundingBox, _Parts);
end;

procedure TShapeFileReader.doOnPolyLineZShape(_RecordNo: LongWord;
  const _BoundingBox: TShpBoundingBoxZ; const _Parts: TShpPartZArr);
begin
  if Assigned(FOnPolyLineZShape) then
    FOnPolyLineZShape(Self, _RecordNo, _BoundingBox, _Parts);
end;

procedure TShapeFileReader.doOnPolygonShape(_RecordNo: LongWord;
  const _BoundingBox: TShpBoundingBox;
  const _Parts: TShpPartArr);
begin
  if Assigned(FOnPolygonShape) then
    FOnPolygonShape(Self, _RecordNo, _BoundingBox, _Parts);
end;

procedure TShapeFileReader.doOnPolygonMShape(_RecordNo: LongWord;
  const _BoundingBox: TShpBoundingBoxM; const _Parts: TShpPartMArr);
begin
  if Assigned(FOnPolygonMShape) then
    FOnPolygonMShape(Self, _RecordNo, _BoundingBox, _Parts);
end;

procedure TShapeFileReader.doOnPolygonZShape(_RecordNo: LongWord;
  const _BoundingBox: TShpBoundingBoxZ; const _Parts: TShpPartZArr);
begin
  if Assigned(FOnPolygonZShape) then
    FOnPolygonZShape(Self, _RecordNo, _BoundingBox, _Parts);
end;

procedure TShapeFileReader.doOnMultiPointShape(_RecordNo: LongWord;
  const _BoundingBox: TShpBoundingBox; _Points: TShpPointArr);
begin
  if Assigned(FOnMultiPointShape) then
    FOnMultiPointShape(Self, _RecordNo, _BoundingBox, _Points);
end;

procedure TShapeFileReader.doOnMultiPointMShape(_RecordNo: LongWord;
  const _BoundingBox: TShpBoundingBoxM; _Points: TShpPointMArr);
begin
  if Assigned(FOnMultiPointMShape) then
    FOnMultiPointMShape(Self, _RecordNo, _BoundingBox, _Points);
end;

procedure TShapeFileReader.doOnMultiPointZShape(_RecordNo: LongWord;
  const _BoundingBox: TShpBoundingBoxZ;
  _Points: TShpPointZArr);
begin
  if Assigned(FOnMultiPointZShape) then
    FOnMultiPointZShape(Self, _RecordNo, _BoundingBox, _Points);
end;

procedure TShapeFileReader.doReadMultiPartShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
  var _Box: TShpBoundingBox; var _Parts: TShpPartArr;
  out _NumParts: LongWord; out _NumPoints: LongWord);
var
  PartsIdx: packed array of LongWord;
  Points: packed array of TShpPoint;
  PointIdx: LongWord;
  i: LongWord;
  j: LongWord;
  Len: Int64;
begin
  _Stream.ReadBuffer(_Box, SizeOf(_Box));
  _Stream.ReadBuffer(_NumParts, SizeOf(_NumParts));
  _Stream.ReadBuffer(_NumPoints, SizeOf(_NumPoints));
  SetLength(PartsIdx, _NumParts);
  _Stream.ReadBuffer(PartsIdx[0], Length(PartsIdx) * SizeOf(PartsIdx[0]));
  SetLength(Points, _NumPoints);
  _Stream.ReadBuffer(Points[0], Length(Points) * SizeOf(Points[0]));
  Assert(_RecordHeader.ContentLength >= SizeOf(LongWord) + SizeOf(_Box) +
    SizeOf(_NumParts) + SizeOf(_NumPoints)
    + Length(PartsIdx) * SizeOf(PartsIdx[0])
    + Length(Points) * SizeOf(Points[0]));
  SetLength(_Parts, _NumParts);
  for i := 0 to _NumParts - 1 do begin
    PointIdx := PartsIdx[i];
    if i + 1 < _NumParts then
      Len := PartsIdx[i + 1] - PointIdx
    else
      Len := _NumPoints - PointIdx;
    SetLength(_Parts[i], Len);
    for j := 0 to Len - 1 do begin
      _Parts[i][j] := Points[PointIdx + j];
    end;
  end;
end;

procedure TShapeFileReader.ReadMultiPartShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
  var _Box: TShpBoundingBox; var _Parts: TShpPartArr);
var
  NumPoints: LongWord;
  NumParts: LongWord;
begin
  doReadMultiPartShape(_Stream, _RecordHeader, _Box, _Parts, NumParts, NumPoints);
end;

procedure TShapeFileReader.ReadMultiPartMShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
  var _Box: TShpBoundingBoxM; var _PartsM: TShpPartMArr);
var
  Box: TShpBoundingBox;
  Parts: TShpPartArr;
  NumParts: LongWord;
  NumPoints: LongWord;
  PartIdx: LongWord;
  MRange: TShpMinMax;
  MValues: packed array of Double;
  PointIdx: Integer;
  Offset: Integer;
begin
  doReadMultiPartShape(_Stream, _RecordHeader, Box, Parts, NumParts, NumPoints);
  _Box.XMin := Box.XMin;
  _Box.XMax := Box.XMax;
  _Box.YMin := Box.YMin;
  _Box.XMax := Box.YMax;

  SetLength(MValues, NumPoints);
  // According to the technical description, PolyLineM and PolygonM records optionally(!)
  // contain M-values, so we need to check whether the ContentLength is
  // large enough to contain M-values or not.
  if _RecordHeader.ContentLength =
    SizeOf(LongWord) //                  Shape Type
    + SizeOf(TShpBoundingBox) //         Box (4 * double)
    + SizeOf(NumParts) //                NumParts (1 * integer)
    + SizeOf(NumPoints) //               NumPoints (1 * integer)
    + NumParts * SizeOf(LongWord) //     Parts (NumParts * integer)
    + NumPoints * SizeOf(TShpPoint) {//  Points (NumPoints * 2 * double)} then begin
    _Box.FMMin := -1E39;
    _Box.FMMax := -1E39;
    for PointIdx := Low(MValues) to High(MValues) do begin
      MValues[PointIdx] := -1E39;
    end;
  end else if _RecordHeader.ContentLength =
    SizeOf(LongWord) //                  Shape Type
    + SizeOf(TShpBoundingBox) //         Box (4 * double)
    + SizeOf(NumParts) //                NumParts (1 * integer)
    + SizeOf(NumPoints) //               NumPoints (1 * integer)
    + NumParts * SizeOf(LongWord) //     Parts (NumParts * integer)
    + NumPoints * SizeOf(TShpPoint) //   Points (NumPoints * 2 * double)
    + SizeOf(TShpMinMax) //              M Min / Max (2 * double) (optional)
    + NumPoints * SizeOf(Double) {//     Marray (NumPoints * double) (optional)} then begin
    _Stream.ReadBuffer(MRange, SizeOf(MRange));
    _Stream.ReadBuffer(MValues[0], Length(MValues) * SizeOf(MValues[0]));
  end else
    raise Exception.CreateFmt(_('ContentLength %d is not valid for a PolyLineZ or PolygonZ record.'),
      [_RecordHeader.ContentLength]);

  SetLength(_PartsM, Length(Parts));
  Offset := 0;
  for PartIdx := Low(Parts) to High(Parts) do begin
    SetLength(_PartsM[PartIdx], Length(Parts[PartIdx]));
    for PointIdx := Low(Parts[PartIdx]) to High(Parts[PartIdx]) do begin
      _PartsM[PartIdx][PointIdx].X := Parts[PartIdx][PointIdx].X;
      _PartsM[PartIdx][PointIdx].Y := Parts[PartIdx][PointIdx].Y;
      _PartsM[PartIdx][PointIdx].FM := MValues[Offset];
      Inc(Offset);
    end;
  end;
end;

procedure TShapeFileReader.ReadMultiPartZShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
  var _Box: TShpBoundingBoxZ; var _PartsZ: TShpPartZArr);
var
  Box: TShpBoundingBox;
  Parts: TShpPartArr;
  NumParts: LongWord;
  NumPoints: LongWord;
  PartIdx: LongWord;
  ZRange: TShpMinMax;
  ZValues: packed array of Double;
  MRange: TShpMinMax;
  MValues: packed array of Double;
  PointIdx: Integer;
  Offset: Integer;
begin
  doReadMultiPartShape(_Stream, _RecordHeader, Box, Parts, NumParts, NumPoints);
  _Box.XMin := Box.XMin;
  _Box.XMax := Box.XMax;
  _Box.YMin := Box.YMin;
  _Box.XMax := Box.YMax;

  _Stream.ReadBuffer(ZRange, SizeOf(ZRange));
  _Box.ZMin := ZRange.FMin;
  _Box.ZMax := ZRange.FMax;

  SetLength(ZValues, NumPoints);
  _Stream.ReadBuffer(ZValues[0], Length(ZValues) * SizeOf(ZValues[0]));

  SetLength(MValues, NumPoints);
  // According to the technical description, PolyLineZ and PolygonZ records optionally(!)
  // contain M-values, so we need to check whether the ContentLength is
  // large enough to contain M-values or not.
  if _RecordHeader.ContentLength =
    SizeOf(LongWord) // Shape Type
    + SizeOf(TShpBoundingBox)
    + SizeOf(NumParts)
    + SizeOf(NumPoints)
    + NumParts * SizeOf(LongWord)
    + NumPoints * SizeOf(TShpPoint)
    + SizeOf(TShpMinMax)
    + NumPoints * SizeOf(Double) then begin
    _Box.FMMin := -1E39;
    _Box.FMMax := -1E39;
    for PointIdx := Low(MValues) to High(MValues) do begin
      MValues[PointIdx] := -1E39;
    end;
  end else if _RecordHeader.ContentLength =
    SizeOf(LongWord) //                Shape Type
    + SizeOf(TShpBoundingBox) //       Box
    + SizeOf(NumParts) //              NumParts
    + SizeOf(NumPoints) //             NumPoints
    + NumParts * SizeOf(LongWord) //   Parts
    + NumPoints * SizeOf(TShpPoint) // Points
    + SizeOf(TShpMinMax) //            ZMin / Max
    + NumPoints * SizeOf(Double) //    Zarray
    + SizeOf(TShpMinMax) //            MMin / Max (optional)
    + NumPoints * SizeOf(Double) {//   Marray} then begin
    _Stream.ReadBuffer(MRange, SizeOf(MRange));
    _Stream.ReadBuffer(MValues[0], Length(MValues) * SizeOf(MValues[0]));
  end else
    raise Exception.CreateFmt(_('ContentLength %d is not valid for a PolyLineZ or PolygonZ record.'),
      [_RecordHeader.ContentLength]);

  SetLength(_PartsZ, Length(Parts));
  Offset := 0;
  for PartIdx := Low(Parts) to High(Parts) do begin
    SetLength(_PartsZ[PartIdx], Length(Parts[PartIdx]));
    for PointIdx := Low(Parts[PartIdx]) to High(Parts[PartIdx]) do begin
      _PartsZ[PartIdx][PointIdx].X := Parts[PartIdx][PointIdx].X;
      _PartsZ[PartIdx][PointIdx].Y := Parts[PartIdx][PointIdx].Y;
      _PartsZ[PartIdx][PointIdx].FM := MValues[Offset];
      _PartsZ[PartIdx][PointIdx].Z := ZValues[Offset];
      Inc(Offset);
    end;
  end;
end;

procedure TShapeFileReader.doReadMultiPointShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
  var _Box: TShpBoundingBox; var _Points: TShpPointArr; out _NumPoints: LongWord);
begin
  _Stream.ReadBuffer(_Box, SizeOf(_Box));

  _Stream.ReadBuffer(_NumPoints, SizeOf(_NumPoints));
  SetLength(_Points, _NumPoints);
  _Stream.ReadBuffer(_Points[0], Length(_Points) * SizeOf(_Points[0]));
end;

procedure TShapeFileReader.ReadMultiPointShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
  var _Box: TShpBoundingBox; var _Points: TShpPointArr);
var
  NumPoints: LongWord;
begin
  doReadMultiPointShape(_Stream, _RecordHeader, _Box, _Points, NumPoints);
end;

procedure TShapeFileReader.ReadMultiPointMShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
  var _Box: TShpBoundingBoxM; var _Points: TShpPointMArr);
var
  Box: TShpBoundingBox;
  NumPoints: LongWord;
  Points: TShpPointArr;
  MRange: TShpMinMax;
  MValues: packed array of Double;
  PointIdx: Integer;
begin
  doReadMultiPointShape(_Stream, _RecordHeader, Box, Points, NumPoints);
  _Box.XMin := Box.XMin;
  _Box.XMax := Box.XMax;
  _Box.YMin := Box.YMin;
  _Box.XMax := Box.YMax;

  SetLength(MValues, NumPoints);
  // According to the technical description, MultiPointM records optionally(!)
  // contain M-values, so we need to check whether the ContentLength is
  // large enough to contain M-values or not.
  if _RecordHeader.ContentLength =
    SizeOf(LongWord) // Shape Type
    + SizeOf(TShpBoundingBox)
    + SizeOf(NumPoints)
    + NumPoints * SizeOf(TShpPoint) then begin
    _Box.FMMin := -1E39;
    _Box.FMMax := -1E39;
    for PointIdx := Low(MValues) to High(MValues) do begin
      MValues[PointIdx] := -1E39;
    end;
  end else if _RecordHeader.ContentLength =
    SizeOf(LongWord)
    + SizeOf(TShpBoundingBox)
    + SizeOf(NumPoints)
    + NumPoints * SizeOf(TShpPoint)
    + SizeOf(TShpMinMax)
    + NumPoints * SizeOf(Double) then begin
    _Stream.ReadBuffer(MRange, SizeOf(MRange));
    _Box.FMMin := MRange.FMin;
    _Box.FMMax := MRange.FMax;
    _Stream.ReadBuffer(MValues[0], Length(MValues) * SizeOf(MValues[0]));
  end else begin
    raise Exception.CreateFmt(_('ContentLength %d is not valid for a MultiPointM record.'),
      [_RecordHeader.ContentLength]);
  end;

  SetLength(_Points, Length(Points));
  for PointIdx := Low(Points) to High(Points) do begin
    _Points[PointIdx].X := Points[PointIdx].X;
    _Points[PointIdx].Y := Points[PointIdx].Y;
    _Points[PointIdx].FM := MValues[PointIdx];
  end;
end;

procedure TShapeFileReader.ReadMultiPointZShape(_Stream: TStream; _RecordHeader: TShpRecordHeader;
  var _Box: TShpBoundingBoxZ; var _PointsZ: TShpPointZArr);
var
  Box: TShpBoundingBox;
  NumPoints: LongWord;
  Points: TShpPointArr;
  ZRange: TShpMinMax;
  ZValues: packed array of Double;
  MRange: TShpMinMax;
  MValues: packed array of Double;
  PointIdx: Integer;
begin
  doReadMultiPointShape(_Stream, _RecordHeader, Box, Points, NumPoints);
  _Box.XMin := Box.XMin;
  _Box.XMax := Box.XMax;
  _Box.YMin := Box.YMin;
  _Box.XMax := Box.YMax;

  _Stream.ReadBuffer(ZRange, SizeOf(ZRange));
  _Box.ZMin := ZRange.FMin;
  _Box.ZMax := ZRange.FMax;

  SetLength(ZValues, NumPoints);
  _Stream.ReadBuffer(ZValues[0], Length(ZValues) * SizeOf(ZValues[0]));

  SetLength(MValues, NumPoints);
  // According to the technical description, MultiPointZ records optionally(!)
  // contain M-values, so we need to check whether the ContentLength is
  // large enough to contain M-values or not.
  if _RecordHeader.ContentLength =
    SizeOf(LongWord)
    + SizeOf(TShpBoundingBox)
    + SizeOf(NumPoints)
    + NumPoints * SizeOf(TShpPoint)
    + SizeOf(TShpMinMax)
    + NumPoints * SizeOf(Double) then begin
    _Box.FMMin := -1E39;
    _Box.FMMax := -1E39;
    for PointIdx := Low(MValues) to High(MValues) do begin
      MValues[PointIdx] := -1E39;
    end;
  end else if _RecordHeader.ContentLength =
    SizeOf(LongWord)
    + SizeOf(TShpBoundingBox)
    + SizeOf(NumPoints)
    + NumPoints * SizeOf(TShpPoint)
    + SizeOf(TShpMinMax)
    + NumPoints * SizeOf(Double)
    + SizeOf(TShpMinMax)
    + NumPoints * SizeOf(Double) then begin
    _Stream.ReadBuffer(MRange, SizeOf(MRange));
    _Box.FMMin := MRange.FMin;
    _Box.FMMax := MRange.FMax;
    _Stream.ReadBuffer(MValues[0], Length(MValues) * SizeOf(MValues[0]));
  end else begin
    raise Exception.CreateFmt(_('ContentLength %d is not valid for a MultiPointZ record.'),
      [_RecordHeader.ContentLength]);
  end;

  SetLength(_PointsZ, Length(Points));
  for PointIdx := Low(Points) to High(Points) do begin
    _PointsZ[PointIdx].X := Points[PointIdx].X;
    _PointsZ[PointIdx].Y := Points[PointIdx].Y;
    _PointsZ[PointIdx].FM := MValues[PointIdx];
    _PointsZ[PointIdx].Z := ZValues[PointIdx];
  end;
end;

procedure TShapeFileReader.AssertStreamPosition(_Position: Int64; _StartPos: Int64;
  _ContentLength: LongWord);
var
  Expected: Int64;
begin
  Expected := _StartPos + _ContentLength;
  Assert(_Position = Expected,
    Format('StreamPosition is %d but should be %d (Length: %d)', [_Position, Expected, _ContentLength]));
end;

procedure TShapeFileReader.ReadFile(const _Filename: string);
var
  StreamPos: Int64;

  procedure HandleNullShape(_Stream: TStream; const _RecordHeader: TShpRecordHeader);
  begin
    AssertStreamPosition(_Stream.Position, StreamPos, _RecordHeader.ContentLength);
    doOnNullShape(_RecordHeader.RecordNo);
  end;

  procedure HandlePointShape(_Stream: TStream; const _RecordHeader: TShpRecordHeader);
  var
    Point: TShpPoint;
  begin
    _Stream.ReadBuffer(Point, SizeOf(Point));
    AssertStreamPosition(_Stream.Position, StreamPos, _RecordHeader.ContentLength);
    doOnPointShape(_RecordHeader.RecordNo, Point);
  end;

  procedure HandlePointMShape(_Stream: TStream; const _RecordHeader: TShpRecordHeader);
  var
    Point: TShpPointM;
  begin
    _Stream.ReadBuffer(Point, SizeOf(Point));
    AssertStreamPosition(_Stream.Position, StreamPos, _RecordHeader.ContentLength);
    doOnPointMShape(_RecordHeader.RecordNo, Point);
  end;

  procedure HandlePointZShape(_Stream: TStream; const _RecordHeader: TShpRecordHeader);
  var
    Point: TShpPointZ;
  begin
    _Stream.ReadBuffer(Point, SizeOf(Point));
    AssertStreamPosition(_Stream.Position, StreamPos, _RecordHeader.ContentLength);
    doOnPointZShape(_RecordHeader.RecordNo, Point);
  end;

  procedure HandlePolyLineShape(_Stream: TStream; const _RecordHeader: TShpRecordHeader);
  var
    Box: TShpBoundingBox;
    Parts: TShpPartArr;
  begin
    ReadMultiPartShape(_Stream, _RecordHeader, Box, Parts);
    AssertStreamPosition(_Stream.Position, StreamPos, _RecordHeader.ContentLength);
    doOnPolyLineShape(_RecordHeader.RecordNo, Box, Parts);
  end;

  procedure HandlePolyLineMShape(_Stream: TStream; const _RecordHeader: TShpRecordHeader);
  var
    Box: TShpBoundingBoxM;
    Parts: TShpPartMArr;
  begin
    ReadMultiPartMShape(_Stream, _RecordHeader, Box, Parts);
    AssertStreamPosition(_Stream.Position, StreamPos, _RecordHeader.ContentLength);
    doOnPolyLineMShape(_RecordHeader.RecordNo, Box, Parts);
  end;

  procedure HandlePolyLineZShape(_Stream: TStream; const _RecordHeader: TShpRecordHeader);
  var
    Box: TShpBoundingBoxZ;
    Parts: TShpPartZArr;
  begin
    ReadMultiPartZShape(_Stream, _RecordHeader, Box, Parts);
    AssertStreamPosition(_Stream.Position, StreamPos, _RecordHeader.ContentLength);
    doOnPolyLineZShape(_RecordHeader.RecordNo, Box, Parts);
  end;

  procedure HandlePolygonShape(_Stream: TStream; const _RecordHeader: TShpRecordHeader);
  var
    Box: TShpBoundingBox;
    Parts: TShpPartArr;
  begin
    ReadMultiPartShape(_Stream, _RecordHeader, Box, Parts);
    AssertStreamPosition(_Stream.Position, StreamPos, _RecordHeader.ContentLength);
    doOnPolygonShape(_RecordHeader.RecordNo, Box, Parts);
  end;

  procedure HandlePolygonMShape(_Stream: TStream; const _RecordHeader: TShpRecordHeader);
  var
    Box: TShpBoundingBoxM;
    Parts: TShpPartMArr;
  begin
    ReadMultiPartMShape(_Stream, _RecordHeader, Box, Parts);
    AssertStreamPosition(_Stream.Position, StreamPos, _RecordHeader.ContentLength);
    doOnPolygonMShape(_RecordHeader.RecordNo, Box, Parts);
  end;

  procedure HandlePolygonZShape(_Stream: TStream; const _RecordHeader: TShpRecordHeader);
  var
    Box: TShpBoundingBoxZ;
    Parts: TShpPartZArr;
  begin
    ReadMultiPartZShape(_Stream, _RecordHeader, Box, Parts);
    AssertStreamPosition(_Stream.Position, StreamPos, _RecordHeader.ContentLength);
    doOnPolygonZShape(_RecordHeader.RecordNo, Box, Parts);
  end;

  procedure HandleMultiPoint(_Stream: TStream; const _RecordHeader: TShpRecordHeader);
  var
    Box: TShpBoundingBox;
    Points: TShpPointArr;
  begin
    ReadMultiPointShape(_Stream, _RecordHeader, Box, Points);
    AssertStreamPosition(_Stream.Position, StreamPos, _RecordHeader.ContentLength);
    doOnMultiPointShape(_RecordHeader.RecordNo, Box, Points);
  end;

  procedure HandleMultiPointM(_Stream: TStream; const _RecordHeader: TShpRecordHeader);
  var
    Box: TShpBoundingBoxM;
    Points: TShpPointMArr;
  begin
    ReadMultiPointMShape(_Stream, _RecordHeader, Box, Points);
    AssertStreamPosition(_Stream.Position, StreamPos, _RecordHeader.ContentLength);
    doOnMultiPointMShape(_RecordHeader.RecordNo, Box, Points);
  end;

  procedure HandleMultiPointZ(_Stream: TStream; const _RecordHeader: TShpRecordHeader);
  var
    BoxZ: TShpBoundingBoxZ;
    PointsZ: TShpPointZArr;
  begin
    ReadMultiPointZShape(_Stream, _RecordHeader, BoxZ, PointsZ);
    AssertStreamPosition(_Stream.Position, StreamPos, _RecordHeader.ContentLength);
    doOnMultiPointZShape(_RecordHeader.RecordNo, BoxZ, PointsZ);
  end;

var
  Stream: TdzFile;
  FileHeader: TShpMainFileHeader;
  Len: Int64;
  RecordHeader: TShpRecordHeader;
  ShapeType: LongWord;
begin
  Stream := TdzFile.Create(_Filename);
  try
    Stream.OpenReadonly;
    Stream.ReadBuffer(FileHeader, SizeOf(FileHeader));
    doOnFileHeader(FileHeader.FileCode, FileHeader.FileLength,
      FileHeader.Version, FileHeader.ShapeType,
      FileHeader.XMin, FileHeader.YMin, FileHeader.ZMin, FileHeader.MMin,
      FileHeader.XMax, FileHeader.YMax, FileHeader.ZMax, FileHeader.MMax);
    Len := FileHeader.FileLength;
    if Stream.Size <> Len then
      raise Exception.CreateFmt(_('Length of .shp file (%d) differs from file size (%d).'),
        [Stream.Size, Len]);
    while not Stream.EOF do begin
      Stream.ReadBuffer(RecordHeader, SizeOf(RecordHeader));
      doOnRecordHeader(RecordHeader.RecordNo, RecordHeader.ContentLength);
      StreamPos := Stream.Position;
      Stream.ReadBuffer(ShapeType, SizeOf(ShapeType));
      case TShapeTypeEnum(ShapeType) of
        stNullShape:
          HandleNullShape(Stream, RecordHeader);
        stPoint:
          HandlePointShape(Stream, RecordHeader);
        stPointM:
          HandlePointMShape(Stream, RecordHeader);
        stPointZ:
          HandlePointZShape(Stream, RecordHeader);
        stPolyLine:
          HandlePolyLineShape(Stream, RecordHeader);
        stPolyLineM:
          HandlePolyLineMShape(Stream, RecordHeader);
        stPolyLineZ:
          HandlePolyLineZShape(Stream, RecordHeader);
        stPolygon:
          HandlePolygonShape(Stream, RecordHeader);
        stPolygonM:
          HandlePolygonMShape(Stream, RecordHeader);
        stPolygonZ:
          HandlePolygonZShape(Stream, RecordHeader);
        stMultiPoint:
          HandleMultiPoint(Stream, RecordHeader);
        stMultiPointM:
          HandleMultiPointM(Stream, RecordHeader);
        stMultiPointZ:
          HandleMultiPointZ(Stream, RecordHeader);
      else
        Stream.Seek(RecordHeader.ContentLength - SizeOf(ShapeType), soCurrent);
      end;
    end;
  finally
    FreeAndNil(Stream);
  end;
end;

class function TShapeFileReader.ShapeTypeToString(_ShapeType: TShapeTypeEnum): string;
begin
  case _ShapeType of
    stNullShape: Result := 'Null Shape';
    stPoint: Result := 'Point';
    stPolyLine: Result := 'PolyLine';
    stPolygon: Result := 'Polygon';
    stMultiPoint: Result := 'MultiPoint';
    stPointZ: Result := 'PointZ';
    stPolyLineZ: Result := 'PolyLineZ';
    stPolygonZ: Result := 'PolygonZ';
    stMultiPointZ: Result := 'MultiPointZ';
    stPointM: Result := 'PointM';
    stPolyLineM: Result := 'PolyLineM';
    stPolygonM: Result := 'PolygonM';
    stMultiPointM: Result := 'MultiPointM';
    stMultiPatch: Result := 'MultiPatch';
  else
    raise Exception.CreateFmt(_('%d is not a valid ShapeType value.'), [Ord(_ShapeType)]);
  end;
end;

{ TShapeFileReader.TShpMainFileHeader }

function TShapeFileReader.TShpMainFileHeader.FileCode: LongWord;
begin
  Result := Swap32(FFileCode);
end;

function TShapeFileReader.TShpMainFileHeader.FileLength: Int64;
begin
  Result := 2 * Swap32(FFileLength);
end;

function TShapeFileReader.TShpMainFileHeader.MMax: TNullableDouble;
begin
  Result := TShapeFileReader.MValueToNullableDouble(FMMax);
end;

function TShapeFileReader.TShpMainFileHeader.MMin: TNullableDouble;
begin
  Result := TShapeFileReader.MValueToNullableDouble(FMMin);
end;

function TShapeFileReader.TShpMainFileHeader.ShapeType: TShapeTypeEnum;
begin
  Result := TShapeTypeEnum(FShapeType);
end;

{ TShapeFileReader.TShpRecordHeader }

function TShapeFileReader.TShpRecordHeader.ContentLength: Int64;
begin
  Result := 2 * Swap32(FContentLength);
end;

function TShapeFileReader.TShpRecordHeader.RecordNo: LongWord;
begin
  Result := Swap32(FRecordNo);
end;

{ TShapeFileReader.TShpPointM }

function TShapeFileReader.TShpPointM.M: TNullableDouble;
begin
  Result := TShapeFileReader.MValueToNullableDouble(FM);
end;

{ TShapeFileReader.TShpPointZ }

function TShapeFileReader.TShpPointZ.M: TNullableDouble;
begin
  Result := TShapeFileReader.MValueToNullableDouble(FM);
end;

{ TShapeFileReader.TShpBoundingBoxM }

function TShapeFileReader.TShpBoundingBoxM.MMax: TNullableDouble;
begin
  Result := TShapeFileReader.MValueToNullableDouble(FMMax);
end;

function TShapeFileReader.TShpBoundingBoxM.MMin: TNullableDouble;
begin
  Result := TShapeFileReader.MValueToNullableDouble(FMMin);
end;

{ TShapeFileReader.TShpBoundingBoxZ }

function TShapeFileReader.TShpBoundingBoxZ.MMax: TNullableDouble;
begin
  Result := TShapeFileReader.MValueToNullableDouble(FMMax);
end;

function TShapeFileReader.TShpBoundingBoxZ.MMin: TNullableDouble;
begin
  Result := TShapeFileReader.MValueToNullableDouble(FMMin);
end;

initialization
  Assert(SizeOf(TShapeFileReader.TShpMainFileHeader) = 100);
  Assert(SizeOf(TShapeFileReader.TShpRecordHeader) = 8);
{$ENDIF DELPHI2007_UP}
end.

