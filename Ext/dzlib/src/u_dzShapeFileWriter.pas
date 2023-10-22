unit u_dzShapeFileWriter;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  Windows,
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzShapeFileConsts,
  u_dzFileStreams;

type
  TShapeFileWriter = class
  protected
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
        MMin: Double; // Little Endian
        MMax: Double; // Little Endian
      end;
      TShpRecordHeader = packed record
        ///<sumamry> Record number in Big Endian format, corresponds to record number in dbf file </summary>
        FRecordNo: LongWord; // Big Endian
        ///<summary> Length of record content in Big Endian format in 16 bit words,
        ///          excluding the header size</summary>
        FContentLength: LongWord; // Big Endian
      end;
      // The .shx file consists of these records
      TShpIndexRecord = packed record
        Offset: LongWord; // Big Endian
        ContentLength: LongWord; // Big Endian
      end;
      TBoundingBox = record
        XMin: Double;
        YMin: Double;
        XMax: Double;
        YMax: Double;
        ZMin: Double;
        ZMax: Double;
        MMin: Double;
        MMax: Double;
        procedure SetXY(_XMin, _YMin: Double; _XMax, _YMax: Double);
        procedure SetXYZ(_XMin, _YMin, _ZMin: Double; _XMax, _YMax, _ZMax: Double);
        procedure SetXYM(_XMin, _YMin, _MMin: Double; _XMax, _YMax, _MMax: Double);
      end;
  private
    procedure OpenStream(const _Filename: string; var _Stream: TdzFile);
  protected
    FShapeType: TShapeTypeEnum;
    FVersion: Integer;
    FBoundingBox: TBoundingBox;
    FHasBoundingBoxBeenSet: Boolean;
    procedure WriteMainHeader(_Stream: TStream; _FileLengthInBytes: Int64);
  public
    constructor Create(_ShapeType: TShapeTypeEnum; _Version: Integer = 1000);
    procedure CalcBoundingBox; virtual; abstract;
    procedure WriteShpToFile(const _Filename: string); virtual;
    procedure WriteShpToStream(_Stream: TStream); virtual; abstract;
    procedure WriteShxToFile(const _Filename: string); virtual;
    procedure WriteShxToStream(_Stream: TStream); virtual; abstract;
    procedure WriteShpAndShx(const _BaseFilename: string); virtual;
  end;

  TShapeFileWriterPoint = class(TShapeFileWriter)
  private
    type
      TShpPointEx = record
        RecordNo: LongWord;
        Point: TShpPoint;
      end;
  private
    FPoints: array of TShpPointEx;
    FCount: Integer;
  public
    constructor Create;
    procedure SetBoundingBox(_XMin, _YMin: Double; _XMax, _YMax: Double);
    procedure CalcBoundingBox; override;
    procedure AddPoint(_x, _y: Double; _RecNo: LongWord);
    procedure WriteShpToStream(_Stream: TStream); override;
    procedure WriteShxToStream(_Stream: TStream); override;
  end;

  TShapeFileWriterPolyLine = class(TShapeFileWriter)
  private
    type
      TShpPolyLineEx = record
        RecnoPt: LongWord;
        Point: TShpPoint;
      end;
    type
      TPolyLine = record
        CtPt: LongWord;
        PLine: array of TShpPolyLineEx;
      end;
  private
    FPLine: array of TPolyLine;
    FCount: Integer;
  public
    constructor Create;
    procedure SetBoundingBox(_XMin, _YMin: Double; _XMax, _YMax: Double);
    procedure CalcBoundingBox; override;
    procedure AddPolyPoint(_RecnoPL, _RecnoPt, _PlEnd: LongWord; _x, _y: Double);
    procedure WriteShpToStream(_Stream: TStream); override;
    procedure WriteShxToStream(_Stream: TStream); override;
  end;

  TShapeFileWriterPolygon = class(TShapeFileWriter)
  private
    type
      TShpPolygonEx = record
        RecnoPt: LongWord;
        Point: TShpPoint;
      end;
    type
      TPolygon = record
        CtPt: LongWord;
        PGon: array of TShpPolygonEx;
      end;
  private
    FPGon: array of TPolygon;
    FCount: Integer;
  public
    constructor Create;
    procedure SetBoundingBox(_XMin, _YMin: Double; _XMax, _YMax: Double);
    procedure CalcBoundingBox; override;
    procedure AddPolyPoint(_RecnoPL, _RecnoPt, _PlEnd: LongWord; _x, _y: Double);
    procedure WriteShpToStream(_Stream: TStream); override;
    procedure WriteShxToStream(_Stream: TStream); override;
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  u_dzFileUtils,
  u_dzConvertUtils;

{ TShapeFileWriterPoint }

constructor TShapeFileWriterPoint.Create;
begin
  inherited Create(stPoint);
  SetLength(FPoints, 100);
end;

procedure TShapeFileWriterPoint.AddPoint(_x, _y: Double; _RecNo: LongWord);
begin
  if FCount >= Length(FPoints) then
    SetLength(FPoints, FCount + FCount div 2);
  FPoints[FCount].Point.X := _x;
  FPoints[FCount].Point.Y := _y;
  FPoints[FCount].RecordNo := _RecNo;
  Inc(FCount);
end;

procedure TShapeFileWriterPoint.CalcBoundingBox;
var
  Min: TShpPoint;
  Max: TShpPoint;
  i: Integer;
  Pnt: PShpPoint;
begin
  if FCount = 0 then
    raise Exception.Create(_('Cannot calc a bounding box for 0 points.'));
  Min := FPoints[0].Point;
  Max := Min;
  for i := 1 to FCount - 1 do begin
    Pnt := @FPoints[i].Point;
    if Pnt.X > Max.X then
      Max.X := Pnt.X;
    if Pnt.Y > Max.Y then
      Max.Y := Pnt.Y;
    if Pnt.X < Min.X then
      Min.X := Pnt.X;
    if Pnt.Y < Min.Y then
      Min.Y := Pnt.Y;
  end;
  SetBoundingBox(Min.X, Min.Y, Max.X, Max.Y);
end;

procedure TShapeFileWriterPoint.SetBoundingBox(_XMin, _YMin, _XMax, _YMax: Double);
begin
  FBoundingBox.SetXY(_XMin, _YMin, _XMax, _YMax);
  FHasBoundingBoxBeenSet := True;
end;

procedure TShapeFileWriterPoint.WriteShpToStream(_Stream: TStream);
var
  ContentLength: Integer;
  Len: Int64;
  i: Integer;
  RecHeader: TShpRecordHeader;
  ShapeType: LongWord;
begin
  if not FHasBoundingBoxBeenSet then
    CalcBoundingBox;
  // The record header does not count towards the record size, only the content which consists
  // of the shape type (4 bytes) and two coordinates 2x 8 bytes.
  // So record size for a point shape is 20 bytes = 10 words.
  ContentLength := SizeOf(Integer) + SizeOf(TShpPoint);
  // the file size on the other hand includes the record headers
  Len := SizeOf(TShpMainFileHeader) + FCount * ContentLength + FCount * SizeOf(TShpRecordHeader);
  WriteMainHeader(_Stream, Len);
  // ContentLength is constant, so we set it only once, it is given in 16 bit words, thus the "div 2".
  RecHeader.FContentLength := Swap32(ContentLength div 2);
  ShapeType := Ord(FShapeType);
  for i := 0 to FCount - 1 do begin
    RecHeader.FRecordNo := Swap32(FPoints[i].RecordNo);
    _Stream.WriteBuffer(RecHeader, SizeOf(RecHeader));
    _Stream.WriteBuffer(ShapeType, SizeOf(ShapeType));
    _Stream.WriteBuffer(FPoints[i].Point, SizeOf(FPoints[i].Point));
  end;
end;

procedure TShapeFileWriterPoint.WriteShxToStream(_Stream: TStream);
var
  RecordSize: Integer;
  Len: Int64;
  i: Integer;
  IdxRec: TShpIndexRecord;
  ContentLength: LongWord;
  Offset: Integer;
begin
  if not FHasBoundingBoxBeenSet then
    CalcBoundingBox;
  // one record in the .shp file consists of
  // * the record header
  // * the shape type
  // * the point coordinates
  ContentLength := SizeOf(Integer) + SizeOf(TShpPoint);
  RecordSize := SizeOf(TShpRecordHeader) + ContentLength;
  Len := SizeOf(TShpMainFileHeader) + FCount * SizeOf(TShpIndexRecord);
  WriteMainHeader(_Stream, Len);
  // the records in the .shp file start after the main file header
  Offset := SizeOf(TShpMainFileHeader);
  // ContentLength is constant, so we set it only once, it is given in 16 bit words, thus the "div 2".
  IdxRec.ContentLength := Swap32(ContentLength div 2);
  for i := 0 to FCount - 1 do begin
    IdxRec.Offset := Swap32((Offset + i * RecordSize) div 2);
    _Stream.WriteBuffer(IdxRec, SizeOf(IdxRec));
  end;
end;

{TShapeFileWriterPolyLine}
constructor TShapeFileWriterPolyLine.Create;
begin
  inherited Create(stPolyLine);
  SetLength(FPLine, 1);
  SetLength(FPLine[0].PLine, 100);
end;

procedure TShapeFileWriterPolyLine.AddPolyPoint(_RecnoPL, _RecnoPt, _PlEnd: LongWord; _x, _y: Double);
begin
  if _RecnoPL > High(FPLine) then begin
    SetLength(FPLine, _RecnoPL);
    FPLine[_RecnoPL - 1].CtPt := _PlEnd;
    SetLength(FPLine[_RecnoPL - 1].PLine, _RecnoPt);
  end else
    SetLength(FPLine[_RecnoPL - 1].PLine, _RecnoPt);
  FPLine[_RecnoPL - 1].PLine[_RecnoPt - 1].Point.X := _x;
  FPLine[_RecnoPL - 1].PLine[_RecnoPt - 1].Point.Y := _y;
  Inc(FCount);
end;

procedure TShapeFileWriterPolyLine.CalcBoundingBox;
var
  Min: TShpPoint;
  Max: TShpPoint;
  i, j: Integer;
  Pnt: PShpPoint;
begin
  if FCount = 0 then
    raise Exception.Create(_('Cannot calc a bounding box for 0 points.'));
  Min := FPLine[0].PLine[0].Point;
  Max := Min;
  for j := 0 to High(FPLine) do begin
    for i := 0 to High(FPLine[j].PLine) do begin
      Pnt := @FPLine[j].PLine[i].Point;
      if Pnt.X > Max.X then
        Max.X := Pnt.X;
      if Pnt.Y > Max.Y then
        Max.Y := Pnt.Y;
      if Pnt.X < Min.X then
        Min.X := Pnt.X;
      if Pnt.Y < Min.Y then
        Min.Y := Pnt.Y;
    end;
  end;
  SetBoundingBox(Min.X, Min.Y, Max.X, Max.Y);
end;

procedure TShapeFileWriterPolyLine.SetBoundingBox(_XMin, _YMin, _XMax, _YMax: Double);
begin
  FBoundingBox.SetXY(_XMin, _YMin, _XMax, _YMax);
  FHasBoundingBoxBeenSet := True;
end;

procedure TShapeFileWriterPolyLine.WriteShpToStream(_Stream: TStream);
var
  ContentLength: Integer;
  Len: Int64;
  i, j, ij, k: Integer;
  RecHeader: TShpRecordHeader;
  ShapeType: LongWord;
  BMin: TShpPoint;
  BMax: TShpPoint;
  Pnt: PShpPoint;

  procedure CalcPLineBoundingBox(intPLine: Integer);
  var
    a: Integer;
  begin
    if FCount = 0 then
      raise Exception.Create(_('Cannot calc a bounding box for 0 points.'));
    BMin := FPLine[intPLine].PLine[0].Point;
    BMax := BMin;
    for a := 0 to High(FPLine[intPLine].PLine) do begin
      Pnt := @FPLine[intPLine].PLine[a].Point;
      if Pnt.X > BMax.X then
        BMax.X := Pnt.X;
      if Pnt.Y > BMax.Y then
        BMax.Y := Pnt.Y;
      if Pnt.X < BMin.X then
        BMin.X := Pnt.X;
      if Pnt.Y < BMin.Y then
        BMin.Y := Pnt.Y;
    end;
  end;

begin
  if not FHasBoundingBoxBeenSet then
    CalcBoundingBox;
  // The record header does not count towards the record size, only the content which consists
  // of the shape type (4 bytes) and two coordinates 2x 8 bytes.
  // So record size for a point shape is 20 bytes = 10 words.

  // the file size on the other hand includes the record headers

  //  Position  Field       Value     Type      Number    Order
  //  Byte 0    Shape Type  3         Integer   1         Little
  //  Byte 4    Box         Box       Double    4         Little
  //  Byte 36   NumParts    NumParts  Integer   1         Little
  //  Byte 40   NumPoints   NumPoints Integer   1         Little
  //  Byte 44   Parts       Parts     Integer   NumParts  Little
  //  Byte X    Points      Points    Point     NumPoints Little
  //  Note:  X = 44 + 4 * NumParts

  // Hinweis: NumParts immer 1 (hab' keine Multiparts)
  //ToDo: Für NumParts größer 1 muss Parts auch als array übergeben werden
  // analog Polygon

  Len := 0;
  ij := 1;
  k := 0;
  for i := 0 to High(FPLine) do
    Len := Len + SizeOf(Integer) + 4 * SizeOf(Double) + 3 * SizeOf(Integer) + (High(FPLine[i].PLine) + 1) * SizeOf(TShpPoint) + SizeOf(TShpRecordHeader);
  Len := Len + SizeOf(TShpMainFileHeader);
  WriteMainHeader(_Stream, Len);
  // ContentLength is constant, so we set it only once, it is given in 16 bit words, thus the "div 2".

  ShapeType := Ord(FShapeType);
  for i := 0 to High(FPLine) do begin
    ContentLength := SizeOf(Integer) + 4 * SizeOf(Double) + 3 * SizeOf(Integer) + (High(FPLine[i].PLine) + 1) * SizeOf(TShpPoint);
    RecHeader.FContentLength := Swap32(ContentLength div 2);
    CalcPLineBoundingBox(i);
    RecHeader.FRecordNo := Swap32(i + 1);
    _Stream.WriteBuffer(RecHeader, SizeOf(RecHeader));
    _Stream.WriteBuffer(ShapeType, SizeOf(ShapeType));
    _Stream.WriteBuffer(BMin.X, SizeOf(BMin.X));
    _Stream.WriteBuffer(BMin.Y, SizeOf(BMin.Y));
    _Stream.WriteBuffer(BMax.X, SizeOf(BMax.X));
    _Stream.WriteBuffer(BMax.Y, SizeOf(BMax.Y));
    _Stream.WriteBuffer(ij, SizeOf(LongWord)); // NumParts - immer ein part, sonst wird "begin of part" zum array
    _Stream.WriteBuffer(FPLine[i].CtPt, SizeOf(LongWord)); // NumPoints
    _Stream.WriteBuffer(k, SizeOf(LongWord)); // begin of part
    for j := 0 to High(FPLine[i].PLine) do
      _Stream.WriteBuffer(FPLine[i].PLine[j].Point, SizeOf(FPLine[i].PLine[j].Point));
  end;
end;

procedure TShapeFileWriterPolyLine.WriteShxToStream(_Stream: TStream);
var
  RecordSize: Integer;
  Len: Int64;
  i: Integer;
  IdxRec: TShpIndexRecord;
  ContentLength: LongWord;
  Offset: Integer;
begin
  if not FHasBoundingBoxBeenSet then
    CalcBoundingBox;
  // one record in the .shp file consists of
  // * the record header
  // * the shape type
  // * the point coordinates
  Len := SizeOf(TShpMainFileHeader) + (High(FPLine) + 1) * SizeOf(TShpIndexRecord);
  WriteMainHeader(_Stream, Len);
  // the records in the .shp file start after the main file header
  Offset := SizeOf(TShpMainFileHeader);
  // ContentLength is constant, so we set it only once, it is given in 16 bit words, thus the "div 2".
  for i := 0 to High(FPLine) do begin
    ContentLength := SizeOf(Integer) + 4 * SizeOf(Double) + 3 * SizeOf(Integer) + (High(FPLine[i].PLine) + 1) * SizeOf(TShpPoint);
    IdxRec.ContentLength := Swap32(ContentLength div 2);
    IdxRec.Offset := Swap32((Offset) div 2);
    RecordSize := SizeOf(TShpRecordHeader) + ContentLength;
    Offset := Offset + RecordSize;
    _Stream.WriteBuffer(IdxRec, SizeOf(IdxRec));
  end;
end;

{TShapeFileWriterPolygon}
constructor TShapeFileWriterPolygon.Create;
begin
  inherited Create(stPolygon);
  SetLength(FPGon, 1);
  SetLength(FPGon[0].PGon, 100);
end;

procedure TShapeFileWriterPolygon.AddPolyPoint(_RecnoPL, _RecnoPt, _PlEnd: LongWord; _x, _y: Double);
begin
  if _RecnoPL > High(FPGon) then begin
    SetLength(FPGon, _RecnoPL);
    FPGon[_RecnoPL - 1].CtPt := _PlEnd;
    SetLength(FPGon[_RecnoPL - 1].PGon, _RecnoPt);
  end else
    SetLength(FPGon[_RecnoPL - 1].PGon, _RecnoPt);
  FPGon[_RecnoPL - 1].PGon[_RecnoPt - 1].Point.X := _x;
  FPGon[_RecnoPL - 1].PGon[_RecnoPt - 1].Point.Y := _y;
  Inc(FCount);
end;

procedure TShapeFileWriterPolygon.CalcBoundingBox;
var
  Min: TShpPoint;
  Max: TShpPoint;
  i, j: Integer;
  Pnt: PShpPoint;
begin
  if FCount = 0 then
    raise Exception.Create(_('Cannot calc a bounding box for 0 points.'));
  Min := FPGon[0].PGon[0].Point;
  Max := Min;
  for j := 0 to High(FPGon) do begin
    for i := 0 to High(FPGon[j].PGon) do begin
      Pnt := @FPGon[j].PGon[i].Point;
      if Pnt.X > Max.X then
        Max.X := Pnt.X;
      if Pnt.Y > Max.Y then
        Max.Y := Pnt.Y;
      if Pnt.X < Min.X then
        Min.X := Pnt.X;
      if Pnt.Y < Min.Y then
        Min.Y := Pnt.Y;
    end;
  end;
  SetBoundingBox(Min.X, Min.Y, Max.X, Max.Y);
end;

procedure TShapeFileWriterPolygon.SetBoundingBox(_XMin, _YMin, _XMax, _YMax: Double);
begin
  FBoundingBox.SetXY(_XMin, _YMin, _XMax, _YMax);
  FHasBoundingBoxBeenSet := True;
end;

procedure TShapeFileWriterPolygon.WriteShpToStream(_Stream: TStream);
var
  ContentLength: Integer;
  Len: Int64;
  i, j, ij, k: Integer;
  RecHeader: TShpRecordHeader;
  ShapeType: LongWord;
  BMin: TShpPoint;
  BMax: TShpPoint;
  Pnt: PShpPoint;

  procedure CalcPLineBoundingBox(intPGon: Integer);
  var
    a: Integer;
  begin
    if FCount = 0 then
      raise Exception.Create(_('Cannot calc a bounding box for 0 points.'));
    BMin := FPGon[intPGon].PGon[0].Point;
    BMax := BMin;
    for a := 0 to High(FPGon[intPGon].PGon) do begin
      Pnt := @FPGon[intPGon].PGon[a].Point;
      if Pnt.X > BMax.X then
        BMax.X := Pnt.X;
      if Pnt.Y > BMax.Y then
        BMax.Y := Pnt.Y;
      if Pnt.X < BMin.X then
        BMin.X := Pnt.X;
      if Pnt.Y < BMin.Y then
        BMin.Y := Pnt.Y;
    end;
  end;

begin
  if not FHasBoundingBoxBeenSet then
    CalcBoundingBox;
  // The record header does not count towards the record size, only the content which consists
  // of the shape type (4 bytes) and two coordinates 2x 8 bytes.
  // So record size for a point shape is 20 bytes = 10 words.

  // the file size on the other hand includes the record headers

  //  Position  Field       Value     Type      Number    Order
  //  Byte 0    Shape Type  5         Integer   1         Little
  //  Byte 4    Box         Box       Double    4         Little
  //  Byte 36   NumParts    NumParts  Integer   1         Little
  //  Byte 40   NumPoints   NumPoints Integer   1         Little
  //  Byte 44   Parts       Parts     Integer   NumParts  Little
  //  Byte X    Points      Points    Point     NumPoints Little
  //  Note:  X = 44 + 4 * NumParts

  // Hinweis: NumParts immer 1 (hab' keine Multiparts)
  // ToDo: Für NumParts größer 1 muss Parts auch als array übergeben werden
  // analog Polyline

  Len := 0;
  ij := 1;
  k := 0;
  for i := 0 to High(FPGon) do
    Len := Len + SizeOf(Integer) + 4 * SizeOf(Double) + 3 * SizeOf(Integer) + (High(FPGon[i].PGon) + 1) * SizeOf(TShpPoint) + SizeOf(TShpRecordHeader);
  Len := Len + SizeOf(TShpMainFileHeader);
  WriteMainHeader(_Stream, Len);
  // ContentLength is constant, so we set it only once, it is given in 16 bit words, thus the "div 2".

  ShapeType := Ord(FShapeType);
  for i := 0 to High(FPGon) do begin
    ContentLength := SizeOf(Integer) + 4 * SizeOf(Double) + 3 * SizeOf(Integer) + (High(FPGon[i].PGon) + 1) * SizeOf(TShpPoint);
    RecHeader.FContentLength := Swap32(ContentLength div 2);
    CalcPLineBoundingBox(i);
    RecHeader.FRecordNo := Swap32(i + 1);
    _Stream.WriteBuffer(RecHeader, SizeOf(RecHeader));
    _Stream.WriteBuffer(ShapeType, SizeOf(ShapeType));
    _Stream.WriteBuffer(BMin.X, SizeOf(BMin.X));
    _Stream.WriteBuffer(BMin.Y, SizeOf(BMin.Y));
    _Stream.WriteBuffer(BMax.X, SizeOf(BMax.X));
    _Stream.WriteBuffer(BMax.Y, SizeOf(BMax.Y));
    _Stream.WriteBuffer(ij, SizeOf(LongWord)); // NumParts - immer ein part, sonst wird "begin of part" zum array
    _Stream.WriteBuffer(FPGon[i].CtPt, SizeOf(LongWord)); // NumPoints
    _Stream.WriteBuffer(k, SizeOf(LongWord)); // begin of part
    for j := 0 to High(FPGon[i].PGon) do
      _Stream.WriteBuffer(FPGon[i].PGon[j].Point, SizeOf(FPGon[i].PGon[j].Point));
  end;
end;

procedure TShapeFileWriterPolygon.WriteShxToStream(_Stream: TStream);
var
  RecordSize: Integer;
  Len: Int64;
  i: Integer;
  IdxRec: TShpIndexRecord;
  ContentLength: LongWord;
  Offset: Integer;
begin
  if not FHasBoundingBoxBeenSet then
    CalcBoundingBox;
  // one record in the .shp file consists of
  // * the record header
  // * the shape type
  // * the point coordinates
  Len := SizeOf(TShpMainFileHeader) + (High(FPGon) + 1) * SizeOf(TShpIndexRecord);
  WriteMainHeader(_Stream, Len);
  // the records in the .shp file start after the main file header
  Offset := SizeOf(TShpMainFileHeader);
  // ContentLength is constant, so we set it only once, it is given in 16 bit words, thus the "div 2".
  for i := 0 to High(FPGon) do begin
    ContentLength := SizeOf(Integer) + 4 * SizeOf(Double) + 3 * SizeOf(Integer) + (High(FPGon[i].PGon) + 1) * SizeOf(TShpPoint);
    IdxRec.ContentLength := Swap32(ContentLength div 2);
    IdxRec.Offset := Swap32((Offset) div 2);
    RecordSize := SizeOf(TShpRecordHeader) + ContentLength;
    Offset := Offset + RecordSize;
    _Stream.WriteBuffer(IdxRec, SizeOf(IdxRec));
  end;
end;

{ TShapeFileWriter }

constructor TShapeFileWriter.Create(_ShapeType: TShapeTypeEnum; _Version: Integer = 1000);
begin
  inherited Create;
  FShapeType := _ShapeType;
  FVersion := _Version;
end;

procedure TShapeFileWriter.WriteMainHeader(_Stream: TStream; _FileLengthInBytes: Int64);
var
  MainHeader: TShpMainFileHeader;

  procedure SetBoundingBox;
  begin
    MainHeader.XMin := FBoundingBox.XMin;
    MainHeader.YMin := FBoundingBox.YMin;
    MainHeader.XMax := FBoundingBox.XMax;
    MainHeader.YMax := FBoundingBox.YMax;
    MainHeader.ZMin := FBoundingBox.ZMin;
    MainHeader.ZMax := FBoundingBox.ZMax;
    MainHeader.MMin := FBoundingBox.MMin;
    MainHeader.MMax := FBoundingBox.MMax;
  end;

begin
  if not FHasBoundingBoxBeenSet then
    raise Exception.Create(_('Please set the bounding box first!'));
  SetBoundingBox;
  MainHeader.FFileCode := Swap32(9994);
  ZeroMemory(@MainHeader.Unused, SizeOf(MainHeader.Unused));
  MainHeader.Version := FVersion;
  MainHeader.FShapeType := Ord(FShapeType);
  MainHeader.FFileLength := Swap32(_FileLengthInBytes div 2);
  _Stream.Position := 0;
  _Stream.WriteBuffer(MainHeader, SizeOf(MainHeader));
end;

procedure TShapeFileWriter.OpenStream(const _Filename: string; var _Stream: TdzFile);
begin
  _Stream := TdzFile.Create(_Filename);
  _Stream.ShareMode := fsNoSharing;
  _Stream.CreateDisposition := fcCreateTruncateIfExists;
  _Stream.AccessMode := faReadWrite;
  _Stream.Open;
end;

procedure TShapeFileWriter.WriteShpAndShx(const _BaseFilename: string);
begin
  WriteShpToFile(_BaseFilename + '.shp');
  WriteShxToFile(_BaseFilename + '.shx');
end;

procedure TShapeFileWriter.WriteShpToFile(const _Filename: string);
var
  st: TdzFile;
begin
  st := nil;
  try
    OpenStream(_Filename, st);
    WriteShpToStream(st);
  finally
    FreeAndNil(st);
  end;
end;

procedure TShapeFileWriter.WriteShxToFile(const _Filename: string);
var
  st: TdzFile;
begin
  st := nil;
  try
    OpenStream(_Filename, st);
    WriteShxToStream(st);
  finally
    FreeAndNil(st);
  end;
end;

{ TShapeFileWriter.TBoundingBox }

procedure TShapeFileWriter.TBoundingBox.SetXY(_XMin, _YMin, _XMax, _YMax: Double);
begin
  XMin := _XMin;
  YMin := _YMin;
  XMax := _XMax;
  YMax := _YMax;
  ZMin := 0;
  ZMax := 0;
  MMin := 0;
  MMax := 0;
end;

procedure TShapeFileWriter.TBoundingBox.SetXYM(_XMin, _YMin, _MMin, _XMax, _YMax, _MMax: Double);
begin
  XMin := _XMin;
  YMin := _YMin;
  XMax := _XMax;
  YMax := _YMax;
  ZMin := 0;
  ZMax := 0;
  MMin := _MMin;
  MMax := _MMax;
end;

procedure TShapeFileWriter.TBoundingBox.SetXYZ(_XMin, _YMin, _ZMin, _XMax, _YMax, _ZMax: Double);
begin
  XMin := _XMin;
  YMin := _YMin;
  XMax := _XMax;
  YMax := _YMax;
  ZMin := _ZMin;
  ZMax := _ZMax;
  MMin := 0;
  MMax := 0;
end;

{$ENDIF DELPHI2007_UP}

end.
