unit u_dzShapeFileConsts;

interface

uses
  u_dzNullableDouble;

type
  TShapeTypeEnum = (
    stNullShape = 0,
    stPoint = 1,
    stPolyLine = 3,
    stPolygon = 5,
    stMultiPoint = 8,
    stPointZ = 11,
    stPolyLineZ = 13,
    stPolygonZ = 15,
    stMultiPointZ = 18,
    stPointM = 21,
    stPolyLineM = 23,
    stPolygonM = 25,
    stMultiPointM = 28,
    stMultiPatch = 31);

type
  PShpPoint = ^TShpPoint;
  TShpPoint = packed record
    X: Double;
    Y: Double;
  end;

implementation

end.
