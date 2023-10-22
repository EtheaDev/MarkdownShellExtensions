unit u_dzTeeChartUtils;

interface

uses
  Classes,
  Chart,
  TeEngine,
  TeeGDIPlus;

///<summary>
/// Calls BeginUpdate for all series of the chart and returns an interface that
/// calls EndUpdate for them when it goes out of scope.
/// @param Chart is the TChart component to handle </summary>
function TChart_BeginUpdate(_Chart: TChart): IInterface;

///<summary>
/// Creates a TTeeGdiPlus renderer, assigns the chart as its TeePanel sets it Antialias property to false
/// @returns the newly created renderer, can be ignored if not needed otherwise </summary>
function TChart_DisableGdiPlusAntialias(_Chart: TChart): TTeeGdiPlus;

implementation

type
  TChartUpdateInt = class(TInterfacedObject, IInterface)
  private
    FChart: TChart;
  public
    constructor Create(_Chart: TChart);
    destructor Destroy; override;
  end;

{ TChartUpdateInt }

constructor TChartUpdateInt.Create(_Chart: TChart);
var
  i: Integer;
begin
  inherited Create;
  FChart := _Chart;
  for i := 0 to FChart.SeriesCount - 1 do begin
    FChart.Series[i].BeginUpdate;
  end;
end;

destructor TChartUpdateInt.Destroy;
var
  i: Integer;
begin
  for i := 0 to FChart.SeriesCount - 1 do begin
    FChart.Series[i].EndUpdate;
  end;
  inherited;
end;

function TChart_BeginUpdate(_Chart: TChart): IInterface;
begin
  Result := TChartUpdateInt.Create(_Chart);
end;

function TChart_DisableGdiPlusAntialias(_Chart: TChart): TTeeGdiPlus;
begin
  Result := TTeeGdiPlus.Create(_Chart);
  Result.TeePanel := _Chart;
  Result.Antialias := False;
end;

end.
