unit u_dzDatasetHelpersSDAC;

interface

uses
  SysUtils,
  Classes,
  DB,
  AdoDb,
  DBAccess,
  MSAccess,
  MemDS,
  u_dzTranslator,
  u_dzDatasetHelpers;

type
  TDataSetHelperSDAC = class(TDatasetHelper)
  protected
    procedure SetParamByName(const _Param: string; _Value: variant); override;
    function TrySetParamByName(const _Param: string; _Value: variant): Boolean; override;
  public
    ///<summary> creates a TDatasetHelper for accessing a TAdoTable or TAdoQuery </summary>
    constructor Create(_Table: TMSTable); overload;
    ///<summary> creates a TDatasetHelper for accessing a query
    ///          @param Query is the TAdoQuery to access
    ///          @param Tablename is the table name to use for automatically
    ///                           generated error messages </summary>
    constructor Create(_Query: TMSQuery; const _Tablename: string); overload;
    constructor Create(_Dataset: TDataSet; const _TableName: string); overload;
  end;

implementation

constructor TDataSetHelperSDAC.Create(_Table: TMSTable);
begin
  inherited Create(_Table, _Table.TableName);
end;

constructor TDataSetHelperSDAC.Create(_Query: TMSQuery; const _Tablename: string);
begin
  inherited Create(_Query, _Tablename);
end;

constructor TDataSetHelperSDAC.Create(_Dataset: TDataset; const _TableName: string);
begin
  inherited Create(_Dataset, _TableName);
end;

procedure TDataSetHelperSDAC.SetParamByName(const _Param: string; _Value: variant);
var
  i: Integer;
  ADQuery: TMSQuery;
begin
  // Do not use ParamByName -> only works if param is unique
  if FDataset is TMSQuery then begin
    ADQuery := TMSQuery(FDataset);
    for i := 0 to ADQuery.Params.Count - 1 do begin
      if SameText(ADQuery.Params[i].Name, _Param) then
        ADQuery.Params[i].Value := _Value;
    end;
  end else
    raise Exception.CreateFmt(_('SetParamByName is not supported for a %s (only TMSQuery descendants).'), [FDataset.ClassName]);
end;

function TDataSetHelperSDAC.TrySetParamByName(const _Param: string; _Value: variant): Boolean;
var
  ADQuery: TMSQuery;
  i: Integer;
begin
  Result := False;
  if FDataset is TMSQuery then begin
    ADQuery := TMSQuery(FDataset);
    for i := 0 to ADQuery.Params.Count - 1 do begin
      if SameText(ADQuery.Params[i].Name, _Param) then begin
        ADQuery.Params[i].Value := _Value;
        Result := True;
      end;
    end
  end else
    raise Exception.CreateFmt(_('TrySetParamByName is not supported for a %s (only TMSQuery descendants).'), [FDataset.ClassName]);
end;

end.

