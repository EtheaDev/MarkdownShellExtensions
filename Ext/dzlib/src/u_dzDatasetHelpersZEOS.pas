unit u_dzDatasetHelpersZEOS;

interface

uses
  SysUtils,
  Classes,
  DB,
  AdoDb,
  ZAbstractConnection,
  ZAbstractRODataset,
  ZAbstractDataset,
  ZDataset,
  u_dzTranslator,
  u_dzDatasetHelpers;

type
  TDataSetHelperZEOS = class(TDatasetHelper)
  protected
    procedure SetParamByName(const _Param: string; _Value: variant); override;
    function TrySetParamByName(const _Param: string; _Value: variant): Boolean; override;
  public
    ///<summary> creates a TDatasetHelper for accessing a TAdoTable or TAdoQuery </summary>
    constructor Create(_Table: TZTable); overload;
    ///<summary> creates a TDatasetHelper for accessing a query
    ///          @param Query is the TAdoQuery to access
    ///          @param Tablename is the table name to use for automatically
    ///                           generated error messages </summary>
    constructor Create(_Query: TZQuery; const _Tablename: string); overload;
    constructor Create(_Dataset: TDataSet; const _TableName: string); overload;
  end;

implementation

constructor TDataSetHelperZEOS.Create(_Table: TZTable);
begin
  inherited Create(_Table, _Table.TableName);
end;

constructor TDataSetHelperZEOS.Create(_Query: TZQuery; const _Tablename: string);
begin
  inherited Create(_Query, _Tablename);
end;

constructor TDataSetHelperZEOS.Create(_Dataset: TDataset; const _TableName: string);
begin
  inherited Create(_Dataset, _TableName);
end;

procedure TDataSetHelperZEOS.SetParamByName(const _Param: string; _Value: variant);
var
  i: Integer;
  ZQuery: TZQuery;
begin
  // Do not use ParamByName -> only works if param is unique
  if FDataset is TZQuery then begin
    ZQuery := TZQuery(FDataset);
    for i := 0 to ZQuery.Params.Count - 1 do begin
      if SameText(ZQuery.Params[i].Name, _Param) then
        ZQuery.Params[i].Value := _Value;
    end;
  end else
    raise Exception.CreateFmt(_('SetParamByName is not supported for a %s (only TADODataSet descendants).'), [FDataset.ClassName]);
end;

function TDataSetHelperZEOS.TrySetParamByName(const _Param: string; _Value: variant): Boolean;
var
  ZQuery: TZQuery;
  i: Integer;
begin
  Result := False;
  if FDataset is TZQuery then begin
    ZQuery := TZQuery(FDataset);
    for i := 0 to ZQuery.Params.Count - 1 do begin
      if SameText(ZQuery.Params[i].Name, _Param) then begin
        ZQuery.Params[i].Value := _Value;
        Result := True;
      end;
    end
  end else
    raise Exception.CreateFmt(_('TrySetParamByName is not supported for a %s (only TADODataSet descendants).'), [FDataset.ClassName]);
end;

end.

