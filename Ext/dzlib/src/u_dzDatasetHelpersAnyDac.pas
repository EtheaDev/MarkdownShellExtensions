unit u_dzDatasetHelpersAnyDac;

interface

uses
  SysUtils,
  Classes,
  DB,
  AdoDb,
  uADDAptManager,
  uADStanIntf,
  uADStanOption,
  uADStanError,
  uADGUIxIntf,
  uADPhysIntf,
  uADStanDef,
  uADStanPool,
  uADStanAsync,
  uADPhysManager,
  uADCompClient,
  uADPhysODBCBase,
  uADPhysMSSQL,
  uADGUIxFormsWait,
  ZAbstractConnection,
  ZAbstractRODataset,
  ZAbstractDataset,
  ZDataset,
  u_dzTranslator,
  u_dzDatasetHelpers;

type
  TDataSetHelperAnyDAC = class(TDatasetHelper)
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

constructor TDataSetHelperAnyDAC.Create(_Table: TZTable);
begin
  inherited Create(_Table, _Table.TableName);
end;

constructor TDataSetHelperAnyDAC.Create(_Query: TZQuery; const _Tablename: string);
begin
  inherited Create(_Query, _Tablename);
end;

constructor TDataSetHelperAnyDAC.Create(_Dataset: TDataset; const _TableName: string);
begin
  inherited Create(_Dataset, _TableName);
end;

procedure TDataSetHelperAnyDAC.SetParamByName(const _Param: string; _Value: variant);
var
  i: Integer;
  ADQuery: TADQuery;
begin
  // Do not use ParamByName -> only works if param is unique
  if FDataset is TADQuery then begin
    ADQuery := TADQuery(FDataset);
    for i := 0 to ADQuery.Params.Count - 1 do begin
      if SameText(ADQuery.Params[i].Name, _Param) then
        ADQuery.Params[i].Value := _Value;
    end;
  end else
    raise Exception.CreateFmt(_('SetParamByName is not supported for a %s (only TADQuery descendants).'), [FDataset.ClassName]);
end;

function TDataSetHelperAnyDAC.TrySetParamByName(const _Param: string; _Value: variant): Boolean;
var
  ADQuery: TADQuery;
  i: Integer;
begin
  Result := False;
  if FDataset is TADQuery then begin
    ADQuery := TADQuery(FDataset);
    for i := 0 to ADQuery.Params.Count - 1 do begin
      if SameText(ADQuery.Params[i].Name, _Param) then begin
        ADQuery.Params[i].Value := _Value;
        Result := True;
      end;
    end
  end else
    raise Exception.CreateFmt(_('TrySetParamByName is not supported for a %s (only TADQuery descendants).'), [FDataset.ClassName]);
end;

end.

