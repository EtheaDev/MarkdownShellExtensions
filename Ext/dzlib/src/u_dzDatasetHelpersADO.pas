unit u_dzDatasetHelpersADO;

interface

uses
  SysUtils,
  Classes,
  DB,
  AdoDb,
  u_dzTranslator,
  u_dzDatasetHelpers;

type
  TDataSetHelperADO = class(TDatasetHelper)
  protected
    procedure SetParamByName(const _Param: string; _Value: variant); override;
    function TrySetParamByName(const _Param: string; _Value: variant): Boolean; override;
  public
    ///<summary> creates a TDatasetHelper for accessing a TAdoTable or TAdoQuery </summary>
    constructor Create(_Table: TAdoTable); overload;
    ///<summary> creates a TDatasetHelper for accessing a query
    ///          @param Query is the TAdoQuery to access
    ///          @param Tablename is the table name to use for automatically
    ///                           generated error messages </summary>
    constructor Create(_Query: TAdoQuery; const _Tablename: string); overload;
    constructor Create(_AdoDataset: TADODataSet; const _TableName: string); overload;
  end;

implementation

{ TDataSetHelperADO }

constructor TDataSetHelperADO.Create(_Table: TAdoTable);
begin
  inherited Create(_Table, _Table.TableName);
end;

type
  THackAdoDataset = class(TCustomAdoDataset)
  end;

constructor TDataSetHelperADO.Create(_Query: TAdoQuery; const _Tablename: string);
begin
  inherited Create(_Query, _Tablename);
end;

constructor TDataSetHelperADO.Create(_AdoDataset: TADODataSet; const _TableName: string);
begin
  inherited Create(_AdoDataset, _TableName);
end;

procedure TDataSetHelperADO.SetParamByName(const _Param: string; _Value: variant);
var
  i: Integer;
  Hack: THackAdoDataset;
begin
  // Do not use ParamByName -> only works if param is unique
  if FDataset is TCustomAdoDataset then begin
    Hack := THackAdoDataset(FDataset);
    for i := 0 to Hack.Parameters.Count - 1 do begin
      if SameText(Hack.Parameters[i].Name, _Param) then
        Hack.Parameters[i].Value := _Value;
    end;
  end else
    raise Exception.CreateFmt(_('SetParamByName is not supported for a %s (only TADODataSet descendants).'), [FDataset.ClassName]);
end;

function TDataSetHelperADO.TrySetParamByName(const _Param: string; _Value: variant): Boolean;
var
  AdoParam: TParameter;
begin
  if FDataset is TCustomAdoDataset then begin
    AdoParam := THackAdoDataset(FDataset).Parameters.FindParam(_Param);
    Result := Assigned(AdoParam);
    if Result then
      AdoParam.Value := _Value
  end else
    raise Exception.CreateFmt(_('TrySetParamByName is not supported for a %s (only TADODataSet descendants).'), [FDataset.ClassName]);
end;

end.

