unit u_dzAdoDbUniqueId;

// NOTE: This works only if only one programm accesses the database!

interface

uses
  SysUtils,
  DB,
  ADODB;

type
  ITableIdProvider = interface ['{ABB6B50B-3518-46CE-A646-02EF92078533}']
    function GetNewId(_Count: integer = 1): integer;
  end;

type
  TAdoDbUniqueIdProvider = class
  private
    FIdTable: TADOTable;
  protected
    function GetNewId(const _TableName: string; _Count: integer): integer;
  public
    ///<summary> IdTable is the table containing the IDs
   ///          It must contain two columns:
    ///          TableName: Text
    ///	         LastID: LongInt </summary>
    constructor Create(_IdTable: TAdoTable);
    destructor Destroy; override;
    function GetTableIdProvider(const _TableName: string): ITableIdProvider;
  end;

type
  TTableIdProvider = class(TInterfacedObject, ITableIdProvider)
  private
    FIdProvider: TAdoDbUniqueIdProvider;
    FTableName: string;
  public
    constructor Create(_IdProvider: TAdoDbUniqueIdProvider; const _TableName: string);
    ///<summary> Returns unique IDs for the table
    ///          @param Count is the number of IDs requested, defaults to 1
    ///          @returns the first of Count IDs </summary>
    function GetNewId(_Count: integer = 1): integer;
  end;

implementation

{ TAdoDbUniqueIdProvider }

constructor TAdoDbUniqueIdProvider.Create(_IdTable: TAdoTable);
begin
  Assert(Assigned(_IdTable));
  Assert(not _IdTable.Active);

  inherited Create;
  FIdTable := _IdTable;
  FIdTable.LockType := ltPessimistic;
  FIdTable.DisableControls;
  FIdTable.Active := True;
end;

destructor TAdoDbUniqueIdProvider.Destroy;
begin
  if Assigned(FIdTable) then begin
    FIdTable.Active := False;
    FIdTable.EnableControls;
  end;
  inherited;
end;

function TAdoDbUniqueIdProvider.GetNewId(const _TableName: string; _Count: integer): integer;
var
  qry: TAdoQuery;
begin
  if not FIdTable.Locate('TableName', _TableName, []) then begin
    FIdTable.Insert;
    FIdTable['TableName'] := _TableName;
    qry := TADOQuery.Create(nil);
    try
      qry.DisableControls;
      qry.Connection := FIdTable.Connection;
      qry.SQL.Text := 'SELECT Max(ID) as MaxId FROM ' + _TableName;
      qry.Prepared := true;
      qry.Open;
      if qry.IsEmpty then
        Result := 0
      else
        Result := qry.FieldByName('MaxId').AsInteger;
    finally
      FreeAndNil(qry);
    end;
  end else begin
    FIdTable.Edit;
    Result := FIdTable.FieldByName('LastId').AsInteger;
  end;
  try
    Inc(Result, _Count);
    FIdTable.FieldByName('LastId').AsInteger := Result;
    FIdTable.Post;
  except
    FIdTable.Cancel;
    raise;
  end;
end;

function TAdoDbUniqueIdProvider.GetTableIdProvider(const _TableName: string): ITableIdProvider;
begin
  Result := TTableIdProvider.Create(Self, _TableName);
end;

{ TTableIdProvider }

constructor TTableIdProvider.Create(_IdProvider: TAdoDbUniqueIdProvider; const _TableName: string);
begin
  inherited Create;
  FIdProvider := _IdProvider;
  FTableName := _TableName;
end;

function TTableIdProvider.GetNewId(_Count: integer): integer;
begin
  Result := FIdProvider.GetNewId(FTableName, _Count);
end;

end.

