unit u_dzDatasetHelpersTdbf;

interface

uses
  u_dzDatasetHelpers,
  dbf; // libs\tdbf\src

type
  TDataSetHelperTdbf = class(TDatasetHelper)
  public
    constructor Create(_Table: TDBF); overload;
  end;

implementation

{ TDataSetHelperTdbf }

constructor TDataSetHelperTdbf.Create(_Table: TDBF);
begin
  inherited Create(_Table, _Table.TableName);
end;

end.
