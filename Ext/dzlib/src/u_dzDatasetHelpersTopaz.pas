unit u_dzDatasetHelpersTopaz;

interface

uses
  SysUtils,
  u_dzDatasetHelpers,
  tzprimds, // libs\topaz\src
  ucommon,
  utzcds,
  utzfds;

type
  TDataSetHelperTopaz = class(TDatasetHelper)
  public
    constructor Create(_Table: TTzDbf); overload;
  end;

implementation

{ TDataSetHelperTopaz }

constructor TDataSetHelperTopaz.Create(_Table: TTzDbf);
begin
  inherited Create(_Table, Extractfilename(_Table.DbfFileName));
end;

end.

