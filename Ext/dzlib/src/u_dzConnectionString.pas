unit u_dzConnectionString;

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
  u_dzTypes;

{$ENDIF DELPHI2007_UP}

type
  TAdoServerType = (asUnknown, asJet, asMsSql, asOracle);

{$IFDEF DELPHI2007_UP}

type
  TConnectionInfoRec = record
  private
    FConnectionString: string;
    procedure SetPart(const _Name, _Value: string);
    function GetPart(const _Name: string): string;
    function GetInitialCatalog: string;
    function GetProvider: string;
    function GetDataSource: string;
    function GetUserId: string;
    procedure SetInitialCatalog(const _Value: string);
    procedure SetProvider(const _Value: string);
    procedure SetDataSource(const _Value: string);
    procedure SetUserId(const _Value: string);
    function GetConnectionString: string;
    procedure SetConnectionString(const _Value: string);
    function GetPassword: string;
    procedure SetPassword(const _Value: string);
  public
    class function DetermineServerType(const _ConnectionString: string): TAdoServerType; static;
    class function EditConnectionString(_ParentHandle: HWND; var _ConnectionString: string): Boolean; overload; static;
    function EditConnectionString(_ParentHandle: HWND): Boolean; overload;
    function GetParts: TStringArray; overload;
    procedure GetParts(_sl: TStrings); overload;
    function ServerType: TAdoServerType;
    property Provider: string read GetProvider write SetProvider;
    property ServerName: string read GetDataSource write SetDataSource;
    property DataSource: string read GetDataSource write SetDataSource;
    property Database: string read GetInitialCatalog write SetInitialCatalog; // same as InitialCatalog
    property InitialCatalog: string read GetInitialCatalog write SetInitialCatalog;
    property Username: string read GetUserId write SetUserId; // same as UserId
    property UserId: string read GetUserId write SetUserId;
    property Password: string read GetPassword write SetPassword;
    property ConnectionString: string read GetConnectionString write SetConnectionString;
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  StrUtils,
  OleDB,
  ComObj,
  ActiveX,
  u_dzVclUtils;

{ TConnectionInfoRec }

class function TConnectionInfoRec.EditConnectionString(_ParentHandle: HWND;
  var _ConnectionString: string): Boolean;
var
  DataInit: IDataInitialize;
  DBPrompt: IDBPromptInitialize;
  DataSource: IUnknown;
  InitStr: PWideChar;
  s: WideString;
begin
  DataInit := CreateComObject(CLSID_DataLinks) as IDataInitialize;
  if _ConnectionString <> '' then begin
    s := _ConnectionString;
    DataInit.GetDataSource(nil, CLSCTX_INPROC_SERVER,
      PWideChar(s), IUnknown, DataSource);
  end;
  DBPrompt := CreateComObject(CLSID_DataLinks) as IDBPromptInitialize;

  TCommonDialog_CenterWithBackgroundThread;
  Result := Succeeded(DBPrompt.PromptDataSource(nil, _ParentHandle,
    DBPROMPTOPTIONS_PROPERTYSHEET, 0, nil, nil, IUnknown, DataSource));
  if Result then begin
    InitStr := nil;
    DataInit.GetInitializationString(DataSource, True, InitStr);
    _ConnectionString := InitStr;
  end;
end;

function TConnectionInfoRec.EditConnectionString(_ParentHandle: HWND): Boolean;
var
  s: string;
begin
  s := FConnectionString;
  Result := EditConnectionString(_ParentHandle, s);
  if Result then
    FConnectionString := s;
end;

function TConnectionInfoRec.GetConnectionString: string;
begin
  Result := FConnectionString;
end;

procedure TConnectionInfoRec.SetConnectionString(const _Value: string);
begin
  FConnectionString := _Value;
end;

function TConnectionInfoRec.GetPart(const _Name: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    GetParts(sl);
    Result := sl.Values[_Name];
  finally
    FreeAndNil(sl);
  end;
end;

procedure TConnectionInfoRec.SetPart(const _Name, _Value: string);
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    GetParts(sl);
    if _Value = '' then begin
      for i := sl.Count - 1 downto 0 do begin
        if SameText(sl.Names[i], _Name) then
          sl.Delete(i);
      end;
    end else
      sl.Values[_Name] := _Value;
    FConnectionString := sl.DelimitedText;
  finally
    FreeAndNil(sl);
  end;
end;

procedure TConnectionInfoRec.GetParts(_sl: TStrings);
begin
  _sl.StrictDelimiter := True;
  _sl.Delimiter := ';';
  _sl.DelimitedText := FConnectionString;
end;

function TConnectionInfoRec.GetParts: TStringArray;
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    GetParts(sl);
    SetLength(Result, sl.Count);
    for i := 0 to sl.Count - 1 do
      Result[i] := sl[i];
  finally
    FreeAndNil(sl);
  end;
end;

function TConnectionInfoRec.GetInitialCatalog: string;
begin
  Result := GetPart('Initial Catalog');
end;

procedure TConnectionInfoRec.SetInitialCatalog(const _Value: string);
begin
  SetPart('Initial Catalog', _Value);
end;

function TConnectionInfoRec.GetProvider: string;
begin
  Result := GetPart('Provider');
end;

procedure TConnectionInfoRec.SetProvider(const _Value: string);
begin
  SetPart('Provider', _Value);
end;

function TConnectionInfoRec.GetDataSource: string;
begin
  Result := GetPart('Data Source');
end;

procedure TConnectionInfoRec.SetDataSource(const _Value: string);
begin
  SetPart('Data Source', _Value);
end;

function TConnectionInfoRec.GetUserId: string;
begin
  Result := GetPart('User ID');
end;

procedure TConnectionInfoRec.SetUserId(const _Value: string);
begin
  SetPart('User ID', _Value);
end;

function TConnectionInfoRec.GetPassword: string;
begin
  Result := GetPart('Password');
end;

procedure TConnectionInfoRec.SetPassword(const _Value: string);
begin
  SetPart('Password', _Value);
end;

function TConnectionInfoRec.ServerType: TAdoServerType;
begin
  Result := DetermineServerType(FConnectionString);
end;

class function TConnectionInfoRec.DetermineServerType(const _ConnectionString: string): TAdoServerType;
begin
  if AnsiContainsText(_ConnectionString, 'Provider=OraOLEDB.Oracle') then
    Result := asOracle
  else if AnsiContainsText(_ConnectionString, 'Provider=SQLOLEDB')
    or AnsiContainsText(_ConnectionString, 'Provider=SQLNCLI10') then
    Result := asMsSql
  else if AnsiContainsText(_ConnectionString, 'Provider=Microsoft.Jet.OLEDB') then
    Result := asJet
  else
    Result := asUnknown;
end;

{$ENDIF DELPHI2007_UP}

end.

