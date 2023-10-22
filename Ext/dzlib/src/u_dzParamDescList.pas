unit u_dzParamDescList;

interface

uses
  Classes;

type
  TParamDesc = class
  protected
    FName: string;
    FDescription: string;
    FMinCount: integer;
    FMaxCount: integer;
  public
    constructor Create(const _Name, _Description: string; _MinCount, _MaxCount: integer);
    function GetCmdMask: string;
    function GetDescription(_Indent: integer): string;
    property Name: string read FName;
    property Description: string read FDescription;
    property MinCount: integer read FMinCount;
    property MaxCount: integer read FMaxCount;
  end;

{$DEFINE __DZ_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TParamDesc;
{$INCLUDE 't_dzObjectListTemplate.tpl'}

type
  ///<summary> List for storing TParamDesc items </summary>
  TParamDescList = class(_DZ_OBJECT_LIST_TEMPLATE_)

  end;

implementation

uses
  StrUtils;

{$INCLUDE 't_dzObjectListTemplate.tpl'}

constructor TParamDesc.Create(const _Name, _Description: string; _MinCount, _MaxCount: integer);
begin
  inherited Create;
  FName := _Name;
  FDescription := _Description;
  FMinCount := _MinCount;
  FMaxCount := _MaxCount;
end;

function TParamDesc.GetCmdMask: string;
begin
  Result := FName;
  if FMaxCount > 1 then
    Result := Result + '...';
  if FMinCount = 0 then
    Result := '[' + Result + ']';
end;

function TParamDesc.GetDescription(_Indent: integer): string;
begin
  Result := fName + StringOfChar(' ', _Indent - Length(fName) - 3) + ' : ' + fDescription;
end;

end.
