///<summary>
/// an unsorted list of Name-Value pairs (unfortunately u_dzNameValueList is sorted) </summary>
unit u_dzNameValueListUnsorted deprecated; // see u_dzUnsortdNameValueList

interface

uses
  SysUtils,
  Classes;

type
  TNameValueItem = class
  private
    FName: string;
    FValue: string;
  public
    constructor Create(const _Name, _Value: string);
    property Name: string read FName;
    property Value: string read FValue write FValue;
  end;

{$DEFINE __DZ_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TNameValueItem;
{$INCLUDE 't_dzObjectListTemplate.tpl'}

type
  ///<summary>
  /// List for storing TNameValueItem items </summary>
  TNameValueListUnsorted = class(_DZ_OBJECT_LIST_TEMPLATE_)
    function AddEntry(const _Name, _Value: string): Integer;
  end;

implementation

{$INCLUDE 't_dzObjectListTemplate.tpl'}

{ TNameValueItem }

constructor TNameValueItem.Create(const _Name, _Value: string);
begin
  inherited Create;
  FName := _Name;
  FValue := _Value;
end;

{ TNameValueListUnsorted }

function TNameValueListUnsorted.AddEntry(const _Name, _Value: string): Integer;
begin
  Result := Add(TNameValueItem.Create(_Name, _Value));
end;

end.

