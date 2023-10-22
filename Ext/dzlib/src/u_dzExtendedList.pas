unit u_dzExtendedList;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes;

type
  TExtendedEntry = class
  private
    FValue: Extended;
  public
    constructor Create(_Value: Extended);
    function ToString: string; overload; {$IFDEF HAS_TOBJECT_TOSTRING} override; {$ENDIF}
    function ToString(const _FormatStr: string = '%g'): string; reintroduce; overload;
    property Value: Extended read FValue write FValue;
  end;

{$DEFINE __DZ_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _LIST_CONTAINER_ = TList;
  _LIST_CONTAINER_ITEM_TYPE_ = pointer;
  _ITEM_TYPE_ = TExtendedEntry;
{$INCLUDE 't_dzListTemplate.tpl'}

type
  {: List for storing TExtendedEntry items }
  TdzExtendedList = class(_DZ_LIST_TEMPLATE_)
  protected
    {: Frees a TExtendedEntry }
    procedure FreeItem(_Item: TExtendedEntry); override;
  public
    procedure Add(_Value: Extended); reintroduce;
    function Extract(_Idx: Integer): Extended; reintroduce;
  end;

implementation

{$INCLUDE 't_dzListTemplate.tpl'}

procedure TdzExtendedList.Add(_Value: Extended);
begin
  inherited Add(TExtendedEntry.Create(_Value));
end;

function TdzExtendedList.Extract(_Idx: Integer): Extended;
var
  Item: TExtendedEntry;
begin
  Item := inherited Extract(_Idx);
  Result := Item.Value;
  FreeItem(Item);
end;

procedure TdzExtendedList.FreeItem(_Item: TExtendedEntry);
begin
  _Item.Free;
end;

{ TExtendedEntry }

constructor TExtendedEntry.Create(_Value: Extended);
begin
  inherited Create;
  FValue := _Value;
end;

function TExtendedEntry.ToString(const _FormatStr: string = '%g'): string;
begin
  Result := Format(_FormatStr, [Value]);
end;

function TExtendedEntry.ToString: string;
begin
  Result := Format('%g', [Value]);
end;

end.
