unit u_dzInputValidatorMarkerList;

interface

uses
  SysUtils,
  Classes,
  Controls,
  ExtCtrls,
  u_dzTranslator,
  u_dzQuickSort;

type
  TControlShapeLink = class
  private
    FName: string;
    FImage: TImage;
  public
    constructor Create(const _Name: string; _Image: TImage);
    property Name: string read FName;
    property Image: TImage read FImage;
  end;

{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TControlShapeLink;
  _KEY_TYPE_ = string;
{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

type
  {: List for storing TControlShapeLink items sorted by string }
  TInputValidatorShapeList = class(_DZ_SORTED_OBJECT_LIST_TEMPLATE_)
  protected
    {: return the key of an item for comparison }
    function KeyOf(const _Item: TControlShapeLink): string; override;
    {: compare the keys of two items, must return a value
       < 0 if Key1 < Key2, = 0 if Key1 = Key2 and > 0 if Key1 > Key2 }
    function Compare(const _Key1, _Key2: string): Integer; override;
  end;

implementation

{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

function TInputValidatorShapeList.KeyOf(const _Item: TControlShapeLink): string;
begin
  Result := _Item.Name;
end;

function TInputValidatorShapeList.Compare(const _Key1, _Key2: string): Integer;
begin
  Result := CompareText(_Key1, _Key2);
end;

{ TControlShapeLink }

constructor TControlShapeLink.Create(const _Name: string; _Image: TImage);
begin
  inherited Create;
  FName := _Name;
  FImage := _Image;
end;

end.

