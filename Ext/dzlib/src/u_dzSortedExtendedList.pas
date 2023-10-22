unit u_dzSortedExtendedList;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzQuicksort;

type
  TExtendedEntry = class
  private
    FValue: Extended;
  public
    constructor Create(_Value: Extended);
    function ToString: string; overload;
{$IFDEF HAS_TOBJECT_TOSTRING} override;
{$ENDIF}
    function ToString(const _FormatStr: string = '%g'): string; reintroduce; overload;
    property Value: Extended read FValue write FValue;
  end;

{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TExtendedEntry;
  _KEY_TYPE_ = Extended;
{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

type
  ///<summary>
  /// List for storing TExtendedEntry items sorted by Extended </summary>
  TdzSortedExtendedList = class(_DZ_SORTED_OBJECT_LIST_TEMPLATE_)
  protected
    ///<summary>
    /// @returns the key of an item for comparison </sumary>
    function KeyOf(const _Item: TExtendedEntry): Extended; override;
    ///<summary>
    /// Compares the keys of two items
    /// @returns < 0 if Key1 < Key2
    ///            0 if Key1 = Key2
    ///          > 0 if Key1 > Key2 </summary>
    function Compare(const _Key1, _Key2: Extended): Integer; override;
  public
    function Add(const _Value: Extended): Integer; reintroduce;
  end;

implementation

uses
  Math;

{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

function TdzSortedExtendedList.KeyOf(const _Item: TExtendedEntry): Extended;
begin
  Result := _Item.Value;
end;

function TdzSortedExtendedList.Add(const _Value: Extended): Integer;
var
  Entry: TExtendedEntry;
begin
  Entry := TExtendedEntry.Create(_Value);
  try
    Result := inherited Add(Entry);
    if Result <> -1 then begin
      // Adding has worked, the Entry is now owned by the list, so it must not be freed.
      // Otherwise it will be freed in the finally block.
      Entry := nil;
    end;
  finally
    FreeAndNil(Entry);
  end;
end;

function TdzSortedExtendedList.Compare(const _Key1, _Key2: Extended): Integer;
begin
  Result := CompareValue(_Key1, _Key2);
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

