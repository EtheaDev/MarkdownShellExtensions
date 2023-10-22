unit u_dzCsvColumnList;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzTypes,
  u_dzNullableTypesUtils,
  u_dzNullableDate,
  u_dzNullableTime,
  u_dzConvertUtils;

type
  ECsvWriter = class(EdzException);
const
  MAX_CSV_DECIMALS = 20;

type
  TCsvColumnType = (cctString, cctInteger, cctDate, cctTime, cctFloat, cctBoolean);

function TCsvColumnTypeToLocalizedName(_cct: TCsvColumnType): string;

type
  ICsvColumnDefinition = interface ['{0B2F01D5-10C7-496D-8885-A9148B880F21}']
    function SetDecimals(_Decimals: Integer): ICsvColumnDefinition;
    function SetUseQuotes: ICsvColumnDefinition;
    function Name: string;
    function ColType: TCsvColumnType;
    function Decimals: Integer;
    function UseQuotes: Boolean;
    function SetBoolStrings(const _BoolToStr: TBoolToStr): ICsvColumnDefinition;
  end;

  ICsvColumn = interface ['{DB7B2613-530B-4C77-88D2-3B78438FB828}']
    procedure SetValue(const _StrValue: string); overload;
{$IFDEF UNICODE}
    procedure SetValue(const _StrValue: AnsiString); overload;
{$ENDIF}
    procedure SetValue(_IntValue: Int8); overload;
    procedure SetValue(_IntValue: Int16); overload;
    procedure SetValue(_IntValue: Integer); overload;
    procedure SetValue(_IntValue: Int64); overload;
    procedure SetValue(_IntValue: UInt8); overload;
    procedure SetValue(_IntValue: UInt16); overload;
    procedure SetValue(_IntValue: UInt32); overload;
    procedure SetValue(_IntValue: UInt64); overload;
    procedure SetValue(_DateValue: TNullableDate); overload;
    procedure SetValue(_TimeValue: TNullableTime); overload;
    procedure SetValue(_FloatValue: Extended); overload;
    procedure SetValue(_BoolValue: Boolean); overload;
    function GetValue: string;
    procedure Clear;
  end;

type
  TdzCsvColumn = class(TInterfacedObject, ICsvColumnDefinition, ICsvColumn)
  private
    FName: string;
    FDecimals: Integer;
    FType: TCsvColumnType;
    FUseQuotes: Boolean;
    FBoolToStr: TBoolToStr;
    FValue: string;
    FFormatSettings: PFormatSettings;
  private // ICsvColumnDefinition
    function Decimals: Integer;
    function Name: string;
    function ColType: TCsvColumnType;
    function SetDecimals(_Decimals: Integer): ICsvColumnDefinition;
    function SetUseQuotes: ICsvColumnDefinition;
    function UseQuotes: Boolean;
    function SetBoolStrings(const _BoolToStr: TBoolToStr): ICsvColumnDefinition;
  private // ICsvColumn
    procedure SetValue(const _StrValue: string); overload;
{$IFDEF UNICODE}
    procedure SetValue(const _StrValue: AnsiString); overload;
{$ENDIF}
    procedure SetValue(_IntValue: Int8); overload;
    procedure SetValue(_IntValue: Int16); overload;
    procedure SetValue(_IntValue: Integer); overload;
    procedure SetValue(_IntValue: Int64); overload;
    procedure SetValue(_IntValue: UInt8); overload;
    procedure SetValue(_IntValue: UInt16); overload;
    procedure SetValue(_IntValue: UInt32); overload;
    procedure SetValue(_IntValue: UInt64); overload;
    procedure SetValue(_DateValue: TNullableDate); overload;
    procedure SetValue(_TimeValue: TNullableTime); overload;
    procedure SetValue(_FloatValue: Extended); overload;
    procedure SetValue(_BoolValue: Boolean); overload;
    function GetValue: string;
    procedure Clear;
  public
    constructor Create(const _Name: string; _Type: TCsvColumnType; _FormatSettings: PFormatSettings);
  end;

{$DEFINE __DZ_INTERFACE_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = ICsvColumnDefinition;
{$INCLUDE 't_dzInterfaceListTemplate.tpl'}

type
  {: List for storing TdzCsvColumn items }
  TdzCsvColumnList = class(_DZ_INTERFACE_LIST_TEMPLATE_)
    function FindColumn(const _Name: string; out _Col: ICsvColumn): Boolean;
    function FindColumnDef(const _Name: string; out _Col: ICsvColumnDefinition): Boolean;
    function HasColumn(const _Name: string): Boolean;
    function GetColumn(const _Name: string): ICsvColumn;
    function GetColumnDef(const _Name: string): ICsvColumnDefinition;
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

function _(const _s: string): string;
begin
  Result := dzgettext(_s);
end;

function TCsvColumnTypeToLocalizedName(_cct: TCsvColumnType): string;
begin
  case _cct of
    cctString: Result := _('String');
    cctInteger: Result := _('Integer');
    cctDate: Result := _('Date');
    cctTime: Result := _('Time');
    cctFloat: Result := _('Float');
    cctBoolean: Result := _('Boolean');
  else
    raise ECsvWriter.CreateFmt(_('Invalid CSV column type %d'), [Ord(_cct)]);
  end;
end;

{$INCLUDE 't_dzInterfaceListTemplate.tpl'}

{ TdzCsvColumn }

constructor TdzCsvColumn.Create(const _Name: string; _Type: TCsvColumnType; _FormatSettings: PFormatSettings);
begin
  inherited Create;
  FName := _Name;
  FType := _Type;
  FDecimals := 0;
  FUseQuotes := False;
  FFormatSettings := _FormatSettings;
  case FType of
    cctFloat: FDecimals := 3;
  end;
  FBoolToStr := TBoolToStr.CreateYN;
end;

function TdzCsvColumn.Decimals: Integer;
begin
  Result := FDecimals;
end;

function TdzCsvColumn.GetValue: string;
begin
  Result := FValue;
  if (Result <> '') and FUseQuotes then
    Result := '"' + FValue + '"';
end;

function TdzCsvColumn.Name: string;
begin
  Result := FName;
end;

procedure TdzCsvColumn.Clear;
begin
  FValue := '';
end;

function TdzCsvColumn.ColType: TCsvColumnType;
begin
  Result := FType;
end;

function TdzCsvColumn.SetBoolStrings(const _BoolToStr: TBoolToStr): ICsvColumnDefinition;
begin
  case FType of
    cctBoolean: begin
        FBoolToStr := _BoolToStr;
      end else
    raise ECsvWriter.CreateFmt(_('The bool strings can not be set for column %s.'), [Name]);
  end;
  Result := Self;
end;

function TdzCsvColumn.SetDecimals(_Decimals: Integer): ICsvColumnDefinition;
begin
  if _Decimals < 0 then
    raise ECsvWriter.Create(_('Decimals must be >0.'));

  if _Decimals > MAX_CSV_DECIMALS then
    raise ECsvWriter.CreateFmt(_('Decimals must be <=%d.'), [MAX_CSV_DECIMALS]);

  case FType of
    cctTime,
      cctFloat: FDecimals := _Decimals;
  else
    raise ECsvWriter.CreateFmt(_('The number of decimals can not be set for column %s.'), [Name]);
  end;
  Result := Self;
end;

function TdzCsvColumn.SetUseQuotes: ICsvColumnDefinition;
begin
  case FType of
    cctString,
      cctBoolean,
      cctTime:
      FUseQuotes := True;
  else
    raise ECsvWriter.CreateFmt(_('Using quotes can not be set for column %s.'), [Name]);
  end;
  Result := Self;
end;

procedure TdzCsvColumn.SetValue(_IntValue: Int8);
begin
  SetValue(Int64(_IntValue));
end;

procedure TdzCsvColumn.SetValue(_IntValue: Int16);
begin
  SetValue(Int64(_IntValue));
end;

procedure TdzCsvColumn.SetValue(_IntValue: Integer);
begin
  SetValue(Int64(_IntValue));
end;

procedure TdzCsvColumn.SetValue(_IntValue: UInt8);
begin
  SetValue(UInt64(_IntValue));
end;

procedure TdzCsvColumn.SetValue(_IntValue: UInt16);
begin
  SetValue(UInt64(_IntValue));
end;

procedure TdzCsvColumn.SetValue(_IntValue: UInt32);
begin
  SetValue(UInt64(_IntValue));
end;

procedure TdzCsvColumn.SetValue(_IntValue: UInt64);

  function IntegerToExtended(_IntValue: UInt64): Extended;
  begin
    Result := _IntValue;
  end;

begin
  case FType of
    cctInteger: begin
        FValue := IntToStr(_IntValue);
      end;
    cctFloat: begin
        FValue := Format('%.*f', [FDecimals, IntegerToExtended(_IntValue)]);
      end else
    raise ECsvWriter.CreateFmt(_('Invalid value type for column %s, must be %s.'),
      [Name, TCsvColumnTypeToLocalizedName(FType)]);
  end;
end;

procedure TdzCsvColumn.SetValue(_IntValue: Int64);

  function IntegerToExtended(_IntValue: Int64): Extended;
  begin
    Result := _IntValue;
  end;

begin
  case FType of
    cctInteger: begin
        FValue := IntToStr(_IntValue);
      end;
    cctFloat: begin
        FValue := Format('%.*f', [FDecimals, IntegerToExtended(_IntValue)]);
      end else
    raise ECsvWriter.CreateFmt(_('Invalid value type for column %s, must be %s.'),
      [Name, TCsvColumnTypeToLocalizedName(FType)]);
  end;
end;

procedure TdzCsvColumn.SetValue(const _StrValue: string);
begin
  case FType of
    cctString: begin
        FValue := _StrValue;
      end;
  else
    raise ECsvWriter.CreateFmt(_('Invalid value type for column %s, must be %s.'),
      [Name, TCsvColumnTypeToLocalizedName(FType)]);
  end;
end;

{$IFDEF UNICODE}
procedure TdzCsvColumn.SetValue(const _StrValue: AnsiString);
begin
  case FType of
    cctString: begin
        FValue := string(_StrValue);
      end;
  else
    raise ECsvWriter.CreateFmt(_('Invalid value type for column %s, must be %s.'),
      [Name, TCsvColumnTypeToLocalizedName(FType)]);
  end;
end;
{$ENDIF}

procedure TdzCsvColumn.SetValue(_TimeValue: TNullableTime);
begin
  case FType of
    cctTime: begin
        if _TimeValue.IsValid then
          FValue := _TimeValue.ToHHmmSSz(FDecimals)
        else
          FValue := '';
      end;
  else
    raise ECsvWriter.CreateFmt(_('Invalid value type for column %s, must be %s.'),
      [Name, TCsvColumnTypeToLocalizedName(FType)]);
  end;
end;

procedure TdzCsvColumn.SetValue(_DateValue: TNullableDate);
begin
  case FType of
    cctDate: begin
        if _DateValue.IsValid then
          FValue := _DateValue.ToYYYYmmDD
        else
          FValue := '';
      end;
  else
    raise ECsvWriter.CreateFmt(_('Invalid value type for column %s, must be %s.'),
      [Name, TCsvColumnTypeToLocalizedName(FType)]);
  end;
end;

procedure TdzCsvColumn.SetValue(_FloatValue: Extended);
begin
  case FType of
    cctFloat: begin
        FValue := Format('%.*f', [FDecimals, _FloatValue], FFormatSettings^);
      end;
  else
    raise ECsvWriter.CreateFmt(_('Invalid value type for column %s, must be %s.'),
      [Name, TCsvColumnTypeToLocalizedName(FType)]);
  end;
end;

procedure TdzCsvColumn.SetValue(_BoolValue: Boolean);
begin
  case FType of
    cctBoolean: begin
        FValue := FBoolToStr.ToString(_BoolValue);
      end else
    raise ECsvWriter.CreateFmt(_('Invalid value type for column %s, must be %s.'),
      [Name, TCsvColumnTypeToLocalizedName(FType)]);
  end;
end;

function TdzCsvColumn.UseQuotes: Boolean;
begin
  Result := FUseQuotes;
end;

{ TdzCsvColumnList }

function TdzCsvColumnList.FindColumnDef(const _Name: string; out _Col: ICsvColumnDefinition): Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do begin
    if SameText(Items[i].Name, _Name) then begin
      Result := True;
      _Col := Items[i];
      Exit; //==>
    end;
  end;
  Result := False;
end;

function TdzCsvColumnList.GetColumn(const _Name: string): ICsvColumn;
begin
  if not FindColumn(_Name, Result) then
    raise ECsvWriter.CreateFmt(_('Column %s not found.'), [_Name]);
end;

function TdzCsvColumnList.GetColumnDef(const _Name: string): ICsvColumnDefinition;
begin
  if not FindColumnDef(_Name, Result) then
    raise ECsvWriter.CreateFmt(_('Column %s not found.'), [_Name]);
end;

function TdzCsvColumnList.HasColumn(const _Name: string): Boolean;
var
  col: ICsvColumn;
begin
  Result := FindColumn(_Name, col);
end;

function TdzCsvColumnList.FindColumn(const _Name: string; out _Col: ICsvColumn): Boolean;
var
  col: ICsvColumnDefinition;
begin
  Result := FindColumnDef(_Name, col);
  if Result then
    _Col := col as ICsvColumn;
end;

{$ENDIF DELPHI2007_UP}
end.
