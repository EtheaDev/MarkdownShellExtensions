{.GXFormatter.config=twm}
///<summary> declares the IDatssetHelper unterface and the TDatasetHelper implementation
///          for typesafe access to database fields </summary>
unit u_dzDatasetHelpers;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes,
  DB,
{$IFDEF HAS_UNIT_WIDESTRINGS}
  WideStrings,
{$ENDIF}
  u_dzTranslator,
  u_dzGuidUtils,
  u_dzNameValueList,
  u_dzVariantUtils, // for TryVar2DateTime
  u_dzStringsGuard;

type
  ///<summary> Interface definition for the Dataset-Helper, the idea is to have simplified
  ///          methods for reading field values, converting them to the appropriate data
  ///          type and generate standardized error messages if something goes wrong
  ///          that contain the table and field name rather than just saying
  ///          "Variant conversion error". </summary>
  IDatasetHelper = interface ['{756CC74A-1623-4FC4-A347-4CA3D90B4D69}']
    ///<summary> Returns a userfriendly name for the underlying dataset to be used in error messages.
    // </summary>
    function GetDatasetName: string;
    ///<summary> returns the field value as a string, raises an exception if it cannot be converted,
    ///          Note that strings are automatically trimmed. </summary>
    function FieldAsString(const _Fieldname: string): string; overload;
    ///<summary> returns the field value as a string, returns the default if it cannot be converted
    ///          Note that strings are automatically trimmed. </summary>
    function FieldAsString(const _Fieldname, _Default: string): string; overload;
    ///<summary> returns the field value as a string, raises an exception if it cannot be converted,
    ///          Note that strings are automatically trimmed. </summary>
    function FieldAsString(_Field: TField): string; overload;
    ///<summary> returns the field value as a string, returns the default if it cannot be converted
    ///          Note that strings are automatically trimmed. </summary>
    function FieldAsString(_Field: TField; const _Default: string): string; overload;
    function TryFieldAsString(const _Fieldname: string; out _Value: string): Boolean; overload;
    function TryFieldAsString(_Field: TField; out _Value: string): Boolean; overload;
    ///<summary> in addition to TryFieldAsString also checks whether the string is non-empty </summary>
    function TryFieldAsNonEmptyString(const _Fieldname: string; out _Value: string): Boolean;
    ///<summary> sets the field as a string, if the value is empty set it to NULL </summary>
    procedure SetFieldStringNotEmpty(const _Fieldname: string; const _Value: string);

    ///<summary> returns the field value as an integer, raise an exception if it cannot be converted </summary>
    function FieldAsInteger(const _Fieldname: string): Integer; overload;
    ///<summary> returns the field value as an integer, raise an exception if it cannot be converted </summary>
    function FieldAsInteger(_Field: TField): Integer; overload;
    ///<summary> returns the field value as an integer, return the default if it cannot be converted </summary>
    function FieldAsInteger(const _Fieldname: string; _Default: Integer): Integer; overload;
    ///<summary> returns the field value as an integer, return the default if it cannot be converted </summary>
    function FieldAsInteger(_Field: TField; _Default: Integer): Integer; overload;
    ///<summary> returns the field value as an integer, raise an exception with the given error message if it cannot be converted </summary>
    function FieldAsInteger(const _Fieldname: string; const _Error: string): Integer; overload;
    ///<summary> returns the field value as an integer, raise an exception with the given error message if it cannot be converted </summary>
    function FieldAsInteger(_Field: TField; const _Error: string): Integer; overload;
    function TryFieldAsInteger(const _Fieldname: string; out _Value: Integer): Boolean; overload;
    function TryFieldAsInteger(_Field: TField; out _Value: Integer): Boolean; overload;

    ///<summary> returns the field value as a double, raise an exception if it cannot be converted </summary>
    function FieldAsDouble(const _Fieldname: string): Double; overload;
    ///<summary> returns the field value as a double, return the default if it cannot be converted </summary>
    function FieldAsDouble(const _Fieldname: string; const _Default: Double): Double; overload;
    ///<summary> returns the field value as a double, raise an exception with the given error message if it cannot be converted </summary>
    function FieldAsDouble(const _Fieldname: string; const _Error: string): Double; overload;
    function TryFieldAsDouble(const _Fieldname: string; out _Value: Double): Boolean;

    ///<summary> returns the field value as an extended, raise an exception if it cannot be converted </summary>
    function FieldAsExtended(const _Fieldname: string): Extended; overload;
    ///<summary> returns the field value as a extended, return the default if it cannot be converted </summary>
    function FieldAsExtended(const _Fieldname: string; const _Default: Extended): Extended; overload;
    ///<summary> returns the field value as a extended, raise an exception with the given error message if it cannot be converted </summary>
    function FieldAsExtended(const _Fieldname: string; const _Error: string): Extended; overload;
    function TryFieldAsExtended(const _Fieldname: string; out _Value: Extended): Boolean;

{$IF Declared(TryVar2DateTime)}
    ///<summary> returns the field value as a TDateTime, raise an exception if it cannot be converted </summary>
    function FieldAsDate(const _Fieldname: string): TDateTime; overload;
    function FieldAsDate(const _Fieldname: string; _Default: TDateTime): TDateTime; overload;
    function TryFieldAsDate(const _Fieldname: string; out _Date: TDateTime): Boolean;
{$IFEND}
    ///<summary> returns the field value as a boolean, raise an exception if it cannot be converted </summary>
    function FieldAsBoolean(const _Fieldname: string): Boolean; overload;
    ///<summary> returns the field value as a boolean, return the default if it cannot be converted </summary>
    function FieldAsBoolean(const _Fieldname: string; _Default: Boolean): Boolean; overload;
{$IF Declared(TNullableGuid)}
    ///<summary> returns the field value as a TNullableGuid record, note that the guid might be
    ///          invalid if the field contained NULL </summary>
    function FieldAsGuid(const _Fieldname: string): TNullableGuid;
    ///<summary> tries to convert the field to a GUID, returns false, if that's not possible </summary>
    function TryFieldAsGuid(const _Fieldname: string; out _Value: TNullableGuid): Boolean;
{$IFEND}
    ///<summary> Opens the dataset </summary>
    procedure Open;
    ///<summary> Closes the dataset </summary>
    procedure Close;

    ///<summary> Moves to the first record of the dataset </summary>
    procedure First;
    ///<summary> Moves to the last record of the dataset </summary>
    procedure Last;
    ///<summary> Moves to the next record of the dataset, returns true if not EOF </summary>
    function Next: Boolean;
    ///<summary> Moves to the previous record of the dataset, returns true if not BOF </summary>
    function Prior: Boolean;
    ///<summary> Moves by Distance records (can be negative), returns the number of records actually moved </summary>
    function MoveBy(_Distance: Integer): Integer;
    ///<summary> Returns true if at the end of the dataset </summary>
    function Eof: Boolean;
    ///<summary> Returns true if at the beginning of the dataset </summary>
    function Bof: Boolean;

    procedure Append;
    ///<summary> insert a new record into the dataset </summary>
    procedure Insert;
    ///<summary> put the current record into edit mode </summary>
    procedure Edit;

    procedure Delete;

    ///<summary> post changes to the current record (must call Insert or Edit first) </summary>
    procedure Post;
    ///<summary> cancel changes to the current record (must call Insert or Edit first) </summary>
    procedure Cancel;

    function IsEmpty: Boolean;

    procedure DisableControls;
    procedure EnableControls;

    function Locate(const _KeyFields: string; const _KeyValues: Variant; _Options: TLocateOptions): Boolean;
    procedure SetParamByName(const _Param: string; _Value: Variant);
    function TrySetParamByName(const _Param: string; _Value: Variant): Boolean;

    ///<summary> returns the field value as variant (getter method for FieldValues property) </summary>
    function GetFieldValue(const _Fieldname: string): Variant;
    ///<summary> sets the field value as variant (setter method for FieldValues property) </summary>
    procedure SetFieldValue(const _Fieldname: string; const _Value: Variant);
    ///<summary> sets the field value, if the filed exists
    ///          @param Fieldname is name of the field to set
    ///          @param Value is the new value
    ///          @returns true, if the field exists, false otherwise </summary>
    function TrySetFieldValue(const _Fieldname: string; const _Value: Variant): Boolean;
    ///<summary> Set the field value to a string, checks wether the string fits into the field.
    ///          @param FieldName is the name of the field to set
    ///          @param Value is the string to set the field to
    ///          @param CheckLength determines wether to check that the value fits into the field
    ///                             if not, an Exception will be raised and field remains unchanged.
    ///          @raises Exception if the new value is too long for the field length </summary>
    procedure SetFieldAsString(const _Fieldname: string; const _Value: string; _CheckLength: Boolean = True);
    procedure ClearField(const _Fieldname: string);

    function GetActive: Boolean;
    procedure SetActive(const _Value: Boolean);

    function HasField(const _Fieldname: string): Boolean;
    function Fields: TFields;

    ///<summary> Copies all values of the current record to the given NameValueList, ignoring
    ///          all fields that either contain NULL or are listed in Ignore.
    ///          @param NameValueList is a TdzNameValueList that returns the current record as name=value pairs
    ///          @param Ignore is an array of string with all field names that should not be copied
    ///</summary>
    procedure ToNameValueList(_Values: TNameValueList; const _Ignore: array of string);
    ///<summary> Copies all values from the given NameValueList to the current record
    ///          @param NameValueList is a TdzNameValueList that contains the values
    ///</summary>
    procedure FromNameValueList(_Values: TNameValueList);
    ///<summary> allows access to field values as variants </summary>
    property FieldValues[const _Fieldname: string]: Variant read GetFieldValue write SetFieldValue; default;
    property Active: Boolean read GetActive write SetActive;
  end;

type
  ///<summary> implements the IDatasetHelper interface
  ///          Note: You might want to instantiate a TDatasetHelperBDE,
  ///                TDatasetHelperADO or TDatasetHelperTDBF instead. </summary>
  TDatasetHelper = class(TInterfacedObject, IDatasetHelper)
  private
    function FieldByName(const _Fieldname: string): TField;
    function GetActive: Boolean;
    procedure SetActive(const _Value: Boolean);
    procedure InitSortedFields;
  protected
    FDataset: TDataSet;
    FSortedFields: TStringList;
    FTableName: string;
  protected // implementation of IDatasetHelper, see there for a description
    function GetDatasetName: string;
    function FieldAsString(const _Fieldname: string): string; overload;
    function FieldAsString(_Field: TField): string; overload;
    function FieldAsString(const _Fieldname, _Default: string): string; overload;
    function FieldAsString(_Field: TField; const _Default: string): string; overload;
    function TryFieldAsString(const _Fieldname: string; out _Value: string): Boolean; overload;
    function TryFieldAsString(_Field: TField; out _Value: string): Boolean; overload;
    function TryFieldAsNonEmptyString(const _Fieldname: string; out _Value: string): Boolean;
    procedure SetFieldStringNotEmpty(const _Fieldname: string; const _Value: string);

    function FieldAsInteger(const _Fieldname: string): Integer; overload;
    function FieldAsInteger(_Field: TField): Integer; overload;
    function FieldAsInteger(const _Fieldname: string; _Default: Integer): Integer; overload;
    function FieldAsInteger(_Field: TField; _Default: Integer): Integer; overload;
    function FieldAsInteger(const _Fieldname: string; const _Error: string): Integer; overload;
    function FieldAsInteger(_Field: TField; const _Error: string): Integer; overload;
    function TryFieldAsInteger(const _Fieldname: string; out _Value: Integer): Boolean; overload;
    function TryFieldAsInteger(_Field: TField; out _Value: Integer): Boolean; overload;

    function FieldAsDouble(const _Fieldname: string): Double; overload;
    function FieldAsDouble(const _Fieldname: string; const _Default: Double): Double; overload;
    function FieldAsDouble(const _Fieldname: string; const _Error: string): Double; overload;
    function TryFieldAsDouble(const _Fieldname: string; out _Value: Double): Boolean;

    function FieldAsExtended(const _Fieldname: string): Extended; overload;
    function FieldAsExtended(const _Fieldname: string; const _Default: Extended): Extended; overload;
    function FieldAsExtended(const _Fieldname: string; const _Error: string): Extended; overload;
    function TryFieldAsExtended(const _Fieldname: string; out _Value: Extended): Boolean;

{$IF Declared(TryVar2DateTime)}
    function FieldAsDate(const _Fieldname: string): TDateTime; overload;
    function FieldAsDate(const _Fieldname: string; _Default: TDateTime): TDateTime; overload;
    function TryFieldAsDate(const _Fieldname: string; out _Date: TDateTime): Boolean;
{$IFEND}
    function FieldAsBoolean(const _Fieldname: string): Boolean; overload;
    function FieldAsBoolean(const _Fieldname: string; _Default: Boolean): Boolean; overload;

{$IF Declared(TNullableGuid)}
    function FieldAsGuid(const _Fieldname: string): TNullableGuid;
    function TryFieldAsGuid(const _Fieldname: string; out _Value: TNullableGuid): Boolean;
{$IFEND}
    procedure Open;
    procedure Close;

    procedure First;
    procedure Last;

    function Next: Boolean;
    function Prior: Boolean;
    function MoveBy(_Distance: Integer): Integer;

    function Eof: Boolean;
    function Bof: Boolean;

    procedure Append;
    procedure Insert;
    procedure Edit;

    procedure Delete;

    procedure Post;
    procedure Cancel;

    function IsEmpty: Boolean;

    procedure DisableControls;
    procedure EnableControls;

    function Locate(const _KeyFields: string; const _KeyValues: Variant; _Options: TLocateOptions): Boolean;
    procedure SetParamByName(const _Param: string; _Value: Variant); virtual;
    function TrySetParamByName(const _Param: string; _Value: Variant): Boolean; virtual;

    function GetFieldValue(const _Fieldname: string): Variant;
    procedure SetFieldValue(const _Fieldname: string; const _Value: Variant);
    function TrySetFieldValue(const _Fieldname: string; const _Value: Variant): Boolean;
    procedure SetFieldAsString(const _Fieldname: string; const _Value: string; _CheckLength: Boolean = True);
    procedure ClearField(const _Fieldname: string);
    function Fields: TFields;
    function HasField(const _Fieldname: string): Boolean;
    procedure ToNameValueList(_Values: TNameValueList; const _Ignore: array of string);
    procedure FromNameValueList(_Values: TNameValueList);

    property Active: Boolean read GetActive write SetActive;
    property FieldValues[const _Fieldname: string]: Variant read GetFieldValue write SetFieldValue; default;
  public
    constructor Create(_DataSet: TDataSet; const _Tablename: string);
    destructor Destroy; override;
  end;

function TDataset_DisableControls(_ds: TDataSet): IInterface;

type
{$IFDEF DATASET_GETFIELDS_IS_WIDESTRINGLIST}
  TFieldListStringsType = TWideStrings;
  TFieldListStringListType = TWideStringList;
  IFieldListStringsTypeGuard = IWideStringsGuard;
{$ELSE}
  TFieldListStringsType = TStrings;
  TFieldListStringListType = TStringList;
  IFieldListStringsTypeGuard = IStringsGuard;
{$ENDIF}

function TDataset_GetFieldList(_ds: TDataSet): IFieldListStringsTypeGuard;

implementation

uses
  Variants,
  u_dzMiscUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

type
  TDatasetDisableControlsGuard = class(TInterfacedObject, IInterface)
  private
    FDataset: TDataSet;
  public
    constructor Create(_ds: TDataSet);
    destructor Destroy; override;
  end;

{ TDatasetDisableControlsGuard }

constructor TDatasetDisableControlsGuard.Create(_ds: TDataSet);
begin
  inherited Create;
  FDataset := _ds;
  FDataset.DisableControls;
end;

destructor TDatasetDisableControlsGuard.Destroy;
begin
  if Assigned(FDataset) then
    FDataset.EnableControls;
  inherited;
end;

function TDataset_DisableControls(_ds: TDataSet): IInterface;
begin
  Result := TDatasetDisableControlsGuard.Create(_ds);
end;

function TDataset_GetFieldList(_ds: TDataSet): IFieldListStringsTypeGuard;
var
  sl: TFieldListStringListType;
begin
  sl := TFieldListStringListType.Create;
  try
    _ds.GetFieldNames(sl);
    Result := Guard(sl);
    sl := nil;
  finally
    FreeAndNil(sl);
  end;
end;

{ TDatasetHelper }

constructor TDatasetHelper.Create(_DataSet: TDataSet; const _Tablename: string);
begin
  inherited Create;
  FDataset := _DataSet;
  FTableName := _Tablename;
  if FDataset.Active then
    InitSortedFields;
end;

procedure TDatasetHelper.Delete;
begin
  FDataset.Delete;
end;

destructor TDatasetHelper.Destroy;
begin
  FreeAndNil(FSortedFields);
  inherited;
end;

function TDatasetHelper.GetDatasetName: string;
begin
  Result := FTableName;
end;

function TDatasetHelper.FieldByName(const _Fieldname: string): TField;
var
  idx: Integer;
begin
  Assert(Assigned(FSortedFields), 'FSortedFields is not assigned');

  if FSortedFields.Find(_Fieldname, idx) then
    Result := FDataset.Fields[Integer(FSortedFields.Objects[idx])]
  else
    Result := FDataset.FindField(_Fieldname);
  if not Assigned(Result) then
    raise EDatabaseError.CreateFmt(_('Field "%s" not found in table "%s".'), [_Fieldname, GetDatasetName]);
end;

{$IF Declared(TryVar2DateTime)}
function TDatasetHelper.FieldAsDate(const _Fieldname: string): TDateTime;
begin
  Result := Var2DateTimeEx(FieldByName(_Fieldname).Value, GetDatasetName + '.' + _Fieldname);
end;

function TDatasetHelper.FieldAsDate(const _Fieldname: string; _Default: TDateTime): TDateTime;
begin
  if not TryFieldAsDate(_Fieldname, Result) then
    Result := _Default;
end;

function TDatasetHelper.TryFieldAsDate(const _Fieldname: string; out _Date: TDateTime): Boolean;
begin
  Result := not IsEmpty and TryVar2DateTime(FieldByName(_Fieldname).Value, _Date);
end;
{$IFEND}

function TDatasetHelper.TryFieldAsExtended(const _Fieldname: string; out _Value: Extended): Boolean;
begin
  Result := not IsEmpty and TryVar2Ext(FieldByName(_Fieldname).Value, _Value);
end;

{$IF Declared(TNullableGuid)}
function TDatasetHelper.TryFieldAsGuid(const _Fieldname: string; out _Value: TNullableGuid): Boolean;
begin
  if IsEmpty then
    Result := False
  else begin
    _Value.AssignVariant(FieldByName(_Fieldname).Value);
    Result := _Value.IsValid;
  end;
end;
{$IFEND}

function TDatasetHelper.TryFieldAsInteger(_Field: TField; out _Value: Integer): Boolean;
begin
  Result := not IsEmpty and TryVar2Int(_Field.Value, _Value);
end;

function TDatasetHelper.TryFieldAsInteger(const _Fieldname: string; out _Value: Integer): Boolean;
begin
  Result := not IsEmpty and TryVar2Int(FieldByName(_Fieldname).Value, _Value);
end;

function TDatasetHelper.FieldAsDouble(const _Fieldname: string): Double;
begin
  Result := Var2DblEx(FieldByName(_Fieldname).Value, GetDatasetName + '.' + _Fieldname);
end;

function TDatasetHelper.FieldAsDouble(const _Fieldname, _Error: string): Double;
begin
  Result := Var2DblEx(FieldByName(_Fieldname).Value, _Error);
end;

function TDatasetHelper.FieldAsExtended(const _Fieldname: string): Extended;
begin
  Result := Var2ExtEx(FieldByName(_Fieldname).Value, GetDatasetName + '.' + _Fieldname);
end;

function TDatasetHelper.FieldAsExtended(const _Fieldname: string; const _Default: Extended): Extended;
begin
  Result := Var2Ext(FieldByName(_Fieldname).Value, _Default);
end;

function TDatasetHelper.FieldAsExtended(const _Fieldname, _Error: string): Extended;
begin
  Result := Var2ExtEx(FieldByName(_Fieldname).Value, _Error);
end;

{$IF Declared(TNullableGuid)}
function TDatasetHelper.FieldAsGuid(const _Fieldname: string): TNullableGuid;
begin
  Result.AssignVariant(FieldByName(_Fieldname).Value);
end;
{$IFEND}

function TDatasetHelper.FieldAsInteger(const _Fieldname: string): Integer;
begin
  Result := Var2IntEx(FieldByName(_Fieldname).Value, GetDatasetName + '.' + _Fieldname);
end;

function TDatasetHelper.FieldAsInteger(_Field: TField): Integer;
begin
  Result := Var2IntEx(_Field.Value, GetDatasetName + '.' + _Field.FieldName);
end;

function TDatasetHelper.FieldAsInteger(const _Fieldname, _Error: string): Integer;
begin
  Result := Var2IntEx(FieldByName(_Fieldname).Value, _Error);
end;

function TDatasetHelper.FieldAsInteger(_Field: TField; const _Error: string): Integer;
begin
  Result := Var2IntEx(_Field.Value, _Error);
end;

function TDatasetHelper.FieldAsString(_Field: TField): string;
begin
  Result := Trim(Var2StrEx(_Field.Value, GetDatasetName + '.' + _Field.FieldName));
end;

function TDatasetHelper.FieldAsString(_Field: TField; const _Default: string): string;
begin
  Result := Trim(Var2Str(_Field.Value, _Default));
end;

function TDatasetHelper.FieldAsString(const _Fieldname: string): string;
begin
  Result := Trim(Var2StrEx(FieldByName(_Fieldname).Value, GetDatasetName + '.' + _Fieldname));
end;

function TDatasetHelper.TryFieldAsString(_Field: TField; out _Value: string): Boolean;
begin
  Result := not IsEmpty and TryVar2Str(_Field.Value, _Value);
end;

function TDatasetHelper.TryFieldAsString(const _Fieldname: string; out _Value: string): Boolean;
begin
  Result := not IsEmpty and TryVar2Str(FieldByName(_Fieldname).Value, _Value);
end;

function TDatasetHelper.TryFieldAsNonEmptyString(const _Fieldname: string; out _Value: string): Boolean;
begin
  Result := TryFieldAsString(_Fieldname, _Value) and (_Value <> '');
end;

function TDatasetHelper.FieldAsBoolean(const _Fieldname: string): Boolean;
begin
  Result := FieldAsInteger(_Fieldname) <> 0;
end;

function TDatasetHelper.FieldAsDouble(const _Fieldname: string; const _Default: Double): Double;
begin
  if not TryFieldAsDouble(_Fieldname, Result) then
    Result := _Default;
end;

function TDatasetHelper.TryFieldAsDouble(const _Fieldname: string; out _Value: Double): Boolean;
begin
  Result := not IsEmpty and TryVar2Dbl(FieldByName(_Fieldname).Value, _Value);
end;

function TDatasetHelper.FieldAsInteger(const _Fieldname: string; _Default: Integer): Integer;
begin
  if not TryFieldAsInteger(_Fieldname, Result) then
    Result := _Default;
end;

function TDatasetHelper.FieldAsInteger(_Field: TField; _Default: Integer): Integer;
begin
  if not TryFieldAsInteger(_Field, Result) then
    Result := _Default;
end;

function TDatasetHelper.FieldAsString(const _Fieldname, _Default: string): string;
begin
  Result := Trim(Var2Str(FieldByName(_Fieldname).Value, _Default));
end;

function TDatasetHelper.FieldAsBoolean(const _Fieldname: string; _Default: Boolean): Boolean;
begin
  Result := FieldAsInteger(_Fieldname, BoolToInt(_Default)) <> 0;
end;

procedure TDatasetHelper.SetFieldAsString(const _Fieldname, _Value: string; _CheckLength: Boolean);
var
  fld: TField;
  MaxLen: Integer;
  Len: Integer;
begin
  fld := FieldByName(_Fieldname);
  if _CheckLength then begin
    MaxLen := fld.Size;
    Len := Length(_Value);
    if Len > MaxLen then
      raise Exception.CreateFmt(_('Field %s.%s is not large enough to hold value "%s". (Field size: %d, Value length: %d)'),
        [GetDatasetName, _Fieldname, _Value, MaxLen, Len]);
  end;
  fld.AsString := _Value;
end;

procedure TDatasetHelper.SetFieldStringNotEmpty(const _Fieldname, _Value: string);
begin
  if _Value = '' then
    FieldByName(_Fieldname).Clear
  else
    FieldByName(_Fieldname).Value := _Value;
end;

procedure TDatasetHelper.ClearField(const _Fieldname: string);
begin
  FieldByName(_Fieldname).Clear;
end;

procedure TDatasetHelper.Close;
begin
  FreeAndNil(FSortedFields);
  FDataset.Close;
end;

procedure TDatasetHelper.ToNameValueList(_Values: TNameValueList; const _Ignore: array of string);

  function IsIgnored(const _s: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := Low(_Ignore) to High(_Ignore) do begin
      if SameText(_Ignore[i], _s) then begin
        Result := True;
        Exit;
      end;
    end;
  end;

var
  i: Integer;
  Field: TField;
  FieldName: string;
  Value: string;
begin
  Assert(Assigned(_Values));

  for i := 0 to Fields.Count - 1 do begin
    Field := Fields[i];
    FieldName := Field.FieldName;
    if not IsIgnored(FieldName) then
      if TryFieldAsString(FieldName, Value) then
        _Values.ByName[FieldName] := Value;
  end;
end;

procedure TDatasetHelper.FromNameValueList(_Values: TNameValueList);
var
  i: Integer;
begin
  for i := 0 to _Values.Count - 1 do
    FieldValues[_Values[i].Name] := _Values[i].Value;
end;

function TDatasetHelper.Eof: Boolean;
begin
  Result := FDataset.Eof;
end;

procedure TDatasetHelper.Append;
begin
  FDataset.Append;
end;

function TDatasetHelper.Bof: Boolean;
begin
  Result := FDataset.Bof;
end;

procedure TDatasetHelper.First;
begin
  FDataset.First;
end;

function TDatasetHelper.Next: Boolean;
begin
  FDataset.Next;
  Result := not FDataset.Eof;
end;

function TDatasetHelper.Prior: Boolean;
begin
  FDataset.Prior;
  Result := not FDataset.Bof;
end;

procedure TDatasetHelper.Open;
begin
  FDataset.Open;
  InitSortedFields;
end;

function TDatasetHelper.GetActive: Boolean;
begin
  Result := FDataset.Active;
end;

procedure TDatasetHelper.InitSortedFields;
var
  i: Integer;
begin
  FreeAndNil(FSortedFields);
  FSortedFields := TStringList.Create;
  FSortedFields.Sorted := True;
  for i := 0 to FDataset.FieldCount - 1 do
    FSortedFields.AddObject(FDataset.Fields[i].FullName, Pointer(i));
end;

procedure TDatasetHelper.SetActive(const _Value: Boolean);
begin
  FDataset.Active := _Value;
end;

function TDatasetHelper.GetFieldValue(const _Fieldname: string): Variant;
begin
  Result := FieldByName(_Fieldname).Value;
end;

function TDatasetHelper.HasField(const _Fieldname: string): Boolean;
begin
  Result := (FDataset.FindField(_Fieldname) <> nil);
end;

function TDatasetHelper.Fields: TFields;
begin
  Result := FDataset.Fields;
end;

procedure TDatasetHelper.SetFieldValue(const _Fieldname: string; const _Value: Variant);
begin
  FieldByName(_Fieldname).Value := _Value;
end;

function TDatasetHelper.TrySetFieldValue(const _Fieldname: string; const _Value: Variant): Boolean;
var
  Field: TField;
begin
  Field := FDataset.FindField(_Fieldname);
  Result := Assigned(Field);
  if Result then
    Field.Value := _Value;
end;

procedure TDatasetHelper.SetParamByName(const _Param: string; _Value: Variant);
begin
  raise Exception.CreateFmt(_('SetParamByName is not supported for a %s.'), [FDataset.ClassName]);
end;

function TDatasetHelper.TrySetParamByName(const _Param: string; _Value: Variant): Boolean;
begin
  raise Exception.CreateFmt(_('TrySetParamByName is not supported for a %s.'), [FDataset.ClassName]);
end;

procedure TDatasetHelper.Cancel;
begin
  FDataset.Cancel;
end;

procedure TDatasetHelper.Edit;
begin
  FDataset.Edit;
end;

procedure TDatasetHelper.Insert;
begin
  FDataset.Insert;
end;

function TDatasetHelper.IsEmpty: Boolean;
begin
  Result := FDataset.IsEmpty;
end;

procedure TDatasetHelper.EnableControls;
begin
  FDataset.EnableControls;
end;

procedure TDatasetHelper.DisableControls;
begin
  FDataset.DisableControls;
end;

procedure TDatasetHelper.Last;
begin
  FDataset.Last;
end;

function TDatasetHelper.Locate(const _KeyFields: string; const _KeyValues: Variant;
  _Options: TLocateOptions): Boolean;
begin
  Result := FDataset.Locate(_KeyFields, _KeyValues, _Options);
end;

function TDatasetHelper.MoveBy(_Distance: Integer): Integer;
begin
  Result := FDataset.MoveBy(_Distance);
end;

procedure TDatasetHelper.Post;
begin
  FDataset.Post;
end;

end.

