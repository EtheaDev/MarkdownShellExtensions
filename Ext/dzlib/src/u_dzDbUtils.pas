unit u_dzDbUtils;

{$INCLUDE 'dzlib.inc'}

{$IFDEF BDE_IS_DEPRECATED}
{$IFNDEF NO_BDE_HINT}
{$MESSAGE HINT 'The BDE has been deprecated for a long time'}
{$ENDIF}
{$ENDIF}

interface

{$IFNDEF BDE_IS_DEPRECATED}
uses
  SysUtils,
  Classes,
  DB,
  DBTables,
  u_dzTranslator;

///<summary> Deletes the table file if it exists, only tested for dbase tables
///          @returns true, if successfull, false otherwise </summary>
function TTable_DeleteTable(_tbl: TTable): Boolean;

///<summary> Deletes all table indices, only tested for dbase tables </summary>
procedure TTable_DeleteAllIndices(_tbl: TTable);

///<summary> Force deletes all indices by patching the dbf file and deleting the .mdx file </summary>
procedure TDbaseTable_DeleteAllIndices(const _DbfFilename: string; _DeleteIdxFile: Boolean);

///<summary> Adds a field definition to a table </summary>
procedure TTable_AddFieldDef(_tbl: TTable; const _Name: string; _DataType: TFieldType; _Precision, _Size: Integer);
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}

{$ENDIF ~BDE_IS_DEPRECATED}

implementation

{$IFNDEF BDE_IS_DEPRECATED}
uses
  u_dzFileUtils;

function _(const _s: string): string;
begin
  Result := dzlibGetText(_s);
end;

function TTable_DeleteTable(_tbl: TTable): Boolean;
begin
  Result := FileExists(IncludeTrailingPathDelimiter(_tbl.DatabaseName) + _tbl.TableName);
  if Result then
    _tbl.DeleteTable;
end;

procedure TTable_DeleteAllIndices(_tbl: TTable);
var
  i: Integer;
  sl: TStringList;
begin
  if _tbl.TableName = '' then
    Exit;
  if _tbl.Active then
    _tbl.Active := False;
  _tbl.IndexName := '';
  sl := TStringList.Create;
  try
    _tbl.GetIndexNames(sl);
    for i := 0 to sl.Count - 1 do
      _tbl.DeleteIndex(sl[i]);
  finally
    sl.Free;
  end;
end;

procedure TDbaseTable_PatchIndexByte(const _Filename: string; _Value: Boolean);
const
  BYTE_TO_PATCH = 28;
var
  b: Byte;
  st: TFileStream;
  Pos: Int64;
begin
  st := TFileStream.Create(_Filename, fmOpenReadWrite or fmShareExclusive);
  try
    Pos := st.Seek(BYTE_TO_PATCH, soFromBeginning);
    if Pos <> BYTE_TO_PATCH then
      raise Exception.CreateFmt(_('Could not seek to byte %d'), [BYTE_TO_PATCH]);
    st.ReadBuffer(b, SizeOf(b));
    Pos := st.Seek(BYTE_TO_PATCH, soFromBeginning);
    if Pos <> BYTE_TO_PATCH then
      raise Exception.CreateFmt(_('Could not seek to byte %d'), [BYTE_TO_PATCH]);
    if _Value then
      b := 1
    else
      b := 0;
    st.WriteBuffer(b, SizeOf(b));
  finally
    FreeAndNil(st);
  end;
end;

procedure TDbaseTable_DeleteAllIndices(const _DbfFilename: string; _DeleteIdxFile: Boolean);
var
  IdxFile: string;
begin
  TDbaseTable_PatchIndexByte(_DbfFilename, False);

  IdxFile := ChangeFileExt(_DbfFilename, '.MDX');
  if TFileSystem.FileExists(IdxFile) then
    TFileSystem.DeleteFile(IdxFile)
end;

procedure TTable_AddFieldDef(_tbl: TTable; const _Name: string; _DataType: TFieldType; _Precision, _Size: Integer);
var
  fd: TFieldDef;
begin
  fd := _tbl.FieldDefs.AddFieldDef;
  fd.Name := _Name;
  fd.DataType := _DataType;
  fd.Precision := _Precision;
  fd.Size := _Size;
end;
{$ENDIF ~BDE_IS_DEPRECATED}

end.

