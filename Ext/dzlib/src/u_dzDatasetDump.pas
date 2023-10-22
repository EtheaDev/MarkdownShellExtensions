///<summary>
/// Add this unit only for debug purposes.
/// It exports TDataset_Dump for dumping a dataset from the evaluate and modify dialog
/// of the Delphi debugger. </summary>
unit u_dzDatasetDump;

interface

uses
  SysUtils,
  Classes,
  DB;

///<summary>
/// @param Count:  1 -> Dump the current record only (default)
///               -1 -> Dump the whole dataset
///                0 -> Dump from the current record until EOF
///               >1 -> Dump a maximum of n records starting from the current
/// @returns the file name the data was written to </summary>
function TDataset_Dump(_ds: TDataset; _Count: Integer = 1): string;

implementation

uses
  u_dzLineBuilder,
  u_dzVariantUtils,
  u_dzFileUtils,
  u_dzExecutor,
  u_dzOsUtils;

const
  DUMP_ALL = -1;

procedure TDataset_DumpHeaders(_ds: TDataset; _sl: TStrings);
var
  lb: TLineBuilder;
  i: Integer;
  fld: TField;
begin
  lb := TLineBuilder.Create;
  try
    for i := 0 to _ds.FieldCount - 1 do begin
      fld := _ds.Fields[i];
      lb.Add(fld.FieldName);
    end;
    _sl.Add(lb.Content)
  finally
    FreeAndNil(lb);
  end;
end;

procedure TDataset_DumpCurrent(_ds: TDataset; _sl: TStrings);
var
  lb: TLineBuilder;
  i: Integer;
  fld: TField;
begin
  lb := TLineBuilder.Create;
  try
    for i := 0 to _ds.FieldCount - 1 do begin
      fld := _ds.Fields[i];
      lb.Add(Var2Str(fld.Value));
    end;
    _sl.Add(lb.Content)
  finally
    FreeAndNil(lb);
  end;
end;

procedure TDataset_DumpToEof(_ds: TDataset; _sl: TStrings);
begin
  while not _ds.Eof do begin
    TDataset_DumpCurrent(_ds, _sl);
    _ds.Next;
  end;
end;

procedure TDataset_DumpAll(_ds: TDataset; _sl: TStrings);
begin
  _ds.First;
  TDataset_DumpToEof(_ds, _sl);
end;

function TDataset_Dump(_ds: TDataset; _Count: Integer = 1): string;
var
  sl: TStringList;
  bm: TBookmark;
begin
  Result := '';
  sl := TStringList.Create;
  try
    if not Assigned(_ds) then begin
      sl.Add('<dataset not assigned>');
      if _Count = -1 then begin
        // do nothing this is a dummy call to fool the linker
        Exit; //==>
      end;
    end else if not _ds.Active then begin
      sl.Add('<dataset not active>');
    end else if _ds.IsEmpty then begin
      sl.Add('<dataset is empty>');
    end else begin
      bm := nil;
      _ds.DisableControls;
      try
        bm := _ds.GetBookmark;
        TDataset_DumpHeaders(_ds, sl);
        if _Count = DUMP_ALL then begin
          TDataset_DumpAll(_ds, sl);
        end else begin
          if _ds.Eof then begin
            sl.Add('<dataset is at eof>');
          end else if _Count = 0 then begin
            TDataset_DumpToEof(_ds, sl);
          end else begin
            while (not _ds.Eof) and (_Count > 0) do begin
              TDataset_DumpCurrent(_ds, sl);
              _ds.Next;
              Dec(_Count);
            end;
          end;
        end;
      finally
        _ds.GotoBookmark(bm);
        _ds.EnableControls;
      end;
    end;
    if Assigned(_ds) then
      Result := _ds.Name;
    if Result = '' then
      Result := 'NONAME';
    Result := itpd(TFileSystem.GetTempPath) + 'dump_of_' + Result + '.txt';
    sl.SaveToFile(Result);
    OpenFileWithAssociatedApp(Result, True);
  finally
    FreeAndNil(sl);
  end;
end;

initialization
  // Make sure the linker doesn't eliminate the function
  TDataset_Dump(nil, -1);
end.
