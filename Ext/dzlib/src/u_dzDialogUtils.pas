unit u_dzDialogUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  Classes,
  Types,
  Controls,
  u_dzTranslator,
  u_dzTypes;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
type
  TFileFilter = record
  private
    FDescription: string;
    FMask: string;
  public
    procedure Init(const _Description, _Mask: string; _AddMaskToDesc: Boolean);
    property Description: string read FDescription;
    property Mask: string read FMask;
  end;

type
  TFileFilterArr = array of TFileFilter;

  PFileFilterBuilder = ^TFileFilterBuilder;
  TFileFilterBuilder = record
  private
  private
    FFilters: TFileFilterArr;
    FIncludeAllFiles: Boolean;
    FAllSupported: string;
  public
    function Init(_IncludeAllFiles: Boolean = True; const _AllSupported: string = ''): PFileFilterBuilder;
    ///<summary> Adds a new filter, the first one added is the default.
    ///          @param Description is the file type description e.g. 'Text file'. This descrition
    ///                             should be localized.
    ///          @param Mask is the file mask, e.g. '*.txt', it should not be localized.
    ///          @param AddMaskToDesc determines whether the mask is appended to the descripition as
    ///                               ' (*.bla)', defaults to true, e.g. 'Text file (*.txt)
    ///          @returns the interface itself, so chaining is possible like this:
    ///                   od.Filter := FileFilterHelper.AddFilter('bla', '*.bla').AddFilter('blub', '*.blu').Value;
    function Add(const _Description: string; const _Mask: string;
      _AddMaskToDesc: Boolean = True): PFileFilterBuilder;
    ///<summary>
    /// Adds an entry <descriptionfmt> (<maskfmt>)|<maskfmt>
    /// where both *fmts are passed through Format(*fmt, Values) </summary>
    function AddFmt(const _DescriptionFmt: string; const _MaskFmt: string;
      const _Values: array of const): PFileFilterBuilder;
    function AddAvi: PFileFilterBuilder;
    function AddBmp: PFileFilterBuilder;
    function AddCsv: PFileFilterBuilder;
    function AddDbf: PFileFilterBuilder;
    function AddEmf: PFileFilterBuilder;
    function AddExe: PFileFilterBuilder;
    function AddGif: PFileFilterBuilder;
    function AddHtml: PFileFilterBuilder;
    function AddIni: PFileFilterBuilder;
    function AddJpg: PFileFilterBuilder;
    function AddLog: PFileFilterBuilder;
    function AddMdb: PFileFilterBuilder;
    function AddOdt: PFileFilterBuilder;
    function AddOds: PFileFilterBuilder;
    function AddPdf: PFileFilterBuilder;
    function AddPicture: PFileFilterBuilder;
    function AddRtf: PFileFilterBuilder;
    function AddTiff: PFileFilterBuilder;
    function AddTxt: PFileFilterBuilder;
    function AddXml: PFileFilterBuilder;
    function AddXls: PFileFilterBuilder;
    function Value: string;
    function Filter: string; deprecated; // use Value instead
  end;

function FileFilterBuilder(_IncludeAllFiles: Boolean = True; const _AllSupported: string = ''): TFileFilterBuilder;
{$ENDIF}

function TOpenDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; _FileMustExist: Boolean = True): Boolean; overload;

function TOpenDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  const _fn: string; out _Files: TStringArray; _FileMustExist: Boolean = True): Boolean; overload;

function TSaveDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; const _DefaultExt: string = ''): Boolean;

implementation

uses
  Dialogs,
  SysUtils,
  u_dzVclUtils,
  u_dzStringUtils,
  u_dzStringArrayUtils;

function _(const _s: string): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := dzlibGetText(_s);
end;

function TOpenDialogXp_Init(_Owner: TWinControl; const _Title: string; const _Filter: string;
  const _fn: string; _FileMustExist: Boolean): TOpenDialog;
begin
  Result := TOpenDialog.Create(_Owner);
  try
    Result.Name := '';
    Result.Title := _Title;
    Result.Filter := _Filter;
    Result.Options := [ofHideReadOnly, ofPathMustExist, ofEnableSizing];
    if _FileMustExist then
      Result.Options := Result.Options + [ofFileMustExist];

    Result.FileName := ExtractFileName(_fn);
    Result.InitialDir := ExtractFileDir(_fn);
    Result.DefaultExt := '*';
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TOpenDialogXP_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; _FileMustExist: Boolean): Boolean; overload;
var
  od: TOpenDialog;
begin
  od := TOpenDialogXp_Init(_Owner, _Title, _Filter, _fn, _FileMustExist);
  try
    Result := od.Execute({$IFDEF OPENDIALOG_EXCUTE_HAS_HANDLE}_Owner.Handle{$ENDIF});
    if not Result then
      Exit;
    _fn := od.FileName;
  finally
    FreeAndNil(od);
  end;
end;

function TOpenDialogXP_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  const _fn: string; out _Files: TStringArray; _FileMustExist: Boolean): Boolean; overload;
var
  od: TOpenDialog;
begin
  od := TOpenDialogXp_Init(_Owner, _Title, _Filter, _fn, _FileMustExist);
  try
    od.Options := od.Options + [ofAllowMultiSelect];
    Result := od.Execute({$IFDEF OPENDIALOG_EXCUTE_HAS_HANDLE}_Owner.Handle{$ENDIF});
    if not Result then
      Exit; //==>

    _Files := TStringArray_FromStrings(od.Files);
  finally
    FreeAndNil(od);
  end;
end;

{$IF Declared(TFileOpenDialog)}
function TFileOpenDialog_Init(_Owner: TWinControl; const _Title: string; const _Filter: string;
  const _fn: string; _FileMustExist: Boolean = True): TFileOpenDialog;
var
  FilterParts: TStringArray;
  i: Integer;
  FileType: TFileTypeItem;
begin
  Result := TFileOpenDialog.Create(_Owner);
  try
    Result.Name := '';
    Result.Options := [fdoPathMustExist];
    if _FileMustExist then
      Result.Options := Result.Options + [fdoFileMustExist];
    Result.Title := _Title;
    FilterParts := SplitString(_Filter, '|');
    i := 0;
    while i < Length(FilterParts) - 1 do begin
      FileType := Result.FileTypes.Add;
      FileType.DisplayName := FilterParts[i];
      FileType.FileMask := FilterParts[i + 1];
      Inc(i, 2);
    end;
    Result.FileName := ExtractFileName(_fn);
    Result.DefaultFolder := ExtractFileDir(_fn);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TFileOpenDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; _FileMustExist: Boolean = True): Boolean; overload;
var
  od: TFileOpenDialog;
begin
  od := TFileOpenDialog_Init(_Owner, _Title, _Filter, _fn, _FileMustExist);
  try
    Result := od.Execute;
    if not Result then
      Exit; //==>

    _fn := od.FileName;
  finally
    FreeAndNil(od);
  end;
end;

function TFileOpenDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  const _fn: string; out _Files: TStringArray; _FileMustExist: Boolean = True): Boolean; overload;
var
  od: TFileOpenDialog;
begin
  od := TFileOpenDialog_Init(_Owner, _Title, _Filter, _fn, _FileMustExist);
  try
    od.Options := od.Options + [fdoAllowMultiSelect];
    Result := od.Execute;
    if not Result then
      Exit; //==>

    _Files := TStringArray_FromStrings(od.Files);
  finally
    FreeAndNil(od);
  end;
end;
{$IFEND}

function TOpenDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; _FileMustExist: Boolean): Boolean;
begin
  TCommonDialog_CenterWithBackgroundThread;
{$IF Declared(TFileOpenDialog)}
  if Win32MajorVersion >= 6 then begin
    // only available on Windows Vista and later
    Result := TFileOpenDialog_Execute(_Owner, _Title, _Filter, _fn, _FileMustExist);
    Exit; //==>
  end;
{$IFEND}
  Result := TOpenDialogXP_Execute(_Owner, _Title, _Filter, _fn, _FileMustExist);
end;

function TOpenDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  const _fn: string; out _Files: TStringArray; _FileMustExist: Boolean = True): Boolean; overload;
begin
  TCommonDialog_CenterWithBackgroundThread;
{$IF Declared(TFileOpenDialog)}
  if Win32MajorVersion >= 6 then begin
    // only available on Windows Vista and later
    Result := TFileOpenDialog_Execute(_Owner, _Title, _Filter, _fn, _Files, _FileMustExist);
    Exit; //==>
  end;
{$IFEND}
  Result := TOpenDialogXP_Execute(_Owner, _Title, _Filter, _fn, _Files, _FileMustExist);
end;

function TSaveDialogXP_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; const _DefaultExt: string = ''): Boolean;
var
  sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(_Owner);
  try
    sd.Name := '';
    sd.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
    sd.Title := _Title;
    sd.Filter := _Filter;
    sd.FileName := ExtractFileName(_fn);
    sd.InitialDir := ExtractFileDir(_fn);
    sd.DefaultExt := _DefaultExt;
    Result := sd.Execute({$IFDEF OPENDIALOG_EXCUTE_HAS_HANDLE}_Owner.Handle{$ENDIF});
    if not Result then
      Exit; //==>
    _fn := sd.FileName;
  finally
    FreeAndNil(sd);
  end;
end;

{$IF Declared(TFileSaveDialog)}
function TFileSaveDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; const _DefaultExt: string = ''): Boolean;
var
  sd: TFileSaveDialog;
  FilterParts: TStringArray;
  i: Integer;
  FileType: TFileTypeItem;
begin
  sd := TFileSaveDialog.Create(_Owner);
  try
    sd.Name := '';
    sd.Options := [fdoOverWritePrompt, fdoNoReadOnlyReturn, fdoPathMustExist];
    sd.Title := _Title;
    sd.DefaultExtension := _DefaultExt;
    FilterParts := SplitString(_Filter, '|');
    i := 0;
    while i < Length(FilterParts) - 1 do begin
      FileType := sd.FileTypes.Add;
      FileType.DisplayName := FilterParts[i];
      FileType.FileMask := FilterParts[i + 1];
      Inc(i, 2);
    end;
    sd.FileName := ExtractFileName(_fn);
    sd.DefaultFolder := ExtractFileDir(_fn);
    Result := sd.Execute;
    if not Result then
      Exit; //==>
    _fn := sd.FileName;
  finally
    FreeAndNil(sd);
  end;
end;
{$IFEND}

function TSaveDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; const _DefaultExt: string = ''): Boolean;
begin
  TCommonDialog_CenterWithBackgroundThread;
{$IF Declared(TFileSaveDialog)}
  if Win32MajorVersion >= 6 then begin
    // only available on Windows Vista and later
    Result := TFileSaveDialog_Execute(_Owner, _Title, _Filter, _fn, _DefaultExt);
    Exit; //==>
  end;
{$IFEND}
  // fallback for older Delphi versions and Windows before Vista
  Result := TSaveDialogXP_Execute(_Owner, _Title, _Filter, _fn, _DefaultExt);
end;

{$IF Declared(TFileFilterBuilder)}
function FileFilterBuilder(_IncludeAllFiles: Boolean = True; const _AllSupported: string = ''): TFileFilterBuilder;
begin
  Result.Init(_IncludeAllFiles, _AllSupported);
end;

{ TFileFilterBuilder }

function TFileFilterBuilder.Init(_IncludeAllFiles: Boolean;
  const _AllSupported: string): PFileFilterBuilder;
begin
  FIncludeAllFiles := _IncludeAllFiles;
  FAllSupported := _AllSupported;
  Result := @Self;
end;

function TFileFilterBuilder.Add(const _Description, _Mask: string;
  _AddMaskToDesc: Boolean): PFileFilterBuilder;
var
  Idx: Integer;
begin
  Idx := Length(FFilters);
  SetLength(FFilters, Idx + 1);
  FFilters[Idx].Init(_Description, _Mask, _AddMaskToDesc);
  Result := @Self;
end;

function TFileFilterBuilder.AddFmt(const _DescriptionFmt, _MaskFmt: string;
  const _Values: array of const): PFileFilterBuilder;
begin
  Result := Add(Format(_DescriptionFmt, _Values), Format(_MaskFmt, _Values));
end;

function TFileFilterBuilder.AddAvi: PFileFilterBuilder;
begin
  Result := Add(_('AVI files'), '*.AVI');
end;

function TFileFilterBuilder.AddBmp: PFileFilterBuilder;
begin
  Result := Add(_('Bitmap Files'), '*.BMP');
end;

function TFileFilterBuilder.AddCsv: PFileFilterBuilder;
begin
  Result := Add(_('Comma-separated values'), '*.CSV');
end;

function TFileFilterBuilder.AddDbf: PFileFilterBuilder;
begin
  Result := Add(_('DBase tables'), '*.DBF');
end;

function TFileFilterBuilder.AddEmf: PFileFilterBuilder;
begin
  Result := Add(_('Windows Enhanced Metafile'), '*.EMF');
end;

function TFileFilterBuilder.AddExe: PFileFilterBuilder;
begin
  Result := Add(_('Executable Files'), '*.EXE');
end;

function TFileFilterBuilder.AddGif: PFileFilterBuilder;
begin
  Result := Add(_('GIF Image'), '*.GIF');
end;

function TFileFilterBuilder.AddHtml: PFileFilterBuilder;
begin
  Result := Add(_('Hypertext Markup Language'), '*.html');
end;

function TFileFilterBuilder.AddIni: PFileFilterBuilder;
begin
  Result := Add(_('INI files'), '*.INI');
end;

function TFileFilterBuilder.AddJpg: PFileFilterBuilder;
begin
  Result := Add(_('JPEG Files'), '*.jpg;*.jpeg');
end;

function TFileFilterBuilder.AddLog: PFileFilterBuilder;
begin
  Result := Add(_('Log files'), '*.LOG');
end;

function TFileFilterBuilder.AddMdb: PFileFilterBuilder;
begin
  Result := Add(_('Microsoft Access Databases'), '*.mdb');
end;

function TFileFilterBuilder.AddOds: PFileFilterBuilder;
begin
  Result := Add(_('Open Document Spreadsheet'), '*.ODS');
end;

function TFileFilterBuilder.AddOdt: PFileFilterBuilder;
begin
  Result := Add(_('Open Document Text'), '*.ODT');
end;

function TFileFilterBuilder.AddPdf: PFileFilterBuilder;
begin
  Result := Add(_('Portable Document Format'), '*.PDF');
end;

function TFileFilterBuilder.AddPicture: PFileFilterBuilder;
begin
  Result := Add(_('Picture files'), '*.bmp;*.jpg;*.jpeg').AddBmp.AddJpg;
end;

function TFileFilterBuilder.AddRtf: PFileFilterBuilder;
begin
  Result := Add(_('Rich Text Format'), '*.RTF');
end;

function TFileFilterBuilder.AddTiff: PFileFilterBuilder;
begin
  Result := Add(_('Tagged Image File Format'), '*.TIF;*.TIFF');
end;

function TFileFilterBuilder.AddTxt: PFileFilterBuilder;
begin
  Result := Add(_('Text files'), '*.TXT');
end;

function TFileFilterBuilder.AddXls: PFileFilterBuilder;
begin
  Result := Add(_('Microsoft Excel File Format'), '*.XLS');
end;

function TFileFilterBuilder.AddXml: PFileFilterBuilder;
begin
  Result := Add(_('Extensible Markup Language'), '*.XML');
end;

function TFileFilterBuilder.Filter: string;
begin
  Result := Value;
end;

function TFileFilterBuilder.Value: string;

  procedure AddToResult(const _Description, _Mask: string); overload;
  begin
    if Result <> '' then
      Result := Result + '|';
    Result := Result + _Description + '|' + _Mask;
  end;

  procedure AddToResult(_Filter: TFileFilter); overload;
  begin
    AddToResult(_Filter.Description, _Filter.Mask);
  end;

var
  i: Integer;
  s: string;
begin
  Result := '';

  if FAllSupported <> '' then begin
    s := '';
    for i := Low(FFilters) to High(FFilters) do begin
      if s <> '' then
        s := s + ';';
      s := s + FFilters[i].Mask;
    end;
    AddToResult(FAllSupported, s);
  end;

  for i := Low(FFilters) to High(FFilters) do begin
    AddToResult(FFilters[i]);
  end;

  if FIncludeAllFiles then
    AddToResult(_('all files') + ' (*.*)', '*.*');
end;

{ TFileFilter }

procedure TFileFilter.Init(const _Description, _Mask: string; _AddMaskToDesc: Boolean);
begin
  FDescription := _Description;
  FMask := _Mask;
  if _AddMaskToDesc then
    FDescription := FDescription + ' (' + _Mask + ')';
end;
{$IFEND}

// This would have been too easy: In theory setting this to true would
// show the "new" TFileOpen/SaveDialogs available in Windows Vista+ even when using TOpen/SaveDialog
// with a fallback to the old dialogs for Windows XP.
// Unfortunately this apparently only works if styling is enabled for the program.
initialization
//{$IF Declared(UseLatestCommonDialogs)}
//  UseLatestCommonDialogs := True;
//{$IFEND}
end.
