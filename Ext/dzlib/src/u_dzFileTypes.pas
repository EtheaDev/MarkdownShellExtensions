unit u_dzFileTypes;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Registry,
  u_dzTranslator,
  u_dzQuicksort;

type
  TFileType = class
  private
    FExtension: string;
    FDescription: string;
    FInternalName: string;
  public
    constructor Create(const _Extension, _InternalName, _Description: string);
    property Extension: string read FExtension;
    property InternalName: string read FInternalName;
    property Description: string read FDescription;
  end;

{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TFileType;
  _KEY_TYPE_ = string;
{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

type
  {: List for storing TFileType items sorted by string }
  TFileTypeList = class(_DZ_SORTED_OBJECT_LIST_TEMPLATE_)
  protected
    {: return the key of an item for comparison }
    function KeyOf(const _Item: TFileType): string; override;
    {: compare the keys of two items, must return a value
       < 0 if Key1 < Key2, = 0 if Key1 = Key2 and > 0 if Key1 > Key2 }
    function Compare(const _Key1, _Key2: string): Integer; override;
  public
    constructor Create;
  end;

implementation

uses
  StrUtils,
  JclSysInfo,
  u_dzMiscUtils,
  u_dzStringUtils;

{ TFileType }

constructor TFileType.Create(const _Extension, _InternalName, _Description: string);
begin
  inherited Create;
  FExtension := _Extension;
  FInternalName := _InternalName;
  FDescription := _Description;
  if FDescription = '' then
    FDescription := Format(_('"%s" File'), [UpperCase(Copy(FExtension, 2))]);
end;

{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

function ReadResourceString(const _Module: string; _Idx: Integer): string;
const
  MaxResStringLen = 4096; // according to http://msdn.microsoft.com/en-us/library/cc194808.aspx
var
  Handle: HMODULE;
  Len: Integer;
begin
  Result := '';
  Handle := LoadLibrary(PChar(_Module));
  if Handle <> 0 then begin
    try
      SetLength(Result, MaxResStringLen + 1);
      Len := LoadString(Handle, _Idx, @Result[1], Length(Result));
      SetLength(Result, Len);
    finally
      FreeLibrary(Handle);
    end;
  end;
end;

procedure ExpandDescription(var _Description: string);
var
  p: Integer;
  ModuleName: string;
  IdxStr: string;
  idx: Integer;
  s: string;
begin
  if LeftStr(_Description, 1) <> '@' then
    Exit;
  if Pos(',-', _Description) = 0 then
    Exit;
  ExpandEnvironmentVar(_Description);
  p := Pos(',-', _Description);
  ModuleName := Copy(_Description, 2, p - 2);
  IdxStr := Copy(_Description, p + 2);
  if not TryStrToInt(IdxStr, idx) then
    Exit;
  s := ReadResourceString(ModuleName, idx);
  if s <> '' then
    _Description := s + '*';
end;

type
  TRegistryHack = class(TRegistry)
  end;

// Returns rdInteger, rdString and rdBinary as string

function TRegistry_GetDataAsString(_Reg: TRegistry; const _ValueName: string): string;
var
  Reg: TRegistryHack absolute _Reg;
  Info: TRegDataInfo;
  Buffer: array of Byte;
begin
  Result := '';
  if _Reg.GetDataInfo(_ValueName, Info) and (Info.DataSize > 0) then begin
    case Info.RegData of
      rdString, rdExpandString: begin
          SetString(Result, nil, Info.DataSize);
          Reg.GetData(_ValueName, PChar(Result), Info.DataSize, Info.RegData);
          Result := PChar(Result);
        end;
      rdInteger:
        Result := IntToStr(Reg.ReadInteger(_ValueName));
      rdBinary, rdUnknown: begin
          SetLength(Buffer, Info.DataSize);
          Reg.ReadBinaryData(_ValueName, Pointer(Buffer)^, Info.DataSize);
          Result := HexDump(Buffer, Length(Buffer));
        end;
    end;
  end;
end;

function ReadDescription(const _InternalName: string): string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKeyReadOnly('\' + _InternalName);
    try
      Result := Reg.ReadString('');
      if Result <> '' then
        Exit;
      if Reg.ValueExists('FriendlyTypeName') then
        Result := TRegistry_GetDataAsString(Reg, 'FriendlyTypeName');
    finally
      Reg.CloseKey;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

constructor TFileTypeList.Create;
var
  Reg: TRegistry;
  sl: TStringList;
  i: Integer;
  Extensions: TStringList;
  s: string;
  ext: string;
  InternalName: string;
  Description: string;
  IsAtLeastVista: Boolean;
  j: Integer;
begin
  inherited Create;

  IsAtLeastVista := (GetWindowsVersion in [wvWinVista, wvWinServer2008,
    wvWin7, wvWinServer2008R2, wvWin8, wvWin8RT, wvWinServer2012, wvWin81, wvWin81RT, wvWinServer2012R2]);
  { TODO -otwm : Is there any way to check for newer Windows versions even if they don't exist yet? }
//  IsAtLeastVista := True;

  Extensions := nil;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    sl := TStringList.Create;
    try
      Reg.OpenKeyReadOnly('\');
      try
        Reg.GetKeyNames(sl);
      finally
        Reg.CloseKey;
      end;
      Extensions := TStringList.Create;
      for i := 0 to sl.Count - 1 do begin
        s := sl[i];
        if LeftStr(s, 1) = '.' then
          Extensions.Add(s);
      end;
    finally
      FreeAndNil(sl);
    end;
    for i := 0 to Extensions.Count - 1 do begin
      ext := Extensions[i];
      Reg.OpenKeyReadOnly('\' + ext);
      try
        InternalName := Reg.ReadString('');
      finally
        Reg.CloseKey;
      end;
      if InternalName <> '' then begin
        Description := ReadDescription(InternalName);
        ExpandDescription(Description);
        Add(TFileType.Create(ext, InternalName, Description));
      end else if IsAtLeastVista then begin
        if Reg.OpenKeyReadOnly('\' + ext + '\OpenWithProgIDs') then begin
          try
            sl := TStringList.Create;
            try
              Reg.GetValueNames(sl);
              for j := 0 to sl.Count - 1 do begin
                InternalName := sl[j];
                Description := ReadDescription(InternalName);
                ExpandDescription(Description);
                Add(TFileType.Create(ext, InternalName, Description));
              end;
            finally
              FreeAndNil(sl);
            end;
          finally
            Reg.CloseKey;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(Extensions);
    FreeAndNil(Reg);
  end;
end;

function TFileTypeList.KeyOf(const _Item: TFileType): string;
begin
  Result := _Item.Extension;
end;

function TFileTypeList.Compare(const _Key1, _Key2: string): Integer;
begin
  Result := CompareText(_Key1, _Key2);
end;

end.

