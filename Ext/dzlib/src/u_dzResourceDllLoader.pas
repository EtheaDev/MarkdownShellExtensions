{.GXFormatter.config=twm}
unit u_dzResourceDllLoader;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Contnrs,
  u_dzTranslator,
  u_dzVersionInfo,
  u_dzCustomDllLoader,
  u_dzObjectGuard;

type
  ///<summary> wrapper for the Load/FreeLibrary and GetProcAddress API calls </summary>
  TdzResourceDllLoader = class(TdzCustomDllLoader)
  private
    FKernelHandle: THandle;
    FMyHandle: THandle;
    function GetEntryPoint(const _EntryPoint: string): Pointer;
  protected
    ///<summary> Points to the loaded resourcedll </summary>
    FDllHandle: Pointer;
//    ///<summary> Version info of dll </summary>
//    FDllVersion: IFileInfo;
    procedure LoadDll; override;
    procedure UnloadDll; override;
  public
    ///<summary> calls GetEntryPoint and raises ENoEntryPoint if it returns nil
    ///          @param EntryPoint is the name of the entry point to get
    ///          @param DefaultFunc is a function pointer to assign if the entry point cannot be found
    ///                             if it is nil, an ENoEntryPoint exception will be raise in that case.
    ///                             Note: This function pointer must match the calling convention of
    ///                             the entry point and unless the calling convention is cdecl
    ///                             it must also match number of parameters of the entry point.
    ///                             See also the NotSupportedN functions in this unit.
    ///          @returns a pointer to the entry pointer
    ///          @raises ENoEntryPoint on failure </summary>
    function GetProcAddressEx(const _EntryPoint: string; _DefaultFunc: Pointer = nil): Pointer; overload; override;
    ///<summary> calls GetEntryPoint for MSC mangled entry points and raises ENoEntryPoint if it returns nil
    ///          @param EntryPoint is the name of the entry point to get
    ///          @param DWordParams is the number of DWord parameters of the entry point, used to
    ///                             generate the actual name of the entry point
    ///          @param DefaultFunc is a function pointer to assign if the entry point cannot be found
    ///                             if it is nil, an ENoEntryPoint exception will be raised in that case.
    ///                             Note: This function pointer must match the calling convention of
    ///                             the entry point and unless the calling convention is cdecl
    ///                             it must also match number of parameters of the entry point.
    ///                             See also the NotSupportedN functions in u_dzDllLoader.
    ///          @returns a pointer to the entry pointer
    ///          @raises ENoEntryPoint on failure </summary>
    function GetProcAddressEx(const _EntryPoint: string; _DWordParams: Integer; _DefaultFunc: Pointer = nil): Pointer; overload; override;
  public
    ///<summary> assumes that the dll has already been loaded and uses the given DllHandle,
    ///          NOTE: The destructor will call FreeLibrary anyway, so make sure you don't
    ///                store the dll handle anywhere else! </summary>
    constructor Create(const _DllName: string; _DllHandle: Pointer); overload;
    ///<summary> tries to load the given dll and raises EDllLoadError if it fails
    ///          @param DllName is the name of the dll to load, can contain absolute path
    ///          @raises EDllLoadError on failure </summary>
    constructor Create(const _DllName: string); overload;
    ///<summary> Generates a TVersionInfo object on demand and returns it </summary>
//    function DllVersion: IFileInfo;
    ///<summary> returns the full path of the dll that has been loaded </summary>
    function DllFilename: string; override;
    ///<summary> Returns a dummy TVersionInfo object </summary>
    function DllVersion: IFileInfo; override;
  end;

implementation

uses
  u_dzMiscUtils,
  u_dzStringUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE} inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TdzResourceDllLoader }

type
  TDllEntry = class
    LoadCount: Integer;
    FileName: string;
    Handle: Pointer;
    constructor Create(_FileName: string; _Handle: Pointer);
  end;

var
  gblDllListGuard: IInterface;
  gblInternalDllList: TObjectList;

function DllList: TObjectList;
begin
  if not Assigned(gblInternalDllList) then begin
    gblInternalDllList := TObjectList.Create;
    gblDllListGuard := TObjectGuard.Create(gblInternalDllList);
  end;
  Result := gblInternalDllList;
end;

constructor TdzResourceDllLoader.Create(const _DllName: string; _DllHandle: Pointer);
begin
  inherited Create;
  FDllName := ChangeFileExt(ExtractFileName(_DllName), '');
  FDllHandle := _DllHandle;
end;

constructor TdzResourceDllLoader.Create(const _DllName: string);
begin
  inherited Create(ChangeFileExt(ExtractFileName(_DllName), ''));
end;

function TdzResourceDllLoader.DllFilename: string;
begin
  Result := '@INMEM@\' + FDllName;
end;

function TdzResourceDllLoader.DllVersion: IFileInfo;
begin
  Result := TDummyFileInfo.Create;
end;

procedure TdzResourceDllLoader.LoadDll;

  procedure ChangeReloc(POrigBase, PBaseTemp, PReloc, PBaseTarget: Pointer; dwRelocSize: DWORD);
  type
    TRelocBlock = packed record
      dwAddress: DWORD;
      dwSize: DWORD;
    end;
    PRelocBlock = ^TRelocBlock;
  var
    pCurrentRelocBlock: PRelocBlock;
    RelocCount: DWORD;
    PCurrentStart: PWord;
    i: Integer;
    pRelocAddress: PInteger;
    iDif: Integer;
  begin
    pCurrentRelocBlock := PReloc;
    iDif := Integer(PBaseTarget) - Integer(POrigBase);
    PCurrentStart := Pointer(Integer(PReloc) + 8);
    while (not isBadReadPtr(pCurrentRelocBlock, SizeOf(TRelocBlock))) and
      (not isBadReadPtr(PCurrentStart, SizeOf(Pointer))) and
      (DWORD(pCurrentRelocBlock) < DWORD(PReloc) + dwRelocSize) do begin
      RelocCount := (pCurrentRelocBlock^.dwSize - 8) div SizeOf(Word);
      for i := 0 to RelocCount - 1 do begin // FI:W528 - i is not used in for loop code
        if (not isBadReadPtr(PCurrentStart, SizeOf(Pointer))) and
          (PCurrentStart^ xor $3000 < $1000) then begin
          pRelocAddress := Pointer(pCurrentRelocBlock^.dwAddress + PCurrentStart^ mod $3000 + DWORD(PBaseTemp));
          if (not isBadWritePtr(pRelocAddress, SizeOf(Integer))) then
            pRelocAddress^ := pRelocAddress^ + iDif;
        end;
        PCurrentStart := Pointer(DWORD(PCurrentStart) + SizeOf(Word));
      end;
      pCurrentRelocBlock := Pointer(PCurrentStart);
      PCurrentStart := Pointer(DWORD(PCurrentStart) + 8);
    end;
  end;

  procedure CreateImportTable(pLibraryHandle, pImportTable: Pointer);
  type
    TImportBlock = packed record
      dwCharacteristics: DWORD;
      dwTimeDateStamp: DWORD;
      dwForwarderChain: DWORD;
      dwName: DWORD;
      pFirstThunk: Pointer;
    end;
    PImportBlock = ^TImportBlock;
  var
    pIBlock: PImportBlock;
    pThunksRead: PDWord;
    pThunksWrite: PDWord;
    pDllName: PAnsiChar;
    dwLibraryHandle: DWORD;
    dwOldProtect: DWORD;
  begin
    pIBlock := pImportTable;
    while (not isBadReadPtr(pIBlock, SizeOf(TImportBlock))) and
      (pIBlock^.pFirstThunk <> nil) and (pIBlock^.dwName <> 0) do begin
      pDllName := Pointer(DWORD(pLibraryHandle) + DWORD(pIBlock^.dwName));
      if (not isBadReadPtr(pDllName, 4)) then begin
        dwLibraryHandle := LoadLibraryA(pDllName);
        pThunksRead := Pointer(DWORD(pIBlock^.pFirstThunk) + DWORD(pLibraryHandle));
        pThunksWrite := pThunksRead;
        if (DWORD(pIBlock^.dwTimeDateStamp) = $FFFFFFFF) then
          pThunksRead := Pointer(DWORD(pIBlock^.dwCharacteristics) + DWORD(pLibraryHandle));
        while (not isBadReadPtr(pThunksRead, SizeOf(DWORD))) and
          (not isBadReadPtr(pThunksWrite, SizeOf(Word))) and
          (pThunksRead^ <> 0) do begin
          if VirtualProtect(pThunksWrite, SizeOf(DWORD), PAGE_EXECUTE_READWRITE, dwOldProtect) then begin
            if (DWORD(pThunksRead^) and $80000000 <> 0) then
              pThunksWrite^ := DWORD(GetProcAddress(dwLibraryHandle, PAnsiChar(pThunksRead^ and $FFFF)))
            else
              pThunksWrite^ := DWORD(GetProcAddress(dwLibraryHandle, PAnsiChar(DWORD(pLibraryHandle) + pThunksRead^ + SizeOf(Word))));
            VirtualProtect(pThunksWrite, SizeOf(DWORD), dwOldProtect, dwOldProtect);
          end;
          Inc(pThunksRead);
          Inc(pThunksWrite);
        end;
      end;
      pIBlock := Pointer(DWORD(pIBlock) + SizeOf(TImportBlock));
    end;
  end;

var
  FPUControlWord: Word;
  Res, Glob: THandle;
  DllMain: function(dwHandle, dwReason, dwReserved: DWORD): DWORD; stdcall;
  IDH: PImageDosHeader;
  INH: PImageNtHeaders;
  SEC: PImageSectionHeader;
  dwSecCount: DWORD;
  dwmemsize: DWORD;
  i: Integer;
  pAll: Pointer;
  DllEntry: TDllEntry;
begin
  FDllHandle := nil;

  for i := DllList.Count - 1 downto 0 do begin
    DllEntry := DllList[i] as TDllEntry;
    if SameText(DllEntry.FileName, FDllName) then begin
      DllEntry.LoadCount := DllEntry.LoadCount + 1;
      FDllHandle := DllEntry.Handle;
      Exit; // ---> already loaded
    end;
  end;

  // save the FPU Control Word
  asm
    FNSTCW  FPUControlWord
  end;
  try
    FKernelHandle := GetModuleHandle('kernel32.dll');
    FMyHandle := GetModuleHandle(nil);

    Res := FindResource(FMyHandle, PChar(FDllName), 'RESOURCEDLL');
    if Res = 0 then
      raise EDllLoadError.CreateFmt(_('Could not find RESOURCEDLL "%s"'), [FDllName]);

    Glob := LoadResource(FMyHandle, Res);
    if Glob = 0 then
      raise EDllLoadError.CreateFmt(_('Could not load RESOURCEDLL "%s"'), [FDllName]);

    IDH := LockResource(Glob);
    if not Assigned(IDH) then
      raise EDllLoadError.CreateFmt(_('Could not lock RESOURCEDLL "%s"'), [FDllName]);

  // UnlockFresource and FreeResource are obsolete in Win32

    if (isBadReadPtr(IDH, SizeOf(TImageDosHeader))) or (IDH^.e_magic <> IMAGE_DOS_SIGNATURE) then
      raise EDllLoadError.CreateFmt(_('RESOURCEDLL "%s" is not valid (Check #1)'), [FDllName]);

    INH := Pointer(Cardinal(IDH) + Cardinal(IDH^._lfanew));
    if (isBadReadPtr(INH, SizeOf(TImageNtHeaders))) or (INH^.Signature <> IMAGE_NT_SIGNATURE) then
      raise EDllLoadError.CreateFmt(_('RESOURCEDLL "%s" is not valid (Check #2)'), [FDllName]);

    SEC := Pointer(Integer(INH) + SizeOf(TImageNtHeaders));
    dwmemsize := INH^.OptionalHeader.SizeOfImage;
    if (dwmemsize = 0) then
      raise EDllLoadError.CreateFmt(_('RESOURCEDLL "%s" is not valid (Check #3)'), [FDllName]);

    pAll := VirtualAlloc(nil, dwmemsize, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
    if (pAll = nil) then begin
      Exit;
    end;

    dwSecCount := INH^.FileHeader.NumberOfSections;
    CopyMemory(pAll, IDH, DWORD(SEC) - DWORD(IDH) + dwSecCount * SizeOf(TImageSectionHeader));
    for i := 0 to dwSecCount - 1 do begin // FI:W528 - i is not used in for loop code
      CopyMemory(Pointer(DWORD(pAll) + SEC^.VirtualAddress),
        Pointer(DWORD(IDH) + DWORD(SEC^.PointerToRawData)),
        SEC^.SizeOfRawData);
      SEC := Pointer(Integer(SEC) + SizeOf(TImageSectionHeader));
    end;

    if (INH^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress <> 0) then
      ChangeReloc(Pointer(INH^.OptionalHeader.ImageBase),
        pAll,
        Pointer(DWORD(pAll) + INH^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress),
        pAll,
        INH^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].Size);
    CreateImportTable(pAll, Pointer(DWORD(pAll) + INH^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress));

    @DllMain := Pointer(INH^.OptionalHeader.AddressOfEntryPoint + DWORD(pAll));
    if (INH^.OptionalHeader.AddressOfEntryPoint <> 0) then begin
      DllMain(DWORD(pAll), DLL_PROCESS_ATTACH, 0);
    end;
    FDllHandle := pAll;

    DllList.Add(TDllEntry.Create(FDllName, FDllHandle));

  finally
    // restore the FPU Control Word
    asm
        FNCLEX
        FLDCW FPUControlWord
    end;
  end;
end;

procedure TdzResourceDllLoader.UnloadDll;
var
  DllMain: function(dwHandle, dwReason, dwReserved: DWORD): DWORD; stdcall;
  IDH: PImageDosHeader;
  INH: PImageNtHeaders;
  i: Integer;
  DllEntry: TDllEntry;
begin
  IDH := FDllHandle;
  if (isBadReadPtr(IDH, SizeOf(TImageDosHeader))) or
    (IDH^.e_magic <> IMAGE_DOS_SIGNATURE) then begin
    Exit;
  end;

  INH := Pointer(Cardinal(IDH) + Cardinal(IDH^._lfanew));
  if (isBadReadPtr(INH, SizeOf(TImageNtHeaders))) or
    (INH^.Signature <> IMAGE_NT_SIGNATURE) then begin
    Exit;
  end;

  @DllMain := Pointer(INH^.OptionalHeader.AddressOfEntryPoint + DWORD(IDH));

  for i := DllList.Count - 1 downto 0 do begin
    DllEntry := (DllList[i] as TDllEntry);
    if DllEntry.Handle = IDH then begin
      DllEntry.LoadCount := DllEntry.LoadCount - 1;
      if DllEntry.LoadCount = 0 then begin
        if Assigned(DllMain) then
          DllMain(DWORD(IDH), DLL_PROCESS_DETACH, 0);

        VirtualFree(IDH, 0, MEM_RELEASE);
        DllList.Delete(i);
      end;
    end;
  end;
end;

function TdzResourceDllLoader.GetEntryPoint(const _EntryPoint: string): Pointer;
var
  NtHeader: PImageNtHeaders;
  DosHeader: PImageDosHeader;
  DataDirectory: PImageDataDirectory;
  ExportDirectory: PImageExportDirectory;
  i: Integer;
  iExportOrdinal: Integer;
  ExportName: string;
  dwPosDot: DWORD;
  dwNewmodule: DWORD;
  pFirstExportName: Pointer;
  pFirstExportAddress: Pointer;
  pFirstExportOrdinal: Pointer;
  pExportAddr: PDWord;
  pExportNameNow: PDWord;
  pExportOrdinalNow: PWord;
  Idx: Integer;
begin
  Result := nil;
  if (_EntryPoint = '') then
    Exit;

  DosHeader := FDllHandle;
  if (isBadReadPtr(DosHeader, SizeOf(TImageDosHeader)) or
    (DosHeader^.e_magic <> IMAGE_DOS_SIGNATURE)) then
    Exit; {Wrong PE (DOS) Header}

  NtHeader := Pointer(DWORD(DosHeader^._lfanew) + DWORD(DosHeader));
  if (isBadReadPtr(NtHeader, SizeOf(TImageNtHeaders)) or
    (NtHeader^.Signature <> IMAGE_NT_SIGNATURE)) then
    Exit; {Wrong PW (NT) Header}

  DataDirectory := @NtHeader^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];
  if (DataDirectory = nil) or (DataDirectory^.VirtualAddress = 0) then
    Exit; {Library has no exporttable}

  ExportDirectory := Pointer(DWORD(DosHeader) + DWORD(DataDirectory^.VirtualAddress));
  if isBadReadPtr(ExportDirectory, SizeOf(TImageExportDirectory)) then
    Exit;

  pFirstExportName := Pointer(DWORD(ExportDirectory^.AddressOfNames) + DWORD(DosHeader));
  pFirstExportOrdinal := Pointer(DWORD(ExportDirectory^.AddressOfNameOrdinals) + DWORD(DosHeader));
  pFirstExportAddress := Pointer(DWORD(ExportDirectory^.AddressOfFunctions) + DWORD(DosHeader));

  iExportOrdinal := -1; {if we dont find the correct ExportOrdinal}
  for i := 0 to ExportDirectory^.NumberOfNames - 1 do {for each export do}begin
    pExportNameNow := Pointer(Integer(pFirstExportName) + SizeOf(Pointer) * i);
    if (not isBadReadPtr(pExportNameNow, SizeOf(DWORD))) then begin
      ExportName := string(PAnsiChar(pExportNameNow^ + DWORD(DosHeader)));

      if Pos('@', _EntryPoint) = 0 then begin
        Idx := Pos('@', ExportName);
        if Idx > 0 then begin
          ExportName := Copy(ExportName, 1, Idx - 1);
          Idx := Pos('_', ExportName);
          if Idx = 1 then
            ExportName := Copy(ExportName, 2);
        end;
      end;

      if (ExportName = _EntryPoint) then {is it the export we search? Calculate the ordinal.}begin
        pExportOrdinalNow := Pointer(Integer(pFirstExportOrdinal) + SizeOf(Word) * i);
        if (not isBadReadPtr(pExportOrdinalNow, SizeOf(Word))) then
          iExportOrdinal := pExportOrdinalNow^;
      end;
    end;
  end;

  if (iExportOrdinal < 0) or (iExportOrdinal > Integer(ExportDirectory^.NumberOfFunctions)) then
    Exit; {havent found the ordinal}

  pExportAddr := Pointer(iExportOrdinal * 4 + Integer(pFirstExportAddress));
  if (isBadReadPtr(pExportAddr, SizeOf(DWORD))) then
    Exit;

  {Is the Export outside the ExportSection? If not its NT specific forwared function}
  if (pExportAddr^ < DWORD(DataDirectory^.VirtualAddress)) or
    (pExportAddr^ > DWORD(DataDirectory^.VirtualAddress + DataDirectory^.Size)) then begin
    if (pExportAddr^ <> 0) then {calculate export address}
      Result := Pointer(pExportAddr^ + DWORD(DosHeader));
  end else begin {forwarded function (like kernel32.EnterCriticalSection -> NTDLL.RtlEnterCriticalSection)}
    ExportName := string(PAnsiChar(LongWord(FDllHandle) + pExportAddr^));
    dwPosDot := Pos('.', ExportName);
    if (dwPosDot > 0) then begin
      dwNewmodule := GetModuleHandle(PChar(Copy(ExportName, 1, dwPosDot - 1)));
      if (dwNewmodule = 0) then
        dwNewmodule := LoadLibrary(PChar(Copy(ExportName, 1, dwPosDot - 1)));
      if (dwNewmodule <> 0) then
        Result := GetProcAddress(dwNewmodule, PChar(Copy(ExportName, dwPosDot + 1, Length(ExportName))));
    end;
  end;
end;

function TdzResourceDllLoader.GetProcAddressEx(const _EntryPoint: string;
  _DWordParams: Integer; _DefaultFunc: Pointer): Pointer;
begin
  Result := GetProcAddressEx(_EntryPoint, _DefaultFunc)
end;

function TdzResourceDllLoader.GetProcAddressEx(const _EntryPoint: string;
  _DefaultFunc: Pointer): Pointer;
var
  ErrCode: LongWord;
begin
  Result := GetEntryPoint(_EntryPoint);
  if not Assigned(Result) then begin
    if Assigned(_DefaultFunc) then
      Result := _DefaultFunc
    else begin
      ErrCode := GetLastError;
      RaiseLastOsErrorEx(ErrCode, Format(_('Could not find entry point %s in %s'#13#10'ERROR= %%d, %%s'), [_EntryPoint, FDllName]));
    end;
  end;
end;

{ TDllEntry }

constructor TDllEntry.Create(_FileName: string; _Handle: Pointer);
begin
  LoadCount := 1;
  FileName := _FileName;
  Handle := _Handle;
end;

initialization

finalization
  gblDllListGuard := nil;

end.
