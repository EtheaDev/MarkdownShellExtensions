unit u_dzPackageInfo;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils,
  Classes;

type
  TPackageInfo = class
  private
    FDescription: string;
    FUnits: TStringList;
    FRequired: TStringList;
    procedure PackageInfoMethod(const _Name: string; _NameType: TNameType; _Flags: Byte);
    function GetRequired: TStrings;
    function GetUnits: TStrings;
  public
    constructor Create(const _PackageName: string);
    destructor Destroy; override;
    property Description: string read FDescription;
    property Units: TStrings read GetUnits;
    property Required: TStrings read GetRequired;
  end;

implementation

{ TPackageInfo }

// this code is taken from SysUtils.GetPackageDescription, but uses an already opened HModule

function GetPackageDescription(ResModule: HModule): string;
var
  ResInfo: HRSRC;
  ResData: HGLOBAL;
begin
  ResInfo := FindResource(ResModule, 'DESCRIPTION', RT_RCDATA);
  if ResInfo <> 0 then begin
    ResData := LoadResource(ResModule, ResInfo);
    if ResData <> 0 then
      try
        Result := PWideChar(LockResource(ResData));
        UnlockResource(ResData);
      finally
        FreeResource(ResData);
      end;
  end;
end;

procedure PackageInfoProc(const _Name: string; _NameType: TNameType; _Flags: Byte; _Param: Pointer);
begin
  TPackageInfo(_Param).PackageInfoMethod(_Name, _NameType, _Flags);
end;

constructor TPackageInfo.Create(const _PackageName: string);
var
  PackageHandle: HModule;
  Flags: Integer;
begin
  FUnits := TStringList.Create;
  FRequired := TStringList.Create;

  PackageHandle := LoadLibraryEx(PChar(_PackageName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if PackageHandle <> 0 then begin
    try
      FDescription := GetPackageDescription(PackageHandle);
      FUnits.BeginUpdate;
      FRequired.BeginUpdate;
      GetPackageInfo(PackageHandle, Self, Flags, PackageInfoProc);
    finally
      FRequired.EndUpdate;
      FUnits.EndUpdate;
      FreeLibrary(PackageHandle);
    end;
  end;
end;

destructor TPackageInfo.Destroy;
begin
  FreeAndNil(FUnits);
  FreeAndNil(FRequired);
  inherited;
end;

function TPackageInfo.GetRequired: TStrings;
begin
  Result := FRequired;
end;

function TPackageInfo.GetUnits: TStrings;
begin
  Result := FUnits;
end;

procedure TPackageInfo.PackageInfoMethod(const _Name: string; _NameType: TNameType; _Flags: Byte);
begin
  case _NameType of
    ntContainsUnit: begin
        FUnits.Add(_Name);
      end;
    ntRequiresPackage: begin
        FRequired.Add(_Name);
      end;
    ntDcpBpiName: begin
        asm nop end;
      end;
  end;
end;

end.
