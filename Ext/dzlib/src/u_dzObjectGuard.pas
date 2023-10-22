unit u_dzObjectGuard;

interface

uses
  SysUtils,
  Classes;

type
  ///<summary>
  /// This class is meant to be overridden by descendants that guard a particular
  /// object class, e.g. a StringList (see u_dzStringsGuard) </summary>
  TObjectGuard = class(TInterfacedObject, IInterface)
  private
    FGuardedObject: TObject;
  protected
    property GuardedObject: TObject read FGuardedObject;
  public
    constructor Create(_GuardedObject: TObject);
    destructor Destroy; override;
  end;

type
  TMultiObjectGuard = class(TObjectGuard)
  public
    constructor Create(const _ObjArr: array of TObject);
  end;

function GuardObject(_Obj: TObject): IInterface;
function GuardObjects(const _ObjArr: array of TObject): IInterface;

implementation

uses
  Contnrs;

function GuardObject(_Obj: TObject): IInterface;
begin
  Result := TObjectGuard.Create(_Obj);
end;

function GuardObjects(const _ObjArr: array of TObject): IInterface;
begin
  Result := TMultiObjectGuard.Create(_ObjArr);
end;

{ TObjectGuard }

constructor TObjectGuard.Create(_GuardedObject: TObject);
begin
  inherited Create;
  FGuardedObject := _GuardedObject;
end;

destructor TObjectGuard.Destroy;
begin
  FreeAndNil(FGuardedObject);
  inherited;
end;

{ TMultiObjectGuard }

constructor TMultiObjectGuard.Create(const _ObjArr: array of TObject);
var
  List: TObjectList;
  i: Integer;
begin
  List := TObjectList.Create;
  inherited Create(List);
  for i := Low(_ObjArr) to High(_ObjArr) do
    List.Add(_ObjArr[i]);
end;

end.
