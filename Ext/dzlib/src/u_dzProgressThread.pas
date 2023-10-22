unit u_dzProgressThread;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes,
  u_dzErrorThread;

type
  ///<summary>
  /// Inherits from TErrorThread, a thread that handles exceptions and provides the error message
  /// as well as has a HasFinished property.
  /// In addition to that this class provides ProgressMax and ProgressPos properties </summary>
  /// NOTE: Do not override Execute, override doExecute instead. </summary>
  TProgressThread = class(TErrorThread)
  private
    function GetProgressPercent: Integer;
  protected
    FProgressMax: Integer;
    FProgressPos: Integer;
  public
    property ProgressMax: Integer read FProgressMax;
    property ProgressPos: Integer read FProgressPos;
    property ProgressPercent: Integer read GetProgressPercent;
  end;

{$DEFINE __DZ_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TProgressThread;
{$INCLUDE 't_dzObjectListTemplate.tpl'}

type
  ///<summary>
  /// List for storing TProgressThread items </summary>
  TProgressThreadList = class(_DZ_OBJECT_LIST_TEMPLATE_)
  public
    function StillRunning: Integer;
    procedure GetErrors(_sl: TStrings);
    function TotalProgressPercent: Integer;
    function TotalProgressMax: Int64;
    function TotalProgressPos: Int64;
  end;

implementation

{$INCLUDE 't_dzObjectListTemplate.tpl'}

{ TProgressThreadList }

procedure TProgressThreadList.GetErrors(_sl: TStrings);
var
  i: Integer;
  s: string;
begin
  for i := 0 to Count - 1 do begin
    s := Items[i].ErrorMessage;
    if s <> '' then begin
      UniqueString(s);
      _sl.Add(s);
    end;
  end;
end;

function TProgressThreadList.StillRunning: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if not Items[i].HasFinished then
      Inc(Result);
end;

function TProgressThreadList.TotalProgressMax: Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + Items[i].ProgressMax;
end;

function TProgressThreadList.TotalProgressPercent: Integer;
var
  TotalPos: Double;
  TotalMax: Int64;
begin
  TotalMax := TotalProgressMax;
  if TotalMax = 0 then
    Result := 0
  else begin
    TotalPos := TotalProgressPos;
    Result := Round(TotalPos * 100 / TotalMax);
  end;
end;

function TProgressThreadList.TotalProgressPos: Int64;
var
  i: Integer;
  Item: TProgressThread;
begin
  Result := 0;
  for i := 0 to Count - 1 do begin
    Item := Items[i];
    if Item.HasFinished then begin
      // just in case the thread has not set its ProgressPos to ProgressMax when it finished
      Result := Result + Item.ProgressMax;
    end else
      Result := Result + Item.ProgressPos;
  end;
end;

{ TProgressThread }

function TProgressThread.GetProgressPercent: Integer;
var
  PosDbl: Double;
begin
  if FProgressMax = 0 then
    Result := 0
  else begin
    PosDbl := ProgressPos;
    Result := Round(PosDbl * 100 / FProgressMax);
  end;
end;

end.
