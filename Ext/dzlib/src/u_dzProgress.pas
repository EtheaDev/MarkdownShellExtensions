unit u_dzProgress;

interface

uses
  Windows,
  SysUtils,
  Controls,
  w_dzProgress;

type
  IdzProgress = interface ['{68DDF5DC-D71B-4D9A-9C45-D65E9D259C32}']
    function GetAction: string;
    function GetIsActionVisible: boolean;
    function GetIsAbortVisible: boolean;
    function GetProgressMax: integer;
    function GetProgressPos: integer;
    procedure SetAction(const Value: string);
    procedure SetIsActionVisible(const Value: boolean);
    procedure SetIsAbortVisible(const Value: boolean);
    procedure SetProgressMax(const Value: integer);
    procedure SetProgressPos(const Value: integer);
    property ProgressPos: integer read GetProgressPos write SetProgressPos;
    property ProgressMax: integer read GetProgressMax write SetProgressMax;
    property Action: string read GetAction write SetAction;
    property IsAbortVisible: boolean read GetIsAbortVisible write SetIsAbortVisible;
    property IsActionVisible: boolean read GetIsActionVisible write SetIsActionVisible;
    procedure Progress(_Position: integer; const _Action: string {; var _Abort: boolean}); overload;
    procedure Progress(_Position: integer {; var _Abort: boolean}); overload;
  end;

type
  TdzProgress = class(TInterfacedObject, IdzProgress)
  private
    FForm: Tf_dzProgress;
  private // implement IdzProgress
    function GetAction: string;
    function GetIsActionVisible: boolean;
    function GetIsAbortVisible: boolean;
    function GetProgressMax: integer;
    function GetProgressPos: integer;
    procedure SetAction(const _Value: string);
    procedure SetIsActionVisible(const _Value: boolean);
    procedure SetIsAbortVisible(const _Value: boolean);
    procedure SetProgressMax(const _Value: integer);
    procedure SetProgressPos(const _Value: integer);
    procedure Progress(_Position: integer; const _Action: string {; var _Abort: boolean}); overload;
    procedure Progress(_Position: integer {; var _Abort: boolean}); overload;
  public
    constructor Create(_Owner: TWinControl; const _Caption: string);
    destructor Destroy; override;
  end;

implementation

uses
  u_dzVclUtils;

{ TdzProgress }

constructor TdzProgress.Create(_Owner: TWinControl; const _Caption: string);
begin
  inherited Create;
  FForm := Tf_dzProgress.Create(_Owner);
  TForm_CenterOn(FForm, _Owner);
  FForm.FormCaption := _Caption;
  FForm.Show;
end;

destructor TdzProgress.Destroy;
begin
  FreeAndNil(FForm);
  inherited;
end;

function TdzProgress.GetAction: string;
begin
  Result := FForm.Action;
end;

function TdzProgress.GetIsActionVisible: boolean;
begin
  Result := FForm.IsActionVisible;
end;

function TdzProgress.GetIsAbortVisible: boolean;
begin
  Result := FForm.IsAbortVisible;
end;

function TdzProgress.GetProgressMax: integer;
begin
  Result := FForm.ProgressMax;
end;

function TdzProgress.GetProgressPos: integer;
begin
  Result := FForm.ProgressPos;
end;

procedure TdzProgress.Progress(_Position: integer; const _Action: string {; var _Abort: boolean});
begin
  FForm.Progress(_Position, _Action {, _Abort});
end;

procedure TdzProgress.Progress(_Position: integer {; var _Abort: boolean});
begin
  FForm.Progress(_Position {, _Abort});
end;

procedure TdzProgress.SetAction(const _Value: string);
begin
  FForm.Action := _Value;
end;

procedure TdzProgress.SetIsActionVisible(const _Value: boolean);
begin
  FForm.IsActionVisible := _Value;
end;

procedure TdzProgress.SetIsAbortVisible(const _Value: boolean);
begin
  FForm.IsAbortVisible := _Value;
end;

procedure TdzProgress.SetProgressMax(const _Value: integer);
begin
  FForm.ProgressMax := _Value;
end;

procedure TdzProgress.SetProgressPos(const _Value: integer);
begin
  FForm.ProgressPos := _Value;
end;

end.

