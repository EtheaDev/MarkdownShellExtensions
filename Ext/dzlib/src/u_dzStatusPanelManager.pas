unit u_dzStatusPanelManager;

interface

uses
  SysUtils,
  Classes,
  Comctrls,
  u_dzTranslator;

type
  TStatusPanelManager = class
  private
    FStatusBar: TStatusbar;
    FPanelNames: TStringList;
    function GetTexts(const _Name: string): string;
    procedure SetTexts(const _Name, _Value: string);
    function GetIndex(const _Name: string): Integer;
  public
    constructor Create(_StatusBar: TStatusbar);
    destructor Destroy; override;
    procedure Add(const _Name: string; _Width: Integer = 150);
    procedure Delete(const _Name: string);
    function HasPanel(const _Name: string): Boolean;
    property Texts[const _Name: string]: string read GetTexts write SetTexts; default;
  end;

implementation

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE}inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, DZLIB_TRANSLATION_DOMAIN);
end;

{ TStatusPanelManager }

constructor TStatusPanelManager.Create(_StatusBar: TStatusbar);
begin
  inherited Create;
  FStatusBar := _StatusBar;
  FPanelNames := TStringList.Create;
end;

destructor TStatusPanelManager.Destroy;
begin
  FreeAndNil(FPanelNames);
  inherited;
end;

procedure TStatusPanelManager.Add(const _Name: string; _Width: Integer);
var
  Idx: Integer;
  pnl: TStatusPanel;
begin
  Idx := FPanelNames.IndexOf(_Name);
  if Idx <> -1 then
    raise Exception.Create(_('List does not allow duplicates'));

  FPanelNames.Add(_Name);
  pnl := FStatusBar.Panels.Add;
  pnl.Width := _Width;
end;

procedure TStatusPanelManager.Delete(const _Name: string);
var
  Idx: Integer;
begin
  Idx := GetIndex(_Name);
  FPanelNames.Delete(Idx);
  FStatusBar.Panels.Delete(Idx);
end;

function TStatusPanelManager.GetIndex(const _Name: string): Integer;
begin
  Result := FPanelNames.IndexOf(_Name);
  if Result = -1 then
    raise Exception.CreateFmt(_('Entry "%s" not found in list.'), [_Name]);
end;

function TStatusPanelManager.GetTexts(const _Name: string): string;
var
  Idx: Integer;
begin
  Idx := GetIndex(_Name);
  Result := FStatusBar.Panels[Idx].Text;
end;

function TStatusPanelManager.HasPanel(const _Name: string): Boolean;
begin
  Result := (FPanelNames.IndexOf(_Name) <> -1);
end;

procedure TStatusPanelManager.SetTexts(const _Name, _Value: string);
var
  Idx: Integer;
begin
  Idx := GetIndex(_Name);
  FStatusBar.Panels[Idx].Text := _Value;
end;

end.
