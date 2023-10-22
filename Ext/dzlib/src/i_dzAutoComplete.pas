unit i_dzAutoComplete;

interface

uses
  Windows,
  ActiveX,
  Classes,
  StdCtrls,
  Messages,
  u_dzVclUtils;

// declarations based on Ken White's answer on Stack Overflow:
// https://stackoverflow.com/a/5465826/49925

const
  IID_IAutoComplete: TGUID = '{00bb2762-6a77-11d0-a535-00c04fd7d062}';
  IID_IAutoComplete2: TGUID = '{EAC04BC0-3791-11d2-BB95-0060977B464C}';
  CLSID_IAutoComplete: TGUID = '{00BB2763-6A77-11D0-A535-00C04FD7D062}';

  IID_IACList: TGUID = '{77A130B0-94FD-11D0-A544-00C04FD7d062}';
  IID_IACList2: TGUID = '{470141a0-5186-11d2-bbb6-0060977b464c}';

  CLSID_ACLHistory: TGUID = '{00BB2764-6A77-11D0-A535-00C04FD7D062}';
  CLSID_ACListISF: TGUID = '{03C036F1-A186-11D0-824A-00AA005B4383}';
  CLSID_ACLMRU: TGUID = '{6756a641-de71-11d0-831b-00aa005b4383}';

type
  IACList = interface(IUnknown)
    ['{77A130B0-94FD-11D0-A544-00C04FD7d062}']
    function Expand(pszExpand: POLESTR): HResult; stdcall;
  end;

const
  //options for IACList2
  ACLO_NONE = 0; // don't enumerate anything
  ACLO_CURRENTDIR = 1; // enumerate current directory
  ACLO_MYCOMPUTER = 2; // enumerate MyComputer
  ACLO_DESKTOP = 4; // enumerate Desktop Folder
  ACLO_FAVORITES = 8; // enumerate Favorites Folder
  ACLO_FILESYSONLY = 16; // enumerate only the file system

type
  IACList2 = interface(IACList)
    ['{470141a0-5186-11d2-bbb6-0060977b464c}']
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(var pdwFlag: DWORD): HResult; stdcall;
  end;

  IAutoComplete = interface(IUnknown)
    ['{00bb2762-6a77-11d0-a535-00c04fd7d062}']
    function Init(hwndEdit: HWND; const punkACL: IUnknown;
      pwszRegKeyPath, pwszQuickComplete: POLESTR): HResult; stdcall;
    function Enable(fEnable: BOOL): HResult; stdcall;
  end;

const
  //options for IAutoComplete2
  ACO_NONE = 0;
  ACO_AUTOSUGGEST = $1;
  ACO_AUTOAPPEND = $2;
  ACO_SEARCH = $4;
  ACO_FILTERPREFIXES = $8;
  ACO_USETAB = $10;
  ACO_UPDOWNKEYDROPSLIST = $20;
  ACO_RTLREADING = $40;

type
  IAutoComplete2 = interface(IAutoComplete)
    ['{EAC04BC0-3791-11d2-BB95-0060977B464C}']
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(out pdwFlag: DWORD): HResult; stdcall;
  end;

type
  ///<summary>
  /// abstract ancestor class for classes implementing IEnumString
  /// Descendants must at least override Next, Skip and Reset. </summary>
  TEnumStringAbstract = class(TInterfacedObject, IEnumString)
  protected
    // IEnumString
    function Next(celt: LongInt; out elt; pceltFetched: PLongint): HResult; virtual; stdcall; abstract;
    function Skip(celt: LongInt): HResult; virtual; stdcall; abstract;
    function Reset: HResult; virtual; stdcall; abstract;
    function Clone(out enm: IEnumString): HResult; virtual; stdcall;
  end;

type
  ///<summary>
  /// abstract ancestor for a helper component used by u_dzAutoCompleteStrings and u_dzAutoCompleteDirs </summary>
  TAutoCompleteHelper = class(TWindowProcHook)
  protected
    FAutoComplete: IAutoComplete;
    function GetEdit: TCustomEdit;
    function CreateEnumStringInt: IEnumString; virtual; abstract;
    procedure WmNcCreate; override;
    // We cannot declare this as a message handler because WM_WINDOW_PROC_HOOK_HELPER
    // is not a constant but a variable initialized with RegisterWindowMessage.
    // So this method is simply called from NewWindowProc.
    procedure WmWindowProcHookHelper(var _Msg: TMessage); virtual;
    procedure NewWindowProc(var _Msg: TMessage); override;
    function SetAutoComplete: Boolean;
  public
    constructor Create(_ed: TCustomEdit); reintroduce;
  end;

implementation

uses
  ComObj,
  Controls;

var
  WM_WINDOW_PROC_HOOK_HELPER: Word = 0; // initialized on startup using RegisterWindowMessage

{ TEnumStringAbstract }

function TEnumStringAbstract.Clone(out enm: IEnumString): HResult;
begin
  Result := E_NOTIMPL;
  Pointer(enm) := nil;
end;

{ TAutoCompleteHelper }

constructor TAutoCompleteHelper.Create(_ed: TCustomEdit);
begin
  inherited Create(_ed);
//  Name := _ed.Name + 'AutoCompleteHelper';
  Self.Name := '';
  SetAutoComplete;
end;

function TAutoCompleteHelper.GetEdit: TCustomEdit;
begin
  Result := FCtrl as TCustomEdit;
end;

procedure TAutoCompleteHelper.NewWindowProc(var _Msg: TMessage);
begin
  if (_Msg.Msg = CM_WANTSPECIALKEY) then begin
    if (_Msg.wParam = VK_RETURN) or (_Msg.wParam = VK_ESCAPE) then begin
      if IsAutoSuggestDropdownVisible then begin
        _Msg.Result := 1;
        PostMessage(FCtrl.Handle, WM_WINDOW_PROC_HOOK_HELPER, 0, 0);
        Exit; //==>
      end;
    end;
  end else if (_Msg.Msg = WM_WINDOW_PROC_HOOK_HELPER) then begin
    WmWindowProcHookHelper(_Msg);
  end;
  inherited NewWindowProc(_Msg);
end;

function TAutoCompleteHelper.SetAutoComplete: Boolean;
var
  Dummy: IUnknown;
  Opt: DWORD;
  AC2: IAutoComplete2;
  Strings: IEnumString;
  ed: TCustomEdit;
  Res: HResult;
begin
  Result := False;
  try
    ed := GetEdit;
    Dummy := CreateComObject(CLSID_IAutoComplete);
    if (Dummy <> nil) and
      (Dummy.QueryInterface(IID_IAutoComplete, FAutoComplete) = S_OK) then begin
      Strings := CreateEnumStringInt;
      Res := FAutoComplete.Init(ed.Handle, Strings, nil, nil);
      if S_OK = Res then begin
        FAutoComplete.Enable(True);

        if S_OK = FAutoComplete.QueryInterface(IID_IAutoComplete2, AC2) then begin
          Opt := ACO_AUTOAPPEND or ACO_AUTOSUGGEST or ACO_UPDOWNKEYDROPSLIST;
          AC2.SetOptions(Opt);

          Result := True;
        end;
      end;
    end;
  except
    //CLSID_IAutoComplete is not available
  end;
end;

procedure TAutoCompleteHelper.WmNcCreate;
begin
  inherited;
  SetAutoComplete;
end;

procedure TAutoCompleteHelper.WmWindowProcHookHelper(var _Msg: TMessage);
var
  ed: TCustomEdit;
begin
  ed := GetEdit;
  ed.SelStart := ed.GetTextLen;
end;

procedure InitializeCustomMessages;
begin
  WM_WINDOW_PROC_HOOK_HELPER := RegisterWindowMessage('WM_WINDOW_PROC_HOOK_HELPER');
end;

initialization
  InitializeCustomMessages;
end.
