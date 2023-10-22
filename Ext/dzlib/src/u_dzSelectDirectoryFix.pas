///<summary>
/// Fixes the SelectDirectory function of Delphi 2007 (not sure whether it needs fixing
/// in later versions) </summary>
unit u_dzSelectDirectoryFix;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils,
  FileCtrl,
{$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  UITypes,
{$ENDIF}
  Controls;

{$IF not Declared(TSelectDirExtOpt)}
type
  TSelectDirExtOpt = (sdNewFolder, sdShowEdit, sdShowShares, sdNewUI, sdShowFiles, sdValidateDir);
  TSelectDirExtOpts = set of TSelectDirExtOpt;
{$IFEND}

///<summary>
/// Bugixed version of the FilCtrl SelectDirectory function with identical parameters
/// The following bugs have been fixed:
/// 1. Positioning the dialog works for all tested monitor combinations. This means
///    not only that the correct monitor is being selected (which is already fixed
///    Delphi 10.1 (and possibly earlier) but also that it is correctly centered
///    on that monitor.
/// 2. The given directory is not only selected but the tree view is also scrolled
///    to make the entry visible.
/// In addition to that, if passing a Parent parameter <> nil, the dialog will be
/// centered on that parent (or the form the parent belongs to), taking the monitor
/// work area into account. </summary>
function SelectDirectory(const Caption: string; const Root: WideString;
  var Directory: string; Options: TSelectDirExtOpts = [sdNewUI]; Parent: TWinControl = nil): Boolean;

///<summary>
/// Same as SelectDirectory above but with a different name so it can be called explicitly rather
/// than relying on the order of units in the uses clause. </summary>
function dzSelectDirectory(const Caption: string; const Root: WideString;
  var Directory: string; Options: TSelectDirExtOpts = [sdNewUI]; Parent: TWinControl = nil): Boolean;

implementation

uses
  Consts,
  ShlObj,
  ActiveX,
  Dialogs,
  Forms,
  Messages,
  Classes,
  u_dzTypes,
  u_dzVclUtils;

{$IF Declared(TFileOpenDialog)}
function dzSelectDirectoryVistaAndUp(const Caption: string; const Root: string; var Directory: string;
  Options: TSelectDirExtOpts; Parent: TWinControl): Boolean;
var
  DlgOptions: TFileDialogOptions;
  Dlg: TFileOpenDialog;
begin
  // _Options will be ignored since there is no equivalent in TFileDialogOptions
  DlgOptions := [fdoPickFolders, fdoForceFileSystem];

  Dlg := TFileOpenDialog.Create(Parent);
  try
    Dlg.Options := DlgOptions;
    if Caption <> '' then
      Dlg.Title := Caption;
    if Directory <> '' then
      Dlg.DefaultFolder := Directory;

    if Assigned(Parent) then
      Result := Dlg.Execute(Parent.Handle)
    else
      Result := Dlg.Execute;

    if Result then begin
      Directory := Dlg.FileName;
    end;
  finally
    Dlg.Free;
  end;
end;
{$IFEND}

type
  TSelectDirCallback = class(TObject)
  private
    FWndProcInstanceStub: Pointer;
    FWndProcPrevious: TFNWndProc;
    FWnd: HWND;
    FParent: TWinControl;
    FDirectory: string;
    FInitialized: Boolean;
    FPositioned: Boolean;
    procedure WndProcSubClassed(var _Msg: TMessage);
    procedure SetDialogPosition;
    procedure SubClass(_Wnd: HWND);
    procedure UnsubClass;
  protected
    function SelectDirCB(_Wnd: HWND; _uMsg: UINT; _lParam, _lpData: lParam): Integer;
  public
    constructor Create(const _Directory: string; _Parent: TWinControl);
  end;

{ TSelectDirCallback }

constructor TSelectDirCallback.Create(const _Directory: string; _Parent: TWinControl);
begin
  inherited Create;
  FParent := _Parent;
  FDirectory := _Directory;
end;

// subclass the given window by replacing its WindowProc

{$IF not Declared(NativeInt)}
// Delphi 6 does not declare it
type
  NativeInt = Integer;
{$IFEND}

procedure TSelectDirCallback.SubClass(_Wnd: HWND);
begin
  if FWndProcPrevious <> nil then
    Exit;
  FWnd := _Wnd;
  FWndProcPrevious := TFNWndProc(GetWindowLong(_Wnd, GWL_WNDPROC));
  FWndProcInstanceStub := MakeObjectInstance(WndProcSubClassed);
  SetWindowLong(_Wnd, GWL_WNDPROC, NativeInt(FWndProcInstanceStub));
end;

// un-subclass the window by restoring the previous WindowProc

procedure TSelectDirCallback.UnsubClass;
begin
  if FWndProcPrevious <> nil then begin
    SetWindowLong(FWnd, GWL_WNDPROC, NativeInt(FWndProcPrevious));
    FreeObjectInstance(FWndProcInstanceStub);
    FWndProcPrevious := nil;
    FWndProcInstanceStub := nil;
  end;
end;

// The WindowsProc method set by sublcassing the window.
// Waits for the first WM_SIZE message, sets the dialog position
// and un-subclasses the window.

procedure TSelectDirCallback.WndProcSubClassed(var _Msg: TMessage);
begin
  if (_Msg.Msg = WM_SIZE) then begin
    SetDialogPosition;
    _Msg.Result := CallWindowProc(FWndProcPrevious, FWnd, _Msg.Msg, _Msg.WParam, _Msg.lParam);
    UnsubClass;
  end;
  _Msg.Result := CallWindowProc(FWndProcPrevious, FWnd, _Msg.Msg, _Msg.WParam, _Msg.lParam);
end;

procedure TSelectDirCallback.SetDialogPosition;
var
  Rect: TRect;
  Monitor: TMonitor;
  ltwh: TRectLTWH;
  RefLtwh: TRectLTWH;
  frm: TCustomForm;
begin
  GetWindowRect(FWnd, Rect);
  if Assigned(FParent) then begin
    // this is new: Center on the parent form if a parent was given
    frm := GetParentForm(FParent);
    Monitor := Screen.MonitorFromWindow(frm.Handle);
    TRectLTWH_Assign(RefLtwh, frm.BoundsRect);
  end else begin
    if Assigned(Application.MainForm) then
      Monitor := Screen.MonitorFromWindow(Application.MainForm.Handle)
    else
      Monitor := Screen.MonitorFromWindow(0);
    TRectLTWH_Assign(RefLtwh, Monitor.BoundsRect);
  end;
  TRectLTWH_Assign(ltwh, Rect);
  ltwh.Left := RefLtwh.Left + RefLtwh.Width div 2 - ltwh.Width div 2;
  ltwh.Top := RefLtwh.Top + RefLtwh.Height div 2 - ltwh.Height div 2;
  TMonitor_MakeFullyVisible(Monitor, ltwh);
  SetWindowPos(FWnd, 0, ltwh.Left, ltwh.Top, 0, 0, SWP_NOZORDER or SWP_NOSIZE);
end;

function TSelectDirCallback.SelectDirCB(_Wnd: HWND; _uMsg: UINT; _lParam, _lpData: lParam): Integer;
{$IF not Declared(SInvalidPath)}
resourcestring
  SInvalidPath = '"%s" is an invalid path';
{$IFEND}

  procedure SelectDirectory;
  begin
    if FDirectory <> '' then begin
      // we use PostMessage to asynchronously select the directory
      PostMessage(_Wnd, BFFM_SETSELECTION, Windows.WParam(True), Windows.lParam(PChar(FDirectory)));
    end;
  end;

begin
  Result := 0;
  if _uMsg = BFFM_INITIALIZED then begin
    // Subclass the window to catch the WM_SIZE message when it is automatically being resized
    // later in the initialization process. Only then it is possible to get the final size
    // and position it correctly.
    SubClass(_Wnd);
    FInitialized := True;
    // Selecting the directory here only selects the entry but does not necessarily make
    // it visible. So we set it here and again further below.
    SelectDirectory;
  end else if (_uMsg = BFFM_VALIDATEFAILEDW) or (_uMsg = BFFM_VALIDATEFAILEDA) then begin
    // default code copied from FileCtrl
    MessageDlg(Format(SInvalidPath, [PChar(_lParam)]), mtError, [mbOK], 0); //FI:W541 Casting from Integer to Pointer type (or vice versa)
    Result := 1;
  end else if _uMsg = BFFM_SELCHANGED then begin
    if FInitialized and not FPositioned then begin
      FPositioned := True;
      // The first call to SelectDirectory only selects it but does not scroll the tree view
      // to make it visible. That's what this second call is for.
      SelectDirectory;
    end;
  end;
end;

// This is the actual callback function passed to the Windows API. lpData is the TSelectDirCallback
// object we created. Here we simply call its SelectDirCB method.

function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: lParam): Integer stdcall;
begin
  Result := TSelectDirCallback(lpData).SelectDirCB(Wnd, uMsg, lParam, lpData);
end;

function SelectDirectory(const Caption: string; const Root: WideString;
  var Directory: string; Options: TSelectDirExtOpts = [sdNewUI]; Parent: TWinControl = nil): Boolean;
begin
  Result := dzSelectDirectory(Caption, Root, Directory, Options, Parent);
end;

// This is copied from FileCtrl, mostly unchanged. I removed the WITH statement though.

function dzSelectDirectoryXP(const Caption: string; const Root: WideString;
  var Directory: string; Options: TSelectDirExtOpts = [sdNewUI]; Parent: TWinControl = nil): Boolean;
{$IF not Declared(BIF_NEWDIALOGSTYLE)}
const
  BIF_NEWDIALOGSTYLE = $0040;
{$IFEND}
{$IF not Declared(BIF_NONEWFOLDERBUTTON)}
const
  BIF_NONEWFOLDERBUTTON = $200;
{$IFEND}
{$IF not Declared(BIF_SHAREABLE)}
const
  BIF_SHAREABLE = $8000;
{$IFEND}

var
  BrowseInfo: TBrowseInfo;
  OldErrorMode: Cardinal;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
  CoInitResult: HRESULT;
  SelectDirCallback: TSelectDirCallback;
  WindowList: Pointer;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
begin
  Result := False;
  if not SysUtils.DirectoryExists(Directory) then
    Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then begin
    Buffer := ShellMalloc.Alloc(MAX_PATH * SizeOf(Char));
    try
      RootItemIDList := nil;
      if Root <> '' then begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(Application.Handle, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;

      // fill BrowseInfo
      if (Parent = nil) or not Parent.HandleAllocated then
        BrowseInfo.hwndOwner := Application.Handle
      else
        BrowseInfo.hwndOwner := Parent.Handle;
      BrowseInfo.pidlRoot := RootItemIDList;
      BrowseInfo.pszDisplayName := Buffer;
      BrowseInfo.lpszTitle := PChar(Caption);
      BrowseInfo.lpfn := SelectDirCB;
      BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS;
      if sdNewUI in Options then
        BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_NEWDIALOGSTYLE;
      if not (sdNewFolder in Options) then
        BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_NONEWFOLDERBUTTON;
      if sdShowEdit in Options then
        BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_EDITBOX;
      if not (sdNewUI in Options) and (sdShowShares in Options) then
        BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_SHAREABLE;
      if sdShowFiles in Options then
        BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_BROWSEINCLUDEFILES;
      if sdValidateDir in Options then
        BrowseInfo.ulFlags := BrowseInfo.ulFlags or BIF_VALIDATE;

      SelectDirCallback := TSelectDirCallback.Create(Directory, Parent);
      try
        BrowseInfo.lParam := lParam(SelectDirCallback);
        // Not sure if this is necessary. Delphi 2007 does it, Delphi 10.1 doesn't
        if sdNewUI in Options then begin
          CoInitResult := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
          if CoInitResult = RPC_E_CHANGED_MODE then
            BrowseInfo.ulFlags := BrowseInfo.ulFlags and not BIF_NEWDIALOGSTYLE;
        end;
        try
          WindowList := DisableTaskWindows(0);
          OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
          try
            ItemIDList := ShBrowseForFolder(BrowseInfo);
          finally
            SetErrorMode(OldErrorMode);
            EnableTaskWindows(WindowList);
          end;
        finally
          if sdNewUI in Options then
            CoUninitialize;
        end;
      finally
        SelectDirCallback.Free;
      end;
      Result := ItemIDList <> nil;
      if Result then begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

function dzSelectDirectory(const Caption: string; const Root: WideString;
  var Directory: string; Options: TSelectDirExtOpts = [sdNewUI]; Parent: TWinControl = nil): Boolean;
begin
{$IF Declared(TFileOpenDialog)}
  if Win32MajorVersion < 6 then begin
{$IFEND}
    Result := dzSelectDirectoryXP(Caption, Root, Directory, Options, Parent);
{$IF Declared(TFileOpenDialog)}
  end else begin
    Result := dzSelectDirectoryVistaAndUp(Caption, Root, Directory, Options, Parent);
  end;
{$IFEND}
end;

end.
