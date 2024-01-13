{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions                                              }
{       (Preview Panel, Thumbnail Icon, MD Text Editor)                        }
{                                                                              }
{       Copyright (c) 2021-2024 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors: Ariel Montes                                             }
{                                                                              }
{       https://github.com/EtheaDev/MarkdownShellExtensions                    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit MDTextEditor.ViewerMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, ImgList, Contnrs,
  SynEdit, ActnList, Menus, ToolWin,
  StdActns, SynEditHighlighter,
  MDShellEx.Resources, SynEditPrint, SynEditOptionsDialog, ActnCtrls, ActnMan,
  ActnMenus, SynCompletionProposal, SynEditTypes, SynEditMiscClasses,
  SynEditSearch, XPStyleActnCtrls, System.Actions, SVGIconImage, Vcl.Buttons,
  Vcl.CategoryButtons, Vcl.WinXCtrls, System.ImageList, Vcl.VirtualImageList,
  MDShellEx.Settings
  , System.Generics.Collections
  , Vcl.PlatformVclStylesActnCtrls
  {$IFNDEF NO_VCL_STYLES}
  , Vcl.Styles.Fixes
  , Vcl.Styles.FormStyleHooks
  , Vcl.Styles.NC
  , Vcl.Styles.OwnerDrawFix
  , Vcl.Styles.Utils.ScreenTips
  , Vcl.Styles.Utils.SysStyleHook
  , Vcl.Styles.Utils
  , Vcl.Styles.Utils.SysControls
  , Vcl.Styles.UxTheme
  , Vcl.Styles.Hooks
  , Vcl.Styles.Utils.Forms
  , Vcl.Styles.Utils.ComCtrls
  , Vcl.Styles.Utils.StdCtrls
  , Vcl.Styles.Ext
  {$ENDIF}
  , HTMLUn2
  , HtmlGlobals
  , HtmlView
  , uDragDropUtils
  , Vcl.StyledButton
  , Vcl.StyledToolbar
  , dlgInputUrl
  ;

const
  SET_FILE_NAME = 'HiglightSettings';

resourcestring
  PAGE_HEADER_FIRST_LINE_LEFT = '$TITLE$';
  PAGE_HEADER_FIRST_LINE_RIGHT = 'Page count: $PAGECOUNT$';
  PAGE_FOOTER_FIRST_LINE_LEFT = 'Print Date: $DATE$. Time: $TIME$';
  PAGE_FOOTER_FIRST_LINE_RIGHT = 'Page $PAGENUM$ of $PAGECOUNT$';
  FILE_NOT_FOUND = 'File "%s" not found!';
  SMODIFIED = 'Changed';
  SUNMODIFIED = 'Not changed';
  STATE_READONLY = 'Read-only';
  STATE_INSERT = 'Insert';
  STATE_OVERWRITE = 'Overwrite';
  CLOSING_PROBLEMS = 'Problem closing!';
  STR_ERROR = 'ERROR!';
  STR_UNEXPECTED_ERROR = 'UNEXPECTED ERROR!';
  CONFIRM_CHANGES = 'ATTENTION: the content of file "%s" is changed: do you want to save the file?';

type
  TEditingFile = class
  private
    FIcon : TIcon;
    FFileName : string;
    FExtension: string;
    FShowXMLText: Boolean;
    FMarkDownFile: TMarkDownFile;
    FHTMLViewer: THTMLViewer;
    FUseDarkStyle: Boolean;
    procedure ReadFromFile;
    procedure SaveToFile;
    function GetFileName: string;
    function GetName: string;
    procedure SetFileName(const Value: string);
    function GetImageName: string;
    procedure SetHTMLViewer(const Value: THTMLViewer);
    procedure UpdateRootPath;
    function GetServerRoot: string;
  public
    SynEditor: TSynEdit;
    TabSheet: TTabSheet;
    Splitter: TSplitter;
    Constructor Create(const EditFileName : string;
      const AUseDarkStyle: Boolean);
    Destructor Destroy; override;
    procedure ShowMarkDownAsHTML(const ASettings: TEditorSettings;
      const AReloadImages: Boolean);
    property HTMLViewer: THTMLViewer read FHTMLViewer write SetHTMLViewer;
    property FileName: string read GetFileName write SetFileName; //with full path
    property Name: string read GetName; //only name of file
    property ImageName: string read GetImageName;
    property Extension: string read FExtension;
    property ServerRoot: string read GetServerRoot;
  end;

  TfrmMain = class(TForm, IDragDrop)
    OpenDialog: TOpenDialog;
    ActionList: TActionList;
    acOpenFile: TAction;
    acSave: TAction;
    SaveDialog: TSaveDialog;
    acEditCut: TEditCut;
    acEditCopy: TEditCopy;
    acEditPaste: TEditPaste;
    acEditSelectAll: TEditSelectAll;
    acEditUndo: TEditUndo;
    popEditor: TPopupMenu;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    Sep2MenuItem: TMenuItem;
    acSearch: TAction;
    acReplace: TAction;
    SearchMenuItem: TMenuItem;
    ReplaceMenuItem: TMenuItem;
    acQuit: TAction;
    acNewFile: TAction;
    acAbout: TAction;
    acClose: TAction;
    acCloseAll: TAction;
    acSaveAll: TAction;
    acSearchAgain: TAction;
    actnPrint: TAction;
    PrinterSetupDialog: TPrinterSetupDialog;
    PrintDialog: TPrintDialog;
    SynEditPrint: TSynEditPrint;
    actnPrinterSetup: TAction;
    actnPrintPreview: TAction;
    actnPageSetup: TAction;
    actnEditOptions: TAction;
    actnEnlargeFont: TAction;
    actnReduceFont: TAction;
    actnSaveAs: TAction;
    StatusBar: TStatusBar;
    actnSettings: TAction;
    SynEditSearch: TSynEditSearch;
    SV: TSplitView;
    catMenuItems: TCategoryButtons;
    panlTop: TPanel;
    lblTitle: TLabel;
    SettingsToolBar: TToolBar;
    ColorSettingsToolButton: TToolButton;
    EditOptionsToolButton: TToolButton;
    VirtualImageList: TVirtualImageList;
    actMenu: TAction;
    MenuButtonToolbar: TToolBar;
    MenuToolButton: TToolButton;
    PageSetupToolButton: TToolButton;
    PrinterSetupToolButton: TToolButton;
    AboutToolButton: TToolButton;
    QuitToolButton: TToolButton;
    ToolButton9: TToolButton;
    OpenRecentAction: TAction;
    RecentPopupMenu: TPopupMenu;
    SaveMenuItem: TMenuItem;
    CloseMenuItem: TMenuItem;
    Sep1MenuItem: TMenuItem;
    SelectAllMenuItem: TMenuItem;
    ReformatTextMenuItem: TMenuItem;
    N1: TMenuItem;
    CloseAllMenuItem: TMenuItem;
    ClientPanel: TPanel;
    PageControl: TPageControl;
    PopHTMLViewer: TPopupMenu;
    acZoomIn: TAction;
    acZoomOut: TAction;
    ZoomInMenuItem: TMenuItem;
    ZoomOutMenuItem: TMenuItem;
    acSaveHTMLFile: TAction;
    SaveHTMLfileMenuItem: TMenuItem;
    acSavePDFFile: TAction;
    SavePDFfileMenuItem: TMenuItem;
    Close_MenuItem: TMenuItem;
    Close_AllMenuItem: TMenuItem;
    PopHTMLSep: TMenuItem;
    ProcessorDialectLabel: TLabel;
    ProcessorDialectComboBox: TComboBox;
    VirtualImageList20: TVirtualImageList;
    PanelCloseButton: TPanel;
    SVGIconImageCloseButton: TSVGIconImage;
    LoadTimer: TTimer;
    VirtualImageListToolbar: TVirtualImageList;
    ToolbarActionList: TActionList;
    acHeader1: TAction;
    acHeader2: TAction;
    acHeader3: TAction;
    acLink: TAction;
    acImage: TAction;
    acTable: TAction;
    acBold: TAction;
    acItalic: TAction;
    acStrike: TAction;
    acUnderline: TAction;
    acCode: TAction;
    acMarker: TAction;
    acSuperscript: TAction;
    acSubscript: TAction;
    acUnorderedList: TAction;
    acOrderedList: TAction;
    acBlockquote: TAction;
    acHorizontalRule: TAction;
    acHelp: TAction;
    StyledToolbar: TStyledToolbar;
    btHeader1: TStyledToolButton;
    btHeader3: TStyledToolButton;
    btHeader2: TStyledToolButton;
    btSeparator1: TStyledToolButton;
    btLink: TStyledToolButton;
    btImage: TStyledToolButton;
    btTable: TStyledToolButton;
    btSeparator2: TStyledToolButton;
    btBold: TStyledToolButton;
    btItalic: TStyledToolButton;
    btStrike: TStyledToolButton;
    btUnderline: TStyledToolButton;
    btCode: TStyledToolButton;
    btMarker: TStyledToolButton;
    btSuperscript: TStyledToolButton;
    btSubscript: TStyledToolButton;
    btSeparator3: TStyledToolButton;
    btUnorderedList: TStyledToolButton;
    btOrderedList: TStyledToolButton;
    btBlockquote: TStyledToolButton;
    btHorizontalRule: TStyledToolButton;
    btSeparator4: TStyledToolButton;
    btHelp: TStyledToolButton;
    acRefresh: TAction;
    RefreshMenuItem: TMenuItem;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure acOpenFileExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShowSRDialog(aReplace: boolean);
    procedure DoSearchReplaceText(AReplace: boolean;
      ABackwards: boolean);
    procedure acSearchExecute(Sender: TObject);
    procedure acReplaceExecute(Sender: TObject);
    procedure acQuitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acNewFileExecute(Sender: TObject);
    procedure acSearchUpdate(Sender: TObject);
    procedure acReplaceUpdate(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure acSaveUpdate(Sender: TObject);
    procedure acCloseAllUpdate(Sender: TObject);
    procedure acCloseAllExecute(Sender: TObject);
    procedure acSaveAllUpdate(Sender: TObject);
    procedure acSaveAllExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure acSearchAgainExecute(Sender: TObject);
    procedure acSearchAgainUpdate(Sender: TObject);
    procedure actnPrinterSetupExecute(Sender: TObject);
    procedure actnPrintPreviewExecute(Sender: TObject);
    procedure actnPrintExecute(Sender: TObject);
    procedure actnPageSetupExecute(Sender: TObject);
    procedure actnEditOptionsExecute(Sender: TObject);
    procedure actnEditingUpdate(Sender: TObject);
    procedure actnFontExecute(Sender: TObject);
    procedure actnSaveAsExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actnSettingsExecute(Sender: TObject);
    procedure actnSettingsUpdate(Sender: TObject);
    procedure actionForFileUpdate(Sender: TObject);
    procedure HistoryListClick(Sender: TObject);
    procedure actMenuExecute(Sender: TObject);
    procedure acEditUndoExecute(Sender: TObject);
    procedure SVOpened(Sender: TObject);
    procedure SVClosed(Sender: TObject);
    procedure SVClosing(Sender: TObject);
    procedure SVOpening(Sender: TObject);
    procedure ActionListExecute(Action: TBasicAction; var Handled: Boolean);
    procedure catMenuItemsMouseLeave(Sender: TObject);
    procedure catMenuItemsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure catMenuItemsGetHint(Sender: TObject; const Button: TButtonItem;
      const Category: TButtonCategory; var HintStr: string;
      var Handled: Boolean);
    procedure RecentPopupMenuPopup(Sender: TObject);
    procedure OpenRecentActionExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure FormBeforeMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure acEditCutExecute(Sender: TObject);
    procedure acEditCopyExecute(Sender: TObject);
    procedure acEditPasteExecute(Sender: TObject);
    procedure acEditSelectAllExecute(Sender: TObject);
    procedure acEditUndoUpdate(Sender: TObject);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acZoomExecute(Sender: TObject);
    procedure acEditCopyUpdate(Sender: TObject);
    procedure acSaveHTMLFileExecute(Sender: TObject);
    procedure acSavePDFFileExecute(Sender: TObject);
    procedure ProcessorDialectComboBoxSelect(Sender: TObject);
    procedure PageControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PageControlMouseEnter(Sender: TObject);
    procedure PageControlMouseLeave(Sender: TObject);
    procedure SVGIconImageCloseButtonClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure LoadTimerTimer(Sender: TObject);
    procedure acToolbarUpdate(Sender: TObject);
    procedure acHeader1Execute(Sender: TObject);
    procedure acHeader2Execute(Sender: TObject);
    procedure acHeader3Execute(Sender: TObject);
    procedure acLinkExecute(Sender: TObject);
    procedure acImageExecute(Sender: TObject);
    procedure acTableExecute(Sender: TObject);
    procedure acBoldExecute(Sender: TObject);
    procedure acItalicExecute(Sender: TObject);
    procedure acStrikeExecute(Sender: TObject);
    procedure acUnderlineExecute(Sender: TObject);
    procedure acCodeExecute(Sender: TObject);
    procedure acMarkerExecute(Sender: TObject);
    procedure acSuperscriptExecute(Sender: TObject);
    procedure acSubscriptExecute(Sender: TObject);
    procedure acUnorderedListExecute(Sender: TObject);
    procedure acOrderedListExecute(Sender: TObject);
    procedure acBlockquoteExecute(Sender: TObject);
    procedure acHorizontalRuleExecute(Sender: TObject);
    procedure acHelpExecute(Sender: TObject);
    procedure acHelpUpdate(Sender: TObject);
    procedure acHTMLViewerUpdate(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
  private
    FEditingInProgress: Boolean;
    FirstAction: Boolean;
    MinFormWidth, MinFormHeight, MaxFormWidth, MaxFormHeight: Integer;
    FProcessingFiles: Boolean;
    FEditorSettings: TEditorSettings;
    currentDir: string;
    EditFileList: TObjectList;
    fSearchFromCaret: boolean;
    gbSearchBackwards: boolean;
    gbSearchCaseSensitive: boolean;
    gbSearchFromCaret: boolean;
    gbSearchSelectionOnly: boolean;
    gbSearchTextAtCaret: boolean;
    gbSearchWholeWords: boolean;
    gsSearchText: string;
    gsSearchTextHistory: string;
    gsReplaceText: string;
    gsReplaceTextHistory: string;
    FEditorOptions: TSynEditorOptionsContainer;
    FMDFontSize: Integer;
    FHTMLFontSize: Integer;
    FMDFile: TEditingFile;
    FDropTarget: TDropTarget;
    // implement IDragDrop
    function DropAllowed(const FileNames: array of string): Boolean;
    procedure Drop(const FileNames: array of string);
    function GetMarkdownHelpFileName: TFileName;
    procedure CloseSplitViewMenu;
    procedure UpdateHighlighters;
    procedure UpdateFromSettings(AEditor: TSynEdit);
    function DialogPosRect: TRect;
    procedure AdjustCompactWidth;
    function OpenFile(const FileName: string;
      const ARaiseError: Boolean = True): Boolean;
    function AddEditingFile(const EditingFile: TEditingFile): Integer;
    procedure RemoveEditingFile(EditingFile: TEditingFile);
    function CurrentEditFile: TEditingFile;
    function CurrentEditor: TSynEdit;
    function ModifiedCount: integer;
    procedure InitSynEditPrint;
    procedure InitEditorOptions;
    procedure SetSynEditPrintProperties(SynEditPrint : TSynEditPrint);
    procedure UpdateEditorsOptions;
    function CurrentEditorState : string;
    procedure UpdateStatusBarPanels;
    procedure AddOpenedFile(const AFileName: string);
    procedure UpdateMDViewer(const AReloadImages: Boolean);
    procedure SynEditChange(Sender: TObject);
    procedure SynEditEnter(Sender: TObject);
    procedure UpdateHighlighter(ASynEditor: TSynEdit);
    procedure SetMDFontSize(const Value: Integer);
    procedure LoadOpenedFiles;
    procedure SetHTMLFontSize(const Value: Integer);
    procedure HTMLToPDF(const APDFFileName: TFileName);
    procedure FileSavedAskToOpen(const AFileName: string);
    function CanAcceptFileName(const AFileName: string): Boolean;
    function AcceptedExtensions: string;
    procedure SplitterMoved(Sender: TObject);
    procedure WriteSettingsToIni;
    procedure ConfirmChanges(EditingFile: TEditingFile);
    procedure HtmlViewerHotSpotClick(Sender: TObject; const ASource: ThtString;
      var Handled: Boolean);
    procedure ShowTabCloseButtonOnHotTab;
    procedure InsertMDCommand(const ACommand: string);
    procedure InsertMDCommandContains(const ACommandOpen, ACommandClose: string);
    procedure InputUrl(const APrefix: string; AType: TInputType);
    property MDFontSize: Integer read FMDFontSize write SetMDFontSize;
    property HTMLFontSize: Integer read FHTMLFontSize write SetHTMLFontSize;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWindowHandle; override;
  public
    procedure ManageExceptions(Sender: TObject; E: Exception);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Vcl.Themes
  , System.IOUtils
  , System.StrUtils
  , System.UITypes
  , Winapi.ShellAPI
  , MDShellEx.Misc
  , Xml.XMLDoc
  , dlgReplaceText
  , dlgSearchText
  , dlgConfirmReplace
  , MDShellEx.About
  , DPageSetup
  , FSynHighlightProp
  , Math
  , Winapi.SHFolder
  , MDShellEx.SettingsForm
  , BegaHtmlPrintPreviewForm
  , BegaPreview
  , SynPDF
  , vmHtmlToPdf
  , MarkdownProcessor
  , MarkdownUtils
  , uLogExcept
  , Vcl.StyledTaskDialog
  , RegularExpressions
  , MDShellEx.dlgCreateTable
  ;

{$R *.dfm}

const
  STATUSBAR_PANEL_FONTNAME = 0;
  STATUSBAR_PANEL_FONTSIZE = 1;
  STATUSBAR_PANEL_CARET = 2;
  STATUSBAR_PANEL_MODIFIED = 3;
  STATUSBAR_PANEL_STATE = 4;
  STATUSBAR_MESSAGE = 5;

function DoubleQuote(const AValue: string): string;
begin
  Result := '"'+AValue+'"';
end;

procedure UpdateApplicationStyle(const VCLStyleName: string);
begin
  if StyleServices.Enabled then
    TStyleManager.SetStyle(VCLStyleName);
end;

{ TEditingFile }

procedure TEditingFile.ShowMarkDownAsHTML(const ASettings: TEditorSettings;
  const AReloadImages: Boolean);
var
  LStream: TStringStream;
  LOldPos: Integer;
begin
  if AReloadImages then
    HtmlViewer.clear;
  FMarkDownFile := TMarkDownFile.Create(SynEditor.Lines.Text,
    ASettings.ProcessorDialect, True);
  //Load HTML content into HTML-Viewer
  LOldPos := HtmlViewer.VScrollBarPosition;
  HtmlViewer.DefFontSize := ASettings.HTMLFontSize;
  HtmlViewer.DefFontName := ASettings.HTMLFontName;
  LStream := TStringStream.Create(FMarkDownFile.HTML);
  try
    HtmlViewer.LoadFromStream(LStream);
    HtmlViewer.VScrollBarPosition := LOldPos;
    dmResources.StopLoadingImages(False);
    HtmlViewer.Visible := True;
  finally
    LStream.Free;
  end;
end;

procedure TEditingFile.ReadFromFile;
begin
  try
    //Try loading UTF-8 file
    SynEditor.Lines.LoadFromFile(FileName, TEncoding.UTF8);
  except
    on E: EEncodingError do
    begin
      //Try to load ANSI file
      SynEditor.Lines.LoadFromFile(FileName, TEncoding.ANSI);
    end
    else
      raise;
  end;
end;

constructor TEditingFile.Create(const EditFileName: string;
  const AUseDarkStyle: Boolean);
var
  Filter : Word;
begin
  inherited Create;
  FUseDarkStyle := AUseDarkStyle;
  FShowXMLText := False;

  {$IFNDEF NO_VCL_STYLED}
  if not IsStyleHookRegistered(TCustomSynEdit, TScrollingStyleHook) then
    TStyleManager.Engine.RegisterStyleHook(TCustomSynEdit, TScrollingStyleHook);
  {$ENDIF}

  FileName := EditFileName;
  Fextension := ExtractFileExt(FileName);

  FIcon := TIcon.Create;
  if FileExists(FileName) then
    FIcon.Handle := ExtractAssociatedIcon(hInstance, PChar(DoubleQuote(FileName)),Filter);
end;

function TEditingFile.GetFileName: string;
begin
  Result := FFileName;
end;

function TEditingFile.GetImageName: string;
begin
  if FUseDarkStyle then
    Result := 'markdown-white'
  else
    Result := 'markdown-black';
end;

function TEditingFile.GetName: string;
begin
  Result := ExtractFileName(FFileName);
end;

function TEditingFile.GetServerRoot: string;
begin
  if HTMLViewer <> nil then
    Result := HTMLViewer.ServerRoot
  else
    Result := '';
end;

procedure TEditingFile.SetFileName(const Value: string);
var
  Ext : string;
begin
  FFileName := Value;
  Ext := ExtractFileExt(FFileName);
  if TabSheet <> nil then
    TabSheet.Caption := Name;
  UpdateRootPath;
end;

procedure TEditingFile.UpdateRootPath;
begin
  if Assigned(HTMLViewer) then
    HTMLViewer.ServerRoot := ExtractFilePath(FFileName);
end;

procedure TEditingFile.SetHTMLViewer(const Value: THTMLViewer);
begin
  FHTMLViewer := Value;
  UpdateRootPath;
end;

destructor TEditingFile.Destroy;
begin
  FreeAndNil(FIcon);
  FreeAndNil(HTMLViewer);
  FreeAndNil(SynEditor);
  inherited;
end;

procedure TEditingFile.SaveToFile;
begin
  SynEditor.Lines.SaveToFile(Self.FileName);
  SynEditor.Modified := False;
  SynEditor.OnChange(SynEditor);
end;

{ TfrmMain }

procedure TfrmMain.acOpenFileExecute(Sender: TObject);
var
  i : integer;
begin
  if OpenDialog.Execute then
  begin
    for i := 0 to OpenDialog.Files.Count -1 do
      OpenFile(OpenDialog.Files[i], False);
  end;
  UpdateMDViewer(True);
end;

function TfrmMain.CanAcceptFileName(const AFileName: string): Boolean;
begin
  Result := IsFileNameWithExt(AFileName, AMarkDownFileExt);
end;

function TfrmMain.AcceptedExtensions: string;
begin
  //Check file extensions
  Result := GetFileMasks(AMarkDownFileExt);
end;

function TfrmMain.OpenFile(const FileName : string;
  const ARaiseError: Boolean = True): Boolean;
var
  EditingFile: TEditingFile;
  I, J: Integer;
  LErrorMsg: string;
begin
  Screen.Cursor := crHourGlass;
  Try
    FProcessingFiles := True;
    if FileExists(FileName) then
    begin
      if not CanAcceptFileName(FileName) then
        raise Exception.CreateFmt('Cannot open file with extensions different from "%s"',
          [AcceptedExtensions]);

      //looking for the file already opened
      EditingFile := nil;
      I := -1;
      for J := 0 to EditFileList.Count -1 do
      begin
        if SameText(FileName, TEditingFile(EditFileList.Items[J]).FileName) then
        begin
          EditingFile := TEditingFile(EditFileList.Items[J]);
          I := J;
          PageControl.ActivePageIndex := I;
          break;
        end;
      end;
      //searching EditingFile object
      Try
        if not Assigned(EditingFile) then
        begin
          EditingFile := TEditingFile.Create(FileName, FEditorSettings.UseDarkStyle);
          //Add file to list
          I := AddEditingFile(EditingFile);
        end;

        //Opens the file
        EditingFile.ReadFromFile;

        Result := True;
      Except
        if I >= 0 then
          RemoveEditingFile(EditingFile)
        else
          EditingFile.Free;
        raise;
      End;
      AddOpenedFile(FileName);
    end
    else
    begin
      Result := False;
      if ARaiseError then
      begin
        LErrorMsg := Format(FILE_NOT_FOUND,[FileName]);
        StatusBar.Panels[STATUSBAR_MESSAGE].Text := LErrorMsg;
        Raise EFilerError.Create(LErrorMsg);
      end;
    end;
  Finally
    FProcessingFiles := False;
    Screen.Cursor := crDefault;
  End;
end;

procedure TfrmMain.OpenRecentActionExecute(Sender: TObject);
var
  LRect: TRect;
  LPoint: TPoint;
begin
  LRect := catMenuItems.Categories[0].Items[2].Bounds;
  LPoint.X := LRect.Right;
  LPoint.Y := LRect.Bottom+LRect.Height;
  LPoint   := ClientToScreen(LPoint);
  RecentPopupMenu.Popup(LPoint.X, LPoint.Y);
end;

procedure TfrmMain.acSaveExecute(Sender: TObject);
begin
  CurrentEditFile.SaveToFile;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(EditFileList);
  FreeAndNil(SynEditPrint);
  FreeAndNil(FEditorSettings);
  FreeAndNil(FEditorOptions);
  inherited;
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(27) then
    dmResources.StopLoadingImages(True);
end;

procedure TfrmMain.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (Shift = [ssCtrl]) and (CurrentEditFile <> nil) then
  begin
    if CurrentEditFile.SynEditor.Focused then
    begin
      actnReduceFont.Execute;
      Handled := True;
    end
    else if CurrentEditFile.HTMLViewer.Focused then
    begin
      acZoomOut.Execute;
      Handled := True;
    end;
  end;
end;

procedure TfrmMain.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (Shift = [ssCtrl]) and (CurrentEditFile <> nil) then
  begin
    if CurrentEditFile.SynEditor.Focused then
    begin
      actnEnlargeFont.Execute;
      Handled := True;
    end
    else if CurrentEditFile.HTMLViewer.Focused then
    begin
      acZoomIn.Execute;
      Handled := True;
    end;
  end;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  AdjustCompactWidth;
end;

function TfrmMain.GetMarkdownHelpFileName: TFileName;
begin
  //After deploy/install the Help folder is located into exe folder
  Result := ExtractFilePath(Application.ExeName)+
    '.\Help\MarkDown Support Test.md';
  //In development environment the Help folder is located into ..\Help\ folter
  if not FileExists(Result) then
    Result := ExtractFilePath(Application.ExeName)+
      '..\Help\MarkDown Support Test.md';
end;

procedure TfrmMain.ShowSRDialog(aReplace: boolean);
var
  dlg: TTextSearchDialog;
begin
  if AReplace then
    dlg := TTextReplaceDialog.Create(Self)
  else
    dlg := TTextSearchDialog.Create(Self);
  with dlg do
  try
    // assign search options
    SearchBackwards := gbSearchBackwards;
    SearchCaseSensitive := gbSearchCaseSensitive;
    SearchFromCursor := gbSearchFromCaret;
    SearchInSelectionOnly := gbSearchSelectionOnly;
    // start with last search text
    SearchText := gsSearchText;
    if gbSearchTextAtCaret then
    begin
      // if something is selected search for that text
      if CurrentEditor.SelAvail and (CurrentEditor.BlockBegin.Line = CurrentEditor.BlockEnd.Line) then
        SearchText := CurrentEditor.SelText
      else
        SearchText := CurrentEditor.GetWordAtRowCol(CurrentEditor.CaretXY);
    end;
    SearchTextHistory := gsSearchTextHistory;
    if AReplace then
      with dlg as TTextReplaceDialog do
      begin
        ReplaceText := gsReplaceText;
        ReplaceTextHistory := gsReplaceTextHistory;
      end;
    SearchWholeWords := gbSearchWholeWords;
    if ShowModal = mrOK then
    begin
      gbSearchBackwards := SearchBackwards;
      gbSearchCaseSensitive := SearchCaseSensitive;
      gbSearchFromCaret := SearchFromCursor;
      gbSearchSelectionOnly := SearchInSelectionOnly;
      gbSearchWholeWords := SearchWholeWords;
      gsSearchText := SearchText;
      gsSearchTextHistory := SearchTextHistory;
      if AReplace then
        with dlg as TTextReplaceDialog do
        begin
          gsReplaceText := ReplaceText;
          gsReplaceTextHistory := ReplaceTextHistory;
        end;
      fSearchFromCaret := gbSearchFromCaret;
      if gsSearchText <> '' then
      begin
        DoSearchReplaceText(AReplace, gbSearchBackwards);
        fSearchFromCaret := TRUE;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfrmMain.SplitterMoved(Sender: TObject);
var
  LEditingFile: TEditingFile;
begin
  LEditingFile := TEditingFile(TSplitter(Sender).Tag);
  LEditingFile.ShowMarkDownAsHTML(FEditorSettings, True);
end;

procedure TfrmMain.SVClosed(Sender: TObject);
begin
  // When TSplitView is closed, adjust ButtonOptions and Width
  catMenuItems.ButtonOptions := catMenuItems.ButtonOptions - [boShowCaptions];
  actMenu.Hint := 'Expand';
end;

procedure TfrmMain.SVClosing(Sender: TObject);
begin
  if SV.Opened then
    SV.OpenedWidth := SV.Width;
end;

procedure TfrmMain.SVGIconImageCloseButtonClick(Sender: TObject);
begin
  PanelCloseButton.Visible := False;
  PageControl.ActivePageIndex := PanelCloseButton.Tag;
  acClose.Execute;
  ShowTabCloseButtonOnHotTab;
end;

procedure TfrmMain.SVOpened(Sender: TObject);
begin
  // When not animating, change size of catMenuItems when TSplitView is opened
  catMenuItems.ButtonOptions := catMenuItems.ButtonOptions + [boShowCaptions];
  actMenu.Hint := 'Collapse';
end;

procedure TfrmMain.SVOpening(Sender: TObject);
begin
  // When animating, change size of catMenuItems at the beginning of open
  catMenuItems.ButtonOptions := catMenuItems.ButtonOptions + [boShowCaptions];
end;

procedure TfrmMain.DestroyWindowHandle;
begin
  FreeAndNil(FDropTarget);
  inherited;
end;

function TfrmMain.DialogPosRect: TRect;
begin
  GetWindowRect(Self.Handle, Result);
end;

procedure TfrmMain.DoSearchReplaceText(AReplace: boolean;
  ABackwards: boolean);
var
  Options: TSynSearchOptions;
begin
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not fSearchFromCaret then
    Include(Options, ssoEntireScope);
  if gbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);
  if CurrentEditor.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    if ssoBackwards in Options then
      CurrentEditor.BlockEnd := CurrentEditor.BlockBegin
    else
      CurrentEditor.BlockBegin := CurrentEditor.BlockEnd;
    CurrentEditor.CaretXY := CurrentEditor.BlockBegin;
  end;

  if ConfirmReplaceDialog <> nil then
    ConfirmReplaceDialog.Free;
end;

procedure TfrmMain.Drop(const FileNames: array of string);
var
  i: Integer;
begin
  for i := 0 to Length(FileNames)-1 do
  begin
    if CanAcceptFileName(FileNames[i]) then
      OpenFile(FileNames[i], False);
  end;
  UpdateMDViewer(True);
end;

function TfrmMain.DropAllowed(const FileNames: array of string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(FileNames)-1 do
  begin
    Result := CanAcceptFileName(FileNames[i]);
    if Result then
      Break;
  end;
end;

procedure TfrmMain.acSearchExecute(Sender: TObject);
begin
  ShowSRDialog(false);
end;

procedure TfrmMain.acRefreshExecute(Sender: TObject);
begin
  UpdateMDViewer(True);
end;

procedure TfrmMain.acReplaceExecute(Sender: TObject);
begin
  ShowSRDialog(true);
end;

procedure TfrmMain.acQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  LFileList: TStringList;
  I: Integer;
  LCurrentFileName: string;
  LEditingFile: TEditingFile;
begin
  LFileList := TStringList.Create;
  try
    for I := 0 to EditFileList.Count -1 do
    begin
      LEditingFile := TEditingFile(EditFileList.Items[I]);
      //Confirm save changes
      ConfirmChanges(LEditingFile);
      LFileList.Add(LEditingFile.FileName);
    end;
    if CurrentEditFile <> nil then
      LCurrentFileName := CurrentEditFile.FileName
    else
      LCurrentFileName := '';
    FEditorSettings.UpdateOpenedFiles(LFileList, LCurrentFileName);
    FEditorSettings.WriteSettings(nil, FEditorOptions);
  finally
    LFileList.Free;
  end;
end;

procedure TfrmMain.LoadOpenedFiles;
var
  I: Integer;
  LFileName: string;
  LIndex: Integer;
  LCurrentFileName: string;
begin
  LIndex := -1;
  for I := 0 to FEditorSettings.OpenedFileList.Count-1 do
  begin
    LCurrentFileName := FEditorSettings.CurrentFileName;
    LFileName := FEditorSettings.OpenedFileList.Strings[I];
    if OpenFile(LFileName, False) and SameText(LFileName, LCurrentFileName) then
      LIndex := I;
  end;
  if LIndex <> -1 then
    PageControl.ActivePageIndex := LIndex;
  UpdateMDViewer(True);
end;

procedure TfrmMain.LoadTimerTimer(Sender: TObject);
begin
  if FEditingInProgress then
  begin
    FEditingInProgress := False;
    dmResources.StopLoadingImages(False);
    UpdateMDViewer(True);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  FileVersionStr: string;
  LMarkDownMasks: string;
begin
  //Add Toolbar Shortcuts
  //Ctrl+Shift+Up/Down arrows
  acSuperscript.ShortCut := ShortCut(VK_UP, [ssCtrl, ssShift]);
  acSubscript.ShortCut := ShortCut(VK_DOWN, [ssCtrl, ssShift]);
  //Ctrl+Key number on the numeric keypad
  acHeader1.ShortCut := ShortCut(VK_NUMPAD1, [ssCtrl]);
  acHeader2.ShortCut := ShortCut(VK_NUMPAD2, [ssCtrl]);
  acHeader3.ShortCut := ShortCut(VK_NUMPAD3, [ssCtrl]);
  //Ctrl+Key number above the keyboard letters
  acHeader1.SecondaryShortCuts.Add('Ctrl+1');
  acHeader2.SecondaryShortCuts.Add('Ctrl+2');
  acHeader3.SecondaryShortCuts.Add('Ctrl+3');

  //Initialize AnimatedTaskDialog font size
  Screen.MessageFont.Size := Round(Screen.MessageFont.Size*1.2);
  InitializeStyledTaskDialogs(True, Screen.MessageFont);

  LMarkDownMasks := GetFileMasks(AMarkDownFileExt);
  OpenDialog.Filter :=
    Format('%s (%s)|%s', [MARKDOWN_FILES, LMarkDownMasks, LMarkDownMasks]);

  //Build opened-files list
  EditFileList := TObjectList.Create(True);
  FEditorOptions := TSynEditorOptionsContainer.create(self);
  FEditorSettings := TEditorSettings.CreateSettings(nil, FEditorOptions);
  dmResources.Settings := FEditorSettings;

  {$IFNDEF NO_VCL_STYLES}
  if not IsStyleHookRegistered(TCustomSynEdit, TScrollingStyleHook) then
    TStyleManager.Engine.RegisterStyleHook(TCustomSynEdit, TScrollingStyleHook);
  {$ENDIF}

  if (Trim(FEditorSettings.StyleName) <> '') and not SameText('Windows', FEditorSettings.StyleName) then
    TStyleManager.TrySetStyle(FEditorSettings.StyleName, False);

  //Double font for title
  lblTitle.Font.Size := lblTitle.Font.Size * 2;

  //Version
  FileVersionStr := GetVersionString(GetModuleLocation());
  Application.Title := Application.Title + ' (Ver.'+FileVersionStr+')';
  Caption := Application.Title;

  WindowState := wsMaximized;

  UpdateFromSettings(nil);

  //Explicit attach imagelist
  ActionList.Images := VirtualImageList;

  //staring folder
  CurrentDir := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath);

  //Initialize print output
  InitSynEditPrint;

  //Update all editor options
  UpdateEditorsOptions;
end;

procedure TfrmMain.acNewFileExecute(Sender: TObject);
var
  NewExt : string;
  EditingFile : TEditingFile;
begin
  NewExt := 'md';

  //Create object to manage new file
  EditingFile := TEditingFile.Create(CurrentDir+'New.'+NewExt,
    FEditorSettings.UseDarkStyle);
  Try
    AddEditingFile(EditingFile);
    if EditingFile.SynEditor.CanFocus then
      EditingFile.SynEditor.SetFocus;
  Except
    EditingFile.Free;
    raise;
  End;
end;

procedure TfrmMain.acSearchUpdate(Sender: TObject);
begin
  acSearch.Enabled := (CurrentEditor <> nil) and (CurrentEditor.Text <> '');
end;

procedure TfrmMain.acToolbarUpdate(Sender: TObject);
var
  LAction: TAction;
begin
  LAction := Sender as TAction;
  LAction.Enabled := (CurrentEditFile <> nil) and
    (CurrentEditFile.SynEditor.Focused) and
    not (CurrentEditFile.SynEditor.SelectionMode = TSynSelectionMode.smColumn);
end;

procedure TfrmMain.acReplaceUpdate(Sender: TObject);
begin
  acReplace.Enabled := (CurrentEditor <> nil) and (CurrentEditor.Text <> '')
    and not CurrentEditor.ReadOnly;
end;

procedure TfrmMain.acCloseExecute(Sender: TObject);
begin
  //Remove editing file
  RemoveEditingFile(CurrentEditFile);
end;

procedure TfrmMain.acEditCopyExecute(Sender: TObject);
begin
  CurrentEditor.CopyToClipboard;
end;

procedure TfrmMain.acEditCopyUpdate(Sender: TObject);
begin
  acEditCopy.Enabled := (CurrentEditFile <> nil) and
    (CurrentEditFile.SynEditor.SelEnd - CurrentEditFile.SynEditor.SelStart > 0);
end;

procedure TfrmMain.acEditCutExecute(Sender: TObject);
begin
  CurrentEditor.CutToClipboard;
end;

procedure TfrmMain.acEditPasteExecute(Sender: TObject);
begin
  CurrentEditor.PasteFromClipboard;
end;

procedure TfrmMain.acEditSelectAllExecute(Sender: TObject);
begin
  CurrentEditor.SelectAll;
end;

procedure TfrmMain.acEditUndoExecute(Sender: TObject);
begin
  if CurrentEditor <> nil then
    CurrentEditor.Undo;
end;

procedure TfrmMain.acEditUndoUpdate(Sender: TObject);
begin
  acEditUndo.Enabled := (CurrentEditor <> nil) and CurrentEditor.Modified;
end;

procedure TfrmMain.InsertMDCommand(const ACommand: string);
var
  LSynEdit: TSynEdit;
  LSelectedText: string;
begin
  LSynEdit := CurrentEditFile.SynEditor;
  LSelectedText := LSynEdit.SelText;
  //If there is no selected text
  if LSelectedText = '' then
  begin
    LSynEdit.SelText := ACommand;
  end
  //If there is selected text
  else
  begin
    //Replace list commands keeping indentation
    if (ACommand = '1. ') or (ACommand = '* ') then
    begin
      LSelectedText := TRegEx.Replace(LSelectedText,
        '(^[\t ]*)(### |## |# |\* |- |\d+\. |> )*((?![\t ]*[*]{3,}|[\t ]*[-]{3,}).+)',
        '$1'+ACommand+'$3', [roMultiLine]);
    end
    //Remove other commands if is not the "---" command
    else if not ACommand.StartsWith('---') then
    begin
      //Remove ordered list commands from the selected text
      LSelectedText := TRegEx.Replace(LSelectedText, '\d+\. ', '', [roMultiLine]);
      //Remove any other "not contains" commands
      LSelectedText := TRegEx.Replace(LSelectedText,
        '(^[\t ]*)(### |## |# |\* |- |> )?', '', [roMultiLine]);
      //Add the command to all lines with content except lines with the
      //commands "---" and "***" at the begining
      LSelectedText := TRegEx.Replace(LSelectedText,
        '^(?![\t ]*[*]{3,}|[\t ]*[-]{3,}).+$', ACommand+'$0', [roMultiLine]);
    end
    else
      //Add the command to all lines with content except lines with the
      //commands "---" and "***" at the end
      LSelectedText := TRegEx.Replace(LSelectedText,
        '^(?![\t ]*[-]{3,}|[\t ]*[*]{3,}).+$', '$0'+sLineBreak+ACommand, [roMultiLine]);
    //Write elaborated text back to the document
    LSynEdit.SelText := LSelectedText;
  end;
end;

procedure TfrmMain.InsertMDCommandContains(const ACommandOpen, ACommandClose: string);
var
  LSynEdit: TSynEdit;
  LSelectedText: string;
  LSize: Integer;
  LCoord: TBufferCoord;
begin
  LSize := Length(ACommandClose);
  LSynEdit := CurrentEditFile.SynEditor;
  LSelectedText := LSynEdit.SelText;
  //If there is no selected text, add the opening and closing commands and
  //place the caret in the middle
  if LSelectedText = '' then
  begin
    LSynEdit.SelText := ACommandOpen+ACommandClose;
    //Move the editor caret inside the open and close comands
    LCoord := LSynEdit.CaretXY;
    LCoord.Char := LCoord.Char-LSize;
    LSynEdit.SetCaretAndSelection(LCoord, LCoord, LCoord);
  end
  else
    //Insert three backticks (```) when the selected text has multiple rows in
    //order to preserve the indentation
    if (ACommandOpen = '`') and (Pos(sLineBreak,LSelectedText)>0) then
      LSynEdit.SelText := '```'+sLineBreak+LSelectedText+sLineBreak+'```'
    else
      //Insert the open and close command in each row, except in the empty rows
      //or the rows with line commands '---' or '***'
      LSynEdit.SelText := TRegEx.Replace(LSelectedText,
      '(^)((?![\t ]*[-]{3,}|[\t ]*[*]{3,}).+$)',
      '$1'+ACommandOpen+'$2'+ACommandClose, [roMultiLine]);
end;

procedure TfrmMain.InputUrl(const APrefix: string; AType: TInputType);
var
  LSynEdit: TSynEdit;
  LSelectedText: string;
begin
  LSynEdit := CurrentEditFile.SynEditor;
  LSelectedText := LSynEdit.SelText;

  if InputUrlDialog(Self, CurrentEditFile.ServerRoot,
    APrefix, AType, LSelectedText) = mrOk then
    LSynEdit.SelText := LSelectedText;
end;

procedure TfrmMain.acHeader1Execute(Sender: TObject);
begin
  InsertMDCommand('# ');
end;

procedure TfrmMain.acHeader2Execute(Sender: TObject);
begin
  InsertMDCommand('## ');
end;

procedure TfrmMain.acHeader3Execute(Sender: TObject);
begin
  InsertMDCommand('### ');
end;

procedure TfrmMain.acLinkExecute(Sender: TObject);
begin
  InputUrl('[', itLink);
end;

procedure TfrmMain.acImageExecute(Sender: TObject);
begin
  InputUrl('![', itImage);
end;

procedure TfrmMain.acTableExecute(Sender: TObject);
var
  LdlgTable: TdlgCreateTable;
  LSynEdit: TSynEdit;
begin
  LdlgTable := TdlgCreateTable.Create(self);
  LSynEdit := CurrentEditFile.SynEditor;
  try
    LdlgTable.ShowModal();
    if LdlgTable.ModalResult = mrOk then
      LSynEdit.SelText := LdlgTable.ATableText;
  finally
    LdlgTable.Free();
  end;
end;

procedure TfrmMain.acBoldExecute(Sender: TObject);
begin
  InsertMDCommandContains('**', '**');
end;

procedure TfrmMain.acItalicExecute(Sender: TObject);
begin
  InsertMDCommandContains('_', '_');
end;

procedure TfrmMain.acStrikeExecute(Sender: TObject);
begin
  InsertMDCommandContains('~~', '~~');
end;

procedure TfrmMain.acUnderlineExecute(Sender: TObject);
begin
  InsertMDCommandContains('++', '++');
end;

procedure TfrmMain.acCodeExecute(Sender: TObject);
begin
  InsertMDCommandContains('`', '`');
end;

procedure TfrmMain.acMarkerExecute(Sender: TObject);
begin
  InsertMDCommandContains('==', '==');
end;

procedure TfrmMain.acSubscriptExecute(Sender: TObject);
begin
  InsertMDCommandContains('<sub>', '</sub>');
end;

procedure TfrmMain.acSuperscriptExecute(Sender: TObject);
begin
  InsertMDCommandContains('<sup>', '</sup>');
end;

procedure TfrmMain.acUnorderedListExecute(Sender: TObject);
begin
  InsertMDCommand('* ');
end;

procedure TfrmMain.acOrderedListExecute(Sender: TObject);
begin
  InsertMDCommand('1. ');
end;

procedure TfrmMain.acBlockquoteExecute(Sender: TObject);
begin
  InsertMDCommand('> ');
end;

procedure TfrmMain.acHorizontalRuleExecute(Sender: TObject);
begin
  InsertMDCommand('---'+sLineBreak);
end;

procedure TfrmMain.acHTMLViewerUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (CurrentEditFile <> nil) and (CurrentEditFile.HTMLViewer <> nil);
end;

procedure TfrmMain.acHelpExecute(Sender: TObject);
var
  LHelpFileName: TFileName;
begin
  LHelpFileName := GetMarkdownHelpFileName;
  if OpenFile(LHelpFileName) then
    UpdateMDViewer(True);
end;

procedure TfrmMain.acHelpUpdate(Sender: TObject);
var
  LHelpFileName: TFileName;
begin
  LHelpFileName := GetMarkdownHelpFileName;
  acHelp.Visible := FileExists(LHelpFileName);
end;

procedure TfrmMain.SynEditChange(Sender: TObject);
begin
  if Sender = CurrentEditor then
  begin
    FEditingInProgress := True;
    LoadTimer.Enabled := False;
    dmResources.StopLoadingImages(True);
    UpdateMDViewer(False);
  end;
end;

procedure TfrmMain.SynEditEnter(Sender: TObject);
begin
  CloseSplitViewMenu;
end;

function TfrmMain.AddEditingFile(const EditingFile: TEditingFile): Integer;
var
  LTabSheet: TTabSheet;
  LEditor: TSynEdit;
  LFEViewer: THtmlViewer;
  LSplitter: TSplitter;
begin
  //Add file to opened-list
  Result := EditFileList.Add(EditingFile);
  //Create the Tabsheet page associated to the file
  LTabSheet := nil;
  LEditor := nil;
  LFEViewer := nil;
  Try
    LTabSheet := TTabSheet.Create(self);
    LTabSheet.PageControl := PageControl;
    //Use TAG of tabsheet to store the object pointer
    LTabSheet.Tag := NativeInt(Pointer(EditingFile));
    LTabSheet.Caption := EditingFile.Name;
    LTabSheet.Hint := EditingFile.FileName;
    LTabSheet.Imagename := EditingFile.ImageName+'-gray';
    LTabSheet.Parent := PageControl;
    LTabSheet.TabVisible := True;
    EditingFile.TabSheet := LTabSheet;

    LFEViewer := THtmlViewer.Create(nil);
    LFEViewer.ScrollBars := ssNone;
    LFEViewer.Align := alRight;
    LFEViewer.Width := LTabSheet.Width div 2;
    LFEViewer.Parent := LTabSheet;
    LFEViewer.PopupMenu := PopHTMLViewer;
    LFEViewer.DefBackground := StyleServices.GetSystemColor(clWindow);
    LFEViewer.OnHotSpotClick := HtmlViewerHotSpotClick;
    LFEViewer.OnImageRequest := dmResources.HtmlViewerImageRequest;
    LFEViewer.ScrollBars := TScrollStyle.ssVertical;

    LSplitter := TSplitter.Create(LTabSheet);
    LSplitter.Align := alRight;
    LSplitter.Left := LFEViewer.Left-1;
    LSplitter.AutoSnap := False;
    LSplitter.Width := 6;
    LSplitter.Parent := LTabSheet;
    LSplitter.Beveled := True;
    LSplitter.OnMoved := SplitterMoved;
    LSplitter.Tag := NativeInt(EditingFile);

    EditingFile.Splitter := LSplitter;
    EditingFile.HTMLViewer := LFEViewer;

    //Create the SynEdit object editor into the TabSheet that is the owner
    LEditor := TSynEdit.Create(LTabSheet);
    LEditor.OnChange := SynEditChange;
    LEditor.OnEnter := SynEditEnter;
    LEditor.MaxUndo := 5000;
    LEditor.Align := alClient;
    LEditor.Parent := LTabSheet;
    LEditor.SearchEngine := SynEditSearch;
    LEditor.PopupMenu := popEditor;

    //Assign user preferences to the editor
    FEditorOptions.AssignTo(LEditor);
    LEditor.MaxScrollWidth := 3000;
    EditingFile.SynEditor := LEditor;
    UpdateFromSettings(LEditor);
    UpdateHighlighter(LEditor);

    LEditor.Visible := True;

    //Show the tabsheet
    LTabSheet.Visible := True;
  Except
    LTabSheet.Free;
    LEditor.Free;
    LFEViewer.Free;
    raise;
  End;

    //Make the Tabsheet the current page
  PageControl.ActivePage := LTabSheet;

    //and call "change" of pagecontrol
  PageControl.OnChange(PageControl);
end;

procedure TfrmMain.UpdateMDViewer(const AReloadImages: Boolean);
begin
  if FProcessingFiles then
    Exit;
  if (CurrentEditor <> nil) then
  begin
    Screen.Cursor := crHourGlass;
    try
      if CurrentEditor.Modified then
        pageControl.ActivePage.Imagename := CurrentEditFile.ImageName
      else
        pageControl.ActivePage.Imagename := CurrentEditFile.ImageName+'-gray';

      StatusBar.Panels[STATUSBAR_MESSAGE].Text := CurrentEditFile.FileName;

      //Show HTML content of MarkDown
      CurrentEditFile.ShowMarkDownAsHTML(FEditorSettings, AReloadImages);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.PageControlChange(Sender: TObject);
begin
  CloseSplitViewMenu;
  //Setting the Editor caption as the actual file opened
  if CurrentEditFile <> nil then
  begin
    FMDFile := CurrentEditFile;
    if (FMDFile <> nil) and (FMDFile.SynEditor.CanFocus) then
      FMDFile.SynEditor.SetFocus;
  end;
  UpdateMDViewer(False);
end;

procedure TfrmMain.PageControlMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
{$WRITEABLECONST ON}
const oldPos : integer = -2;
{$WRITEABLECONST OFF}
var
  iot : integer;

  LhintPause: Integer;
  LTabIndex: Integer;
begin
  inherited;
  LTabIndex := PageControl.IndexOfTabAt(X, Y);
  if (LTabIndex >= 0) and (PageControl.Hint <> PageControl.Pages[LTabIndex].Hint) then
  begin
    LHintPause := Application.HintPause;
    try
      if PageControl.Hint <> '' then
        Application.HintPause := 0;
      Application.CancelHint;
      PageControl.Hint := PageControl.Pages[LTabIndex].Hint;
      PageControl.ShowHint := true;
      Application.ProcessMessages; // force hint to appear
    finally
      Application.HintPause := LHintPause;
    end;
  end;

  iot := TTabControl(Sender).IndexOfTabAt(x,y);
  if (iot > -1) then
  begin
    if iot <> oldPos then
      ShowTabCloseButtonOnHotTab;
  end;
  oldPos := iot;
end;

procedure TfrmMain.ProcessorDialectComboBoxSelect(Sender: TObject);
var
  LDialect: TMarkdownProcessorDialect;
begin
  LDialect := TMarkdownProcessorDialect(ProcessorDialectComboBox.ItemIndex);
  if FEditorSettings.ProcessorDialect <> LDialect then
  begin
    FEditorSettings.ProcessorDialect := LDialect;
    WriteSettingsToIni;
    UpdateMDViewer(False);
  end;
end;

procedure TfrmMain.acSaveUpdate(Sender: TObject);
begin
  acSave.Enabled := (CurrentEditor <> nil) and (CurrentEditor.Modified);
end;

procedure TfrmMain.CloseSplitViewMenu;
begin
  SV.Close;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
  FDropTarget := TDropTarget.Create(WindowHandle, Self);
end;

function TfrmMain.CurrentEditFile: TEditingFile;
begin
  if (PageControl.ActivePage <> nil) then
    Result := TEditingFile(PageControl.ActivePage.Tag)
  else
    Result := nil;
end;

function TfrmMain.CurrentEditor: TSynEdit;
begin
  if CurrentEditFile <> nil then
    Result := CurrentEditFile.SynEditor else
    Result := nil;
end;

procedure TfrmMain.ConfirmChanges(EditingFile: TEditingFile);
var
  LConfirm: integer;
begin
  //Confirm save changes
  if EditingFile.SynEditor.Modified then
  begin
    LConfirm := StyledMessageDlg(
      Format(CONFIRM_CHANGES,[EditingFile.FileName]),
      mtWarning, [mbYes, mbNo], 0);
    if LConfirm = mrYes then
      EditingFile.SaveToFile
    else if LConfirm = mrCancel then
      Abort;
    //if LConfirm = mrNo continue without saving
  end;
end;

procedure TfrmMain.RemoveEditingFile(EditingFile: TEditingFile);
var
  i : integer;
  pos : integer;
begin
  pos := -1;
  for i := 0 to EditFileList.Count -1 do
  begin
    if EditFileList.Items[i] = EditingFile then
    begin
      pos := i;
      break;
    end;
  end;
  if pos = -1 then
    raise EComponentError.Create(CLOSING_PROBLEMS);

  //Confirm save changes
  ConfirmChanges(EditingFile);

  PanelCloseButton.Visible := False;

  //Remove the reference
  if FMDFile = EditingFile then
    FMDFile := nil;

  //Delete the file from the Opened-List
  EditFileList.Delete(pos);

  //Delete the TabSheet
  PageControl.Pages[pos].Free;

  //Activate the previous page and call "change" of pagecontrol
  if pos > 0 then
    PageControl.ActivePageIndex := pos-1;

  //Force "change" of Page
  PageControl.OnChange(PageControl);
end;

procedure TfrmMain.acCloseAllUpdate(Sender: TObject);
begin
  acCloseAll.Enabled := EditFileList.Count > 0;
end;

procedure TfrmMain.acCloseAllExecute(Sender: TObject);
begin
  FProcessingFiles := True;
  try
    PanelCloseButton.Visible := False;
    while EditFileList.Count > 0 do
      RemoveEditingFile(TEditingFile(EditFileList.items[0]));
  finally
    FProcessingFiles := False;
  end;
end;

procedure TfrmMain.acSaveAllUpdate(Sender: TObject);
begin
  acSaveAll.Enabled := ModifiedCount > 0;
end;

function TfrmMain.ModifiedCount: integer;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to EditFileList.Count -1 do
  begin
    if TEditingFile(EditFileList.items[i]).SynEditor.Modified then
    begin
      Inc(Result);
    end;
  end;
end;

procedure TfrmMain.acSaveAllExecute(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to EditFileList.Count -1 do
  with TEditingFile(EditFileList.items[i]) do
  begin
    if SynEditor.Modified then
    begin
      SaveToFile;
    end;
  end;
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  ShowAboutForm(DialogPosRect, Title_MDViewer);
end;

procedure TfrmMain.acSearchAgainExecute(Sender: TObject);
begin
  gbSearchFromCaret := True;
  DoSearchReplaceText(False, gbSearchBackwards);
end;

procedure TfrmMain.acSearchAgainUpdate(Sender: TObject);
begin
  acSearchAgain.Enabled := gsSearchText <> '';
end;

procedure TfrmMain.InitSynEditPrint;
var
  AFont: TFont;
begin
  AFont := TFont.Create;
  Try
    with SynEditPrint.Header do begin
        {First line, default font, left aligned}
      Add(PAGE_HEADER_FIRST_LINE_LEFT, nil, taLeftJustify, 1);
        {First line, default font, right aligned}
      Add(PAGE_HEADER_FIRST_LINE_RIGHT, nil, taRightJustify, 1);
      AFont.Assign(DefaultFont);
      AFont.Size := 6;
    end;
    with SynEditPrint.Footer do begin
      AFont.Assign(DefaultFont);
      Add(PAGE_FOOTER_FIRST_LINE_LEFT, nil, taRightJustify, 1);
      AFont.Size := 6;
      Add(PAGE_FOOTER_FIRST_LINE_RIGHT, AFont, taLeftJustify, 1);
    end;
  Finally
    AFont.Free;
  End;
end;

procedure TfrmMain.actnPrinterSetupExecute(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TfrmMain.actnPrintPreviewExecute(Sender: TObject);
var
  PreviewForm: TBegaHtmlPrintPreviewForm;
begin
  PreviewForm := TBegaHtmlPrintPreviewForm.Create(nil);
  try
    PreviewForm.HtmlViewer := CurrentEditFile.HTMLViewer;
    PreviewForm.ShowModal;
  finally
    PreviewForm.free;
  end;
end;

procedure TfrmMain.actnPrintExecute(Sender: TObject);
begin
  SetSynEditPrintProperties(SynEditPrint);
  if PrintDialog.Execute then
  begin
    SynEditPrint.Print;
  end;
end;

procedure TfrmMain.actnPageSetupExecute(Sender: TObject);
begin
  SetSynEditPrintProperties(PageSetupDlg.SynEditPrint);
  PageSetupDlg.SetValues(SynEditPrint);
  if PageSetupDlg.ShowModal = mrOk then
    PageSetupDlg.GetValues(SynEditPrint);
end;

procedure TfrmMain.SetMDFontSize(const Value: Integer);
var
  LScaleFactor: Single;
begin
  if (CurrentEditor <> nil) and (Value >= MinfontSize) and (Value <= MaxfontSize) then
  begin
    if FMDFontSize <> 0 then
      LScaleFactor := CurrentEditor.Font.Height / FMDFontSize
    else
      LScaleFactor := 1;
    CurrentEditor.Font.PixelsPerInch := Self.PixelsPerInch;
    CurrentEditor.Font.Height := Round(Value * LScaleFactor * Self.ScaleFactor);
    FEditorSettings.MDFontSize := Value;
  end;
  FMDFontSize := Value;
end;

procedure TfrmMain.FileSavedAskToOpen(const AFileName: string);
begin
  if StyledMessageDlg(Format(FILE_SAVED,[AFileName]),
    TMsgDlgType.mtInformation, [mbYes, MbNo], 0) = mrYes then
  begin
    ShellExecute(handle, 'open', PChar(AFilename), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TfrmMain.acSaveHTMLFileExecute(Sender: TObject);
var
  LStream: TStringStream;
begin
  SaveDialog.FileName := ChangeFileExt(CurrentEditFile.FileName, '.htm');
  SaveDialog.Filter := 'MarkDown file in HTML (*.htm)|*.htm';
  if SaveDialog.Execute then
  begin
    LStream := TStringStream.Create(CurrentEditFile.HTMLViewer.Text,
      TEncoding.UTF8);
    try
      LStream.SaveToFile(SaveDialog.FileName);
      FileSavedAskToOpen(SaveDialog.FileName);
    finally
      LStream.Free;
    end;
  end;
end;

procedure TfrmMain.acSavePDFFileExecute(Sender: TObject);
begin
  SaveDialog.FileName := ChangeFileExt(CurrentEditFile.FileName, '.pdf');
  SaveDialog.Filter := 'Markdown file in PDF (*.pdf)|*.pdf';
  if SaveDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      HTMLToPDF(SaveDialog.FileName);
      FileSavedAskToOpen(SaveDialog.FileName);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.SetHTMLFontSize(const Value: Integer);
var
  LScaleFactor: Single;
begin
  if (CurrentEditor <> nil) and (Value >= MinfontSize) and (Value <= MaxfontSize) then
  begin
    if FHTMLFontSize <> 0 then
      LScaleFactor := CurrentEditFile.HTMLViewer.DefFontSize / FMDFontSize
    else
      LScaleFactor := 1;
    CurrentEditFile.HTMLViewer.DefFontSize := Round(Value * LScaleFactor * Self.ScaleFactor);
    FEditorSettings.HTMLFontSize := Value;
  end;
  FHTMLFontSize := Value;
end;

procedure TfrmMain.SetSynEditPrintProperties(SynEditPrint : TSynEditPrint);
begin
  SynEditPrint.SynEdit := CurrentEditor;
  SynEditPrint.Title := CurrentEditFile.FFileName;
  SynEditPrint.Highlighter := dmResources.SynXMLSyn;
end;

procedure TfrmMain.actnEditOptionsExecute(Sender: TObject);
var
  LEditOptionsDialog: TSynEditOptionsDialog;
begin
  if CurrentEditor <> nil then
    FEditorOptions.Assign(CurrentEditor);
  LEditOptionsDialog := TSynEditOptionsDialog.Create(nil);
  try
    if LEditOptionsDialog.Execute(FEditorOptions) then
    begin
      UpdateEditorsOptions;
    end;
  finally
    LEditOptionsDialog.Free;
  end;
end;

procedure TfrmMain.UpdateEditorsOptions;
var
  i : integer;
  EditingFile : TEditingFile;
begin
  FEditorSettings.MDFontName := FEditorOptions.Font.Name;
  MDFontSize := FEditorOptions.Font.Size;

  for i := 0 to EditFileList.Count -1 do
  begin
    EditingFile := TEditingFile(EditFileList.items[i]);
    FEditorOptions.AssignTo(EditingFile.SynEditor);
    EditingFile.HTMLViewer.DefFontName := FEditorSettings.HTMLFontName;
    EditingFile.HTMLViewer.DefFontSize := FEditorSettings.HTMLFontSize;
    EditingFile.HTMLViewer.DefBackground := StyleServices.GetSystemColor(clWindow);
  end;
  Statusbar.Panels[STATUSBAR_PANEL_FONTNAME].Text := FEditorOptions.Font.Name;
  Statusbar.Panels[STATUSBAR_PANEL_FONTSIZE].Text := IntToStr(FEditorOptions.Font.Size);
end;

procedure TfrmMain.UpdateFromSettings(AEditor: TSynEdit);
begin
  UpdateApplicationStyle(FEditorSettings.StyleName);
  if AEditor <> nil then
  begin
  FEditorSettings.ReadSettings(AEditor.Highlighter, self.FEditorOptions);
    AEditor.ReadOnly := False;
  end
  else
    FEditorSettings.ReadSettings(nil, self.FEditorOptions);

  ProcessorDialectComboBox.ItemIndex := ord(FEditorSettings.ProcessorDialect);

  if FEditorSettings.MDFontSize >= MinfontSize then
    MDFontSize := FEditorSettings.MDFontSize
  else
    MDFontSize := MinfontSize;
  if FEditorSettings.HTMLFontSize >= MinfontSize then
    HTMLFontSize := FEditorSettings.HTMLFontSize
  else
    HTMLFontSize := MinfontSize;
  InitEditorOptions;
  UpdateEditorsOptions;
  UpdateHighlighter(AEditor);
end;

procedure TfrmMain.UpdateHighlighter(ASynEditor: TSynEdit);
var
  LBackgroundColor: TColor;
begin
  if ASynEditor = nil then
    Exit;
  LBackgroundColor := StyleServices.GetSystemColor(clWindow);
  ASynEditor.Highlighter := dmResources.GetSynHighlighter(
    FEditorSettings.UseDarkStyle, LBackgroundColor);
  //Assign custom colors to the Highlighter
  FEditorSettings.ReadSettings(ASynEditor.Highlighter, self.FEditorOptions);
end;

procedure TfrmMain.UpdateHighlighters;
var
  LEditingFile: TEditingFile;
  I: Integer;
begin
  for i := 0 to EditFileList.Count -1 do
  begin
    LEditingFile := EditFileList.Items[i] as TEditingFile;
    UpdateHighlighter(LEditingFile.SynEditor);
  end;
end;

procedure TfrmMain.actnEditingUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := CurrentEditFile <> nil;
end;

procedure TfrmMain.actnFontExecute(Sender: TObject);
begin
  if Sender = actnEnlargeFont then
    MDFontSize := FEditorOptions.Font.Size+1
  else if Sender = actnReduceFont then
    MDFontSize := FEditorOptions.Font.Size-1
  else
    Exit;
  FEditorOptions.Font.Size := MDFontSize;
  UpdateEditorsOptions;
end;

procedure TfrmMain.InitEditorOptions;
begin
  with FEditorOptions do
  begin
    Font.Name := FEditorSettings.MDFontName;
    Font.Size := MDFontSize;
    TabWidth := 2;
    WantTabs := False;
    Options := Options - [eoSmartTabs];
    Gutter.Font.Name := Font.Name;
    Gutter.Font.Size := Font.Size;
  end;
end;

procedure TfrmMain.actnSaveAsExecute(Sender: TObject);
begin
  SaveDialog.FileName := ChangeFileExt(CurrentEditFile.FileName, '.md');
  SaveDialog.Filter := 'Markdown Text file (*.md)';

  SaveDialog.FileName := CurrentEditFile.FileName;
  if SaveDialog.Execute then
  begin
    if CurrentEditFile.FileName <> SaveDialog.FileName then
    begin
      CurrentEditFile.FileName := SaveDialog.FileName;
    end;
    CurrentEditFile.SaveToFile;

    //call the "onchange" event of PageControl
    PageControl.OnChange(PageControl);
  end;
end;

procedure TfrmMain.acZoomExecute(Sender: TObject);
var
  LValue: Integer;
begin
  if Sender = acZoomIn then
    LValue := 1
  else
    LValue := -1;
  FEditorSettings.HTMLFontSize := FEditorSettings.HTMLFontSize + LValue;
  CurrentEditFile.ShowMarkDownAsHTML(FEditorSettings, False);
end;

procedure TfrmMain.RecentPopupMenuPopup(Sender: TObject);
var
  I: Integer;
  LMenuItem: TMenuItem;
  LFileName: string;
begin
  RecentPopupMenu.Items.Clear;
  for I := 0 to FEditorSettings.HistoryFileList.Count -1 do
  begin
    LFileName := FEditorSettings.HistoryFileList.Strings[I];
    LMenuItem := TMenuItem.Create(nil);
    if Length(LFileName) > 100 then
      LMenuItem.Caption := Copy(LFileName,1,20)+'...'+RightStr(LFileName, 80)
    else
      LMenuItem.Caption := LFileName;
    LMenuItem.Hint := LFileName;
    LMenuItem.OnClick := HistoryListClick;
    RecentPopupMenu.Items.Add(LMenuItem);
  end;
end;

function TfrmMain.CurrentEditorState: string;
begin
  if CurrentEditor = nil then
    Result := ''
  else if CurrentEditor.ReadOnly then
    Result := STATE_READONLY
  else if CurrentEditor.InsertMode then
    Result := STATE_INSERT
  else
    Result := STATE_OVERWRITE;
end;

procedure TfrmMain.UpdateStatusBarPanels;
var
  ptCaret: TBufferCoord;
begin
  if CurrentEditor <> nil then
  begin
    ptCaret := CurrentEditor.CaretXY;
    StatusBar.Panels[STATUSBAR_PANEL_CARET].Text := Format(' %6d:%3d ', [ptCaret.Line, ptCaret.Char]);
    if CurrentEditor.Modified then
      StatusBar.Panels[STATUSBAR_PANEL_MODIFIED].Text := SMODIFIED
    else
      StatusBar.Panels[STATUSBAR_PANEL_MODIFIED].Text := SUNMODIFIED;
    StatusBar.Panels[STATUSBAR_PANEL_STATE].Text := CurrentEditorState;
  end
  else
  begin
    StatusBar.Panels[STATUSBAR_PANEL_CARET].Text := '';
    StatusBar.Panels[STATUSBAR_PANEL_MODIFIED].Text := '';
    StatusBar.Panels[STATUSBAR_PANEL_STATE].Text := '';
  end;
end;

procedure TfrmMain.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
var
  LMinMaxInfo: PMinMaxInfo;
begin
  if not (csReading in ComponentState) then
  begin
    LMinMaxInfo := Message.MinMaxInfo;
    with LMinMaxInfo^ do
    begin
      with ptMinTrackSize do
      begin
        if MinFormWidth > 0 then X := MinFormWidth;
        if MinFormHeight > 0 then Y := MinFormHeight;
      end;
      with ptMaxTrackSize do
      begin
        if MaxFormWidth > 0 then X := MaxFormWidth;
        if MaxFormHeight > 0 then Y := MaxFormHeight;
      end;
      ConstrainedResize(ptMinTrackSize.X, ptMinTrackSize.Y, ptMaxTrackSize.X,
        ptMaxTrackSize.Y);
    end;
  end;
  inherited;
end;

procedure TfrmMain.ActionListExecute(Action: TBasicAction;
  var Handled: Boolean);
begin
  if (Action <> actMenu) and (Action <> OpenRecentAction) then
    CloseSplitViewMenu;
  Handled := False;
end;

procedure TfrmMain.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  InitialDir : string;
  LFileName: string;
begin
  if CurrentEditFile <> nil then
    Caption := Application.Title+' - '+CurrentEditFile.FileName
  else
    Caption := Application.Title;

  UpdateStatusBarPanels;
  if not FirstAction then
  begin
    FirstAction := True;
    //Load previous opened-files
    LoadOpenedFiles;

    //Initialize Open and Save Dialog with application path
    LFileName := ParamStr(1);
    if LFileName <> '' then
    begin
      TLogPreview.Add(Format('ParamStr(1): %s', [LFileName]));
      //Load file passed at command line
      InitialDir := ExtractFilePath(LFileName);
      OpenFile(LFileName);
      UpdateMDViewer(True);
    end
    else
      InitialDir := '.';

    OpenDialog.InitialDir := InitialDir;
    SaveDialog.InitialDir := InitialDir;
  end;
  LoadTimer.Enabled := True;
end;

procedure TfrmMain.actMenuExecute(Sender: TObject);
begin
  if SV.Opened then
    CloseSplitViewMenu
  else
    SV.Open;
end;

procedure TfrmMain.WriteSettingsToIni;
begin
  if CurrentEditor <> nil then
    FEditorSettings.WriteSettings(CurrentEditor.Highlighter, FEditorOptions)
  else
    FEditorSettings.WriteSettings(nil, FEditorOptions);
end;

procedure TfrmMain.actnSettingsExecute(Sender: TObject);
begin
  if ShowSettings(DialogPosRect,
    Title_MDViewer,
    CurrentEditor, FEditorSettings, True) then
  begin
    WriteSettingsToIni;
    UpdateFromSettings(CurrentEditor);
    UpdateMDViewer(True);
    UpdateHighlighters;
  end;
end;

procedure TfrmMain.actnSettingsUpdate(Sender: TObject);
begin
  actnSettings.Enabled := True;
end;

procedure TfrmMain.actionForFileUpdate(Sender: TObject);
begin
  (Sender As TAction).Enabled := (CurrentEditor <> nil)
    and not CurrentEditor.ReadOnly;
end;

procedure TfrmMain.AddOpenedFile(const AFileName: string);
var
  i : integer;
begin
  //Add the opened file to the opened-file list
  i := FEditorSettings.HistoryFileList.IndexOf(AFileName);
  if i >= 0 then
    FEditorSettings.HistoryFileList.Delete(i);
  //max 15 items
  if FEditorSettings.HistoryFileList.Count > 15 then
    FEditorSettings.HistoryFileList.Delete(14);
  //add the last opened-file at first position
  FEditorSettings.HistoryFileList.Insert(0, AFileName);
end;

procedure TfrmMain.AdjustCompactWidth;
begin
  //Change size of compact because Scrollbars appears
  if (Height / ScaleFactor) > 900 then
    SV.CompactWidth := Round(44 * ScaleFactor)
  else
    SV.CompactWidth := Round(66 * ScaleFactor);
  if (CurrentEditFile <> nil) and (CurrentEditFile.HTMLViewer <> nil) and (CurrentEditFile.HTMLViewer.Width > CurrentEditFile.TabSheet.Width) then
    CurrentEditFile.HTMLViewer.Width := width div 3;
end;

procedure TfrmMain.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  LockWindowUpdate(0);
end;

procedure TfrmMain.FormBeforeMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  LockWindowUpdate(Handle);
end;

procedure TfrmMain.catMenuItemsGetHint(Sender: TObject;
  const Button: TButtonItem; const Category: TButtonCategory;
  var HintStr: string; var Handled: Boolean);
var
  LActionDisabled: Boolean;
begin
  inherited;
  if not Assigned(Button) then
    Exit;
  if Button.Action is TAction then
    LActionDisabled := not TAction(Button.Action).Enabled
  else if Button.Action is TFileAction then
    LActionDisabled := not TFileAction(Button.Action).Enabled
  else
    LActionDisabled := False;
  if LActionDisabled then
  begin
    HintStr := '';
    Handled := True;
  end;
end;

procedure TfrmMain.catMenuItemsMouseLeave(Sender: TObject);
begin
  inherited;
  Screen.cursor := crDefault;
end;

procedure TfrmMain.catMenuItemsMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  LButton: TButtonItem;
begin
  inherited;

  LButton := catMenuItems.GetButtonAt(X,Y)  ;
  if Assigned(LButton) and Assigned(LButton.Action) then
  begin
    if (LButton.Action is TAction) then
    begin
      TAction(LButton.Action).Update;
      if TAction(LButton.Action).Enabled then
        Screen.Cursor := crHandPoint
      else
        Screen.Cursor := crNo;
    end
    else if LButton.Action is TFileAction then
    begin
      TFileAction(LButton.Action).Update;
      if TFileAction(LButton.Action).Enabled then
        Screen.Cursor := crHandPoint
      else
        Screen.Cursor := crNo;
    end;
  end
  else
    Screen.Cursor := crDefault;
end;

procedure TfrmMain.HistoryListClick(Sender: TObject);
var
  LFileName : string;
begin
  LFilename := (Sender as TMenuItem).Hint;
  //Load the selected file
  OpenFile(LFileName);
  UpdateMDViewer(True);
  if (CurrentEditor <> nil) and (CurrentEditor.CanFocus) then
    (CurrentEditor.SetFocus);
  CloseSplitViewMenu;
end;

procedure TfrmMain.HTMLToPDF(const APDFFileName: TFileName);
var
  lHtmlToPdf: TvmHtmlToPdfGDI;
  LOldColor: TColor;
begin
  lHtmlToPdf := TvmHtmlToPdfGDI.Create();
  try
    lHtmlToPdf.PDFMarginLeft := FEditorSettings.PDFPageSettings.MarginLeft;
    lHtmlToPdf.PDFMarginTop := FEditorSettings.PDFPageSettings.MarginTop;
    lHtmlToPdf.PDFMarginRight := FEditorSettings.PDFPageSettings.MarginRight;
    lHtmlToPdf.PDFMarginBottom := FEditorSettings.PDFPageSettings.MarginBottom;
    lHtmlToPdf.PDFScaleToFit := True;
    lHtmlToPdf.PrintOrientation := FEditorSettings.PDFPageSettings.PrintOrientation;
    lHtmlToPdf.DefaultPaperSize := TPDFPaperSize(FEditorSettings.PDFPageSettings.PaperSize);

    //Change the background color of HTML Viewer to create a PDF file with white background
    //when a dark theme is active
    LOldColor := CurrentEditFile.HTMLViewer.DefBackground;
    try
      SendMessage(CurrentEditFile.HTMLViewer.Handle, WM_SETREDRAW, WPARAM(False), 0);
      CurrentEditFile.HTMLViewer.DefBackground := clWhite;
      lHtmlToPdf.SrcViewer := CurrentEditFile.HTMLViewer;

      lHtmlToPdf.PrintPageNumber := False;
      lHtmlToPdf.TextPageNumber := 'Page %d/%d';
      lHtmlToPdf.PageNumberPositionPrint := ppBottom;

      lHtmlToPdf.Execute;
      lHtmlToPdf.SaveToFile(APDFFileName);
    finally
      CurrentEditFile.HTMLViewer.DefBackground := LOldColor;
    end;
  finally
    SendMessage(CurrentEditFile.HTMLViewer.Handle, WM_SETREDRAW, WPARAM(True), 0);
    lHtmlToPdf.Free;
  end;
end;

procedure TfrmMain.HtmlViewerHotSpotClick(Sender: TObject;
  const ASource: ThtString; var Handled: Boolean);
var
  LFileName: TFileName;
  LWorkingFolder: string;
begin
  LFileName := ASource;
  if not FileExists(LFileName) then
  begin
    //Search file in local folder same as current edit file
    LWorkingFolder := IncludeTrailingPathDelimiter(
      CurrentEditFile.HTMLViewer.ServerRoot);
    LFileName := LWorkingFolder+LFileName;
  end;
  //Search file with markdown extensions
  if not FileExists(LFileName) then
    FileWithExtExists(LFileName, AMarkDownFileExt);

  if FileExists(LFileName) then
  begin
    OpenFile(LFileName, True);
    UpdateMDViewer(True);
    Handled := True;
  end
  else
  begin
    dmResources.HtmlViewerHotSpotClick(Sender,
      ASource, Handled);
  end;
end;

procedure TfrmMain.ShowTabCloseButtonOnHotTab;
var
  iot : integer;
  cp : TPoint;
  rectOver: TRect;
begin
  cp := PageControl.ScreenToClient(Mouse.CursorPos);
  iot := PageControl.IndexOfTabAt(cp.X, cp.Y);

  if iot > -1 then
  begin
    rectOver := PageControl.TabRect(iot);

    PanelCloseButton.Left := rectOver.Right - PanelCloseButton.Width;
    PanelCloseButton.Top := rectOver.Top + ((rectOver.Height div 2) - (PanelCloseButton.Height div 2))
      + StyledToolbar.Height + StyledToolbar.Margins.Top + StyledToolbar.Margins.Bottom + 1;

    PanelCloseButton.Tag := iot;
    PanelCloseButton.Show;
  end
  else
  begin
    PanelCloseButton.Tag := -1;
    PanelCloseButton.Hide;
  end;
end;

procedure TfrmMain.PageControlMouseEnter(Sender: TObject);
begin
  ShowTabCloseButtonOnHotTab;
end;

procedure TfrmMain.PageControlMouseLeave(Sender: TObject);
begin
  if PanelCloseButton <> FindVCLWindow(Mouse.CursorPos) then
  begin
    PanelCloseButton.Hide;
    PanelCloseButton.Tag := -1;
  end;
end;

procedure TfrmMain.ManageExceptions(Sender: TObject; E: Exception);
begin
  //This is an event-handler for exceptions that replace Delphi standard handler
  if E is EAccessViolation then
  begin
    if StyledMessageDlg(STR_UNEXPECTED_ERROR,
      Format('Unexpected Error: %s%s',[sLineBreak,E.Message]),
      TMsgDlgType.mtError,
      [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbAbort], 0) = mrAbort then
    Application.Terminate;
  end
  else
  begin

    StyledMessageDlg(STR_ERROR,
      Format('Error: %s%s',[sLineBreak,E.Message]),
      TMsgDlgType.mtError,
      [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbHelp], 0);
  end;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

end.
