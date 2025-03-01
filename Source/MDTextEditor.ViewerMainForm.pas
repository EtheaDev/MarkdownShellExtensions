{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions                                              }
{       (Preview Panel, Thumbnail Icon, MD Text Editor)                        }
{                                                                              }
{       Copyright (c) 2021-2025 (Ethea S.r.l.)                                 }
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
  , dlgInputUrl
  , HTMLUn2
  , HtmlGlobals
  , HtmlView
  , uDragDropUtils
  , Vcl.StyledButton
  , Vcl.StyledToolbar
  , Vcl.ButtonStylesAttributes
  , Vcl.StyledButtonGroup
  , Vcl.StyledCategoryButtons
  , Vcl.FormTabsBar
  , ChildForm
  ;

const
  SET_FILE_NAME = 'HiglightSettings';
  SV_COLLAPSED_WIDTH = 42;
  SV_COLLAPSED_WIDTH_WITH_SCROLLBARS = 60;


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
  FILE_CHANGED_RELOAD = 'File "%s" Date/Time changed! Do you want to reload it?';
  CREATE_NEW_FILE = 'Create new file';
  CONFIRM_CREATE_NEW_FILE = 'The file "%s" doesn''t exists! Do you want to create it now?';

type
  TEditingFile = class
  private
    //FIcon : TIcon;
    FFileName : string;
    FFileAge: TDateTime;
    FExtension: string;
    FShowXMLText: Boolean;
    FMarkDownFile: TMarkDownFile;
    FHTMLViewer: THTMLViewer;
    FEditorSettings: TEditorSettings;
    FChildForm: TMDIChildForm;
    FViewerUpdated: Boolean;
    procedure ReadFromFile;
    procedure SaveToFile;
    function GetFileName: string;
    function GetName: string;
    procedure SetFileName(const Value: string);
    procedure LoadFromFile(const AFileName: string);
    function GetImageName: string;
    procedure SetHTMLViewer(const Value: THTMLViewer);
    procedure UpdateRootPath;
    function GetServerRoot: string;
    procedure SetChildForm(const Value: TMDIChildForm);
    procedure UpdateFileNameInChildForm;
  public
    SynEditor: TSynEdit;
    Splitter: TSplitter;
    property ChildForm: TMDIChildForm read FChildForm write SetChildForm;
    constructor Create(const EditFileName : string;
      const AEditorSettings: TEditorSettings);
    destructor Destroy; override;
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
    catMenuItems: TStyledCategoryButtons;
    panlTop: TPanel;
    lblTitle: TLabel;
    SettingsToolBar: TStyledToolbar;
    ColorSettingsToolButton: TStyledToolButton;
    EditOptionsToolButton: TStyledToolButton;
    VirtualImageList: TVirtualImageList;
    actMenu: TAction;
    MenuButtonToolbar: TStyledToolbar;
    MenuToolButton: TStyledToolButton;
    PageSetupToolButton: TStyledToolButton;
    PrinterSetupToolButton: TStyledToolButton;
    AboutToolButton: TStyledToolButton;
    QuitToolButton: TStyledToolButton;
    SepToolButton: TStyledToolButton;
    OpenRecentAction: TAction;
    RecentPopupMenu: TPopupMenu;
    SaveMenuItem: TMenuItem;
    CloseMenuItem: TMenuItem;
    Sep1MenuItem: TMenuItem;
    SelectAllMenuItem: TMenuItem;
    N1: TMenuItem;
    CloseAllMenuItem: TMenuItem;
    ClientPanel: TPanel;
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
    VirtualImageList20: TVirtualImageList;
    LoadTimer: TTimer;
    VirtualImageListToolBar: TVirtualImageList;
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
    acRefresh: TAction;
    RefreshMenuItem: TMenuItem;
    CheckFileChangedTimer: TTimer;
    StyledToolbar: TStyledToolbar;
    btHeader1: TStyledToolButton;
    btHeader2: TStyledToolButton;
    btHeader3: TStyledToolButton;
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
    btLayoutBoth: TStyledToolButton;
    acLayoutBoth: TAction;
    acLayoutMarkDown: TAction;
    acLayoutViewer: TAction;
    btLayoutMarkDown: TStyledToolButton;
    btLayoutViewer: TStyledToolButton;
    SepLayoutButtons: TStyledToolButton;
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
    procedure ChildFormActivate(Sender: TObject);
    procedure ChildFormClose(Sender: TObject; var Action: TCloseAction);
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
    procedure CheckFileChangedTimerTimer(Sender: TObject);
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
    procedure AppDeactivate(Sender: TObject);
    procedure AppActivate(Sender: TObject);
    procedure SVResize(Sender: TObject);
    procedure acLayoutUpdate(Sender: TObject);
    procedure acLayoutExecute(Sender: TObject);
  private
    FormTabsBar: TSpecialFormTabsBar;
    FEditingInProgress: Boolean;
    FirstAction: Boolean;
    MinFormWidth, MinFormHeight, MaxFormWidth, MaxFormHeight: Integer;
    FProcessingFiles: Boolean;
    FEditorSettings: TEditorSettings;
    CurrentDir: string;
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
    FDropTarget: TDropTarget;
    CurrentChildForm: TMDIChildForm;
    // implement IDragDrop
    function DropAllowed(const FileNames: array of string): Boolean;
    procedure Drop(const FileNames: array of string);
    function GetMarkdownHelpFileName: TFileName;
    procedure CloseSplitViewMenu;
    procedure UpdateHighlighters;
    procedure UpdateApplicationLayout(const ALayoutMode: TLayoutMode);
    procedure UpdateFromSettings(AEditor: TSynEdit);
    function DialogPosRect: TRect;
    procedure AdjustCompactWidth;
    procedure AdjustViewerWidth;
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
    procedure UpdateChildFormImage(AChildForm: TMDIChildForm; AModified: Boolean;
      const AImageName: string);
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
  , Winapi.ShLwApi
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
  LStream := TStringStream.Create(FMarkDownFile.HTML, TEncoding.UTF8);
  try
    //HtmlViewer.LoadFromStream(LStream); //si "perde" i caratteri speciali unicode codepoint
    HtmlViewer.LoadFromString(LStream.DataString);
    HtmlViewer.VScrollBarPosition := LOldPos;
    HtmlViewer.Refresh;
    dmResources.StopLoadingImages(False);
  finally
    LStream.Free;
  end;
  FViewerUpdated := True;
end;

procedure TEditingFile.ReadFromFile;
begin
  LoadFromFile(FileName);
end;

constructor TEditingFile.Create(const EditFileName: string;
  const AEditorSettings: TEditorSettings);
begin
  inherited Create;
  FEditorSettings := AEditorSettings;
  FShowXMLText := False;

  if not IsStyleHookRegistered(TCustomSynEdit, TScrollingStyleHook) then
    TStyleManager.Engine.RegisterStyleHook(TCustomSynEdit, TScrollingStyleHook);

  FileName := EditFileName;
  Fextension := ExtractFileExt(FileName);
end;

function TEditingFile.GetFileName: string;
begin
  Result := FFileName;
end;

function TEditingFile.GetImageName: string;
begin
  if FEditorSettings.UseDarkStyle then
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

procedure TEditingFile.UpdateFileNameInChildForm;
begin
  if Assigned(FChildForm) then
  begin
    FChildForm.Caption := ExtractFileName(Self.FFileName);
    FChildForm.Hint := Self.FFileName;
  end;
end;

procedure TEditingFile.SetChildForm(const Value: TMDIChildForm);
begin
  FChildForm := Value;
  UpdateFileNameInChildForm;
end;

procedure TEditingFile.SetFileName(const Value: string);
var
  Ext : string;
begin
  FFileName := Value;
  Ext := ExtractFileExt(FFileName);
  UpdateRootPath;
  UpdateFileNameInChildForm;
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
  //FreeAndNil(FIcon);
  HTMLViewer := nil;
  SynEditor := nil;
  inherited;
end;

procedure TEditingFile.LoadFromFile(const AFileName: string);
begin
  try
    //Try loading UTF-8 file
    SynEditor.Lines.LoadFromFile(AFileName, TEncoding.UTF8);
  except
    on E: EEncodingError do
    begin
      //Try to load ANSI file
      SynEditor.Lines.LoadFromFile(AFileName, TEncoding.ANSI);
    end
    else
      raise;
  end;
  SynEditor.Modified := False;
  FileAge(AFileName, FFileAge);
end;

procedure TEditingFile.SaveToFile;
begin
  SynEditor.Lines.SaveToFile(Self.FileName);
  SynEditor.Modified := False;
  FileAge(Self.FileName, FFileAge);
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
          EditingFile.ChildForm.Show;
          break;
        end;
      end;
      //searching EditingFile object
      Try
        if not Assigned(EditingFile) then
        begin
          EditingFile := TEditingFile.Create(FileName, FEditorSettings);
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

procedure TfrmMain.AppActivate(Sender: TObject);
begin
  CheckFileChangedTimerTimer(CheckFileChangedTimer);
end;

procedure TfrmMain.AppDeactivate(Sender: TObject);
begin
  CheckFileChangedTimer.Enabled := False;
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
  if not (csDestroying in ComponentState) then
  begin
    AdjustCompactWidth;
    AdjustViewerWidth;
  end;
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
  LChildForm: TMDIChildForm;
  LEditingFile: TEditingFile;
begin
  LChildForm := TSplitter(Sender).Owner as TMDIChildForm;
  LEditingFile := TEditingFile(LChildForm.EditingFile);
  LEditingFile.ShowMarkDownAsHTML(FEditorSettings, True);
  var LContainerWidth := ClientWidth - catMenuItems.Width;
  FEditorSettings.ViewerPercentSize := Round(
    CurrentEditFile.HTMLViewer.Width * 100 / LContainerWidth);
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

procedure TfrmMain.SVResize(Sender: TObject);
begin
  StyledToolbar.Margins.Left :=
    Round(SV.Width - (SV_COLLAPSED_WIDTH * ScaleFactor) + (3 * ScaleFactor));
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
  begin
    EditFileList.Items[LIndex];
    UpdateMDViewer(True);
  end;
end;

procedure TfrmMain.LoadTimerTimer(Sender: TObject);
begin
  if FEditingInProgress then
  begin
    LoadTimer.Enabled := False;
    if FEditorSettings.AutoRefreshWhenEditing then
    begin
      FEditingInProgress := False;
      dmResources.StopLoadingImages(False);
      UpdateMDViewer(True);
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  FileVersionStr: string;
  LMarkDownMasks: string;
begin
  FormTabsBar := TSpecialFormTabsBar.CreateForMainForm(Self, ClientPanel);

  //To check Application Focus
  Application.OnActivate := AppActivate;
  Application.OnDeactivate := AppDeactivate;

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

  //Initialize AnimatedTaskDialog font size and use of CommandLinks
  Screen.MessageFont.Size := Round(Screen.MessageFont.Size*1.2);
  InitializeStyledTaskDialogs(True, Screen.MessageFont, True);

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
  NewExt, LNewFileName: string;
  EditingFile: TEditingFile;
  LCount: Integer;
begin
  NewExt := 'md';

  //Create object to manage new file
  LCount := 0;
  LNewFileName := Format('%s%s.%s', [CurrentDir,'New',NewExt]);
  while FileExists(LNewFileName) do
  begin
    Inc(LCount);
    LNewFileName := Format('%s%s(%d).%s', [CurrentDir,'New',LCount,NewExt]);
  end;

  EditingFile := TEditingFile.Create(LNewFileName, FEditorSettings);
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
  LAction.Enabled := (CurrentEditFile <> nil) and (CurrentEditFile.SynEditor <> nil) and
    (CurrentEditFile.SynEditor.Focused);
end;

procedure TfrmMain.acReplaceUpdate(Sender: TObject);
begin
  acReplace.Enabled := (CurrentEditor <> nil) and (CurrentEditor.Text <> '')
    and not CurrentEditor.ReadOnly;
end;

procedure TfrmMain.acCloseExecute(Sender: TObject);
begin
  //Remove editing file
  CurrentChildForm.Close;
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

procedure TfrmMain.acLayoutExecute(Sender: TObject);
begin
  if Sender = acLayoutBoth then
    UpdateApplicationLayout(lmBoth)
  else if Sender = acLayoutMarkDown then
    UpdateApplicationLayout(lmMarkDown)
  else if Sender = acLayoutViewer then
    UpdateApplicationLayout(lmHTML);
end;

procedure TfrmMain.acLayoutUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := CurrentEditFile <> nil;
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

procedure TfrmMain.UpdateApplicationLayout(const ALayoutMode: TLayoutMode);
begin
  FEditorSettings.LayoutMode := ALayoutMode;
  if CurrentEditFile <> nil then
  begin
    case ALayoutMode of
      lmBoth:
      begin
        CurrentEditFile.HTMLViewer.Align := alRight;
        AdjustViewerWidth;
        CurrentEditFile.HTMLViewer.Visible := True;
        CurrentEditFile.Splitter.Visible := True;
        CurrentEditFile.Splitter.Left := CurrentEditFile.HTMLViewer.left -1;
        CurrentEditFile.SynEditor.Visible := True;
        btLayoutBoth.Down := True;
      end;
      lmMarkDown:
      begin
        CurrentEditFile.HTMLViewer.Visible := False;
        CurrentEditFile.Splitter.Visible := False;
        CurrentEditFile.SynEditor.Visible := True;
        btLayoutMarkDown.Down := True;
      end;
      lmHTML:
      begin
        CurrentEditFile.SynEditor.Visible := False;
        CurrentEditFile.Splitter.Visible := False;
        CurrentEditFile.HTMLViewer.Align := alClient;
        CurrentEditFile.HTMLViewer.Visible := True;
        CurrentEditFile.Splitter.Visible := False;
        btLayoutViewer.Down := True;
      end;
    end;
    if CurrentEditFile.HTMLViewer.Visible and not
       CurrentEditFile.FViewerUpdated then
      UpdateMDViewer(True);
  end;
end;

procedure TfrmMain.UpdateChildFormImage(AChildForm: TMDIChildForm;
  AModified: Boolean; const AImageName: string);
var
  LImageName: string;
begin
  if AModified then
    LImageName := AImageName
  else
    LImageName := AImageName+'-gray';
  AChildForm.SetIconByName(VirtualImageList20, LImageName);
  FormTabsBar.Invalidate;
end;

procedure TfrmMain.SynEditChange(Sender: TObject);
begin
  if Sender = CurrentEditor then
  begin
    FEditingInProgress := True;
    LoadTimer.Enabled := False;
    dmResources.StopLoadingImages(True);
    //UpdateMDViewer(False);
    if not FProcessingFiles then
      LoadTimer.Enabled := True;
  end;
end;

procedure TfrmMain.SynEditEnter(Sender: TObject);
begin
  CloseSplitViewMenu;
end;

function TfrmMain.AddEditingFile(const EditingFile: TEditingFile): Integer;
var
  LChildForm: TMDIChildForm;
  LEditor: TSynEdit;
  LFEViewer: THtmlViewer;
  LSplitter: TSplitter;
begin
  //Add file to opened-list
  Result := EditFileList.Add(EditingFile);
  //Create the Child Form associated to the file
  LChildForm := nil;
  LEditor := nil;
  LFEViewer := nil;
  Try
    LChildForm := TMDIChildForm.Create(Application);
    LChildForm.OnClose := ChildFormClose;
    LChildForm.EditingFile := EditingFile;
    LChildForm.OnActivate := ChildFormActivate;
    UpdateChildFormImage(LChildForm, False, EditingFile.ImageName);
    EditingFile.ChildForm := LChildForm;

    LFEViewer := THtmlViewer.Create(LChildForm);
    LFEViewer.ScrollBars := ssNone;
    LFEViewer.Width := LChildForm.Width div 2;
    LFEViewer.Parent := LChildForm;
    LFEViewer.PopupMenu := PopHTMLViewer;
    LFEViewer.DefBackground := StyleServices.GetSystemColor(clWindow);
    LFEViewer.OnHotSpotClick := HtmlViewerHotSpotClick;
    LFEViewer.OnImageRequest := dmResources.HtmlViewerImageRequest;
    LFEViewer.ScrollBars := TScrollStyle.ssVertical;

    LSplitter := TSplitter.Create(LChildForm);
    LSplitter.Align := alRight;
    LSplitter.Left := LFEViewer.Left-1;
    LSplitter.AutoSnap := False;
    LSplitter.Width := 6;
    LSplitter.Parent := LChildForm;
    LSplitter.Beveled := True;
    LSplitter.OnMoved := SplitterMoved;

    EditingFile.Splitter := LSplitter;
    EditingFile.HTMLViewer := LFEViewer;

    //Create the SynEdit object editor into the ChildForm that is the owner
    LEditor := TSynEdit.Create(LChildForm);
    LEditor.OnChange := SynEditChange;
    LEditor.OnEnter := SynEditEnter;
    LEditor.MaxUndo := 5000;
    LEditor.Align := alClient;
    LEditor.Parent := LChildForm;
    LEditor.SearchEngine := SynEditSearch;
    LEditor.PopupMenu := popEditor;

    //Assign user preferences to the editor
    FEditorOptions.AssignTo(LEditor);
    EditingFile.SynEditor := LEditor;
    UpdateFromSettings(LEditor);
    UpdateHighlighter(LEditor);

    //Make the ChildForm the current page
    //and call "change" method passing EditingChild
    ChildFormActivate(LChildForm);

    //Show Editor or Viewer or both
    UpdateApplicationLayout(FEditorSettings.LayoutMode);
  Except
    LChildForm.Free;
    LEditor.Free;
    LFEViewer.Free;
    raise;
  End;
end;

procedure TfrmMain.UpdateMDViewer(const AReloadImages: Boolean);
begin
  if FProcessingFiles then
    Exit;
  if (CurrentEditor <> nil) and (CurrentEditFile <> nil) then
  begin
    LoadTimer.Enabled := False;
    if (CurrentEditFile.HTMLViewer.Visible) then
    begin
      Screen.Cursor := crHourGlass;
      try
        UpdateChildFormImage(CurrentChildForm, CurrentEditor.Modified,
          CurrentEditFile.ImageName);

        StatusBar.Panels[STATUSBAR_MESSAGE].Text := CurrentEditFile.FileName;

        //Show HTML content of MarkDown
        CurrentEditFile.ShowMarkDownAsHTML(FEditorSettings, AReloadImages);
      finally
        Screen.Cursor := crDefault;
      end;
    end
    else
    begin
      CurrentEditFile.FViewerUpdated := False;
    end;
  end;
end;

procedure TfrmMain.ChildFormClose(Sender: TObject; var Action: TCloseAction);
begin
  //Remove editing file
  var LEditingFile := (Sender as TMDIChildForm).EditingFile as TEditingFile;
  RemoveEditingFile(LEditingFile);
  //Reset CurrentChildForm pointer
  CurrentChildForm := nil;
  Action := caFree;
end;

procedure TfrmMain.ChildFormActivate(Sender: TObject);
begin
  CurrentChildForm := Sender as TMDIChildForm;
  CloseSplitViewMenu;
  //Setting the Editor caption as the actual file opened
  if (CurrentEditFile <> nil) and (CurrentEditFile.SynEditor <> nil) then
  begin
    if (CurrentEditFile.SynEditor.CanFocus) then
      CurrentEditFile.SynEditor.SetFocus;
  end;
  UpdateMDViewer(False);
end;

procedure TfrmMain.acSaveUpdate(Sender: TObject);
begin
  acSave.Enabled := (CurrentEditor <> nil) and (CurrentEditor.Modified);
end;

procedure TfrmMain.CheckFileChangedTimerTimer(Sender: TObject);
var
  LFileAge: TDateTime;
begin
  CheckFileChangedTimer.Enabled := False;
  Try
    //Check if opened files are changed on Disk
    for var I := 0 to EditFileList.Count -1 do
    begin
      var LEditFile := TEditingFile(EditFileList.items[I]);
      if FileAge(LEditFile.FileName, LFileAge) then
      begin
        if LFileAge <> LEditFile.FFileAge then
        begin
          var LConfirm := StyledMessageDlg(Format(FILE_CHANGED_RELOAD,[LEditFile.FileName]),
            mtWarning, [mbYes, mbNo], 0);
          if LConfirm = mrYes then
          begin
            LEditFile.ReadFromFile;
            UpdateChildFormImage(LEditFile.ChildForm, False, LEditFile.ImageName);
            SynEditChange(LEditFile.SynEditor);
          end
          else
            LEditFile.FFileAge := LFileAge;
        end;
      end;
    end;
  Finally
    CheckFileChangedTimer.Enabled := True;
  End;
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
  if (CurrentChildForm <> nil) then
    Result := TEditingFile(CurrentChildForm.EditingFile)
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
    LConfirm := StyledMessageDlg(Format(CONFIRM_CHANGES,[EditingFile.FileName]),
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

  //Delete the file from the Opened-List
  CurrentChildForm := nil;
  EditFileList.Delete(pos);
end;

procedure TfrmMain.acCloseAllUpdate(Sender: TObject);
begin
  acCloseAll.Enabled := EditFileList.Count > 0;
end;

procedure TfrmMain.acCloseAllExecute(Sender: TObject);
begin
  FProcessingFiles := True;
  try
    while EditFileList.Count > 0 do
      TEditingFile(EditFileList.items[0]).ChildForm.Close;
  finally
    FProcessingFiles := False;
    UpdateStatusBarPanels;
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
begin
  if (CurrentEditor <> nil) and (Value >= MinfontSize) and (Value <= MaxfontSize) then
  begin
    CurrentEditor.Font.Height := -Round(Value * Self.ScaleFactor);
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
var
  LStyle: TStyledButtonDrawType;
begin
  UpdateApplicationStyle(FEditorSettings.StyleName);
  if AEditor <> nil then
  begin
    FEditorSettings.ReadSettings(AEditor.Highlighter, self.FEditorOptions);
    AEditor.ReadOnly := False;
  end
  else
    FEditorSettings.ReadSettings(nil, self.FEditorOptions);

  //Rounded Buttons for StyledButtons
  if FEditorSettings.ButtonDrawRounded then
    LStyle := btRounded
  else
    LStyle := btRoundRect;
  TStyledButton.RegisterDefaultRenderingStyle(LStyle);

  //Rounded Buttons for StyledToolbars
  if FEditorSettings.ToolbarDrawRounded then
    LStyle := btRounded
  else
    LStyle := btRoundRect;
  TStyledToolbar.RegisterDefaultRenderingStyle(LStyle);
  StyledToolbar.StyleDrawType := LStyle;
  SettingsToolBar.StyleDrawType := LStyle;

  //Rounded Buttons for menus: StyledCategories and StyledButtonGroup
  if FEditorSettings.MenuDrawRounded then
    LStyle := btRounded
  else
    LStyle := btRoundRect;
  TStyledCategoryButtons.RegisterDefaultRenderingStyle(LStyle);
  TStyledButtonGroup.RegisterDefaultRenderingStyle(LStyle);
  catMenuItems.StyleDrawType := LStyle;
  MenuButtonToolbar.StyleDrawType := LStyle;

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
  UpdateApplicationLayout(FEditorSettings.LayoutMode);
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

    //call the "onactivate" event of ChildForm
    ChildFormActivate(CurrentEditFile.ChildForm);
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
    if FileExists(LFileName) then
    begin
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
    StatusBar.Panels[STATUSBAR_MESSAGE].Text := '';
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
  InitialDir: string;
  LFileName: TFileName;
begin
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
  //LoadTimer.Enabled := True;
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

procedure TfrmMain.AdjustViewerWidth;
begin
  if (CurrentEditFile <> nil) and (CurrentEditFile.HTMLViewer <> nil) then
  begin
    var LContainerWidth := ClientWidth - catMenuItems.Width;
    CurrentEditFile.HTMLViewer.Width :=
      Round(LContainerWidth * FEditorSettings.ViewerPercentSize / 100);
  end;
end;

procedure TfrmMain.AdjustCompactWidth;
begin
  //Change size of compact view because Scrollbars appears
  if (Height / ScaleFactor) > 900 then
    SV.CompactWidth := Round(SV_COLLAPSED_WIDTH * ScaleFactor)
  else
    SV.CompactWidth := Round(SV_COLLAPSED_WIDTH_WITH_SCROLLBARS * ScaleFactor);
  if (CurrentEditFile <> nil) and (CurrentEditFile.HTMLViewer <> nil) and (CurrentEditFile.HTMLViewer.Width > CurrentEditFile.ChildForm.Width) then
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
  else if not PathIsURL(PChar(ASource)) then
  begin
    //Try to create Local File
    if StyledTaskMessageDlg(CREATE_NEW_FILE,
      Format(CONFIRM_CREATE_NEW_FILE, [LFileName]),
      TMsgDlgType.mtConfirmation,
      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel],
      0, TMsgDlgBtn.mbYes) = mrYes then
    begin
      var LStreamWriter := TFile.CreateText(LFileName);
      LStreamWriter.Write(Format('# %s', [ExtractFileName(LFileName)]));
      LStreamWriter.Flush;
      LStreamWriter.Close;
      OpenFile(LFileName, True);
      UpdateMDViewer(True);
      Handled := True;
    end;
  end;
  if not Handled then
  begin
    //Try to Open an URL
    dmResources.HtmlViewerHotSpotClick(Sender,
      ASource, Handled);
  end;
end;

procedure TfrmMain.ManageExceptions(Sender: TObject; E: Exception);
begin
  //This is an event-handler for exceptions that replace Delphi standard handler
  if E is EAccessViolation then
  begin
    if StyledTaskMessageDlg(STR_UNEXPECTED_ERROR,
      Format('Unexpected Error: %s%s',[sLineBreak,E.Message]),
      TMsgDlgType.mtError,
      [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbAbort], 0) = mrAbort then
    Application.Terminate;
  end
  else
  begin

    StyledTaskMessageDlg(STR_ERROR,
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
