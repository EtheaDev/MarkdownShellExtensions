{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions                                              }
{       (Preview Panel, Thumbnail Icon, MD Text Editor)                        }
{                                                                              }
{       Copyright (c) 2021-2023 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
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
{                                                                              }
{  The Initial Developer of the Original Code is Rodrigo Ruz V.                }
{  Portions created by Rodrigo Ruz V. are Copyright 2011-2021 Rodrigo Ruz V.   }
{  All Rights Reserved.                                                        }
{******************************************************************************}
unit MDShellEx.SettingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, ColorGrd, StdCtrls, CheckLst, SynEdit,
  ActnList, SynEditHighlighter, SynUnicode, System.ImageList, Vcl.ImgList,
  SVGIconImageListBase, SVGIconImageList, MDShellEx.Settings, Vcl.ButtonGroup,
  Vcl.ToolWin, MDShellEx.Resources, Vcl.VirtualImageList, MDShellEx.About, Vcl.WinXCtrls,
  SVGIconImage, Vcl.NumberBox;

type
  TSVGSettingsForm = class(TForm)
    pc: TPageControl;
    tsColors: TTabSheet;
    stGeneral: TTabSheet;
    tsFont: TTabSheet;
    stTheme: TTabSheet;
    paLeft: TPanel;
    paElements: TPanel;
    BoxElements: TListBox;
    paElemTitle: TPanel;
    VertSplitter: TSplitter;
    paAttributesContainer: TPanel;
    paAttributes: TPanel;
    StatusBar: TStatusBar;
    SynEdit: TSynEdit;
    cbTextAttrib: TGroupBox;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    cbUnderline: TCheckBox;
    cbStrikeOut: TCheckBox;
    gbWhiteSpace: TGroupBox;
    cbForeground: TCheckBox;
    cbBackground: TCheckBox;
    OpenDialog: TOpenDialog;
    SettingsImageList: TSVGIconImageList;
    ElementColorGroupBox: TGroupBox;
    ForegroundColorBox: TColorBox;
    ForegroundColorLabel: TLabel;
    BackgroundColorLabel: TLabel;
    BackgroundColorBox: TColorBox;
    MenuButtonGroup: TButtonGroup;
    TitlePanel: TPanel;
    ThemeLeftPanel: TPanel;
    ThemesRadioGroup: TRadioGroup;
    SelectThemeRadioGroup: TRadioGroup;
    ThemeClientPanel: TPanel;
    ResetPanel: TPanel;
    ResetButton: TButton;
    MDGroupBox: TGroupBox;
    FontLabel: TLabel;
    MDFontComboBox: TComboBox;
    SizeLabel: TLabel;
    MDFontSizeEdit: TEdit;
    MDUpDown: TUpDown;
    HTMLGroupBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    HTMLFontComboBox: TComboBox;
    HTMLFontSizeEdit: TEdit;
    HTMLUpDown: TUpDown;
    ShowMDCheckBox: TCheckBox;
    RenderingGroupBox: TGroupBox;
    DownloadFromWebCheckBox: TCheckBox;
    RescalingImageCheckBox: TCheckBox;
    tsPDFLayout: TTabSheet;
    OrientationImageList: TSVGIconImageList;
    OrientationRadioGroup: TRadioGroup;
    SVGIconPosition: TSVGIconImage;
    MarginLeftEdit: TNumberBox;
    MarginRightEdit: TNumberBox;
    MarginTopEdit: TNumberBox;
    MarginBottomEdit: TNumberBox;
    PaperSizeRadioGroup: TRadioGroup;
    MarginTopLabel: TLabel;
    MarginLeftLabel: TLabel;
    MarginRightLabel: TLabel;
    MarginBottomLabel: TLabel;
    MarkdownGroupBox: TGroupBox;
    ProcessorDialectComboBox: TComboBox;
    ProcessorDialectLabel: TLabel;
    procedure BoxElementsClick(Sender: TObject);
    procedure cbForegroundClick(Sender: TObject);
    procedure cbBackgroundClick(Sender: TObject);
    procedure cbFontStyleClick(Sender: TObject);
    procedure GetActiveAttribute;
    procedure SynEditClick(Sender: TObject);
    procedure SynEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ColorGridClick(Sender: TObject);
    procedure ExitFromSettings(Sender: TObject);
    procedure ColorBoxSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuButtonGroupButtonClicked(Sender: TObject; Index: Integer);
    procedure SelectThemeRadioGroupClick(Sender: TObject);
    procedure ThemesRadioGroupClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure FontDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure OrientationRadioGroupClick(Sender: TObject);
  private
    FHighlighter: TSynCustomHighlighter;
    FSourceSynEdit: TSynEdit;
    FFileName: string;
    FAboutForm: TFrmAbout;
    FTitle: string;
    procedure PopulateAvailThemes;
    procedure AssignSettings(ASettings: TSettings);
    procedure UpdateSettings(ASettings: TSettings);
    function GetCurrentElement: TSynHighlighterAttributes;
    procedure RefreshColorBoxes;
    procedure RefreshDefaultCheckBox;
    procedure RefreshTextAttributes;
    procedure ColorBoxChanged;
    function GetBackGroundColor: TColor;
    function GetForeGroundColor: TColor;
    procedure AddElements;
    procedure RefreshMap;
    procedure CloneSynEdit(Source, Dest : TSynEdit );
    procedure ChangeAllDefaultColors(const OldForeground, NewForeground,
      OldBackGround, NewBackGround: TColor);
    function GetCurrentIsWhiteSpace: Boolean;
    procedure SetTitle(const Value: string);
    procedure ChangePage(AIndex: Integer);
    procedure CreateAboutForm;
    function SelectedStyleName: string;
    function SelectedStyleIsDark: Boolean;
    property CurrentElement: TSynHighlighterAttributes read GetCurrentElement;
    property CurrentIsWhiteSpace: Boolean read GetCurrentIsWhiteSpace;
    property ForeGroundColor: TColor read GetForeGroundColor;
    property BackGroundColor: TColor read GetBackGroundColor;
    property Title: string read FTitle write SetTitle;
  public
  end;

function ShowSettings(const AParentRect: TRect;
  const ATitle: string;
  const ASourceSynEdit: TSynEdit;
  const ASettings: TSettings;
  AFromPreview: Boolean): Boolean;

implementation

uses
{$IFNDEF DISABLE_STYLES}
  Vcl.Themes,
{$ENDIF}
  MarkdownProcessor,
  uRegistry;

{$R *.dfm}

function ShowSettings(const AParentRect: TRect;
  const ATitle: string;
  const ASourceSynEdit: TSynEdit;
  const ASettings: TSettings; AFromPreview: Boolean): Boolean;
type
  TSynCustomHighlighterClass = class of TSynCustomHighlighter;
var
  HighLightSettingsClass: TSynCustomHighlighterClass;
  LSettingsForm: TSVGSettingsForm;
  I: integer;
begin
  Result := False;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I].ClassType = TSVGSettingsForm then
    begin
      Screen.Forms[I].BringToFront;
      exit;
    end;

  LSettingsForm := TSVGSettingsForm.Create(nil);
  with LSettingsForm do
  Try
    Title := ATitle;
    AssignSettings(ASettings);
    if (AparentRect.Left <> 0) and (AparentRect.Right <> 0) then
    begin
      LSettingsForm.Left := (AParentRect.Left + AParentRect.Right - LSettingsForm.Width) div 2;
      LSettingsForm.Top := (AParentRect.Top + AParentRect.Bottom - LSettingsForm.Height) div 2;
    end;
    StatusBar.SimpleText := FFileName;

    FSourceSynEdit := ASourceSynEdit;
    if Assigned(FSourceSynEdit) then
    begin
      SynEdit.Color := FSourceSynEdit.Color;
      SynEdit.Font.Assign(FSourceSynEdit.Font);
      SynEdit.Gutter.Assign(FSourceSynEdit.Gutter);
      SynEdit.ActiveLineColor := FSourceSynEdit.ActiveLineColor;
      HighLightSettingsClass := TSynCustomHighlighterClass(
        FSourceSynEdit.Highlighter.ClassType);
      FHighlighter := HighLightSettingsClass.Create(nil);
      SynEdit.Highlighter := FHighlighter;
      CloneSynEdit(ASourceSynEdit,SynEdit);
      SynEdit.Text := ASourceSynEdit.Text;
      AddElements;
    end;
    try
      Result := ShowModal = mrOk;
      if Result then
        UpdateSettings(ASettings);
    Finally
      if Assigned(FHighlighter) then
        FHighlighter.Free;
    End;

  Finally
    LSettingsForm.Free;
  End;
end;

{ TSVGSettingsForm }

procedure TSVGSettingsForm.AddElements;
var
  i : integer;
begin
  //Add Elements as Highlighters attributes
  For i := 0 to FHighlighter.AttrCount -1 do
  begin
    BoxElements.AddItem(FHighlighter.Attribute[I].Name, FHighlighter.Attribute[I]);
    //Chage WiteSpace Element position to 0
    if FHighlighter.Attribute[I] = FHighlighter.WhitespaceAttribute then
      BoxElements.Items.Move(BoxElements.Items.Count-1, 0);
  end;
  BoxElements.ItemIndex := 0;
  RefreshMap;
end;

procedure TSVGSettingsForm.BoxElementsClick(Sender: TObject);
begin
  RefreshMap;
end;

procedure TSVGSettingsForm.RefreshColorBoxes;
begin
  if (CurrentElement.ForeGround <> ForeGroundColor) or
    CurrentIsWhiteSpace then
  begin
    ForegroundColorBox.Enabled := True;
    ForegroundColorBox.Selected := CurrentElement.ForeGround;
  end
  else
  begin
    ForegroundColorBox.Enabled := False;
    ForegroundColorBox.Selected := ForeGroundColor;
  end;
  if (CurrentElement.Background <> BackGroundColor) or
    CurrentIsWhiteSpace then
  begin
    BackgroundColorBox.Enabled := True;
    BackgroundColorBox.Selected := CurrentElement.BackGround;
  end
  else
  begin
    BackgroundColorBox.Enabled := False;
    BackgroundColorBox.Selected := BackGroundColor;
  end;
end;

procedure TSVGSettingsForm.RefreshDefaultCheckBox;
begin
  cbForeground.OnClick := nil;
  cbBackground.OnClick := nil;
  Try
    cbForeground.Checked := CurrentElement.ForeGround = ForegroundColor;
    cbBackground.Checked := CurrentElement.Background = BackgroundColor;
  Finally
    cbForeground.OnClick := cbForegroundClick;
    cbBackground.OnClick := cbBackgroundClick;
  End;
end;

procedure TSVGSettingsForm.RefreshTextAttributes;
begin
  with CurrentElement do
  begin
    //Text Attributes
    cbBold.Checked := fsBold in Style;
    cbItalic.Checked := fsItalic in Style;
    cbUnderline.Checked := fsUnderline in Style;
    cbStrikeOut.Checked := fsStrikeOut in Style;
  end;
end;

procedure TSVGSettingsForm.ResetButtonClick(Sender: TObject);
var
  LBackGroundColor: TColor;
begin
{$IFNDEF DISABLE_STYLES}
  LBackGroundColor := TStyleManager.Style[SelectedStyleName].GetSystemColor(clWindow);
{$ELSE}
  LBackGroundColor := clWindow;
{$ENDIF}
  SynEdit.Highlighter.Assign(dmResources.GetSynHighlighter(
    SelectedStyleIsDark, LBackGroundColor));
end;

procedure TSVGSettingsForm.RefreshMap;
begin
  //imposta la mappa sulla base delle impostazioni della lista
  with CurrentElement do
  begin
    RefreshColorBoxes;
    RefreshDefaultCheckBox;
    RefreshTextAttributes;
    gbWhiteSpace.Visible := CurrentElement <> FHighlighter.WhitespaceAttribute;
  end;
end;

function TSVGSettingsForm.GetCurrentElement: TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes(BoxElements.Items.Objects[BoxElements.ItemIndex]);
end;

function TSVGSettingsForm.GetCurrentIsWhiteSpace: Boolean;
begin
  Result := CurrentElement.Name = 'Whitespace';
end;

procedure TSVGSettingsForm.cbForegroundClick(Sender: TObject);
begin
  if cbForeground.Checked then
    CurrentElement.Foreground := ForeGroundColor
  else
    CurrentElement.Foreground := clDefault;
  RefreshColorBoxes;
  RefreshDefaultCheckBox;
end;

procedure TSVGSettingsForm.ChangeAllDefaultColors(const OldForeground,
  NewForeground, OldBackGround, NewBackGround: TColor);
var
  I: Integer;
  LAttribute: TSynHighlighterAttributes;
begin
  for i := 0 to FHighlighter.AttrCount -1 do
  begin
    LAttribute := FHighlighter.Attribute[I];
    if LAttribute.Name <> 'Whitespace' then
    begin
      if LAttribute.Foreground = OldForeground then
        LAttribute.Foreground := NewForeground;
      if LAttribute.Background = OldBackGround then
        LAttribute.Background := NewBackGround;
    end;
  end;
end;

procedure TSVGSettingsForm.cbBackgroundClick(Sender: TObject);
begin
  if cbBackground.Checked then
    CurrentElement.Background := BackGroundColor
  else
    CurrentElement.Background := clDefault;
  RefreshColorBoxes;
  RefreshDefaultCheckBox;
end;

procedure TSVGSettingsForm.FontDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with Control as TComboBox do
  begin
    Canvas.fillrect(rect);
    Canvas.Font.Name  := Items[Index];
    Canvas.textout(rect.Left, rect.Top, Items[Index]);
  end;
end;

procedure TSVGSettingsForm.ColorBoxChanged;
begin
  cbForeground.OnClick := nil;
  cbBackground.OnClick := nil;
  try
    if CurrentIsWhiteSpace then
    begin
      ChangeAllDefaultColors(CurrentElement.Foreground, ForegroundColorBox.Selected,
        CurrentElement.Background, BackGroundColorBox.Selected);
      CurrentElement.Foreground := ForegroundColorBox.Selected;
      CurrentElement.Background := BackgroundColorBox.Selected;
    end
    else
    begin
      if CurrentElement.ForeGround <> ForeGroundColor then
        CurrentElement.Foreground := ForegroundColorBox.Selected
      else
        CurrentElement.Foreground := ForeGroundColor;

      if CurrentElement.Background <> BackGroundColor then
        CurrentElement.Background := BackgroundColorBox.Selected
      else
        CurrentElement.Background := BackGroundColor;
    end;
    RefreshDefaultCheckBox;
  finally
    cbForeground.OnClick := cbForegroundClick;
    cbBackground.OnClick := cbBackgroundClick;
  end;
end;

procedure TSVGSettingsForm.cbFontStyleClick(Sender: TObject);
var
  FontStyle : TFontStyle;
begin
  if Sender = cbBold then
    FontStyle := fsBold
  else if Sender = cbItalic then
    FontStyle := fsItalic
  else if Sender = cbUnderline then
    FontStyle := fsUnderline
  else if Sender = cbStrikeOut then
    FontStyle := fsStrikeOut
  else
    Exit;

  with (Sender as Tcheckbox) do
  begin
    if Checked then
      CurrentElement.Style := CurrentElement.Style + [fontStyle]
    else
      CurrentElement.Style := CurrentElement.Style - [fontStyle];
  end;
  RefreshMap;
end;

procedure TSVGSettingsForm.GetActiveAttribute;
var
  Token : UnicodeString;
  Attr : TSynHighlighterAttributes;
begin
  //Recupera l'attributo attivo a partire dall'editor
  Token := '';
  SynEdit.GetHighlighterAttriAtRowCol(SynEdit.CaretXY,Token,Attr);
  if Attr <> nil then
  begin
    BoxElements.ItemIndex := BoxElements.Items.IndexOf(Attr.Name);
    RefreshMap;
  end
  else
    BoxElements.ItemIndex := 0; //goto WiteSpace Element
end;

procedure TSVGSettingsForm.SelectThemeRadioGroupClick(Sender: TObject);
begin
  ThemeClientPanel.StyleName := SelectedStyleName;
  CreateAboutForm;
end;

procedure TSVGSettingsForm.SetTitle(const Value: string);
begin
  FTitle := Value;
  TitlePanel.Caption := '  '+FTitle+' - '+TitlePanel.Caption;
  Caption := TitlePanel.Caption;
end;

procedure TSVGSettingsForm.SynEditClick(Sender: TObject);
begin
  GetActiveAttribute;
end;

procedure TSVGSettingsForm.SynEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  GetActiveAttribute;
end;

procedure TSVGSettingsForm.ThemesRadioGroupClick(Sender: TObject);
begin
  PopulateAvailThemes;
end;

procedure TSVGSettingsForm.ColorGridClick(Sender: TObject);
begin
  ColorBoxChanged;
end;

procedure TSVGSettingsForm.FormCreate(Sender: TObject);
begin
  MDFontComboBox.Items.Assign(Screen.Fonts);
  HTMLFontComboBox.Items.Assign(Screen.Fonts);
  tsColors.TabVisible := false;
  stGeneral.TabVisible := false;
  tsFont.TabVisible := false;
  stTheme.TabVisible := false;
  tsPDFLayout.TabVisible := false;

  TitlePanel.Font.Height := Round(TitlePanel.Font.Height * 1.5);
  MenuButtonGroup.Font.Height := Round(MenuButtonGroup.Font.Height * 1.2);
end;

procedure TSVGSettingsForm.FormDestroy(Sender: TObject);
begin
  FAboutForm.Free;
end;

procedure TSVGSettingsForm.ColorBoxSelect(Sender: TObject);
begin
  ColorBoxChanged;
end;

function TSVGSettingsForm.GetBackGroundColor: TColor;
begin
  Result := FHighlighter.WhitespaceAttribute.Background;
end;

function TSVGSettingsForm.GetForeGroundColor: TColor;
begin
  Result := FHighlighter.WhitespaceAttribute.Foreground;
end;

procedure TSVGSettingsForm.ChangePage(AIndex: Integer);
begin
  pc.ActivePageIndex := AIndex;
end;

procedure TSVGSettingsForm.AssignSettings(ASettings: TSettings);
begin
  if not (ASettings is TEditorSettings) and (MenuButtonGroup.items.Count > 5) then
    MenuButtonGroup.items.Delete(5);
  ChangePage(ASettings.ActivePageIndex);
  MenuButtonGroup.ItemIndex := pc.ActivePageIndex +1;
  SettingsImageList.FixedColor := ASettings.ButtonTextColor;
  SVGIconPosition.FixedColor := ASettings.ButtonTextColor;
  FFileName := ASettings.SettingsFileName;
  ThemesRadioGroup.ItemIndex := Ord(ASettings.ThemeSelection);

  ShowMDCheckBox.Checked := ASettings.ShowMarkDown;
  MDFontComboBox.ItemIndex := MDFontComboBox.Items.IndexOf(ASettings.MDFontName);
  MDUpDown.Position := ASettings.MDFontSize;

  HTMLFontComboBox.ItemIndex := HTMLFontComboBox.Items.IndexOf(ASettings.HTMLFontName);
  HTMLUpDown.Position := ASettings.HTMLFontSize;

  ProcessorDialectComboBox.ItemIndex := ord(ASettings.ProcessorDialect);

  RescalingImageCheckBox.Checked := ASettings.RescalingImage;
  DownloadFromWebCheckBox.Visible := ASettings is TEditorSettings;
  if DownloadFromWebCheckBox.Visible then
    DownloadFromWebCheckBox.Checked := TEditorSettings(ASettings).DownloadFromWeb
  else
    DownloadFromWebCheckBox.Checked := False;

  OrientationRadioGroup.ItemIndex := Ord(ASettings.PDFPageSettings.PrintOrientation);
  OrientationRadioGroupClick(OrientationRadioGroup);
  PaperSizeRadioGroup.ItemIndex := Ord(ASettings.PDFPageSettings.PaperSize);
  MarginLeftEdit.ValueFloat := ASettings.PDFPageSettings.MarginLeft;
  MarginRightEdit.ValueFloat := ASettings.PDFPageSettings.MarginRight;
  MarginTopEdit.ValueFloat := ASettings.PDFPageSettings.MarginTop;
  MarginBottomEdit.ValueFloat := ASettings.PDFPageSettings.MarginBottom;

  PopulateAvailThemes;
end;

function TSVGSettingsForm.SelectedStyleIsDark: Boolean;
var
  LThemeAttributes: TThemeAttribute;
begin
  TThemeAttribute.GetStyleAttributes(SelectedStyleName, LThemeAttributes);
  if not Assigned(LThemeAttributes) then
    Result := not IsWindowsAppThemeLight
  else
    Result := LThemeAttributes.ThemeType = ttDark;
end;

function TSVGSettingsForm.SelectedStyleName: string;
begin
  if SelectThemeRadioGroup.ItemIndex <> -1 then
    Result := SelectThemeRadioGroup.Items[SelectThemeRadioGroup.ItemIndex]
  else
    Result := DefaultStyleName;
end;

procedure TSVGSettingsForm.UpdateSettings(ASettings: TSettings);
begin
  ASettings.ActivePageIndex := pc.ActivePageIndex;
  ASettings.ThemeSelection := TThemeSelection(ThemesRadioGroup.ItemIndex);

  ASettings.ShowMarkDown := ShowMDCheckBox.Checked;
  ASettings.MDFontName := MDFontComboBox.Text;
  ASettings.MDFontSize := MDUpDown.Position;

  ASettings.HTMLFontName := HTMLFontComboBox.Text;
  ASettings.HTMLFontSize := HTMLUpDown.Position;

  ASettings.ProcessorDialect := TMarkdownProcessorDialect(ProcessorDialectComboBox.ItemIndex);

  ASettings.StyleName := SelectedStyleName;
  ASettings.RescalingImage := RescalingImageCheckBox.Checked;
  if ASettings is TEditorSettings then
    TEditorSettings(ASettings).DownloadFromWEB := DownloadFromWEBCheckBox.Checked;

  ASettings.PDFPageSettings.PrintOrientation := TPrinterOrientation(OrientationRadioGroup.ItemIndex);
  ASettings.PDFPageSettings.PaperSize := PaperSizeRadioGroup.ItemIndex;
  ASettings.PDFPageSettings.MarginLeft := MarginLeftEdit.ValueFloat;
  ASettings.PDFPageSettings.MarginRight := MarginRightEdit.ValueFloat;
  ASettings.PDFPageSettings.MarginTop := MarginTopEdit.ValueFloat;
  ASettings.PDFPageSettings.MarginBottom := MarginBottomEdit.ValueFloat;
end;

procedure TSVGSettingsForm.MenuButtonGroupButtonClicked(Sender: TObject;
  Index: Integer);
begin
  if Sender is TButtonGroup then
  begin
    case Index of
      0: ExitFromSettings(nil);
      1,2,3,4,5: ChangePage(Index -1);
    else
      Beep;
    end;
  end;
end;

procedure TSVGSettingsForm.OrientationRadioGroupClick(Sender: TObject);
begin
  SVGIconPosition.ImageIndex := OrientationRadioGroup.ItemIndex;
end;

procedure TSVGSettingsForm.CreateAboutForm;
begin
  FAboutForm.Free;
  FAboutForm := TFrmAbout.Create(Self);
  FAboutForm.BorderIcons := [];
  FAboutForm.Title := FTitle;
  FAboutForm.Parent := ThemeClientPanel;
  FAboutForm.Align := alClient;
  FAboutForm.DisableButtons;
  FAboutForm.btnOK.OnClick := ExitFromSettings;
  FAboutForm.Visible := True;
end;

procedure TSVGSettingsForm.PopulateAvailThemes;
var
  I: Integer;
  IsLight: Boolean;
  LStyleName: string;
  LThemeAttributes: TThemeAttribute;
begin
  if TThemeSelection(ThemesRadioGroup.ItemIndex) = tsAsWindows then
    IsLight := IsWindowsAppThemeLight
  else
    IsLight := TThemeSelection(ThemesRadioGroup.ItemIndex) = tsLightTheme;

  SelectThemeRadioGroup.Items.Clear;
{$IFNDEF DISABLE_STYLES}
  for I := 0 to High(TStyleManager.StyleNames) do
  begin
    LStyleName := TStyleManager.StyleNames[I];
    TThemeAttribute.GetStyleAttributes(LStyleName, LThemeAttributes);
    if not Assigned(LThemeAttributes) then
      Continue;
    if IsLight and (LThemeAttributes.ThemeType = ttLight) or
      (not IsLight and (LThemeAttributes.ThemeType = ttDark)) then
      SelectThemeRadioGroup.Items.Add(LStyleName);
  end;
{$ELSE}
    LStyleName := 'Windows';
    TThemeAttribute.GetStyleAttributes(LStyleName, LThemeAttributes);
{$ENDIF}
  SelectThemeRadioGroup.OnClick := nil;
  try
    TStringList(SelectThemeRadioGroup.Items).Sort;
{$IFNDEF DISABLE_STYLES}
    SelectThemeRadioGroup.ItemIndex :=
      SelectThemeRadioGroup.Items.IndexOf(TStyleManager.ActiveStyle.Name);
{$ELSE}
    SelectThemeRadioGroup.ItemIndex := 0;
{$ENDIF}
    if SelectThemeRadioGroup.ItemIndex = -1 then
      SelectThemeRadioGroup.ItemIndex := 0;
  finally
    SelectThemeRadioGroup.OnClick := SelectThemeRadioGroupClick;
    SelectThemeRadioGroupClick(SelectThemeRadioGroup);
  end;
end;

procedure TSVGSettingsForm.ExitFromSettings(Sender: TObject);
begin
  //Salva i parametri su file
  if Assigned(FSourceSynEdit) and Assigned(SynEdit) then
    CloneSynEdit(SynEdit, FSourceSynEdit);
  ModalResult := mrOk;
end;

procedure TSVGSettingsForm.CloneSynEdit(Source, Dest: TSynEdit);
begin
  Dest.Highlighter.Assign(Source.Highlighter);
  Dest.Font.Assign(Source.Font);
  Dest.Color := Source.Color;
end;

end.
