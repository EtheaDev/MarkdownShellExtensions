{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions                                              }
{       (Preview Panel, Thumbnail Icon, MD Text Editor)                        }
{                                                                              }
{       Copyright (c) 2021-2026 (Ethea S.r.l.)                                 }
{       Author: Ariel Montes                                                   }
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
unit dlgInputUrl;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ExtDlgs, System.ImageList, Vcl.ImgList, Vcl.Clipbrd,
  SVGIconImageListBase, SVGIconImageList, System.Actions, Vcl.ActnList,
  Vcl.VirtualImageList, MDShellEx.Resources, Vcl.StdActns,
  Vcl.StyledButton, Vcl.ButtonStylesAttributes;

type
  TInputType = (itImage, itLink);

  TInputUrlDialog = class(TForm)
    OpenDialog: TOpenDialog;
    OpenPictureDialog: TOpenPictureDialog;
    paButton: TPanel;
    btConfirm: TStyledButton;
    btCancel: TStyledButton;
    paEdit: TPanel;
    ActionList: TActionList;
    VirtualImageList: TVirtualImageList;
    paImage: TPanel;
    Image: TImage;
    acPaste: TAction;
    acOpenDlg: TAction;
    acOpenPictureDlg: TAction;
    acConfirm: TAction;
    acCancel: TAction;
    lbText: TLabel;
    edText: TEdit;
    TopPanel: TPanel;
    lbUrl: TLabel;
    edUrl: TEdit;
    btPaste: TStyledButton;
    btOpenDlg: TStyledButton;
    procedure FormCreate(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acOpenDlgExecute(Sender: TObject);
    procedure acOpenPictureDlgExecute(Sender: TObject);
    procedure acPasteUpdate(Sender: TObject);
    procedure acConfirmExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure edUrlChange(Sender: TObject);
  private
    FInputType: TInputType;
    FSelectedText, FSelectedUrl: string;
    FStopImageRequest: Boolean;
    FRootFolder: string;
    function GetEditedText: string;
    function GetEditedUrl: string;
    function LoadImage(const AFileName: TFileName): Boolean;
    function DefaultDialogPath: string;
    function IsUrl(const AText: string): Boolean;
  public
    constructor CreateUrl(AOwner: TComponent;
      const ARootFolder, AText, AUrl: string;
      const AInputType: TInputType);
    property EditedText: string read GetEditedText;
    property EditedUrl: string read GetEditedUrl;
  end;

function InputUrlDialog(AOwner: TComponent;
  const ARootFolder: string;
  const APrefix: string; AType: TInputType;
  var ASelectedText: string): TModalResult;

implementation

{$R *.dfm}

uses
  System.StrUtils
  , System.IOUtils
  , Winapi.ShLwApi
  , u_dzAutoCompleteFiles
  ;

function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD;
  pszTo: PChar; dwAtrTo: DWORD): LongBool; stdcall; external 'shlwapi.dll' name 'PathRelativePathToW';

function AbsToRel(const AbsPath, BasePath: string): string;
var
  Path: array[0..MAX_PATH-1] of char;
begin
  PathRelativePathTo(@Path[0], PChar(BasePath), FILE_ATTRIBUTE_DIRECTORY, PChar(AbsPath), 0);
  result := Path;
end;

function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall;
  external 'shlwapi.dll' name 'PathCanonicalizeW';

function RelToAbs(const RelPath, BasePath: string): string;
var
  Dst: array[0..MAX_PATH-1] of char;
begin
  PathCanonicalize(@Dst[0], PChar(IncludeTrailingPathDelimiter(BasePath) + RelPath));
  result := Dst;
end;

function CalcText(const AStrStart, AStrEnd, ASelectedText: string): string;
var
  LSubString: string;
  LStart, LEnd: Integer;
begin
  LStart := 0;
  //Calculates superfluous characters at the beginning of the string, skipping
  //the first occurrence of '](' if found ')](' to handle the case '[![immagine](url)](path)'
  //and correctly extracts the path
  if AStrStart = '](' then
    LStart := Pos(')'+AStrStart, ASelectedText);
  if LStart > 0 then
    LStart := LStart+Length(AStrStart)+1
  else
    //Calculates superfluous characters at the beginning of the string for all other cases.
    //Note: Before launching this function, the initial characters of the selected text are checked.
    LStart := Pos(AStrStart, ASelectedText)+Length(AStrStart);
  //Removes superfluous characters from the beginning of the string
  LSubString := Copy(ASelectedText, LStart, Length(ASelectedText));

  //Calculates superfluous characters at the end of the string
  if (AStrStart = '[') and StartsStr('[![', ASelectedText) then
    //Handles the case '[![immagine](url)](path)'
    LEnd := Pos(')'+AStrEnd, LSubString)-Length(AStrEnd)+1
  else
    //Handles all other cases.
    LEnd := Pos(AStrEnd, LSubString)-Length(AStrEnd);
  //Removes superfluous characters from the end of the string
  LSubString := Copy(LSubString, 0, LEnd);

  Result := LSubString;
end;

function InputUrlDialog(AOwner: TComponent;
  const ARootFolder: string;
  const APrefix: string; AType: TInputType;
  var ASelectedText: string): TModalResult;
var
  LInputUrl: TInputUrlDialog;
  LText, LURL: string;
begin
  //Calculate Text and Url from the selected text to allow the modifications
  if (Pos('](', ASelectedText) > 0) and StartsStr(APrefix, TrimLeft(ASelectedText)) then
  begin
    LText := CalcText(APrefix, ']', ASelectedText);
    LUrl := CalcText('](', ')', ASelectedText);
  end
  else
    LText := ASelectedText;
  try
    //Create and initialize the InputUrlDialog form
    LInputUrl := TInputUrlDialog.CreateUrl(AOwner, ARootFolder, LText, LUrl, AType);
    LInputUrl.ShowModal;

    Result := LInputUrl.ModalResult;
    //Add the result command to the Editor page
    if LInputUrl.ModalResult = mrOK then
      ASelectedText := APrefix+LInputUrl.EditedText+']('+LInputUrl.EditedUrl+')';
  finally
    FreeAndNil(LInputUrl);
  end;
end;

procedure TInputUrlDialog.FormCreate(Sender: TObject);
begin
  TEdit_ActivateAutoCompleteFiles(edUrl, '*.*');

  //Modify the GUI based on the type of element to insert (link or image)
  if FInputType = itImage then
  begin
    caption := Format('Insert image (%s)', [FRootFolder]);
    lbText.Caption := 'Alt Text';
    btOpenDlg.Action := acOpenPictureDlg;
  end
  else if FInputType = itLink then
  begin
    caption := Format('Insert link (%s)', [FRootFolder]);
    lbText.Caption := 'Link Text';
    btOpenDlg.Action := acOpenDlg;
  end;
  paImage.Visible := FInputType = itImage;

  //Load the selected text into the edit components in the MainForm
  edText.Text := FSelectedText;
  edUrl.Text := FSelectedUrl;
end;

function TInputUrlDialog.GetEditedText: string;
begin
  Result := edText.Text;
end;

function TInputUrlDialog.GetEditedUrl: string;
begin
  Result := edUrl.Text;
end;

function TInputUrlDialog.DefaultDialogPath: string;
var
  LFileName: TFileName;
begin
  Result := '';
  if edUrl.Text <> '' then
  begin
    LFileName := edUrl.Text;
    dmResources.TryExpandSpaces(FRootFolder, LFileName);
    LFileName := StringReplace(LFileName, '/', '\', [rfReplaceAll]);
    LFileName := StringReplace(LFileName, '\.\', '\', [rfReplaceAll]);
    Result := ExtractFilePath(LFileName);
    if not DirectoryExists(Result) then
      Result := '';
  end;
  if Result = '' then
    Result := FRootFolder;
end;

function TInputUrlDialog.IsUrl(const AText: string): Boolean;
begin
  Result := PathIsURL(PChar(AText));
end;

procedure TInputUrlDialog.acPasteExecute(Sender: TObject);
begin
  //Check if the clipboard format is text
  if Clipboard.HasFormat(CF_TEXT) then
    edUrl.Text := Clipboard.AsText;
end;

procedure TInputUrlDialog.acPasteUpdate(Sender: TObject);
begin
  //Enable the paste button if the clipboard content is a URL
  acPaste.Enabled := Clipboard.HasFormat(CF_TEXT) and IsUrl(Clipboard.AsText);
end;

procedure TInputUrlDialog.acOpenDlgExecute(Sender: TObject);
begin
  OpenDialog.InitialDir := DefaultDialogPath;
  if OpenDialog.Execute then
    edUrl.Text := OpenDialog.FileName;
end;

procedure TInputUrlDialog.acOpenPictureDlgExecute(Sender: TObject);
begin
  OpenPictureDialog.InitialDir := DefaultDialogPath;
  if OpenPictureDialog.Execute then
    edUrl.Text := OpenPictureDialog.FileName;
end;

procedure TInputUrlDialog.acConfirmExecute(Sender: TObject);
begin
  Close;
  ModalResult := mrOk;
end;

procedure TInputUrlDialog.acCancelExecute(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

constructor TInputUrlDialog.CreateUrl(AOwner: TComponent;
  const ARootFolder, AText, AUrl: string; const AInputType: TInputType);
begin
  inherited Create(AOwner);
  FSelectedText := AText;
  FSelectedUrl := AUrl;
  FInputType := AInputType;
  FStopImageRequest := False;
  FRootFolder := ARootFolder;
end;

function TInputUrlDialog.LoadImage(
  const AFileName: TFileName): Boolean;
var
  LFullName: TFileName;
  LMaxWidth: Integer;
  LStream: TStream;
begin
  Result := False;

  if FStopImageRequest then
    Exit;

  LMaxWidth := Self.Monitor.Width;

  LStream := nil;
  LFullName := AFileName;
  dmResources.TryExpandSpaces(FRootFolder, LFullName);

  if dmResources.LoadFileContent(LFullName, FRootFolder, LMaxWidth,
    Self.Color, LStream) then
  begin
    if Assigned(LStream) and (LStream.Size > 0) then
    begin
      LStream.Position := 0;
      Image.Picture.LoadFromStream(LStream);
      Result := True;
    end
    else
      Image.Picture := nil;
  end
  else
    Image.Picture := nil;
end;

procedure TInputUrlDialog.edUrlChange(Sender: TObject);
begin
  //Defaults to the file name as alternative text for the image
  if (FInputType = itImage) then
  begin
    if LoadImage(edUrl.Text) and (FSelectedText = '') then
      edText.Text := ChangeFileExt(ExtractFileName(edUrl.Text),'');
  end
  else if (FInputType = itLink) then
  begin
    if FileExists(edUrl.Text) and (FSelectedText = '') then
      edText.Text := ChangeFileExt(ExtractFileName(edUrl.Text),'');
  end;
  //Replaces the full path with the relative path when the path matches the
  //path of the open file
  if AnsiStartsText(FRootFolder, edUrl.Text) then
    edUrl.Text := StringReplace(StringReplace(edUrl.Text, FRootFolder, '.',
      [rfReplaceAll, rfIgnoreCase]), '\', '/', [rfReplaceAll]);
end;

end.
