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
    function LocalFileName(const AText: string): TFileName;
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
  , MarkDownViewerCommon
  , MarkdownHtmlRenderer
  , Vcl.StyledTaskDialog
  ;

resourcestring
  WARNING_FILE_URL = 'The file is not on the drive of the document (or the '+
    'document has not been saved yet), so it cannot be linked with a path '+
    'relative to the document: the link will be "%s".'+sLineBreak+sLineBreak+
    'In safe mode (the default) the file: links are removed, so the file will '+
    'not be shown: copy it in the folder of the document (or in a subfolder), '+
    'or enable the unsafe mode.'+sLineBreak+sLineBreak+
    'Do you want to insert the link anyway?';

const
  //the videos of mexVideo (IsVideoUrl)
  VideoFileMask = '*.mp4;*.m4v;*.webm;*.ogv;*.mov';

//A Windows path ("C:\folder\file", "C:/folder/file", "\\server\share"):
//in Markdown it must become a URL
function IsWindowsPath(const AText: string): Boolean;
begin
  Result := ((Length(AText) >= 3) and CharInSet(AText[1], ['A'..'Z', 'a'..'z']) and
    (AText[2] = ':') and CharInSet(AText[3], ['\', '/'])) or
    StartsStr('\\', AText);
end;

//The chars of a path that are not valid (or have a meaning) in the URL of a
//Markdown link: percent-encoded
function EncodeURLPath(const APath: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(APath) do
  begin
    C := APath[I];
    if (Ord(C) <= 32) or CharInSet(C, ['%', '#', '?', '(', ')', '<', '>', '"',
      '[', ']', '^', '`', '{', '}', '|']) then
      Result := Result + Format('%%%.2X', [Ord(C)])
    else
      Result := Result + C;
  end;
end;

//The URL of a local file for the Markdown document of ARootFolder: relative
//to the document, with "/" ("../images/image.png"); a file URL when it cannot
//be relative (another drive, document not saved yet): "file:///D:/image.png"
function FileNameToURL(const AFileName, ARootFolder: string): string;
var
  LFileName: string;
begin
  LFileName := ExpandFileName(AFileName);
  if (ARootFolder <> '') and SameText(ExtractFileDrive(LFileName),
    ExtractFileDrive(ExpandFileName(ARootFolder))) then
  begin
    Result := ExtractRelativePath(IncludeTrailingPathDelimiter(
      ExpandFileName(ARootFolder)), LFileName);
    Result := EncodeURLPath(StringReplace(Result, '\', '/', [rfReplaceAll]));
  end
  else if StartsStr('\\', LFileName) then
    Result := 'file:' + EncodeURLPath(StringReplace(LFileName, '\', '/', [rfReplaceAll]))
  else
    Result := 'file:///' + EncodeURLPath(StringReplace(LFileName, '\', '/', [rfReplaceAll]));
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
    //also the videos: an image of a video file is a video (mexVideo)
    OpenPictureDialog.Filter := 'Images and videos|' + GraphicFileMask(TGraphic) +
      ';' + VideoFileMask + '|Videos (' + VideoFileMask + ')|' + VideoFileMask +
      '|' + OpenPictureDialog.Filter;
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
  //Never a Windows path in the document: the URL, relative to the document
  Result := Trim(edUrl.Text);
  if IsWindowsPath(Result) then
    Result := FileNameToURL(Result, FRootFolder)
  else if not IsUrl(Result) then
    //a relative path written by hand: "/" and no spaces
    Result := StringReplace(StringReplace(Result, '\', '/', [rfReplaceAll]),
      ' ', '%20', [rfReplaceAll]);
end;

function TInputUrlDialog.LocalFileName(const AText: string): TFileName;
begin
  //The local file of the text of edUrl: a Windows path or the URL of the
  //document; empty for a web URL
  if IsWindowsPath(AText) then
    Result := AText
  else if not URLToLocalFileName(FRootFolder, AText, Result) then
    Result := '';
end;

function TInputUrlDialog.DefaultDialogPath: string;
begin
  Result := '';
  if edUrl.Text <> '' then
  begin
    Result := ExtractFilePath(LocalFileName(Trim(edUrl.Text)));
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
    edUrl.Text := FileNameToURL(OpenDialog.FileName, FRootFolder);
end;

procedure TInputUrlDialog.acOpenPictureDlgExecute(Sender: TObject);
begin
  OpenPictureDialog.InitialDir := DefaultDialogPath;
  if OpenPictureDialog.Execute then
    edUrl.Text := FileNameToURL(OpenPictureDialog.FileName, FRootFolder);
end;

procedure TInputUrlDialog.acConfirmExecute(Sender: TObject);
begin
  //a file URL (another drive): emptied in safe mode
  if StartsText('file:', EditedUrl) and (StyledMessageDlg(Format(WARNING_FILE_URL,
    [EditedUrl]), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes) then
  begin
    //the button has already set ModalResult: the dialog stays open
    ModalResult := mrNone;
    Exit;
  end;
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
  //a local file (Windows path or URL of the document), else a web URL
  LFullName := LocalFileName(AFileName);
  if LFullName = '' then
    LFullName := AFileName;

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
var
  LFileName: TFileName;
begin
  //Defaults to the file name as alternative text for the image
  //(the URL written in the document is GetEditedUrl)
  LFileName := LocalFileName(Trim(edUrl.Text));
  if (FInputType = itImage) then
  begin
    if (LoadImage(Trim(edUrl.Text)) or IsVideoUrl(LFileName)) and
      (FSelectedText = '') and (LFileName <> '') then
      edText.Text := ChangeFileExt(ExtractFileName(LFileName),'');
  end
  else if (FInputType = itLink) then
  begin
    if (LFileName <> '') and FileExists(LFileName) and (FSelectedText = '') then
      edText.Text := ChangeFileExt(ExtractFileName(LFileName),'');
  end;
end;

end.
