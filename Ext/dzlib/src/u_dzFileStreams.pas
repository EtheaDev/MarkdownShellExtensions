{.GXFormatter.config=twm}
///<summary> This unit implements a TdzFile and a TdzTempFile class.
///          TdzFile encapsulates the rather complex CreateFile WinApi call,
///          TdzTempFile is a descendant of TdzFile, uses a temporary filename
///          and sets the properties to sensible values for temporary files. </summary>
unit u_dzFileStreams;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Classes,
  SysUtils,
  Windows,
  u_dzTypes, // necessary for correcting NativeInt
  u_dzTranslator;

type
  ///<summary> Parent of all exceptions raised in u_dzFiles </summary>
  EdzFile = class(Exception);

type
  ///<summary> File access modes
  ///     <ul>
  ///       <li>faRead = open file for reading</li>
  ///     <li>faWrite = open file for writing</li>
  ///   </ul></summary>
  TFileAccessModes = (faRead, faWrite);
  ///<summary> Set of file access modes. Used to pass combinations of faRead and faWrite </summary>
  TFileAccessModeSet = set of TFileAccessModes;
  ///<summary> File share modes.
  ///     <ul>
  ///       <li>fsRead = share file for reading</li>
  ///       <li>fsWrite = share file for writing</li>
  ///     </ul></summary>
  TFileShareModes = (fsRead, fsWrite);
  ///<summary> Set of file share modes. Used to pass combinations of fsRead and fsWrite </summary>
  TFileShareModeSet = set of TFileShareModes;
  ///<summary> Possible actions to take on opening a file.
  ///     <ul>
  ///       <li>fcCreateFailIfExists = Create a new file. If it already exists, fail.</li>
  ///       <li>fcCreateTruncateIfExists = Create a new file. If it already exists, truncate it.</li>
  ///       <li>fcOpenFailIfNotExists = Open an existing file. If it does not exists, fail.</li>
  ///       <li>fcOpenCreateIfNotExists = Open an existing file. If it does not exist, create it.</li>
  ///       <li>fcOpenTruncateFailIfNotExists = Open an existing file and truncate it. If it does not exist, fail.</li>
  ///     </ul></summary>
  TFileCreateDisposition = (fcCreateFailIfExists, fcCreateTruncateIfExists,
    fcOpenFailIfNotExists, fcOpenCreateIfNotExists,
    fcOpenTruncateFailIfNotExists);

const
  ///<summary> Constant to pass the file access mode read/write </summary>
  faReadWrite = [faRead, faWrite];
  ///<summary> Constant to pass the file share mode read/write </summary>
  fsReadWrite = [fsRead, fsWrite];
  ///<summary> Constant to pass the file share mode no sharing </summary>
  fsNoSharing = [];

type
  ///<summary> Enhanced TFileStream based on THandleStream.
  ///          Allows to specify all the parameters that can be specified to the
  ///          Windows API function CreateFile </summary>
  TdzFile = class(THandleStream)
  protected
    ///<summary> Stores the Filename property </summary>
    FFilename: string;
    ///<summary> Stores the AccessMode property </summary>
    FAccessMode: TFileAccessModeSet;
    ///<summary> Stores the ShareMode property </summary>
    FShareMode: TFileShareModeSet;
    ///<summary> Stores the CreateDisposition property </summary>
    FCreateDisposition: TFileCreateDisposition;
    ///<summary> Stores the SecurityAttributes property </summary>
    FSecurityAttributes: PSecurityAttributes;
    ///<summary> Stores the FileAttributes property </summary>
    FFileAttributes: DWORD;
    ///<summary> Stores the FileFlags property </summary>
    FFileFlags: DWORD;
    ///<summary> Stores the ResetReadOnly property </summary>
    FResetReadOnly: Boolean;
    ///<summary> Set method for FileAttributes property </summary>
    procedure SetFileAttributes(_FileAttributes: DWORD);
    ///<summary> Set method for FileFlags property </summary>
    procedure SetFileFlags(_FileFlags: DWORD);
  public
    ///<summary>
    /// Short for
    /// .Create
    /// .OpenReadOnly
    /// .ReadBuffer
    /// .Free
    /// @raises the usual exceptions that can occur in these operations  </summary>
    class procedure CreateReadFree(const _fn: string; var _Buffer; _Size: Integer);
    ///<summary>
    /// Short for
    /// .Create
    /// .OpenCreateWriteNoSharing
    /// .WriteBuffer
    /// .Free
    /// @raises the usual exceptions that can occur in these operations  </summary>
    class procedure CreateWriteFree(const _fn: string; const _Buffer; _Size: Integer);
    ///<summary> Creates a TdzFile object.
    ///          @param Filename is a string containing the name of the file to open </summary>
    constructor Create(const _Filename: string);
    ///<summary>Flushes and closes the file if it is still open, destroys the object </summary>
    destructor Destroy; override;
    ///<summary>
    /// Checks the handle and returns true if it is <> INVALID_HANDLE_VALUE </summary>
    function IsOpen: Boolean;
    ///<summary> Opens the file as specified by the properties, raises an exception on error </summary>
    procedure Open;
    ///<summary> Opens the file as specified by the properties, returns false on error </summary>
    function OpenNoException: Boolean;
    ///<summary>
    /// Opens the file in readonly mode, with
    /// * CreateDisposition = fcOpenFailIfNotExists
    /// * ShareMode = [fsRead]
    /// * AccessMode = [faRead]
    /// raises an exception on error </summary>
    procedure OpenReadonly;
    ///<summary>
    /// Opens a new file for writing only (or truncates an existing file):
    /// * AccessMode := [faWrite];
    /// * ShareMode := fsNoSharing;
    /// * CreateDisposition := fcCreateTruncateIfExists;
    /// raises an exception on error </summary>
    procedure OpenCreateWriteNoSharing;
    ///<summary> Opens the file and seeks to the end. Returns the new position (that is: The file length). </summary>
    function Append: LongInt;
    ///<summary>
    /// Like TStream.WriteBuffer but if an error occurs, includes the file name in the
    /// exception message.
    /// NOTE: TStream.WriteBuffer is *not* virtual, so you must declare the object as
    /// TdzFile rather TStream for this method to be called. </summary.
    procedure WriteBuffer(const _Buffer; _Count: NativeInt); overload;
{$IF SizeOf(LongInt) <> SizeOf(NativeInt)}
    procedure WriteBuffer(const _Buffer; _Count: LongInt); overload;
{$IFEND}
    ///<summary> Closes the file and sets the handle to INVALID_HANDLE_VALUE </summary>
    procedure Close;
    ///<summary> returns true if Position = Size </summary>
    function Eof: Boolean;
    ///<summary> The file's name as passed to the Create constructor. </summary>
    property Filename: string read FFilename;
    ///<summary> The file's access mode.
    ///          Can be set to [faRead], [faWrite] or [faRead, faWrite]. </summary>
    property AccessMode: TFileAccessModeSet read FAccessMode write FAccessMode;
    ///<summary> The file's share mode.
    ///          Can be set to [], [fsRead], [fsWrite], [fsRead, fsWrite]. There are also
    ///          two predefined constants fsNoSharing and fsReadWrite </summary>
    property ShareMode: TFileShareModeSet read FShareMode write FShareMode;
    ///<summary> The file's security attributes.
    ///          See the Windows API function CreateFile for details. </summary>
    property SecurityAttributes: PSecurityAttributes read FSecurityAttributes write FSecurityAttributes;
    ///<summary> Specifies the action to take in various error conditions.
    ///     <ul>
    ///       <li>fcCreateFailIfExists = Create a new file. If it already exists, fail.</li>
    ///       <li>fcCreateTruncateIfExists = Create a new file. If it already exists, truncate it.</li>
    ///       <li>fcOpenFailIfNotExists = Open an existing file. If it does not exists, fail.</li>
    ///       <li>fcOpenCreateIfNotExists = Open an existing file. If it does not exist, create it.</li>
    ///       <li>fcOpenTruncateFailIfNotExists = Open an existing file and truncate it. If it does not exist, fail.</li>
    ///     </ul></summary>
    property CreateDisposition: TFileCreateDisposition read FCreateDisposition write FCreateDisposition;
    ///<summary> The file's attributes.
    ///          Can be set to a combination of:
    ///     <ul>
    ///       <li>FILE_ATTRIBUTE_ARCHIVE</li>
    ///       <li>FILE_ATTRIBUTE_COMPRESSED</li>
    ///       <li>FILE_ATTRIBUTE_HIDDEN</li>
    ///       <li>FILE_ATTRIBUTE_NORMAL</li>
    ///       <li>FILE_ATTRIBUTE_OFFLINE</li>
    ///       <li>FILE_ATTRIBUTE_READONLY</li>
    ///       <li>FILE_ATTRIBUTE_SYSTEM</li>
    ///       <li>FILE_ATTRIBUTE_TEMPORARY</li>
    ///     </ul>
    ///          See the Windows API help for a description. Note that these attribute
    ///          are only used when actually creating a new file. </summary>
    property FileAttributes: DWORD read FFileAttributes write SetFileAttributes;
    ///<summary> The file's flags.
    ///          Can be set to a combination of:
    ///     <ul>
    ///       <li>FILE_FLAG_WRITE_THROUGH</li>
    ///       <li>FILE_FLAG_OVERLAPPED</li>
    ///       <li>FILE_FLAG_NO_BUFFERING</li>
    ///       <li>FILE_FLAG_RANDOM_ACCESS</li>
    ///       <li>FILE_FLAG_SEQUENTIAL_SCAN</li>
    ///       <li>FILE_FLAG_DELETE_ON_CLOSE</li>
    ///       <li>FILE_FLAG_BACKUP_SEMANTICS</li>
    ///       <li>FILE_FLAG_POSIX_SEMANTICS</li>
    ///     </ul>
    ///          See the Windows API help for a description. Note that these attribute
    ///          are only used when actually creating a new file. </summary>
    property FileFlags: DWORD read FFileFlags write SetFileFlags;
    ///<summary> This flag determines whether to try resetting the readonly flag of the file
    ///          if opening fails. Only used if AccessMode contains faWrite.
    ///   Usually you don't want this, so the default is false </summary>
    property ResetReadOnly: Boolean read FResetReadOnly write FResetReadOnly;
  end;

type
  ///<summary> Represents a temporary file.
  ///          This is just a TdzFile object which is created in the TEMP directory and
  ///          the following properties: <br>
  ///            AccessMode := faReadWrite; <br>
  ///            ShareMode := fsReadWrite; <br>
  ///            CreateDisposition := fcCreateTruncateIfExists; <br>
  ///            FileAttributes := FILE_ATTRIBUTE_TEMPORARY; <br>
  ///            FileFlags := FILE_FLAG_DELETE_ON_CLOSE; <br>
  ///</summary>
  TdzTempFile = class(TdzFile)
  public
    ///<summary> For a description of how these parameters are used, see
    ///          the documentation of the Windows GetTempFileName API function.
    ///          @param Directory is the directory in which the file should be
    ///                           created. Defaults to the %TEMP% directory
    ///          @param Prefix is a string used as the prefix for the filename,
    ///                        defaults to 'dz'.
    ///          @param Unique Unsigned integer to be used in creating the
    ///                        temporary file name. For more information, see
    ///                        the documentation of the Windows GetTempFilename
    ///                        API function. Defaults to 0. </summary>
    constructor Create(const _Directory: string = ''; const _Prefix: string = 'dz';
      _Unique: Word = 0);
  end;

implementation

uses
  u_dzFileUtils,
  u_dzMiscUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

const
  cFileAccessModeValues: array[TFileAccessModes] of DWORD =
    (GENERIC_READ, GENERIC_WRITE);
  cFileShareModeValues: array[TFileShareModes] of DWORD =
    (FILE_SHARE_READ, FILE_SHARE_WRITE);
  cFileCreateDispositionValues: array[TFileCreateDisposition] of DWORD =
    (CREATE_NEW, CREATE_ALWAYS, OPEN_EXISTING, OPEN_ALWAYS, TRUNCATE_EXISTING);

type
  ///<summary> This is a hack to access the fHandle field which is private in the
  ///          declaration of THandleStream. It will stop working when Borland
  ///          adds more fields in front of FHandle. Check when upgrading! </summary>
{$IFDEF DELPHI2007_UP}
  THandleStreamHack = TdzFile;
{$ELSE}
  THandleStreamHack = class(TStream)
  private
    FHandle: Integer;
  end;
{$ENDIF}

type
{$IFDEF THANDLESTREAM_CREATE_HANDLE_IS_THANDLE}
  THandleStreamCreateHandleCast = THandle;
{$ELSE}
  THandleStreamCreateHandleCast = Integer;
{$ENDIF}
{$IFDEF THANDLESTREAM_HANDLE_IS_THANDLE}
  THandleCast = THandle;
{$ELSE}
  THandleCast = Integer;
{$ENDIF}

const
  INVALID_HANDLE_VALUE = -1;

class procedure TdzFile.CreateReadFree(const _fn: string; var _Buffer; _Size: Integer);
var
  Stream: TdzFile;
begin
  Stream := TdzFile.Create(_fn);
  try
    Stream.OpenReadonly;
    Stream.ReadBuffer(_Buffer, _Size);
  finally
    FreeAndNil(Stream);
  end;
end;

class procedure TdzFile.CreateWriteFree(const _fn: string; const _Buffer; _Size: Integer);
var
  Stream: TdzFile;
begin
  Stream := TdzFile.Create(_fn);
  try
    Stream.OpenCreateWriteNoSharing;
    Stream.WriteBuffer(_Buffer, NativeInt(_Size));
  finally
    FreeAndNil(Stream);
  end;
end;

constructor TdzFile.Create(const _Filename: string);
begin
  inherited Create(THandleStreamCreateHandleCast(INVALID_HANDLE_VALUE));
  FFilename := _Filename;
  FAccessMode := [faRead];
  FShareMode := [fsRead];
  FCreateDisposition := fcOpenFailIfNotExists;
  FSecurityAttributes := nil;
  FFileAttributes := FILE_ATTRIBUTE_NORMAL;
  FFileFlags := 0;
end;

destructor TdzFile.Destroy;
begin
  Close;
  inherited;
end;

function TdzFile.IsOpen: Boolean;
begin
  Result := (THandleStreamHack(Self).FHandle <> THandleCast(INVALID_HANDLE_VALUE));
end;

procedure TdzFile.Close;
begin
  if IsOpen then
    CloseHandle(THandleStreamHack(Self).FHandle);
  THandleStreamHack(Self).FHandle := THandleCast(INVALID_HANDLE_VALUE);
end;

procedure TdzFile.OpenReadonly;
begin
  CreateDisposition := fcOpenFailIfNotExists;
  ShareMode := [fsRead, fsWrite];
  AccessMode := [faRead];
  Open;
end;

procedure TdzFile.OpenCreateWriteNoSharing;
begin
  CreateDisposition := fcCreateTruncateIfExists;
  ShareMode := fsNoSharing;
  AccessMode := [faWrite];
  Open;
end;

procedure TdzFile.Open;
var
  LastError: Cardinal;
begin
  if not OpenNoException then begin
    LastError := GetLastError;
    RaiseLastOSErrorEx(LastError, Format(_('%%1:s (%%0:d) trying to open "%s"'), [FFilename]));
  end;
end;

function TdzFile.OpenNoException: Boolean;
var
  fam: TFileAccessModes;
  Access: DWORD;
  fsm: TFileShareModes;
  TheShareMode: DWORD;
  Disposition: DWORD;
  TriedResetReadonly: Boolean;
  ApiHandle: THandle;
begin
  Access := 0;
  for fam := Low(TFileAccessModes) to High(TFileAccessModes) do
    if fam in FAccessMode then
      Access := Access or cFileAccessModeValues[fam];
  TheShareMode := 0;
  for fsm := Low(TFileShareModes) to High(TFileShareModes) do
    if fsm in FShareMode then
      TheShareMode := TheShareMode or cFileShareModeValues[fsm];
  Disposition := cFileCreateDispositionValues[FCreateDisposition];

  TriedResetReadonly := False;
  repeat
    ApiHandle := Windows.CreateFile(PChar(FFilename), Access,
      TheShareMode, FSecurityAttributes, Disposition, FFileAttributes or FFileFlags, 0);
    Result := (ApiHandle <> THandle(INVALID_HANDLE_VALUE));
    if not Result and ResetReadOnly and not TriedResetReadonly then
      TFileSystem.SetReadonly(FFilename, False, ehReturnFalse);
  until Result or TriedResetReadonly or not ResetReadOnly or not (faWrite in AccessMode) and ResetReadOnly;
  if Result then
    THandleStreamHack(Self).FHandle := THandleCast(ApiHandle)
  else
    THandleStreamHack(Self).FHandle := THandleCast(INVALID_HANDLE_VALUE);
end;

function TdzFile.Append: LongInt;
begin
  Open;
  Result := Seek(0, soFromEnd);
end;

procedure TdzFile.SetFileAttributes(_FileAttributes: DWORD);
const
  cValidAttributes = FILE_ATTRIBUTE_ARCHIVE or FILE_ATTRIBUTE_COMPRESSED or
    FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_NORMAL or
    FILE_ATTRIBUTE_OFFLINE or FILE_ATTRIBUTE_READONLY or
    FILE_ATTRIBUTE_SYSTEM or FILE_ATTRIBUTE_TEMPORARY;
begin
  if (_FileAttributes and not cValidAttributes) <> 0 then
    raise EdzFile.Create(_('Invalid file attributes'));
  FFileAttributes := _FileAttributes;
end;

procedure TdzFile.SetFileFlags(_FileFlags: DWORD);
const
  cValidFlags = (FILE_FLAG_WRITE_THROUGH or FILE_FLAG_OVERLAPPED or
    FILE_FLAG_NO_BUFFERING or FILE_FLAG_RANDOM_ACCESS or
    FILE_FLAG_SEQUENTIAL_SCAN or FILE_FLAG_DELETE_ON_CLOSE or
    FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_POSIX_SEMANTICS);
begin
  if (_FileFlags and not cValidFlags) <> 0 then
    raise EdzFile.Create(_('Invalid file flags'));
  FFileFlags := _FileFlags;
end;

procedure TdzFile.WriteBuffer(const _Buffer; _Count: NativeInt);
var
  Written: Integer;
  LastError: LongWord;
  ErrStrFmt: string;
begin
  if _Count <> 0 then begin
    Written := Write(_Buffer, _Count);
    if (Written <> _Count) then begin
      LastError := GetLastError;
      // Note: The %% for the first two parameters is not an error. These will automatically
      // be replaced with a single % by the format function.
      ErrStrFmt := Format(_('Error %%1:s (%%0:d) writing to file %s. Wrote only %d of %d bytes.'),
        [FFilename, Written, _Count]);
      RaiseLastOSErrorEx(LastError, ErrStrFmt);
    end;
  end;
end;

{$IF SizeOf(LongInt) <> SizeOf(NativeInt)}

procedure TdzFile.WriteBuffer(const _Buffer; _Count: LongInt);
begin
  WriteBuffer(_Buffer, NativeInt(_Count));
end;
{$IFEND}

function TdzFile.Eof: Boolean;
begin
  Result := Position = Size;
end;

{ TdzTempFile }

constructor TdzTempFile.Create(const _Directory: string; const _Prefix: string; _Unique: Word);
begin
  inherited Create(TFileSystem.GetTempFileName(_Directory, _Prefix, _Unique));
  AccessMode := faReadWrite;
  ShareMode := fsReadWrite;
  CreateDisposition := fcCreateTruncateIfExists;
  FileAttributes := FILE_ATTRIBUTE_TEMPORARY;
  FileFlags := FILE_FLAG_DELETE_ON_CLOSE;
end;

end.
