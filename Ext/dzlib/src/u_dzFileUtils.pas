{.GXFormatter.config=twm}
/// <summary>
/// implements utility functions for file accesss </summary>
unit u_dzFileUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Types,
  u_dzTranslator,
  u_dzTypes;

type
  EFileUtils = class(Exception);
  ECreateUniqueDir = class(EFileUtils);
  /// <summary>
  /// raised by DelTree if the DirName parameter is not a valid directory name </summary>
  EDirNotFound = class(EFileUtils);
  EPathTooLong = class(EFileUtils);
  EInvalidPropertyCombination = class(EFileUtils);
  EFileNotFound = class(EFileUtils);
  EBackupFailed = class(EFileUtils);

type
  TFileAttributes = (
    dfaReadonly,
    dfaHidden, // Hidden files
    dfaSysFile, // System files
    dfaDirectory, // Directory files
    dfaArchive // Archive files
    );

  TFileAttributeSet = set of TFileAttributes;

const
  ALL_FILES_ATTRIB_SET = [dfaHidden, dfaSysFile, dfaDirectory, dfaArchive];

type
  TFileInfoRec = record
    Filename: string;
    Size: Int64;
    Timestamp: TDateTime;
  end;

const
  FILE_READ_DATA = $0001; // file & pipe
  FILE_LIST_DIRECTORY = $0001; // directory
  FILE_WRITE_DATA = $0002; // file & pipe
  FILE_ADD_FILE = $0002; // directory
  FILE_APPEND_DATA = $0004; // file
  FILE_ADD_SUBDIRECTORY = $0004; // directory
  FILE_CREATE_PIPE_INSTANCE = $0004; // named pipe
  FILE_READ_EA = $0008; // file & directory
  FILE_WRITE_EA = $0010; // file & directory
  FILE_EXECUTE = $0020; // file
  FILE_TRAVERSE = $0020; // directory
  FILE_DELETE_CHILD = $0040; // directory
  FILE_READ_ATTRIBUTES = $0080; // all
  FILE_WRITE_ATTRIBUTES = $0100; // all
  FILE_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $1FF;
  FILE_GENERIC_READ = STANDARD_RIGHTS_READ or FILE_READ_DATA or FILE_READ_ATTRIBUTES or
    FILE_READ_EA or SYNCHRONIZE;
  FILE_GENERIC_WRITE = STANDARD_RIGHTS_WRITE or FILE_WRITE_DATA or FILE_WRITE_ATTRIBUTES or
    FILE_WRITE_EA or FILE_APPEND_DATA or SYNCHRONIZE;
  FILE_GENERIC_EXECUTE = STANDARD_RIGHTS_EXECUTE or FILE_READ_ATTRIBUTES or FILE_EXECUTE or
    SYNCHRONIZE;

type
  TOnFileEnumCallback = procedure(_Sender: TObject; const _Filename: string) of object;

type
  /// <summary>
  /// a simple wrapper around FindFirst/FindNext which allows to search for
  /// specified attributes only (e.g. only directories), it automatically
  /// ignores the special '.' and '..' directories. </summary>
  TSimpleDirEnumerator = class
  protected
    /// stores the search mask ('c:\windows\*.exe')
    FMask: string;
    /// set of attributes a file must match
    FMustHaveAttr: TFileAttributeSet;
    /// set of attributes a file may have
    FMayHaveAttr: TFileAttributeSet;
    /// internally used TSearchRec structure
    FSr: TSearchRec;
    /// true if FindFirst was called and returned no error code
    FActive: Boolean;
    /// number of matching files found
    FMatchCount: Integer;
  public
    /// <summary>
    /// Creates a TSimpleDirEnumerator, sets the Mask, MayHaveAttr and MustHaveAttr
    /// properties.
    /// MustHaveAttr is set to []
    /// @param Mask is the file search mask and should include a path
    /// @param MayHaveAttr is the value for the MayHaveAttr property, defaults to
    ///                    ALL_FILES_ATTRIB_SET which includes all possible attributes,
    ///                    so calling FindNext will find any files or subdirectories,
    ///                    except the special '.' and '..' directories </summary>
    constructor Create(const _Mask: string; _MayHaveAttr: TFileAttributeSet = ALL_FILES_ATTRIB_SET);
    ///<summary>
    /// creates a TSimpleDirEnumerator for enumerating directories only </summary>
    constructor CreateForDirsOnly(const _Mask: string);
    ///<summary>
    /// creates a TSimpleDirEnumerator for enumerating files only </summary>
    //constructor CreateForFilesOnly(const _Mask: string);
    /// <summary>
    /// Destructor, will call FindClose if necessary </summary>
    destructor Destroy; override;
    /// <summary>
    /// creates a TSimpleDirEnumerator, calls its FindAll method and frees it
    /// @param List is a string list to which the files will be appended, may be nil
    /// @param IncludePath determines whether the List of filenames includes the full path or not </summary>
    class function Execute(const _Mask: string; _List: TStrings;
      _MayHaveAttr: TFileAttributeSet = ALL_FILES_ATTRIB_SET; _IncludePath: Boolean = False; _Sort: Boolean = True): Integer;
    /// <summary>
    /// creates a TSimpleDirEnumerator, calls its FindAll method and frees it
    /// @param List is a string list to which the files will be appended, may be nil
    /// @param IncludePath determines whether the List of filenames includes the full path or not </summary>
    class function EnumFilesOnly(const _Mask: string; _List: TStrings;
      _IncludePath: Boolean = False; _Sort: Boolean = True): Integer; overload;
    class function EnumFilesOnly(const _Mask: string;
      _IncludePath: Boolean = False; _Sort: Boolean = True): TStringArray; overload;
    ///<summary>
    /// Calls the given callback for all files matching the mask. To Abort, raise EAbort. </summary>
    class procedure EnumFilesOnly(const _Mask: string; _Callback: TOnFileEnumCallback;
      _IncludePath: Boolean = False; _Sort: Boolean = True); overload;
    /// <summary>
    /// creates a TSimpleDirEnumerator, calls its FindAll method and frees it
    /// @param List is a string list to which the files will be appended, may be nil
    /// @param IncludePath determines whether the List of filenames includes the full path or not </summary>
    class function EnumDirsOnly(const _Mask: string; _List: TStrings;
      _IncludePath: Boolean = False; _Sort: Boolean = True): Integer; overload;
    class function EnumDirsOnly(const _Mask: string; _IncludePath: Boolean = False; _Sort: Boolean = True): TStringArray; overload;
    /// <summary>
    /// Calls SysUtils.FindFirst on first call and SysUtls.FindNext in later
    /// calls.
    /// @param Filename is the name of the file found, if result is true, if you need
    ///       more information about it, use the SR property, note that it
    ///       does not include the path
    /// @param IncludePath determines whether the List of filenames includes the full path or not
    ///                    defaults to false
    /// @Returns true, if a matching file was found, false otherwise </summary>
    function FindNext(out _Filename: string; _IncludePath: Boolean = False): Boolean; overload;
    /// <summary>
    /// Calls SysUtils.FindFirst on first call and SysUtls.FindNext in later
    /// calls. If it returns true, use the SR property to get information about
    /// the file. See the overloaded @link(FindNext) version if you need only
    /// the filename.
    /// @Returns true, if a matching file was found, false otherwise </summary>
    function FindNext: Boolean; overload;
    /// <summary>
    /// Calls FindNext until it returns false, stores all filenames in List and
    /// returns the number of files found.
    /// @param List is a TStrings object which will be filled with the filenames
    ///        of matching files, may be nil.
    /// @param IncludePath determines whether the List of filenames includes the full path or not
    /// @returns the number of matching files </summary>
    function FindAll(_List: TStrings = nil; _IncludePath: Boolean = False): Integer;
    /// <summary>
    /// Calls FindClose so FindNext will start again. Reset does not change any
    /// properties (e.g. Mask, MustHaveAttr, MayHaveAttr) </summary>
    procedure Reset;
    /// <summary>
    /// Returns the number of matches so far, that is the number of successful
    /// calls to FindNext. </summary>
    property MatchCount: Integer read FMatchCount;
    /// <summary>
    /// Returns the search mask </summary>
    property Mask: string read FMask; // write fMask;
    /// <summary>
    /// the set of attributes a file must have to be found by FindNext </summary>
    property MustHaveAttr: TFileAttributeSet read FMustHaveAttr write FMustHaveAttr;
    /// <summary>
    /// the set of allowed attributes for a file to be found by FindNext </summary>
    property MayHaveAttr: TFileAttributeSet read FMayHaveAttr write FMayHaveAttr;
    /// <summary>
    /// the search rec containing additional information about the file </summary>
    property Sr: TSearchRec read FSr;
  end;

type
  /// <summary>
  /// possible return values for the callback function </summary>
  TCopyProgressResult = (
    prContinue, // continue with the copy/move operation
    prCancel, // cancel the operation, cannot be resumed
    prStop, // stop the operation, can be resumed, if cfwRestartable was passed
    prQuiet); // continue the operation, do not call the callback

  /// <summary>
  /// reason for calling the callback function </summary>
  TCopyProgressReason = (
    prChunkFinished, // a chunk of the file has been copied
    prStreamSwitch); // started to copy a new stream (set in the first callback)

type
  /// <summary>
  /// Represents the status of a CopyFile/MoveFileWithProgress operation, passed
  /// as parameter to the callback function. </summary>
  TCopyProgressStatus = class
  protected
    FTotalFileSize: LARGE_INTEGER;
    FTotalBytesTransferred: LARGE_INTEGER;
    FStreamSize: LARGE_INTEGER;
    FStreamBytesTransferred: LARGE_INTEGER;
    FStreamNumber: LongWord;
    FCallbackReason: TCopyProgressReason;
    FSourceFile: THandle;
    FDestinationFile: THandle;
  public
    /// <summary>
    /// total size of the file </summary>
    property TotalFileSize: LARGE_INTEGER read FTotalFileSize;
    /// <summary>
    /// total bytes that have been transferred so far </summary>
    property TotalBytesTransferred: LARGE_INTEGER read FTotalBytesTransferred;
    /// <summary>
    /// size of the stream that is currently being transferred </summary>
    property StreamSize: LARGE_INTEGER read FStreamSize;
    /// <summary>
    /// bytes of the current stream taht have been transferred so far </summary>
    property StreamBytesTransferred: LARGE_INTEGER read FStreamBytesTransferred;
    /// <summary>
    /// Number of the current stream, starts with 1 (usually always 1) </summary>
    property StreamNumber: LongWord read FStreamNumber;
    /// <summary>
    /// reason for callback </summary>
    property CallbackReason: TCopyProgressReason read FCallbackReason;
    /// <summary>
    /// Handle of source file </summary>
    property SourceFile: THandle read FSourceFile;
    /// <summary>
    /// Handle of destination file </summary>
    property DestinationFile: THandle read FDestinationFile;
  end;

  ///<summary>
  /// Type for OnCopyFileProgress event
  ///  @param Status is the currenct status of the operation
  ///  @param Continue determines whether to continue copying or aborting, defaults
  ///         to prContinue </summary>
  TCopyFileProgressEvt = procedure(_Status: TCopyProgressStatus;
    var _Continue: TCopyProgressResult) of object;

  /// <summary>
  /// defines the action to take if a file already exists but has a different content </summary>
  TFileExistsAction = (feaIgnore, feaOverwrite);
  TQueryFileSyncAction = (fsaCopy, fsaSkip);
  TOnSyncing = procedure(_Sender: TObject; const _SrcDir, _DstDir: string) of object;
  TOnSyncingFile = procedure(_Sender: TObject; const _Source, _Dest: string; _Total, _Done: Int64) of object;

  ///<summary>
  /// called if a destination file already exists
  /// @param Action is the action to take, default is feaIgnore </summary>
  TOnFileExists = procedure(_Sender: TObject; const _SrcFile, _DstFile: TFileInfoRec;
    var _Action: TFileExistsAction) of object;

  ///<summary>
  /// Called instead of TOnFileExists if a destination file does not exist to allow filtering
  /// of e.g. file types.
  /// @param SyncIt must be set to false if the file should be skipped, default is true for
  ///               copying the file </summary>
  TOnQueryFileSync = procedure(_Sender: TObject; const _SrcFile: TFileInfoRec; const _DstFile: string;
    var _Action: TQueryFileSyncAction) of object;

  /// <summary>
  /// Synchronizes two directories </summary>
  TDirectorySync = class
  private
    FCurrentSource: string;
    FCurrentDest: string;
    FOnSyncingDir: TOnSyncing;
    FOnSyncingFile: TOnSyncingFile;
    FOnFileExists: TOnFileExists;
    FOnQueryFileSync: TOnQueryFileSync;
//    FOnDifferentFileExists: TOnDifferentFileExists;
//    FCheckContent: boolean;
//    procedure doOnDifferentFileExists(const _Filename: string; var _Action: TFileExistsAction);
    procedure doOnSyncingDir(const _SrcDir, _DstDir: string);
    ///<summary>
    /// Called before once before copying a file and possible several times while it is being
    /// copied to display a progress. </summary>
    procedure doOnSyncingFile(const _SrcFile, _DstFile: string; _Total, _Done: Int64);
    function doOnFileExists(const _SrcDir, _DstDir, _Filename: string): TFileExistsAction;
    function doOnQueryFileSync(const _SrcFile, _DstFile: string): TQueryFileSyncAction;
    procedure ProgressStatusCallback(_Status: TCopyProgressStatus; var _Continue: TCopyProgressResult);
  public
    /// <summary>
    /// Checks if there are files in the source directory that are already in
    /// the destination directory, for each file that exists, the OnFileExists
    /// event is called. </summary>
    procedure CheckOneWay(const _SrcDir, _DstDir: string);
    /// <summary>
    /// copies all files from DirA to DirB if they don't already exist
    /// (not implemented: if CheckContent=true, the content existing files will be checked and if
    ///                   it doesn't match, OnDifferentFileExists is called)
    /// @param FlattenDirHierarchy determines whether all files should be copied
    ///                            directly to DstDir or if subdirectories should
    ///                            be created, default is false </summary>
    procedure SyncOneWay(const _SrcDir, _DstDir: string; _FlattenDirHierarchy: Boolean = False);
    /// <summary>
    /// calls SyncOneWay(DirA, DirB) and SyncOneWay(DirB, DirA)
    /// (not implemented: if CheckContent=true, the content existing files will be checked and if
    ///                   it doesn't match, OnDifferentFileExists is called) </summary>
    procedure SyncBothWays(const _DirA, _DirB: string);
//    {! Not implemented: Called, if the content of an existing file is different }
//    property OnDifferentFileExists: TOnDifferentFileExists read FOnDifferentFileExists write FOnDifferentFileExists;
//    {! Not implemented: if true, OnDifferentFileExists will be called }
//    property CheckContent: boolean read FCheckContent write FCheckContent default false;
    /// <summary>
    /// called when a new directory is entered, to abort synchronization,
    /// raise an exception (e.g. SysUtils.Abort), and catch it in the calling method </summary>
    property OnSyncingDir: TOnSyncing read FOnSyncingDir write FOnSyncingDir;
    /// <summary>
    /// called when a file is being copied, to abort synchronization,
    /// raise an exception (e.g. SysUtils.Abort), and catch it in the calling method </summary>
    property OnSyncingFile: TOnSyncingFile read FOnSyncingFile write FOnSyncingFile;
    /// <summary>
    /// called from CheckOneWay if a destination file already exists </summary>
    property OnFileExists: TOnFileExists read FOnFileExists write FOnFileExists;
    ///<summary>
    /// Called from CheckOneWay instead of OnFileExists if the destination file does not
    /// exist. This is to allow filtering on e.g. file type. </summary>
    property OnQueryFileSync: TOnQueryFileSync read FOnQueryFileSync write FOnQueryFileSync;
  end;

  IUniqueTempDir = interface ['{D9A4A428-66AE-4BBC-B1CA-22CE4DE2FACB}']
    function Path: string;
    ///<summary> Path including trailing path delimiter </summary>
    function PathBS: string;
  end;

type
  TCopyFileFlags = (cfFailIfExists, cfForceOverwrite, cfRaiseException);
  TCopyFileFlagSet = set of TCopyFileFlags;
  TCopyFileFlagIfExists = (cfeFailIfExists, cfeOverwriteIfExists);
  TCopyFileFlagOverwriteReadonly = (cforDoNotOverwriteReadonly, cforOverwriteReadonly);
  TMoveFileExFlags = (mfCopyAllowed, {mfCreateHardlink,}mfDelayUntilReboot, mfFailIfNotTrackable,
    mfReplaceExisting, mfWriteThrough);
  TMoveFileExFlagSet = set of TMoveFileExFlags;
  TMatchingFileResult = (mfNotFound, mfDirectory, mfFile, mfSpecial);
  TCopyFileWithProgressRestartable = (cfwrNotRestartable, cfwrRestartable);
  TCopyFileWithProgressFlags = (cfwFailIfExists, cfwRestartable, cfwRaiseException);
  TCopyFileWithProgressFlagSet = set of TCopyFileWithProgressFlags;
  TCopyFileWithProgressResult = (cfwOK, cfwAborted, cfwError);
  TMoveFileWithProgressFlags = (
    mfwFailIfExists, /// < fail if the destination file already exists
    mfwAllowCopy, /// < allow using copy and delete if necessary
    mfwDelayUntilReboot, /// < wait until next reboot for moving the file
    mfwWriteThrough, /// < Setting this value guarantees that a move performed as a copy and delete operation is flushed to disk before the function returns.
    mfwFailIfNotTrackable, /// < The function fails if the source file is a link source, but the file cannot be tracked after the move.
    mfwRaiseException); /// < raise an exception if there is an error
  TMoveFileWithProgressFlagSet = set of TMoveFileWithProgressFlags;
  TCopyDirCreateIntermediate = (cdciCreateIntermediate, cdciDoNotCreateIntermediate);

const
  /// <summary>
  /// set of char constant containing all characters that are invalid in a filename </summary>
  INVALID_FILENAME_CHARS: set of AnsiChar = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];

type
  /// <summary>
  /// This class owns all utility functions as class methods so they don't pollute the name space </summary>
  TFileSystem = class
  public
    class function CheckAccessToFile(DesiredAccess: DWORD; const Filename: WideString): Boolean;
    ///<summary>
    /// wraps the windows API function GetFullPathName </summary>
    class function GetFullPathName(const _fn: string): string;
{$IFDEF SUPPORTS_STATIC}
    static;
{$ENDIF}
    /// <summary>
    /// Returns a temporary filename.
    /// @param Directory is a string with the directory to create the file in, defaults
    ///                  to the TEMP directory.
    /// @param Prefix is a string with a prefix for the filename, defaults to 'dz'.
    /// @param Unique is an word that the function converts to a hexadecimal string
    /// for use in creating the temporary filename.)
    /// <ul>
    ///   <li>If Unique is nonzero, the function appends the hexadecimal string to
    ///       <b>Prefix</b>
    ///       to form the temporary filename. In this case, the function does not create
    ///       the specified file, and does not test whether the filename is unique.</li>
    ///   <li>If Unique is zero, the function uses a hexadecimal string derived
    ///       from the current system time. In this case, the function uses different
    ///       values until it finds a unique filename, and then it creates the file
    ///       in the <b>Directory</b>.</li>
    /// </ul>
    /// @returns a filename to use for a temporary file. </summary>
    class function GetTempFileName(_Directory: string = ''; const _Prefix: string = 'dz';
      _Unique: Word = 0): string;
//    ///<summary>
//    /// Returns a temporary filename which is ensured not to already exist before but has been created
//    /// in this call.
//    /// @param Directory is a string with the directory to create the file in, defaults
//    ///                  to the TEMP directory.
//    /// @param Prefix is a string with a prefix for the filename, defaults to 'dz'.
//    /// @param Ext is the extension for the filename, defaults to '.tmp'. </summary>
//    class function GetTempFileNameEx(_Directory: string = ''; const _Prefix: string = 'dz';
//      const _Ext: string = '.tmp'): string;
    /// <summary>
    /// Calls the corresponding Windows function and returns the short path name
    /// for an *existing* file or directory. </summary>
    class function GetShortPathname(const _LongName: string): string;

    /// <summary>
    /// Creates a unique subdirectory under BaseDir with the given Prefix
    /// if Basedir is an empty string the system's %TEMP% directory is used.
    /// @returns the name of the created directory </summary>
    class function CreateUniqueDirectory(_BaseDir: string = ''; const _Prefix: string = 'dz'): string;
    class function CreateUniqueTempDir(_DeleteOnlyIfEmpty: Boolean = False;
      const _Prefix: string = 'dz'): IUniqueTempDir; overload;
    class function CreateUniqueTempDir(const _BaseDir: string; _DeleteOnlyIfEmpty: Boolean = False;
      const _Prefix: string = 'dz'): IUniqueTempDir; overload;

    /// <summary>
    /// Calls the Win32Api function GetTempPath but returns a string rather than
    /// a PChar.
    /// @returns a string with the TEMP directory </summary>
    class function GetTempPath: string;

    /// <summary>
    /// Moves the file Source to Dest using the Windows MoveFile function.
    /// NOTE: this function is deprecated, use the overloaded version!
    /// @param Source is a string containing the name of the existing file
    /// @param Dest is a string containing the destination file name
    /// @param RaiseException is a boolean which controls whether the function
    ///        retrieves the Windows error and raises an exception
    ///        if it fails. If false, it will not raise an exception
    ///        but just return false if moving the file fails.
    /// @returns true, if the file could be moved, false otherwise. </summary>
    class function MoveFile(const _Source, _Dest: string;
      _RaiseException: Boolean): Boolean; overload; deprecated;
    /// <summary>
    /// Moves the file Source to Dest using the Windows MoveFile function.
    /// @param Source is a string containing the name of the existing file
    /// @param Dest is a string containing the destination file name
    /// @param ErrorHandling is a TErrorHandlingEnum which controls whether the function
    ///        retrieves the Windows error and raises an exception
    ///        if it fails. If ehReturnFalse, it will not raise an exception
    ///        but just return false if moving the file fails.
    /// @returns true, if the file could be moved, false otherwise. </summary>
    class function MoveFile(const _Source, _Dest: string;
      _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean; overload;

    /// <summary>
    /// Moves the file Source to Dest using the Windows MoveFileEx function.
    /// NOTE: this function is deprecated, use the overloaded version!
    /// @param Source is a string containing the name of the existing file
    /// @param Dest is a string containing the destination file name
    /// @Param Flags is a set of flags corresponding to the Windows MoveEx flags
    /// @param RaiseException is a boolean which controls whether the function
    ///        retrieves the Windows error and raises an exception
    ///        if it fails. If false, it will not raise an exception
    ///        but just return false if moving the file fails.
    /// @returns true, if the file could be moved, false otherwise. </summary>
    class function MoveFileEx(const _Source, _Dest: string; _Flags: TMoveFileExFlagSet;
      _RaiseException: Boolean): Boolean; overload; deprecated;
    /// <summary>
    /// Moves the file Source to Dest using the Windows MoveFileEx function.
    /// @param Source is a string containing the name of the existing file
    /// @param Dest is a string containing the destination file name
    /// @Param Flags is a set of flags corresponding to the Windows MoveEx flags
    /// @param ErrorHandling is a TErrorHandlingEnum which controls whether the function
    ///        retrieves the Windows error and raises an exception
    ///        if it fails. If ehReturnFalse, it will not raise an exception
    ///        but just return false if moving the file fails.
    /// @returns true, if the file could be moved, false otherwise. </summary>
    class function MoveFileEx(const _Source, _Dest: string; _Flags: TMoveFileExFlagSet;
      _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean; overload;

    /// <summary>
    /// Copies the file Source to Dest using the Windows CopyFile function.
    /// NOTE: this function is deprecated, use the overloaded version!
    /// @param Source is a string containing the name of the existing file
    /// @param Dest is a string containing the destination file name
    /// @param FailIfExists is a boolean specifying whether the copy operation
    ///                     should fail if the destination file already exists.
    /// @param RaiseException is a boolean which controls whether the function
    ///                       retrieves the Windows error and raises an exception
    ///                       if it fails. If false, it will not raise an exception
    ///                       but just return false if copying the file fails.
    /// @param ForceOverwrite is a boolean which controls whether the function removes
    ///                       a read-only flag from the destination file if necessary.
    /// @returns true, if the file could be copied, false otherwise.
    /// @raises  EOSError if an error occurs and cfwRaiseException was passed </summary>
    class function CopyFile(const _Source, _Dest: string; _FailIfExists: Boolean;
      _RaiseException: Boolean = True; _ForceOverwrite: Boolean = False): Boolean; overload; deprecated;
    /// <summary>
    /// Copies the file Source to Dest using the Windows CopyFile function.
    /// @param Source is a string containing the name of the existing file
    /// @param Dest is a string containing the destination file name
    /// @param IfExists specifies whether the copy operation should fail if the destination
    ///                 file already exists. It can have the values
    ///                 * cfeFailIfExists   and
    ///                 * cfeOverwriteIfExists
    ///                 Defaults to cfeFailIfExists.
    /// @param ErrorHandling is a TErrorHandlingEnum which controls whether the function retrieves
    ///                      the Windows error and raises an exception if it fails.
    ///                      If ehReturnFalse, it will not raise an exception
    ///                      but just return false if copying the file fails.
    /// @param IfReadOnly controls whether the function removes a read-only flag from the
    ///                   destination file if necessary. It can have the values
    ///                   * cforDoNotOverwriteReadonly    and
    ///                   * cforOverwriteReadonly
    ///                   Defaults to cforDoNotOverwriteReadonly.
    /// @returns true, if the file could be copied, false otherwise.
    /// @raises  EOSError if an error occurs and cfwRaiseException was passed </summary>
    class function CopyFile(const _Source, _Dest: string;
      _IfExists: TCopyFileFlagIfExists = cfeFailIfExists;
      _ErrorHandling: TErrorHandlingEnum = ehRaiseException;
      _IfReadOnly: TCopyFileFlagOverwriteReadonly = cforDoNotOverwriteReadonly): Boolean; overload;

    /// <summary>
    /// Copies the file Source to Dest using the Windows CopyFile function.
    /// @param Source is a string containing the name of the existing file
    /// @param Dest is a string containing the destination file name
    /// @param Flags is a set of TCopyFileFlags specifying whether the copy operation
    ///        cfFailIfExists: fail if the destination file already exists.
    ///        cfForceOverwrite: remove a read-only flag from the destination file if necessary.
    ///        cfRaiseException: retrieve the Windows error and raise an exception if it fails.
    ///          If not set, it will not raise an exception but just return false if
    ///          copying the file fails.
    /// @returns true, if the file could be copied, false otherwise.
    /// @raises  EOSError if an error occurs and cfwRaiseException was passed </summary>
    class function CopyFile(const _Source, _Dest: string; _Flags: TCopyFileFlagSet): Boolean; overload;

    /// <summary>
    /// Copies the file Source to Dest using the Windows CopyFileEx function which
    /// allows for a progress callback
    /// @param Source is a string containing the name of the existing file
    /// @param Dest is a string containing the destination file name
    /// @param Flags is a set of TCopyFileWithProgressFlags specifying whether the copy operation
    ///        cfwFailIfExists: fail if the destination file already exists.
    ///        cfwRestartable: stores information in the destination file that allows
    ///          to restart a stopped copy operation
    ///        cfwRaiseException: retrieve the Windows error and raise an exception if it fails.
    ///          If not set, it will not raise an exception but just return cfwAborted
    ///          or cfwError if copying the file fails. (set by default)
    /// @returns cfeOK, if the copying succeeds, cfeAborted if the copying was aborted or
    ///          stopped in the callback function and cfeError on any other error.
    /// @raises  EOSError if an error occurs and cfwRaiseException was passed </summary>
    class function CopyFileWithProgress(const _Source, _Dest: string;
      _Progress: TCopyFileProgressEvt;
      _Flags: TCopyFileWithProgressFlagSet): TCopyFileWithProgressResult; overload;

    class function CopyFileWithProgress(const _Source, _Dest: string;
      _Progress: TCopyFileProgressEvt;
      _IfExists: TCopyFileFlagIfExists = cfeFailIfExists;
      _ErrorHandling: TErrorHandlingEnum = ehRaiseException;
      _Restartable: TCopyFileWithProgressRestartable = cfwrNotRestartable): TCopyFileWithProgressResult; overload;

    ///<summary>
    /// Copies all files that match the given Mask from SrcDir to DestDir and
    /// returns the number of files that were copied.
    /// If cfRaiseException is set in Flags, any error will raise an EOsError exception
    /// and the copying process will be aborted, otherwise errors will be silently
    /// ignored.
    /// If a destination file exists depending on the other Flag values the following
    /// happens:
    /// * If cfFailIfExists is set, the file is skipped or an exception is raised
    /// * If cfFailIfExists is not set, the file will be overwritten. If that fails
    ///   the file is skipped or an exception is raised
    /// * If cfFailIfExists is not set and cfForceOverwrite is set, the function
    ///   will also try to overwrite readonly files.
    /// if FilesSkipped is given, all skipped files will be added to that list (may be nil) </summary>
    class function CopyMatchingFiles(const _Mask, _SrcDir, _DestDir: string; _Flags: TCopyFileFlagSet;
      _FilesSkipped: TStrings = nil): Integer;

    ///<summary>
    /// Creates an emtpy (0-byte) file with the given file name </summary>
    class procedure CreateEmptyFile(const _fn: string);
    /// <summary>
    /// Copies the file Source to Dest using the Windows MoveFileWithProgress function which
    /// allows for a progress callback
    /// NOTE: If the file can be moved rather than copied, no call to the callback
    ///       function will occur!
    /// @param Source is a string containing the name of the existing file
    /// @param Dest is a string containing the destination file name
    /// @param Flags is a set of TCopyFileWithProgressFlags specifying whether the copy operation
    ///        cfwFailIfExists: fail if the destination file already exists.
    ///        cfwRestartable: stores information in the destination file that allows
    ///          to restart a stopped copy operation
    ///        cfwRaiseException: retrieve the Windows error and raise an exception if it fails.
    ///          If not set, it will not raise an exception but just return cfwAborted
    ///          or cfwError if copying the file fails. (set by default)
    /// @returns cfeOK, if the copying succeeds, cfeAborted if the copying was aborted or
    ///          stopped in the callback function and cfeError on any other error.
    /// @raises  EOSError if an error occurs and cfwRaiseException was passed </summary>
    class function MoveFileWithProgress(const _Source, _Dest: string; _Progress: TCopyFileProgressEvt;
      _Flags: TMoveFileWithProgressFlagSet = [mfwRaiseException]): TCopyFileWithProgressResult;

    /// <summary>
    /// Creates a directory (parent directories must already exist)
    /// NOTE: this function is deprecated, use the overloaded version!
    /// @param DirectoryName is the name for the new directory
    /// @param RaiseException determines whether an exception is raised on error, default = true
    /// @returns true, if the directory was created
    /// @raises EOSError if there was an error and RaiseException was true </summary>
    class function CreateDir(const _DirectoryName: string; _RaiseException: Boolean): Boolean; overload; deprecated;
    /// <summary>
    /// Creates a directory (parent directories must already exist)
    /// @param DirectoryName is the name for the new directory
    /// @param ErrorHandling is a TErrorHandlingEnum which controls whether the function retrieves
    ///                      the Windows error and raises an exception if it fails.
    ///                      If ehReturnFalse, it will not raise an exception
    ///                      but just return false if copying the file fails.
    /// @returns true, if the directory was created
    /// @raises EOSError if there was an error and RaiseException was true </summary>
    class function CreateDir(const _DirectoryName: string;
      _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean; overload;

    /// <summary>
    /// Creates a new directory, including the creation of parent directories as needed.
    /// NOTE: this function is deprecated, use the overloaded version!
    /// @param DirectoryPath is the name for the new directory
    /// @param RaiseException determines whether an exception is raised on error, default = true
    /// @returns true, if the directory was created
    /// @raises EOSError if there was an error and RaiseException was true </summary>
    class function ForceDir_old(const _DirectoryPath: string; _RaiseException: Boolean): Boolean; deprecated;
    /// <summary>
    /// Creates a directory (parent directories must already exist)
    /// @param DirectoryName is the name for the new directory
    /// @param ErrorHandling is a TErrorHandlingEnum which controls whether the function retrieves
    ///                      the Windows error and raises an exception if it fails.
    ///                      If ehReturnFalse, it will not raise an exception
    ///                      but just return false if copying the file fails.
    /// @returns true, if the directory was created
    /// @raises EOSError if there was an error and RaiseException was true </summary>
    class function ForceDir(const _DirectoryPath: string;
      _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean;

    ///<summary> See also CopyMatchingFiles </summary>
    class function CopyDir_Old(const _SrcDir, _DestDir: string; _CreateIntermediate: Boolean;
      _RaiseException: Boolean): Boolean; deprecated;
    class function CopyDir(const _SrcDir, _DestDir: string;
      _CreateIntermediate: TCopyDirCreateIntermediate = cdciCreateIntermediate;
      _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean;

    /// <summary>
    /// Sets a file's readonly flag
    /// @param Filename is the file to change
    /// @param Set determines whether to set or clear the flag
    /// @returns true, if the readonly flag has been changed
    /// @raises EOSError if there was an error and RaiseException was true </summary>
    class function SetReadonly(const _Filename: string; _Value: Boolean;
      _RaiseException: Boolean): Boolean; overload; deprecated;
    /// <summary>
    /// Sets a file's readonly flag
    /// @param Filename is the file to change
    /// @param Set determines whether to set or clear the flag
    /// @param ErrorHandling is a TErrorHandlingEnum which controls whether the function retrieves
    ///                      the Windows error and raises an exception if it fails.
    ///                      If ehReturnFalse, it will not raise an exception
    ///                      but just return false if copying the file fails.
    /// @returns true, if the readonly flag has been changed
    /// @raises EOSError if there was an error and RaiseException was true </summary>
    class function SetReadonly(const _Filename: string; _Value: Boolean;
      _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean; overload;

    /// <summary>
    /// Deletes the file using the SysUtils.DeleteFile function.
    /// @param Filename is a string containing the name of the file
    /// @param RaiseException is a boolean which controls whether the function
    ///        retrieves the Windows error and raises an exception
    ///        if it fails. If false, it will not raise an exception
    ///        but just return false if deleting the file fails.
    /// @param Force is a boolean which controls whether this function will try to delete
    ///        readonly files, If true, it will use SetFileAttr to reset the
    ///        readonly attribut and try to delete the file again.
    /// @returns true, if the file could be deleted, false otherwise.
    /// @raises EOSError if there was an error and RaiseException was true </summary>
    class function DeleteFile(const _Filename: string; _RaiseException: Boolean = True;
      _Force: Boolean = False): Boolean;

    /// <summary>
    /// Deletes all files in a directory matching a given filemask (non-recursive)
    /// @param Dir is a string containting the directory in which the files are to be
    ///            deleted, must NOT be empty
    /// @param Mask is a string containting the file search mask, all files matching
    ///             this mask will be deleted
    /// @param RaiseException is a boolean which controls whether the function
    ///                       retrieves the Windows error and raises an exception
    ///                       if it fails. If false, it will not raise an exception
    ///                       but just return false if moving the file fails.
    /// @param Force is a boolean which controls whether this function will try to delete
    ///              readonly files, If true, it will use SetFileAttr to reset the
    ///              readonly attribut and try to delete the file again.
    /// @param ExceptMask is a string contaning a mask for files not to delete even if they
    ///                   match the Mask, defaults to an empty string meaning no exceptions.
    ///                   The comparison is case insensitive.
    /// @returns the number of files that could *not* be deleted.
    /// @raises EOSError if there was an error and RaiseException was true </summary>
    class function DeleteMatchingFiles(const _Dir, _Mask: string;
      _RaiseException: Boolean = True; _Force: Boolean = False): Integer; overload;
    class function DeleteMatchingFiles(const _Dir, _Mask: string; const _ExceptMask: string = '';
      _RaiseException: Boolean = True; _Force: Boolean = False): Integer; overload; deprecated;
    class function DeleteMatchingFiles(const _Dir, _Mask: string; const _ExceptMasks: array of string;
      _RaiseException: Boolean = True; _Force: Boolean = False): Integer; overload;

    ///<summary>
    /// Gets a list of directories matching the given mask.
    /// @param Mask is the name mask to match, note that it can contain wildcards only in the
    ///             last part of the name (e.g. 'c:\grmpf*\trallala' will NOT work)
    /// @param sl is a TStrings to which all matching directories will be added (it will not be cleared)
    ///           it can be NIL if the caller only wants the count, not the actual list.
    /// @param IncludePath determines whether the list should contain only the directory names or
    ///                    the full paths.
    /// @returns the number of matching directories </summary>
    class function FindMatchingDirs(const _Mask: string; _sl: TStrings; _IncludePath: Boolean = False): Integer;

    ///<summary>
    /// Finds the first directory matching the mask.
    /// @param Mask is the name mask to match, note that it can contain wildcards only in the
    ///             last part of the name (e.g. 'c:\grmpf*\trallala' will NOT work)
    /// @param Dir is a String to which the matching directory will be written.
    /// @returns true, if a matching directory was found. </summary>
    class function FindMatchingDir(const _Mask: string; out _Dir: string): Boolean;

    ///<summary>
    /// Gets a list of files matching the given mask. Only regular files will be found, no
    /// directories, no hidden files, no readonly files (files with faArchive will be found).
    /// See TSimpleDirEnumerator if you need more flexibility.
    /// @param Mask is the name mask to match, note that it can contain wildcards only in the
    ///             last part of the name (e.g. 'c:\grmpf*\trallala' will NOT work)
    /// @param sl is a TStrings to which all matching files will be added (it will not be cleared)
    ///           it can be NIL if the caller only wants the count, not the actual list.
    /// @param IncludePath determines whether the list should contain only the file names or
    ///                    the full paths.
    /// @returns the number of matching files </summary>
    class function FindMatchingFiles(const _Mask: string; _sl: TStrings; _IncludePath: Boolean = False;
      _Sort: Boolean = True): Integer; overload;
    class function FindMatchingFiles(const _Mask: string;
      _IncludePath: Boolean = False; _Sort: Boolean = True): TStringArray; overload;

    /// <summary>
    /// tries to find a matching file
    /// @param Mask is the filename mask to match
    /// @param Filename is the name of the file which has been found, only valid if result <> mfNotFound
    /// @returns mfNotFound, if no file was found, or mfDirectory, mfFile or mfSpecial
    ///          describing the type of the file which has been found.
    /// @NOTE: If there are multiple matches, the file name returned is not deterministic.
    ///        On an NTFS volume it is the last one in the NTFS sort order but that's not guaranteed. </summary>
    class function FindMatchingFile(const _Mask: string; out _Filename: string): TMatchingFileResult; overload;
    class function FindMatchingFile(const _Mask: string): TMatchingFileResult; overload;

    ///<summary>
    /// Returns true, if the given file exists. Note that wildcards are not supported! If you
    /// need wildcards, use FindMatchingFile.
    /// @param RaiseException determines whether an exception should be raised if the file does not exist
    /// @raises EFileNotFound if the file does not exist and RaiseException is true </summary>
    class function FileExists(const _Filename: string): Boolean; overload;
    class function FileExists(const _Filename: string; _RaiseException: Boolean): Boolean; overload; deprecated; // use AssertFileExists instead
    ///<summary>
    /// Checks if the given file exists. Note that wildcards are not supported! If you
    /// need wildcards, use AssertMatchingFileExists.
    /// @raises EFileNotFound if the file does not exist. </summary>
    class procedure AssertFileExists(const _Filename: string);
    class procedure AssertMatchingFileExists(const _Mask: string);

    ///<summary>
    /// @param RaiseException determines whether an exception should be raised if the directory does not exist
    /// @raises EDirNotFound if the directory does not exist and RaiseException is true </summary>
    class function DirExists(const _DirName: string): Boolean; overload;
    class function DirExists(const _DirName: string; _RaiseException: Boolean): Boolean; overload; deprecated; // use AssertDirExists instead
    ///<summary>
    /// Checks if the given directory exists.
    /// @raises EDirNotFound if the directory does not exist. </summary>
    class procedure AssertDirExists(const _DirName: string);

    ///<summary>
    /// @param DirNames is a TStrings object containing a list of directory names to check.
    /// @returns true, if all directories in the list exist, false otherwise
    /// @NOTE: Passing an empty list will return True. </summary>
    class function AllDirsExist(_DirNames: TStrings): Boolean;

    /// <summary>
    /// Deletes an empty directory using the SysUtils function RemoveDir
    /// The function will fail if the directory is not empty.
    /// @param DirName is the name of the directory to delete
    /// @param RaiseException is a boolean which controls whether the function
    ///                       retrieves the Windows error and raises an exception
    ///                       if it fails. If false, it will not raise an exception
    ///                       but just return false if deleting the directory fails.
    /// @param Force is a boolean which controls whether this function will try to delete
    ///              readonly directories, If true, it will use SetFileAttr to reset the
    ///              readonly attribut and try to delete the directory again.
    /// @returns True, if the directory could be deleted, false otherwise.
    /// @raises EOSError if there was an error and RaiseException was true </summary>
    class function RemoveDir(const _DirName: string; _RaiseException: Boolean = True;
      _Force: Boolean = False): Boolean;

    /// <summary>
    /// function is deprecated, use DelDirTree instead!
    /// Note the different order of parameters of the new function! </summary>
    class function DelTree(const _DirName: string; _Force: Boolean = False;
      _RaiseException: Boolean = True): Boolean; deprecated;
    /// <summary>
    /// Deletes a directory with all files and subdirectories.
    /// Note: This new function has a different order of parameters than
    ///       the old DelTree function.
    /// @param DirName is the name of the directory to delete
    /// @param RaiseExceptin is a boolean which controls whether the function
    ///                      retrieves the Windows error and raises an exception
    ///                      if it fails. If false, it will not raise an exception
    ///                      but just return false if deleting the directory fails.
    /// @param Force specifies whether it should also delete readonly files
    /// @returns True, if the directory could be deleted, false otherwise.
    /// @raises EOSError if there was an error and RaiseException was true </summary>
    class function DelDirTree(const _DirName: string; _RaiseException: Boolean = True;
      _Force: Boolean = False): Boolean;

    /// <summary>
    /// reads a text file and returns its content as a string
    /// @param Filename is the name of the file to read
    /// @returns the file's content as a string </summary>
    class function ReadTextFile(const _Filename: string): string;

    ///<summary>
    /// Expands a relative FileName relative to the given BaseDir. </summary>
    class function ExpandFileNameRelBaseDir(const _Filename, _BaseDir: string): string;

    ///<summary>
    /// checks if a filename is relative (meaning: Not an absolute path)
    /// (This calls shlwapi.PathIsRelativeA/W.)
    /// @returns true, if the filename is relative </summary>
    class function PathIsRelative(const _Filename: string): Boolean;

    /// <summary>
    /// checks whether the given string is a valid filename (without path), that is
    /// does not contain one of the characters defined in INVALID_FILENAME_CHARS
    /// @param s is the string to check
    /// @param AllowDot determines whether a dot ('.') is allowed in the filename
    ///        the default is true, but you might not want that
    /// @param AllowPathChars determines whether the name may contain '\' characters and a single
    ///                       ':' as the second character, so a full path can be checked.
    /// @returns true, if the string is a valid filename, false otherwise </summary>
    class function IsValidFilename(const _s: string; _AllowDot: Boolean = True;
      _AllowPathChars: Boolean = False): Boolean; overload;
    /// <summary>
    /// checks whether the given string is a valid filename (without path), that is
    /// does not contain one of the characters defined in INVALID_FILENAME_CHARS and
    /// returns the first error position.
    /// @param s is the string to check
    /// @param ErrPos is the first error position, only valid it result = false
    /// @param AllowDot determines whether a dot ('.') is allowed in the filename
    ///        the default is true, but you might not want that
    /// @param AllowPathChars determines whether the name may contain '\' characters and a single
    ///                       ':' as the second character, so a full path can be checked.
    /// @returns true, if the string is a valid filename, false otherwise </summary>
    class function IsValidFilename(const _s: string; out _ErrPos: Integer; _AllowDot: Boolean = True;
      _AllowPathChars: Boolean = False): Boolean; overload;
    ///<summary>
    /// Replaces all invalid characters in the file name with the given character </summary>
    /// @param S is the input filename
    /// @param ReplaceChar is the character to use for replacing in valid characters
    ///                    defaults to '_'
    /// @param AllowPathChars determines whether the name may contain '\' characters and a single
    ///                       ':' as the second character, so a full path can be converted.
    /// @returns a valid filename </summary>
    class function MakeValidFilename(const _s: string; _ReplaceChar: Char = '_'; _AllowPathChars: Boolean = True): string;

    class function ContainsWildcard(const _Mask: string): Boolean;

    /// <summary>
    /// see also IsFileWritable
    /// @Returns true if the file exists and its readonly flag is set </summary>
    class function IsFileReadonly(const _Filename: string): Boolean;
    /// <summary>
    /// @Returns true if the file exists and is writable </summary>
    class function IsFileWritable(const _Filename: string): Boolean;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
    /// <summary>
    /// creates a backup of the file appending the current date and time to the base
    /// file name. If the copy operation fails, an underscore and a number will be appended
    /// to the filename. That number will be incremented, until the copy operation succeeds
    /// or it reaches 999.
    /// The original file will remain unchanged.
    /// See also TFileGenerationHandler and GenerateBackupFilename.
    /// @param Filename is the name of the file to back up
    /// @param BackupDir is a directory in which to create the backup file, if empty
    ///                  the same directory as the original file is used
    /// @returns the full filename of the created backup file
    /// @raises EBackupFailed if the copy operation failed even for the 1000th attempt. </summary>
    class function BackupFile(const _Filename: string; const _BackupDir: string = ''): string;
    /// <summary>
    /// Moves the file to a new, unique name, appending the current date and time to the base
    /// file name. If the move operation fails, an underscore and a number will be appended
    /// to the filename. That number will be incremented, until the copy operation succeeds
    /// or it reaches 999.
    /// See also TFileGenerationHandler and GenerateBackupFilename.
    /// @param Filename is the name of the file to move
    /// @param BackupDir is a directory in which to create the backup file, if empty
    ///                  the same directory as the original file is used
    /// @returns the full filename the file has been moved to.
    /// @raises EBackupFailed if the copy operation failed even for the 1000th attempt. </summary>
    class function MoveFileToBackup(const _Filename: string; const _BackupDir: string = ''): string;
{$ENDIF}
    ///<summary>
    /// Generates a backup of the filename by appending the current date and time to the base
    /// @param Filename is the name of the file to back up
    /// @param BackupDir is a directory in which to create the backup file, if empty
    ///                  the same directory as the original file is used
    /// @returns the full filename for the backup file </summary>
    class function GenerateBackupFilename(const _Filename: string; _BackupDir: string = ''): string;
{$IFDEF SUPPORTS_STATIC}
    static;
{$ENDIF}

    ///<summary>
    /// Uses kernel32.GetFullPathName and then compares the result using SameText.
    /// Note that this does not handle all possible cases, e.g. two files with totally different
    /// names could be the same due to hard links, soft links or multiple UNC paths pointing to
    // the same directory. </summary>
    class function IsSameFilename(const _fn1, _fn2: string): Boolean;

    ///<summary>
    /// Appends date and optionally time to the base filename using the format _YYYY-MM-DD_HH-MM-SS
    /// e.g. c:\test.txt -> c:\test_1966-05-05.txt or c:\test_1966-05-05_08-15-00.txt
    /// @param Filename is the filename to append to
    /// @param DateTime is a TDateTime value to append
    /// @param IncludeTime determines whether to append the date only or date and time
    /// see also AppendDate and AppendDateAndTime </summary>
    class function AppendDateTime(const _Filename: string; _DateTime: TDateTime; _IncludeTime: Boolean): string; overload;
{$IFDEF SUPPORTS_STATIC}
    static;
{$ENDIF}

    ///<summary>
    /// Appends the current date to the filename in the format _YYYY-MM-DD </summary>
    class function AppendDate(const _Filename: string): string; overload;
{$IFDEF SUPPORTS_STATIC}
    static;
{$ENDIF}

    ///<summary>
    /// Appends the given date to the filename in the format _YYYY-MM-DD </summary>
    class function AppendDate(const _Filename: string; _Date: TDateTime): string; overload;
{$IFDEF SUPPORTS_STATIC}
    static;
{$ENDIF}

    ///<summary>
    /// Appends the current date and time to the filename in the format _YYYY-MM-DD_HH-MM-SS </summary>
    class function AppendDateAndTime(const _Filename: string): string; overload;
{$IFDEF SUPPORTS_STATIC}
    static;
{$ENDIF}

    ///<summary>
    /// Appends the given date and time to the filename in the format _YYYY-MM-DD_HH-MM-SS </summary>
    class function AppendDateAndTime(const _Filename: string; _DateTime: TDateTime): string; overload;
{$IFDEF SUPPORTS_STATIC}
    static;
{$ENDIF}

    /// <summary>
    /// @returns a TFileInfoRec containing the filename, filesize and last access
    ///          timestamp of the file </summary>
    class function GetFileInfo(const _Filename: string): TFileInfoRec;
    /// <summary>
    /// Tries to get the file information containing filename, filesize
    /// and last access timestamp of the file.
    /// @param Info will contain these values, only valid if result = true </summary>
    class function TryGetFileInfo(const _Filename: string; out _Info: TFileInfoRec): Boolean;
    class function TryGetFileSize(const _Filename: string; out _Size: Int64): Boolean;
    class function GetFileSize(const _Filename: string): Int64;

    /// <summary>
    /// Returns the free space (in bytes) on the disk with the given drive letter </summary>
    class function DiskFree(_DriveLetter: AnsiChar): Int64; overload;
    /// <summary>
    /// Returns the free space (in bytes) on the disk on which the given directory is located </summary>
    class function DiskFree(const _Directory: string): Int64; overload;
    class function GetVolumeName(_DriveLetter: Char): string;
    class function GetRemoteVolumeName(const _Share: string): string;
    class procedure GetLocalVolumeNames(_sl: TStrings; _HdOnly: Boolean = False; _IgnoreEmpty: Boolean = True);

    ///<summary>
    /// changes the "full" file extension where "full" means it handles multiple
    /// extensions like .doc.exe </summary>
    class function ChangeFileExtFull(const _Filename, _NewExt: string): string;

    ///<summary>
    /// Like SysUtils.ChangeFileExt, only changes the part after the last '.' in the
    /// filename. </summary>
    class function ChangeFileExtLast(const _Filename, _NewExt: string): string;

    ///<summary>
    /// extracts the "full" file extension where "full" means it handles multiple
    /// extensions like .doc.exe
    /// @param IncludeDot determines whether to include the leading dot </summary>
    class function ExtractFileExtFull(const _Filename: string; _IncludeDot: Boolean = True): string;

    ///<summary>
    /// Like SysUtils.ExtractFileExt, only changes the part after the last '.' in the
    /// filename.
    /// @param IncludeDot determines whether to include the leading dot </summary>
    class function ExtractFileExtLast(const _Filename: string; _IncludeDot: Boolean = True): string;

    ///<summary>
    /// removes the "full" file extension where "full" means it handles multiple
    /// extensions like .doc.exe </summary>
    class function RemoveFileExtFull(const _Filename: string): string;

    ///<summary>
    /// Short for ChangeFileExtLast(_Filename, '') </summary>
    class function RemoveFileExtLast(const _Filename: string): string;

    ///<summary>
    /// @param Ext is the full file extension to check for, including the leading dot.
    /// @returns True, if the full extension of Filename matches the given extension.
    ///                Comparison is case insensitive
    ///          False if the extensions don't machch. </summary>
    class function HasFileExtFull(const _Filename: string; const _Ext: string): Boolean;
    ///<summary>
    /// @param Ext is the last file extension to check for, including the leading dot.
    /// @returns True, if the last extension of Filename matches the given extension.
    ///                Comparison is case insensitive
    ///          False if the extensions don't machch. </summary>
    class function HasFileExtLast(const _Filename: string; const _Ext: string): Boolean;

    ///<summary>
    /// Sets the file's date and time to the given time or to the current time
    /// @param Fileanme is the name of the file to touch
    /// @param lpSystemTime is a pointer to a TSystemTime record givng the time,
    ///                     Defaults to NIL which uses the current system time.
    /// @raises EFileNotFound if the file does not exist
    /// @raises EOsError if there calling the Windows API fails </summary>
    class procedure TouchFileTimes(const _Filename: string; lpSystemTime: PSystemTime = nil);
  end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
type
  TFilename = record
  private
    FFull: string;
  public
    procedure Init(const _Full: string);
    property Full: string read FFull;
    ///<summary>
    /// Returns the drive of the file name.
    /// For file names with drive letters, the result is in the form "<drive>:".
    /// For file names with a UNC path the result is in the form "'\\<servername>\<sharename>'".
    // If the path contains neither style of path prefix, the result is an empty string. </summary>
    function Drive: string;
    ///<summary> Returns the directory excluding the last backslash, unless the path is the
    ///          root directory of a drive. </summary>
    function Directory: string;
    ///<summary> Returns the directory including the last backslash. </summary>
    function DirectoryBS: string;
    ///<summary> Returns the filename without directory and without extension(s).
    ///          Note that this treats everything after the first '.' as extension(s). </summary>
    function FilenameOnly: string;
    ///<summary> Returns the filename + extension(s) </summary>
    function Filename: string;
    ///<summary>
    /// returns an array of extensions, all including the leading '.' </summary>
    function GetExtensions: TStringArray;
    ///<symmary> Returns the extension(s) only.
    ///          Note that this treats everything after the first '.' as extension(s). </summary>
    function Extension: string;
    ///<symmary> Returns the (last) extension only.
    ///          Note that this treats only the text after the last '.' as extension(s). </summary>
    function LastExtension: string;
    ///<summary>
    /// @Returns the filename including the full path but without extension(s).
    /// @NOTE: This treats everything after the first '.' as extension(s) </summary>
    function BaseName: string;
    ///<summary>
    /// replaces the drive part of the path with the given NewDrive. </summary>
    procedure ReplaceDrive(const _NewDrive: string);
    ///<summary>
    /// Replaces the directory part (including the drive) with the given NewDir </summary>
    procedure ReplaceDirectory(const _NewDir: string);
    ///<summary>
    /// Replaces filename and extension(s) with the given filename </summary>
    procedure ReplaceFilename(const _Filename: string);
    ///<summary>
    /// Replaces filename, but not extension(s) with the given filename </summary>
    procedure ReplaceFilenameOnly(const _FilenameOnly: string);
    ///<summary>
    /// Replaces the full extension with the given one.
    /// @param Extension is the new extension including the leading dot e.g. '.txt' </summary>
    procedure ReplaceExtension(const _Extension: string);
    ///<summary>
    /// Replaces the whole extension with the given array of extensions
    /// Example: ['.bla', '.blub', '.tmp'] will change the extension to '.bla.blub.tmp' </summary>
    procedure ReplaceExtensions(const _Extensions: TStringArray);
    ///<summary>
    /// Replaces the last extension of the filename with the given one
    /// @param Extension is the new extension including the leading dot e.g. '.txt'
    /// Example:
    /// Given the file name 'c:\bla\file.bla.txt'
    /// ReplaceLastExtension('.doc') will change the file name to 'c:\bla\file.bla.doc' </summary>
    procedure ReplaceLastExtension(const _Extension: string);
    ///<summary>
    /// @returns true, if the filename contains either a drive letter or is a UNC, false otherwise
    /// @param Root either contains '<drive>:\' or '\\<server>\<share>' if the result is true. </summary>
    function TryGetRootDir(out _Root: string): Boolean;
    ///<summary>
    /// @Returns the first Depth parts of the filename as separated by PathDelim characters, without
    ///          the trailing path delimiter.
    /// example: Full = 'c:\test\trallala'
    ///          Parts(>=3) = 'c:\test\trallala'
    ///          Parts(2) = 'c:\test'
    ///          Parts(1) = 'c:'
    ///          Parts(0) = ''
    ///          Parts(-1) = 'c:\test'
    ///          Parts(-2) = 'c:'
    ///          Parts(<=-3) = ''
    /// Note that every call will parse the filename again, so you should buffer this
    /// value if you need it multiple times. If you need all parts, consider using Split </summary>
    function Parts(_Depth: Integer): string;
    ///<summary>
    /// Returns the number of parts separated by PathDelim characters.
    /// Note that every call will calculate Depth again, so you should buffer this
    /// value if you need it multpile times. </summary>
    function Depth: Integer;
    ///<summary>
    /// Combines the functionality of Parts and Depth by returning all Parts in sl and the Depth
    /// as function result </summary>
    function GetParts(_sl: TStrings): Integer; deprecated; // use Split instead
    ///<summary>
    /// Splits all parts of the file name in a TStringArray
    /// @returns the number of parts </summary>
    function Split: TStringArray;
    ///<summary>
    /// @returns True, if the last extension matches the given one (case insensitively)
    ///          Fales otherwise </summary>
    function HasExtensionLast(const _Ext: string): Boolean;
    ///<summary>
    /// @returns True, if the full extension matches the given one (case insensitively)
    ///          Fales otherwise </summary>
    function HasExtensionFull(const _Ext: string): Boolean;
    class function Combine(_Parts: TStringArray): TFilename; static;
    ///<summary>
    /// Same as Init </summary>
    class operator Implicit(const _s: string): TFilename;
    ///<summary>
    /// Same as Full </summary>
    class operator Implicit(_a: TFilename): string;
    class operator Add(const _a: TFilename; const _b: string): string;
    class operator Add(const _a: string; const _b: TFilename): string;
  end;

type
  // TFilename might not be this record, because there are multiple declarations of this type
  // in the RTL and other libraries, so it depends on the units included and their order which
  // one is used. TdzFilename will always be this record.
  TdzFilename = TFilename;

type
  TSearchPath = record
  private
    FValue: string;
    function AsStringlist: TStringList;
  public
    function Value: string;
    ///<summary>
    /// searches the file in the path
    /// @param FilenameOnly is the file name without a directory
    /// @param is the full file name if the file, only valid it result = true
    /// @returns true, if the file has been found, false otherwise </summary>
    function TryFindFile(const _FilenameOnly: string; out _FoundFile: TFilename): Boolean;
    ///<summary>
    /// Calls TryFindFile and raises EFileNotFound if that call returns false </summary>
    function FindFile(const _FilenameOnly: string): TFilename;
    function AnyDirExists: Boolean;
    function AllDirsExist(out _FirstNonExistingDir: string): Boolean;
    procedure Init(const _s: string);
    function PartCount: Integer;
    function Part(_Idx: Integer): string;
    procedure GetParts(_Parts: TStrings); overload;
    function GetParts: TStringArray; overload;
    procedure AssignParts(_Parts: TStrings);
    class operator Implicit(_sl: TStrings): TSearchPath;
    class operator Implicit(const _s: string): TSearchPath;
    class operator Implicit(_a: TSearchPath): string;
    class operator Equal(_a, _b: TSearchPath): Boolean;
    class operator NotEqual(_a, _b: TSearchPath): Boolean;
    class operator Add(_a, _b: TSearchPath): TSearchPath;
    class operator Add(_a: TSearchPath; const _b: string): TSearchPath;
  end;
{$ENDIF}

type
  /// <summary>
  /// callback event for generating a filename for the given generation </summary>
  TOnGenerateFilename = procedure(_Sender: TObject; _Generation: Integer; var _Filename: string) of object;

type
  /// <summary>
  /// This class handles keeping generations of files, e.g. log files. The default
  /// is to keep 10 generations </summary>
  TFileGenerationHandler = class
  private
    FBaseName: string;
    FSuffix: string;
    FOnGenerateFilename: TOnGenerateFilename;
    FMaxGenerations: Integer;
    FResultContainsNumber: Boolean;
    FOldestIsHighest: Boolean;
    FPrependZeros: Integer;
    function GenerateFilename(_Generation: Integer): string;
  public
    /// <summary>
    /// @param BaseName is the base filename to which by default _<n> followed by
    ///                 the Suffix will be appended
    /// @param Suffix is the suffix for the filename, usually an extension which
    ///               must include the dot (.), but it is also possible to pass
    ///               an arbitrary string like '_backup'. </summary>
    constructor Create(const _BaseName, _Suffix: string);
    /// <summary>
    /// generates the filename and returns it </summary>
    function Execute(_KeepOriginal: Boolean): string;
    /// <summary>
    /// the maximum of file generations that should be kept </summary>
    property MaxGenerations: Integer read FMaxGenerations write FMaxGenerations default 5;
    /// <summary>
    /// should the resulting filename contain a number? </summary>
    property ResultContainsNumber: Boolean read FResultContainsNumber write FResultContainsNumber default False;
    /// <summary>
    /// does the oldest file have the highest number? </summary>
    property OldestIsHighest: Boolean read FOldestIsHighest write FOldestIsHighest default True;
    property PrependZeros: Integer read FPrependZeros write FPrependZeros default 0;
    /// <summary>
    /// allows read access to the file's base name as passed to the constructor </summary>
    property BaseName: string read FBaseName;
    property Suffix: string read FSuffix;
    /// <summary>
    /// callback event for generating a filename for the given generation </summary>
    property OnGenerateFilename: TOnGenerateFilename read FOnGenerateFilename write FOnGenerateFilename;
  end;

/// <summary>
/// This is an abbreviation for IncludeTrailingPathDelimiter </summary>
function itpd(const _DirName: string): string;

///<summary>
/// This is an abbreviation for ExcludeTrailingPathDelimiter </summary>
function etpd(const _DirName: string): string;

///<summary>
/// Assign a filename and open the file.
/// if Rewrite fails, File is zeroed
/// @raise exception, if  rewrite fails </summary>
procedure TextFile_AssignAndRewrite(var _File: TextFile; const _Filename: string);
function TextFile_IsOpen(const _File: TextFile): Boolean;
procedure TextFile_Close(var _File: TextFile);

implementation

uses
  StrUtils,
  Masks,
  u_dzMiscUtils,
  u_dzStringUtils,
  u_dzDateUtils,
  u_dzStringArrayUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

function itpd(const _DirName: string): string;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := IncludeTrailingPathDelimiter(_DirName);
end;

function etpd(const _DirName: string): string;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := ExcludeTrailingPathDelimiter(_DirName);
end;

{ TSimpleDirEnumerator }

constructor TSimpleDirEnumerator.Create(const _Mask: string;
  _MayHaveAttr: TFileAttributeSet = ALL_FILES_ATTRIB_SET);
begin
  FMask := _Mask;
  FMustHaveAttr := [];
  FMayHaveAttr := _MayHaveAttr;
end;

constructor TSimpleDirEnumerator.CreateForDirsOnly(const _Mask: string);
begin
  Create(_Mask, [dfaDirectory, dfaArchive]);
  FMustHaveAttr := [dfaDirectory];
end;
(*
constructor TSimpleDirEnumerator.CreateForFilesOnly(const _Mask: string);
begin
  Create(_Mask, [dfaArchive]);
end;
*)
destructor TSimpleDirEnumerator.Destroy;
begin
  Reset;
  inherited;
end;

class function TSimpleDirEnumerator.EnumDirsOnly(const _Mask: string; _List: TStrings;
  _IncludePath, _Sort: Boolean): Integer;
var
  enum: TSimpleDirEnumerator;
  List: TStringList;
begin
  enum := TSimpleDirEnumerator.CreateForDirsOnly(_Mask);
  try
    List := TStringList.Create;
    try
      Result := enum.FindAll(List, _IncludePath);
      if _Sort then
        List.Sort;
      _List.AddStrings(List);
    finally
      FreeAndNil(List);
    end;
  finally
    FreeAndNil(enum);
  end;
end;

class function TSimpleDirEnumerator.EnumDirsOnly(const _Mask: string; _IncludePath, _Sort: Boolean): TStringArray;
var
  dirs: TStringList;
begin
  dirs := TStringList.Create;
  try
    EnumDirsOnly(_Mask, dirs, _IncludePath, _Sort);
    Result := TStringArray_FromStrings(dirs);
  finally
    FreeAndNil(dirs);
  end;
end;

class procedure TSimpleDirEnumerator.EnumFilesOnly(const _Mask: string; _Callback: TOnFileEnumCallback;
  _IncludePath: Boolean = False; _Sort: Boolean = True);
var
  enum: TSimpleDirEnumerator;
  List: TStringList;
  i: Integer;
begin
  enum := TSimpleDirEnumerator.Create(_Mask, [dfaArchive]);
  try
    List := TStringList.Create;
    try
      enum.FindAll(List, _IncludePath);
      if _Sort then
        List.Sort;
      for i := 0 to List.Count - 1 do
        _Callback(enum, List[i]);
    finally
      FreeAndNil(List);
    end;
  finally
    FreeAndNil(enum);
  end;
end;

class function TSimpleDirEnumerator.EnumFilesOnly(const _Mask: string; _IncludePath,
  _Sort: Boolean): TStringArray;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    EnumFilesOnly(_Mask, sl, _IncludePath, _Sort);
    Result := TStringArray_FromStrings(sl);
  finally
    FreeAndNil(sl);
  end;
end;

class function TSimpleDirEnumerator.EnumFilesOnly(const _Mask: string; _List: TStrings;
  _IncludePath, _Sort: Boolean): Integer;
begin
  Result := Execute(_Mask, _List, [dfaArchive], _IncludePath, _Sort);
end;

class function TSimpleDirEnumerator.Execute(const _Mask: string; _List: TStrings;
  _MayHaveAttr: TFileAttributeSet = ALL_FILES_ATTRIB_SET;
  _IncludePath: Boolean = False; _Sort: Boolean = True): Integer;
var
  enum: TSimpleDirEnumerator;
  List: TStringList;
begin
  enum := TSimpleDirEnumerator.Create(_Mask, _MayHaveAttr);
  try
    List := TStringList.Create;
    try
      Result := enum.FindAll(List, _IncludePath);
      if Assigned(_List) then begin
        if _Sort then
          List.Sort;
        _List.AddStrings(List);
      end;
    finally
      FreeAndNil(List);
    end;
  finally
    FreeAndNil(enum);
  end;
end;

function TSimpleDirEnumerator.FindAll(_List: TStrings = nil; _IncludePath: Boolean = False): Integer;
var
  s: string;
  Path: string;
begin
  if _IncludePath then
    Path := ExtractFilePath(FMask)
  else
    Path := '';
  Result := 0;
  while FindNext(s) do begin
    Inc(Result);
    if Assigned(_List) then
      _List.Add(Path + s);
  end;
end;

function TSimpleDirEnumerator.FindNext(out _Filename: string; _IncludePath: Boolean = False): Boolean;
var
  Res: Integer;
  Attr: Integer;

  function AttrOk(_EnumAttr: TFileAttributes; _SysAttr: Integer): Boolean;
  begin
    Result := True;
    if _EnumAttr in FMustHaveAttr then
      if (Attr and _SysAttr) = 0 then
        Result := False;
  end;

  procedure CondAddAttr(_EnumAttr: TFileAttributes; _SysAttr: Integer);
  begin
    if _EnumAttr in FMayHaveAttr then
      Attr := Attr + _SysAttr;
  end;

var
  Path: string;
begin
  Path := ExtractFilePath(FMask);
  repeat
    if not FActive then begin
      FMatchCount := 0;
      Attr := 0;
      CondAddAttr(dfaReadonly, SysUtils.faReadOnly);
      CondAddAttr(dfaHidden, SysUtils.faHidden);
      CondAddAttr(dfaSysFile, SysUtils.faSysFile);
      CondAddAttr(dfaDirectory, SysUtils.faDirectory);
      CondAddAttr(dfaArchive, SysUtils.faArchive);
      Res := FindFirst(FMask, Attr, FSr);
      Result := (Res = 0);
      if Result then
        FActive := True;
    end else begin
      Res := SysUtils.FindNext(FSr);
      Result := (Res = 0);
    end;
    if not Result then
      Exit;
    if (Sr.Name = '.') or (Sr.Name = '..') then
      Continue;
    if FMustHaveAttr <> [] then begin
      Attr := FSr.Attr;
      if not AttrOk(dfaReadonly, SysUtils.faReadOnly) then
        Continue;
      if not AttrOk(dfaHidden, SysUtils.faHidden) then
        Continue;
      if not AttrOk(dfaSysFile, SysUtils.faSysFile) then
        Continue;
      if not AttrOk(dfaDirectory, SysUtils.faDirectory) then
        Continue;
      if not AttrOk(dfaArchive, SysUtils.faArchive) then
        Continue;
    end;
    Inc(FMatchCount);
    if _IncludePath then
      _Filename := Path + Sr.Name
    else
      _Filename := Sr.Name;
    Exit;
  until False;
end;

function TSimpleDirEnumerator.FindNext: Boolean;
var
  s: string;
begin
  Result := FindNext(s);
end;

procedure TSimpleDirEnumerator.Reset;
begin
  if FActive then
    FindClose(FSr);
  FActive := False;
end;

{ TFileSystem }

class function TFileSystem.GetTempPath: string;
var
  Res: Integer;
  LastError: Integer;
begin
  // according to MSDN the maximum length of the TEMP path is MAX_PATH+1 (261)
  // an AnsiString / WideString always has one additional character to the length for storing
  // the terminating 0
  SetLength(Result, MAX_PATH);
  Res := Windows.GetTempPath(MAX_PATH + 1, PChar(Result));
  if Res <= 0 then begin
    // GetLastError must be called before _(), otherwise the error code gets lost
    LastError := GetLastError;
    RaiseLastOSErrorEx(LastError, _('TFileSystem.GetTempPath: %1:s (code: %0:d) calling Windows.GetTempPath'));
  end;
  SetLength(Result, Res);
end;

// declared wrongly in WINDOWS

function GetVolumeInformation(lpRootPathName: PChar;
  lpVolumeNameBuffer: PChar; nVolumeNameSize: DWORD; lpVolumeSerialNumber: PDWORD;
  lpMaximumComponentLength, lpFileSystemFlags: LPDWORD;
  lpFileSystemNameBuffer: PChar; nFileSystemNameSize: DWORD): BOOL; stdcall; external kernel32 Name 'GetVolumeInformationA';

class function TFileSystem.GetVolumeName(_DriveLetter: Char): string;
begin
  Result := GetRemoteVolumeName(_DriveLetter + ':\');
end;

class function TFileSystem.HasFileExtFull(const _Filename, _Ext: string): Boolean;
begin
  Result := SameText(_Ext, ExtractFileExtFull(_Filename));
end;

class function TFileSystem.HasFileExtLast(const _Filename, _Ext: string): Boolean;
begin
  Result := SameText(_Ext, ExtractFileExtLast(_Filename));
end;

class function TFileSystem.GetRemoteVolumeName(const _Share: string): string;
var
  Res: LongBool;
begin
  SetLength(Result, MAX_PATH + 1);
  Res := GetVolumeInformation(PChar(itpd(_Share)), PChar(Result), Length(Result), nil, nil, nil, nil, 0);
  if Res then begin
    Result := PChar(Result);
  end else
    Result := '';
end;

class procedure TFileSystem.GetLocalVolumeNames(_sl: TStrings; _HdOnly: Boolean = False; _IgnoreEmpty: Boolean = True);
type
  TDriveType = (dtUnknown, dtNoDrive, dtFloppy, dtFixed, dtNetwork, dtCDROM, dtRAM);
var
  DriveBits: set of 0..25;
  DriveNum: Integer;
  DriveChar: Char;
  DriveType: TDriveType;
  s: string;
begin
  Integer(DriveBits) := Windows.GetLogicalDrives;
  for DriveNum := 0 to 25 do begin
    if not (DriveNum in DriveBits) then
      Continue;
    DriveChar := Char(DriveNum + Ord('a'));
    DriveType := TDriveType(Windows.GetDriveType(PChar(DriveChar + ':\')));
    if not _HdOnly or (DriveType = dtFixed) then begin
      s := GetVolumeName(DriveChar);
      if s <> '' then begin
        _sl.AddObject(s, Pointer(DriveNum)); //FI:W541 Casting from Integer to Pointer type (or vice versa)
      end else begin
        if not _IgnoreEmpty then begin
          s := _('<no volume name>');
          _sl.AddObject(s, Pointer(DriveNum)); //FI:W541 Casting from Integer to Pointer type (or vice versa)
        end;
      end;
    end;
  end;
end;

function re2ehe(_RaiseException: Boolean): TErrorHandlingEnum;
begin
  if _RaiseException then
    Result := ehRaiseException
  else
    Result := ehReturnFalse;
end;
{$IFNDEF DELPHI2005_UP}
// Delphi 6 and 7 do not understand that this calls the non deprecated overload
// I can't be bothered to add lots of ifdefs, so I turn this warning off for the rest of the unit.
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

class function TFileSystem.CreateDir(const _DirectoryName: string; _RaiseException: Boolean): Boolean;
begin
  Result := Self.CreateDir(_DirectoryName, re2ehe(_RaiseException));
end;

class function TFileSystem.CreateDir(const _DirectoryName: string;
  _ErrorHandling: TErrorHandlingEnum): Boolean;
var
  LastError: Cardinal;
begin
  Result := SysUtils.CreateDir(_DirectoryName);
  if not Result then begin
    if _ErrorHandling = ehRaiseException then begin
      // GetLastError must be called before _(), otherwise the error code gets lost
      LastError := GetLastError;
      // duplicate % so they get passed through the format function
      RaiseLastOSErrorEx(LastError, Format(_('Error %%1:s (%%0:d) creating directory "%s"'), [_DirectoryName]));
    end;
  end;
end;

class procedure TFileSystem.CreateEmptyFile(const _fn: string);
var
  Handle: Integer;
begin
  Handle := SysUtils.FileCreate(_fn);
  SysUtils.FileClose(Handle);
end;

class function TFileSystem.CreateUniqueDirectory(_BaseDir: string = ''; const _Prefix: string = 'dz'): string;
var
  Pid: DWORD;
  Counter: Integer;
  Ok: Boolean;
  s: string;
begin
  Result := '';
  if _BaseDir = '' then
    _BaseDir := GetTempPath;
  Pid := GetCurrentProcessId;
  s := itpd(_BaseDir) + _Prefix + '_' + IntToStr(Pid) + '_';
  Counter := 0;
  Ok := False;
  while not Ok do begin
    Result := s + IntToStr(Counter);
    Ok := Self.CreateDir(Result, ehReturnFalse);
    if not Ok then begin
      Inc(Counter);
      if Counter > 1000 then
        raise ECreateUniqueDir.CreateFmt(_('Could not find a unique directory name based on "%s"'), [Result]);
    end;
  end;
end;

type
  TUniqueTempDir = class(TInterfacedObject, IUniqueTempDir)
  private
    FPath: string;
    FDeleteOnlyIfEmpty: Boolean;
    function Path: string;
    ///<summary> Path including trailing path delimiter </summary>
    function PathBS: string;
  public
    constructor Create(const _Path: string; _DeleteOnlyIfEmpty: Boolean = False);
    destructor Destroy; override;
  end;

class function TFileSystem.CreateUniqueTempDir(_DeleteOnlyIfEmpty: Boolean = False;
  const _Prefix: string = 'dz'): IUniqueTempDir;
begin
  Result := CreateUniqueTempDir(GetTempPath, _DeleteOnlyIfEmpty, _Prefix);
end;

class function TFileSystem.CreateUniqueTempDir(const _BaseDir: string;
  _DeleteOnlyIfEmpty: Boolean = False; const _Prefix: string = 'dz'): IUniqueTempDir;
var
  s: string;
begin
  s := CreateUniqueDirectory(_BaseDir, _Prefix);
  Result := TUniqueTempDir.Create(s, _DeleteOnlyIfEmpty);
end;

class function TFileSystem.GetTempFileName(_Directory: string = ''; const _Prefix: string = 'dz';
  _Unique: Word = 0): string;
var
  Res: Integer;
  LastError: Cardinal;
begin
  if _Directory = '' then
    _Directory := GetTempPath;
  SetLength(Result, MAX_PATH);
  Res := Windows.GetTempFileName(PChar(_Directory), PChar(_Prefix), _Unique, PChar(Result));
  if Res = 0 then begin
    // GetLastError must be called before _(), otherwise the error code gets lost
    LastError := GetLastError;
    RaiseLastOSErrorEx(LastError, _('TFileSystem.GetTempFilename: %1:s (Code: %0:d) calling Windows.GetTempFileName'));
  end;
  Result := PChar(Result); // remove trailing characters
end;

//class function TFileSystem.GetTempFileNameEx(_Directory: string; const _Prefix,
//  _Ext: string): string;
//var
//  st: TdzFile;
//  i: Integer;
//begin
//  if _Directory = '' then
//    _Directory := GetTempPath;
//  for i := 0 to 256 * 16 - 1 do begin
//    Result := itpd(_Directory) + _Prefix + IntToHex(MainThreadID, 3) + IntToHex(Random(256 * 16), 2) + _Ext;
//    st := TdzFile.Create(Result);
//    try
//      st.AccessMode := faReadWrite;
//      st.ShareMode := fsNoSharing;
//      st.CreateDisposition := fcCreateFailIfExists;
//      if st.OpenNoException then
//        exit;
//    finally
//      FreeAndNil(st);
//    end;
//  end;
//  raise Exception.CreateFmt(_('Unable to create a temporary file from %s.'), [itpd(_Directory) + _Prefix + '*' + _Ext]);
//end;

class function TFileSystem.CheckAccessToFile(DesiredAccess: DWORD; const Filename: WideString): Boolean;
const
  GenericFileMapping: TGenericMapping = (
    GenericRead: FILE_GENERIC_READ;
    GenericWrite: FILE_GENERIC_WRITE;
    GenericExecute: FILE_GENERIC_EXECUTE;
    GenericAll: FILE_ALL_ACCESS
    );
var
  LastError: DWORD;
  LengthNeeded: DWORD;
  SecurityDescriptor: PSecurityDescriptor;
  ClientToken: THandle;
  AccessMask: DWORD;
  PrivilegeSet: TPrivilegeSet;
  PrivilegeSetLength: DWORD;
  GrantedAccess: DWORD;
  AccessStatus: BOOL;
begin
  Result := False;
  LastError := GetLastError;
  if not GetFileSecurityW(PWideChar(Filename), OWNER_SECURITY_INFORMATION or
    GROUP_SECURITY_INFORMATION or DACL_SECURITY_INFORMATION, nil, 0,
    LengthNeeded) and (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
    Exit;
  SetLastError(LastError);
  Inc(LengthNeeded, $1000);
  SecurityDescriptor := PSecurityDescriptor(LocalAlloc(LPTR, LengthNeeded));
  if not Assigned(SecurityDescriptor) then
    Exit;
  try
    if not GetFileSecurityW(PWideChar(Filename), OWNER_SECURITY_INFORMATION or
      GROUP_SECURITY_INFORMATION or DACL_SECURITY_INFORMATION,
      SecurityDescriptor, LengthNeeded, LengthNeeded) then
      Exit;
    if not ImpersonateSelf(SecurityImpersonation) then
      Exit;
    try
      if not OpenThreadToken(GetCurrentThread, TOKEN_QUERY or
        TOKEN_IMPERSONATE or TOKEN_DUPLICATE, False, ClientToken) then
        Exit;
      try
        AccessMask := DesiredAccess;
        MapGenericMask(AccessMask, GenericFileMapping);
        PrivilegeSetLength := SizeOf(TPrivilegeSet);
        if AccessCheck(SecurityDescriptor, ClientToken, AccessMask,
          GenericFileMapping, PrivilegeSet, PrivilegeSetLength, GrantedAccess,
          AccessStatus) then
          Result := AccessStatus;
      finally
        CloseHandle(ClientToken);
      end;
    finally
      RevertToSelf;
    end;
  finally
    LocalFree(HLOCAL(SecurityDescriptor));
  end;
end;

function _TouchFileTimes(FileHandle: THandle; lpSystemTime: PSystemTime):
  BOOL; stdcall; external 'IMAGEHLP.DLL' Name 'TouchFileTimes';

class procedure TFileSystem.TouchFileTimes(const _Filename: string; lpSystemTime: PSystemTime);
var
  Handle: THandle;
  Res: BOOL;
  LastError: DWORD;
begin
  Handle := CreateFile(PChar(_Filename), GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then
    raise EFileNotFound.CreateFmt(_('File not found: "%s"'), [_Filename]);
  try
    Res := _TouchFileTimes(Handle, lpSystemTime);
    if not Res then begin
      LastError := GetLastError;
      RaiseLastOSErrorEx(LastError, Format(_('Error %%1:s (%%0:d) while trying to change the date and time of "%s"'), [_Filename]));
    end;
  finally
    CloseHandle(Handle);
  end;
end;

class function TFileSystem.TryGetFileInfo(const _Filename: string;
  out _Info: TFileInfoRec): Boolean;
var
  Sr: TSearchRec;
  Res: Integer;
begin
  Res := FindFirst(_Filename, faAnyFile, Sr);
  Result := (Res = 0);
  if Result then begin
    try
      _Info.Filename := _Filename;
      _Info.Size := Sr.Size;
{$IFDEF RTL220_UP}
      _Info.Timestamp := Sr.Timestamp;
{$ELSE}
      _Info.Timestamp := FileDateToDateTime(Sr.Time);
{$ENDIF}
    finally
      FindClose(Sr);
    end;
  end;
end;

class function TFileSystem.GetFileInfo(const _Filename: string): TFileInfoRec;
begin
  if not TryGetFileInfo(_Filename, Result) then
    raise EFileNotFound.CreateFmt(_('File not found: "%s"'), [_Filename]);
end;

class function TFileSystem.GetFileSize(const _Filename: string): Int64;
begin
  if not TryGetFileSize(_Filename, Result) then
    raise EFileNotFound.CreateFmt(_('File not found: "%s"'), [_Filename]);
end;

class function TFileSystem.TryGetFileSize(const _Filename: string;
  out _Size: Int64): Boolean;
var
  Info: TFileInfoRec;
begin
  Result := TryGetFileInfo(_Filename, Info);
  if Result then
    _Size := Info.Size;
end;

class function TFileSystem.DiskFree(_DriveLetter: AnsiChar): Int64;
begin
  if _DriveLetter in ['a'..'z'] then
    _DriveLetter := UpCase(_DriveLetter);

  if not (_DriveLetter in ['A'..'Z']) then
    Result := -1
  else begin
    try
      Result := SysUtils.DiskFree(Ord(_DriveLetter) - Ord('A') + 1);
    except
      Result := -1;
    end;
  end;
end;

class function TFileSystem.DiskFree(const _Directory: string): Int64;
var
  TotalSpace: Int64;
begin
  try
    if not SysUtils.GetDiskFreeSpaceEx(PChar(_Directory), Result, TotalSpace, nil) then
      Result := -1;
  except
    Result := -1;
  end;
end;

class function TFileSystem.GetShortPathname(const _LongName: string): string;
var
  Res: Integer;
  LastError: Cardinal;
begin
  SetLength(Result, MAX_PATH);
  Res := Windows.GetShortPathname(PChar(_LongName), PChar(Result), Length(Result));
  if Res = 0 then begin
    LastError := GetLastError;
    RaiseLastOSErrorEx(LastError, _('TFileSystem.GetShortPathname: %1:s (Code: %0:d) calling Windows.GetShortPathname'));
  end else if Res > MAX_PATH then
    raise EPathTooLong.CreateFmt(_('Short pathname is longer than MAX_PATH (%d) characters'), [MAX_PATH]);
  Result := PChar(Result); // truncate at first #0
end;

class function TFileSystem.MoveFile(const _Source, _Dest: string; _RaiseException: Boolean): Boolean;
begin
  Result := Self.MoveFile(_Source, _Dest, re2ehe(_RaiseException));
end;

class function TFileSystem.MoveFile(const _Source, _Dest: string;
  _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean;
var
  LastError: Cardinal;
begin
  Result := Windows.MoveFile(PChar(_Source), PChar(_Dest));
  if not Result and (_ErrorHandling = ehRaiseException) then begin
    LastError := GetLastError;
    // duplicate % so they get passed through the format function
    RaiseLastOSErrorEx(LastError, Format(_('Error %%1:s (%%0:d) while trying to move "%s" to "%s".'), [_Source, _Dest]));
  end;
end;

class function TFileSystem.MoveFileEx(const _Source, _Dest: string; _Flags: TMoveFileExFlagSet;
  _ErrorHandling: TErrorHandlingEnum = ehRaiseException): Boolean;
var
  LastError: Cardinal;
  Flags: DWORD;
begin
  Flags := 0;
  if mfCopyAllowed in _Flags then
    Flags := Flags or MOVEFILE_COPY_ALLOWED;
  if mfDelayUntilReboot in _Flags then
    Flags := Flags or MOVEFILE_DELAY_UNTIL_REBOOT;
  if mfFailIfNotTrackable in _Flags then
    Flags := Flags or MOVEFILE_FAIL_IF_NOT_TRACKABLE;
  if mfReplaceExisting in _Flags then
    Flags := Flags or MOVEFILE_REPLACE_EXISTING;
  if mfWriteThrough in _Flags then
    Flags := Flags or MOVEFILE_WRITE_THROUGH;

  Result := Windows.MoveFileEx(PChar(_Source), PChar(_Dest), Flags);
  if not Result and (_ErrorHandling = ehRaiseException) then begin
    LastError := GetLastError;
    // duplicate % so they get passed through the format function
    RaiseLastOSErrorEx(LastError, Format(_('Error %%1:s (%%0:d) while trying to move "%s" to "%s".'), [_Source, _Dest]));
  end;
end;

class function TFileSystem.MoveFileEx(const _Source, _Dest: string; _Flags: TMoveFileExFlagSet;
  _RaiseException: Boolean): Boolean;
begin
  Result := Self.MoveFileEx(_Source, _Dest, _Flags, re2ehe(_RaiseException));
end;

class function TFileSystem.SetReadonly(const _Filename: string; _Value: Boolean;
  _RaiseException: Boolean): Boolean;
begin
  Result := Self.SetReadonly(_Filename, _Value, re2ehe(_RaiseException));
end;

class function TFileSystem.SetReadonly(const _Filename: string; _Value: Boolean;
  _ErrorHandling: TErrorHandlingEnum): Boolean;
var
  Attr: Integer;
  LastError: Cardinal;
begin
  Attr := FileGetAttr(_Filename);
  if _Value then
    Attr := Attr or SysUtils.faReadOnly
  else
    Attr := Attr and not SysUtils.faReadOnly;
  if FileSetAttr(_Filename, Attr) <> 0 then begin
    if _ErrorHandling = ehRaiseException then begin
      LastError := GetLastError;
      // duplicate % so they get passed through the format function
      RaiseLastOSErrorEx(LastError, Format(_('Error %%1:s (%%0:d) while changing the readonly flag of "%s"'), [_Filename]));
    end;
    Result := False
  end else
    Result := True;
end;

class function TFileSystem.CopyFile(const _Source, _Dest: string; _FailIfExists: Boolean;
  _RaiseException: Boolean = True; _ForceOverwrite: Boolean = False): Boolean;
var
  LastError: Cardinal;
begin
  Result := Windows.CopyFile(PChar(_Source), PChar(_Dest), _FailIfExists);
  if not Result and not _FailIfExists and _ForceOverwrite then begin
    Self.SetReadonly(_Dest, False, ehReturnFalse);
    Result := Windows.CopyFile(PChar(_Source), PChar(_Dest), _FailIfExists);
  end;
  if not Result and _RaiseException then begin
    LastError := GetLastError;
    // duplicate % so they get passed through the format function
    RaiseLastOSErrorEx(LastError, Format(_('Error %%1:s (%%0:d) while trying to copy "%s" to "%s".'), [_Source, _Dest]));
  end;
end;

class function TFileSystem.CopyFile(const _Source, _Dest: string; _IfExists: TCopyFileFlagIfExists;
  _ErrorHandling: TErrorHandlingEnum; _IfReadOnly: TCopyFileFlagOverwriteReadonly): Boolean;
var
  LastError: Cardinal;
begin
  Result := Windows.CopyFile(PChar(_Source), PChar(_Dest), (_IfExists = cfeFailIfExists));
  if not Result and (_IfExists = cfeOverwriteIfExists) and (_IfReadOnly = cforOverwriteReadonly) then begin
    Self.SetReadonly(_Dest, False, ehReturnFalse);
    Result := Windows.CopyFile(PChar(_Source), PChar(_Dest), (_IfExists = cfeFailIfExists));
  end;
  if not Result and (_ErrorHandling = ehRaiseException) then begin
    LastError := GetLastError;
    // duplicate % so they get passed through the format function
    RaiseLastOSErrorEx(LastError, Format(_('Error %%1:s (%%0:d) while trying to copy "%s" to "%s".'), [_Source, _Dest]));
  end;
end;

class function TFileSystem.CopyFile(const _Source, _Dest: string; _Flags: TCopyFileFlagSet): Boolean;
var
  IfExists: TCopyFileFlagIfExists;
  ErrorHandling: TErrorHandlingEnum;
  IfReadOnly: TCopyFileFlagOverwriteReadonly;
begin
  if cfFailIfExists in _Flags then
    IfExists := cfeFailIfExists
  else
    IfExists := cfeOverwriteIfExists;
  if cfRaiseException in _Flags then
    ErrorHandling := ehRaiseException
  else
    ErrorHandling := ehReturnFalse;
  if cfForceOverwrite in _Flags then
    IfReadOnly := cforOverwriteReadonly
  else
    IfReadOnly := cforDoNotOverwriteReadonly;
  Result := Self.CopyFile(_Source, _Dest,
    IfExists, ErrorHandling, IfReadOnly);
end;

class function TFileSystem.AppendDateTime(const _Filename: string; _DateTime: TDateTime;
  _IncludeTime: Boolean): string;
var
  BaseFilename: string;
  Ext: string;
begin
  Ext := ExtractFileExtFull(_Filename);
  BaseFilename := ChangeFileExtFull(_Filename, '');
  Result := BaseFilename + '_' + ReplaceChars(DateTime2Iso(_DateTime, _IncludeTime), ': ', '-_') + Ext;
end;

class function TFileSystem.AppendDate(const _Filename: string; _Date: TDateTime): string;
begin
  Result := AppendDateTime(_Filename, _Date, False);
end;

class function TFileSystem.AppendDate(const _Filename: string): string;
begin
  Result := AppendDateTime(_Filename, SysUtils.Date, False);
end;

class function TFileSystem.AppendDateAndTime(const _Filename: string): string;
begin
  Result := AppendDateTime(_Filename, Now, True);
end;

class function TFileSystem.AppendDateAndTime(const _Filename: string; _DateTime: TDateTime): string;
begin
  Result := AppendDateTime(_Filename, _DateTime, True);
end;

class function TFileSystem.GenerateBackupFilename(const _Filename: string; _BackupDir: string = ''): string;
var
  Ext: string;
  FilenameOnly: string;
  Base: string;
begin
  if _BackupDir = '' then
    _BackupDir := ExtractFilePath(_Filename);
  _BackupDir := itpd(_BackupDir);
  FilenameOnly := ExtractFileName(_Filename);
  Ext := ExtractFileExt(FilenameOnly);
  Base := ChangeFileExt(FilenameOnly, '');
  Result := _BackupDir + Base + '_' + ReplaceChars(DateTime2Iso(Now, True), ': ', '-_') + Ext;
end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
class function TFileSystem.BackupFile(const _Filename: string; const _BackupDir: string = ''): string;
var
  i: Integer;
  BackupFilename: TFilename;
  LastError: Cardinal;
  ErrorMessage: string;
begin
  BackupFilename := GenerateBackupFilename(_Filename, _BackupDir);
  Result := BackupFilename;

  ForceDir(ExtractFilePath(BackupFilename));

  i := 0;
  while not Self.CopyFile(_Filename, Result, cfeFailIfExists, ehReturnFalse) do begin
    Inc(i);
    if i = 1000 then begin
      LastError := GetLastError;
      ErrorMessage := SysErrorMessage(LastError);
      raise EBackupFailed.CreateFmt(
        _('Failed to create a backup of "%s". Tried 1000 different file names based on "%s".')
        + ' (%s)',
        [_Filename, BackupFilename.Filename, ErrorMessage]);
    end;
    Result := BackupFilename.DirectoryBS
      + BackupFilename.FilenameOnly + '_' + IntToStr(i)
      + BackupFilename.Extension;
  end;
end;

class function TFileSystem.MoveFileToBackup(const _Filename: string; const _BackupDir: string = ''): string;
var
  i: Integer;
  BackupFilename: TFilename;
  LastError: Cardinal;
  ErrorMessage: string;
begin
  BackupFilename := GenerateBackupFilename(_Filename, _BackupDir);
  Result := BackupFilename;

  ForceDir(ExtractFilePath(BackupFilename));

  i := 0;
  while not Self.MoveFileEx(_Filename, Result, [mfCopyAllowed], ehReturnFalse) do begin
    Inc(i);
    if i = 1000 then begin
      LastError := GetLastError;
      ErrorMessage := SysErrorMessage(LastError);
      raise EBackupFailed.CreateFmt(
        _('Failed to create a backup of "%s". Tried 1000 different file names based on "%s".')
        + ' (%s)',
        [_Filename, BackupFilename.Filename, ErrorMessage]);
    end;
    Result := BackupFilename.DirectoryBS
      + BackupFilename.FilenameOnly + '_' + IntToStr(i)
      + BackupFilename.Extension;
  end;
end;

{$ENDIF}

class function TFileSystem.ContainsWildcard(const _Mask: string): Boolean;
begin
  Result := (Pos('?', _Mask) > 0) or (Pos('*', _Mask) > 0);
end;

class function TFileSystem.CopyDir_Old(const _SrcDir, _DestDir: string; _CreateIntermediate,
  _RaiseException: Boolean): Boolean;
var
  CreateIntermediate: TCopyDirCreateIntermediate;
begin
  if _CreateIntermediate then
    CreateIntermediate := cdciCreateIntermediate
  else
    CreateIntermediate := cdciDoNotCreateIntermediate;

  Result := Self.CopyDir(_SrcDir, _DestDir, CreateIntermediate, re2ehe(_RaiseException));
end;

class function TFileSystem.CopyDir(const _SrcDir, _DestDir: string;
  _CreateIntermediate: TCopyDirCreateIntermediate; _ErrorHandling: TErrorHandlingEnum): Boolean;
var
  SrcDirBs: string;
  ParentDir: string;
  Files: TStringList;
  i: Integer;
  DestDirBS: string;
begin
  Result := False;
  SrcDirBs := itpd(_SrcDir);
  ParentDir := ExtractFileDir(_DestDir);
  if (ParentDir <> '') and not DirExists(ParentDir) then begin
    if _CreateIntermediate = cdciDoNotCreateIntermediate then begin
      if _ErrorHandling = ehRaiseException then
        raise Exception.CreateFmt(_('Error copying directory "%s" to "%s": Destination path "%s" does not exist.'),
          [_SrcDir, _DestDir, ParentDir]);
      Exit;
    end;
  end;

  if TFileSystem.DirExists(_DestDir) then begin
    if _ErrorHandling = ehRaiseException then
      raise Exception.CreateFmt(_('Directory %s already exists.'), [_DestDir]);
    Exit;
  end;

  if not TFileSystem.ForceDir(_DestDir, _ErrorHandling) then
    Exit;

  DestDirBS := itpd(_DestDir);
  Files := TStringList.Create;
  try
    TSimpleDirEnumerator.Execute(SrcDirBs + '*', Files, [dfaArchive, dfaReadonly], False);
    for i := 0 to Files.Count - 1 do
      Self.CopyFile(SrcDirBs + Files[i], DestDirBS + Files[i], cfeFailIfExists, _ErrorHandling);
  finally
    FreeAndNil(Files);
  end;
end;

type
  TProgressRedir = class(TCopyProgressStatus)
  private
    FOnProgress: TCopyFileProgressEvt;
  private
    FCancelFlag: BOOL;
    // should an exception be raised within the OnProgress callback, it is stored here
    FExceptAddr: Pointer;
    FExceptMsg: string;
    FExceptClass: string;
    function doProgress(): TCopyProgressResult;
  public
    constructor Create(_OnProgress: TCopyFileProgressEvt);
  end;

//  PROGRESS_CONTINUE = 0;
//  PROGRESS_CANCEL = 1;
//  PROGRESS_STOP = 2;
//  PROGRESS_QUIET = 3;

//  CALLBACK_CHUNK_FINISHED = $00000000;
//  CALLBACK_STREAM_SWITCH = $00000001;

function ProgressCallback(
  _TotalFileSize, _TotalBytesTransferred, _StreamSize, _StreamBytesTransferred: LARGE_INTEGER;
  _StreamNumber, _CallbackReason: LongWord;
  _SourceFile, _DestinationFile: THandle; _Data: Pointer): LongWord; far; stdcall;
var
  Status: TProgressRedir;
begin
  try
    Status := TProgressRedir(_Data);
    Status.FTotalFileSize := _TotalFileSize;
    Status.FTotalBytesTransferred := _TotalBytesTransferred;
    Status.FStreamSize := _StreamSize;
    Status.FStreamBytesTransferred := _StreamBytesTransferred;
    Status.FStreamNumber := _StreamNumber;
    case _CallbackReason of
      CALLBACK_CHUNK_FINISHED: Status.FCallbackReason := prChunkFinished;
      CALLBACK_STREAM_SWITCH: Status.FCallbackReason := prStreamSwitch;
    else
    // Shouldn't happen, assume CALLBACK_CHUNK_FINISHED for now
      Status.FCallbackReason := prChunkFinished;
    end;
    Status.FSourceFile := _SourceFile;
    Status.FDestinationFile := _DestinationFile;
    case Status.doProgress() of
      prContinue: Result := PROGRESS_CONTINUE;
      prCancel: Result := PROGRESS_CANCEL;
      prStop: Result := PROGRESS_STOP;
      prQuiet: Result := PROGRESS_QUIET;
    else // should not happen, assume prContinue
      Result := PROGRESS_CONTINUE;
    end;
  except
    // Ignore exceptions here since the progess display should not affect the actual copying.
    // Any exceptions whithin doProgress should be handled there and communicated to the main
    // thread.
    Result := PROGRESS_CONTINUE;
  end;
end;

//  COPY_FILE_FAIL_IF_EXISTS = $00000001;
//  COPY_FILE_RESTARTABLE = $00000002;

class function TFileSystem.CopyFileWithProgress(const _Source, _Dest: string;
  _Progress: TCopyFileProgressEvt;
  _Flags: TCopyFileWithProgressFlagSet): TCopyFileWithProgressResult;
var
  IfExists: TCopyFileFlagIfExists;
  ErrorHandling: TErrorHandlingEnum;
  Restartable: TCopyFileWithProgressRestartable;
begin
  if cfwFailIfExists in _Flags then
    IfExists := cfeFailIfExists
  else
    IfExists := cfeOverwriteIfExists;

  if cfwRestartable in _Flags then
    Restartable := cfwrRestartable
  else
    Restartable := cfwrNotRestartable;

  if cfwRaiseException in _Flags then
    ErrorHandling := ehRaiseException
  else
    ErrorHandling := ehReturnFalse;

  Result := Self.CopyFileWithProgress(_Source, _Dest, _Progress, IfExists, ErrorHandling, Restartable);
end;

class function TFileSystem.CopyFileWithProgress(const _Source, _Dest: string;
  _Progress: TCopyFileProgressEvt; _IfExists: TCopyFileFlagIfExists;
  _ErrorHandling: TErrorHandlingEnum;
  _Restartable: TCopyFileWithProgressRestartable): TCopyFileWithProgressResult;
var
  Redir: TProgressRedir;
  Flags: DWORD;
  Res: BOOL;
  LastError: DWORD;
begin
  Result := cfwError;
  Redir := TProgressRedir.Create(_Progress);
  try
    Flags := 0;
    if _IfExists = cfeFailIfExists then
      Flags := Flags or COPY_FILE_FAIL_IF_EXISTS;
    if _Restartable = cfwrRestartable then
      Flags := Flags or COPY_FILE_RESTARTABLE;
    Res := Windows.CopyFileEx(PChar(_Source), PChar(_Dest), @ProgressCallback, Redir,
      @Redir.FCancelFlag, Flags);
    if Redir.FExceptAddr <> nil then begin
      if _ErrorHandling = ehRaiseException then begin
        raise Exception.CreateFmt(_('Error %s (%s) in progress callback while trying to copy "%s" to "%s".'),
          [Redir.FExceptMsg, Redir.FExceptClass, _Source, _Dest])at Redir.FExceptAddr;
      end;
      Exit; //==>
    end;
    if not Res then begin
      LastError := GetLastError;
      if LastError = ERROR_REQUEST_ABORTED then
        Result := cfwAborted
      else begin
        if _ErrorHandling = ehRaiseException then begin
          // duplicate % so they get passed through the format function
          RaiseLastOSErrorEx(LastError, Format(_('Error %%1:s (%%0:d) while trying to copy "%s" to "%s".'), [_Source, _Dest]));
        end;
      end;
    end else
      Result := cfwOK;
  finally
    FreeAndNil(Redir);
  end;
end;

class function TFileSystem.CopyMatchingFiles(const _Mask, _SrcDir, _DestDir: string;
  _Flags: TCopyFileFlagSet; _FilesSkipped: TStrings = nil): Integer;
var
  i: Integer;
  Files: TStringList;
  s: string;
  SrcDirBs: string;
  DestDirBS: string;
begin
  Result := 0;
  SrcDirBs := itpd(_SrcDir);
  DestDirBS := itpd(_DestDir);
  Files := TStringList.Create;
  try
    TSimpleDirEnumerator.Execute(SrcDirBs + _Mask, Files, [dfaHidden, dfaSysFile, dfaArchive]);
    for i := 0 to Files.Count - 1 do begin
      s := Files[i];
      if Self.CopyFile(SrcDirBs + s, DestDirBS + s, _Flags) then
        Inc(Result)
      else begin
        if Assigned(_FilesSkipped) then
          _FilesSkipped.Add(s);
      end;
    end;
  finally
    FreeAndNil(Files);
  end;
end;

//  MOVEFILE_REPLACE_EXISTING       = $00000001;
//  MOVEFILE_COPY_ALLOWED           = $00000002;
//  MOVEFILE_DELAY_UNTIL_REBOOT     = $00000004;
//  MOVEFILE_WRITE_THROUGH          = $00000008;
//  MOVEFILE_CREATE_HARDLINK        = $00000010;
//  MOVEFILE_FAIL_IF_NOT_TRACKABLE  = $00000020;

class function TFileSystem.MoveFileWithProgress(const _Source, _Dest: string;
  _Progress: TCopyFileProgressEvt;
  _Flags: TMoveFileWithProgressFlagSet = [mfwRaiseException]): TCopyFileWithProgressResult;
var
  Redir: TProgressRedir;
  Flags: DWORD;
  Res: BOOL;
  LastError: DWORD;
begin
  Redir := TProgressRedir.Create(_Progress);
  try
    Flags := MOVEFILE_REPLACE_EXISTING;
    if mfwFailIfExists in _Flags then
      Flags := Flags - MOVEFILE_REPLACE_EXISTING;
    if mfwAllowCopy in _Flags then
      Flags := Flags or MOVEFILE_COPY_ALLOWED;
    if mfwDelayUntilReboot in _Flags then
      Flags := Flags or MOVEFILE_DELAY_UNTIL_REBOOT;
    if mfwWriteThrough in _Flags then
      Flags := Flags or MOVEFILE_WRITE_THROUGH;
    if mfwFailIfNotTrackable in _Flags then
      Flags := Flags or MOVEFILE_FAIL_IF_NOT_TRACKABLE;
    Res := Windows.MoveFileWithProgress(PChar(_Source), PChar(_Dest),
      @ProgressCallback, Redir, Flags);
    if not Res then begin
      LastError := GetLastError;
      if mfwRaiseException in _Flags then begin
        // duplicate % so they get passed through the format function
        RaiseLastOSErrorEx(LastError, Format(_('Error %%1:s (%%0:d) while trying to move "%s" to "%s".'), [_Source, _Dest]));
      end;

      if LastError = ERROR_REQUEST_ABORTED then
        Result := cfwAborted
      else
        Result := cfwError;
    end else
      Result := cfwOK;
  finally
    FreeAndNil(Redir);
  end;
end;

class function TFileSystem.DeleteFile(const _Filename: string; _RaiseException: Boolean = True;
  _Force: Boolean = False): Boolean;
var
  Attr: Integer;
  LastError: Cardinal;
begin
  Result := SysUtils.DeleteFile(_Filename);
  if not Result and _Force then begin
    Attr := FileGetAttr(_Filename);
    Attr := Attr and not SysUtils.faReadOnly;
    FileSetAttr(_Filename, Attr);
    Result := SysUtils.DeleteFile(_Filename);
  end;
  if not Result and _RaiseException then begin
    LastError := GetLastError;
    // duplicate % so they get passed through the format function
    RaiseLastOSErrorEx(LastError, Format(_('Error %%1:s (%%0:d) deleting file "%s"'), [_Filename]));
  end;
end;

class function TFileSystem.DeleteMatchingFiles(const _Dir, _Mask: string;
  _RaiseException, _Force: Boolean): Integer;
begin
  Result := DeleteMatchingFiles(_Dir, _Mask, [], _RaiseException, _Force);
end;

class function TFileSystem.DeleteMatchingFiles(const _Dir, _Mask: string;
  const _ExceptMask: string; _RaiseException, _Force: Boolean): Integer;
begin
  Result := DeleteMatchingFiles(_Dir, _Mask, [_ExceptMask], _RaiseException, _Force);
end;

class function TFileSystem.DeleteMatchingFiles(const _Dir, _Mask: string;
  const _ExceptMasks: array of string; _RaiseException,
  _Force: Boolean): Integer;

  function MatchesAnyExceptMask(const _s: string): Boolean;
  var
    i: Integer;
    Mask: string;
  begin
    for i := Low(_ExceptMasks) to High(_ExceptMasks) do begin
      Mask := LowerCase(_ExceptMasks[i]);
      if MatchesMask(_s, Mask) then begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;

var
  Sr: TSearchRec;
  dir: string;
begin
  Assert(_Dir <> '', 'Dir parameter must not be an empty string');
  Assert(_Mask <> '', 'Mask parameter must not be an empty string');

  Result := 0;
  dir := IncludeTrailingPathDelimiter(_Dir);
  if 0 = FindFirst(dir + _Mask, faAnyFile, Sr) then begin
    try
      repeat
        if (Sr.Name <> '.') and (Sr.Name <> '..') then
          if ((Sr.Attr and SysUtils.faDirectory) = 0) then
            if not MatchesAnyExceptMask(LowerCase(Sr.Name)) then
              if not DeleteFile(dir + Sr.Name, _RaiseException, _Force) then
                Inc(Result);
      until 0 <> FindNext(Sr);
    finally
      FindClose(Sr);
    end;
  end;
end;

class function TFileSystem.FileExists(const _Filename: string): Boolean;
var
  OldErrorMode: Cardinal;
begin
  OldErrorMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    Result := SysUtils.FileExists(_Filename);
  finally
    SetErrorMode(OldErrorMode)
  end;
end;

class function TFileSystem.FileExists(const _Filename: string; _RaiseException: Boolean): Boolean;
begin
  if _RaiseException then begin
    AssertFileExists(_Filename);
    Result := True;
  end else
    Result := FileExists(_Filename);
end;

class procedure TFileSystem.AssertFileExists(const _Filename: string);
begin
  if not FileExists(_Filename) then
    raise EFileNotFound.CreateFmt(_('File not found: %s'), [_Filename]);
end;

class procedure TFileSystem.AssertMatchingFileExists(const _Mask: string);
begin
  if FindMatchingFile(_Mask) <> mfFile then
    raise EFileNotFound.CreateFmt(_('Matching file not found: %s'), [_Mask]);
end;

class function TFileSystem.DirExists(const _DirName: string): Boolean;
begin
  Result := SysUtils.DirectoryExists(_DirName);
end;

class function TFileSystem.DirExists(const _DirName: string; _RaiseException: Boolean): Boolean;
begin
  Result := False;
  if _RaiseException then
    AssertDirExists(_DirName)
  else
    Result := DirExists(_DirName);
end;

class procedure TFileSystem.AssertDirExists(const _DirName: string);
begin
  if not SysUtils.DirectoryExists(_DirName) then
    raise EDirNotFound.CreateFmt(_('Directory not found: %s'), [_DirName]);
end;

class function TFileSystem.AllDirsExist(_DirNames: TStrings): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to _DirNames.Count - 1 do begin
    if not DirExists(_DirNames[i]) then begin
      Result := False;
      Exit; //==>
    end;
  end;
end;

class function TFileSystem.FindMatchingDirs(const _Mask: string; _sl: TStrings;
  _IncludePath: Boolean = False): Integer;
var
  enum: TSimpleDirEnumerator;
begin
  enum := TSimpleDirEnumerator.Create(_Mask, [dfaDirectory, dfaArchive]);
  try
    enum.MustHaveAttr := [dfaDirectory];
    Result := enum.FindAll(_sl, _IncludePath);
  finally
    FreeAndNil(enum);
  end;
end;

class function TFileSystem.FindMatchingDir(const _Mask: string; out _Dir: string): Boolean;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    Result := FindMatchingDirs(_Mask, sl, True) > 0;
    if Result then
      _Dir := sl[0];
  finally
    FreeAndNil(sl);
  end;
end;

class function TFileSystem.FindMatchingFile(const _Mask: string): TMatchingFileResult;
var
  fn: string;
begin
  Result := FindMatchingFile(_Mask, fn);
end;

class function TFileSystem.FindMatchingFiles(const _Mask: string;
  _IncludePath: Boolean; _Sort: Boolean): TStringArray;
var
  Cnt: Integer;
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    Cnt := FindMatchingFiles(_Mask, sl, _IncludePath, _Sort);
    SetLength(Result, Cnt);
    for i := 0 to Cnt - 1 do
      Result[i] := sl[i];
  finally
    FreeAndNil(sl);
  end;
end;

class function TFileSystem.FindMatchingFiles(const _Mask: string; _sl: TStrings;
  _IncludePath: Boolean = False; _Sort: Boolean = True): Integer;
begin
  Result := TSimpleDirEnumerator.Execute(_Mask, _sl, [dfaArchive], _IncludePath, _Sort);
end;

class function TFileSystem.FindMatchingFile(const _Mask: string; out _Filename: string): TMatchingFileResult;
var
  Sr: TSearchRec;
begin
  Result := mfNotFound;
  if 0 = FindFirst(_Mask, faAnyFile, Sr) then
    try
      repeat
        if (Sr.Name <> '.') and (Sr.Name <> '..') then begin
          _Filename := Sr.Name;
          if (Sr.Attr and SysUtils.faDirectory) <> 0 then
            Result := mfDirectory
          else
            Result := mfFile;
          Exit;
        end;
      until 0 <> FindNext(Sr);
    finally
      FindClose(Sr);
    end;
end;

class function TFileSystem.ForceDir_old(const _DirectoryPath: string; _RaiseException: Boolean): Boolean;
begin
  Result := Self.ForceDir(_DirectoryPath, re2ehe(_RaiseException));
end;

class function TFileSystem.ForceDir(const _DirectoryPath: string;
  _ErrorHandling: TErrorHandlingEnum): Boolean;
var
  LastError: Cardinal;
begin
  try
    Result := SysUtils.ForceDirectories(_DirectoryPath);
  except
    on e: Exception do begin
      // ForceDirectories can raise EInOutError if the directory path contains empty parts
      if _ErrorHandling = ehRaiseException then
        raise Exception.CreateFmt(_('Error creating directory "%s": %s (%s)'), [_DirectoryPath, e.Message, e.ClassName]);
      Result := False;
      Exit;
    end;
  end;
  if not Result and (_ErrorHandling = ehRaiseException) then begin
    LastError := GetLastError;
    // duplicate % so they get passed through the format function
    RaiseLastOSErrorEx(LastError, Format(_('Error %%1:s (%%0:d) creating directory "%s"'), [_DirectoryPath]));
  end;
end;

class function TFileSystem.RemoveDir(const _DirName: string; _RaiseException: Boolean = True; _Force: Boolean = False): Boolean;
var
  Attr: Integer;
  LastError: Cardinal;
begin
  Result := SysUtils.RemoveDir(_DirName);
  if not Result and _Force then begin
    Attr := FileGetAttr(_DirName);
    Attr := Attr and not SysUtils.faReadOnly;
    FileSetAttr(_DirName, Attr);
    Result := SysUtils.RemoveDir(_DirName);
  end;
  if not Result and _RaiseException then begin
    LastError := GetLastError;
    // duplicate % so they get passed through the format function
    RaiseLastOSErrorEx(LastError, Format(_('Error %%1:s (%%0:d) deleting directory "%s"'), [_DirName]));
  end;
end;

class function TFileSystem.DelTree(const _DirName: string; _Force: Boolean = False; _RaiseException: Boolean = True): Boolean;
begin
  Result := DelDirTree(_DirName, _RaiseException, _Force);
end;

class function TFileSystem.DelDirTree(const _DirName: string; _RaiseException,
  _Force: Boolean): Boolean;
var
  Sr: TSearchRec;
  Filename: string;
begin
  Result := DirectoryExists(ExcludeTrailingPathDelimiter(_DirName));
  if not Result then begin
    if _RaiseException then
      raise EDirNotFound.CreateFmt(_('"%s" does not exist or is not a directory'), [_DirName]);
    Exit;
  end;
  if 0 = FindFirst(IncludeTrailingPathDelimiter(_DirName) + '*.*', faAnyFile, Sr) then
    try
      repeat
        if (Sr.Name = '.') or (Sr.Name = '..') then begin
          // ignore
        end else begin
          Filename := IncludeTrailingPathDelimiter(_DirName) + Sr.Name;
          if (Sr.Attr and SysUtils.faDirectory) <> 0 then begin
            Result := DelDirTree(Filename, _RaiseException, _Force);
            if not Result then
              Exit;
          end else begin
            Result := DeleteFile(Filename, _RaiseException, _Force);
            if not Result then
              Exit;
          end;
        end;
      until 0 <> FindNext(Sr);
    finally
      SysUtils.FindClose(Sr);
    end;
  Result := RemoveDir(_DirName, _RaiseException, _Force);
end;

class function TFileSystem.ReadTextFile(const _Filename: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(_Filename);
    Result := sl.Text;
  finally
    FreeAndNil(sl);
  end;
end;

class function TFileSystem.IsFileReadonly(const _Filename: string): Boolean;
var
  Attributes: Word;
begin
  Result := False;
  if FileExists(_Filename) then begin
    Attributes := FileGetAttr(_Filename);
    Result := ((Attributes and SysUtils.faReadOnly) <> 0);
  end;
end;

class function TFileSystem.IsFileWritable(const _Filename: string): Boolean;
var
  Attributes: Word;
  Res: THandle;
begin
  Result := False;
  if not FileExists(_Filename) then begin
      // File does not exit
    Exit; //==>
  end;
  Attributes := FileGetAttr(_Filename);
  if ((Attributes and SysUtils.faReadOnly) <> 0) then begin
    // File has ReadOnly flag
    Exit; //==>
  end;
  Res := CreateFile(PChar(_Filename), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if Res = INVALID_HANDLE_VALUE then begin
        // opening file failed
    Exit; //==>
  end;
  CloseHandle(Res);
  Result := True;
end;

class function TFileSystem.GetFullPathName(const _fn: string): string;
var
  Buffer: array[0..MAX_PATH] of Char;
  Res: DWORD;
  LastError: DWORD;
  NamePart: PChar;
begin
  Res := Windows.GetFullPathName(PChar(_fn), MAX_PATH, @Buffer[0], NamePart);
  if Res = 0 then begin
    LastError := GetLastError;
    RaiseLastOSErrorEx(LastError, _('%s (code %d) calling Windows.GetFullPathName'));
  end;
  if Res >= MAX_PATH then
    raise Exception.CreateFmt(_('Windows.GetFullPathName returned a length > MAX_PATH (%d)'), [Res]);
  Result := PChar(@Buffer);
end;

class function TFileSystem.IsSameFilename(const _fn1, _fn2: string): Boolean;
var
  fn1: string;
  fn2: string;
begin
  fn1 := GetFullPathName(_fn1);
  fn2 := GetFullPathName(_fn2);
  Result := SameText(fn1, fn2);
end;

class function TFileSystem.IsValidFilename(const _s: string; out _ErrPos: Integer;
  _AllowDot: Boolean = True; _AllowPathChars: Boolean = False): Boolean;
var
  i: Integer;
  NotAllowed: TCharSet;
begin
  Result := False;

  if _s = '' then begin
    _ErrPos := 0;
    Exit; //==>
  end;

  if Length(_s) > MAX_PATH then begin
    _ErrPos := MAX_PATH;
    Exit; //==>
  end;

  NotAllowed := INVALID_FILENAME_CHARS;
  if not _AllowDot then
    Include(NotAllowed, '.');

  if _AllowPathChars then
    Exclude(NotAllowed, '\');

  for i := 1 to Length(_s) do begin
    if CharInSet(_s[i], NotAllowed) then begin
      if not _AllowPathChars or (i <> 2) or (_s[i] <> ':') or not CharInSet(UpCase(_s[1]), ['A'..'Z']) then begin
        _ErrPos := i;
        Exit; //==>
      end;
    end;
  end;
  Result := True;
end;

class function TFileSystem.IsValidFilename(const _s: string; _AllowDot: Boolean = True;
  _AllowPathChars: Boolean = False): Boolean;
var
  ErrPos: Integer;
begin
  Result := IsValidFilename(_s, ErrPos, _AllowDot, _AllowPathChars);
end;

class function TFileSystem.MakeValidFilename(const _s: string; _ReplaceChar: Char = '_';
  _AllowPathChars: Boolean = True): string;
var
  i: Integer;
  InvalidChars: set of AnsiChar;
begin
  Result := _s;
  InvalidChars := INVALID_FILENAME_CHARS;
  if _AllowPathChars then
    InvalidChars := InvalidChars - ['\'];
  for i := 1 to Length(Result) do begin
    if CharInSet(Result[i], InvalidChars) then begin
      if not _AllowPathChars or (i <> 2) or (Result[2] <> ':') or not CharInSet(UpCase(Result[1]), ['A'..'Z']) then
        Result[i] := _ReplaceChar;
    end;
  end;
end;

// taken from
// http://stackoverflow.com/a/5330691/49925
const
  shlwapi32 = 'shlwapi.dll';

function PathIsRelativeAPI(pszPath: PChar): BOOL; stdcall; external shlwapi32
{$IFDEF UNICODE}
Name 'PathIsRelativeW';
{$ELSE}
Name 'PathIsRelativeA';
{$ENDIF}

function PathCanonicalize(pszBuf: PChar; pszPath: PChar): BOOL; stdcall; external shlwapi32
{$IFDEF UNICODE}
Name 'PathCanonicalizeW';
{$ELSE}
Name 'PathCanonicalizeA';
{$ENDIF}

class function TFileSystem.PathIsRelative(const _Filename: string): Boolean;
begin
  Result := PathIsRelativeAPI(PChar(_Filename));
end;

class function TFileSystem.ExpandFileNameRelBaseDir(const _Filename, _BaseDir: string): string;
var
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  if PathIsRelative(_Filename) then begin
    Result := IncludeTrailingPathDelimiter(_BaseDir) + _Filename;
  end else begin
    Result := _Filename;
  end;
  if PathCanonicalize(@Buffer[0], PChar(Result)) then begin
    Result := Buffer;
  end;
end;

class function TFileSystem.ExtractFileExtFull(const _Filename: string; _IncludeDot: Boolean = True): string;
var
  p: Integer;
  fn: string;
begin
  fn := SysUtils.ExtractFileName(_Filename);
  p := Pos('.', fn);
  if p = 0 then
    Result := ''
  else begin
    if _IncludeDot then
      Result := TailStr(fn, p)
    else
      Result := TailStr(fn, p + 1);
  end;
end;

class function TFileSystem.ExtractFileExtLast(const _Filename: string; _IncludeDot: Boolean = True): string;
begin
  Result := SysUtils.ExtractFileExt(_Filename);
  if not _IncludeDot then
    Result := TailStr(Result, 2);
end;

class function TFileSystem.RemoveFileExtFull(const _Filename: string): string;
var
  Path: string;
  fn: string;
  p: Integer;
begin
  Path := ExtractFilePath(_Filename);
  fn := ExtractFileName(_Filename);
  p := Pos('.', fn);
  if p = 0 then
    Result := fn
  else
    Result := LeftStr(fn, p - 1);

  if Path <> '' then begin
    itpd(Path);
    Result := Path + Result;
  end;
end;

class function TFileSystem.RemoveFileExtLast(const _Filename: string): string;
begin
  Result := ChangeFileExtLast(_Filename, '');
end;

class function TFileSystem.ChangeFileExtFull(const _Filename: string; const _NewExt: string): string;
begin
  Result := RemoveFileExtFull(_Filename) + _NewExt;
end;

class function TFileSystem.ChangeFileExtLast(const _Filename, _NewExt: string): string;
begin
  Result := SysUtils.ChangeFileExt(_Filename, _NewExt);
end;

{ TProgressRedir }

constructor TProgressRedir.Create(_OnProgress: TCopyFileProgressEvt);
begin
  inherited Create;
  FOnProgress := _OnProgress;
end;

function TProgressRedir.doProgress(): TCopyProgressResult;
begin
  Result := prContinue;
  try
    if Assigned(FOnProgress) then
      FOnProgress(Self, Result);
  except
    on e: Exception do begin
      FExceptAddr := ExceptAddr;
      FExceptMsg := e.Message;
      FExceptClass := e.ClassName;
    end;
  end;
end;

{ TFileGenerationHandler }

constructor TFileGenerationHandler.Create(const _BaseName, _Suffix: string);
begin
  inherited Create;
  FMaxGenerations := 5;
  FOldestIsHighest := True;
  FResultContainsNumber := False;
  FPrependZeros := 0;
  FBaseName := _BaseName;
  FSuffix := _Suffix;
end;

function TFileGenerationHandler.Execute(_KeepOriginal: Boolean): string;

  function doNoNumberOldIsHighest(): string;
  var
    i: Integer;
    dst: string;
    src: string;
    MaxGen: Integer;
  begin
    MaxGen := FMaxGenerations - 1;
    for i := MaxGen - 1 downto 1 do begin
      dst := GenerateFilename(i + 1);
      if FileExists(dst) then
        TFileSystem.DeleteFile(dst);
      src := GenerateFilename(i);
      if FileExists(src) then
        TFileSystem.MoveFile(src, dst);
    end;
    dst := GenerateFilename(1);
    Result := GenerateFilename(0);
    if FileExists(dst) then
      TFileSystem.DeleteFile(dst);
    if FileExists(Result) then begin
      if _KeepOriginal then
        TFileSystem.CopyFile(Result, dst, cfeFailIfExists)
      else
        TFileSystem.MoveFile(Result, dst);
    end;
  end;

  function doNumberOldIsHighest(): string;
  var
    i: Integer;
    dst: string;
    src: string;
    MaxGen: Integer;
  begin
    MaxGen := FMaxGenerations;
    for i := MaxGen - 1 downto 1 do begin
      dst := GenerateFilename(i + 1);
      if FileExists(dst) then
        TFileSystem.DeleteFile(dst);
      src := GenerateFilename(i);
      if FileExists(src) then
        TFileSystem.MoveFile(src, dst);
    end;
    Result := GenerateFilename(1);
  end;

  function doNoNumberOldIsLowest(): string;
  var
    i: Integer;
    MaxGen: Integer;
    src: string;
    dst: string;
    SlotFound: Boolean;
  begin
    Result := GenerateFilename(0);
    if not FileExists(Result) then
      Exit;

    SlotFound := False;
    MaxGen := FMaxGenerations - 1;
    for i := 1 to MaxGen do begin
      dst := GenerateFilename(i);
      if not FileExists(dst) then begin
        SlotFound := True;
        Break;
      end;
    end;

    if not SlotFound then begin
      dst := GenerateFilename(1);
      if FileExists(dst) then
        TFileSystem.DeleteFile(dst);
      for i := 2 to MaxGen do begin
        src := GenerateFilename(i);
        if FileExists(src) then
          TFileSystem.MoveFile(src, dst);
        dst := src;
      end;
    end;

    if _KeepOriginal then
      TFileSystem.CopyFile(Result, dst, cfeFailIfExists)
    else
      TFileSystem.MoveFile(Result, dst);
  end;

  function doNumberOldIsLowest(): string;
  var
    i: Integer;
    MaxGen: Integer;
  begin
    MaxGen := FMaxGenerations;
    for i := 1 to MaxGen do begin
      Result := GenerateFilename(i);
      if not FileExists(Result) then
        Exit;
    end;

    TFileSystem.DeleteFile(GenerateFilename(1));
    for i := 2 to MaxGen do begin
      TFileSystem.MoveFile(GenerateFilename(i), GenerateFilename(i - 1));
    end;
    Result := GenerateFilename(MaxGen);
    if _KeepOriginal then
      TFileSystem.CopyFile(GenerateFilename(MaxGen - 1), Result, cfeFailIfExists);
  end;

begin
  if FResultContainsNumber then begin
    if _KeepOriginal then
      raise EInvalidPropertyCombination.Create(_('Combination of ResultContainsNumber and KeepOriginal is not allowed'));
    if FOldestIsHighest then begin
      Result := doNumberOldIsHighest();
    end else begin
      Result := doNumberOldIsLowest();
    end;
  end else begin
    if FOldestIsHighest then begin
      Result := doNoNumberOldIsHighest();
    end else begin
      Result := doNoNumberOldIsLowest();
    end;
  end;
end;

function TFileGenerationHandler.GenerateFilename(_Generation: Integer): string;
begin
  if _Generation = 0 then
    Result := FBaseName + FSuffix
  else begin
    if FPrependZeros = 0 then
      Result := FBaseName + '_' + IntToStr(_Generation) + FSuffix
    else
      Result := Format('%s_%.*u%s', [FBaseName, FPrependZeros, _Generation, FSuffix]);
  end;
  if Assigned(FOnGenerateFilename) then
    FOnGenerateFilename(Self, _Generation, Result);
end;

{ TDirectorySync }

//procedure TDirectorySync.doOnDifferentFileExists(const _Filename: string; var _Action: TFileExistsAction);
//begin
//  _Action := feaIgnore;
//  if Assigned(FOnDifferentFileExists) then
//    FOnDifferentFileExists(_Filename, _Action);
//end;

function TDirectorySync.doOnFileExists(const _SrcDir, _DstDir, _Filename: string): TFileExistsAction;
var
  src: TFileInfoRec;
  dst: TFileInfoRec;
begin
  Result := feaIgnore;
  if not Assigned(FOnFileExists) then
    Exit;
  if not TFileSystem.TryGetFileInfo(_SrcDir + _Filename, src) then
    Exit;
  if not TFileSystem.TryGetFileInfo(_DstDir + _Filename, dst) then
    Exit;

  FOnFileExists(Self, src, dst, Result);
end;

function TDirectorySync.doOnQueryFileSync(const _SrcFile, _DstFile: string): TQueryFileSyncAction;
var
  src: TFileInfoRec;
begin
  Result := fsaCopy;
  if not Assigned(FOnQueryFileSync) then
    Exit;

  if not TFileSystem.TryGetFileInfo(_SrcFile, src) then begin
    // File vanished
    Result := fsaSkip;
    Exit;
  end;
  FOnQueryFileSync(Self, src, _DstFile, Result);
end;

procedure TDirectorySync.doOnSyncingDir(const _SrcDir, _DstDir: string);
begin
  if Assigned(FOnSyncingDir) then
    FOnSyncingDir(Self, _SrcDir, _DstDir);
end;

procedure TDirectorySync.doOnSyncingFile(const _SrcFile, _DstFile: string; _Total, _Done: Int64);
begin
  if Assigned(FOnSyncingFile) then
    FOnSyncingFile(Self, _SrcFile, _DstFile, _Total, _Done);
end;

procedure TDirectorySync.ProgressStatusCallback(_Status: TCopyProgressStatus;
  var _Continue: TCopyProgressResult);
begin
  try
    doOnSyncingFile(FCurrentSource, FCurrentDest, _Status.TotalFileSize.QuadPart, _Status.TotalBytesTransferred.QuadPart);
  except
    on e: EAbort do
      _Continue := prCancel;
  end;
end;

procedure TDirectorySync.CheckOneWay(const _SrcDir, _DstDir: string);
var
  Filename: string;
  EnumA: TSimpleDirEnumerator;
  DstDirBS: string;
  SrcDirBs: string;
begin
  doOnSyncingDir(_SrcDir, _DstDir);
  SrcDirBs := itpd(_SrcDir);
  DstDirBS := itpd(_DstDir);
  EnumA := TSimpleDirEnumerator.Create(SrcDirBs + '*.*');
  try
    while EnumA.FindNext(Filename) do begin
      if (EnumA.Sr.Attr and SysUtils.faDirectory) <> 0 then begin
        CheckOneWay(SrcDirBs + Filename, DstDirBS + Filename);
      end else if FileExists(DstDirBS + Filename) then begin
        doOnFileExists(SrcDirBs, DstDirBS, Filename);
      end else begin
        doOnSyncingFile(SrcDirBs + Filename, DstDirBS + Filename, EnumA.Sr.Size, 0);
      end;
    end;
  finally
    FreeAndNil(EnumA);
  end;
end;

procedure TDirectorySync.SyncOneWay(const _SrcDir, _DstDir: string; _FlattenDirHierarchy: Boolean = False);
const
  TMPEXT = '.tmp';
var
  Filename: string;
  EnumA: TSimpleDirEnumerator;
  DstDirBS: string;
  SrcDirBs: string;
  Destination: string;
  TempFile: string;
begin
  doOnSyncingDir(_SrcDir, _DstDir);
  SrcDirBs := itpd(_SrcDir);
  DstDirBS := itpd(_DstDir);
  if not DirectoryExists(DstDirBS) then
    TFileSystem.ForceDir(DstDirBS);
  EnumA := TSimpleDirEnumerator.Create(SrcDirBs + '*.*');
  try
    while EnumA.FindNext(Filename) do begin
      FCurrentSource := SrcDirBs + Filename;
      if (EnumA.Sr.Attr and SysUtils.faDirectory) <> 0 then begin
        if _FlattenDirHierarchy then
          FCurrentDest := _DstDir
        else begin
          FCurrentDest := DstDirBS + Filename;
        end;
        SyncOneWay(FCurrentSource, FCurrentDest, _FlattenDirHierarchy);
      end else begin
        if not SameText(ExtractFileExt(Filename), TMPEXT) then begin // do not sync .tmp files
          Destination := DstDirBS + Filename;
          TempFile := Destination + TMPEXT;
          if FileExists(TempFile) then
            TFileSystem.DeleteFile(TempFile, False);

          if FileExists(Destination) then begin
            if doOnFileExists(SrcDirBs, DstDirBS, Filename) = feaOverwrite then begin
              TFileSystem.DeleteFile(Destination, False);
              // copy to temp
              FCurrentDest := TempFile;
              doOnSyncingFile(FCurrentSource, FCurrentDest, EnumA.Sr.Size, 0);
              if cfwOK <> TFileSystem.CopyFileWithProgress(FCurrentSource, FCurrentDest, ProgressStatusCallback, []) then
                SysUtils.Abort;

              // move to dest
              FCurrentSource := TempFile;
              FCurrentDest := Destination;
              doOnSyncingFile(FCurrentSource, FCurrentDest, EnumA.Sr.Size, 0);
              if cfwOK <> TFileSystem.MoveFileWithProgress(FCurrentSource, FCurrentDest, ProgressStatusCallback, []) then
                SysUtils.Abort;
            end;
          end else if doOnQueryFileSync(FCurrentSource, FCurrentDest) = fsaCopy then begin
            // copy to temp
            FCurrentDest := TempFile;
            doOnSyncingFile(FCurrentSource, FCurrentDest, EnumA.Sr.Size, 0);
            if cfwOK <> TFileSystem.CopyFileWithProgress(FCurrentSource, FCurrentDest, ProgressStatusCallback, []) then
              SysUtils.Abort;
            // move to dest
            FCurrentSource := TempFile;
            FCurrentDest := Destination;
            // todo: Should that really be displayed? Usually the move operation is so fast that you can't see this anyway.
            doOnSyncingFile(FCurrentSource, FCurrentDest, EnumA.Sr.Size, 0);
            if cfwOK <> TFileSystem.MoveFileWithProgress(FCurrentSource, FCurrentDest, ProgressStatusCallback, [mfwFailIfExists]) then
              SysUtils.Abort;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(EnumA);
  end;
end;

procedure TDirectorySync.SyncBothWays(const _DirA, _DirB: string);
begin
  SyncOneWay(_DirA, _DirB);
  SyncOneWay(_DirB, _DirA);
end;

{ TUniqueTempDir }

constructor TUniqueTempDir.Create(const _Path: string; _DeleteOnlyIfEmpty: Boolean = False);
begin
  inherited Create;
  FPath := _Path;
  FDeleteOnlyIfEmpty := _DeleteOnlyIfEmpty;
end;

destructor TUniqueTempDir.Destroy;
begin
  // delete directory, fail silently on errors
  if FDeleteOnlyIfEmpty then
    TFileSystem.RemoveDir(FPath, False)
  else
    TFileSystem.DelDirTree(FPath, False);
  inherited;
end;

function TUniqueTempDir.Path: string;
begin
  Result := FPath;
end;

function TUniqueTempDir.PathBS: string;
begin
  Result := itpd(FPath);
end;

procedure TextFile_AssignAndRewrite(var _File: TextFile; const _Filename: string);
begin
  ZeroMemory(@_File, SizeOf(_File));
  AssignFile(_File, _Filename);
{$I-}
  Rewrite(_File);
{$I+}
  if IOResult <> 0 then begin
    ZeroMemory(@_File, SizeOf(_File));
    raise Exception.CreateFmt(_('Could not open file "%s" for writing.'), [_Filename]);
  end;
end;

function TextFile_IsOpen(const _File: TextFile): Boolean;
begin
  Result := (TTextRec(_File).Mode <> 0);
end;

procedure TextFile_Close(var _File: TextFile);
begin
  CloseFile(_File);
  TTextRec(_File).Mode := 0;
end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
{ TFilename }

class operator TFilename.Add(const _a: TFilename; const _b: string): string;
begin
  Result := _a.Full + _b;
end;

class operator TFilename.Add(const _a: string; const _b: TFilename): string;
begin
  Result := _a + _b.Full;
end;

class operator TFilename.Implicit(const _s: string): TFilename;
begin
  Result.Init(_s);
end;

class operator TFilename.Implicit(_a: TFilename): string;
begin
  Result := _a.Full;
end;

procedure TFilename.Init(const _Full: string);
begin
  FFull := _Full;
end;

function TFilename.Drive: string;
begin
  Result := ExtractFileDrive(FFull);
end;

function TFilename.Directory: string;
begin
  Result := ExtractFileDir(FFull);
end;

function TFilename.DirectoryBS: string;
begin
  Result := Directory;
  if Result <> '' then
    Result := itpd(Directory);
end;

function TFilename.Filename: string;
begin
  Result := ExtractFileName(FFull);
end;

function TFilename.FilenameOnly: string;
begin
  Result := TFileSystem.RemoveFileExtFull(Filename);
end;

function TFilename.Extension: string;
begin
  Result := TFileSystem.ExtractFileExtFull(FFull);
end;

function TFilename.LastExtension: string;
begin
  Result := TFileSystem.ExtractFileExtLast(FFull);
end;

function TFilename.GetExtensions: TStringArray;
var
  Ext: string;
  p: Integer;
  len: Integer;
  i: Integer;
begin
  SetLength(Result, 0);

  Ext := Extension;
  len := 0;
  p := RPosStr('.', Ext);
  while p > 0 do begin
    Inc(len);
    SetLength(Result, len);
    for i := len - 1 downto 1 do
      Result[i] := Result[i - 1];
    Result[0] := TailStr(Ext, p);
    Ext := LeftStr(Ext, p - 1);
    p := RPosStr('.', Ext);
  end;
end;

function TFilename.GetParts(_sl: TStrings): Integer;
var
  s: string;
  p: Integer;
  Part: string;
begin
  Assert(Assigned(_sl), 'sl parameter not assigned');

  _sl.Clear;
  s := FFull;
  Result := 0;
  Part := '';
  while s <> '' do begin
    if Result = 0 then begin
      if StartsStr(PathDelim + PathDelim, s) then begin //FI:W510
        // UNC path
        p := PosEx(PathDelim, s, 3);
        if p > 0 then
          p := PosEx(PathDelim, s, p + 1);
        if p > 0 then begin
          _sl.Add(Copy(s, 1, p - 1));
          s := Copy(s, p + 1);
        end else begin
          _sl.Add(s);
          s := '';
        end;
      end else begin
        // no UNC, might be a drive, but could also be '\bla\blub' or 'bla\blub'
        _sl.Add(ExtractStr(s, PathDelim));
      end;
    end else begin
      _sl.Add(ExtractStr(s, PathDelim));
    end;
    Inc(Result);
  end;
  Assert(_sl.Count = Result);
end;

function TFilename.Split: TStringArray;

  procedure Append(const _s: string);
  var
    len: Integer;
  begin
    len := Length(Result);
    SetLength(Result, len + 1);
    Result[len] := _s;
  end;

var
  s: string;
  p: Integer;
  Part: string;
begin
  SetLength(Result, 0);
  s := FFull;
  Part := '';
  while s <> '' do begin
    if Length(Result) = 0 then begin
      if StartsStr(PathDelim + PathDelim, s) then begin //FI:W510
        // UNC path
        p := PosEx(PathDelim, s, 3);
        if p > 0 then
          p := PosEx(PathDelim, s, p + 1);
        if p > 0 then begin
          Append(Copy(s, 1, p - 1));
          s := Copy(s, p + 1);
        end else begin
          Append(s);
          s := '';
        end;
      end else begin
        // no UNC, might be a drive, but could also be '\bla\blub' or 'bla\blub'
        Append(ExtractStr(s, PathDelim));
      end;
    end else begin
      Append(ExtractStr(s, PathDelim));
    end;
  end;
end;

function TFilename.BaseName: string;
begin
  Result := TFileSystem.RemoveFileExtFull(FFull);
end;

class function TFilename.Combine(_Parts: TStringArray): TFilename;
var
  len: Integer;
  i: Integer;
  s: string;
begin
  len := Length(_Parts);
  if len = 0 then
    Result := ''
  else begin
    s := _Parts[0];
    for i := 1 to len - 1 do
      s := s + '\' + _Parts[i];
    Result := s;
  end;
end;

function TFilename.Depth: Integer;
begin
  Result := Length(Split);
end;

function TFilename.HasExtensionFull(const _Ext: string): Boolean;
begin
  Result := TFileSystem.HasFileExtFull(FFull, _Ext);
end;

function TFilename.HasExtensionLast(const _Ext: string): Boolean;
begin
  Result := TFileSystem.HasFileExtLast(FFull, _Ext);
end;

function TFilename.Parts(_Depth: Integer): string;
var
  sa: TStringArray;
  len: Integer;
  i: Integer;
begin
  sa := Split;
  len := Length(sa);
  if _Depth < 0 then
    _Depth := len + _Depth;
  if _Depth < 0 then begin
    Result := '';
    Exit; //==>
  end;
  Result := '';
  if _Depth > len then
    _Depth := len;
  for i := 0 to _Depth - 1 do begin
    if Result <> '' then
      Result := Result + PathDelim + sa[i]
    else
      Result := sa[i];
  end;
end;

procedure TFilename.ReplaceDrive(const _NewDrive: string);
var
  drv: string;
begin
  drv := ExtractFileDrive(FFull);
  if drv = '' then
    raise Exception.CreateFmt(_('Path %s does not contain a drive, cannot replace it.'), [FFull]);
  FFull := _NewDrive + TailStr(FFull, Length(drv) + 1);
end;

procedure TFilename.ReplaceDirectory(const _NewDir: string);
var
  s: string;
begin
  s := _NewDir;
  if s <> '' then
    s := itpd(s);
  FFull := s + Filename;
end;

procedure TFilename.ReplaceExtension(const _Extension: string);
begin
  FFull := TFileSystem.ChangeFileExtFull(FFull, _Extension);
end;

procedure TFilename.ReplaceExtensions(const _Extensions: TStringArray);
var
  i: Integer;
  Ext: string;
begin
  Ext := '';
  for i := 0 to Length(_Extensions) - 1 do begin
    Ext := Ext + _Extensions[i];
  end;
  ReplaceExtension(Ext);
end;

procedure TFilename.ReplaceFilename(const _Filename: string);
begin
  FFull := DirectoryBS + _Filename;
end;

procedure TFilename.ReplaceFilenameOnly(const _FilenameOnly: string);
begin
  FFull := DirectoryBS + _FilenameOnly + Extension;
end;

procedure TFilename.ReplaceLastExtension(const _Extension: string);
begin
  FFull := TFileSystem.ChangeFileExtLast(FFull, _Extension);
end;

function TFilename.TryGetRootDir(out _Root: string): Boolean;
begin
  Result := Length(FFull) >= 3;
  if Result and StartsStr(PathDelim + PathDelim, FFull) then begin //FI:W510
    Result := True;
    _Root := Parts(1);
  end else if Result and CharInSet(UpCase(FFull[1]), ['A'..'Z']) and (FFull[2] = ':') and (FFull[3] = PathDelim) then begin
    Result := True;
    _Root := itpd(Parts(1));
  end else
    Result := False;
end;

{ TSearchPath }

class operator TSearchPath.Implicit(const _s: string): TSearchPath;
begin
  Result.Init(_s);
end;

class operator TSearchPath.Implicit(_a: TSearchPath): string;
begin
  Result := _a.Value;
end;

class operator TSearchPath.Implicit(_sl: TStrings): TSearchPath;
begin
  Result.AssignParts(_sl);
end;

class operator TSearchPath.Add(_a, _b: TSearchPath): TSearchPath;
var
  sla: TStringList;
  slb: TStringList;
begin
  slb := nil;
  sla := _a.AsStringlist;
  try
    slb := _b.AsStringlist;
    sla.AddStrings(slb);
    Result.FValue := sla.DelimitedText;
  finally
    FreeAndNil(slb);
    FreeAndNil(sla);
  end;
end;

class operator TSearchPath.Add(_a: TSearchPath; const _b: string): TSearchPath;
var
  sl: TStringList;
begin
  sl := _a.AsStringlist;
  try
    sl.Add(_b);
    Result.FValue := sl.DelimitedText;
  finally
    FreeAndNil(sl);
  end;
end;

function TSearchPath.AnyDirExists: Boolean;
var
  sl: TStringList;
  i: Integer;
begin
  Result := True;
  sl := AsStringlist;
  try
    for i := 0 to sl.Count - 1 do begin
      if sl[i] <> '' then
        if TFileSystem.DirExists(sl[i]) then
          Exit; //==>
    end;
  finally
    FreeAndNil(sl);
  end;
  Result := False;
end;

function TSearchPath.AllDirsExist(out _FirstNonExistingDir: string): Boolean;
var
  i: Integer;
  sl: TStringList;
begin
  sl := AsStringlist;
  try
    for i := 0 to sl.Count - 1 do begin
      if sl[i] <> '' then
        if not TFileSystem.DirExists(sl[i]) then begin
          Result := False;
          _FirstNonExistingDir := sl[i];
          Exit; //=>
        end;
    end;
  finally
    FreeAndNil(sl);
  end;
  Result := True;
end;

procedure TSearchPath.AssignParts(_Parts: TStrings);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.StrictDelimiter := True;
    sl.QuoteChar := '"';
    sl.Delimiter := ';';
    sl.AddStrings(_Parts);
    FValue := sl.DelimitedText;
  finally
    FreeAndNil(sl);
  end;
end;

function TSearchPath.AsStringlist: TStringList;
begin
  Result := TStringList.Create;
  Result.StrictDelimiter := True;
  Result.QuoteChar := '"';
  Result.Delimiter := ';';
  Result.DelimitedText := Value;
end;

class operator TSearchPath.Equal(_a, _b: TSearchPath): Boolean;
begin
  Result := _a.Value = _b.Value;
end;

class operator TSearchPath.NotEqual(_a, _b: TSearchPath): Boolean;
begin
  Result := not (_a = _b);
end;

function TSearchPath.Part(_Idx: Integer): string;
var
  sl: TStringList;
begin
  sl := AsStringlist;
  try
    Result := sl[_Idx];
  finally
    FreeAndNil(sl);
  end;
end;

function TSearchPath.PartCount: Integer;
var
  sl: TStringList;
begin
  sl := AsStringlist;
  try
    Result := sl.Count;
  finally
    FreeAndNil(sl);
  end;
end;

function TSearchPath.TryFindFile(const _FilenameOnly: string; out _FoundFile: TFilename): Boolean;
var
  sl: TStringList;
  i: Integer;
  fn: TFilename;
  dir: string;
begin
  fn := _FilenameOnly;
  sl := AsStringlist;
  try
    for i := 0 to sl.Count - 1 do begin
      dir := sl[i];
      if dir <> '' then begin
        fn.ReplaceDirectory(dir);
        Result := TFileSystem.FileExists(fn);
        if Result then begin
          _FoundFile := fn;
          Exit;
        end;
      end;
    end;
  finally
    FreeAndNil(sl);
  end;
  Result := False;
end;

function TSearchPath.FindFile(const _FilenameOnly: string): TFilename;
begin
  if not TryFindFile(_FilenameOnly, Result) then
    raise EFileNotFound.CreateFmt(_('Could not find file "%s" in search path "%s".'),
      [_FilenameOnly, Value]);
end;

procedure TSearchPath.GetParts(_Parts: TStrings);
var
  sl: TStringList;
begin
  sl := AsStringlist;
  try
    _Parts.Assign(sl);
  finally
    FreeAndNil(sl);
  end;
end;

function TSearchPath.GetParts: TStringArray;
var
  sl: TStringList;
begin
  sl := AsStringlist;
  try
    Result := TStringArray_FromStrings(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TSearchPath.Init(const _s: string);
begin
  FValue := _s;
end;

function TSearchPath.Value: string;
begin
  Result := FValue;
end;
{$ENDIF}

initialization
  // according to MSDN:
  // Best practice is that all applications call the process-wide SetErrorMode function
  // with a parameter of SEM_FAILCRITICALERRORS at startup. This is to prevent error
  // mode dialogs from hanging the application.
  SetErrorMode(SEM_FAILCRITICALERRORS);
end.
