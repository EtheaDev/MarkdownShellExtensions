unit u_dzTextFile;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  u_dzTranslator;

type
  ///<summary> defines the file access for TTextFile</summary>
  TTextFileAccess = (tfaRead, tfaWrite, tfaAppend);
  ///<summary> a wrapper class for System.TextFile</summary>
  TTextFile = class
  private
    FFile: TextFile;
    FIsFileOpen: boolean;
    FFilename: string;
  public
    ///<summary> Creates a new TTextFile instance with the given filename and file access </summary>
    constructor Create(const _Filename: string; _Access: TTextFileAccess); overload;
    ///<summary> Destroys the TTextFile instance and closes the file </summary>
    destructor Destroy; override;
    ///<summary> Writes a string to the file </summary>
    procedure Write(const _s: string);
    ///<summary> Writes a string followed by CR/LF to the file </summary>
    procedure WriteLn(const _s: string);
    ///<summary> Reads from the file up to the next CR/LF and returns the string </summary>
    function ReadLn: string;
    property Filename: string read FFilename;
  end;

implementation

{ TTextFile }

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE}inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

constructor TTextFile.Create(const _Filename: string; _Access: TTextFileAccess);
var
  EInOut: EInOutError;
begin
{$I+}
  inherited Create;
  AssignFile(FFile, _Filename);
  try
    case _Access of
      tfaRead: Reset(FFile);
      tfaWrite: Rewrite(FFile);
      tfaAppend: Append(FFile);
    else
      raise Exception.CreateFmt(_('Invalid file access value (%d)'), [Ord(_Access)]);
    end;
  except
    on e: EInOutError do begin
      EInOut := EInOutError.CreateFmt(_('%s accessing file %s'), [e.Message, _Filename]);
      EInOut.ErrorCode := e.ErrorCode;
      raise EInOut;
    end;
  end;
  FIsFileOpen := true;
end;

destructor TTextFile.Destroy;
begin
  if FIsFileOpen then
    Close(FFile);
  inherited;
end;

function TTextFile.ReadLn: string;
begin
  System.ReadLn(FFile, Result);
end;

procedure TTextFile.Write(const _s: string);
begin
  System.Write(FFile, _s);
end;

procedure TTextFile.WriteLn(const _s: string);
begin
  System.WriteLn(FFile, _s);
end;

end.

