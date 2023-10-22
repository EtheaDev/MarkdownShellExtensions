unit u_dzSaxParser;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ActiveX,
  ComObj,
  MSXML2_TLB,
  u_dzTranslator;

const
  // idea taken from Delphi 10.1 unit System.Win.ComObj:
  EExceptionRaisedHRESULT = HResult(E_UNEXPECTED or (1 shl 29)); // turn on customer bit

type
  TDocumentLocation = (dlbNoBraces, dlbBraces, dlbBracesAndSpace);
  TDocumentLocationDecoration = (dlcBraces, dlcSpaceBefore, dlcSpaceAfter);
  TDocumentLocationDecorationSet = set of TDocumentLocationDecoration;

type
  ///<summary>
  /// Use this class as an ancestor SAX parsers. It handles ECatastrophicFailure exceptions in a way
  /// that gives you the original exception and even stops the debugger at the correct source
  /// position. It also implements all those methods that are IVBSAXContentHandler and IDispatch
  /// methods that are usually left empty so the descencants. These usually only need to
  /// overide the startElement and endElement methods
  ///   @longcode(##
  ///     type
  ///       TMySaxParser = class(TdzSaxParser)
  ///       protected
  ///         procedure startElement(var strNamespaceURI: WideString; var strLocalName: WideString;
  ///           var strQName: WideString; const oAttributes: IVBSAXAttributes); override;
  ///         procedure endElement(var strNamespaceURI: WideString; var strLocalName: WideString;
  ///           var strQName: WideString); override;
  ///       end;
  ///       [...]
  ///     MyParser := TMySaxParser.Create;
  ///     try
  ///       MyParser.ParseFile(SomeFilename);
  ///     finally
  ///       MyParser.Free;
  ///     end;
  ///   ##) }
  /// </summary>
  TdzSaxParser = class(TComponent, IVBSAXContentHandler, IDispatch)
  protected
    ///<summary>
    /// Initialized in method SafeCallException with its ExceptionObject parameter.
    /// Used in ParseUrl to re-raise the exception. </summary>
    FExceptObject: TObject;
    ///<summary>
    /// Initialized in method SafeCallException with its ExceptionAddr parameter.
    /// Used in ParseUrl to re-raise the exception. </summary>
    FExceptAddr: Pointer;
    FDocumentLocationDecoration: TDocumentLocationDecorationSet;
    FDocumentLocator: IVBSAXLocator;
    FXmlFile: WideString;
    ///<summary>
    /// @returns a localized location string of the form 'line: %d column: %d file: %s'
    /// @NOTE: If FDocumentLocator is not assigned (normally should automatically do that)
    ///        the string will only be of the form 'file: %s'.
    /// @NOTE: The property DocumentLocationDecoration determines whether the string is enclosed
    ///        in braces and a spaces is prepended or appended </summary>
    function GetDocumentLocationStr: string;
  protected // implements IDispatch, all methods are dummies
    ///<summary> Dummy method, returns S_FALSE </summary>
    function GetTypeInfoCount(out Count: Integer): HResult; virtual; stdcall;
    ///<summary> Dummy method, returns S_FALSE </summary>
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; virtual; stdcall;
    ///<summary> Dummy method, returns S_FALSE </summary>
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; virtual; stdcall;
    ///<summary> Dummy method, returns S_FALSE </summary>
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; virtual; stdcall;
  protected // implements IVBSAXContentHandler, many methods are dummies
    ///<summmary> Dummy method, does nothing </summary>
    procedure _Set_documentLocator(const Param1: IVBSAXLocator); virtual; safecall;
    ///<summmary> Dummy method, does nothing </summary>
    procedure startDocument; virtual; safecall;
    ///<summmary> Dummy method, does nothing </summary>
    procedure endDocument; virtual; safecall;
    ///<summmary> Dummy method, does nothing </summary>
    procedure startPrefixMapping(var strPrefix: WideString; var strURI: WideString); virtual; safecall;
    ///<summmary> Dummy method, does nothing </summary>
    procedure endPrefixMapping(var strPrefix: WideString); virtual; safecall;
    ///<summmary> Dummy method, does nothing </summary>
    procedure characters(var strChars: WideString); virtual; safecall;
    ///<summmary> Dummy method, does nothing </summary>
    procedure ignorableWhitespace(var strChars: WideString); virtual; safecall;
    ///<summmary> Dummy method, does nothing </summary>
    procedure processingInstruction(var strTarget: WideString; var strData: WideString); virtual; safecall;
    ///<summmary> Dummy method, does nothing </summary>
    procedure skippedEntity(var StrName: WideString); virtual; safecall;
    ///<summary>
    /// Called on start of an XML element. You usually want to override this </summary>
    procedure startElement(var strNamespaceURI: WideString; var strLocalName: WideString;
      var strQName: WideString; const oAttributes: IVBSAXAttributes); virtual; safecall;
    ///<summary>
    /// Called on start of an XML element. You usually want to override this </summary>
    procedure endElement(var strNamespaceURI: WideString; var strLocalName: WideString;
      var strQName: WideString); virtual; safecall;
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    ///<summary>
    /// Parses the given file and calls the IVBSAXContentHandler methods </summary>
    procedure ParseFile(const _Filename: string);
    ///<summary>
    /// For Delphi Exceptions saves ExceptObject and ExceptAddr in the fields FExceptObject
    /// and FExceptAddress so they can be used in ParseFile. For other exceptions, calls
    /// ComObj.HandleSafeCallException.
    /// @returns EExceptionRaisedHRESULT for Delphi exceptions or the result of a call to
    ///          HandleSafeCallException for other exceptions.
    /// see also https://stackoverflow.com/a/38993751 </summary>
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
    ///<summary>
    /// Determines the format of the string returned by GetDocumentLocationStr </summary>
    property DocumentLocationDecoration: TDocumentLocationDecorationSet read
      FDocumentLocationDecoration write FDocumentLocationDecoration;
  end;

implementation

function _(const _s: string): string; inline;
begin
  Result := dzlibGetText(_s);
end;

{ TdzSaxParser }

constructor TdzSaxParser.Create(_Owner: TComponent);
begin
  inherited;
  CoInitialize(nil);
end;

destructor TdzSaxParser.Destroy;
begin
  CoUninitialize;
  inherited;
end;

function TdzSaxParser.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;
var
  GUID: TGUID;
  exc: Exception;
begin
  if ExceptObject is Exception then begin
    exc := Exception(ExceptObject);
    FExceptObject := exc.NewInstance;
    Exception(FExceptObject).Create(exc.Message);
    Exception(FExceptObject).HelpContext := exc.HelpContext;
    FExceptAddr := ExceptAddr;
    Result := EExceptionRaisedHRESULT;
  end else begin
    ZeroMemory(@GUID, SizeOf(GUID));
    Result := HandleSafeCallException(ExceptObject, ExceptAddr, GUID, '', '');
  end;
end;

procedure TdzSaxParser.ParseFile(const _Filename: string);
var
  exc: Exception;
  XMLReader: IVBSAXXMLReader;
begin
  FXmlFile := _Filename;
  try
    XMLReader := CoSAXXMLReader60.Create;
    XMLReader._Set_contentHandler(Self);
    XMLReader.ParseUrl(FXmlFile);
  except
    on e: EOleException do begin
      if e.ErrorCode = EExceptionRaisedHRESULT then begin
        if Assigned(FExceptObject) then begin
          exc := Exception(FExceptObject);
          FExceptObject := nil;
          raise exc at FExceptAddr;
        end;
      end;
      raise;
    end;
  end;
end;

function TdzSaxParser.GetDocumentLocationStr: string;
begin
  if Assigned(FDocumentLocator) then begin
    Result := Format(_('line: %d column: %d file: %s'),
      [FDocumentLocator.lineNumber, FDocumentLocator.columnNumber, ExtractFileName(FXmlFile)]);
  end else begin
    Result := Format(_('file: %s'),
      [ExtractFileName(FXmlFile)]);
  end;
  if dlcBraces in FDocumentLocationDecoration then
    Result := '(' + Result + ')';
  if dlcSpaceBefore in FDocumentLocationDecoration then
    Result := ' ' + Result;
  if dlcSpaceAfter in FDocumentLocationDecoration then
    Result := Result + ' ';
end;

procedure TdzSaxParser.characters(var strChars: WideString);
begin
  // do nothing
end;

procedure TdzSaxParser.endPrefixMapping(var strPrefix: WideString);
begin
  // do nothing
end;

function TdzSaxParser.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer;
  DispIDs: Pointer): HResult;
begin
  // do nothing
  Result := S_FALSE;
end;

function TdzSaxParser.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  // do nothing
  Result := S_FALSE;
end;

function TdzSaxParser.GetTypeInfoCount(out Count: Integer): HResult;
begin
  // do nothing
  Result := S_FALSE;
end;

procedure TdzSaxParser.ignorableWhitespace(var strChars: WideString);
begin
  // do nothing
end;

function TdzSaxParser.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word;
  var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  // do nothing
  Result := S_FALSE;
end;

procedure TdzSaxParser.processingInstruction(var strTarget, strData: WideString);
begin
  // do nothing
end;

procedure TdzSaxParser.skippedEntity(var StrName: WideString);
begin
  // do nothing
end;

procedure TdzSaxParser.startDocument;
begin
  // do nothing
end;

procedure TdzSaxParser.startElement(var strNamespaceURI, strLocalName, strQName: WideString;
  const oAttributes: IVBSAXAttributes);
begin
  // do nothing
end;

procedure TdzSaxParser.endDocument;
begin
  // do nothing
end;

procedure TdzSaxParser.endElement(var strNamespaceURI, strLocalName, strQName: WideString);
begin
  // do nothing
end;

procedure TdzSaxParser.startPrefixMapping(var strPrefix, strURI: WideString);
begin
  // do nothing
end;

procedure TdzSaxParser._Set_documentLocator(const Param1: IVBSAXLocator);
begin
  FDocumentLocator := Param1;
end;

end.
