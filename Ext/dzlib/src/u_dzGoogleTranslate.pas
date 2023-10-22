unit u_dzGoogleTranslate;

{$INCLUDE 'dzlib.inc'}
{.$INCLUDE 'dzlibjedi.inc'}

{$IFNDEF HAS_JSON_SUPPORT}
{$IFNDEF NO_JSON_SUPPORT_HINT}
{$MESSAGE HINT 'This unit requires RTL with json parser support (Delphi >= 2010)'}
{$ENDIF}
{$ENDIF ~HAS_JSON_SUPPORT}

interface

{$IFDEF HAS_JSON_SUPPORT}
uses
{$IFDEF HAS_UNIT_GENERICS_COLLECTIONS}
  System.Generics.Collections,
{$ENDIF}
  SysUtils,
{$IFDEF HAS_UNIT_NETENCODING}
  NetEncoding,
{$ENDIF}
  IdHttp; // Indy

type
  TGoogleTranslate = class
  private
    FReferrer: string;
    FUserAgent: string;
    FSourceLang: string;
    FTargetLang: string;
  public
    ///<summary>
    /// @param SourceLang is the source language code, e.g. 'en', see the API description
    ///                   for a list of possible values.
    /// @param TargetLang is the target language code, e.g. 'de', see the API description
    ///                   for a list of possible values. </summary>
    constructor Create(const _SourceLang, _TargetLang: string);
    ///<summary>The Google Translate API license requires you to display this branding near
    ///         the input and result of translations. </summary>
    function GetBranding: string;
    function Translate(const _Input: string): string;
    ///<summary> Referrer is the url to the website using the translation service </summary>
    property Referrer: string read FReferrer write FReferrer;
    ///<summary> UserAgent is the http user agent, e.g. your application's name, defaults
    ///                    to 'TGoogleTranslate'. </summary>
    property UserAgent: string read FUserAgent write FUserAgent;
  end;
{$ENDIF HAS_JSON_SUPPORT}

implementation

{$IFDEF HAS_JSON_SUPPORT}
uses
  Classes,
  Character,
  dbxjson, // database / dbx
{$IFDEF HAS_UNIT_SYSTEM_JSON}
  System.JSON,
{$ENDIF HAS_UNIT_SYSTEM_JSON}
  HttpApp,
  u_dzStringUtils,
  u_dzTranslator;

{$IFNDEF JSONOBJECT_HAS_COUNT}
// Delphi XE6 deprecated TJSONObject.Size (replaced by .Count) and .Get() (replaced by Pairs[])
// to be backwards compatible to XE5 and earlier, we introdcue this class helper.
type
  TJSONObjectHelper = class helper for TJSONObject
  private
    function GetPairs(_Idx: Integer): TJSONPair;
  public
    function Count: integer;
    property Pairs[_Idx: Integer]: TJSONPair read GetPairs;
  end;

function TJSONObjectHelper.GetPairs(_Idx: Integer): TJSONPair;
begin
  Result := Get(_Idx);
end;

function TJSONObjectHelper.Count: Integer;
begin
  Result := Size;
end;
{$ENDIF}

type
  TResponse = record
  private
    FData: string;
    FDetails: string;
    FStatus: Integer;
  public
    procedure Init(const _Answer: string);
  end;

{ TGoogleTranslate }

constructor TGoogleTranslate.Create(const _SourceLang, _TargetLang: string);
begin
  inherited Create;
  FReferrer := '';
  FUserAgent := 'TGoogleTranslate';
  FSourceLang := _SourceLang;
  FTargetLang := _TargetLang;
end;

function URLEncode(const _s: string): string;
const
  NoConversion = ['A'..'Z', 'a'..'z', '*', '@', '.', '_', '-', '/', ':', '=', '?'];
begin
  Result := UrlEncodeControlChars(_s, [#0..#255] - NoConversion);
end; // URLEncode

function ConvertToCodePage(s: string; cp: Integer): string;
var
  data: TStringStream;
  Size: Integer;
  Buffer: TBytes;
  encode: TEncoding;
begin
  if s = '' then begin
    Result := '';
    Exit;
  end;

  data := TStringStream.Create();
  try
    data.WriteString(s);
    data.Seek(0, soFromBeginning);

    Size := data.Size - data.Position;
    SetLength(Buffer, Size);
    data.ReadBuffer(Buffer[0], Size);

    encode := TEncoding.GetEncoding(cp);
    Size := TEncoding.GetBufferEncoding(Buffer, encode);
    Result := encode.GetString(Buffer, Size, Length(Buffer) - Size);
  finally
    FreeAndNil(data);
  end;
end;

function LookupCodepage(sc: string): Integer;
begin
  // Finds the codepage of each language, so that the data from Google can be converted correctly
  sc := UpperCase(sc);

  if (sc = 'ZH-CN') then
    Result := 936
  else if (sc = 'ZH-TW') then
    Result := 950
  else if (sc = 'SV') then
    Result := 28592
  else if (sc = 'TH') then
    Result := 874
  else if (sc = 'RU') then
    Result := 20866
  else if (sc = 'JA') then
    Result := 932
  else if (sc = 'EL') then
    Result := 28597
  else if (sc = 'FA') then
    Result := 1256
  else if (sc = 'KO') then
    Result := 51949
  else if (sc = 'ID') then
    Result := 28591
  else if (sc = 'UK') then
    Result := 1251
  else
    Result := 28591;
end;

function TGoogleTranslate.GetBranding: string;
begin
  Result := 'powered by Google';
end;

function TGoogleTranslate.Translate(const _Input: string): string;
const
  url_template = 'http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&langpair=%s|%s&q=%s';
var
  url: string;
//  status: string;
  s: string;
  utfs: UTF8String;
  http: TidHttp;
  Response: TResponse;
  cp: Integer;
begin
  http := TidHttp.Create;
  try
    s := URLEncode(_Input);
    utfs := UTF8String(s);
    url := Format(url_template, [FSourceLang, FTargetLang, string(utfs)]);

    http.Request.Referer := FReferrer;
    http.Request.UserAgent := FUserAgent;
    s := http.Get(url);

    Response.Init(s);
    if Response.FStatus <> 200 then
      raise Exception.Create(Response.FDetails);
    cp := LookupCodepage(FTargetLang);
    Result := ConvertToCodePage(Response.FData, cp);
  finally
    http.Free;
  end;
end;

function CharIsWhitespace(Ch: Char): Boolean; inline;
begin
{$IFDEF RTL250_UP}
  Result := Ch.IsWhiteSpace;
{$ELSE}
{$IFDEF RTL210_UP}
  Result := TCharacter.IsWhiteSpace(Ch);
{$ELSE}
  Result := TCharHelper.IsWhiteSpace(Ch);
{$ENDIF}
{$ENDIF}
end;

{ TResponse }

function StripNonJson(_s: string): string;
var
  Ch: Char;
  inString: Boolean;
begin
  Result := '';
  inString := False;
  for Ch in _s do begin
    if Ch = '"' then
      inString := not inString;
    if CharIsWhitespace(Ch) and not inString then
      continue;

    Result := Result + Ch;
  end;
end;

procedure TResponse.Init(const _Answer: string);
var
//  i: integer;
//  p: TJSONPair;
  o: TJSONObject;
  responseData: TJSONPair;
  rdo: TJSONObject;
  responseDetails: TJSONPair;
  responseStatus: TJSONPair;
  RootValue: TJSONValue;
  s: string;
begin
  FData := '';
  FDetails := '';
  FStatus := 0;

  s := StripNonJson(_Answer);

//  WriteLn(s);
//  WriteLn;

// OK:
//{"responseData":{"translatedText":"Here the German text."},"responseDetails":null,
//"responseStatus":200}

// Error:
//{"responseData": null, "responseDetails": "invalid translation language pair",
// "responseStatus": 400}

  RootValue := TJSONObject.ParseJSONValue(BytesOf(s), 0);

  if not Assigned(RootValue) or not (RootValue is TJSONObject) then
    raise Exception.Create('Parsing error on JSON answer: Root object not found.');

  o := TJSONObject(RootValue);

//  for i := 0 to o.Size - 1 do begin
//    p := o.Get(i);
//    WriteLn(i, ': ', p.JsonString.Value, ' = ', p.JsonValue.ClassName, ': ', p.JsonValue.Value);
//  end;

// OK:
//0: responseData = TJSONObject:
//1: responseDetails = TJSONNull:
//2: responseStatus = TJSONNumber: 200

// Error:
//0: responseData = TJSONNull:
//1: responseDetails = TJSONString: invalid translation language pair
//2: responseStatus = TJSONNumber: 400

  if o.Count <> 3 then
    raise Exception.CreateFmt(_('Parsing error on JSON answer: Root object size is %d not 3.'),
      [o.Count]);

  responseData := o.Pairs[0];
  if responseData.JsonString.Value <> 'responseData' then
    raise Exception.CreateFmt(_('Parsing error on JSON answer: First pair name is "%s" not "responseData".'),
      [responseData.JsonString.Value]);
  if responseData.JsonValue.Null then
    FData := ''
  else if responseData.JsonValue is TJSONObject then begin
    rdo := TJSONObject(responseData.JsonValue);
    if rdo.Count <> 1 then
      raise Exception.CreateFmt(_('Parsing error on JSON answer: responseData object size is %d not 1.'),
        [rdo.Count]);
    responseData := rdo.Pairs[0];
    if responseData.JsonString.Value <> 'translatedText' then
      raise Exception.CreateFmt(_('Parsing error on JSON answer: First responseData pair is "%s" not "translatedText".'),
        [responseData.JsonString.Value]);
    if not (responseData.JsonValue is TJSONString) then
      raise Exception.Create(_('Parsing error on JSON answer: translatedText data is not a string.'));
{$IFDEF HAS_UNIT_NETENCODING}
    FData := TNetEncoding.HTML.Decode(TJSONString(responseData.JsonValue).Value);
{$ELSE}
    FData := HTMLDecode(TJSONString(responseData.JsonValue).Value);
{$ENDIF}
  end else
    raise Exception.Create(_('Parsing error on JSON answer: responseData is neither NULL nor an object.'));

  responseDetails := o.Pairs[1];
  if responseDetails.JsonString.Value <> 'responseDetails' then
    raise Exception.CreateFmt(_('Parsing error on JSON answer: Second pair name is "%s" not "responseDetails".'),
      [responseDetails.JsonString.Value]);
  if responseDetails.JsonValue.Null then
    FDetails := ''
  else if responseDetails.JsonValue is TJSONString then
    FDetails := TJSONString(responseDetails.JsonValue).Value
  else
    raise Exception.Create(_('Parsing error on JSON answer: responseDetails is neither NULL nor a string.'));

  responseStatus := o.Pairs[2];
  if responseStatus.JsonString.Value <> 'responseStatus' then
    raise Exception.CreateFmt(_('Parsing error on JSON answer: Thrid pair name is "%s" not "responseStatus".'),
      [responseStatus.JsonString.Value]);
  if not (responseStatus.JsonValue is TJSONNumber) then
    raise Exception.Create(_('Parsing error on JSON answer: responseStatus is not a number.'));
  FStatus := TJSONNumber(responseStatus.JsonValue).AsInt;

end;
{$ENDIF HAS_JSON_SUPPORT}

end.

