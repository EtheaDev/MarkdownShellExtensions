{.GXFormatter.config=twm}
///<summary>
/// Implements a bunch of commonly used string functions. This started out as a copy of
/// my Virtual Pascal Unit TwmString adapted to Delphi.
/// @author twm </summary>
unit u_dzStringUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  Classes,
  StrUtils,
  SysUtils,
{$IFDEF SUPPORTS_UNICODE}
  AnsiStrings,
{$ENDIF SUPPORTS_UNICODE}
  u_dzTranslator,
  u_dzTypes;

type
///<summary> Ancestor of all exceptions raised in u_dzStringUtils </summary>
  EStringfunc = class(EdzException);
///<summary> Raised by NthWord if the word index is out of range (1..word count) </summary>
  EWordIndexOutOfRange = class(EStringfunc);
///<summary> Raised by CenterStr if the given string ist too long. </summary>
  EStringTooLong = class(EStringfunc);
///<summary> Raised by NthCharOf if passed a char index of 0. </summary>
  ECharIndexOutOfRange = class(EStringfunc);

type
  TCharSet = set of AnsiChar;

const
  ///<summary>
  /// Characters that are usually used as word delimiters.
  /// This can be passed when a function takes a TCharSet of delimiters.
  /// Warning: These are not complete and do not support UNICODE. </summary>
  STANDARD_DELIMITERS = [Chr(0)..Chr(255)] - ['a'..'z', 'A'..'Z',
    '0'..'9', 'ä', 'Ä', 'ö', 'Ö', 'ü', 'Ü', 'ß'];
  STANDARD_CONTROL_CHARS = [#0..' '];

///<summary>
/// converts to ASCII (real ASCII, only #0..#127)
/// tries to keep special characters readable
/// eg. UUml -> U
/// source:
/// https://stackoverflow.com/a/1892432/49925
function StrToLowAscii(const _s: WideString): AnsiString;

{$IFNDEF SUPPORTS_UNICODE}
function CharInSet(_c: Char; const _CharSet: TSysCharSet): Boolean;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$ENDIF SUPPORTS_UNICODE}

///<summary>
/// Cuts off the string at the first #0 and returns the new length </summary>
function StrTrimNul(var _s: AnsiString): Integer;
///<summary> use StrTrimNul </summary>
function StrTrimZero(var _s: AnsiString): Integer; deprecated; // use StrTrimNul

///<summary>
/// Converts an array of byte to a string, interpreting the bytes as AnsiString </summary>
function ByteArrayToString(const _buf: array of Byte): string;
///<summary>
/// Converts an AnsiString to an array of bytes </summary>
function StringToByteArray(const _s: string): TBytes; overload;
///<summary>
/// Converts an AnsiString to an array of bytes up to a given lenght
/// @NOTE: Does not check whether Length(Buf) >= ArraySize! </summary>
procedure StringToByteArray(const _s: string; var _buf: array of Byte; _ArraySize: Integer); overload;

///<summary>
/// @returns true, if the given string is one of the strings in the given array
/// Comparison is case sensitive </summary>
function IsStringIn(const _s: string; const _Arr: array of string; out _Idx: Integer): Boolean; overload;
function IsStringIn(const _s: string; const _Arr: array of string): Boolean; overload;

///<summary>
/// @returns true, if the given string is one of the strings in the given array
/// Comparison is case insensitive </summary>
function IsTextIn(const _s: string; const _Arr: array of string; out _Idx: Integer): Boolean; overload;
function IsTextIn(const _s: string; const _Arr: array of string): Boolean; overload;

///<summary>
/// Function is deprecated, use ExtractStr instead </summary>
function GetDelStr(var _Zeile: string; _Del: Char): string; deprecated;

///<summary>
/// Extracts the substring from the start of Source up to the Delimiter </summary>
function ExtractStr(var _Source: string; _Delimiter: Char): string; overload;
function ExtractStr(var _Source: string; _Delimiters: TCharSet): string; overload;
///<summary>
/// Extracts a substring from the start of Source up to the Delimiter
/// @returns true, if a substring (even an empty one) was found. </summary>
function ExtractStr(var _Source: string; _Delimiters: TCharSet; out _SubStr: string): Boolean; overload;
///<summary>
/// Extracts a substring from the start of Source up to the Delimiter
/// @returns true, if a substring (even an empty one) was found. </summary>
function ExtractStr(var _Source: string; _Delimiter: Char; out _SubStr: string): Boolean; overload;
///<summary>
/// Extracts a substring from the start of Source up to the Delimiter
/// @param LastWasDelimiter is a boolean that is to be used in repeated calls
///                         to the function to tell the next call that the
///                         last call returned an empty string because there
///                         was a double delimiter.
/// Use like this:
/// b := false;
/// while ExtractStr(Source, [' ', #9], s, b) do
///   HandleSubstring(s);
///</summary>
function ExtractStr(var _Source: string; _Delimiters: TCharSet; out _SubStr: string; var _LastWasDelimiter: Boolean): Boolean; overload;
///<summary>
/// Extracts a substring from the start of Source up to the Delimiter
/// @param LastWasDelimiter is a boolean that is to be used in repeated calls
///                         to the function to tell the next call that the
///                         last call returned an empty string because there
///                         was a double delimiter.
/// Use like this:
/// b := false;
/// while ExtractStr(Source, [' ', #9], s, b) do
///   HandleSubstring(s);
///</summary>
function ExtractStr(var _Source: string; _Delimiter: Char; out _SubStr: string; var _LastWasDelimiter: Boolean): Boolean; overload;

///<summary>
/// Converts a char to lower case. </summary>
function LoCase(_c: WideChar): WideChar; overload;
function LoCase(_c: AnsiChar): AnsiChar; overload;

// function UpStr(const _s: string): string; // use SysUtils.(Ansi)UpperCase instead
// function LoStr(const _s: string): string; // use SysUtils.(Ansi)LowerCase instead

///<summary>
/// Like SysUtils.Trim but only removes spaces, no special characters </summary>
function TrimSpaces(const _s: string): string;
///<summary>
/// Like SysUtils.TrimRight but only removes spaces, no special characters </summary>
function RTrimSpaces(const _s: string): string;
///<summary>
/// Like SysUtils.TrimLeft but only removes spaces, no special characters </summary>
function LTrimSpaces(const _s: string): string;
type
  TTrimStr = function(const _s: string): string;
const
///<summary>
/// Function is deprcated, use RTrimSpaces instead, or possibily SysUtils.TrimRight </summary>
  RTrimStr: TTrimStr = RTrimSpaces deprecated;
///<summary>
/// Function is deprcated, use LTrimSpaces instead, or possibly SysUtils.TrimLeft </summary>
  LTrimStr: TTrimStr = LTrimSpaces deprecated;
///<summary>
/// Function is deprcated, use TrimSpaces instead, or possibly SysUtils.Trim </summary>
  TrimStr: TTrimStr = TrimSpaces deprecated;

///<summary>
/// Creates a string with Cnt spaces. </summary>
function SpaceStr(_Cnt: Integer): string;

///<summary>
/// Function is deprecated, use StrUtils.DupeString instead </summary>
function StringOf(_c: Char; _Cnt: Integer): string; deprecated;

///<summary>
/// Prepend a backslash to the string if there isn't one already. </summary>
function PrependBackslash(const _s: string): string;

///<summary>
/// Replaces an existing extension in Name with Ext or adds Ext to Name if
/// it does not have an extension.
/// @param Name is the (file) name to which the extension is to be added.
/// @param Ext is the extension, Note that a dot (.) will be prefixed if there is none
/// NOTE: Consider using TFileSystem.ChangeFileExtFull or .ChangeFileExtLast instead. </summary>
function ForceExtension(const _Name, _Ext: string): string;
///<summary>
/// Returns only the filename (incl. extension) portion of Name. </summary>
function JustFilename(const _Name: string): string;
///<summary>
/// Function is deprecated (because it did a different thing but what the name said)
/// use RemoveSuffixIfMatching instead </summary>
function RemoveFileExtIfMatching(const _s, _Suffix: string): string; deprecated;
///<summary>
/// Removes a suffix (can be a file extension, but can also be any arbitrary string)
/// from a string if it matches the given one
/// @param s is the input string
/// @param Suffix is the suffix to remove, if the string has it (comparison is case insensitive)
/// @returns the string without the suffix, if it matched, the unchanged string otherwise </summary>
function RemoveSuffixIfMatching(const _s, _Suffix: string): string;

///<summary>
/// Appends spaces to the string S to bring it to the given length. If S is too long and
/// AllowTruncate is true (default) it is truncated and thus the result is guaranteed to
/// be Len characters long. If AllowTruncate is false, no truncation is done and the string
/// is only guaranteed to be at least Len characters long. </summary>
function RPadStr(const _s: string; _Len: Integer; _AllowTruncate: Boolean = True): string; overload;

///<summary>
/// Appends PadChar to the string S to bring it to the given length. If S is too long and
/// AllowTruncate is true (default) it is truncated and thus the result is guaranteed to
/// be Len characters long. If AllowTruncate is false, no truncation is done and the string
/// is only guaranteed to be at least Len characters long. </summary>
function RPadStr(const _s: string; _Len: Integer; _PadChar: Char; _AllowTruncate: Boolean = True): string; overload;

///<summary>
/// Prepends spaces to the string S to bring it to the given length. If S is too long and
/// AllowTruncate is true (default) it is truncated (at the start) and thus the result is
/// guaranteed to be Len characters long. If AllowTruncate is false, no truncation is done
/// and the string is only guaranteed to be at least Len characters long. </summary>
function LPadStr(const _s: string; _Len: Integer; _AllowTruncate: Boolean = True): string; overload;

///<summary>
/// Prepends PatChar to the string S to bring it to the given length. If S is too long and
/// AllowTruncate is true (default) it is truncated (at the start) and thus the result is
/// guaranteed to be Len characters long. If AllowTruncate is false, no truncation is done
/// and the string is only guaranteed to be at least Len characters long. </summary>
function LPadStr(const _s: string; _Len: Integer; _PadChar: Char; _AllowTruncate: Boolean = True): string; overload;

///<summary>
/// @Returns true, if SubStr is found in Str and sets Head to the text before and Tail to the text
/// after SubStr. Otherwise Head and Tail are undefined. </summary>
function FindString(const _SubStr, _Str: string; var _Head, _Tail: string): Boolean;

///<summary>
/// @Returns the rightmost position of Sub in S or 0 if Sub is not found in S. </summary>
function RPosStr(const _Sub: string; _s: string): Integer;

///<summary>
/// Converts a PChar to as Pascal string. Treats NIL as an empty string. </summary>
function Str2Pas(_p: PChar): string;
///<summary>
/// Allocates a new PChar and sets it to the contents of S, the length is set exactly to the
/// length of S. </summary>
function StrPNew(const _s: string): PChar;
///<summary>
/// Returns a pointer to a temporary string containing S. Warning: This uses a global constant for
/// ShortStrings respectively just typecasts AnsiStrings to PChar. Use with care!
/// NOTE: This functio is deprecated, just typecast AnsiString to PAnsiChar. </summary>
function Pas2Str(var _s: string): PChar; deprecated; // just typecast AnsiStrings to PAnsiChar

///<summary>
/// Reads a line from the file F and returns it as PChar. P is allocated to the correct length. </summary>
procedure StrReadLn(var _f: file; _p: PChar);
///<summary>
/// Reads a 0-terminated string from file F and returns it as PChar. P is allocated to the correct
/// length. </summary>
procedure StrReadZ(var _f: file; _p: PChar);

{$IF not Declared(PosEx)}
function PosEx(const _SubStr, _s: string; _Offset: Integer = 1): Integer;
{$DEFINE POSEX_IMPLEMENTATION_REQUIRED}
{$IFEND}

{$IF not Declared(SameStr)}
function SameStr(const _s1, _s2: string): Boolean;
{$DEFINE SAMESTR_IMPLEMENTATION_REQUIRED}
{$IFEND}

{$IF not Declared(StartsText)}
function StartsText(const _Start, _s: string): Boolean;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$DEFINE STARTSTEXT_IMPLEMENTATION_REQUIRED}
{$IFEND}

{$IF not Declared(StartsStr)}
function StartsStr(const _Start, _s: string): Boolean;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$DEFINE STARTSSTR_IMPLEMENTATION_REQUIRED}
{$IFEND}

{$IF not Declared(EndsText)}
function EndsText(const _End, _s: string): Boolean;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$DEFINE ENDSTEXT_IMPLEMENTATION_REQUIRED}
{$IFEND}

{$IF not Declared(EndsStr)}
function EndsStr(const _End, _s: string): Boolean;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$DEFINE ENDSSTR_IMPLEMENTATION_REQUIRED}
{$IFEND}

{$IF not Declared(ContainsStr)}
function ContainsStr(const _Text, _SubText: string): Boolean;
{$DEFINE CONTAINSSTR_IMPLEMENTATION_REQUIRED}
{$IFEND}

{$IF not Declared(ReplaceStr)}
function ReplaceStr(const _Text, _FromText, _ToText: string): string;
{$DEFINE REPLACESTR_IMPLEMENTATION_REQUIRED}
{$IFEND}

///</summary>
/// @Returns true if Searched starts with the string Match. </summary>
function MatchStr(const _Searched, _Match: string): Boolean;
///<summary>
/// @Returns true if Searched starts with the string Match ignoring case. </summary>
function UMatchStr(const _Searched, _Match: string): Boolean;

///<summary>
/// Creates a string of the form '...end of the string' with a maximum length. </summary>
function LDotDotStr(const _s: string; _MaxLen: Integer): string;
///<summary>
/// Creates a string of the form 'Start of string...' with a maximum length. </summary>
function RDotDotStr(const _s: string; _MaxLen: Integer): string;

///<summary>
/// Converts the Value to a string and adds '0' to the left to the given length. </summary>
function ZeroPadLeft(_Value: Integer; _Len: Integer): string;

///<summary>
/// Centers the given string, that is right and left pads it with spaces to MaxLenght characters. </summary>
function CenterStr(const _s: string; _MaxLen: Integer): string;

///<summary>
/// @returns the sub string starting from position Start </summary>
{$IFDEF SUPPORTS_UNICODE_STRING}
function TailStr(const _s: AnsiString; _Start: Integer): AnsiString; overload;
{$ENDIF}
function TailStr(const _s: string; _Start: Integer): string; overload;

///<summary>
/// Cuts off the rightmost CutLen characters of a string </summary>
function StrCutRight(const _s: string; _CutLen: Integer): string;

///<summary>
/// @returns part of S left of Delimiter (or all of it if it does not contain Delimiter) </summary>
function LeftStrOf(const _s: string; const _Delimiter: string): string;
///<summary>
/// @returns part of S right of the last Delimiter (or all of it if it does not contain Delimiter) </summary>
function RightStrOf(const _s: string; const _Delimiter: string): string;
///<summary>
/// @returns part of S right of the first Delimiter (or all of it if it does not contain Delimiter) </summary>
function TailStrOf(const _s: string; const _Delimiter: string): string;

///<summary>
/// @returns the next postion of SubStr in S starting from Start. Start is 1 based.
///          Returns 0 if SubStr was not found.
/// Note: Function is deprecated, use StrUtils.PosEx instead! </summary>
function PosStr(const _SubStr, _s: string; _Start: Integer): Integer; deprecated;

///<summary>
/// Replaces all occurences of characters in Search in S with the corresponding character in Replace.
/// If there is no matching character in Replace, the character will be omitted. </summary>
function ReplaceChars(const _s, _Search, _Replace: string): string; overload;

///<summary>
/// Replaces all occurences of characters in Search in S with the Replace string.
/// If Replace is an empty string, the characters will be omitted. </summary>
function ReplaceChars(const _s: string; _Search: TCharSet; const _Replace: string; _RemoveDuplicates: Boolean = True): string; overload;

///<summary>
/// Replaces all control characters (ord(c) < ord(' ')) with ReplaceChar.
/// If RemoveDuplicates is true, a sequence of control characters is replaced by a single ReplaceChar. </summary>
function ReplaceCtrlChars(const _s: string; _ReplaceChar: Char; _RemoveDuplicates: Boolean = True): string; overload;
{$IFDEF SUPPORTS_UNICODE}
function ReplaceCtrlChars(const _s: AnsiString; _ReplaceChar: AnsiChar; _RemoveDuplicates: Boolean = True): AnsiString; overload;
{$ENDIF SUPPORTS_UNICODE}

///<summary>
/// Replaces all control characters (ord(c) < ord(' ')) with Spaces.
/// If RemoveDuplicates is true, a sequence of control characters is replaced by a single space. </summary>
function CtrlcharsToSpace(const _s: string; _RemoveDuplicates: Boolean = True): string;

///<summary>
/// Checks whether a string contains only given chars </summary>
function ContainsOnlyCharsFrom(const _s: string; _ValidChars: TCharSet): Boolean;

///<summary>
/// Replaces all control characters (ord(c) <= ord(' '), " and ') with Prefix<hexcode> </summary>
function HexEncodeControlChars(_Prefix: Char; const _s: string; _ControlChars: TCharSet = STANDARD_CONTROL_CHARS): string;
function HexDecodeControlChars(const _Prefix: Char; const _s: string): string;

///<summary>
/// Replaces all control characters (ord(c) <= ord(' '), " and ') with %<hexcode> </summary>
function UrlEncodeControlChars(const _s: string; _ControlChars: TCharSet = STANDARD_CONTROL_CHARS): string;
function UrlDecodeControlChars(const _s: string): string;

///<summary>
/// @returns the WordNo'th word, (counting from 1), using the given Delimiters.
/// NOTE: Duplicate delimiters are ignored, so 'abc  def' will be split into two words (which you
///       would expect), but also 'abc'#9#9'def' is two words (which you might not expect) </summary>
function nthWord(const _s: string; _WordNo: Integer; const _Delimiter: string): string; overload;
///<summary>
/// @returns the WordNo'th word, (counting from 1), using the given Delimiters.
/// NOTE: Duplicate delimiters are ignored, so 'abc  def' will be split into two words (which you
///       would expect), but also 'abc'#9#9'def' is two words (which you might not expect) </summary>
function nthWord(const _s: string; _WordNo: Integer; _Delimiter: TCharSet): string; overload;

///<summary>
/// @returns the Nth character of S or ' ' if S has less than N charaters. </summary>
function nthCharOf(const _s: string; _n: Integer): Char;

{$IFDEF SUPPORTS_UNICODE}
///<summary>
/// Extract the first word of S using the given delimiters. The word is deleted from S.
/// See also ExtractStr.
/// NOTE: Duplicate delimiters are ignored, so 'abc  def' will be split into two words (which you
///       would expect), but also 'abc'#9#9'def' is two words (which you might not expect) </summary>
function ExtractFirstWord(var _s: AnsiString; const _Delimiter: AnsiString): AnsiString; overload;
///<summary>
/// Extract the first word of S using the given delimiters. The word is deleted from S.
/// See also ExtractStr.
/// NOTE: Duplicate delimiters are ignored, so 'abc  def' will be split into two words (which you
///       would expect), but also 'abc'#9#9'def' is two words (which you might not expect) </summary>
function ExtractFirstWord(var _s: AnsiString; _Delimiter: TCharSet): AnsiString; overload;
///<summary>
/// Extract the first word of S using the given delimiters. The word is deleted from S.
/// See also ExtractStr.
/// NOTE: Duplicate delimiters are ignored, so 'abc  def' will be split into two words (which you
///       would expect), but also 'abc'#9#9'def' is two words (which you might not expect)
/// @returns true, if a word could be extracted, false otherwise </summary>
function ExtractFirstWord(var _s: AnsiString; const _Delimiter: AnsiString; out _FirstWord: AnsiString): Boolean; overload;
///<summary>
/// Extract the first word of S using the given delimiters. The word is deleted from S.
/// See also ExtractStr.
/// NOTE: Duplicate delimiters are ignored, so 'abc  def' will be split into two words (which you
///       would expect), but also 'abc'#9#9'def' is two words (which you might not expect)
/// @returns true, if a word could be extracted, false otherwise </summary>
function ExtractFirstWord(var _s: AnsiString; _Delimiter: TCharSet; out _FirstWord: AnsiString): Boolean; overload;
{$ENDIF SUPPORTS_UNICODE}

///<summary>
/// Extract the first word of S using the given delimiters. The word is deleted from S.
/// See also ExtractStr.
/// NOTE: Duplicate delimiters are ignored, so 'abc  def' will be split into two words (which you
///       would expect), but also 'abc'#9#9'def' is two words (which you might not expect) </summary>
function ExtractFirstWord(var _s: string; const _Delimiter: string): string; overload;
///<summary>
/// Extract the first word of S using the given delimiters. The word is deleted from S.
/// See also ExtractStr.
/// NOTE: Duplicate delimiters are ignored, so 'abc  def' will be split into two words (which you
///       would expect), but also 'abc'#9#9'def' is two words (which you might not expect) </summary>
function ExtractFirstWord(var _s: string; _Delimiter: TCharSet): string; overload;
///<summary>
/// Extract the first word of S using the given delimiters. The word is deleted from S.
/// See also ExtractStr.
/// NOTE: Duplicate delimiters are ignored, so 'abc  def' will be split into two words (which you
///       would expect), but also 'abc'#9#9'def' is two words (which you might not expect)
/// @returns true, if a word could be extracted, false otherwise </summary>
function ExtractFirstWord(var _s: string; const _Delimiter: string; out _FirstWord: string): Boolean; overload;
///<summary>
/// Extract the first word of S using the given delimiters. The word is deleted from S.
/// See also ExtractStr.
/// NOTE: Duplicate delimiters are ignored, so 'abc  def' will be split into two words (which you
///       would expect), but also 'abc'#9#9'def' is two words (which you might not expect)
/// @returns true, if a word could be extracted, false otherwise </summary>
function ExtractFirstWord(var _s: string; _Delimiter: TCharSet; out _FirstWord: string): Boolean; overload;

///<summary>
/// Extracts the first N characters of a string </summary>
function ExtractFirstN(var _s: string; _n: Integer): string;

///<summary>
/// Split string s into the list of substrings delimited by any of the given delimiters
/// NOTE: Duplicate delimiters are ignored, so 'abc  def' will be split into two strings (which you
///       would expect), but also 'abc'#9#9'def' is two strings (which you might not expect)
/// @param sl is the stringlist in which to return the result. If it is not empty, the
///           new strings will be appended to the existing content.
/// @param s is the string to split
/// @param Delimiters is a string/array of char containing all delimiter characters
/// @returns the sl parameter </summary>
function SplitString(_sl: TStrings; _s: string; const _Delimiters: string): TStrings; overload;
function SplitString(_sl: TStrings; _s: string; const _Delimiters: array of Char): TStrings; overload;

///<summary>
/// Split string s into the array of substrings delimited by any of the given delimiters
/// NOTE: Duplicate delimiters are ignored, so 'abc  def' will be split into two strings (which you
///       would expect), but also 'abc'#9#9'def' is two strings (which you might not expect)
/// @param s is the string to split
/// @param Delimiters is a string/array of char containing all delimiter characters
/// @returns a TStringArray with the parts </summary>
function SplitString(_s: string; const _Delimiters: string): TStringArray; overload;
function SplitString(_s: string; const _Delimiters: array of Char): TStringArray; overload;

{$IFDEF SUPPORTS_UNICODE}
function Copy(const _s: AnsiString; _Pos, _Len: Integer): AnsiString; overload;
function Copy(const _s: AnsiString; _Pos: Integer): AnsiString; overload;
function Copy(const _s: string; _Pos, _Len: Integer): string; overload;
function Copy(const _s: string; _Pos: Integer): string; overload;
{$ENDIF SUPPORTS_UNICODE}
{$IFNDEF DELPHI2005_UP}
// Delphi 6/7 does not have Copy without the length parameter
function Copy(const _s: string; _Pos: Integer): string; overload;
function Copy(const _s: string; _Pos, _Len: Integer): string; overload;
{$ENDIF ~DELPHI2005_UP}

///<summary>
/// Converts Tab characters into SpcCount spaces </summary>
function Tab2Spaces(const _s: string; _SpcCount: Integer): string;

function StartsWith(const _Start, _s: string): Boolean;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

function EndsWith(const _End, _s: string): Boolean;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

function UStartsWith(const _Start, _s: string): Boolean;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function UEndsWith(const _End, _s: string): Boolean;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

function UnquoteString(const _s: string; _Quote: Char = '"'): string;

///<summary>
/// @returns the string, if it isn't NIL or 'NULL' if it is. </summary>
function StringOrNull(_p: PChar): string;

{$IF Declared(TFormatSettings)}
///<summary>
/// @returns the default locale settings as read from the system's regional settings </summary>
function GetUserDefaultLocaleSettings: TFormatSettings;
function GetSystemDefaultLocaleSettings: TFormatSettings;
{$IFEND}

///<summary>
/// Read the content of the file into a string and return it </summary>
function LoadStringFromFile(const _Filename: string): string;
///<summary>
/// Write the content of the string to a file </summary>
procedure SaveStringToFile(const _Filename: string; const _Content: string);

function MakeUniqueString(const _s: string): string; overload;
function MakeUniqueString(const _s: WideString): WideString; overload;

implementation

uses
  SysConst,
  u_dzConvertUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

function ForceExtension(const _Name, _Ext: string): string;
var
  p: Integer;
begin
  p := RPosStr('.', _Name);
  if p = 0 then
    Result := _Name + '.' + _Ext
  else
    Result := LeftStr(_Name, p) + _Ext;
end;

function JustFilename(const _Name: string): string;
var
  p: Integer;
begin
  p := RPosStr('\', _Name);
  if p = 0 then
    Result := _Name
  else
    Result := TailStr(_Name, p + 1);
end;

function RemoveFileExtIfMatching(const _s, _Suffix: string): string;
begin
  Result := RemoveSuffixIfMatching(_s, _Suffix);
end;

// Inlined functions must be implemented before it is used.
function UEndsWith(const _End, _s: string): Boolean;
begin
  Result := AnsiEndsText(_End, _s);
end;

function RemoveSuffixIfMatching(const _s, _Suffix: string): string;
begin
  if UEndsWith(_Suffix, _s) then
    Result := LeftStr(_s, Length(_s) - Length(_Suffix))
  else
    Result := _s;
end;

function nthWordStartAndEnd(const _s: string; _WordNo: Integer;
  const _Delimiter: TCharSet; out _Start, _Ende: Integer): Boolean; overload;
var
  i: Integer;
begin
  if _WordNo = 0 then
    raise EWordIndexOutOfRange.Create(_('nthWord: 0th word not available'));
  _Start := 0;
  _Ende := 0;
  i := 1;
  while i <= Length(_s) do begin
    while (i <= Length(_s)) and CharInSet(nthCharOf(_s, i), _Delimiter) do
      Inc(i);
    Dec(_WordNo);
    if _WordNo = 0 then
      _Start := i;
    while (i <= Length(_s)) and not CharInSet(nthCharOf(_s, i), _Delimiter) do
      Inc(i);
    if _WordNo = 0 then begin
      _Ende := i;
      Break;
    end;
  end;
  Result := (_Start <> 0) and (_Ende <> 0);
end;

function nthWordStartAndEnd(const _s: string; _WordNo: Integer;
  const _Delimiter: string; out _Start, _Ende: Integer): Boolean; overload;
var
  i: Integer;
  DelimiterSet: TCharSet;
begin
  DelimiterSet := [];
  for i := 1 to Length(_Delimiter) do
    Include(DelimiterSet, AnsiChar(_Delimiter[i]));
  Result := nthWordStartAndEnd(_s, _WordNo, DelimiterSet, _Start, _Ende);
end;

{$IFDEF SUPPORTS_UNICODE}

function nthWordStartAndEnd(const _s: AnsiString; _WordNo: Integer;
  const _Delimiter: AnsiString; out _Start, _Ende: Integer): Boolean; overload;
begin
  Result := nthWordStartAndEnd(string(_s), _WordNo, string(_Delimiter), _Start, _Ende);
end;

function nthWordStartAndEnd(const _s: AnsiString; _WordNo: Integer;
  const _Delimiter: TCharSet; out _Start, _Ende: Integer): Boolean; overload;
begin
  Result := nthWordStartAndEnd(string(_s), _WordNo, _Delimiter, _Start, _Ende);
end;
{$ENDIF SUPPORTS_UNICODE}

function nthWord(const _s: string; _WordNo: Integer; const _Delimiter: string): string;
var
  Start, Ende: Integer;
begin
  if nthWordStartAndEnd(_s, _WordNo, _Delimiter, Start, Ende) then
    Result := Copy(_s, Start, Ende - Start)
  else
    Result := '';
end;

function nthWord(const _s: string; _WordNo: Integer; _Delimiter: TCharSet): string;
var
  Start, Ende: Integer;
begin
  if nthWordStartAndEnd(_s, _WordNo, _Delimiter, Start, Ende) then
    Result := Copy(_s, Start, Ende - Start)
  else
    Result := '';
end;

function ExtractFirstWord(var _s: string; _Delimiter: TCharSet): string;
begin
  if not ExtractFirstWord(_s, _Delimiter, Result) then begin // s contained only Delimiters
    Result := '';
    _s := '';
  end;
end;

function ExtractFirstWord(var _s: string; const _Delimiter: string): string;
begin
  if not ExtractFirstWord(_s, _Delimiter, Result) then begin // s contained only Delimiters
    Result := '';
    _s := '';
  end;
end;

function ExtractFirstWord(var _s: string; const _Delimiter: string; out _FirstWord: string): Boolean;
var
  Start, Ende: Integer;
begin
  Result := nthWordStartAndEnd(_s, 1, _Delimiter, Start, Ende);
  if Result then begin
    _FirstWord := Copy(_s, Start, Ende - Start);
    _s := TailStr(_s, Ende + 1);
  end;
end;

function ExtractFirstWord(var _s: string; _Delimiter: TCharSet; out _FirstWord: string): Boolean;
var
  Start, Ende: Integer;
begin
  Result := nthWordStartAndEnd(_s, 1, _Delimiter, Start, Ende);
  if Result then begin
    _FirstWord := Copy(_s, Start, Ende - Start);
    _s := TailStr(_s, Ende + 1);
  end;
end;

{$IFDEF SUPPORTS_UNICODE}
function ExtractFirstWord(var _s: AnsiString; _Delimiter: TCharSet): AnsiString;
begin
  if not ExtractFirstWord(_s, _Delimiter, Result) then begin // s contained only Delimiters
    Result := '';
    _s := '';
  end;
end;

function ExtractFirstWord(var _s: AnsiString; const _Delimiter: AnsiString): AnsiString;
begin
  if not ExtractFirstWord(_s, _Delimiter, Result) then begin // s contained only Delimiters
    Result := '';
    _s := '';
  end;
end;

function ExtractFirstWord(var _s: AnsiString; const _Delimiter: AnsiString; out _FirstWord: AnsiString): Boolean;
var
  Start, Ende: Integer;
begin
  Result := nthWordStartAndEnd(_s, 1, _Delimiter, Start, Ende);
  if Result then begin
    _FirstWord := Copy(_s, Start, Ende - Start);
    _s := TailStr(_s, Ende + 1);
  end;
end;

function ExtractFirstWord(var _s: AnsiString; _Delimiter: TCharSet; out _FirstWord: AnsiString): Boolean;
var
  Start, Ende: Integer;
begin
  Result := nthWordStartAndEnd(_s, 1, _Delimiter, Start, Ende);
  if Result then begin
    _FirstWord := Copy(_s, Start, Ende - Start);
    _s := TailStr(_s, Ende + 1);
  end;
end;
{$ENDIF SUPPORTS_UNICODE}

function ExtractFirstN(var _s: string; _n: Integer): string;
begin
  Result := Copy(_s, 1, _n);
  _s := TailStr(_s, _n + 1);
end;

function SplitString(_sl: TStrings; _s: string; const _Delimiters: string): TStrings; //FI:O801
// Note: _s cannot be const, because it is passed to ExtractFirstWord which needs a var parameter
var
  s: string;
begin
  Result := _sl;
  while _s <> '' do begin
    s := ExtractFirstWord(_s, _Delimiters);
    Result.Add(s);
  end;
end;

function SplitString(_sl: TStrings; _s: string; const _Delimiters: array of Char): TStrings; //FI:O801
// Note: _s cannot be const, because it is passed to ExtractFirstWord which needs a var parameter
// Note: The code is the same as in the overloaded version, but Delimiters is an array of char
//       here.
var
  s: string;
begin
  Result := _sl;
  while _s <> '' do begin
    s := ExtractFirstWord(_s, _Delimiters);
    Result.Add(s);
  end;
end;

function SplitString(_s: string; const _Delimiters: string): TStringArray; //FI:O801
// Note: _s cannot be const, because it is passed to ExtractFirstWord which needs a var parameter
var
  s: string;
  Idx: Integer;
begin
  // Maximum number of possible substrings is Len(s) div 2 + 1:
  // e.g. 'a b c d' (len=7) -> 'a', 'b', 'c', 'd'
  SetLength(Result, Length(_s) div 2 + 1);
  Idx := 0;
  while _s <> '' do begin
    s := ExtractFirstWord(_s, _Delimiters);
    Result[Idx] := s;
    Inc(Idx);
  end;
  SetLength(Result, Idx);
end;

function SplitString(_s: string; const _Delimiters: array of Char): TStringArray; //FI:O801
// Note: _s cannot be const, because it is passed to ExtractFirstWord which needs a var parameter
// Note: The code is the same as in the overloaded version, but Delimiters is an array of char
//       here.
var
  s: string;
  Idx: Integer;
begin
  // Maximum number of possible substrings is Len(s) div 2 + 1:
  // e.g. 'a b c d' (len=7) -> 'a', 'b', 'c', 'd'
  SetLength(Result, Length(_s) div 2 + 1);
  Idx := 0;
  while _s <> '' do begin
    s := ExtractFirstWord(_s, _Delimiters);
    Result[Idx] := s;
    Inc(Idx);
  end;
  SetLength(Result, Idx);
end;

function ReplaceChars(const _s, _Search, _Replace: string): string;
var
  i, j: LongInt;
  p: Integer;
begin
  SetLength(Result, Length(_s));
  j := 1;
  for i := 1 to Length(_s) do begin
    p := Pos(_s[i], _Search);
    if p <> 0 then begin
      if Length(_Replace) >= p then begin
        Result[j] := _Replace[p];
        Inc(j);
      end
    end else begin
      Result[j] := _s[i];
      Inc(j);
    end;
  end;
  SetLength(Result, j - 1);
end;

function ReplaceChars(const _s: string; _Search: TCharSet; const _Replace: string; _RemoveDuplicates: Boolean = True): string;
var
  i: LongInt;
  Dup: Boolean;
begin
  Result := '';
  Dup := False;
  for i := 1 to Length(_s) do begin
    if CharInSet(_s[i], _Search) then begin
      if not Dup or not _RemoveDuplicates then begin
        Result := Result + _Replace;
        Dup := True;
      end;
    end else begin
      Result := Result + _s[i];
      Dup := False;
    end;
  end;
end;

function ReplaceCtrlChars(const _s: string; _ReplaceChar: Char; _RemoveDuplicates: Boolean = True): string;
var
  i: Integer;
  Dup: Boolean;
begin
  Result := _s;
  Dup := False;
  for i := Length(Result) downto 1 do
    if Ord(Result[i]) < Ord(' ') then begin
      if not Dup or not _RemoveDuplicates then begin
        Result[i] := _ReplaceChar;
        Dup := True;
      end else
        System.Delete(Result, i, 1);
    end else
      Dup := False;
end;

{$IFDEF SUPPORTS_UNICODE}
function ReplaceCtrlChars(const _s: AnsiString; _ReplaceChar: AnsiChar; _RemoveDuplicates: Boolean = True): AnsiString;
begin
  Result := AnsiString(ReplaceCtrlChars(string(_s), Char(_ReplaceChar), _RemoveDuplicates));
end;
{$ENDIF SUPPORTS_UNICODE}

function ContainsOnlyCharsFrom(const _s: string; _ValidChars: TCharSet): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(_s) do
    if not CharInSet(_s[i], _ValidChars) then
      Exit;
  Result := True;
end;

function CtrlcharsToSpace(const _s: string; _RemoveDuplicates: Boolean = True): string;
begin
  Result := ReplaceCtrlChars(_s, ' ', _RemoveDuplicates);
end;

function HexEncodeControlChars(_Prefix: Char; const _s: string; _ControlChars: TCharSet): string;
var
  i: Integer;
begin
  Result := '';
  Include(_ControlChars, AnsiChar(_Prefix));
  for i := 1 to Length(_s) do begin
    if CharInSet(_s[i], _ControlChars) then
      Result := Result + Format('%s%.2x', [_Prefix, Ord(_s[i])]) // do not translate
    else
      Result := Result + _s[i];
  end;
end;

function HexDecodeControlChars(const _Prefix: Char; const _s: string): string;
var
  i: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(_s) do begin
    if (_s[i] = _Prefix) and (i + 2 <= Length(_s)) and isHexDigit(_s[i + 1]) and isHexDigit(_s[i + 2]) then begin
      Result := Result + Chr(Hex2Long(_s[i + 1] + _s[i + 2]));
      Inc(i, 2);
    end else
      Result := Result + _s[i];
    Inc(i);
  end;
end;

function UrlEncodeControlChars(const _s: string; _ControlChars: TCharSet = STANDARD_CONTROL_CHARS): string;
begin
  Result := HexEncodeControlChars('%', _s, _ControlChars);
end;

function UrlDecodeControlChars(const _s: string): string;
begin
  Result := HexDecodeControlChars('%', _s);
end;

function LDotDotStr(const _s: string; _MaxLen: Integer): string;
begin
  if Length(_s) > _MaxLen then
    Result := '..' + Copy(_s, Length(_s) - _MaxLen + 3, _MaxLen - 2)
  else
    Result := _s;
end;

function RDotDotStr(const _s: string; _MaxLen: Integer): string;
begin
  if Length(_s) > _MaxLen then
    Result := Copy(_s, 3, _MaxLen - 2) + '..'
  else
    Result := _s;
end;

{$IFDEF SAMESTR_IMPLEMENTATION_REQUIRED}

function SameStr(const _s1, _s2: string): Boolean;
begin
  Result := (_s1 = _s2);
end;
{$ENDIF}

{$IFDEF STARTSTEXT_IMPLEMENTATION_REQUIRED}
function StartsText(const _Start, _s: string): Boolean;
begin
  Result := AnsiStartsText(_Start, _s);
end;
{$ENDIF}

{$IFDEF STARTSSTR_IMPLEMENTATION_REQUIRED}
function StartsStr(const _Start, _s: string): Boolean;
begin
  Result := AnsiStartsStr(_Start, _s);
end;
{$ENDIF}

{$IFDEF ENDSTEXT_IMPLEMENTATION_REQUIRED}
function EndsText(const _End, _s: string): Boolean;
begin
  Result := AnsiEndsText(_End, _s);
end;
{$ENDIF}

{$IFDEF ENDSSTR_IMPLEMENTATION_REQUIRED}
function EndsStr(const _End, _s: string): Boolean;
begin
  Result := AnsiEndsStr(_End, _s);
end;
{$ENDIF}

{$IFDEF CONTAINSSTR_IMPLEMENTATION_REQUIRED}
function ContainsStr(const _Text, _SubText: string): Boolean;
begin
  Result := (Pos(_SubText, _Text) > 0);
end;
{$ENDIF}

{$IFDEF REPLACESTR_IMPLEMENTATION_REQUIRED}

function ReplaceStr(const _Text, _FromText, _ToText: string): string;
begin
  Result := StringReplace(_Text, _FromText, _ToText, [rfReplaceAll]);
end;
{$ENDIF}

function MatchStr(const _Searched, _Match: string): Boolean;
begin
  Result := SameStr(LeftStr(_Searched, Length(_Match)), _Match);
end;

function UMatchStr(const _Searched, _Match: string): Boolean;
begin
  Result := SameText(LeftStr(_Searched, Length(_Match)), _Match);
end;

// this function is compatible with StrNew/StrDispose in *SysUtils*

function StrPNew(const _s: string): PChar;
var
  Size: Cardinal;
begin
  Size := Length(_s);
  Result := StrAlloc(Size + 1);
  StrMove(Result, @_s[1], Size);
  (Result + Size)^ := #0;
end;

function Pas2Str(var _s: string): PChar;
begin
  Result := PChar(_s);
end;

function Str2Pas(_p: PChar): string;
begin
  if _p = nil then
    Result := ''
  else
    Result := StrPas(_p);
end;

function RPadStr(const _s: string; _Len: Integer; _PadChar: Char; _AllowTruncate: Boolean = True): string;
begin
  if Length(_s) >= _Len then begin
    if _AllowTruncate then
      Result := LeftStr(_s, _Len)
    else
      Result := _s;
  end else
    Result := _s + StringOfChar(_PadChar, _Len - Length(_s));
end;

function RPadStr(const _s: string; _Len: Integer; _AllowTruncate: Boolean = True): string;
begin
  Result := RPadStr(_s, _Len, ' ', _AllowTruncate);
end;

function LPadStr(const _s: string; _Len: Integer; _PadChar: Char; _AllowTruncate: Boolean = True): string;
begin
  if Length(_s) >= _Len then begin
    if _AllowTruncate then
      Result := RightStr(_s, _Len)
    else
      Result := _s;
  end else
    Result := StringOfChar(_PadChar, _Len - Length(_s)) + _s;
end;

function LPadStr(const _s: string; _Len: Integer; _AllowTruncate: Boolean = True): string;
begin
  Result := LPadStr(_s, _Len, ' ', _AllowTruncate);
end;

function RTrimSpaces(const _s: string): string;
begin
  Result := _s;
  while (Length(Result) > 0) and (nthCharOf(Result, Length(Result)) = ' ') do
    System.Delete(Result, Length(Result), 1);
end;

function LTrimSpaces(const _s: string): string;
begin
  Result := _s;
  while LeftStr(Result, 1) = ' ' do
    System.Delete(Result, 1, 1);
end;

// twm: There is probably a more efficient way to implement this.

function RPosStr(const _Sub: string; _s: string): Integer;
var
  p: Integer;
begin
  Result := 0;
  p := Pos(_Sub, _s);
  while p > 0 do begin
    Inc(Result, p);
    _s := TailStr(_s, p + 1);
    p := Pos(_Sub, _s);
  end;
end;

function PrependBackslash(const _s: string): string;
begin
  if LeftStr(_s, 1) = '\' then
    Result := _s
  else
    Result := '\' + _s;
end;

function StringOf(_c: Char; _Cnt: Integer): string;
begin
  if _Cnt <= 0 then
    Result := ''
  else
    Result := StrUtils.DupeString(_c, _Cnt);
end;

function SpaceStr(_Cnt: Integer): string;
begin
  if _Cnt <= 0 then
    Result := ''
  else
    Result := DupeString(' ', _Cnt);
end;

function TrimSpaces(const _s: string): string;
var
  i, L: Integer;
begin
  L := Length(_s);
  i := 1;
  while (i <= L) and (_s[i] = ' ') do
    Inc(i);
  if i > L then
    Result := ''
  else begin
    while _s[L] = ' ' do
      Dec(L);
    Result := Copy(_s, i, L - i + 1);
  end;
end;

//procedure Error(_Desc: string);
//begin
//  WriteLn(_Desc);
//  Halt(1);
//end;

function LoCase(_c: WideChar): WideChar;
begin
  Result := _c;
  case _c of
    'A'..'Z':
      Result := WideChar(Word(_c) or $0020);
  end;
end;

function LoCase(_c: AnsiChar): AnsiChar;
begin
  Result := _c;
  case _c of
    'A'..'Z':
      Result := AnsiChar(Byte(_c) or $20);
  end;
end;

{function UpStr(const _s: string): string;
  var
    i: integer;
  begin
  SetLength(Result, Length(_s));
  for i:=1 to Length(_s) do begin
    Result[i]:=UpCase(_s[i]);
  end;
end;}

{function LoStr(const _s : string) : string;
  var
    i: integer;
  begin
  SetLength(result, Length(_s));
  for i:=1 to Length(_s) do
    Result[i]:=LoCase(_s[i]);
end;}

procedure StrReadZ(var _f: file; _p: PChar);
begin
  BlockRead(_f, _p^, SizeOf(_p^));
  while _p^ <> #0 do begin
    Inc(_p);
    BlockRead(_f, _p^, SizeOf(_p^));
  end;
end;

procedure StrReadLn(var _f: file; _p: PChar);
begin
  BlockRead(_f, _p^, SizeOf(_p^));
  while _p^ <> #13 do begin
    Inc(_p);
    BlockRead(_f, _p^, SizeOf(_p^));
  end;
  _p^ := #0
end;

function ExtractStr(var _Source: string; _Delimiter: Char): string;
var
  p: Integer;
begin
  p := Pos(_Delimiter, _Source);
  if p = 0 then begin
    Result := _Source;
    _Source := '';
  end else begin
    Result := LeftStr(_Source, p - 1);
    _Source := TailStr(_Source, p + 1);
  end;
end;

function ExtractStr(var _Source: string; _Delimiters: TCharSet): string;
begin
  if not ExtractStr(_Source, _Delimiters, Result) then
    Result := '';
end;

function ExtractStr(var _Source: string; _Delimiters: TCharSet; out _SubStr: string; var _LastWasDelimiter: Boolean): Boolean;
var
  p: Integer;
begin
  if _LastWasDelimiter then begin
    _LastWasDelimiter := False;
    Result := True;
    _Source := '';
    _SubStr := '';
    Exit;
  end;

  p := 1;
  while p <= Length(_Source) do begin
    if CharInSet(_Source[p], _Delimiters) then begin
      _SubStr := LeftStr(_Source, p - 1);
      _Source := TailStr(_Source, p + 1);
      if _Source = '' then
        _LastWasDelimiter := True;
      Result := True;
      Exit;
    end;
    Inc(p);
  end;
  Result := _Source <> '';
  if Result then begin
    _SubStr := LeftStr(_Source, p - 1);
    _Source := '';
  end;
end;

function ExtractStr(var _Source: string; _Delimiters: TCharSet; out _SubStr: string): Boolean;
var
  b: Boolean;
begin
  b := False;
  Result := ExtractStr(_Source, _Delimiters, _SubStr, b);
end;

function ExtractStr(var _Source: string; _Delimiter: Char; out _SubStr: string; var _LastWasDelimiter: Boolean): Boolean;
begin
  Result := ExtractStr(_Source, [_Delimiter], _SubStr, _LastWasDelimiter);
end;

function ExtractStr(var _Source: string; _Delimiter: Char; out _SubStr: string): Boolean;
var
  b: Boolean;
begin
  b := False;
  Result := ExtractStr(_Source, [_Delimiter], _SubStr, b);
end;

function GetDelStr(var _Zeile: string; _Del: Char): string;
begin
  Result := ExtractStr(_Zeile, _Del);
end;

function CenterStr(const _s: string; _MaxLen: Integer): string;
var
  L: Integer;
begin
  if Length(_s) > _MaxLen then
    raise EStringTooLong.Create(_('Cannot center string that is larger than the maximum length.'));
  L := (_MaxLen - Length(_s)) div 2;
  Result := SpaceStr(L) + _s + SpaceStr(L);
  if Odd(Length(_s)) then
    Result := Result + ' ';
end;

function TailStr(const _s: string; _Start: Integer): string;
begin
  if _Start > Length(_s) then
    Result := ''
  else
    Result := Copy(_s, _Start, Length(_s) - _Start + 1);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}

function TailStr(const _s: AnsiString; _Start: Integer): AnsiString; overload;
begin
  if _Start > Length(_s) then
    Result := ''
  else
    Result := Copy(_s, _Start, Length(_s) - _Start + 1);
end;
{$ENDIF}

function StrCutRight(const _s: string; _CutLen: Integer): string;
begin
  Result := LeftStr(_s, Length(_s) - _CutLen);
end;

function LeftStrOf(const _s: string; const _Delimiter: string): string;
var
  p: Integer;
begin
  p := Pos(_Delimiter, _s);
  if p = 0 then
    Result := _s
  else
    Result := LeftStr(_s, p - 1);
end;

function RightStrOf(const _s: string; const _Delimiter: string): string;
var
  p: Integer;
begin
  p := RPosStr(_Delimiter, _s);
  if p = 0 then
    Result := _s
  else
    Result := TailStr(_s, p + Length(_Delimiter));
end;

function TailStrOf(const _s: string; const _Delimiter: string): string;
var
  p: Integer;
begin
  p := Pos(_Delimiter, _s);
  if p = 0 then
    Result := _s
  else
    Result := TailStr(_s, p + Length(_Delimiter));
end;

{$IFDEF POSEX_IMPLEMENTATION_REQUIRED}

(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function PosEx is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): Aleksandr Sharahov
 *
 * ***** END LICENSE BLOCK ***** *)

function PosEx(const _SubStr, _s: string; _Offset: Integer = 1): Integer;
asm
       test  eax, eax
       jz    @Nil
       test  edx, edx
       jz    @Nil
       dec   ecx
       jl    @Nil

       push  esi
       push  ebx

       mov   esi, [edx-4]  //Length(Str)
       mov   ebx, [eax-4]  //Length(Substr)
       sub   esi, ecx      //effective length of Str
       add   edx, ecx      //addr of the first char at starting position
       cmp   esi, ebx
       jl    @Past         //jump if EffectiveLength(Str)<Length(Substr)
       test  ebx, ebx
       jle   @Past         //jump if Length(Substr)<=0

       add   esp, -12
       add   ebx, -1       //Length(Substr)-1
       add   esi, edx      //addr of the terminator
       add   edx, ebx      //addr of the last char at starting position
       mov   [esp+8], esi  //save addr of the terminator
       add   eax, ebx      //addr of the last char of Substr
       sub   ecx, edx      //-@Str[Length(Substr)]
       neg   ebx           //-(Length(Substr)-1)
       mov   [esp+4], ecx  //save -@Str[Length(Substr)]
       mov   [esp], ebx    //save -(Length(Substr)-1)
       movzx ecx, byte ptr [eax] //the last char of Substr

@Loop:
       cmp   cl, [edx]
       jz    @Test0
@AfterTest0:
       cmp   cl, [edx+1]
       jz    @TestT
@AfterTestT:
       add   edx, 4
       cmp   edx, [esp+8]
       jb   @Continue
@EndLoop:
       add   edx, -2
       cmp   edx, [esp+8]
       jb    @Loop
@Exit:
       add   esp, 12
@Past:
       pop   ebx
       pop   esi
@Nil:
       xor   eax, eax
       ret
@Continue:
       cmp   cl, [edx-2]
       jz    @Test2
       cmp   cl, [edx-1]
       jnz   @Loop
@Test1:
       add   edx,  1
@Test2:
       add   edx, -2
@Test0:
       add   edx, -1
@TestT:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found
@String:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx+1]
       jnz   @AfterTestT
       cmp   esi, -2
       jge   @Found
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+3]
       jnz   @AfterTestT
       add   esi, 4
       jl    @String
@Found:
       mov   eax, [esp+4]
       add   edx, 2

       cmp   edx, [esp+8]
       ja    @Exit

       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
end;
{$ENDIF}

function PosStr(const _SubStr, _s: string; _Start: Integer): Integer;
begin
  Result := PosEx(_SubStr, _s, _Start);
end;

function FindString(const _SubStr, _Str: string; var _Head, _Tail: string): Boolean;
var
  p: Integer;
begin
  p := Pos(_SubStr, _Str);
  Result := (p <> 0);
  if Result then begin
    _Head := LeftStr(_Str, p - 1);
    _Tail := TailStr(_Str, p + Length(_SubStr));
  end;
end;

function nthCharOf(const _s: string; _n: Integer): Char;
begin
  if _n = 0 then
    raise ECharIndexOutOfRange.Create(_('Strings do not have a 0th character.'));
  if _n <= Length(_s) then
    Result := _s[_n]
  else
    Result := ' ';
end;

function Tab2Spaces(const _s: string; _SpcCount: Integer): string;
var
  i: Integer;
  Spaces: string;
begin
  // twm: This is not particularly efficient, just don't use it on large strings. ;-)
  Result := '';
  Spaces := SpaceStr(_SpcCount);
  for i := 1 to Length(_s) do begin
    if _s[i] = #9 then
      Result := Result + Spaces
    else
      Result := Result + _s[i];
  end;
end;

function StartsWith(const _Start, _s: string): Boolean;
begin
  Result := AnsiStartsStr(_Start, _s)
end;

function UStartsWith(const _Start, _s: string): Boolean;
begin
  Result := AnsiStartsText(_Start, LeftStr(_s, Length(_Start)));
end;

function EndsWith(const _End, _s: string): Boolean;
begin
  Result := AnsiEndsStr(_End, RightStr(_s, Length(_End)));
end;

function UnquoteString(const _s: string; _Quote: Char): string;
var
  Len: Integer;
begin
  Len := Length(_s);
  if (Len > 1) and (_s[1] = _Quote) and (_s[Len] = _Quote) then
    Result := Copy(_s, 2, Len - 2)
  else
    Result := _s;
end;

function StringOrNull(_p: PChar): string;
begin
  if Assigned(_p) then
    Result := string('"' + _p + '"')
  else
    Result := 'NULL'; // do not translate
end;

{$IF Declared(TFormatSettings)}

function GetSystemDefaultLocaleSettings: TFormatSettings;
begin
{$IFDEF RTL220_UP}
  Result := TFormatSettings.Create(GetSystemDefaultLCID);
{$ELSE}
  GetLocaleFormatSettings(GetSystemDefaultLCID, Result);
{$ENDIF}
end;

function GetUserDefaultLocaleSettings: TFormatSettings;
begin
{$IFDEF RTL220_UP}
  Result := TFormatSettings.Create(GetUserDefaultLCID);
{$ELSE}
  GetLocaleFormatSettings(GetUserDefaultLCID, Result);
{$ENDIF}
end;
{$IFEND}

function ZeroPadLeft(_Value: Integer; _Len: Integer): string;
var
  s: AnsiString;
begin
  Str(_Value, s);
  Result := string(s);
  while Length(Result) < _Len do
    Result := '0' + Result;
end;

function LoadStringFromFile(const _Filename: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(_Filename);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure SaveStringToFile(const _Filename: string; const _Content: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := _Content;
    sl.SaveToFile(_Filename);
  finally
    sl.Free;
  end;
end;

function StringToByteArray(const _s: string): TBytes;
var
  s: AnsiString;
  Len: Integer;
begin
  s := AnsiString(_s);
  Len := Length(s);
  SetLength(Result, Len + 1);
  ZeroMemory(@Result[0], Len + 1);
  if Len > 0 then
    Move(s[1], Result[0], Len);
end;

procedure StringToByteArray(const _s: string; var _buf: array of Byte; _ArraySize: Integer); overload;
var
  s: AnsiString;
  Len: Integer;
begin
  s := AnsiString(_s);
  Len := Length(s);
  if Len > _ArraySize - 1 then
    Len := _ArraySize - 1;
  ZeroMemory(@_buf[0], _ArraySize);
  if Len > 0 then
    Move(s[1], _buf[0], Len);
end;

function ByteArrayToString(const _buf: array of Byte): string;
var
  s: AnsiString;
begin
  s := PAnsiChar(@_buf[0]);
  Result := string(s);
end;

function StrTrimNul(var _s: AnsiString): Integer;
var
  i: Integer;
begin
  Result := Length(_s);
  for i := 1 to Result do begin
    if _s[i] = #0 then begin
      Result := i - 1;
      SetLength(_s, Result);
      Exit; //==>
    end;
  end;
end;

function StrTrimZero(var _s: AnsiString): Integer;
begin
  Result := StrTrimNul(_s);
end;

function IsStringIn(const _s: string; const _Arr: array of string; out _Idx: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(_Arr) - 1 do
    if _s = _Arr[i] then begin
      Result := True;
      _Idx := i;
      Exit; //==>
    end;
end;

function IsStringIn(const _s: string; const _Arr: array of string): Boolean;
var
  Idx: Integer;
begin
  Result := IsStringIn(_s, _Arr, Idx);
end;

function IsTextIn(const _s: string; const _Arr: array of string; out _Idx: Integer): Boolean; overload;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(_Arr) - 1 do
    if SameText(_s, _Arr[i]) then begin
      _Idx := i;
      Result := True;
      Exit; //==>
    end;
end;

function IsTextIn(const _s: string; const _Arr: array of string): Boolean;
var
  Idx: Integer;
begin
  Result := IsTextIn(_s, _Arr, Idx);
end;

function StrToLowAscii(const _s: WideString): AnsiString;
const
  CodePage = 20127; //20127 = us-ascii
begin
  SetLength(Result, WideCharToMultiByte(CodePage, 0, PWideChar(_s),
    Length(_s), nil, 0, nil, nil));
  WideCharToMultiByte(CodePage, 0, PWideChar(_s), Length(_s),
    PAnsiChar(Result), Length(Result), nil, nil);
end;

{$IFNDEF SUPPORTS_UNICODE}

function CharInSet(_c: Char; const _CharSet: TSysCharSet): Boolean;
begin
  Result := _c in _CharSet;
end;

{$ENDIF ~SUPPORTS_UNICODE}

{$IFDEF SUPPORTS_UNICODE}

function Copy(const _s: AnsiString; _Pos, _Len: Integer): AnsiString;
begin
  SetLength(Result, _Len);
  Move(_s[_Pos], Result[1], _Len);
end;

function Copy(const _s: AnsiString; _Pos: Integer): AnsiString;
begin
  Result := Copy(_s, _Pos, Length(_s) - _Pos);
end;

function Copy(const _s: string; _Pos, _Len: Integer): string; inline;
begin
  Result := System.Copy(_s, _Pos, _Len);
end;

function Copy(const _s: string; _Pos: Integer): string; inline;
begin
  Result := System.Copy(_s, _Pos);
end;
{$ENDIF SUPPORTS_UNICODE}

{$IFNDEF DELPHI2005_UP}
// Delphi 6 does not have Copy without the length parameter
function Copy(const _s: string; _Pos: Integer): string;
begin
  Result := Copy(_s, _Pos, Length(_s) - _Pos + 1);
end;

function Copy(const _s: string; _Pos, _Len: Integer): string;
begin
  Result := System.Copy(_s, _Pos, _Len);
end;
{$ENDIF ~DELPHI2005_UP}

function MakeUniqueString(const _s: string): string;
begin
  Result := _s;
  UniqueString(Result);
end;

function MakeUniqueString(const _s: WideString): WideString; overload;
begin
  Result := _s;
  UniqueString(Result);
end;

end.
