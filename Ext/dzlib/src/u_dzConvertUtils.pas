{.GXFormatter.config=twm}
///<summary>
/// Integer to string and string to integer conversion functions for decimal
/// hexadecimal and custom number bases. This was taken from u_dzStringUtils
/// which originally was a Delphi conversion of TwmStringFunc. </summary>
unit u_dzConvertUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  u_dzTypes,
  u_dzTranslator;

{$IF Declared(TFormatSettings)}
// Delphi 6 does not have that type
var
  ///<summary>
  /// contains the User's format setting, but with decimal separator = '.' and no thousands
  /// separator </summary>
  DZ_FORMAT_DECIMAL_POINT: TFormatSettings;
{$IFEND}

{$IF Declared(FormatSettings)}
function DecimalSeparator: Char; inline;
{$IFEND}

type
  ///<summary>
  /// Raised by the number conversion functions if a digit is invalid for the given base. </summary>
  EdzConvert = class(EdzException);
  EDigitOutOfRange = class(EdzConvert);
  ///<summary>
  /// raised if there is a conversion error in one of the Str2XxxEx functions </summary>
  EStringConvertError = class(EdzConvert);

type
{$IFDEF DELPHI2005_UP}
  ULong = LongWord deprecated; // use UInt32
{$ELSE}
  ULong = LongWord; // use UInt32
{$ENDIF}

type
  TBaseN = 2..36;

const
  MaxLongWord = $FFFFFFFF;

const
  /// <summary>
  /// Array containing all characters that can be used as digits </summary>
  DIGIT_CHARS: string = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

// Str <-> Decimal conversion
///<summary>
/// Returns true if A is a valid decimal digit </summary>
function isDecDigit(_a: Char): Boolean; overload;
{$IFDEF unicode}
function isDecDigit(_a: AnsiChar): Boolean; overload;
{$ENDIF}

///<summary>
/// Returns true if S is a valid positive decimal number </summary>
function isDec(const _s: string): Boolean; overload;
{$IFDEF unicode}
function isDec(const _s: AnsiString): Boolean; overload;
{$ENDIF}

///<summary>
/// Converts a decimal digit to its number equivalent
/// @Raises EDigitOutOfRange if there is an invalid digit. </summary>
function DecDigit2Long(_a: Char): UInt32; overload;
{$IFDEF unicode}
function DecDigit2Long(_a: AnsiChar): UInt32; overload;
{$ENDIF}
///<summary>
/// Converts a string representing a positive decimal number to a number
/// @Raises EDigitOutOfRange if there is an invalid digit. </summary>
function Dec2Long(const _s: string): UInt32; overload;
{$IFDEF unicode}
function Dec2Long(const _s: AnsiString): UInt32; overload;
{$ENDIF}

function TryDec2Long(const _s: string; out _l: UInt32): Boolean; overload;
{$IFDEF unicode}
function TryDec2Long(const _s: AnsiString; out _l: UInt32): Boolean; overload;
{$ENDIF}

///<summary>
/// Converts a positive number to its 2 digit decimal representation (left pads with '0') </summary>
function Long2Dec2(_l: UInt32): string;
///<summary>
/// Converts a positive number to its 4 digit decimal representation (left pads with '0') </summary>
function Long2Dec4(_l: UInt32): string;
///<summary>
/// Converts a positive number to its N digit decimal representation (left pads with '0') </summary>
function Long2DecN(_l: UInt32; _n: UInt32): string;
///<summary>
/// Converts a positive number to its decimal representation </summary>
function Long2Dec(_l: UInt32): string;
function Long2DecA(_l: UInt32): AnsiString;

// Str <-> Hex conversion
///<summary>
/// Returns true if A is a valid hexadecimal (base 16) digit </summary>
function isHexDigit(_a: Char): Boolean;
///<summary>
/// Returns true if S is a valid hexadecimal (base 16) number </summary>
function isHex(const _s: string): Boolean;
///<summary>
/// Converts a hexadecimal digit to its number equivalent
/// Raises EDigitOutOfRange if there is an invalid digit. </summary>
function HexDigit2Long(_a: Char): Longint;
///<summary>
/// Converts a string representing a hexadecimal number to a number
/// @Raises EDigitOutOfRange if there is an invalid digit. </summary>
function Hex2Long(const _s: string): UInt32;
///<summary>
/// Tries to interpret the string as the hexadecimal interpretation of a number and
/// returns the value.
/// @value is the converted value, only valid it result = true
/// @returns true, if the string could be converted, false otherwise </summary>
function TryHex2Long(const _s: string; out _Value: UInt32): Boolean;
///<summary>
/// Converts a number to its hexadecimal representation </summary>
function Long2Hex(_l: UInt32): string;
///<summary>
/// converts a number to its hexadecimal representation left padding with 0 to a length of 2 </summary>
function Long2Hex2(_l: UInt32): string;
///<summary>
/// converts a number to its hexadecimal representation left padding with 0 to a length of 4 </summary>
function Long2Hex4(_l: UInt32): string;

///<summary>
/// converts a number to its hexadecimal representation left padding with 0 to a length of Digits </summary>
function Long2HexN(_l: UInt32; _Digits: Byte): string;

// Str <-> any numeric system conversion up to Base36 (that is digits 0..Z)
///<summary>
/// Returns true if A is a valid digit in the given Base. </summary>
function isDigit(_a: Char; _Base: TBaseN): Boolean; overload;
{$IFDEF unicode}
function isDigit(_a: AnsiChar; _Base: TBaseN): Boolean; overload;
{$ENDIF}

///<summary>
/// Returns true if S is a valid number in the given Base. </summary>
function isNumber(const _s: string; _Base: TBaseN): Boolean;
///<summary>
/// Converts a Base digit to its number equivalent.
/// @Raises EDigitOutOfRange if there is an invalid digit. </summary>
function Digit2Long(_a: Char; _Base: TBaseN): UInt32;
///<summary>
/// Converts a string representing a number in Base to a number.
/// @Raises EDigitOutOfRange if there is an invalid digit.  </summary>
function Num2Long(const _s: string; _Base: TBaseN): UInt32;

///<summary>
/// Tries to convert a string representing a number in Base to a number.
/// @Value contains the converted number, only valid if Result = true
/// @returns true, if the conversion succeeds, false otherwise. </summary>
function TryNum2Long(const _s: string; _Base: TBaseN; out _Value: UInt32): Boolean; overload;
{$IFDEF unicode}
function TryNum2Long(const _s: AnsiString; _Base: TBaseN; out _Value: UInt32): Boolean; overload;
{$ENDIF}

///<summary>
/// Converts a number to its Base representation. </summary>
function Long2Num(_l: UInt32; _Base: Byte; _MinWidth: Integer = 1): string;
///<summary>
/// Returns the number of characters in S that are valid digits in the given Base. </summary>
function isNumberN(const _s: string; _Base: TBaseN): Integer;

///<summary>
/// Reduces an Integer to a Byte value by cutting it off at 0 and 255 </summary>
function ReduceToByte(const _Value: Integer): Byte;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function ReduceToInt8(const _Value: Integer): Int8;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function ReduceToUInt8(const _Value: Integer): UInt8;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function ReduceToInt16(const _Value: Integer): Int16;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function ReduceToUInt16(const _Value: Integer): UInt16;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function ReduceToInt32(const _Value: Int64): Int32;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function ReduceToUInt32(const _Value: Int64): UInt32;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

///<summary>
/// Converts a string of the form '-hh:mm:ss', 'hh:mm:ss',
/// '+hh:mm:ss', 'mm:ss' or 'ss' to a number of seconds. </summary>
function TimeStrToSeconds(const _Zeit: string): Integer;
///<summary>
/// deprecated, use SecondsToTimeStr instead </summary>
function SecondsToStr(_Seconds: Integer): string; deprecated;
///<summary>
/// Converts a number of seconds to a string of the form
/// 'hh:mm:ss'. The string will always contain hours and minutes
/// even if Seconds < 60. </summary>
function SecondsToTimeStr(_Seconds: Integer): string;
{$IFDEF Delphi7up}
function TimeToSeconds(_Zeit: TDateTime): Integer; deprecated;
{$ENDIF}

///<summary>
/// Converts a string to an integer.
/// If s can not be converted, it returns the Default.
/// @param(s string to convert)
/// @param(Default value to return if s can not be converted)
/// @returns(the integer value of s or Default, if s can not be converted) </summary>
function Str2Int(const _s: string; _Default: Integer): Integer; overload;

///<summary>
/// Converts a string to an integer.
/// If s can not be converted, it raises an exception EStringConvertError.
/// @param s string to convert
/// @param Source string to include in the exception message
/// @returns the integer value of s
/// @raises EStringConvertError if s can not be converted </summary>
function Str2Int(const _s: string; const _Source: string): Integer; overload;

///<summary>
/// Does the same as TryStrToInt but does not change Value if the string cannot be converted. </summary>
function TryStr2Int(const _s: string; var _Value: Integer): Boolean; overload;
{$IFDEF SUPPORTS_UNICODE}
function TryStr2Int(const _s: AnsiString; var _Value: Integer): Boolean; overload;
{$ENDIF SUPPORTS_UNICODE}

///<summary>
/// Converts a string to an int64.
/// If s can not be converted, it returns the Default.
/// @param s string to convert
/// @param Default value to return if s can not be converted
/// @returns the int64 value of s or Default, if s can not be converted </summary>
function Str2Int64(const _s: string; _Default: Int64): Int64; overload;

///<summary>
/// Converts a string to an int64.
/// If s can not be converted, it raises an exception EStringConvertError.
/// @param s string to convert
/// @param Source string to include in the exception message
/// @returns the integer value of s
/// @raises EStringConvertError if s can not be converted </summary>
function Str2Int64(const _s: string; const _Source: string): Int64; overload;

///<summary>
/// tries to guess the decimal separator </summary>
function GuessDecimalSeparator(const _s: string): Char;

///<summary>
/// Converts a string to a float.
/// If s can not be converted, it returns the Default.
/// @param s string to convert
/// @param Default value to return if s can not be converted
/// @param DecSeparator is the decimal separator, defaults to '.'
///        if passed as #0, GuessDecimalSeparator is called to guess it
/// @returns the float value of s or Default, if s can not be converted </summary>
function Str2Float(const _s: string; _Default: Extended; _DecSeparator: Char = '.'): Extended; overload;

/// <summary>
/// Converts a string to a float.
/// If s can not be converted, it raises an exception EStringConvertError.
/// @param s string to convert
/// @param Source string to include in the exception message
/// @param DecSeparator is the decimal separator, defaults to '.'
///        if passed as #0, GuessDecimalSeparator is called to guess it
/// @returns the float value of s
/// @raises EStringConvertError if s can not be converted </summary>
function Str2Float(const _s: string; const _Source: string; _DecSeparator: Char = '.'): Extended; overload;

///<summary>
/// tries to convert a string to a float, returns false if it fails
/// @param s is the string to convert
/// @param flt is the float, only valid if the function returns true
/// @param DecSeparator is the decimal separator to use, defaults to '.',
///        if passed as #0, GuessDecimalSeparator is called to guess it
/// @returns true, if s could be converted, false otherwise
/// NOTE: This is not thread safe in Delphi 6 because there it changes the global
///       variable DecimalSeparator in SysUtils. </summary>
//{$IFNDEF Win64}
function TryStr2Float(const _s: string; out _flt: Extended; _DecSeparator: Char = '.'): Boolean; overload;
//{$ENDIF}
///<summary>
/// tries to convert a string to a float, returns false if it fails
/// @param s is the string to convert
/// @param flt is the float, only valid if the function returns true
/// @param DecSeparator is the decimal separator to use, defaults to '.',
///        if passed as #0, GuessDecimalSeparator is called to guess it
/// @returns true, if s could be converted, false otherwise
/// NOTE: This is not thread safe in Delphi 6 because there it changes the global
///       variable DecimalSeparator in SysUtils. </summary>
function TryStr2Float(const _s: string; out _flt: Double; _DecSeparator: Char = '.'): Boolean; overload;
function TryStr2Float(const _s: string; out _flt: Single; _DecSeparator: Char = '.'): Boolean; overload;

///<summary>
/// Converts a floating point number to a string using the given decimal separator
/// in "General number format" with 15 significant digits
/// @param flt is an extended floating point value
/// @param DecSeparator is the decimal separator to use
/// @returns a string representation of the floating point value </summary>
function Float2Str(_flt: Extended; _DecSeparator: Char = '.'): string; overload;

///<summary>
/// Converts a floating point number to a string using the given with, number of decimals
/// and the given decimal separator, if width is too small the smallest representation possible
/// will be used (eg. Float2Str(5.24, 3, 2, '.') = '5.24')
/// @param flt is an extended floating point value
/// @param Width is the total number of digits (including the decimal separator)
/// @param Decimals is the number of decimals to use
/// @param DecSeparator is the decimal separator to use, defaults to '.'
/// @returns a string representation of the floating point value </summary>
function Float2Str(_flt: Extended; _Width, _Decimals: Integer; _DecSeparator: Char = '.'): string; overload;
///<summary>
/// Converts a floating point number to a string using the given number of decimals
/// and a '.' as decimal separator.
/// @param flt is an extended floating point value
/// @param Decimals is the number of decimals to use
/// @param DecSeparator is the decimal separator to use, defaults to '.'
/// @returns a string representation of the floating point value </summary>
function Float2Str(_flt: Extended; _Decimals: Integer; _DecSeparator: Char = '.'): string; overload;

///<summary>
/// Tries to round a floating point value to a word value
/// @param flt is the value to convert
/// @param wrd returns the word value, only valid if result = true
/// @returns true, if the result can be stored i a word, false otherwise. </summary>
function TryRound(_flt: Extended; out _wrd: Word): Boolean;

///<summary>
/// these contants refer to the "Xx binary byte" units as defined by the
/// International Electronical Commission (IEC) and endorsed by the
/// IEE and CiPM </summary>
const
  OneKibiByte = Int64(1024);
  OneMebiByte = Int64(1024) * OneKibiByte;
  OneGibiByte = Int64(1024) * OneMebiByte;
  OneTebiByte = Int64(1024) * OneGibiByte;
  OnePebiByte = Int64(1024) * OneTebiByte;
  OneExbiByte = Int64(1024) * OnePebiByte;

///<summary>
/// Converts a file size to a human readable string, e.g. 536870912000 = 5.00 GiB (gibibyte) </summary>
function FileSizeToHumanReadableString(_FileSize: Int64): string;

const
  SecondsPerMinute = 60;
  MinutesPerHour = 60;
  SecondsPerHour = SecondsPerMinute * MinutesPerHour;
  HoursPerDay = 24;
  MinutesPerDay = HoursPerDay * MinutesPerHour;
  SecondsPerDay = MinutesPerDay * SecondsPerMinute;
  MillisecondsPerSecond = 1000;
  MillisecondsPerMinute = SecondsPerMinute * MillisecondsPerSecond;
  MillisecondsPerHour = SecondsPerHour * MillisecondsPerSecond;
  MillisecondsPerDay = SecondsPerDay * MillisecondsPerSecond;
  MicrosecondsPerMillisecond = 1000;
  MicrosecondsPerSecond = MillisecondsPerSecond * MicrosecondsPerMillisecond;
  MicrosecondsPerMinute = Int64(SecondsPerMinute) * MicrosecondsPerSecond;
  MicrosecondsPerHour = Int64(SecondsPerHour) * MicrosecondsPerSecond;
  MicrosecondsPerDay = Int64(SecondsPerDay) * MicrosecondsPerSecond;

const
  OneMicrosecond = 1 / MSecsPerDay / 1000;

///<summary>
/// returns a human readable string of the form '5d 23h' or '25h 15m' or '20m 21s' </summary>
function SecondsToHumanReadableString(_Seconds: Int64): string; overload;
///<summary>
/// returns a human readable string of the form '5d 23h' or '25h 15m' or '20m 21s'
/// @note that the value will get rounded to full seconds. </summary>
function SecondsToHumanReadableString(const _Seconds: Extended): string; overload;

{$IF Declared(TFormatSettings)}
///<summary>
/// returns the default locale settings as read from the user's regional settings </summary>
function GetUserDefaultLocaleSettings: TFormatSettings; deprecated; // use u_dzStringUtils.GetUserDefaultLocaleSettings instead
///<summary>
/// returns the default locale settings as read from the system's regional settings </summary>
function GetSystemDefaultLocaleSettings: TFormatSettings; deprecated; // use u_dzStringUtils.GetSystemDefaultLocaleSettings instead
{$IFEND}

///<summary>
/// returns the long word split into an array of byte
/// @param Value is the LongWord value to split
/// @param MsbFirst, if true the most significant byte is the first in the array (Motorola format)
///                  if false the least significatn byte is the first in the array (Intel format) </summary>
function LongWord2ByteArr(_Value: LongWord; _MsbFirst: Boolean = False): TBytes;

///<summary>
/// returns the the array of byte combined into a LongWord
/// @param Value is the array to combine
/// @param MsbFirst, if true the most significant byte is the first in the array (Motorola format)
///                  if false the least significatn byte is the first in the array (Intel format) </summary>
function ByteArr2LongWord(const _Arr: array of Byte; _MsbFirst: Boolean = False): LongWord;

///<summary>
/// returns a 16 bit in reversed byte order, e.g. $1234 => $3412)
/// aka converts intel (little endian) to motorola (big endian) byte order format
/// (This is just an alias for system.swap for consistency with Swap32.)
///</summary
function Swap16(_Value: Word): Word;

///<summary>
/// returns a 32 bit value in reversed byte order e.g. $12345678 -> $78563412
/// aka converts intel (little endian) to motorola (big endian) byte order format </summary>
function Swap32(_Value: LongWord): LongWord;
function Swap32pas(_Value: LongWord): LongWord;

///<summary>
/// returns a 64 bit value in reversed byte order e.g. $123456789ABCDEF0 -> $F0DEBC9A78563412
/// aka converts intel (little endian) to motorola (big endian) byte order format </summary>
function Swap64(_Value: UInt64): UInt64;

function BitReverse32(v: LongWord): LongWord;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
type
  TBoolToStr = record
  private
    FBoolStrings: array[False..True] of string;
  public
    ///<summary> Uses 'True' and 'False' </summary>
    class function CreateTrueFalse: TBoolToStr; static;
    ///<summary> Uses 'Yes' and 'No' </summary>
    class function CreateYesNo: TBoolToStr; static;
    ///<summary> Uses 'Y' and 'N' </summary>
    class function CreateYN: TBoolToStr; static;
    ///<summary> Uses _('True') and _('False') </summary>
    class function CreateTrueFalseLocalized: TBoolToStr; static;
    ///<summary> Uses _('Yes') and _('No') </summary>
    class function CreateYesNoLocalized: TBoolToStr; static;
    ///<summary> Uses _('Y(es)') and _('N(o)') if it was translated,
    ///          otherwise it uses 'Y' and 'N'. </summary>
    class function CreateYNLocalized: TBoolToStr; static;
    ///<summary> Uses the TrueStr and FalseStr supplied. </summary>
    class function Create(const _TrueStr, _FalseStr: string): TBoolToStr; static;
    function ToString(_b: Boolean): string;
  end;
{$ENDIF}

///<summary> Uses 'True' and 'False' (no translation) </summary>
function Bool2Str(_b: Boolean): string;

type
  TBitNumber64 = 0..63;
  TByteNumber64 = 0..7;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
type
  ///<summary>
  /// Stores up to 64 bits similar to the Delphi TBits class but
  /// as a record, so it does not need a destructor </summary>
  TBits64 = record
  public
    type
      TBitNumber = TBitNumber64;
      TByteNumber = TByteNumber64;
      TValue = UInt64;
    const
      Low = 0;
      Bits = 64;
      High = Bits - 1;
  private
    // unfortunately Delphi 2007 does not allow us to use TValue here
    FValue: UInt64;
  public
    class function Create(_Value: TValue): TBits64; static;
    class function AllSet: TBits64; static;
    class function NoneSet: TBits64; static;
    procedure Init(_Value: TValue);
    function IsBitSet(_BitNo: TBitNumber): Boolean;
    procedure SetBit(_BitNo: TBitNumber; _BitValue: Boolean);
    ///<summary>
    /// interpret the given bit range as an integer and return it </summary>
    function Extract(_BitFirst, _BitLast: TBitNumber): TValue;
    ///<summary>
    /// Overwrite the given bit range with the given value (reverse of Extract) </summary>
    procedure Overwrite(_BitFirst, _BitLast: TBitNumber; _Value: TValue);
    function Value: TValue;
    function GetByte(_ByteNo: TByteNumber): Byte;
    procedure SetByte(_ByteNo: TByteNumber; _Value: Byte);
    function AsString: string;
    class operator BitwiseAnd(_a, _b: TBits64): TBits64;
    class operator BitwiseOr(_a, _b: TBits64): TBits64;
    class operator BitwiseXor(_a, _b: TBits64): TBits64;
    // There is no BitwiseNot operator, but the LogicalNot also works
    class operator LogicalNot(_a: TBits64): TBits64;
    class operator Equal(_a, _b: TBits64): Boolean;
  end;
{$ENDIF}

type
  TBitNumber32 = 0..31;
  TByteNumber32 = 0..3;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
type
  ///<summary>
  /// Stores up to 32 bits similar to the Delphi TBits class but
  /// as a record, so it does not need a destructor </summary>
  TBits32 = record
  public
    type
      TBitNumber = TBitNumber32;
      TByteNumber = TByteNumber32;
      TValue = UInt32;
    const
      Low = 0;
      Bits = 32;
      High = Bits - 1;
  private
    // unfortunately Delphi 2007 does not allow us to use TValue here
    FValue: UInt32;
  public
    class function Create(_Value: TValue): TBits32; static;
    class function AllSet: TBits32; static;
    class function NoneSet: TBits32; static;
    procedure Init(_Value: TValue);
    function IsBitSet(_BitNo: TBitNumber): Boolean;
    procedure SetBit(_BitNo: TBitNumber; _BitValue: Boolean);
    ///<summary>
    /// interpret the given bit range as an integer and return it </summary>
    function Extract(_BitFirst, _BitLast: TBitNumber): TValue;
    ///<summary>
    /// Overwrite the given bit range with the given value (reverse of Extract) </summary>
    procedure Overwrite(_BitFirst, _BitLast: TBitNumber; _Value: TValue);
    function Value: TValue;
    function GetByte(_ByteNo: TByteNumber): Byte;
    procedure SetByte(_ByteNo: TByteNumber; _Value: Byte);
    function AsString: string;
    class operator BitwiseAnd(_a, _b: TBits32): TBits32;
    class operator BitwiseOr(_a, _b: TBits32): TBits32;
    class operator BitwiseXor(_a, _b: TBits32): TBits32;
    // There is no BitwiseNot operator, but the LogicalNot also works
    class operator LogicalNot(_a: TBits32): TBits32;
    class operator Equal(_a, _b: TBits32): Boolean;
  end;
{$ENDIF}

type
  TBitNumber16 = 0..15;
  TByteNumber16 = 0..1;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
type
  ///<summary>
  /// Stores up to 16 bits similar to the Delphi TBits class but
  /// as a record, so it does not need a destructor </summary>
  TBits16 = record
  public
    type
      TBitNumber = TBitNumber16;
      TByteNumber = TByteNumber16;
      TValue = UInt16;
    const
      Low = 0;
      Bits = 16;
      High = Bits - 1;
  private
    // unfortunately Delphi 2007 does not allow us to use TValue here
    FValue: UInt16;
  public
    class function Create(_Value: TValue): TBits16; static;
    class function AllSet: TBits16; static;
    class function NoneSet: TBits16; static;
    procedure Init(_Value: TValue);
    function IsBitSet(_BitNo: TBitNumber): Boolean;
    function IsAnyBitSet: Boolean;
    procedure SetBit(_BitNo: TBitNumber; _BitValue: Boolean);
    ///<summary>
    /// interpret the given bit range as an integer and return it </summary>
    function Extract(_BitFirst, _BitLast: TBitNumber): TValue;
    ///<summary>
    /// Overwrite the given bit range with the given value (reverse of Extract) </summary>
    procedure Overwrite(_BitFirst, _BitLast: TBitNumber; _Value: TValue);
    function Value: TValue;
    function GetByte(_ByteNo: TByteNumber): Byte;
    procedure SetByte(_ByteNo: TByteNumber; _Value: Byte);
    function AsString: string;
    class operator BitwiseAnd(_a, _b: TBits16): TBits16;
    class operator BitwiseOr(_a, _b: TBits16): TBits16;
    class operator BitwiseXor(_a, _b: TBits16): TBits16;
    // There is no BitwiseNot operator, but the LogicalNot also works
    class operator LogicalNot(_a: TBits16): TBits16;
    class operator Equal(_a, _b: TBits16): Boolean;
  end;
{$ENDIF}

type
  TBitNumber8 = 0..7;
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
type
  ///<summary>
  /// Stores up to 8 bits similar to the Delphi TBits class but
  /// as a record, so it does not need a destructor </summary>
  TBits8 = record
  public
    type
      TBitNumber = TBitNumber8;
      TByteNumber = 0..0;
      TValue = UInt8;
    const
      Low = 0;
      Bits = 8;
      High = Bits - 1;
  private
    // unfortunately Delphi 2007 does not allow us to use TValue here
    FValue: UInt8;
  public
    class function Create(_Value: TValue): TBits8; static;
    class function AllSet: TBits8; static;
    class function NoneSet: TBits8; static;
    procedure Init(_Value: TValue);
    function IsBitSet(_BitNo: TBitNumber): Boolean;
    function IsAnyBitSet: Boolean;
    procedure SetBit(_BitNo: TBitNumber; _BitValue: Boolean);
    ///<summary>
    /// interpret the given bit range as an integer and return it </summary>
    function Extract(_BitFirst, _BitLast: TBitNumber): TValue;
    ///<summary>
    /// Overwrite the given bit range with the given value (reverse of Extract) </summary>
    procedure Overwrite(_BitFirst, _BitLast: TBitNumber; _Value: TValue);
    function Value: TValue;
    function GetByte(_ByteNo: TByteNumber): Byte;
    procedure SetByte(_ByteNo: TByteNumber; _Value: Byte);
    function AsString: string;
    // according to the documentation:
    // > For a logical operator and a bitwise operator using the same symbol,
    // > the logical operator is used only when the operands are booleans.
    // > Since the type of the class of this class operator is not a boolean,
    // > a logical operator will only be used when the other operand is a Boolean.
    // So, what does that mean?
    // Tests have shown that, if only one of BitwiseXxx or LogicalXxx exists, that one
    // will always be called. And that's what we want anyway.
    class operator BitwiseAnd(_a, _b: TBits8): TBits8;
    class operator BitwiseOr(_a, _b: TBits8): TBits8;
    class operator BitwiseXor(_a, _b: TBits8): TBits8;
    // There is no BitwiseNot operator, but the LogicalNot also works
    class operator LogicalNot(_a: TBits8): TBits8;
    class operator Equal(_a, _b: TBits8): Boolean;
  end;
{$ENDIF}
  { TODO -otwm :
    Create a generic TdzBits record that stores the value in a dynamically allocated byte array.
    Since this array is automatically initialized/finalized, it can still be a record rather than a class. }
//type
//  TdzBits = record
//  private
//    FValue: TBytes;
//  public
//    class function Create(_Size: Integer): TdzBits; static;
//    procedure Init(_Size: Integer);
//    function IsBitSet(_BitNo: Word): Boolean;
//    procedure SetBit(_BitNo: Word; _BitValue: Boolean);
//    function Value: Byte;
//    function AsString: string;
//  end;

implementation

uses
  Windows,
  DateUtils,
  StrUtils,
  u_dzStringUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

function isDigit(_a: Char; _Base: TBaseN): Boolean;
begin
  Result := (Pos(UpCase(_a), LeftStr(DIGIT_CHARS, _Base)) <> 0);
end;

{$IFDEF unicode}
function isDigit(_a: AnsiChar; _Base: TBaseN): Boolean; overload;
begin
  Result := (Pos(UnicodeString(UpCase(_a)), UnicodeString(LeftStr(DIGIT_CHARS, _Base))) <> 0);
end;
{$ENDIF}

function isNumber(const _s: string; _Base: TBaseN): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Length(_s) = 0 then
    Exit;
  for i := 1 to Length(_s) do
    if not isDigit(_s[i], _Base) then
      Exit;
  Result := True;
end;

function isNumberN(const _s: string; _Base: TBaseN): Integer;
begin
  Result := 0;
  while (Result < Length(_s)) and isDigit(_s[Result + 1], _Base) do
    Inc(Result);
end;

function Digit2Long(_a: Char; _Base: TBaseN): UInt32;
begin
  Result := Pos(UpCase(_a), LeftStr(DIGIT_CHARS, _Base));
  if Result = 0 then
    raise EDigitOutOfRange.CreateFmt(_('Digit out of range %s'), [_a]);
  Dec(Result);
end;

function Num2Long(const _s: string; _Base: TBaseN): UInt32;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(_s) do
    if isDigit(_s[i], _Base) then
      Result := (Result * _Base + UInt32(Pos(UpCase(_s[i]), DIGIT_CHARS)) - 1)
    else
      raise EDigitOutOfRange.CreateFmt(_('Digit #%d (%s) out of range'), [i, _s[i]]);
end;

function TryNum2Long(const _s: string; _Base: TBaseN; out _Value: UInt32): Boolean;
var
  i: Integer;
begin
  Result := False;
  _Value := 0;
  for i := 1 to Length(_s) do begin
    if isDigit(_s[i], _Base) then
      _Value := (_Value * _Base + UInt32(Pos(UpCase(_s[i]), DIGIT_CHARS)) - 1)
    else begin
      Exit;
    end;
  end;
  Result := True;
end;

{$IFDEF unicode}
function TryNum2Long(const _s: AnsiString; _Base: TBaseN; out _Value: UInt32): Boolean;
var
  i: Integer;
begin
  Result := False;
  _Value := 0;
  for i := 1 to Length(_s) do begin
    if isDigit(_s[i], _Base) then
      _Value := (_Value * _Base + UInt32(Pos(string(UpCase(_s[i])), DIGIT_CHARS)) - 1)
    else begin
      Exit;
    end;
  end;
  Result := True;
end;
{$ENDIF}

function Long2Num(_l: UInt32; _Base: Byte; _MinWidth: Integer = 1): string;
var
  m: Byte;
begin
  Result := '';
  while _l > 0 do begin
    m := _l mod _Base;
    _l := _l div _Base;
    Result := DIGIT_CHARS[m + 1] + Result;
  end;
  while Length(Result) < _MinWidth do
    Result := '0' + Result;
end;

// Inlined method must be implemented before it is called
function ReduceToUInt8(const _Value: Integer): UInt8;
begin
  // this could call EnsureRange(0, MaxUIn8) in Math, but I am not sure which Delphi
  // versions support these functions.
  if _Value < 0 then
    Result := 0
  else if _Value > MaxUInt8 then
    Result := MaxUInt8
  else
    Result := _Value;
end;

function ReduceToByte(const _Value: Integer): Byte;
begin
  Result := ReduceToUInt8(_Value);
end;

function ReduceToInt8(const _Value: Integer): Int8;
begin
  // this could call EnsureRange(MinInt8, MaxIn8) in Math, but I am not sure which Delphi
  // versions support these functions.
  if _Value < MinInt8 then
    Result := MinInt8
  else if _Value > MaxInt8 then
    Result := MaxInt8
  else
    Result := _Value;
end;

function ReduceToUInt16(const _Value: Integer): UInt16;
begin
  // this could call EnsureRange(0, MaxUIn16) in Math, but I am not sure which Delphi
  // versions support these functions.
  if _Value < 0 then
    Result := 0
  else if _Value > MaxUInt16 then
    Result := MaxUInt16
  else
    Result := _Value;
end;

function ReduceToInt16(const _Value: Integer): Int16;
begin
  // this could call EnsureRange(MinInt16, MaxIn16) in Math, but I am not sure which Delphi
  // versions support these functions.
  if _Value < MinInt16 then
    Result := MinInt16
  else if _Value > MaxInt16 then
    Result := MaxInt16
  else
    Result := _Value;
end;

function ReduceToUInt32(const _Value: Int64): UInt32;
begin
  if _Value < 0 then
    Result := 0
  else if _Value > MaxUInt32 then
    Result := MaxUInt32
  else
    Result := _Value;
end;

function ReduceToInt32(const _Value: Int64): Int32;
begin
  if _Value < MinInt32 then
    Result := MinInt32
  else if _Value > MaxInt32 then
    Result := MaxInt32
  else
    Result := _Value;
end;

function isHexDigit(_a: Char): Boolean;
begin
  Result := isDigit(_a, 16);
end;

function isHex(const _s: string): Boolean;
begin
  Result := isNumber(_s, 16);
end;

function HexDigit2Long(_a: Char): Longint;
begin
  Result := Digit2Long(_a, 16);
end;

function Hex2Long(const _s: string): UInt32;
begin
  Result := Num2Long(_s, 16);
end;

function TryHex2Long(const _s: string; out _Value: UInt32): Boolean;
begin
  Result := TryNum2Long(_s, 16, _Value);
end;

function Long2Hex(_l: UInt32): string;
begin
  Result := Long2Num(_l, 16);
end;

function Long2HexN(_l: UInt32; _Digits: Byte): string;
begin
  Result := Long2Hex(_l);
  Result := StringOfChar('0', _Digits - Length(Result)) + Result;
end;

function Long2Hex2(_l: UInt32): string;
begin
  Result := Long2Hex(_l);
  if Length(Result) < 2 then
    Result := '0' + Result;
end;

function Long2Hex4(_l: UInt32): string;
var
  Len: Integer;
begin
  Result := Long2Hex(_l);
  Len := Length(Result);
  if Len < 4 then
    Result := LeftStr('0000', 4 - Len) + Result;
end;

function isDecDigit(_a: Char): Boolean;
begin
  Result := isDigit(_a, 10);
end;

{$IFDEF unicode}
function isDecDigit(_a: AnsiChar): Boolean;
begin
  Result := isDigit(Char(_a), 10);
end;
{$ENDIF}

function isDec(const _s: string): Boolean;
begin
  Result := isNumber(_s, 10);
end;

{$IFDEF unicode}
function isDec(const _s: AnsiString): Boolean;
begin
  Result := isDec(string(_s));
end;
{$ENDIF}

function DecDigit2Long(_a: Char): UInt32;
begin
  Result := Digit2Long(_a, 10);
end;

{$IFDEF unicode}
function DecDigit2Long(_a: AnsiChar): UInt32;
begin
  Result := Digit2Long(Char(_a), 10);
end;
{$ENDIF}

function Dec2Long(const _s: string): UInt32;
var
  c: Char;
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(_s) do begin
    c := _s[i];
    Result := Result * 10 + DecDigit2Long(c);
  end;
end;

{$IFDEF unicode}
function Dec2Long(const _s: AnsiString): UInt32; overload;
var
  c: AnsiChar;
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(_s) do begin
    c := _s[i];
    Result := Result * 10 + DecDigit2Long(c);
  end;
end;
{$ENDIF}

function TryDec2Long(const _s: string; out _l: UInt32): Boolean;
begin
  Result := TryNum2Long(_s, 10, _l);
end;

{$IFDEF unicode}
function TryDec2Long(const _s: AnsiString; out _l: UInt32): Boolean;
begin
  Result := TryNum2Long(_s, 10, _l);
end;
{$ENDIF}

function Long2Dec(_l: UInt32): string;
var
  s: AnsiString;
begin
  Str(_l, s);
  Result := string(s);
end;

function Long2DecA(_l: UInt32): AnsiString;
begin
  Str(_l, Result);
end;

function Long2Dec2(_l: UInt32): string;
begin
  Result := Long2DecN(_l, 2);
end;

function Long2Dec4(_l: UInt32): string;
begin
  Result := Long2DecN(_l, 4);
end;

function Long2DecN(_l: UInt32; _n: UInt32): string;
begin
  Result := Long2Dec(_l);
  if UInt32(Length(Result)) < _n then
    Insert(DupeString('0', _n - UInt32(Length(Result))), Result, 1);
end;

function TimeToSeconds(_Zeit: TDateTime): Integer;
begin
  Result := SecondOfTheDay(_Zeit);
end;

function SecondsToTimeStr(_Seconds: Integer): string;
begin
  if _Seconds < 0 then begin
    Result := '-';
    _Seconds := -_Seconds;
  end else
    Result := '';
  Result := Result + Format('%.2d:%.2d:%.2d', [_Seconds div 3600, (_Seconds div 60) mod 60, _Seconds mod 60]);
end;

function SecondsToStr(_Seconds: Integer): string;
begin
  Result := SecondsToTimeStr(_Seconds);
end;

function TimeStrToSeconds(const _Zeit: string): Integer;
var
  Zeit: string;
  s: string;
  Len: Integer;
  Sign: Integer;
begin
  Len := Length(_Zeit);
  if Len = 0 then begin
    Result := 0;
    Exit;
  end;

  Sign := 1;
  case _Zeit[1] of
    '-': begin
        Zeit := TailStr(_Zeit, 2);
        Sign := -1;
      end;
    '+', ' ':
      Zeit := TailStr(_Zeit, 2);
  else
    Zeit := _Zeit;
  end;

  s := ExtractFirstWord(Zeit, [':']);
  if s = '' then
    Result := 0
  else
    Result := StrToInt(s);

  s := ExtractFirstWord(Zeit, [':']);
  if s <> '' then
    Result := Result * 60 + StrToInt(s);

  s := ExtractFirstWord(Zeit, [':']);
  if s <> '' then
    Result := Result * 60 + StrToInt(s);

  Result := Result * Sign;
end;

{$IF Declared(FormatSettings)}
function DecimalSeparator: Char; inline;
begin
  Result := FormatSettings.DecimalSeparator;
end;
{$IFEND}

function Float2Str(_flt: Extended; _DecSeparator: Char = '.'): string;
var
{$IF Declared(TFormatSettings)}
  FmtSettings: TFormatSettings;
{$ELSE}
  SysDecimalSeparator: Char;
{$IFEND}
begin
  if _DecSeparator = #0 then
    _DecSeparator := DecimalSeparator;
{$IF Declared(TFormatSettings)}
  FmtSettings := DZ_FORMAT_DECIMAL_POINT;
  FmtSettings.DecimalSeparator := _DecSeparator;
  Result := FloatToStr(_flt, FmtSettings);
{$ELSE}
  SysDecimalSeparator := DecimalSeparator;
  try
    SysUtils.DecimalSeparator := _DecSeparator;
    Result := FloatToStr(_flt);
  finally
    SysUtils.DecimalSeparator := SysDecimalSeparator;
  end;
{$IFEND}
end;

function Float2Str(_flt: Extended; _Width, _Decimals: Integer; _DecSeparator: Char): string;
var
  s: AnsiString;
begin
  Str(_flt: _Width: _Decimals, s);
  Result := string(s);
  if _DecSeparator = #0 then
    _DecSeparator := DecimalSeparator;
  if _DecSeparator <> '.' then
    Result := ReplaceStr(Result, '.', _DecSeparator);
end;

function Float2Str(_flt: Extended; _Decimals: Integer; _DecSeparator: Char): string;
begin
  Result := Float2Str(_flt, 0, _Decimals, _DecSeparator);
end;

function TryRound(_flt: Extended; out _wrd: Word): Boolean;
begin
  Result := (_flt >= 0) and (_flt <= $FFFF);
  if Result then
    try
      _wrd := Round(_flt);
    except
      Result := False;
    end;
end;

function Str2Int(const _s: string; _Default: Integer): Integer;
var
  e: Integer;
begin
  Val(_s, Result, e);
  if e <> 0 then
    Result := _Default
end;

function Str2Int(const _s: string; const _Source: string): Integer;
var
  e: Integer;
begin
  Val(_s, Result, e);
  if e <> 0 then
    raise EStringConvertError.CreateFmt(_('"%s" is not a valid integer value: %s'), [_s, _Source]);
end;

function TryStr2Int(const _s: string; var _Value: Integer): Boolean;
var
  e: Integer;
  v: Integer;
begin
  Val(_s, v, e);
  Result := (e = 0);
  if Result then
    _Value := v;
end;

{$IFDEF SUPPORTS_UNICODE}
function TryStr2Int(const _s: AnsiString; var _Value: Integer): Boolean;
begin
  Result := TryStr2Int(string(_s), _Value);
end;
{$ENDIF SUPPORTS_UNICODE}

function Str2Int64(const _s: string; _Default: Int64): Int64;
var
  e: Integer;
begin
  Val(_s, Result, e);
  if e <> 0 then
    Result := _Default
end;

function Str2Int64(const _s: string; const _Source: string): Int64;
var
  e: Integer;
begin
  Val(_s, Result, e);
  if e <> 0 then
    raise EStringConvertError.CreateFmt(_('"%s" is not a valid Int64 value: %s'), [_s, _Source]);
end;

function GuessDecimalSeparator(const _s: string): Char;
var
  i: Integer;
  //  DotCnt: integer;
  CommaCnt: Integer;
begin
  //  DotCnt := 0;
  CommaCnt := 0;
  Result := '.';
  for i := 1 to Length(_s) do begin
    case _s[i] of
      '.': begin
            //            Inc(DotCnt);
          Result := '.';
        end;
      ',': begin
          Inc(CommaCnt);
          Result := ',';
        end;
    end;
  end;
  if (Result = ',') and (CommaCnt = 1) then
    Exit;
  Result := '.';
end;

//{$IFNDEF Win64}
function TryStr2Float(const _s: string; out _flt: Extended; _DecSeparator: Char = '.'): Boolean;
var
{$IF Declared(TFormatSettings)}
  FmtSettings: TFormatSettings;
{$ELSE}
  SysDecimalSeparator: Char;
{$IFEND}
begin
  if _DecSeparator = #0 then
    _DecSeparator := GuessDecimalSeparator(_s);
{$IF Declared(TFormatSettings)}
  FmtSettings := DZ_FORMAT_DECIMAL_POINT;
  FmtSettings.DecimalSeparator := _DecSeparator;
  Result := TextToFloat(PChar(_s), _flt, fvExtended, FmtSettings);
{$ELSE}
  SysDecimalSeparator := DecimalSeparator;
  try
    SysUtils.DecimalSeparator := _DecSeparator;
    Result := TextToFloat(PChar(_s), _flt, fvExtended);
  finally
    SysUtils.DecimalSeparator := SysDecimalSeparator;
  end;
{$IFEND}
end;
//{$ENDIF}

function TryStr2Float(const _s: string; out _flt: Double; _DecSeparator: Char = '.'): Boolean;
var
  flt: Double;
begin
  Result := TryStr2Float(_s, flt, _DecSeparator);
  if Result then
    _flt := flt;
end;

function TryStr2Float(const _s: string; out _flt: Single; _DecSeparator: Char = '.'): Boolean;
var
  flt: Single;
begin
  Result := TryStr2Float(_s, flt, _DecSeparator);
  if Result then
    _flt := flt;
end;

function Str2Float(const _s: string; _Default: Extended; _DecSeparator: Char = '.'): Extended;
begin
  if not TryStr2Float(_s, Result, _DecSeparator) then
    Result := _Default
end;

function Str2Float(const _s: string; const _Source: string; _DecSeparator: Char = '.'): Extended;
begin
  if not TryStr2Float(_s, Result, _DecSeparator) then
    raise EStringConvertError.CreateFmt(_('"%s" is not a valid floating point value: %s'), [_s, _Source]);
end;

function FileSizeToHumanReadableString(_FileSize: Int64): string;
begin
  if _FileSize > 5 * OneExbiByte then
    Result := Format(_('%.2f EiB'), [_FileSize / OneExbiByte])
  else if _FileSize > 5 * OnePebiByte then
    Result := Format(_('%.2f PiB'), [_FileSize / OnePebiByte])
  else if _FileSize > 5 * OneTebiByte then
    Result := Format(_('%.2f TiB'), [_FileSize / OneTebiByte])
  else if _FileSize > 5 * OneGibiByte then
    Result := Format(_('%.2f GiB'), [_FileSize / OneGibiByte])
  else if _FileSize > 5 * OneMebiByte then
    Result := Format(_('%.2f MiB'), [_FileSize / OneMebiByte])
  else if _FileSize > 5 * OneKibiByte then
    Result := Format(_('%.2f KiB'), [_FileSize / OneKibiByte])
  else
    Result := Format(_('%d Bytes'), [_FileSize]);
end;

function SecondsToHumanReadableString(_Seconds: Int64): string;
begin
  if _Seconds > SecondsPerDay then
    // Days and hours, ignore minutes and seconds
    Result := Format(_('%dd %dh'), [_Seconds div SecondsPerDay, (_Seconds div SecondsPerHour) mod HoursPerDay])
  else if _Seconds > Round(1.5 * SecondsPerHour) then
    // Hours and minutes, ignore seconds
    Result := Format(_('%dh %dm'), [_Seconds div SecondsPerHour, (_Seconds div SecondsPerMinute) mod MinutesPerHour])
  else if _Seconds > Round(1.5 * SecondsPerMinute) then
    // Minutes and seconds
    Result := Format(_('%dm %ds'), [_Seconds div SecondsPerMinute, _Seconds mod SecondsPerMinute])
  else
    // Seconds only
    Result := Format(_('%ds'), [_Seconds]);
end;

function SecondsToHumanReadableString(const _Seconds: Extended): string;
begin
  Result := SecondsToHumanReadableString(Round(_Seconds));
end;

{$IF Declared(TFormatSettings)}

function GetSystemDefaultLocaleSettings: TFormatSettings;
begin
  Result := u_dzStringUtils.GetSystemDefaultLocaleSettings;
end;

function GetUserDefaultLocaleSettings: TFormatSettings;
begin
  Result := u_dzStringUtils.GetUserDefaultLocaleSettings;
end;
{$IFEND}

function LongWord2ByteArr(_Value: LongWord; _MsbFirst: Boolean = False): TBytes;
begin
  SetLength(Result, SizeOf(_Value));
  if _MsbFirst then begin
    Result[0] := _Value shr 24 and $FF;
    Result[1] := _Value shr 16 and $FF;
    Result[2] := _Value shr 8 and $FF;
    Result[3] := _Value shr 0 and $FF;
  end else begin
    Result[3] := _Value shr 24 and $FF;
    Result[2] := _Value shr 16 and $FF;
    Result[1] := _Value shr 8 and $FF;
    Result[0] := _Value shr 0 and $FF;
  end;
end;

function ByteArr2LongWord(const _Arr: array of Byte; _MsbFirst: Boolean = False): LongWord;
begin
  if Length(_Arr) <> SizeOf(Result) then
    raise Exception.CreateFmt(_('Length of byte array (%d) does not match size of a LongWord (%d)'), [Length(_Arr), SizeOf(Result)]);
  if _MsbFirst then begin
    Result := _Arr[0] shl 24 + _Arr[1] shl 16 + _Arr[2] shl 8 + _Arr[3];
  end else begin
    Result := _Arr[3] shl 24 + _Arr[2] shl 16 + _Arr[1] shl 8 + _Arr[0];
  end;
end;

function Swap16(_Value: Word): Word;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := swap(_Value);
end;
// alternative implementation based on https://stackoverflow.com/a/3065619/49925
//function Swap16(Value: smallint): smallint; register;
//asm
//  rol   ax, 8
//end;

function Swap32(_Value: LongWord): LongWord;
asm
  bswap eax
end;

function Swap32pas(_Value: LongWord): LongWord;
begin
  Result := ((_Value shr 24) and $FF) + (((_Value shr 16) and $FF) shl 8) + (((_Value shr 8) and $FF) shl 16) + ((_Value and $FF) shl 24);
end;

function Swap64(_Value: UInt64): UInt64;
asm
  MOV     EDX,_Value.Int64Rec.Lo
  BSWAP   EDX
  MOV     EAX,_Value.Int64Rec.Hi
  BSWAP   EAX
end;

function BitReverse32(v: LongWord): LongWord;
// source (C code):
// https://apps.topcoder.com/forums/?module=Thread&threadID=514884&start=2
begin
  // swap odd and even bits
  v := ((v shr 1) and $55555555) or ((v and $55555555) shl 1);
  // swap consecutive pairs
  v := ((v shr 2) and $33333333) or ((v and $33333333) shl 2);
  // swap nibbles ...
  v := ((v shr 4) and $0F0F0F0F) or ((v and $0F0F0F0F) shl 4);
  // swap bytes
  v := ((v shr 8) and $00FF00FF) or ((v and $00FF00FF) shl 8);
  // swap 2-byte long pairs
  Result := (v shr 16) or (v shl 16);
end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}

{ TBits64 }

class function TBits64.Create(_Value: TValue): TBits64;
begin
  Result.Init(_Value);
end;

class function TBits64.AllSet: TBits64;
begin
  Result.Init($FFFFFFFFFFFFFFFF);
end;

class function TBits64.NoneSet: TBits64;
begin
  Result.Init(0);
end;

procedure TBits64.Init(_Value: TValue);
begin
  FValue := _Value;
end;

type
  TLoHi64 = packed record
    Lo: UInt32;
    Hi: UInt32;
  end;

function TBits64.IsBitSet(_BitNo: TBitNumber): Boolean;
var
  LoHi: TLoHi64 absolute FValue;
begin
  // shl only supports 32 bits
  if _BitNo > 31 then begin
    Result := ((LoHi.Hi and (1 shl (_BitNo - 32))) <> 0);
  end else begin
    Result := ((LoHi.Lo and (1 shl _BitNo)) <> 0);
  end;
end;

procedure TBits64.SetBit(_BitNo: TBitNumber; _BitValue: Boolean);
var
  LoHi: TLoHi64 absolute FValue;
begin
  // shl only supports 32 bits
  if _BitNo > 31 then begin
    if _BitValue then
      LoHi.Hi := LoHi.Hi or (1 shl (_BitNo - 32))
    else
      LoHi.Hi := LoHi.Hi and not (1 shl (_BitNo - 32));
  end else begin
    if _BitValue then
      LoHi.Lo := LoHi.Lo or (1 shl _BitNo)
    else
      LoHi.Lo := LoHi.Lo and not (1 shl _BitNo);
  end;
end;

function TBits64.Extract(_BitFirst, _BitLast: TBitNumber): TValue;
var
  i: TBitNumber;
begin
  Result := 0;
  for i := _BitLast downto _BitFirst do begin
    Result := Result shl 1;
    if IsBitSet(i) then
      Result := Result + 1;
  end;
end;

procedure TBits64.Overwrite(_BitFirst, _BitLast: TBitNumber; _Value: TValue);
var
  i: TBitNumber;
begin
  for i := _BitFirst to _BitLast do begin
    SetBit(i, (_Value and TValue(1)) <> 0);
    _Value := _Value shr 1;
  end;
end;

function TBits64.Value: TValue;
begin
  Result := FValue;
end;

type
  TByteArr8 = array[TByteNumber64] of Byte;

function TBits64.GetByte(_ByteNo: TByteNumber): Byte;
var
  Bytes: TByteArr8 absolute FValue;
begin
  Result := Bytes[_byteNo];
end;

procedure TBits64.SetByte(_ByteNo: TByteNumber; _Value: Byte);
var
  Bytes: TByteArr8 absolute FValue;
begin
  Bytes[_ByteNo] := _Value;
end;

function TBits64.AsString: string;
var
  i: Integer;
begin
  Result := DupeString('0', Bits);
  for i := High downto Low do
    if IsBitSet(i) then
      Result[Bits - i] := '1';
end;

class operator TBits64.BitwiseAnd(_a, _b: TBits64): TBits64;
begin
  Result.Init(_a.Value and _b.Value);
end;

class operator TBits64.LogicalNot(_a: TBits64): TBits64;
begin
  Result.Init(_a.Value xor $FF);
end;

class operator TBits64.BitwiseOr(_a, _b: TBits64): TBits64;
begin
  Result.Init(_a.Value or _b.Value);
end;

class operator TBits64.BitwiseXor(_a, _b: TBits64): TBits64;
begin
  Result.Init(_a.Value xor _b.Value);
end;

class operator TBits64.Equal(_a, _b: TBits64): Boolean;
begin
  Result := _a.Value = _b.Value;
end;

{ TBits32 }

class function TBits32.AllSet: TBits32;
begin
  Result.Init($FFFFFFFF);
end;

function TBits32.AsString: string;
var
  i: Integer;
begin
  Result := DupeString('0', Bits);
  for i := High downto Low do
    if IsBitSet(i) then
      Result[Bits - i] := '1';
end;

class function TBits32.Create(_Value: TValue): TBits32;
begin
  Result.Init(_Value);
end;

function TBits32.Extract(_BitFirst, _BitLast: TBitNumber): TValue;
var
  i: TBitNumber;
begin
  Result := 0;
  for i := _BitLast downto _BitFirst do begin
    Result := Result shl 1;
    if IsBitSet(i) then
      Result := Result + 1;
  end;
end;

class function TBits32.NoneSet: TBits32;
begin
  Result.Init(0);
end;

procedure TBits32.Overwrite(_BitFirst, _BitLast: TBitNumber; _Value: TValue);
var
  i: TBitNumber;
begin
  for i := _BitFirst to _BitLast do begin
    SetBit(i, (_Value and $00000001) <> 0);
    _Value := _Value shr 1;
  end;
end;

function TBits32.GetByte(_ByteNo: TByteNumber): Byte;
begin
  Result := (FValue shr (_ByteNo * 8)) and $FF;
end;

procedure TBits32.SetByte(_ByteNo: TByteNumber; _Value: Byte);
begin
  _ByteNo := _ByteNo * 8;
  FValue := FValue and ($FFFFFFFF xor ($FF shl _ByteNo)) or (_Value shl _ByteNo);
end;

procedure TBits32.Init(_Value: TValue);
begin
  FValue := _Value;
end;

function TBits32.IsBitSet(_BitNo: TBitNumber): Boolean;
begin
  Result := ((FValue and (1 shl _BitNo)) <> 0);
end;

procedure TBits32.SetBit(_BitNo: TBitNumber; _BitValue: Boolean);
begin
  if _BitValue then
    FValue := FValue or (1 shl _BitNo)
  else
    FValue := FValue and not (1 shl _BitNo);
end;

function TBits32.Value: TValue;
begin
  Result := FValue;
end;

class operator TBits32.BitwiseAnd(_a, _b: TBits32): TBits32;
begin
  Result.Init(_a.Value and _b.Value);
end;

class operator TBits32.LogicalNot(_a: TBits32): TBits32;
begin
  Result.Init(_a.Value xor $FF);
end;

class operator TBits32.BitwiseOr(_a, _b: TBits32): TBits32;
begin
  Result.Init(_a.Value or _b.Value);
end;

class operator TBits32.BitwiseXor(_a, _b: TBits32): TBits32;
begin
  Result.Init(_a.Value xor _b.Value);
end;

class operator TBits32.Equal(_a, _b: TBits32): Boolean;
begin
  Result := _a.Value = _b.Value;
end;

{ TBits16 }

class function TBits16.AllSet: TBits16;
begin
  Result.Init($FFFF);
end;

function TBits16.AsString: string;
var
  i: Integer;
begin
  Result := DupeString('0', Bits);
  for i := High downto Low do
    if IsBitSet(i) then
      Result[Bits - i] := '1';
end;

class function TBits16.Create(_Value: TValue): TBits16;
begin
  Result.Init(_Value);
end;

procedure TBits16.Init(_Value: TValue);
begin
  FValue := _Value;
end;

function TBits16.IsAnyBitSet: Boolean;
begin
  Result := FValue <> 0;
end;

function TBits16.IsBitSet(_BitNo: TBitNumber): Boolean;
begin
  Result := ((FValue and (1 shl _BitNo)) <> 0);
end;

class operator TBits16.BitwiseAnd(_a, _b: TBits16): TBits16;
begin
  Result.Init(_a.Value and _b.Value);
end;

class operator TBits16.LogicalNot(_a: TBits16): TBits16;
begin
  Result.Init(_a.Value xor $FF);
end;

class operator TBits16.BitwiseOr(_a, _b: TBits16): TBits16;
begin
  Result.Init(_a.Value or _b.Value);
end;

class operator TBits16.BitwiseXor(_a, _b: TBits16): TBits16;
begin
  Result.Init(_a.Value xor _b.Value);
end;

function TBits16.Extract(_BitFirst, _BitLast: TBitNumber): TValue;
var
  i: TBitNumber;
begin
  Result := 0;
  for i := _BitLast downto _BitFirst do begin
    Result := Result shl 1;
    if IsBitSet(i) then
      Result := Result + 1;
  end;
end;

class function TBits16.NoneSet: TBits16;
begin
  Result.Init(0);
end;

procedure TBits16.Overwrite(_BitFirst, _BitLast: TBitNumber; _Value: TValue);
var
  i: TBitNumber;
begin
  for i := _BitFirst to _BitLast do begin
    SetBit(i, (_Value and TValue(1)) <> 0);
    _Value := _Value shr 1;
  end;
end;

procedure TBits16.SetBit(_BitNo: TBitNumber; _BitValue: Boolean);
begin
  if _BitValue then
    FValue := FValue or (1 shl _BitNo)
  else
    FValue := FValue and not (1 shl _BitNo);
end;

function TBits16.Value: TValue;
begin
  Result := FValue;
end;

function TBits16.GetByte(_ByteNo: TByteNumber): Byte;
begin
  Result := (FValue shr (_ByteNo * 8)) and $FF;
end;

procedure TBits16.SetByte(_ByteNo: TByteNumber; _Value: Byte);
begin
  _ByteNo := _ByteNo * 8;
  FValue := FValue and ($FFFF xor ($FF shl _ByteNo)) or (_Value shl _ByteNo);
end;

class operator TBits16.Equal(_a, _b: TBits16): Boolean;
begin
  Result := _a.Value = _b.Value;
end;

{ TBits8 }

function TBits8.AsString: string;
var
  i: Integer;
begin
  Result := DupeString('0', Bits);
  for i := High downto Low do
    if IsBitSet(i) then
      Result[Bits - i] := '1';
end;

class function TBits8.AllSet: TBits8;
begin
  Result.Init($FF);
end;

class function TBits8.Create(_Value: TValue): TBits8;
begin
  Result.Init(_Value);
end;

function TBits8.GetByte(_ByteNo: TByteNumber): Byte;
begin
  Result := FValue;
end;

procedure TBits8.SetByte(_ByteNo: TByteNumber; _Value: Byte);
begin
  FValue := _Value;
end;

procedure TBits8.Init(_Value: TValue);
begin
  FValue := _Value;
end;

function TBits8.IsAnyBitSet: Boolean;
begin
  Result := FValue <> 0;
end;

function TBits8.IsBitSet(_BitNo: TBitNumber): Boolean;
begin
  Result := ((FValue and (1 shl _BitNo)) <> 0);
end;

class operator TBits8.BitwiseAnd(_a, _b: TBits8): TBits8;
begin
  Result.Init(_a.Value and _b.Value);
end;

class operator TBits8.LogicalNot(_a: TBits8): TBits8;
begin
  Result.Init(_a.Value xor $FF);
end;

class operator TBits8.BitwiseOr(_a, _b: TBits8): TBits8;
begin
  Result.Init(_a.Value or _b.Value);
end;

class operator TBits8.BitwiseXor(_a, _b: TBits8): TBits8;
begin
  Result.Init(_a.Value xor _b.Value);
end;

class function TBits8.NoneSet: TBits8;
begin
  Result.Init(0);
end;

procedure TBits8.SetBit(_BitNo: TBitNumber; _BitValue: Boolean);
begin
  if _BitValue then
    FValue := FValue or (1 shl _BitNo)
  else
    FValue := FValue and not (1 shl _BitNo);
end;

function TBits8.Extract(_BitFirst, _BitLast: TBitNumber): TValue;
var
  i: Integer;
begin
  Result := 0;
  for i := _BitLast downto _BitFirst do begin
    Result := Result shl 1;
    if IsBitSet(i) then
      Result := Result + 1;
  end;
end;

procedure TBits8.Overwrite(_BitFirst, _BitLast: TBitNumber; _Value: TValue);
var
  i: TBitNumber;
begin
  for i := _BitFirst to _BitLast do begin
    SetBit(i, (_Value and TValue(1)) <> 0);
    _Value := _Value shr 1;
  end;
end;

function TBits8.Value: TValue;
begin
  Result := FValue;
end;

class operator TBits8.Equal(_a, _b: TBits8): Boolean;
begin
  Result := _a.Value = _b.Value;
end;

{$ENDIF}

function Bool2Str(_b: Boolean): string;
begin
  if _b then
    Result := 'True' // do not translate
  else
    Result := 'False'; // do not translate
end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
{ TBoolToStr }

class function TBoolToStr.Create(const _TrueStr, _FalseStr: string): TBoolToStr;
begin
  Result.FBoolStrings[True] := _TrueStr;
  Result.FBoolStrings[False] := _FalseStr;
end;

class function TBoolToStr.CreateTrueFalse: TBoolToStr;
begin
  Result := Create('True', 'False');
end;

class function TBoolToStr.CreateTrueFalseLocalized: TBoolToStr;
begin
  Result := Create(_('True'), _('False'));
end;

class function TBoolToStr.CreateYesNo: TBoolToStr;
begin
  Result := Create('Yes', 'No');
end;

class function TBoolToStr.CreateYesNoLocalized: TBoolToStr;
begin
  Result := Create(_('Yes'), _('No'));
end;

class function TBoolToStr.CreateYN: TBoolToStr;
begin
  Result := Create('Y', 'N');
end;
{$ENDIF}

function GetLocalizedOneLetterYesStr: string;
begin
  // todo: This should really use PGetText, e.g. PGetText('Yes/No one letter', 'Y')
  // Translator: Convert to the equivalent of 'Y' (only one letter)
  Result := _('Y(es)');
  // if it wasn't translated we use English 'Y'
  if Result = 'Y(es)' then
    Result := 'Y';
end;

function GetLocalizedOneLetterNoStr: string;
begin
  // todo: This should really use PGetText, e.g. PGetText('Yes/No one letter', 'N')
  // Translator: Convert to the equivalent of 'N' (only one letter)
  Result := _('N(o)');
  // if it wasn't translated we use English 'N'
  if Result = 'N(o)' then
    Result := 'N';
end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}

class function TBoolToStr.CreateYNLocalized: TBoolToStr;
begin
  Result := Create(GetLocalizedOneLetterYesStr, GetLocalizedOneLetterNoStr);
end;

function TBoolToStr.ToString(_b: Boolean): string;
begin
  Result := FBoolStrings[_b];
end;
{$ENDIF}

function AssertYNStringLength: Boolean;
var
  YesStr: string;
  NoStr: string;
begin
  YesStr := GetLocalizedOneLetterYesStr;
  Assert(Length(YesStr) = 1, 'Translator error: One letter translation for "Y" ("' + YesStr + '") is not a single letter.');

  NoStr := GetLocalizedOneLetterNoStr;
  Assert(Length(NoStr) = 1, 'Translator error: One letter translation for "N" ("' + NoStr + '") is not a single letter.');

  Result := True;
end;

function AssertSwap32: Boolean;
begin
  Assert(Swap32($01234567) = Swap32pas($01234567));
  Assert(Swap32($76543210) = Swap32pas($76543210));
  Result := True;
end;

function AssertBitReverse32: Boolean;
begin
  Assert(BitReverse32($012345678) = $1E6A2C48);
  Result := True;
end;

initialization
{$IF Declared(TFormatSettings)}
  DZ_FORMAT_DECIMAL_POINT := u_dzStringUtils.GetUserDefaultLocaleSettings;
  DZ_FORMAT_DECIMAL_POINT.DecimalSeparator := '.';
  DZ_FORMAT_DECIMAL_POINT.ThousandSeparator := #0;
{$IFEND}

  Assert(AssertYNStringLength);
{$IFNDEF Win64}
  Assert(AssertSwap32);
  Assert(AssertBitReverse32);
{$ENDIF}
end.
