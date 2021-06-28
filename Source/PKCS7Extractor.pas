unit PKCS7Extractor;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$DEFINE PKCS7MANAGEBASE64}

{***************************************************************************}
{                                                                           }
{     PKCS#7 Extractor library for Delphi                                   }
{     Version 1.3.0.0 released June, 15th 2021                              }
{                                                                           }
{     Copyright (C) 2018 Delphi Club Italia                                 }
{                        http://www.delphiclubitalia.it                     }
{                                                                           }
{     Original authors:                                                     }
{         Christian Cristofori              github@christiancristofori.it   }
{         Giancarlo Oneglio                   giancarlo.oneglio@gmail.com   }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the GNU Lesser General Public License, Version 3;         }
{  you may not use this file except in compliance with the License.         }
{                                                                           }
{  This is free software: you can redistribute it and/or modify it under    }
{  the terms of the GNU Lesser General Public License as published by the   }
{  Free Software Foundation, either version 3 of the License, or (at your   }
{  option) any later version.                                               }
{                                                                           }
{  This is distributed in the hope that it will be useful, but WITHOUT      }
{  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or    }
{  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public      }
{  License for more details.                                                }
{                                                                           }
{  You should have received a copy of the GNU Lesser General Public         }
{  License along with this software.                                        }
{  If not, see <http://www.gnu.org/licenses/>.                              }
{                                                                           }
{***************************************************************************}

interface

uses
  Classes;

const
  LIBEAY32_LIBRARY = 'libeay32.dll';

function GetFolder: String;
procedure SetFolder(const Value: String);
function Load: Boolean;
function Loaded: Boolean;
procedure Unload;
function GetVersion: String;
function GetErrorCode: Cardinal;
function GetError: String; overload;
function GetError(const ErrorCode: Cardinal): String; overload;

type
  TVerifyStatus = (vsUnknown, vsFull, vsPartial);

  TSignatureMode = (smUnknown, smPKCS7, smCMS);

  TStreamCreateEvent = procedure(Sender: TObject; var AStream: TStream) of Object;

  TPKCS7Message = class
  private
    fContent: TStream;
    fOnStreamCreate: TStreamCreateEvent;
    fSignatureMode: TSignatureMode;
    fVerifyStatus: TVerifyStatus;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function LoadFromStream(Stream: TStream): Boolean;
    function LoadFromFile(const Filename: String): Boolean;

    function SaveToStream(Stream: TStream): Boolean;
    function SaveToFile(const Filename: String): Boolean;

    property OnStreamCreate: TStreamCreateEvent read fOnStreamCreate write fOnStreamCreate;
    property SignatureMode: TSignatureMode read fSignatureMode;
    property VerifyStatus: TVerifyStatus read fVerifyStatus;
  end;

// Tries to guess destination filename of a .*.p7m file
function GuessFilename(const Filename: String): String;

// Extracts data from PKCS#7 message stream to a stream.
function Extract(InStream, OutStream: TStream): Boolean; overload;

// Utility function: does the above operation from a file to another by
// specifing the filenames.
function Extract(const InFilename, OutFilename: String): Boolean; overload;

// Utility function: doest the above operation from a file to another by specifing
// the filename, this function tries to guess the destination filename by removing
// the final extension, if another extension is present it will do the job.
function Extract(const Filename: String): String; overload;

// Utility function to extract the content from a stream directly to a String.
function Extract(Stream: TStream; var S: String): Boolean; overload;

// Utility function to extract the content from a file directly to a String.
function ExtractToString(const Filename: String; var S: String): Boolean; overload;

// Gets the verification status for a stream.
function Verify(Stream: TStream): TVerifyStatus; overload;

// Gets the verification status for a file.
function Verify(const Filename: String): TVerifyStatus; overload;

// Gets signature mode for a stream.
function SignatureMode(Stream: TStream): TSignatureMode; overload;

// Gets signature mode for a file.
function SignatureMode(const Filename: String): TSignatureMode; overload;

implementation

uses
  Windows, SysUtils;

// ================================================== LIBEAY32.DLL MICRO WRAPPER

const
  READ_BUFFER_SIZE = $1000;
  
  BIO_CTRL_RESET = $01;
  BIO_CTRL_INFO = $03;
  BIO_CTRL_FLUSH = $0B;

  PKCS7_NOVERIFY = $20;

  PKCS7_OP_GET_DETACHED_SIGNATURE = 2;

  CMS_NOVERIFY = $20;

  NID_PKCS7_SIGNED = 22;

  LIBEAY_OK = 1;

type
  PBIO_METHOD = Pointer;

  PSTACK = Pointer;

  CRYPTO_EX_DATA = record
    sk: PSTACK;
    dummy: Integer
  end;

  PPBIO = ^PBIO;
  PBIO = ^BIO;
  BIO = record
    method: PBIO_METHOD;
    callback: Pointer;
    cb_arg: PAnsiChar;
    init,
    shutdown,
    flags,
    retry_reasonm,
    num: Integer;
    ptr: Pointer;
    next_bio,
    prev_bio: PBIO;
    references: Integer;
    num_read,
    num_write: LongWord;
    ex_data: CRYPTO_EX_DATA
  end;

  PASN1_OBJECT = ^ASN1_OBJECT;
  ASN1_OBJECT = record
    sn,
    ln: PAnsiChar;
    nid,
    length: Integer;
    data: PAnsiChar;
    flags: Integer
  end;

  PKCS7_union = record
    ptr: PAnsiChar
  end;

  PCMS_ContentInfo = ^CMS_ContentInfo;
  CMS_ContentInfo = record
    contentType: PAnsiChar;
    d: Pointer
  end;

  PPKCS7 = ^PKCS7;
  PKCS7 = record
    asn1: PAnsiChar;
    length: LongInt;
    state,
    detached: Integer;
    _type: PASN1_OBJECT;
    d: PKCS7_union
  end;

  PX509_STORE = Pointer;

  PPPKCS7 = ^PPKCS7;

  PPEM_PASSWORD_CB = Pointer;

  PSTACK_OF_X509 = Pointer;

const
  // Total number of functions in the TLibeay32Functions structure counting optionals too.
  LIBEAY32_FUNCTIONS_COUNT = 23;
  // Number of optionals functions.
  LIBEAY32_OPTIONAL_COUNT  = 4;

type
  TLibeay32Functions  = packed record
    case Integer of
      0: (
    {00}    sk_num                    : function(const x: PSTACK): Integer cdecl; // OpenSSL 0.9.6

    {01}    BIO_s_mem                 : function: PBIO_METHOD cdecl; // OpenSSL pre-0.9.6
    {02}    BIO_new                   : function(t: PBIO_METHOD): PBIO cdecl; // OpenSSL pre-0.9.6
    {03}    BIO_new_mem_buf           : function(buf: Pointer; len: Integer): PBIO cdecl; // introduced OpenSSL 0.9.6
    {04}    BIO_ctrl                  : function(b: PBIO; c: Integer; l: LongInt; a: Pointer): LongInt cdecl; // OpenSSL pre-0.9.6
    {05}    BIO_read                  : function(b: PBIO; d: Pointer; l: Integer): Integer cdecl; // OpenSSL pre-0.9.6
    {06}    BIO_write                 : function(b: pBIO; const p: Pointer; l: Integer): Integer cdecl; // OpenSSL pre-0.9.6
    {07}    BIO_free                  : function(b: PBIO): Integer cdecl; // OpenSSL pre-0.9.6

    {08}    OBJ_obj2nid               : function(const o: PASN1_OBJECT): Integer cdecl; // OpenSSL pre-0.9.6

    {09}    d2i_PKCS7_bio             : function(b: PBIO; p: PPKCS7): PPKCS7 cdecl; // OpenSSL pre-0.9.6

    {10}    PEM_read_bio_PKCS7        : function(b: PBIO; x: PPPKCS7; c: PPEM_PASSWORD_CB; u: Pointer): PPKCS7 cdecl; // OpenSSL pre-0.9.6
    {11}    SMIME_read_PKCS7          : function(b: PBIO; {var }c: PBIO): PPKCS7 cdecl; // OpenSSL pre-0.9.6

    {12}    PKCS7_ctrl                : function(p: PPKCS7; c, l: Integer; a: PAnsiChar): Integer cdecl; // OpenSSL pre-0.9.6
    {13}    PKCS7_get_signer_info     : function(p: PPKCS7): PSTACK cdecl; // OpenSSL pre-0.9.6
    {14}    PKCS7_dataInit            : function(p: PPKCS7; b: PBIO): PBIO cdecl; // OpenSSL pre-0.9.6
    {15}    PKCS7_free                : procedure(p: PPKCS7) cdecl; // OpenSSL pre-0.9.6

    {16}    PKCS7_verify              : function(p: PPKCS7; c: PSTACK_OF_X509; s: PX509_STORE; i, o: PBIO; f: Integer): Integer cdecl; // OpenSSL pre-0.9.6
        // TODO: see PKCS7_dataVerify

    {17}    X509_STORE_new            : function: PX509_STORE cdecl; // OpenSSL pre-0.9.6
    {18}    X509_STORE_free           : procedure(s: pX509_STORE) cdecl; // OpenSSL pre-0.9.6

    // FROM HERE FUNCTIONS ARE OPTIONALS, DON'T PLACE REQUIRED FUNCTIONS HERE!

    {19}    d2i_CMS_bio               : function(b: PBIO; p: PCMS_ContentInfo): PCMS_ContentInfo cdecl; // introduced OpenSSL 0.9.8h
    {20}    CMS_dataInit              : function(p: PCMS_ContentInfo; b: PBIO): PBIO cdecl; // introduced OpenSSL 0.9.8h
    {21}    CMS_verify                : function(p: PCMS_ContentInfo; c: PSTACK_OF_X509; s: PX509_STORE; i, o: PBIO; f: Integer): Integer cdecl; // introduced OpenSSL 0.9.8h
    {22}    CMS_ContentInfo_free      : procedure(P: PCMS_ContentInfo) cdecl; // introduced OpenSSL 0.9.8h

      );
      1: (Functions                   : Array[0..LIBEAY32_FUNCTIONS_COUNT - 1] of Pointer)
  end;

var
  mFolder: String = '';
  mFunctions: TLibeay32Functions;
  mErrorLoaded: Boolean = False;

function Load: Boolean;
var
  hLibeay32: HMODULE;
  OpenSSL_add_all_digests: procedure cdecl;
  ERR_load_crypto_strings: procedure cdecl;
begin
  // Loads library
  hLibeay32 := GetModuleHandle(LIBEAY32_LIBRARY);
  if hLibeay32 = 0 then
    hLibeay32 := LoadLibrary(PChar(mFolder + LIBEAY32_LIBRARY));
  if hLibeay32 = 0 then
    hLibeay32 := LoadLibrary(LIBEAY32_LIBRARY);

  // Gets functions pointers
  @mFunctions.sk_num := GetProcAddress(hLibeay32, 'sk_num');

  @mFunctions.BIO_s_mem := GetProcAddress(hLibeay32, 'BIO_s_mem');
  @mFunctions.BIO_new := GetProcAddress(hLibeay32, 'BIO_new');
  @mFunctions.BIO_new_mem_buf := GetProcAddress(hLibeay32, 'BIO_new_mem_buf');
  @mFunctions.BIO_ctrl := GetProcAddress(hLibeay32, 'BIO_ctrl');
  @mFunctions.BIO_read := GetProcAddress(hLibeay32, 'BIO_read');
  @mFunctions.BIO_write := GetProcAddress(hLibeay32, 'BIO_write');

  @mFunctions.BIO_free := GetProcAddress(hLibeay32, 'BIO_free');

  @mFunctions.OBJ_obj2nid := GetProcAddress(hLibeay32, 'OBJ_obj2nid');

  @mFunctions.X509_STORE_new := GetProcAddress(hLibeay32, 'X509_STORE_new');
  @mFunctions.X509_STORE_free := GetProcAddress(hLibeay32, 'X509_STORE_free');

  @mFunctions.d2i_PKCS7_bio := GetProcAddress(hLibeay32, 'd2i_PKCS7_bio');

  @mFunctions.PEM_read_bio_PKCS7 := GetProcAddress(hLibeay32, 'PEM_read_bio_PKCS7');
  @mFunctions.SMIME_read_PKCS7 := GetProcAddress(hLibeay32, 'SMIME_read_PKCS7');

  @mFunctions.PKCS7_ctrl := GetProcAddress(hLibeay32, 'PKCS7_ctrl');
  @mFunctions.PKCS7_get_signer_info := GetProcAddress(hLibeay32, 'PKCS7_get_signer_info');
  @mFunctions.PKCS7_dataInit := GetProcAddress(hLibeay32, 'PKCS7_dataInit');
  @mFunctions.PKCS7_free := GetProcAddress(hLibeay32, 'PKCS7_free');
  @mFunctions.PKCS7_verify := GetProcAddress(hLibeay32, 'PKCS7_verify');

  // Optionals functions
  @mFunctions.d2i_CMS_bio := GetProcAddress(hLibeay32, 'd2i_CMS_bio');
  @mFunctions.CMS_dataInit := GetProcAddress(hLibeay32, 'CMS_dataInit');
  @mFunctions.CMS_verify := GetProcAddress(hLibeay32, 'CMS_verify');
  @mFunctions.CMS_ContentInfo_free := GetProcAddress(hLibeay32, 'CMS_ContentInfo_free');

  Result := Loaded;

  // Need to initialize the library.
  if Result then begin
    @OpenSSL_add_all_digests := GetProcAddress(hLibeay32, 'OPENSSL_add_all_algorithms_noconf'); // introduced OpenSSL 0.9.7
    if @OpenSSL_add_all_digests = nil then
      @OpenSSL_add_all_digests := GetProcAddress(hLibeay32, 'OpenSSL_add_all_digests'); // OpenSSL pre-0.9.6
    if @OpenSSL_add_all_digests <> nil then
      OpenSSL_add_all_digests
    else
      Result := False
  end;

  if Result then begin
    @ERR_load_crypto_strings := GetProcAddress(hLibeay32, 'ERR_load_crypto_strings');
    if @ERR_load_crypto_strings <> nil then begin
      ERR_load_crypto_strings;
      mErrorLoaded := True
    end
  end;

  if not Result then
    Unload
end;

procedure SetFolder(const Value: String);
begin
  mFolder := IncludeTrailingPathDelimiter(Value)
end;

procedure Unload;
begin
  mErrorLoaded := False;
  FillChar(mFunctions, SizeOf(TLibeay32Functions), 0)
end;

function GetVersion: String;
var
  SSLeay_version: function(t: Integer): PAnsiChar cdecl;
begin
  Result := '';
  @SSLeay_version := GetProcAddress(GetModuleHandle(LIBEAY32_LIBRARY), 'SSLeay_version'); // OpenSSL pre-0.9.6
  if @SSLeay_version <> nil then
    Result := String(SSLeay_version(0))
end;

function GetErrorCode: Cardinal;
var
  ERR_get_error: function: Cardinal cdecl;
begin
  Result := 0;
  @ERR_get_error := GetProcAddress(GetModuleHandle(LIBEAY32_LIBRARY), 'ERR_get_error'); // OpenSSL pre-0.9.6
  if @ERR_get_error <> nil then
    Result := ERR_get_error
end;

function GetError: String;
begin
  Result := GetError(GetErrorCode)
end;

function GetError(const ErrorCode: Cardinal): String; overload;
var
  P: Array[0..1023] of AnsiChar;
  ERR_error_string_n: procedure(e: Cardinal; b: PAnsiChar; l: {$IFDEF WIN64}UInt64{$ELSE}Cardinal{$ENDIF}) cdecl;
  ERR_error_string: function(e: Cardinal; b: PAnsiChar): PAnsiChar cdecl;
begin
  Result := '';
  if mErrorLoaded and (ErrorCode > 0) then begin
    @ERR_error_string_n := GetProcAddress(GetModuleHandle(LIBEAY32_LIBRARY), 'ERR_error_string_n'); // introduced OpenSSL 0.9.6
    if @ERR_error_string_n <> nil then begin
      FillChar(P, SizeOf(P), 0);
      ERR_error_string_n(ErrorCode, P, SizeOf(P));
      Result := String(P)
    end else begin
      @ERR_error_string := GetProcAddress(GetModuleHandle(LIBEAY32_LIBRARY), 'ERR_error_string'); // OpenSSL pre-0.9.6
      if @ERR_error_string <> nil then
        Result := String(ERR_error_string(ErrorCode, nil))
    end
  end
end;

function GetFolder: String;
var
  hLibeay32: THandle;
  szFilename: Array[0..MAX_PATH] of Char;
begin
  Result := mFolder;
  hLibeay32 := GetModuleHandle(LIBEAY32_LIBRARY);
  if hLibeay32 > 0 then begin
    FillChar(szFilename, SizeOf(szFilename), 0);
    if GetModuleFileName(hLibeay32, szFilename, MAX_PATH) > 0 then
      Result := IncludeTrailingPathDelimiter(ExtractFilePath(szFilename))
  end
end;

function Loaded: Boolean;
var
  I: Integer;
begin
  Result := GetModuleHandle(LIBEAY32_LIBRARY) > 0;
  for I := 0 to LIBEAY32_FUNCTIONS_COUNT - LIBEAY32_OPTIONAL_COUNT - 1 do
    Result := Result and (mFunctions.Functions[I] <> nil)
end;

// ==================================================== BASE64 CHECK AND PREPARE

{$IFDEF PKCS7MANAGEBASE64}
const
  BASE64_BEGIN: AnsiString = '-----BEGIN PKCS7-----'#10; // DO NOT EDIT THIS! DO NOT CHANGE TYPE!
  BASE64_END: AnsiString = #10'-----END PKCS7-----';     // DO NOT EDIT THIS! DO NOT CHANGE TYPE!

function Base64Prepare(Data: Pointer; iSize, iMemory: Integer): Integer;
const
  BASE64_LOOKUP_TABLE: Array[Byte] of Byte = (
         {00} {01} {02} {03} {04} {05} {06} {07} {08} {09} {0A} {0B} {0C} {0D} {0E} {0F}
    {00} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $7F, $FF, $FF, $7F, $FF, $FF,
    {10} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    {20} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $3E, $FF, $3E, $FF, $3F,
    {30} $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $FF, $FF, $FF, $FF, $2B, $2F,
    {40} $FF, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E,
    {50} $0F, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $FF, $FF, $FF, $FF, $3F,
    {60} $FF, $1A, $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23, $24, $25, $26, $27, $28,
    {70} $29, $2A, $2B, $2C, $2D, $2E, $2F, $30, $31, $32, $33, $FF, $FF, $FF, $FF, $FF,
    {80} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    {90} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    {A0} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    {B0} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    {C0} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    {D0} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    {E0} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    {F0} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
var
  I, X, iData, iPadding, iSkip: Int64;
  OldResult: Integer;
  P: Pointer;
begin
  Result := iSize;
  // Counts how many Base64 characters are found, both data or padding (only at
  // the end of the block). CRs and LFs are considered valid but counted for
  // being skipped in the resulting block.
  iData := 0;
  iPadding := 0;
  iSkip := 0;
  for I := 0 to iSize - 1 do
    // This is a padding character
    if PByteArray(Data)^[I] = $3D then begin
      // No more than 2 padding characters are allowed at the end of the block.
      if iPadding < 2 then
        Inc(iPadding)
      else
        Exit
    end else
      // Checks for the corresponding Base64 value.
      case BASE64_LOOKUP_TABLE[PByteArray(Data)^[I]] of
        // Invalid character outside the Base64 alphabet.
        $FF: Exit;
        // CR or LF, will skip this.
        $7F: Inc(iSkip);
      else
        // If I didn't already start the final padding, this is to be considered good data.
        if iPadding = 0 then
          Inc(iData)
        else
          Exit
      end;
  // If data length (including padding) is greater that zero and 32-bit aligned, it's valid Base64 data!
  if ((iData + iPadding) mod 4 = 0) and ((iData + iPadding) > 0) then begin
    // If I've found some CRs and LFs, now let's remove them
    if iSkip > 0 then begin
      // Pre-calculate resulting buffer size.
      Result := iSize - iSkip;
      X := 0;
      // Allocate a temporary buffer.
      GetMem(P, Result);
      try
        for I := 0 to iSize - 1 do
          // Converting URL-safe
          if BASE64_LOOKUP_TABLE[PByteArray(Data)^[I]] = $3E then begin
            PByteArray(P)^[X] := $2B;
            Inc(X)
          end else
          if BASE64_LOOKUP_TABLE[PByteArray(Data)^[I]] = $3F then begin
            PByteArray(P)^[X] := $2F;
            Inc(X)
          end else
          // Copy every character to the temporary buffer skipping CRs and LFs.
          if BASE64_LOOKUP_TABLE[PByteArray(Data)^[I]] <> $7F then begin
            PByteArray(P)^[X] := PByteArray(Data)^[I];
            Inc(X)
          end;
        // Clear the destination buffer.
        FillChar(Data^, iMemory, 0);
        // Copy the temporary buffer to destination.
        Move(P^, Data^, Result)
      finally
        // Release memory used by the temporary buffer.
        FreeMem(P, Result)
      end
    end;
    // Make a copy of the buffer size.
    OldResult := Result;
    // Calculate the length of output buffer include heading and trailing tags.
    Result := Result + Length(BASE64_BEGIN) + Length(BASE64_END);
    // Allocate a temporary buffer.
    GetMem(P, Result);
    try
      // Compose final result:
      // - adds the heading tag;
      Move(BASE64_BEGIN[1], P^, Length(BASE64_BEGIN));
      // - adds the Base64 data;
      Move(Data^, PByteArray(P)^[Length(BASE64_BEGIN)], OldResult);
      // - adds the trailing tag.
      Move(BASE64_END[1], PByteArray(P)^[Length(BASE64_BEGIN) + OldResult], Length(BASE64_END));
      // Clearing the output buffer.
      FillChar(Data^, iMemory, 0);
      // Copying data from temporary buffer to output.
      Move(P^, Data^, Result)
    finally
      // Release
      FreeMem(P, Result)
    end
  end
end;
{$ENDIF}

// =========================================================== LIBRARY FUNCTIONS

function Extract(InStream, OutStream: TStream): Boolean;
begin
  Result := False;
  if Loaded or Load then
    with TPKCS7Message.Create do try
      Result := LoadFromStream(InStream) and SaveToStream(OutStream)
    finally
      Free
    end
end;

function Extract(const InFilename, OutFilename: String): Boolean;
begin
  Result := False;
  if Loaded or Load then
    with TPKCS7Message.Create do try
      Result := LoadFromFile(InFilename) and SaveToFile(OutFilename)
    finally
      Free
    end
end;

function Extract(const Filename: String): String;
begin
  Result := GuessFilename(Filename);
  if (Result = Filename) or (not Extract(Filename, Result)) then
    Result := ''
end;

function Extract(Stream: TStream; var S: String): Boolean;
var
  MStream: TMemoryStream;
  SStream: TStringStream;
{$IF CompilerVersion >= 20}
  Encoding: TEncoding;
  Buffer: TBytes;
  iSize: Integer;
{$IFEND}
begin
  S := '';
  Result := False;
  if Stream = nil then
    Exit;
  MStream := TMemoryStream.Create;
  try
    if Extract(Stream, MStream) then begin
{$IF CompilerVersion >= 20}
      iSize := MStream.Size;
      // That's long enough for preamble detection
      if iSize > 256 then
        iSize := 256;
      // Tries to detect correct encoding.
      SetLength(Buffer, iSize);
      try
        MStream.Position := 0;
        if MStream.Read(Buffer[0], iSize) = iSize then begin
          Encoding := nil;
          TEncoding.GetBufferEncoding(Buffer, Encoding)
        end else
          Encoding := TEncoding.Default
      finally
        SetLength(Buffer, 0)
      end;
{$IFEND}
      // Read this with detected encoding (Delphi 2009+).
      SStream := TStringStream.Create(''{$IF CompilerVersion >= 20}, Encoding{$IFEND});
      try
        Result := SStream.CopyFrom(MStream, 0) = MStream.Size;
        if Result then
          S := SStream.DataString
      finally
        SStream.Free
      end
    end
  finally
    MStream.Free
  end
end;

function ExtractToString(const Filename: String; var S: String): Boolean;
var
  InStream: TFileStream;
begin
  Result := False;
  if FileExists(Filename) then try
    InStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
    try
      InStream.Position := 0;
      Result := Extract(InStream, S)
    finally
      InStream.Free
    end
  except
    Result := False
  end
end;

function GuessFilename(const Filename: String): String;
var
  S: String;
begin
  Result := Filename;
  S := ChangeFileExt(Filename, '');
  if Length(ExtractFileExt(S)) > 0 then
    Result := S
end;

function Verify(Stream: TStream): TVerifyStatus;
var
  P: Int64;
begin
  Result := vsUnknown;
  if (Stream <> nil) and (Loaded or Load) then begin
    P := Stream.Position;
    try
      with TPKCS7Message.Create do try
        if LoadFromStream(Stream) then
          Result := VerifyStatus
      finally
        Free
      end
    finally
      Stream.Position := P
    end
  end
end;

function Verify(const Filename: String): TVerifyStatus;
begin
  Result := vsUnknown;
  if Loaded or Load then
    with TPKCS7Message.Create do try
      if LoadFromFile(Filename) then
        Result := VerifyStatus
    finally
      Free
    end
end;

function SignatureMode(Stream: TStream): TSignatureMode;
var
  P: Int64;
begin
  Result := smUnknown;
  if (Stream <> nil) and (Loaded or Load) then begin
    P := Stream.Position;
    try
      with TPKCS7Message.Create do try
        if LoadFromStream(Stream) then
          Result := SignatureMode
      finally
        Free
      end
    finally
      Stream.Position := P
    end
  end
end;

function SignatureMode(const Filename: String): TSignatureMode;
begin
  Result := smUnknown;
  if Loaded or Load then
    with TPKCS7Message.Create do try
      if LoadFromFile(Filename) then
        Result := SignatureMode
    finally
      Free
    end
end;

// ================================================================== MAIN CLASS

{ TPKCS7Message }

procedure TPKCS7Message.Clear;
begin
  if fContent <> nil then
    fContent.Free;
  fContent := nil;
  fSignatureMode := smUnknown;
  fVerifyStatus := vsUnknown
end;

constructor TPKCS7Message.Create;
begin
  inherited Create;
  fContent := nil;
  fOnStreamCreate := nil;
  Clear
end;

destructor TPKCS7Message.Destroy;
begin
  Clear;
  inherited Destroy
end;

function TPKCS7Message.LoadFromFile(const Filename: String): Boolean;
var
  FStream: TFileStream;
begin
  Result := False;
  if FileExists(Filename) then try
    FStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
    try
      FStream.Position := 0;
      Result := LoadFromStream(FStream)
    finally
      FStream.Free
    end
  except
    Result := False
  end
end;

function TPKCS7Message.LoadFromStream(Stream: TStream): Boolean;
var
  iRead: Integer;
  iTotalRead: Integer;
  iTotalMemory: Integer;
  iTotalWritten: Integer;
  iWritten: Integer;
  pCMS: PCMS_ContentInfo;
  pMemory: Pointer;
  pOutputBIO: PBIO;
  pPKCS: PPKCS7;
  pTempBIO: PBIO;
  pTempStack: PSTACK;
  pTempStore: PX509_STORE;
begin
  // Default return value
  Result := False;
  // Clears class instance variables
  Clear;
  // Parameters check
  if (Stream = nil) or (not Loaded) then
    Exit;

  // Verify data length
  iTotalRead := Stream.Size - Stream.Position;
  if iTotalRead <= 0 then
    Exit;

  pCMS := nil;
  pPKCS := nil;
  try
    {$IFDEF PKCS7MANAGEBASE64}
    // Allocate enough memory for Base64 manipulation
    iTotalMemory := iTotalRead + Length(BASE64_BEGIN) + Length(BASE64_END);
    {$ELSE}
    iTotalMemory := iTotalRead;
    {$ENDIF}
    GetMem(pMemory, iTotalMemory);
    try
      // Read from stream
      if Stream.Read(pMemory^, iTotalRead) = iTotalRead then begin

        {$IFDEF PKCS7MANAGEBASE64}
        // Check if it's Base64 data and manage it.
        iTotalRead := Base64Prepare(pMemory, iTotalRead, iTotalMemory);
        {$ENDIF}

        // Create BIO from this memory
        pTempBIO := mFunctions.BIO_new_mem_buf(pMemory, iTotalRead);

        if pTempBIO <> nil then try
          // Tries to read this as a .P7M file.
          pPKCS := mFunctions.d2i_PKCS7_bio(pTempBIO, nil);

          // If it failed, tries again as a .PEM file. (rewinds BIO to BOF)
          if (pPKCS = nil) and (mFunctions.BIO_ctrl(pTempBIO, BIO_CTRL_RESET, 0, nil) = 1) then 
            pPKCS := mFunctions.PEM_read_bio_PKCS7(pTempBIO, nil, nil, nil);

          // If failed again, tries as a S/MIME message. (rewinds BIO to BOF)
          if (pPKCS = nil) and (mFunctions.BIO_ctrl(pTempBIO, BIO_CTRL_RESET, 0, nil) = 1) then
            pPKCS := mFunctions.SMIME_read_PKCS7(pTempBIO, nil);

          // If CMS cryptographic functions are available (hence using OpenSSL libraries 0.9.8h+)
          if (@mFunctions.d2i_CMS_bio <> nil) and (@mFunctions.CMS_verify <> nil) and (@mFunctions.CMS_ContentInfo_free <> nil) then
            // If failed, let's switch to CMS encoding (rewinds BIO to BOF)
            if (pPKCS = nil) and (mFunctions.BIO_ctrl(pTempBIO, BIO_CTRL_RESET, 0, nil) = 1) then begin
              pCMS := mFunctions.d2i_CMS_bio(pTempBIO, nil);
              if pCMS <> nil then begin
                // We need a X509_STORE here.
                pTempStore := mFunctions.X509_STORE_new;
                if pTempStore <> nil then try
                  if mFunctions.CMS_verify(pCMS, nil, pTempStore, nil, nil, CMS_NOVERIFY) <> LIBEAY_OK then begin
                    mFunctions.CMS_ContentInfo_free(pCMS);
                    pCMS := nil
                  end
                finally
                  mFunctions.X509_STORE_free(pTempStore)
                end
              end
            end
        finally
          mFunctions.BIO_free(pTempBIO)
        end
      end
    finally
      FreeMem(pMemory, iTotalMemory)
    end;

    // Can only chech these when using PKCS#7

    // Check that the given data was signed using PKCS#7.
    // This is a small verification also on Verification = False
    if (pPKCS <> nil) and (mFunctions.OBJ_obj2nid(pPKCS^._type) <> NID_PKCS7_SIGNED) then begin
      mFunctions.PKCS7_free(pPKCS);
      pPKCS := nil
    end;
    if (pPKCS <> nil) and (Pointer(mFunctions.PKCS7_ctrl(pPKCS, PKCS7_OP_GET_DETACHED_SIGNATURE, 0, nil)) <> nil) then begin
      mFunctions.PKCS7_free(pPKCS);
      pPKCS := nil
    end;
    // Reads data about signer and checks that at least one is present.
    // This is a small verification also on Verification = False
    if pPKCS <> nil then begin
      pTempStack := mFunctions.PKCS7_get_signer_info(pPKCS);
      if (pTempStack = nil) or (mFunctions.sk_num(pTempStack) = 0) then begin
        mFunctions.PKCS7_free(pPKCS);
        pPKCS := nil
      end
    end;

    // Going to verify input for both PKCS#7 and CMS
    if (pPKCS <> nil) or (pCMS <> nil) then begin
      // Creates memory structure for output data
      pOutputBIO := mFunctions.BIO_new(mFunctions.BIO_s_mem);
      if pOutputBIO <> nil then try
        // Tries *full* verificiation

        // Verifies PKCS7 while extracting it.
        pTempStore := mFunctions.X509_STORE_new;
        if pTempStore <> nil then try
          if pPKCS <> nil then
            Result := mFunctions.PKCS7_verify(pPKCS, nil, pTempStore, nil, pOutputBIO, PKCS7_NOVERIFY) = LIBEAY_OK
          else
            // No need to test @mFunctions.CMS_verify <> nil since pCMS will be null
            Result := mFunctions.CMS_verify(pCMS, nil, pTempStore, nil, pOutputBIO, CMS_NOVERIFY) = LIBEAY_OK
        finally
          mFunctions.X509_STORE_free(pTempStore)
        end;
        // If we managed to get the data, we have a *full* verification.
        if Result then
          fVerifyStatus := vsFull
        else begin
          // Does no verification, just tries to read.

          // Writes out extracted data.
          if pPKCS <> nil then
            pTempBIO := mFunctions.PKCS7_dataInit(pPKCS, nil)
          else
            // No need to test @mFunctions.CMS_dataInit <> nil since pCMS will be null
            pTempBIO := mFunctions.CMS_dataInit(pCMS, nil);
          if pTempBIO <> nil then try
            GetMem(pMemory, READ_BUFFER_SIZE);
            try
              // Copies data from buffer to output BIO
              iTotalRead := 0;
              iTotalWritten := 0;
              repeat
                iRead := mFunctions.BIO_read(pTempBIO, pMemory, READ_BUFFER_SIZE);
                iTotalRead := iTotalRead + iRead;
                if iRead > 0 then begin
                  iWritten := mFunctions.BIO_write(pOutputBIO, pMemory, iRead);
                  iTotalWritten := iTotalWritten + iWritten;
                end;
              until iRead <= 0;
              Result := iTotalRead = iTotalWritten
            finally
              FreeMem(pMemory, READ_BUFFER_SIZE)
            end
          finally
            mFunctions.BIO_free(pTempBIO)
          end;
          // If we were able to get that data, we have a partial verify.
          if Result then
            fVerifyStatus := vsPartial
        end;

        // Writes to output stream
        if Result and (pOutputBIO^.num_write > 0) then begin
          // Allows developer to use a better TStream descendant for storage (TFileStream, ...)
          if @fOnStreamCreate <> nil then
            fOnStreamCreate(Self, fContent);
          // By default we use TMemorySteam, this could use a bit of RAM.
          if fContent = nil then
            fContent := TMemoryStream.Create;
          GetMem(pMemory, pOutputBIO^.num_write);
          try
            // Copies data from BIO to memory buffer.
            Result := (Cardinal(mFunctions.BIO_read(pOutputBIO, pMemory, pOutputBIO^.num_write)) = pOutputBIO^.num_write) and
              // Copies data from memory to stream.
              (fContent.Write(pMemory^, pOutputBIO^.num_write) = Int64(pOutputBIO^.num_write))
          finally
            FreeMem(pMemory)
          end
        end
      finally
        mFunctions.BIO_free(pOutputBIO)
      end
    end;
  finally
    if Result then begin
      // Saves signature mode.
      if pCMS <> nil then
        fSignatureMode := smCMS
      else if pPKCS <> nil then
        fSignatureMode := smPKCS7;
    end else
      // Since it was a failure, make sure everything is cleared out.
      Clear;
    // Frees everything left in memory. (no need to test @mFunctions.CMS_ContentInfo_free <> nil)
    if pCMS <> nil then
      mFunctions.CMS_ContentInfo_free(pCMS);
    if pPKCS <> nil then
      mFunctions.PKCS7_free(pPKCS)
  end
end;

function TPKCS7Message.SaveToFile(const Filename: String): Boolean;
var
  FStream: TFileStream;
  wMode: Word;
begin
  // File open mode
  wMode := fmOpenReadWrite or fmShareDenyWrite;
  if not FileExists(Filename) then 
    wMode := wMode or fmCreate;
  // Opens file stream
  try
    FStream := TFileStream.Create(Filename, wMode);
    try
      // Saves to output stream
      FStream.Size := 0;
      Result := SaveToStream(FStream)
    finally
      FStream.Free
    end
  except
    Result := False;
    // When something goes wrong when creating a file, make sure
    // we don't leave empty files around.
    if wMode and fmCreate = fmCreate then
      Windows.DeleteFile(PChar(Filename))
  end
end;

function TPKCS7Message.SaveToStream(Stream: TStream): Boolean;
begin
  Result := False;
  if (fContent <> nil) and (Stream <> nil) then
    Result := Stream.CopyFrom(fContent, 0) = fContent.Size
end;

initialization
  FillChar(mFunctions, SizeOf(TLibeay32Functions), 0)

finalization

end.
