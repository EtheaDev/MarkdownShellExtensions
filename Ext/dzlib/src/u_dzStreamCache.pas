unit u_dzStreamCache;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  Windows,
  SysUtils,
  Classes;

type
  ///<summary>
  /// This is a TStream descendant that wraps another stream (the BaseStream) and implements
  /// caching. It is optimized for forward reading and writing and up to a point can also be
  /// used for random access. Worst performance is when reading backwards. </summary>
  TdzStreamCache = class(TStream)
  public
    const
      DefaultCacheSize = 16 * 1024;
  private
    FBaseStream: TStream;
    FOwnsBaseStream: Boolean;
    FCacheSize: Integer;
    FFilePos: Int64;
    FCacheStartPos: Int64;
    FCacheEndPos: Int64;
    FCache: PByte;
    FModified: Boolean;
    ///</summary>
    /// Read the cache from the file </sumamry>
    procedure UpdateCache;
  protected
    procedure SetSize(const _NewSize: Int64); override;
    function GetSize: Int64; override;
  public
    ///<summary>
    /// Creates a new StreamCache
    /// @param BaseStream is the stream to cache.
    /// @param CacheSize is the amount of memory to use for caching, default is DefaultCacheSize
    /// @param OwnsStream determines whether the destructor will free the BaseStream
    ///                   default is false, meaning it won't free it.
    /// Note: Do not access the BaseStream as long as the StreamCache exists. Bad things will happen. </summary>
    constructor Create(_BaseStream: TStream); overload;
    constructor Create(_BaseStream: TStream; _OwnsStream: Boolean); overload;
    constructor Create(_BaseStream: TStream; _CacheSize: Integer); overload;
    constructor Create(_BaseStream: TStream; _CacheSize: Integer; _OwnsStream: Boolean); overload;
    ///<summary>
    /// Destroys the StreamCache, optionally also frees the BaseStream </summary>
    destructor Destroy; override;
    ///<summary>
    /// Write cached and not yet written data to the file. </summary>
    procedure FlushCache; inline;
    ///<summary>
    /// Buffered read method.
    /// Note that after this was called, the BaseStream's Position might be different from the
    /// one of the StreamCache. </summary>
    function Read(var _Buffer; _Count: LongInt): LongInt; override;
    ///<summary>
    /// Buffered write method
    /// Note that after this was called, the BaseStream might not have been updated yet.
    /// and that the BaseStream's Position might be different from the one of the StreamCache. </summary>
    function Write(const _Buffer; _Count: LongInt): LongInt; override;
    ///<summary>
    /// Buffered seek method
    /// Note that after this was called, the BaseStream's Position might be different from the
    /// one of the StreamCache. </summary>
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;
{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

{ TdzStreamCache }

constructor TdzStreamCache.Create(_BaseStream: TStream);
begin
  Create(_BaseStream, DefaultCacheSize, False);
end;

constructor TdzStreamCache.Create(_BaseStream: TStream; _OwnsStream: Boolean);
begin
  Create(_BaseStream, DefaultCacheSize, _OwnsStream);
end;

constructor TdzStreamCache.Create(_BaseStream: TStream; _CacheSize: Integer);
begin
  Create(_BaseStream, _CacheSize, False);
end;

constructor TdzStreamCache.Create(_BaseStream: TStream; _CacheSize: Integer; _OwnsStream: Boolean);
begin
  inherited Create;

  if _CacheSize <= 0 then
    raise Exception.Create('CacheSize must be >0.');

  FBaseStream := _BaseStream;
  FOwnsBaseStream := _OwnsStream;
  FCacheSize := _CacheSize;

  GetMem(FCache, FCacheSize);
// For debug purposes it might be a good idea to zero the cache so we can see what gets written
// to it. If it works as expected, this won't be necessary.
//  ZeroMemory(FCache, FCacheSize);
// sometimes a value <> 0 is more usefull:
//  FillChar(FCache^, FCacheSize, $88);
end;

destructor TdzStreamCache.Destroy;
begin
  if Assigned(FBaseStream) then
    FlushCache;
  FreeMem(FCache, FCacheSize);
  if FOwnsBaseStream then
    FreeAndNil(FBaseStream);
  inherited;
end;

function TdzStreamCache.GetSize: Int64;
begin
  Result := FBaseStream.Size;
  if Result < FCacheEndPos then
    Result := FCacheEndPos;
end;

procedure TdzStreamCache.SetSize(const _NewSize: Int64);
begin
  if _NewSize < FCacheEndPos then
    FlushCache;
  FBaseStream.Size := _NewSize;
end;

procedure TdzStreamCache.FlushCache;
begin
  if FModified then begin
    FBaseStream.Seek(FCacheStartPos, soBeginning);
    FBaseStream.WriteBuffer(FCache^, LongInt(FCacheEndPos - FCacheStartPos));
    FModified := False;
  end;
  FBaseStream.Seek(FFilePos, soBeginning);
  FCacheEndPos := FCacheStartPos;
end;

procedure TdzStreamCache.UpdateCache;
begin
  FCacheStartPos := FBaseStream.Seek(FFilePos, soBeginning);
  FCacheEndPos := FCacheStartPos + FBaseStream.Read(FCache^, FCacheSize);
end;

type
  PUInt64 = ^UInt64;
{$IFDEF DELPHI2007}
  // In Delphi 2007 NativeInt is 64 bits while a pointer is 32 bits.
  dzNativeInt = Integer;
{$ELSE}
  // Lets hope that in all other versions the documentation is correct in stating that
  // SizeOf(NativeInt) = SizeOf(Pointer)
  // The assertion in the initialization checks for this.
  dzNativeInt = NativeInt;
{$ENDIF}

function TdzStreamCache.Read(var _Buffer; _Count: Integer): LongInt;
var
  PSrc: PByte;
begin
  if _Count >= FCacheSize then begin
    FlushCache;
    Result := FBaseStream.Read(_Buffer, _Count)
  end else begin
    if (FCacheStartPos > FFilePos) or (FFilePos + _Count > FCacheEndPos) then begin
      FlushCache;
      UpdateCache;
    end;
    if _Count < FCacheEndPos - FFilePos then
      Result := _Count
    else
      Result := FCacheEndPos - FFilePos;
    PSrc := PByte(dzNativeInt(FCache) + dzNativeInt(FFilePos - FCacheStartPos));
    case Result of
      SizeOf(Byte):
        PByte(@_Buffer)^ := PByte(PSrc)^;
      SizeOf(Word):
        PWord(@_Buffer)^ := PWord(PSrc)^;
      SizeOf(Cardinal):
        PCardinal(@_Buffer)^ := PCardinal(PSrc)^;
      SizeOf(UInt64):
        PUInt64(@_Buffer)^ := PUInt64(PSrc)^;
    else
      Move(PSrc^, _Buffer, Result);
    end;
  end;
  FFilePos := FFilePos + Result;
end;

function TdzStreamCache.Write(const _Buffer; _Count: Integer): LongInt;
var
  PDest: PByte;
begin
  if _Count >= FCacheSize then begin
    FlushCache;
    Result := FBaseStream.Write(_Buffer, _Count);
    FFilePos := FFilePos + Result;
  end else begin
    if (FFilePos < FCacheStartPos) or (FFilePos > FCacheEndPos + 1)
      or (FFilePos + _Count > FCacheStartPos + FCacheSize) then begin
      FlushCache;
      UpdateCache;
    end;
    if FCacheStartPos = FCacheEndPos then begin
      FCacheStartPos := FFilePos;
      FCacheEndPos := FCacheStartPos;
    end;
    Result := _Count;
    PDest := PByte(dzNativeInt(FCache) + dzNativeInt(FFilePos - FCacheStartPos));
    case Result of
      SizeOf(Byte):
        PByte(PDest)^ := PByte(@_Buffer)^;
      SizeOf(Word):
        PWord(PDest)^ := PWord(@_Buffer)^;
      SizeOf(Cardinal):
        PCardinal(PDest)^ := PCardinal(@_Buffer)^;
      SizeOf(UInt64):
        PUInt64(PDest)^ := PUInt64(@_Buffer)^;
    else
      Move(_Buffer, PDest^, Result);
    end;
    FModified := True;
    FFilePos := FFilePos + Result;
    if FFilePos > FCacheEndPos then
      FCacheEndPos := FFilePos;
  end;
end;

function TdzStreamCache.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  NewFilePos: Int64;
begin
  case Origin of
    soBeginning:
      NewFilePos := Offset;
    soCurrent:
      NewFilePos := FFilePos + Offset;
    soEnd:
      NewFilePos := Size + Offset;
  else
    // This should not be possible, but the compiler keeps complaining that
    // NewFilePos might not have been initialized, so we give it an initialization.
    NewFilePos := FFilePos;
  end;
  FFilePos := NewFilePos;
  Result := FFilePos;
end;

initialization
  Assert(SizeOf(dzNativeInt) = SizeOf(Pointer), 'dzNativeInt and Pointer must have the same size');
{$ENDIF DELPHI2007_UP}
end.
