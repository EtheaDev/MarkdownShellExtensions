{****************************************************************************
*                                                                           *
*   This code is free software; you can redistribute it and/or modify it    *
*   under the terms of the Apache License, Version 2.0;                     *
*   You may obtain a copy of the License at                                 *
*     http://www.apache.org/licenses/LICENSE-2.0.                           *
*                                                                           *
*  Unless required by applicable law or agreed to in writing, software      *
*  distributed under the License is distributed on an "AS IS" BASIS,        *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
*  See the License for the specific language governing permissions and      *
*  limitations under the License.                                           *
*                                                                           *
* Part of this Code is based on                                             *
* https://github.com/avk959/LGenerics/blob/master/lgenerics/lgmiscutils.pas *
*****************************************************************************}
unit u_dzTimSortUtils;

interface

uses
  SysUtils,
  u_dzTypes;

{ warning: if aValue > MAX_POSITIVE_POW2 then function will return wrong result }
function RoundUpTwoPower(_Value: NativeInt): NativeInt;

{ returns number of significant bits of Value }
function NSB(_Value: NativeUInt): NativeInt; inline;

function BsrSizeUInt(aValue: NativeUInt): Cardinal;

implementation

const
  MAX_POSITIVE_POW2 = Succ(High(Integer) shr 1);

function IsTwoPower(aValue: NativeUInt): Boolean; inline;
begin
  if aValue <> 0 then
    Result := aValue and Pred(aValue) = 0
  else
    Result := False;
end;

function BsfDWord(const aValue: UInt32): Cardinal;
var
  mask, i: Cardinal;
begin
  mask := 1;
  for i := 0 to 31 do begin
    if aValue and mask <> 0 then begin
      Result := i;
      Exit;
    end;
    mask := mask shl 1;
  end;
  Result := $FFFFFFFF
end;

function BsrDWord(const aValue: UInt32): Cardinal;
var
  mask, i: Cardinal;
begin
  mask := $80000000;
  for i := 31 downto 0 do begin
    if aValue and mask <> 0 then begin
      Result := i;
      Exit;
    end;
    mask := mask shr 1;
  end;
  Result := $FFFFFFFF
end;

function BsfQWord(const aValue: UInt64): Cardinal;
begin
  if lo(aValue) <> 0 then
    Result := BsfDWord(lo(aValue))
  else
    Result := BsfDWord(hi(aValue)) + 32
end;

function BsrQWord(const aValue: UInt64): Cardinal;
begin
  if hi(aValue) <> 0 then
    Result := BsrDWord(hi(aValue)) + 32
  else
    Result := BsrDWord(lo(aValue))
end;

function BsrSizeUInt(aValue: NativeUInt): Cardinal;
begin
{$IFDEF CPU64}
  Result := BsrQWord(aValue);
{$ELSE}
  Result := BsrDWord(aValue);
{$ENDIF}
end;

{ returns number of significant bits of Value }
function NSB(_Value: NativeUInt): NativeInt; inline;
begin
  Result := BsrSizeUInt(_Value) + 1;
end;

{ warning: if aValue > MAX_POSITIVE_POW2 then function will return wrong result }
function RoundUpTwoPower(_Value: NativeInt): NativeInt;
begin
  Assert(_Value <= MAX_POSITIVE_POW2, Format('Argument to RoundUpTwoPower too big (%d)', [_Value]));
  if _Value > 1 then begin
    if not IsTwoPower(_Value) then
      Result := NativeInt(1) shl NSB(_Value)
    else
      Result := _Value; // round not needed ???
  end else
    Result := 2;
end;

end.
