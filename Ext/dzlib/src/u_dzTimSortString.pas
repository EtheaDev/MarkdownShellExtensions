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
* This Code is based on                                                     *
* https://github.com/avk959/LGenerics/blob/master/lgenerics/lgmiscutils.pas *
* but has been heavily modified by me. In particular, I have made it        *
* compatible to Delphi 2007 and removed everything not TimSort releated.    *
* Note that the current code only sorts integers.                           *
*****************************************************************************}
unit u_dzTimSortString;

{$I 'dzlib.inc'}

interface

uses
  Classes,
  SysUtils,
  Math,
  u_dzTypes,
  u_dzTimSortUtils;

{$DEFINE __TIMSORT_TEMPLATE__}
type
  _DZ_TIM_SORT_ITEM_ = string;

// in dzlib\templates
{$INCLUDE 't_dzTimSortTemplate.tpl'}

implementation

{$INCLUDE 't_dzTimSortTemplate.tpl'}

procedure TTimSort.MoveItems(_Src, _Dst: Pointer; _Cnt: Integer);
var
  src: PItemArray absolute _Src;
  dst: PItemArray absolute _Dst;
  i: Integer;
begin
  if UIntPtr(_Src) < UIntPtr(_Dst) then begin
    for i := _Cnt - 1 downto 0 do
      dst[i] := src[i];
  end else begin
    for i := 0 to _Cnt - 1 do
      dst[i] := src[i];
  end;
end;

end.


