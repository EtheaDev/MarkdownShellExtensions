unit u_dzSynopse;

// in my tests in Delphi 2007 on an Intel Xeon 8 core CPU this wasn't any faster than system.move
// -- 2022-05-23 twm
 
interface

uses
  SysUtils,
  u_dzTypes;

procedure MoveFast(const src; var dst; cnt: IntPtr);

implementation

uses
  u_dzGraphicsUtils;

procedure Init;
begin
//  u_dzGraphicsUtils.MoveFast := u_dzSynopse.MoveFast;
end;

// This implementation ist taken from the
// file mormot.core.base.asmx86.inc which is part of the
// Open Source Synopse mORMot framework 2 (https://github.com/synopse/mORMot2)
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
// x86 32-bit assembly used by mormot.core.base.pas

const
  // non-temporal writes should bypass the cache when the size is bigger than
  // half the size of the largest level cache - we assume low 1MB cache here
  NONTEMPORALSIZE = 1 shl 20;

// fast SSE2 version - force define HASNOSSE2 when run any very old CPU
procedure MoveFast(const src; var dst; cnt: IntPtr);
{$IFDEF FPC}nostackframe; assembler;
{$ENDIF}
asm
        // eax=source edx=dest ecx=count
        cmp     eax, edx
        jz      @exit                  // exit if source=dest
        cmp     ecx, 32
        ja      @lrg                   // count > 32 or count < 0
        sub     ecx, 8
        jg      @sml                   // 9..32 byte move
        jmp     dword ptr [@table + 32 + ecx * 4]   // 0..8 byte move
{$ifdef HASNOSSE2}
@sml:   fild    qword ptr [eax + ecx]   // last 8
        fild    qword ptr [eax]         // first 8
        cmp     ecx, 8
        jle     @sml16
        fild    qword ptr [eax + 8]     // second 8
        cmp     ecx, 16
        jle     @sml24
        fild    qword ptr [eax + 16]    // third 8
        fistp   qword ptr [edx + 16]    // third 8
@sml24: fistp   qword ptr [edx + 8]     // second 8
@sml16: fistp   qword ptr [edx]         // first 8
        fistp   qword ptr [edx + ecx]   // last 8
{$else}
@sml:   movq    xmm0, qword ptr [eax + ecx]   // last 8
        movq    xmm1, qword ptr [eax]         // first 8
        cmp     ecx, 8
        jle     @sml16
        movq    xmm2, qword ptr [eax + 8]     // second 8
        cmp     ecx, 16
        jle     @sml24
        movq    xmm3, qword ptr [eax + 16]    // third 8
        movq    qword ptr [edx + 16], xmm3    // third 8
@sml24: movq    qword ptr [edx + 8], xmm2     // second 8
@sml16: movq    qword ptr [edx], xmm1         // first 8
        movq    qword ptr [edx + ecx], xmm0   // last 8
        ret
{$endif HASNOSSE2}
@exit:  rep     ret
        {$ifdef FPC} align 4 {$endif}
@table: dd      @exit, @m01, @m02, @m03, @m04, @m05, @m06, @m07, @m08
{$ifdef WITH_ERMSASM}
@ermsf: push    esi
        push    edi
        mov     esi, eax
        mov     edi, edx
        cld
        rep     movsb  // ERMSB forward move
        pop     edi
        pop     esi
        ret
@ermsb: push    esi
        push    edi
        lea     esi, [eax + ecx - 1]
        lea     edi, [edx + ecx - 1]
        std
        rep     movsb  // ERMSB backward move
        pop     edi
        pop     esi
        cld            // FPC requires this
        ret
{$endif WITH_ERMSASM}
{$ifdef HASNOSSE2}
@lrg:   jng     @exit                // count < 0
        cmp     eax, edx
        ja      @ermsf               // fallback to rep movsb on old CPUs
        sub     edx, ecx
        cmp     eax, edx
        lea     edx, [edx + ecx]
        jna     @ermsf
        jmp     @ermsb
{$else}
@lrgfwd:// large forward move
        {$ifdef WITH_ERMS}
        cmp     ecx, NONTEMPORALSIZE
        jae     @noermf // movntdq was reported to be faster than ERMSB
        cmp     ecx, ERMSB_MIN_SIZE
        ja      @ermsf
@noermf:{$endif WITH_ERMS}
        push    edx
        movups  xmm2, dqword ptr [eax]       // first 16
        lea     eax, [eax + ecx - 16]
        lea     ecx, [ecx + edx - 16]
        movups  xmm1, dqword ptr [eax]       // last 16
        push    ecx
        neg     ecx
        and     edx,  -16                    // 16-byte align writes
        lea     ecx, [ecx + edx + 16]
        pop     edx
        cmp     ecx, -NONTEMPORALSIZE        // assume > 512KB bypass the cache
        jl      @fwnv
        {$ifdef FPC} align 16 {$endif}
@fwd:   movups  xmm0, dqword ptr [eax + ecx] // move by 16 bytes
        movaps  dqword ptr [edx + ecx], xmm0
        add     ecx, 16
        jl      @fwd
@fwde:  movups  dqword ptr [edx], xmm1       // last 16
        pop     edx
        movups  dqword ptr [edx], xmm2       // first 16
        ret
        {$ifdef FPC} align 16 {$endif}
@fwnv:  movdqu  xmm0, dqword ptr [eax + ecx]
        movntdq dqword ptr [edx + ecx], xmm0 // non-temporal move by 16 bytes
        add     ecx, 16
        jl      @fwd
        sfence
        jmp     @fwde
@lrg:   jng     @exit                // count < 0
        cmp     eax, edx
        ja      @lrgfwd
        sub     edx, ecx
        cmp     eax, edx
        lea     edx, [edx + ecx]
        jna     @lrgfwd
        // large backward/overlapping move
        {$ifdef WITH_ERMS}
        cmp     ecx, ERMSB_MIN_SIZE
        ja      @ermsb
        {$endif WITH_ERMS}
        sub     ecx, 16
        push    ecx
        movups  xmm2, dqword ptr [eax + ecx] // last 16
        movups  xmm1, dqword ptr [eax]       // first 16
        add     ecx, edx
        and     ecx, -16              // 16-byte align writes
        sub     ecx, edx
        {$ifdef FPC} align 16 {$endif}
@bwd:   movups  xmm0, dqword ptr [eax + ecx]
        movaps  dqword ptr [edx + ecx], xmm0
        sub     ecx, 16
        jg      @bwd
        pop     ecx
        movups  dqword ptr [edx], xmm1       // first 16
        movups  dqword ptr [edx + ecx], xmm2 // last 16
        ret
{$endif HASNOSSE2}
@m01:   mov     al, [eax]
        mov     [edx], al
        ret
@m02:   movzx   eax, word ptr [eax]
        mov     [edx], ax
        ret
@m03:   movzx   ecx, word ptr [eax]
        mov     al, [eax + 2]
        mov     [edx], cx
        mov     [edx + 2], al
        ret
@m04:   mov     ecx, [eax]
        mov     [edx], ecx
        ret
@m05:   mov     ecx, [eax]
        mov     al, [eax + 4]
        mov     [edx], ecx
        mov     [edx + 4], al
        ret
@m06:   mov     ecx, [eax]
        movzx   eax, word ptr [eax + 4]
        mov     [edx], ecx
        mov     [edx + 4], ax
        ret
@m07:   mov     ecx, [eax]
        mov     eax, [eax + 3]
        mov     [edx], ecx
        mov     [edx + 3], eax
        ret
@m08:   mov     ecx, [eax]
        mov     eax, [eax + 4]
        mov     [edx], ecx
        mov     [edx + 4], eax
end;

initialization
  Init;
end.
