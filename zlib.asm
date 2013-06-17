;
;	zmul, zdiv, zsqrt  --  fix point mult, div, sqrt for NAUTILUS
;
;	This file is a component of the Nautilus digital voice application
;
;	Author: Ray Berry, Bellevue WA
;
;  fixed point math routines - require 386 cpu.  Assume FR_BITS fractional
;  bits- set below at compile time. FR_BITS is assumed to be an EVEN #.
;
;  don't use for zsqrt for general purpose computing - it is deficient
;  in a manner benign to NAUTILUS.
;  overflow behavior:
;		zmul:  result is clamped to maximum magnitude representable
;		zdiv:  div by zero silently returns max legal magnitude
;		       overflow silently returns max legal magnitude
;
;  SCCS ID: @(#)zlib.asm 1.2 96/03/31
;
;	v1.0 12/17/93  rjb	initial version
;	v1.1 01/08/93  rjb	extended zsqrt to return 32 bit result
;
model	large, c
.code
.386

FR_BITS	EQU	20

PUBLIC	zmul, zdiv, zsqrt

proc	zmul
	ARG	a:DWORD, b:DWORD
	; returns a*b
	mov	eax, a
	imul	b		; 64 bit product in edx:eax
	shrd	eax, edx, FR_BITS
	sar	edx, FR_BITS-1
	jns	pr		; if product was neg,
	bt	edx, 31		; preserve sign in cy flag
	inc	edx		; roll top 1/2 to zeros
pr:	jz	m_out		; if zero, no overflow
	mov	eax, 7FFFFFFFH	; else assume pos oflow
	jnc	m_out
	not	eax		; & flip if assume wrong
m_out:	shld	edx, eax, 16	; adj for proper return to caller
	ret
endp	zmul


proc	zdiv
	ARG a:DWORD, b:DWORD
	; return = a/b (signed)  USES: eax, ebx, ecx, edx

	; convert dividend and divisor to absolute values
	xor	eax, eax	; form abs(divisor)
	add	eax, b		; swallow div-by-0 exception
	jz	oflo
	cdq
	xor	eax, edx
	sub	eax, edx	; 
	mov	ebx, eax	; ebx = abs(divisor)
				; the TMS9900 had an ABS instruction
	mov	eax, a		; (& look what happened to IT :)
	cdq	
	xor	eax, edx
	sub	eax, edx
	xor	edx, edx	; abs(dividend) = edx:eax

	; do 64/32 >> 64  divide
	shld	edx, eax, FR_BITS+1
	shl	eax, FR_BITS+1
	xor	ecx, ecx
	xchg	eax, ecx	; cx = bot, ax = 0, dx=top
	xchg	eax, edx	; cx = bot, ax =top, dx =0
	div	ebx
	xchg	eax, ecx	; dx = top(rem), ax=bot
	div	ebx		; quotient now = ecx:eax

	; check overflow
	jcxz	noflo		; things are arranged so that
oflo:	xor	eax, eax	; bit 31 of eax is largest legal bit
	not	eax		; 0xffffffff = max mag

	; restore sign
noflo:	inc	eax		; round result
	shr	eax, 1
	mov	dl, byte ptr a+3
	xor	dl, byte ptr b+3	; quotient neg?
	jns	p3
	neg	eax

p3: 	shld	edx, eax, 16	; compiler expects result in DX:AX

	ret
endp	zdiv

; integer square root of 32 bit FixP # by successive approximation
; warnings:
; 	1. assumes unsigned input. msb is NOT treated as sign bit
;	2. FR_BITS must be EVEN!
;	3. code will FAIL by overflow exception for small negative inputs.
;	4. resultant root, squared, may exceed input argument.
; register usage:
;	ebx:ecx	... stores copy of 64b extended arg throughout ...
;	edi	... "last" estimate of isqrt(source)...
;	esi	... current estimate of isqrt(source)...
;	edx:eax	... used to divide source by edi

proc	zsqrt
	USES	esi, edi
	ARG	arg1:DWORD	; arg1 not used in code

	xor	edi, edi
	mov	ebx, arg1
	bsr	ecx, ebx	; get index of ms bit in arg
	jz	done		; return 0
	inc	di		; build crude 2-bit approx of root
	shr	cl, 1
	rcl	di, 1
	add	cl, 15
	rcl	edi, cl

refine:	mov	esi, edi

	mov	edx, ebx	; prep for division by estimate
	xor	eax, eax
	div	edi

	add	edi, eax	; average quotient into last estimate
	rcr	edi, 1

	sub	esi, edi	; diff between old(si) & new estimate
	inc	esi		; change (-1 to 1) to (0 to 2)
	js	refine		; if negative, keep iterating
	cmp	esi, 2		; almost done if 0 to 2
	ja	refine		; if > 2, keep iterating

done:	mov	eax, edi	; results returned in dx:ax
	shr	eax, 16 - (FR_BITS/2)
	shld	edx, eax, 16
	ret
endp	zsqrt

	END
