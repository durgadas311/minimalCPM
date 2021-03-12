; NETBOOT I/O module for Z180 ASCI1

; already done when included...
;	maclib	z180
;
;false	equ	0
;true	equ	not false
;
;	$*macro

; Z180 ASCI1 registers - only enough for I/O
ctlb	equ	03h	; for CTS
stat	equ	05h
tdr	equ	07h
rdr	equ	09h
; Needed for RTS control, not part of ASCI1...
cntr	equ	0ah
trdr	equ	0bh

; TODO: manage this in platform-specific modules...
;	public	init,conout
;	; exported for SNIOS
;	public	recvbt,recvby,sendby
; Or:	include	chario.asm
;
; Extry:	init,deinit,conin,conout,
;		sendby,recvbt,recvby,putfifo

iovers:	db	'Z180-ASCI1$'
stat0:	db	0

; make certain the console interrupts are off...
init:
	di
	; The only bits that are writeable
	; are the ones we zero... but we'll
	; be kind.
	in0	a,stat
	sta	stat0
	ani	11110110b
	ori	00000100b	; need CTS1E=1
	out0	a,stat
	; safe to EI now???
	; ASCI1 does not have an RTS line,
	; So MinZ simulates that with a FF
	; on the CSIO port.
	xra	a	; low = active for /RTS
	out0	a,trdr
	mvi	a,10h	; transmit enable (send byte)
	out0	a,cntr
	ei
	ret

deinit:
	di
	; TODO: should we turn off RTS?
	mvi	a,0ffh	; high = inactive for /RTS
	out0	a,trdr
	mvi	a,10h	; transmit enable (send byte)
	out0	a,cntr
	lda	stat0
	out0	a,stat
	ei
	ret

;;;;; not used here? ;;;;;
; Get char from console
; Returns: A=char, stripped
conin:	in0	a,stat
	ani	10000000b	; RDRF
	jrz	conin
	in0	a,rdr
	ani	07fh
	ret
	
; Output char to console (not ASCI1!)
; Preserves HL for 'print' caller.
; C=char
conout:
	push	h
	lhld	1
	mvi	l,12	; conout vector
	call	icall
	pop	h
	ret

icall:	pchl

sendby:	push	psw
sendb1:
	in0	a,ctlb
	ani	00100000b	; /CTS
	jrnz	sendb1
	in0	a,stat
	ani	00000010b	; TDRE
	jrz	sendb1
	pop	psw
	out0	a,tdr
	ret

; These are for SNIOS...

; For CP/NET boot, wait long timeout for one char
; Return: CY=timeout else A=char
; At 115200, one char is 1600 cycles...
recvbt:
	push	d
	push	b
	mvi	d,20	; 20x = 3.1 seconds (5.6 seconds)
coni0:	; loop = 156mS
	lxi	b,0	; 65536* = 2883584 = 5177344
coni1:				; 0-wait    3-wait
	in0	a,stat		; 12        21?
	ani	10000000b	;  6        12
	jrnz	coni2		;  6n       12n
	dcx	b		;  4        7
	mov	a,b		;  4        7
	ora	c		;  4        7
	jrnz	coni1		;  8t = 44  14t = 80
	dcr	d
	jrnz	coni0
	pop	b
	pop	d
	stc
	ret
coni2:	in0	a,rdr	; CY=0 from ANI
	pop	b
	pop	d
	ret

; For CP/NET boot, wait short timeout for next char
recvby:
	push	d
	push	b
	mvi	d,2	; 2x = 312mS for next char
	jr	coni0

; Save stray conin characters...
putcon:	; not used here, just discard...
	ret
