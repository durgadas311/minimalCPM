; NETBOOT I/O module for Z180 ASCI0

	maclib	z180

	public	check,sendby,recvbt,recvby,descr

false	equ	0
true	equ	not false

	$*macro

; Z180 ASCI0 registers - only enough for I/O
ctlb	equ	02h	; for CTS
stat	equ	04h
tdr	equ	06h
rdr	equ	08h

	dseg
descr:	db	'Z180-ASCI0',0
stat0:	db	0
	cseg

; make certain the console interrupts are off...
check:
	di
	; The only bits that are writeable
	; are the ones we zero... but we'll
	; be kind.
	in0	a,stat
	sta	stat0
	ani	11110110b
	out0	a,stat
	; safe to EI now???
	ei
	xra	a
	ret

sendby:	push	psw
conot1:
	in0	a,ctlb
	ani	00100000b	; /CTS
	jrnz	conot1
	in0	a,stat
	ani	00000010b	; TDRE
	jrz	conot1
	pop	psw
	out0	a,tdr
	ret

; For CP/NET boot, wait long timeout for first char
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

	end
