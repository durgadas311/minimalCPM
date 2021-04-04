; A cdebug util for WizNET 5500 devices, attached in parallel-SPI interface
VERN	equ	01h

	extrn	wizget
	extrn	last
	public	nvbuf

	maclib	z80

CR	equ	13
LF	equ	10
TAB	equ	9
BS	equ	8
BEL	equ	7
CTLC	equ	3

; entry points in main ROM
conout	equ	0040h
conin	equ	0043h
conine	equ	0046h

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; These defines should be in a common file...

nsock	equ	8
SOCK0	equ	000$01$000b
SOCK1	equ	001$01$000b
SOCK2	equ	010$01$000b
SOCK3	equ	011$01$000b
SOCK4	equ	100$01$000b
SOCK5	equ	101$01$000b
SOCK6	equ	110$01$000b
SOCK7	equ	111$01$000b

	cseg
first:
	db	'C','Z'
	dw	last-first
	dw	descr-first

;	Jump vector for command entry points
	jmp	start	; network configuration util

	dseg
descr:	db	'Debug dump WizNet',0

	cseg
start:
	sspd	savstk
	call	crlf
	; read in currrent config (pick source)
; get config from WIZ850io...
	lxi	h,nvbuf
	lxi	d,0	; offset +0, BSB=0
	mvi	b,32	; entire block
	call	wizget
	; Get only socket 0...
	lxi	h,nvbuf+32	; socket array area
	mvi	d,SOCK0	; BSB 08h = Socket 0 Register Block
	mvi	e,0	; offset +0
	mvi	b,nsock
save0:	push	b
	mvi	b,48	; extended block
	call	wizget	; HL=next block (socket)
	pop	b
	mvi	a,001$00$000b	; socket BSB incr value
	add	d
	mov	d,a
	djnz	save0

	lxi	h,nvbuf
	mvi	d,0
	mvi	b,32
	call	dump
	lxix	nvbuf+32
	mvi	d,SOCK0
	mvi	b,nsock
save2:
	push	b
	ldx	a,+4	; 0x31 if configured
	cpi	31h
	jrnz	save1
	call	crlf
	pushix
	pop	h
	mvi	b,48
	call	dump
save1:
	lxi	b,48
	dadx	b
	mvi	a,001$00$000b	; socket BSB incr value
	add	d
	mov	d,a
	pop	b
	djnz	save2

exit:	lspd	savstk
	ret

; HL=buffer, B=length
; D=bsb ctrl byte
dump:	mvi	c,0	; offset (length-B)
du0:
	call	bsbout
	call	offout
du1:
	call	space
	mov	a,m
	call	hexout
	inx	h
	inr	c
	mov	a,c
	ani	0fh
	jrnz	du2
	call	crlf
	djnz	du0
	jr	du3
du2:	djnz	du1
du3:	mov	a,c
	ani	0fh
	rz
	jr	crlf

; C=offset
offout:	mvi	a,0
	call	hexout
	mov	a,c
	call	hexout
	mvi	a,':'
	jmp	chrout

; D=bsb ctrl byte
bsbout:
	mov	a,d
	rrc
	rrc
	rrc
	ani	1fh
	call	hexout
space:	mvi	a,' '
	jmp	chrout

crlf:
	mvi	a,CR
	call	chrout
	mvi	a,LF
	jmp	chrout

wrdout:
	mov	a,h
	call	hexout
	mov	a,l
hexout:
	push	psw
	rrc
	rrc
	rrc
	rrc
	call	hexdig
	pop	psw
hexdig:
	ani	0fh
	adi	90h
	daa
	aci	40h
	daa
	jmp	chrout

msgout:	ldax	d
	ora	a
	rz
	inx	d
	call	chrout
	jr	msgout

chrout:	push	b
	mov	c,a
	call	conout
	pop	b
	ret

	dseg
	ds	40
stack:	ds	0
savstk	dw	0

nvbuf:	ds	32+(8*48)

	end
