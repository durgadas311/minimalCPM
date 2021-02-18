;	title	'BIOS for CP/NOS 1.2'
;	Character-only functions
;	Modified for Minimal CP/M, Douglas Miller <durgadas311@gmail.com>
;
;	Version 1.1 October, 1981
;	Version 1.2 Beta Test, 08-23-82
;
vers	equ	12	;version 1.2
;
;	Copyright (c) 1980, 1981, 1982
;	Digital Research
;	Box 579, Pacific Grove
;	California, 93950
;
;	perform following functions
;	boot	cold start
;	wboot	(not used under CP/NOS)
;	const	console status
;		reg-a = 00 if no character ready
;		reg-a = ff if character ready
;	conin	console character in (result in reg-a)
;	conout	console character out (char in reg-c)
;	list	list out (char in reg-c)
;
;
cr	equ	0dh	;carriage return
lf	equ	0ah	;line feed
;
buff	equ	0080h	;default buffer

	maclib z180

; Z180 MMU ports
itc	equ	34h
mmu$cbr	equ	38h
mmu$bbr	equ	39h
mmu$cbar equ	3ah

; Z180 ASCI0 ports
ctla	equ	00h	
ctlb	equ	02h	
stat	equ	04h	
tdr	equ	06h	
rdr	equ	08h	
asxt	equ	12h

; Note new cold-boot sequence.
;	1. Arrive first here at 'cboote'.
;	2. Initialize BIOS and page 0 (for NDOS)
;	3. Jump to NDOS cold-boot entry.
;	4. NDOS initializes:
;		4.1. Calls SNIOS init
;		4.2. Calls BDOS init
;		4.3. Intercepts WBOOT
;		4.4. Loads CCP.SPR and jumps to it (every WBOOT)

	org	0
base	equ	$
ndos$pg	equ	base+0f900h
bdos$pg	equ	base+0fd00h

ndoscb	equ	ndos$pg+3	; NDOS cold-boot
ndose	equ	ndos$pg+6
bdose	equ	bdos$pg+6

;	jump vector for indiviual routines
; Cold boot arrives here first...
cboote:	jmp	boot
wboote:	jmp	error	; get's intercepted by NDOS
	jmp	const	; replaced by NDOS routine...
	jmp	conin	; replaced by NDOS routine...
bco:	jmp	conout	; replaced by NDOS routine...
	jmp	list	; replaced by NDOS routine...
	jmp	error
	jmp	error
	jmp	error
	jmp	error
	jmp	error
	jmp	error
	jmp	error
	jmp	error
	jmp	error
	jmp	listst	; replaced by NDOS routine...
	jmp	error
	jmp	wbhook	; +51: warm boot hook, for Z180 TRAP
	jmp	recvbt	; +54: first char of recv
	jmp	recvby	; +57:
	jmp	sendby	; +60:
	jmp	putfifo	; +63: put char to fifo

;
;signon:	;signon message: xxk cp/m vers y.y
;	db	cr,lf,lf
;	db	'64'	;memory size
;	db	'k CP/NOS vers '
;	db	vers/10+'0','.',vers mod 10+'0'
;	db	0
;
boot:	;print signon message and go to NDOS
;
;	device initialization  -  as required
;
	; set MMU for all-memory (org0)
	mvi	a,1111$0000b	; ca at 0xF000, ba at 0x0000
	out0	a,mmu$cbar
	;
	lxi	sp,buff+0080h
; can't print message here - need NDOS/SNIOS
;	lxi	h,signon
;	call	prmsg	;print message
	mvi	a,jmp
	sta	0000h
	sta	0005h
	lxi	h,ndose
	shld	0006h
	xra	a
	sta	0004h
	lxi	h,wboote	; for NDOS init
	shld	0001h
	jmp	ndoscb ;go to NDOS initialization

; must not depend on stack until we clear TRAP.
; HL=return address
wbhook:
	in0	a,itc
	bit	7,a
	jrz	jmphl	; no TRAP
	pop	d	; TRAP address
	dcx	d	; possible opcode start
	bit	6,a
	jrz	wb0
	dcx	d
wb0:	ani	01111111b	;
	out0	a,itc		; reset TRAP latch
	lxi	sp,100h		; need stack to print message...
	push	h		; return address from NDOS
	lxi	h,trapx
	mov	a,d
	call	hexout
	mov	a,e
	call	hexout
	lxi	h,trap
	; NOTE: this message may/will go over CP/NET...
	jmp	msgout	; and return to NDOS
jmphl:	pchl

trap:	db	cr,lf,'*** TRAP '
trapx:	db	'xxxx',cr,lf,0

; puts hex digits at (HL++)
hexout:
	push	psw
	rlc
	rlc
	rlc
	rlc
	call	hexdig
	pop	psw
hexdig:	ani	0fh
	adi	90h
	daa
	aci	40h
	daa
	mov	m,a
	inx	h
	ret

; print message at HL until NUL
msgout:	mov	a,m
	ora	a
	rz
	inx	h
	mov	c,a
	push	h
	call	bco
	pop	h
	jr	msgout

;
;

fifo:	db	0,0,0,0,0,0,0,0
wfifo:	dw	fifo	; fifo write ptr
rfifo:	dw	fifo	; fifo read ptr

putfifo:
	push	h
	push	d
	lhld	wfifo
	mov	m,a
	inx	h
	mov	a,l
	lxi	d,fifo+8
	cmp	e
	jrnz	pf0
	lxi	h,fifo
pf0:	shld	wfifo
	pop	d
	pop	h
	ret

getfifo:
	lhld	rfifo
	mov	a,m
	inx	h
	push	psw
	mov	a,l
	lxi	d,fifo+8
	cmp	e
	jrnz	gf0
	lxi	h,fifo
gf0:	shld	rfifo
	pop	psw
	ret

chkfifo:
	lda	rfifo
	mov	c,a
	lda	wfifo
	sub	c
	rz	; nothing ready
	ori	0ffh
	ret

conclr:	in0	a,stat
	ani	10000000b	; RDRF
	rz	; nothing left
	in0	a,rdr
	call	putfifo
	jr	conclr

const:	;console status to reg-a
	call	conclr
	call	chkfifo
	ret
;
conin:	;console character to reg-a
	call	const
	jrz	conin
	call	getfifo
	ret
;
conout:	;console character from c to console out
	mov	a,c
	jr	sendby
;
conost:
	in0	a,ctlb
	ani	00100000b	; /CTS
	jrnz	cono0
	in0	a,stat
	ani	00000010b	; TDRE
	rz	; not ready
	ori	0ffh
	ret
cono0:	xra	a
	ret
;
list:	;list device out
	ret
;
listst:
	mvi	a,0ffh
	ret
;
;	utility subroutines
error:
	lxi	h,0ffffh
	mov	a,h
	ret

; For CP/NET...
;
; Output char to console
; A=char
sendby:
	push	psw
sendb0:
	in0	a,ctlb
	ani	00100000b	; /CTS
	jrnz	sendb0
	in0	a,stat
	ani	00000010b	; TDRE
	jrz	sendb0
	pop	psw
	out0	a,tdr
	ret

; Return: CY=timeout else A=char
; At 115200, one char is 1600 cycles...
; Destroys BC, D - must preserve!
; char never comes from fifo
recvbt:
	push	d
	push	b
	mvi	d,20	; 20x = 3.1 seconds
coni0:	; loop = 156mS
	lxi	b,0		; 65536 * 44 = 2883584
coni1:
	in0	a,stat		; 12
	ani	10000000b	;  6
	jrnz	coni2		;  6 (n)
	dcx	b		;  4
	mov	a,b		;  4
	ora	c		;  4
	jrnz	coni1		;  8 (t) = 44
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

; For CP/NET, wait short timeout for next char
; Destroys BC, D - must preserve!
recvby:
	push	d
	push	b
	mvi	d,2	; 2x = 312mS for next char
	jr	coni0

	end
