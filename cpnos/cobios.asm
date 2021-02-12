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
wboote:	jmp	error
	jmp	const
	jmp	conin
	jmp	conout
	jmp	list
	jmp	error
	jmp	error
	jmp	error
	jmp	error
	jmp	error
	jmp	error
	jmp	error
	jmp	error
	jmp	error
	jmp	listst	;list status
	jmp	error
	jmp	recvbt	; +51: first char of recv
	jmp	recvby	; +54:
	jmp	sendby	; +57:

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
;
;

; should never get called - network only devices
const:	;console status to reg-a
	xra	a
	ret
;
conin:	;console character to reg-a
	xra	a
	ret
;
conout:	;console character from c to console out
	ret
;
conost:
	mvi	a,0ffh
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
; C=char
sendby:
	push	psw
	in0	a,ctlb
	ani	00100000b	; /CTS
	jrnz	sendby
	in0	a,stat
	ani	00000010b	; TDRE
	jrz	sendby
	pop	psw
	out0	a,tdr
	ret

; Return: CY=timeout else A=char
; At 115200, one char is 1600 cycles...
; Destroys BC, D
recvbt:
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
	stc
	ret
coni2:	in0	a,rdr	; CY=0 from ANI
	ret

; For CP/NET, wait short timeout for next char
; Destroys BC, D
recvby:
	mvi	d,2	; 2x = 312mS for next char
	jr	coni0

	end
