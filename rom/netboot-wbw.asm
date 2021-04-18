; Runs under RomWBW, network boots CP/NOS.

; TODO: need config file for  this...
WIZNET	equ	1	; compiling for WizNET with NVRAM?

	maclib	z180

	extrn	NTWKIN,NTWKST,CNFTBL,SNDMSG,RCVMSG,NTWKER,NTWKBT,NTWKDN,CFGTBL
;	extrn	descr
if WIZNET
	extrn	wizcfg
	public	nvbuf
endif

false	equ	0
true	equ	not false

	$*macro

; RomWBW constants
system	equ	0fff0h
cmdlin	equ	80h

; function values for BC
reboot	equ	0f001h
fconi	equ	00000h
fcono	equ	00100h

CR	equ	13
LF	equ	10
CTLC	equ	3
BEL	equ	7
TAB	equ	9
BS	equ	8
ESC	equ	27
TRM	equ	0
DEL	equ	127

; offsets in msgbuf
FMT	equ	0
DID	equ	1
SID	equ	2
FNC	equ	3
SIZ	equ	4
DAT	equ	5

; relative locations in cpnos (.sys) image
memtop	equ	0	; top of memory, 00 = 64K
comlen	equ	1	; common length
bnktop	equ	2	; banked top (not used)
bnklen	equ	3	; banked length (00)
entry	equ	4	; entry point of OS
cfgtab	equ	6	; CP/NET cfgtbl
org0	equ	16	; not used(?)
ldmsg	equ	128	; load map/message ('$' terminated)
recs	equ	256	; records to load, top-down

; Usage: NETBOOT [nid [args...]]

	; RomWBW load address for us is 0100h
	cseg
	lxi	sp,nbstk
	call	crlf
	lxi	h,descr
	call	msgout
	lxi	h,signon
	call	msgout
if WIZNET
	call	wizcfg
	lxi	h,wizerr
	cc	msgout
endif
	jmp	boot

error:
	lxi	h,neterr
err0:
	call	msgout
	lxi	b,reboot
	call	system
	; NOTREACHED

syntax:
	lxi	h,netsyn
	jr	err0

	dseg
descr:	db	'MT011 WizNET$'
signon:	db	' Network Boot',CR,LF,'$'
netsyn:	db	CR,LF,BEL,'Netboot syntax error',CR,LF,'$'
neterr:	db	CR,LF,BEL,'Network boot error',CR,LF,'$'
if WIZNET
wizerr:	db	CR,LF,'No NVRAM confiuration$'
endif

	cseg
boot:
	; make certain line is NUL-terminated.
	lxi	h,cmdlin	; already NUL terminated, but
				; need to skip 'N'...
bn6:	mov	a,m
	ora	a
	jrz	bn8	; A=0 (can't happen?)
	inx	h
	cpi	' '
	jrz	bn6
	ani	01011111b	; toupper
	cpi	'N'
	jrnz	bn6
	xchg	; cmdlin ptr to DE
	call	getaddr ;get server ID, ignore extra MSDs
	jc	syntax	; error if invalid
	bit	7,b	;test for no entry
	mvi	a,0
	jrnz	bn8	;use 00
	mov	a,l
bn8:	sta	boot$server
	lxi	h,msgbuf+DAT
	mvi	m,0	; len, re-set later
	inx	h
	mvi	c,1	; incl. len and NUL
	; skip any leading blanks
bn1:	call	char
	jrz	bn2	; no string present
	cpi	' '
	jrz	bn1
bn0:	mov	m,a
	inx	h
	inr	c
	call	char
	jrnz	bn0	; copy whole string... can't exceed 128?
bn2:	mvi	m,0	; NUL term
	mov	a,c	; SIZ incl NUL
	sta	msgbuf+SIZ
	dcr	a
	sta	msgbuf+DAT
	mvi	a,1
	sta	msgbuf+FNC
	; msgbuf now setup for boot request.
	call	NTWKIN
	ora	a
	jnz	error
loop:
	lda	boot$server
	sta	msgbuf+DID
	lda	CFGTBL+1	;client$id
	sta	msgbuf+SID
	mvi	a,0xb0
	sta	msgbuf+FMT
	call	netsr	; send request, receive response
	jc	error	; network error
	lda	msgbuf+FMT
	cpi	0xb1
	jnz	error	; invalid response
	lda	msgbuf+FNC
	ora	a
	jz	error	; NAK
	dcr	a
	jrz	ldtxt
	dcr	a
	jrz	stdma
	dcr	a
	jrz	load
	dcr	a
	jnz	error	; unsupported function
	; done - execute boot code
	call	crlf
	lhld	msgbuf+DAT
	pchl	; jump to code...
load:	lhld	dma
	xchg
	lxi	h,msgbuf+DAT
	lxi	b,128
	ldir
	xchg
	shld	dma
netack:
	xra	a
	sta	msgbuf+FNC
	sta	msgbuf+SIZ
	jr	loop
stdma:
	lhld	msgbuf+DAT
	shld	dma
	jr	netack
ldtxt:
	call	crlf
	lxi	h,msgbuf+DAT
	call	msgout
	jr	netack

; Network Send/Receive
netsr:
	lxi	b,msgbuf
	call	SNDMSG
	ora	a
	jrnz	netsre
	lxi	b,msgbuf
	call	RCVMSG
	ora	a
	rz
netsre:	stc
	ret

; Get next character from NUL-terminated line buffer (DE).
char:	ldax	d
	ora	a
	rz
	inx	d
	ret

; Get HEX value from line buffer
; Return: CY=error, HL=value, bit7(B)=1 if no input
getaddr:		;extract address from line buffer (dilimitted by " ")
	setb	7,b	;flag to detect no address entered
	lxi	h,0
ga2:	call	char
	rz		;end of buffer/line before a character was found
	cpi	' '	;skip all leading spaces
	jrnz	ga1	;if not space, then start getting HEX digits
	jr	ga2	;else if space, loop untill not space

ga0:	call	char
	rz
ga1:	call	hexcon	;start assembling digits into 16 bit accumilator
	jrc	chkdlm	;check if valid delimiter before returning error.
	res	7,b	;reset flag
	push	d	;save buffer pointer
	mov	e,a
	mvi	d,0
	dad	h	;shift "accumilator" left 1 digit
	dad	h
	dad	h
	dad	h
	dad	d	;add in new digit
	pop	d	;restore buffer pointer
	jr	ga0	;loop for next digit

chkdlm: cpi	' '	;blank is currently the only valid delimiter
	rz
	stc
	ret

hexcon: 		;convert ASCII character to HEX digit
	cpi	'0'	;must be .GE. "0"
	rc
	cpi	'9'+1	;and be .LE. "9"
	jrc	ok0	;valid numeral.
	ani	01011111b	; toupper
	cpi	'A'	;or .GE. "A"
	rc
	cpi	'F'+1	;and .LE. "F"
	cmc
	rc		;return [CY] if not valid HEX digit
	sui	'A'-'9'-1	;convert letter
ok0:	sui	'0'	;convert (numeral) to 0-15 in (A)
	ret

crlf:	mvi	a,CR	;send Carriage-Return/Line-Feed to console
	call	conout
	mvi	a,LF
	jmp	conout

msgout:	mov	a,m	; BDOS func 9 style msgprt
	cpi	'$'
	rz
	call	conout
	inx	h
	jr	msgout

; TODO: need to make this work after OS clobbered...
conout:
	push	h
	push	d
	push	b
	mov	e,a
	lxi	b,fcono
	rst	1
	pop	b
	pop	d
	pop	h
	ret

	dseg
; variables to network boot CP/NOS
boot$server	ds	1
dma:		ds	2
msgbuf:		ds	5+256
		ds	256
nbstk:		ds	0

if WIZNET
nvbuf:		ds	512
endif

	end
