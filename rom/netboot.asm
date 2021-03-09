; Runs under CP/M, takes over the console and network boots CP/NOS.

	maclib	z180

false	equ	0
true	equ	not false

	$*macro

; CP/M constants
retcpm	equ	0
cmdlin	equ	80h

CR	equ	13
LF	equ	10
CTLC	equ	3
BEL	equ	7
TAB	equ	9
BS	equ	8
ESC	equ	27
TRM	equ	0
DEL	equ	127

; Z180 ASCI0 registers - only enough for I/O
ctlb	equ	02h	; for CTS
stat	equ	04h
tdr	equ	06h
rdr	equ	08h

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

	; CP/M TPA... where we live
	org	00100h
	lxi	sp,nbstk
	call	init	; might impact error path return to CP/M...
	jmp	boot

error:
	lxi	h,neterr
	call	print
	; TODO: may require more to restore working system
	jmp	retcpm

neterr:	db	CR,LF,BEL,'Network boot error',CR,LF,'$'

boot:
	; make certain line is NUL-terminated.
	lxi	h,cmdlin
	mov	c,m
	mvi	b,0
	inx	h
	dad	b
	mvi	m,0

	lxi	d,cmdlin+1
bn7:	call	getaddr ;get server ID, ignore extra MSDs
	jc	error	; error if invalid
	bit	7,b	;test for no entry
	mvi	a,0
	jrnz	bn8	;use 00
	mov	a,l
bn8:	sta	boot$server
	push	d	; save line pointer
	mvi	a,0ffh	; we don't know yet...
	sta	client$id
	call	NTWKIN	; trashes msgbuf...
	pop	d
	ora	a
	jnz	error

	lxi	h,msgbuf+DAT
	mvi	m,0	; len, re-set later
	inx	h
	mvi	c,1	; incl. len and NUL
bn1:
	call	char
	jrz	bn2	; no string present
	cpi	' '
	jrz	bn1
bn0:	mov	m,a
	inx	h
	inr	c
	call	char
	jrz	bn2
	cpi	' '
	jrnz	bn0
bn2:	mvi	m,0
	mov	a,c	; SIZ incl NUL
	sta	msgbuf+SIZ
	dcr	a
	sta	msgbuf+DAT
	lda	boot$server
	sta	msgbuf+DID
	lda	client$id
	sta	msgbuf+SID
	mvi	a,1
	sta	msgbuf+FNC
loop:
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
	call	print
	jr	netack

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
	cpi	'A'	;or .GE. "A"
	rc
	cpi	'F'+1	;and .LE. "F"
	cmc
	rc		;return [CY] if not valid HEX digit
	sui	'A'-'9'-1	;convert letter
ok0:	sui	'0'	;convert (numeral) to 0-15 in (A)
	ret

crlf:	mvi	c,CR	;send Carriage-Return/Line-Feed to console
	call	conout
	mvi	c,LF
	jmp	conout

print:	mov	a,m	; BDOS func 9 style msgprt
	cpi	'$'
	rz
	mov	c,a
	call	conout
	inx	h
	jr	print

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO: manage this in platform-specific modules...
;	public	init,conout
;	; exported for SNIOS
;	public	recvbt,recvby,sendby
; Or:	include	chario.asm

; make certain the console interrupts are off...
init:
	di
	; The only bits that are writeable
	; are the ones we zero... but we'll
	; be kind.
	in0	a,stat
	ani	11110110b
	out0	a,stat
	; safe to EI now???
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
	
; Output char to console
; C=char
conout:
	mov	a,c
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; SNIOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	include	snios.asm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
; variables to network boot CP/NOS
boot$server	ds	1
retry$count:	ds	1
msg$adr:	ds	2
dma:		ds	2
CFGTBL		ds	0	; mimic CP/NET CFGTBL
network$status:	ds	1
client$id	ds	1
		ds	36	; not used
		ds	1	; not used
msgbuf:		ds	5+256
		ds	256
nbstk:		ds	0

	end
