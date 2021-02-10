; ROM monitor/boot for Minimal CP/M System
VERN	equ	001h	; ROM version

; Memory map:
; 0 0000    ROM start
; 7 FFFF    absolute end of ROM
; 8 0000    RAM start
; F FFFF    absolute end of RAM

	maclib	z180

false	equ	0
true	equ	not false

	$*macro

CR	equ	13
LF	equ	10
BEL	equ	7
TAB	equ	9
BS	equ	8
ESC	equ	27
TRM	equ	0
DEL	equ	127

; Z180 internal registers (I/O ports) - CCR
itc	equ	34h
mmu$cbr	equ	38h
mmu$bbr	equ	39h
mmu$cbar equ	3ah
sar0l	equ	20h
sar0h	equ	21h
sar0b	equ	22h
dar0l	equ	23h
dar0h	equ	24h
dar0b	equ	25h
bcr0l	equ	26h
bcr0h	equ	27h
dstat	equ	30h
dmode	equ	31h
dcntl	equ	32h

; ASCI registers
ctla	equ	00h
ctlb	equ	02h
stat	equ	04h
tdr	equ	06h
rdr	equ	08h
asxt	equ	12h

; RAM used...
	org	02000h
savstk:	ds	2
addr0:	ds	2
addr1:	ds	2
line:	ds	64

stack	equ	00000h	; stack at top of memory (wrapped)

; Start of ROM code
	org	00000h

	jmp	init	; must be JMP so Heath CP/M thinks we're an H89

	rept	0008h-$
	db	0ffh
	endm
rst1:	jmp	swtrap
	rept	0010h-$
	db	0ffh
	endm
rst2:	jmp	swtrap
	rept	0018h-$
	db	0ffh
	endm
rst3:	jmp	swtrap
	rept	0020h-$
	db	0ffh
	endm
rst4:	jmp	swtrap
	rept	0028h-$
	db	0ffh
	endm
rst5:	jmp	swtrap
	rept	0030h-$
	db	0ffh
	endm
rst6:	jmp	swtrap
	rept	0038h-$
	db	0ffh
	endm
rst7:	jmp	swtrap

	; NMI not a problem?

swt:	db	CR,LF,'*** RST ',TRM

swtrap:	; try to recover return address...
	pop	d	; should be caller of RST...
	lspd	savstk
	push	d	; not needed?
	lxi	h,swt
	call	msgout
	pop	d
	call	taddr
	call	crlf
	; TODO: print address, etc...
	jmp	debug

; ROM start point - initialize everything
; We know we have at least 64K RAM...
; At RESET, CBAR=1111$0000 and BBR=0 and CBR=0 so there is no RAM
init:
; Might arrive here from a TRAP...
; But, cannot use stack until we re-init MMU...
	in0	a,itc
	bit	7,a
	jnz	trap
init0:
	; map ROM at 0000-1FFF, RAM at 2000-FFFF (pa 80000)
	; Use CA0 for ROM (hard-coded to pa 00000),
	; and BA for RAM at pa 80000.
	mvi	a,1111$0010b	; ca at 0xF000, ba at 0x2000
	out0	a,mmu$cbar
	; both CBR and BBR are "0" - if got here via RESET
	mvi	a,80h	; RAM is at 80000
	out0	a,mmu$bbr
	out0	a,mmu$cbr
	; Now we have RAM for a stack...
	lxi	sp,stack
	sspd	savstk
	; for now, leave ROM in ROM...
	call	coninit
	call	meminit
	lxi	h,signon
	call	msgout
	; save registers on stack, for debugger access...
	jmp	debug

belout:
	mvi	c,BEL
; Output char to console
; C=char
conout:
conot1:
	in0	a,ctlb
	ani	00100000b	; /CTS
	jrnz	conot1
	in0	a,stat
	ani	00000010b	; TDRE
	jrz	conot1
	out0	c,tdr
	ret

; Based on 18.432MHz CPU clock... 115200 baud...
; ctla: MOD=8n1,  TE=1, RE=1
; ctlb: SS=div1, DR=0, PS=1
; stat: RIE=0, TIE=0 (no interrupts)
; asxt: BRG=0, X1=0
coninit:
	mvi	a,01100100b	; TE/RE, 8+n+1
	out0	a,ctla
	mvi	a,00100000b	; 16x, div10, TODO: speed settings
	out0	a,ctlb
	mvi	a,00000000b	; ...
	out0	a,stat
	mvi	a,01100110b	; DCD/CTS, BREAK enable
	out0	a,asxt
	; TODO: what else...
	ret

; DMA 00000-02000 into 80000-82000
; copy core ROM (8K) into 0000 using DMAC
dmarom:
	lxi	h,0000h	; page addr (256B)
	lxi	d,0800h	; page addr (256B)
	lxi	b,2000h	; bytes
; Generic memcpy using DMAC.
; HL=src, DE=dst, all units 256B "pages".
; BC=count, units are bytes
dmacpy:
	xra	a
	out0	a,dar0l	; (256B page boundary)
	out0	e,dar0h ;
	out0	d,dar0b	; dest addr
	out0	a,sar0l	; (256B page boundary)
	out0	l,sar0h ;
	out0	h,sar0b	; source addr
	out0	c,bcr0l	;
	out0	b,bcr0h	; byte count
	mvi	a,00000010b	; mem2mem, burst mode
	out0	a,dmode
	mvi	a,01100000b	; DE0,/DWE0(!/DWE1) - start ch 0
	out0	a,dstat
	mvi	c,dstat
init1:	tstio	01000000b	; wait for DMAC to idle
	jrnz	init1
	ret

adrin3:	pop	h
	mov	e,m
	inx	h
	mov	d,m
	ret

; initialize monitor memory variables
meminit:
	; Force known values in RAM...
	ret

prompt:	db	CR,LF,': ',TRM

; Get char from console
; Returns: A=char, stripped
conin:	in0	a,stat
	ani	10000000b	; RDRF
	jrz	conin
conin0:	in0	a,rdr
	ani	07fh
	ret

; TODO: preserve CPU regs for debug/front-panel
; (by the time we reach intsetup, everything is trashed)
trap:
	pop	h
	dcx	h
	bit	6,a
	jrz	trap0
	dcx	h
trap0:
	mov	b,a
	mvi	a,1111$0010b	; ca at 0xF000, ba at 0x2000
	out0	a,mmu$cbar
	mvi	a,80h	; RAM is at 80000
	out0	a,mmu$bbr
	out0	a,mmu$cbr
	lxi	sp,stack
	push	h
	mov	a,b
	ani	01111111b	; reset TRAP
	out0	a,itc
	lxi	h,trpms
	call	msgout
	pop	d
	call	taddr
	call	crlf
	jmp	init0

trpms:	db	CR,LF,'*** TRAP ',TRM

savram:	; TODO: implement this w/o DMAC?
	lxi	h,000h	; save from 00000h
	lxi	d,300h	; save into 30000h
	lxi	b,16*1024	; save all 16K
	call	dmacpy
	ret

signon:	db	CR,LF,'Minimal System Monitor v'
vernum:	db	(VERN SHR 4)+'0','.',(VERN AND 0fh)+'0'
	db	CR,LF,TRM

errm:	db	CR,LF,BEL,'?',TRM

*********************************************************
**  Debug mode
*********************************************************

debug:
cilp:	lspd	savstk
	lxi	h,cilp		;setup return address
	push	h
	lxi	h,prompt	;prompt for a command
	call	msgout
	call	linein		;wait for command line to be entered
	lxi	d,line
	call	char		;get first character
	rz			;ignore line if it is empty
	lxi	h,comnds	;search table for command character
	mvi	b,ncmnds	;(number of commands)
cci0:	cmp	m		;search command table
	inx	h
	jrz	gotocmd		;command was found, execute it
	inx	h		;step past routine address
	inx	h
	djnz	cci0		;loop untill all valid commands are checked
error	lxi	h,errm		;if command unknown, beep and re-prompt
	jmp	msgout

gotocmd:
	push	d		;save command line buffer pointer
	mov	e,m		;get command routine address
	inx	h
	mov	d,m		;DE = routine address
	xchg			;HL = routine address
	pop	d		;restore buffer pointer
	pchl			;jump to command routine

comnds:
	db	'?'
	dw	Qcomnd
	db	'D'
	dw	Dcomnd
	db	'S'
	dw	Scomnd
	db	'G'
	dw	Gcomnd
	db	'M'
	dw	Mcomnd
	db	'F'
	dw	Fcomnd
	db	'I'
	dw	Icomnd
	db	'O'
	dw	Ocomnd
	db	'V'
	dw	Vcomnd
ncmnds	equ	($-comnds)/3

*********************************************************
**  Command subroutines
*********************************************************

menu:	db	CR,LF,'D <start> <end> - display memory in HEX'
	db	CR,LF,'S <start> - set/view memory'
	db	CR,LF,'G <start> - go to address'
	db	CR,LF,'F <start> <end> <data> - fill memory'
	db	CR,LF,'M <start> <end> <dest> - Move data'
	db	CR,LF,'I <port> - Input from port'
	db	CR,LF,'O <port> <value> - Output to port'
	db	CR,LF,'V - Show ROM version'
	db	0

Qcomnd:	lxi	h,menu
	jmp	msgout

Mcomnd:	call	getaddr
	jc	error
	bit	7,b
	jnz	error
	shld	addr0
	call	getaddr
	jc	error
	bit	7,b
	jnz	error
	shld	addr1
	call	getaddr
	jc	error
	bit	7,b
	jnz	error
	xchg
	lbcd	addr0
	lhld	addr1
	ora	a
	dsbc	b
	jc	error
	inx	h
	mov	c,l
	mov	b,h
	push	d
	xchg
	dad	b
	pop	d
	jc	error
	lhld	addr1
	call	check
	jc	mc0
	lhld	addr0
	call	check
	jnc	mc0
	lhld	addr1
	xchg
	dad	b
	dcx	h
	xchg
	lddr
	ret
mc0:	lhld	addr0
	ldir
	ret
Fcomnd:
	call	getaddr ;get address to start at
	jc	error	;error if non-hex character
	bit	7,b	;test for no address (different from 0000)
	jnz	error	;error if no address was entered
	shld	addr0	;save starting address
	call	getaddr ;get stop address
	jc	error	;error if non-hex character
	bit	7,b	;test for no entry
	jnz	error	;error if no stop address
	shld	addr1	;save stop address
	call	getaddr ;get fill data
	jc	error	;error if non-hex character
	bit	7,b	;test for no entry
	jnz	error	;error if no fill data
	mov	a,h
	ora	a
	jnz	error
	mov	c,l	;(C)=fill data
	lhld	addr1	;get stop address
	lded	addr0	;get start address
fc0:	mov	a,c	;
	stax	d	;put byte in memory
	inx	d	;step to next byte
	mov	a,d	;
	ora	e	;if we reach 0000, stop. (don't wrap around)
	rz		;
	call	check	;test for past stop address
	rc	;quit if past stop address
	jr	fc0

Dcomnd:		;display memory
	call	getaddr ;get address to start at
	jc	error	;error if non-hex character
	bit	7,b	;test for no address (different from 0000)
	jnz	error	;error if no address was entered
	shld	addr0	;save starting address
	call	getaddr ;get stop address
	jc	error	;error if non-hex character
	bit	7,b	;test for no entry
	jnz	error	;error if no stop address
	lded	addr0	;get start address into (DE)
dis0:	call	crlf	;start on new line
	call	taddr	;print current address
	call	space	;delimit it from data
	mvi	b,16	;display 16 bytes on each line
dis1:	ldax	d	;get byte to display
	inx	d	;step to next byte
	call	hexout	;display this byte in HEX
	call	space	;delimit it from others
	mov	a,d
	ora	e	;if we reach 0000, stop. (don't wrap around)
	jrz	dis2
	call	check	;test for past stop address
	jrc	dis2	;quit if past stop address
	djnz	dis1	;else do next byte on this line
dis2:	call	space	;delimit it from data
	call	space
	lded	addr0
	mvi	b,16	;display 16 bytes on each line
dis3:	ldax	d	;get byte to display
	inx	d	;step to next byte
	mvi	c,'.'
	cpi	' '
	jrc	dis4
	cpi	'~'+1
	jrnc	dis4
	mov	c,a
dis4:	call	conout
	mov	a,d
	ora	e	;if we reach 0000, stop. (don't wrap around)
	rz
	call	check	;test for past stop address
	rc	;quit if past stop address
	djnz	dis3	;else do next byte on this line
	sded	addr0
	jr	dis0	;when line is finished, start another

Scomnd: 		;substitute (set) memory
	call	getaddr ;get address to start substitution at
	jc	error	;error if non-hex character
	bit	7,b	;test for no entry
	jnz	error	;error if no address
	xchg		;put address in (DE)
sb1:	call	crlf	;start on new line
	call	taddr	;print address
	call	space	;and delimit it
	ldax	d	;get current value of byte
	call	hexout	;and display it
	call	space	;delimit it from user's (posible) entry
	mvi	b,0	;zero accumilator for user's entry
sb2:	call	conin	;get user's first character
	cpi	CR	;if CR then skip to next byte
	jrz	foward
	cpi	' '	;or if Space then skip to next
	jrz	foward
	cpi	'-'	;if Minus then step back to previous address
	jrz	bakwrd
	cpi	'.'	;if Period then stop substitution
	rz
	call	hexcon	;if none of the above, should be HEX digit
	jrc	error0	;error if not
	jr	sb3	;start accumilating HEX digits
sb0:	call	hexcon	;test for HEX digit
	jrc	error1	;error if not HEX
sb3:	slar	b	;roll accumilator to receive new digit
	slar	b
	slar	b
	slar	b
	ora	b	;merge in new digit
	mov	b,a
sb4:	call	conin	;get next character
	cpi	CR	;if CR then put existing byte into memory
	jrz	putbyte ;  and step to next.
	cpi	'.'
	rz
	cpi	del	;if DEL then restart at same address
	jrz	sb1
	jr	sb0	;else continue entering hex digits
putbyte:
	mov	a,b	;store accumilated byte in memory
	stax	d
foward:
	inx	d	;step to next location
	jr	sb1	;and allow substitution there

bakwrd:
	dcx	d	;move address backward one location
	jr	sb1

error0:	call	belout	;user's entry was not valid, beep and continue
	jr	sb2
error1:	call	belout	;same as above but for different section of routine
	jr	sb4

Gcomnd: 		;jump to address given by user
	call	getaddr ;get address to jump to
	jc	error	;error if non-hex character
	bit	7,b	;test for no entry
	jnz	error	;error if no address entered
	call	crlf	;on new line,
	mvi	c,'G'	;display "GO aaaa?" to ask
	call	conout	;user to verify that we should
	mvi	c,'O'	;jump to this address (in case user
	call	conout	;made a mistake we should not blindly
	call	space	;commit suicide)
	xchg
	call	taddr
	call	space
	mvi	c,'?'
	call	conout
	call	conin	;wait for user to type "Y" to
	cpi	'Y'	;indicate that we should jump.
	rnz		;abort if response was not "Y"
	xchg
	pchl		;else jump to address

inpms:	db	CR,LF,'Input ',TRM
Icomnd:
	call	getaddr ;get port address, ignore extra MSDs
	jc	error	;error if non-hex character
	bit	7,b	;test for no entry
	jnz	error	;error if no address entered
	push	h	; save port
	lxi	h,inpms
	call	msgout
	pop	h
	push	h
	mov	a,l
	call	hexout
	call	space
	mvi	c,'='
	call	conout
	call	space
	pop	b	; port to BC
	mvi	b,0	; safety
	inp	a
	call	hexout
	call	crlf
	ret

; TODO: no feedback?
Ocomnd:
	call	getaddr ;get port address, ignore extra MSDs
	jc	error	;error if non-hex character
	bit	7,b	;test for no entry
	jnz	error	;error if no address entered
	push	h	; save port
	call	getaddr ;get value, ignore extra MSDs
	jc	error	;error if non-hex character
	bit	7,b	;test for no entry
	jnz	error	;error if no value entered
	call	crlf
	pop	b	; port
	mvi	b,0	; safety
	outp	l
	ret

versms:	db	'Version ',TRM

Vcomnd:
	lxi	h,versms
	call	msgout
	lxi	h,vernum
	jmp	msgout

*********************************************************
**  Utility subroutines
*********************************************************

taddr:	mov	a,d	;display (DE) at console in HEX
	call	hexout	;print HI byte in HEX
	mov	a,e	;now do LO byte
hexout:	push	psw	;output (A) to console in HEX
	rlc		;get HI digit in usable (LO) position
	rlc
	rlc
	rlc
	call	nible	;and display it
	pop	psw	;get LO digit back and display it
nible:	ani	00001111b	;display LO 4 bits of (A) in HEX
	adi	90h	;algorithm to convert 4-bits to ASCII
	daa
	aci	40h
	daa
	mov	c,a	;display ASCII digit
	jmp	conout

space:	mvi	c,' '	;send an ASCII blank to console
	jmp	conout

crlf:	mvi	c,CR	;send Carriage-Return/Line-Feed to console
	call	conout
	mvi	c,LF
	jmp	conout

msgout:	mov	a,m	;send string to console, terminated by 00
	ora	a
	rz
	rm
	mov	c,a
	call	conout
	inx	h
	jr	msgout

check:	push	h	;non-destuctive compare HL:DE
	ora	a
	dsbc	d
	pop	h
	ret

; Convert letters to upper-case
toupper:
	cpi	'a'
	rc
	cpi	'z'+1
	rnc
	ani	01011111b
	ret

; Read a line of text into 'line'
; End with CR, honor BS
; Reject all non-printing characters, force toupper
linein:	lxi	h,line	;get string of characters from console, ending in CR
li0:	call	conin	;get a character
	cpi	BS	;allow BackSpacing
	jrz	backup
	cpi	CR
	jrz	li1
	cpi	' '	;ignore other non-print
	jrc	li0
	call	toupper
	mov	m,a	;put character in line nuffer
	inx	h
	mov	c,a
	call	conout	; echo character
	mov	a,l	;else check for pending buffer overflow
	sui	line mod 256
	cpi	64
	rz		;stop if buffer full
	jr	li0	;if not full, keep getting characters

backup:	mov	a,l	;(destructive) BackSpacing
	cpi	line mod 256	;test if at beginning of line
	jrz	li0	;can't backspace past start of line
	mvi	c,bs	;output BS," ",BS to erase character on screen
	call	conout	;and put cursor back one position
	call	space
	mvi	c,bs
	call	conout
	dcx	h	;step buffer pointer back one
	jr	li0	;and continue to get characters

; End line input, A=CR
li1:	mov	m,a	; store CR in buffer
	mvi	c,CR	;display CR so user knows we got it
	jmp	conout	;then return to calling routine

; Get next character from line buffer.
; DE=current pointer within 'line'
; Returns: ZR=EOL else A=char
char:	mov	a,e	;remove a character from line buffer,
	sui	line mod 256	;testing for no more characters
	sui	64
	rz		;return [ZR] condition if at end of buffer
	ldax	d
	cpi	CR
	rz		;also return [ZR] if at end of line
	inx	d	;else step to next character
	ret		;and return [NZ]

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

; Print A to console as decimal (00-99)
decout:
	mvi	d,'0'
decot0:
	sui	10
	jc	decot1
	inr	d
	jmp	decot0
decot1:
	adi	10
	adi	'0'
	push	psw
	mov	c,d
	call	conout
	pop	psw
	mov	c,a
	jmp	conout

	rept	2000h-$
	db	0ffh
	endm
if	($ <> 2000h)
	.error 'core ROM overrun'
endif

	end
