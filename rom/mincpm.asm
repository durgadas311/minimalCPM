; ROM monitor/boot for Minimal CP/M System.
; total size of ROM+modules must not exceed 32K
VERN	equ	012h	; ROM version

romsiz	equ	1000h	; minimum space for ROM

; Memory map:
; 0 0000    ROM start
; 7 FFFF    absolute end of ROM
; 8 0000    RAM start
; F FFFF    absolute end of RAM

	maclib	z180

false	equ	0
true	equ	not false

tdebug	equ	false

	include	config.lib

	$*macro

CR	equ	13
LF	equ	10
CTLC	equ	3
BEL	equ	7
TAB	equ	9
BS	equ	8
ESC	equ	27
TRM	equ	0
DEL	equ	127

; Z180 internal registers (I/O ports) - CCR
; TODO: allow for different offsets.
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
rcr	equ	36h
if z180s
cmr	equ	1eh
ccr	equ	1fh
endif

; ASCI0 registers
ctla	equ	00h
ctlb	equ	02h
stat	equ	04h
tdr	equ	06h
rdr	equ	08h
asxt	equ	12h
if z180s
astcl	equ	1ah
astch	equ	1bh
endif

; RAM used...
	org	02000h
savstk:	ds	2
addr0:	ds	2
addr1:	ds	2
line:	ds	64
; Network Boot
boot$server	ds	1
dma:		ds	2
msgbuf:		ds	5+256
		ds	256
nbstk:		ds	0

; network boot module entry points
	org	03000h
	ds	2	; identifiers
	ds	2	; length (offset of next)
	ds	2	; description offset
command:	; for command modules
NTWKIN:	ds	3	; for netboot (SNIOS) modules...
NTWKST:	ds	3
CNFTBL:	ds	3
SNDMSG:	ds	3
RCVMSG:	ds	3
NTWKER:	ds	3
NTWKBT:	ds	3
NTWKDN:	ds	3
CFGTBL:	ds	0
	ds	1	; network status byte
client$id:
	ds	1
	ds	36	; rest of table

; offsets in msgbuf
FMT	equ	0
DID	equ	1
SID	equ	2
FNC	equ	3
SIZ	equ	4
DAT	equ	5

stack	equ	00000h	; stack at top of memory (wrapped)

; Start of ROM code
	org	00000h

	jmp	init

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
	rept	0040h-$
	db	0ffh
	endm
; public entry points:
	jmp	conout	; 0040
	jmp	conin	; 0043 stripped
	jmp	conine	; 0046 toupper, echo
; for console SNIOS:
	jmp	sendby	; 0049
	jmp	recvbt	; 004c
	jmp	recvby	; 004f
	jmp	putcon	; 0052

	; NMI not a problem?

swt:	db	CR,LF,'*** RST ',TRM

swtrap:	; try to recover return address...
	pop	d	; should be caller of RST...
	lspd	savstk
	push	d	; not needed?
	lxi	h,swt
	call	msgprt
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
if z180s
	mvi	a,xtal	; includes clock divide bit...
	out0	a,ccr
if ovrclk
	; might depend on custom XTAL settings...
	mvi	a,10000000b
	out0	a,cmr
endif
endif
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
	; CPU init:
	xra	a	; refresh off... static RAM
	out	rcr
	; init/optimize memory WAIT...
	in0	a,dcntl
	ani	00$111111b	; erase current mem WAIT
	ori	mwait		; replace with desired
	out0	a,dcntl
	;
	call	coninit
	call	meminit
	lxi	h,signon
	call	msgprt
	; save registers on stack, for debugger access...
	jmp	debug

belout:
	mvi	c,BEL
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

; Based on 18.432MHz CPU clock... 115200 baud...
; ctla: MOD=8n1,  TE=1, RE=1
; ctlb: SS=div1, DR=0(16x), PS=1
; stat: RIE=0, TIE=0 (no interrupts)
; asxt: BRG=0(div10 if DR=0), X1=0
; TODO: does order matter?
coninit:
	; disable Tx/Rx until initialized
	mvi	a,00010100b	; dis Tx/Rx, 8+n+1, RTS off
	out0	a,ctla
	mvi	a,00000000b+asc$ps+asc$dr
	out0	a,ctlb
	mvi	a,00000000b	; ...
	out0	a,stat
	mvi	a,01100110b+asc$brg	; DCD/CTS, BREAK enable TODO: cts/dcd ena?
	out0	a,asxt
	; TODO: what else...
if z180s
if astc
	lxi	h,astc
	out0	l,astcl
	out0	h,astch
endif
endif
	mvi	a,01101100b	; TE/RE, 8+n+1, RTS on, EFR
	out0	a,ctla
	ret

; DMA 00000... into 80000... (8K)
; i.e. copy core ROM (8K) into RAM using DMAC
dmarom:
	mvi	h,00h	; src is ROM bank
	mvi	l,08h	; dst is RAM bank
	lxix	0000h	; 
	lxi	d,0000h	;
	lxi	b,2000h	; bytes
; Generic memcpy using DMAC.
; H=src hi, L=dst hi, IX=src lo, DE=dst lo, BC=count, units are bytes
; destroys DE
dmacpy:
	out0	e,dar0l	;
	out0	d,dar0h ;
	out0	l,dar0b	; dest addr
	pushix
	pop	d
	out0	e,sar0l	;
	out0	d,sar0h ;
	out0	h,sar0b	; source addr
	out0	c,bcr0l	;
	out0	b,bcr0h	; byte count
	mvi	a,00000010b	; mem2mem, burst mode
	out0	a,dmode
	mvi	a,01100000b	; DE0,/DWE0(!/DWE1) - start ch 0
	out0	a,dstat
	mvi	c,dstat
dmac1:	tstio	01000000b	; wait for DMAC to idle
	jrnz	dmac1
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
	in0	a,rdr
	ani	07fh
	ret

; Get char from console, toupper and echo
conine:
	call	conin
	call	toupper
	push	psw
	mov	c,a
	call	conout
	pop	psw
	ret

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
putcon:	; not used in ROM, just discard...
	ret

; TODO: preserve CPU regs for debug
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
	call	msgprt
	pop	d
	call	taddr
	call	crlf
	jmp	init0

trpms:	db	CR,LF,'*** TRAP ',TRM

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
	call	msgprt
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
	; check for added commands
	mov	b,a
	call	char
	jrz	error
	mov	c,a
	call	getmod
	jnc	command
error:	lxi	h,errm		;if command unknown, beep and re-prompt
	jmp	msgprt

gotocmd:
	push	d		;save command line buffer pointer
	mov	e,m		;get command routine address
	inx	h
	mov	d,m		;DE = routine address
	xchg			;HL = routine address
	pop	d		;restore buffer pointer
	pchl			;jump to command routine

; All commands are started with DE=next char in line buffer
comnds:
	db	'?'
	dw	Qcomnd
	db	'B'
	dw	Bcomnd
if tdebug
	db	'C'
	dw	Ccomnd
endif
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

menu:
	db	CR,LF,'D <start> <end> - display memory in HEX'
	db	CR,LF,'S <start> - set/view memory'
	db	CR,LF,'G <start> - go to address'
	db	CR,LF,'F <start> <end> <data> - fill memory'
	db	CR,LF,'M <start> <end> <dest> - Move data'
	db	CR,LF,'I <port> [num] - Input from port'
	db	CR,LF,'O <port> <value> [...] - Output to port'
	db	CR,LF,'V - Show ROM version'
	db	TRM

btcm:	db	' <sid> [tag] - network boot ',TRM

Qcomnd:
	call	netmnu	; may print nothing
	call	dskmnu	; may print nothing
	lxi	h,menu
	call	msgprt
	jmp	cmdmnu	; may print nothing

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
sb2:	call	conine	;get user's first character
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
sb4:	call	conine	;get next character
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
	call	conine	;wait for user to type "Y" to
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
	call	getaddr	; hex number of inputs to do
	jc	error
	bit	7,b
	jrz	ic0
	lxi	h,1
ic0:
	xthl		; save count
	push	h	; re-save port
	lxi	h,inpms
	call	msgprt
	pop	h
	push	h
	mov	a,l
	call	hexout
	call	space
	mvi	c,'='
	call	conout
	; "Input XX ="
	pop	b	; port to BC
	pop	h	; count to HL (L)
	mvi	h,16-3
	mvi	b,0	; safety
	push	b	; C gets trashed by conout
ic1:
	call	space
	pop	b
	push	b
	inp	a
	call	hexout
	dcr	l	; assume <= 256
	jrz	ic2
	dcr	h	; col count
	jrnz	ic1
	call	crlf
	mvi	h,16
	jr	ic1
ic2:
	pop	b	; fix stack
	call	crlf
	ret

; TODO: no feedback?
Ocomnd:
	call	getaddr ;get port address, ignore extra MSDs
	jc	error	;error if non-hex character
	bit	7,b	;test for no entry
	jnz	error	;error if no address entered
	mvi	h,0	; safety
	push	h	; save port
	call	getaddr ;get value, ignore extra MSDs
	jc	error	;error if non-hex character
	bit	7,b	;test for no entry
	jnz	error	;error if no value entered
	call	crlf
oc0:		; L has byte to output...
	pop	b	; port
	push	b
	outp	l
	call	getaddr ;get value, ignore extra MSDs
	jc	error	;error if non-hex character
			;NOTE: some output has been sent
	bit	7,b	;test for no entry
	jrz	oc0	;still more to send
	pop	h	; discard port
	ret

versms:	db	CR,LF,'Version ',TRM

Vcomnd:
	lxi	h,versms
	call	msgprt
	lxi	h,vernum
	jmp	msgprt

if tdebug
Ccomnd:
	call	crlf
	mvi	a,1	; reset/clear
cc0:	out	0ffh
	call	recvbt
	jrc	cc0	; A=0 from recvbt/CY
	ret
endif

; locate module(s) and execute callback
; IY=callback routine
; B=module class, C=module ID, '*'=wildcard
; NOTE: callback made with extended ROM mapped.
; Callback: IX=module base, return A=0 on success
; Returns CY if no module found
locmod:
	; must map-in more of ROM...
	in0	a,mmu$cbar
	push	psw
	mvi	a,1111$1000b	; ca at 0xF000, ba at 0x8000
	out0	a,mmu$cbar
	;
	lxix	modules
	mvi	l,0ffh	; not found
lm1:	ldx	e,+2
	ldx	d,+3	; DE = length (next module)
	ldx	a,+0
	cpi	0ffh	; end of modules?
	jrz	lm9
	mov	a,b
	cpi	'*'	; any
	jrz	lm2
	cpi	'@'	; [A-Z]
	jrz	lm4
	cmpx	+0
	jrnz	lm0
lm2:	mov	a,c
	cpi	'*'
	jrz	lm3
	cmpx	+1
	jrnz	lm0
lm3:	push	h
	push	d
	push	b
	call	calliy
	pop	b
	pop	d
	pop	h
	mov	l,a
lm0:	dadx	d
	jr	lm1
lm4:	ldx	a,+0
	ani	01100000b	; significant bits
	cpi	01000000b	; uppercase letter?
	jrz	lm2
	jr	lm0

calliy:	pciy

lm9:	; restore memory map before returning
	pop	psw
	out0	a,mmu$cbar
	mov	a,l
	ora	a
	rz	; success - found 1 (or more)
	stc
	ret

; print network boot command help based on modules
netmnu:
	mvi	b,'n'
	mvi	c,'*'	; all netboot modules
	lxiy	netprt
	jmp	locmod

netprt:
	call	crlf
	mvi	c,'B'
	call	conout
	ldx	c,+1	; boot char
	call	conout
	lxi	h,btcm
	call	msgprt
	ldx	e,+4
	ldx	d,+5	; DE = offset of description
	pushix
	pop	h
	dad	d
	jmp	msgprt	; A=0 on return

; print added command help based on modules
cmdmnu:
	mvi	b,'@'
	mvi	c,'*'	; all command modules
	lxiy	modprt
	jmp	locmod

; print current module description (help)
modprt:
	call	crlf
	ldx	e,+4
	ldx	d,+5	; DE = offset of description (help)
	pushix
	pop	h
	dad	d
	jmp	msgprt	; A=0 on return

; print disk boot command help based on modules
dskmnu:
	mvi	b,'d'
	mvi	c,'*'	; all disk boot modules
	lxiy	modprt
	jmp	locmod

; search for module and load it, C=char B=class
getmod:
	lxiy	lodmod
	call	locmod
	ret	; CY = not found

lodmod:	mvi	h,00h	; src from ROM bank
	mvi	l,08h	; dst is RAM bank
	lxi	d,3000h	; RAM dest
	ldx	c,+2
	ldx	b,+3
	; IX=module address
	call	dmacpy
	xra	a
	ret

trydsk:	mvi	b,'d'
	push	d
	call	getmod
	pop	d
	jc	error
	jmp	command

; Boot
Bcomnd:
	call	char
	jrnz	bn7
	mvi	a,'C'	; default to console?
bn7:
	mov	c,a
	push	d
	mvi	b,'n'
	call	getmod
	pop	d
	jc	trydsk
; Network boot - SNIOS is loaded...
	; transition to nbstk...
	pop	h
	lxi	sp,nbstk
	push	h
	call	getaddr ;get server ID, ignore extra MSDs
	jc	error	; error if invalid
	bit	7,b	;test for no entry
	mvi	a,0
	jrnz	bn8	;use 00
	mov	a,l
bn8:	sta	boot$server
	push	d	; save line pointer
	call	NTWKIN
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
	mvi	a,1
	sta	msgbuf+FNC
loop:
	lda	boot$server
	sta	msgbuf+DID
	lda	client$id
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

msgprt:	mov	a,m	;send string to console, terminated by 00
	ora	a
	rz
	mov	c,a
	call	conout
	inx	h
	jr	msgprt

print:	mov	a,m	; BDOS func 9 style msgprt
	cpi	'$'
	rz
	mov	c,a
	call	conout
	inx	h
	jr	print

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
	cpi	CTLC
	jrz	liZ
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

; Abort input
liZ:	mvi	c,'^'
	call	conout
	mvi	c,'C'
	call	conout
	pop	h	; always OK?
	ret		; return to caller's caller (main debug loop)

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

modules: ds	0	; modules appended here...

	end
