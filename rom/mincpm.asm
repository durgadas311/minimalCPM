; ROM monitor/boot for Minimal CP/M System
VERN	equ	000h	; ROM version

; Memory map:
; 0 0000    ROM start
; 7 FFFF    absolute end of ROM
; 8 0000    RAM start
; F FFFF    absolute end of RAM

	maclib	z180

false	equ	0
true	equ	not false

usedma	equ	true

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
RegPC:	ds	2
ABUSS:	ds	2

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

cmdtab:
	; console commands
	db	'D' ! dw cmddmp	; Dump memory
	db	'G' ! dw cmdgo	; Go
	db	'S' ! dw cmdsub	; Substitute in memory
	db	'P' ! dw cmdpc	; Set PC
	db	'B' ! dw cmdboot; Boot
	db	'V' ! dw prtver	; Version of ROM
numcmd	equ	($-cmdtab)/3

; TODO: this needs a rewrite...
re$entry:

start:
	lxi	h,prompt
	call	msgout
	call	cmdin
	ani	11011111b	; toupper
	lxi	h,cmdtab
	mvi	b,numcmd
cmloop:
	cmp	m
	inx	h
	jrz	docmd
	inx	h
	inx	h
	djnz	cmloop
	; bad command...
	call	belout
	jmp	start

docmd:
	call	conout
	mov	c,m
	inx	h
	mov	h,m
	mov	l,c
icall:	pchl

swtrap:	; try to recover return address...
	pop	h
	shld	RegPC
	; TODO: print address, etc...
	jmp	start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PC command (set PC)
cmdpc:
	lxi	h,pcms
	call	msgout
	lhld	RegPC
	lxi	d,RegPC	; HL=PC, DE=adr to store
	call	inhexcr
	jrc	cmdpc0	; hex digit entered
	call	adrnl	; show current PC (HL)
	call	inhexcr	; get another char
	rnc	; CR entered, don't update value
cmdpc0:
	xchg	; HL=adr to store
cmdpc1:
	mvi	d,CR
	jmp	adrin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Go command
cmdgo:
	lxi	h,goms
	call	msgout
	lhld	RegPC
	lxi	d,RegPC	; HL=PC, DE=adr to store
	call	inhexcr
	cc	cmdpc1	; read HEX until CR, store in HL
	call	crlf
cmdgo0:
	lhld	RegPC
	pchl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cmdboot:
	lxi	h,bootms
	call	msgout	; complete (B)oot
boot0:
	call	conin
	cpi	CR
	jz	dfboot	; default boot, by phy drv...
	call	belout
	jr	boot0

dfboot:	; TODO...
	call	belout
	jr	start

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
	lxi	sp,0ffffh
	; for now, leave ROM in ROM...
	lxi	h,re$entry
	push	h
	call	coninit
	call	meminit
	lxi	h,signon
	call	msgout
	; save registers on stack, for debugger access...
	jmp	start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Substitute command
cmdsub:
	lxi	h,subms
	call	msgout
	lxi	h,ABUSS
	ora	a	; NC
	mvi	d,CR
	call	adrin
	xchg
cmdsub0:
	call	adrnl
	mov	a,m
	call	hexout
	call	spout
cmdsub1:
	call	hexin
	jrnc	cmdsub4
	cpi	CR
	jrz	cmdsub2
	cpi	'-'
	jrz	cmdsub3
	cpi	'.'
	rz
	call	belout
	jr	cmdsub1
cmdsub2:
	inx	h
	jr	cmdsub0
cmdsub3:
	call	conout
	dcx	h
	jr	cmdsub0
cmdsub4:
	mvi	m,000h
cmdsub5:
	call	conout
	call	hexbin
	rld
	call	inhexcr
	jrnc	cmdsub2
	jr	cmdsub5

inhexcr:
	call	conin
	cpi	CR
	rz
	call	hexchk
	cmc
	rc
	call	belout
	jr	inhexcr

belout:
	mvi	a,BEL
conout:
	push	psw
conot1:
	in0	a,stat
	ani	00000010b	; TDRE
	jrz	conot1
	pop	psw
	out0	a,tdr
	ret

; D=term char (e.g. '.' for Substitute)
; HL=location to store address
; CY=first digit in A
adrin:
	push	h	; adr to store value
	cnc	conin
	cmp	d	; no input?
	jz	adrin3
	lxi	h,0
	stc
adrin0:	cnc	conin
	call	hexchk
	jrc	adrin1
	call	conout
	call	hexbin
	dad	h
	dad	h
	dad	h
	dad	h
	ora	l
	mov	l,a
	jr	adrin0
adrin1:
	cmp	d
	jrz	adrin2
	call	belout
	ora	a
	jr	adrin0
adrin2:
	call	conout
	xchg
	pop	h
	mov	m,e
	inx	h
	mov	m,d
	ret

hexbin:
	sui	'9'+1
	jrnc	hexbi0
	adi	7
hexbi0:
	adi	3
	ret

hexin:
	call	conin
hexchk:
	cpi	'0'
	rc
	cpi	'9'+1
	cmc
	rnc
	cpi	'A'
	rc
	ani	05fh	; toupper
	cpi	'A'
	rc
	cpi	'F'+1
	cmc
	ret

; HL = adr to print
adrnl:
	call	crlf
adrout:
	mov	a,h
	call	hexout
	mov	a,l
	call	hexout
spout:
	mvi	a,' '
	jmp	conout

hexout:
	push	psw
	rlc
	rlc
	rlc
	rlc
	call	hexdig
	pop	psw
hexdig:
	ani	00fh
	adi	090h
	daa
	aci	040h
	daa
	jmp	conout

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

if use$dma
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
endif

adrin3:	pop	h
	mov	e,m
	inx	h
	mov	d,m
	ret

; initialize monitor memory variables
meminit:
	; Force known values
	lxi	h,0
	shld	ABUSS
	shld	RegPC
	ret

prompt:	db	CR,LF,'MinCPM'
	db	': ',TRM
bootms:	db	'oot ',TRM
goms:	db	'o ',TRM
subms:	db	'ubstitute ',TRM
pcms:	db	'rog Counter ',TRM
dmpms:	db	'ump ',TRM

waitcr:
	call	conin
	cpi	CR
	jrnz	waitcr
crlf:
	mvi	a,CR
	call	conout
	mvi	a,LF
	jmp	conout

msgout:
	mov	a,m
	ora	a
	rz
	call	conout
	inx	h
	jr	msgout

; called in the context of a command on console
conin:	in0	a,stat
	ani	10000000b	; RDRF
	jrz	conin
conin0:	in0	a,rdr
	ani	07fh
;	cpi	DEL	; DEL key restarts from anywhere?
;	jz	re$entry
	ret

; wait for command - console or keypad
cmdin:
	jmp	conin

; assume < 100
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
	mov	a,d
	call	conout
	pop	psw
	jmp	conout

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
	mvi	a,1111$1111b
	out0	a,mmu$cbar
	xra	a
	out0	a,mmu$cbr
	out0	a,mmu$bbr
	lxi	sp,0ffffh
	push	h
	mov	a,b
	ani	01111111b	; reset TRAP
	out0	a,itc
	lxi	h,trpms
	call	msgout
	pop	h
	call	adrout
	call	crlf
	jmp	init0

trpms:	db	CR,LF,'*** TRAP ',TRM

savram:	; TODO: implement this w/o DMAC?
	lxi	h,000h	; save from 00000h
	lxi	d,300h	; save into 30000h
	lxi	b,16*1024	; save all 16K
	call	dmacpy
	ret

linix:	mvi	m,0	; terminate buffer
	ret

; input a line from console, allow backspace
; HL=buffer (size 128)
; returns B=num chars, 128 max (never is 0c3h)
linin:
	mvi	b,0	; count chars
lini0	call	conin	; handles DEL (cancel)
	cpi	CR
	jrz	linix
	cpi	BS
	jrz	backup
	cpi	' '
	jrc	chrnak
	cpi	'~'+1
	jrnc	chrnak
chrok:	mov	m,a
	inx	h
	inr	b
	jm	chrovf	; 128 chars max
	call	conout
	jr	lini0
chrovf:	dcx	h
	dcr	b
chrnak:	mvi	a,BEL
	call	conout
	jr	lini0
backup:
	mov	a,b
	ora	a
	jrz	lini0
	dcr	b
	dcx	h
	mvi	a,BS
	call	conout
	mvi	a,' '
	call	conout
	mvi	a,BS
	call	conout
	jr	lini0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Dump command
cmddmp:
	lxi	h,dmpms
	call	msgout
	lxi	h,ABUSS
	ora	a	; NC
	mvi	d,CR
	call	adrin
	xchg	; HL=adr
	mvi	b,8	; 8 lines (one half page, 128 bytes)
dmp0:	push	b
	call	adrnl	; CR,LF,"AAAA " (HL=AAAA)
	push	h
	mvi	b,16
dmp1:	mov	a,m
	call	hexout
	call	spout
	inx	h
	djnz	dmp1
	pop	h
	mvi	b,16
dmp2:	mov	a,m
	cpi	' '
	jrc	dmp3
	cpi	'~'+1
	jrc	dmp4
dmp3:	mvi	a,'.'
dmp4:	call	conout
	inx	h
	djnz	dmp2
	pop	b
	djnz	dmp0
	shld	ABUSS
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Print ROM version command
prtver:
	lxi	h,versms
	call	msgout
	lxi	h,vernum
	call	msgout
	ret

versms:	db	'ersion ',TRM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

signon:	db	CR,LF,'MinCPM'
	db	' Monitor v'
vernum:	db	(VERN SHR 4)+'0','.',(VERN AND 0fh)+'0'
	db	CR,LF,TRM

	rept	2000h-$
	db	0ffh
	endm
if	($ <> 2000h)
	.error 'core ROM overrun'
endif

	end
