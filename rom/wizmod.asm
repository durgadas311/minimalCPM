; Header module for WizNet SNIOS

	extrn	last
	extrn	NTWKIN, NTWKST, CNFTBL, SNDMSG, RCVMSG, NTWKER, NTWKBT, NTWKDN

	public	nvbuf	; for libwznet
	extrn	wizcfg	; from libwznet

conout	equ	0040h

CR	equ	13
LF	equ	10

	cseg
first:
	db	'n','W'
	dw	last-first
	dw	descr-first

;	Jump vector for SNIOS entry points
	jmp	wizinit	; network initialization
	jmp	NTWKST	; network status
	jmp	CNFTBL	; return config table addr
	jmp	SNDMSG 	; send message on network
	jmp	RCVMSG	; receive message from network
	jmp	NTWKER	; network error
	jmp	NTWKBT	; network warm boot
	jmp	NTWKDN	; network shutdown - extension
	; nothing else in cseg here...
	; SNIOS config table must be here

wizinit:
	; initialize W5500 from NVRAM, if present
	call	wizcfg
	lxi	h,wizerr
	cc	msgout
	jmp	NTWKIN

msgout:	mov	a,m
	ora	a
	rz
	inx	h
	mov	c,a
	call	conout
	jmp	msgout

	dseg
descr:	db	'MT011 WizNet',0
wizerr:	db	'No NVRAM confiuration',CR,LF,0
nvbuf:	ds	512

	end
