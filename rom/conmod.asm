; Header module for SNIOS over console

	extrn	last
	extrn	NTWKIN, NTWKST, CNFTBL, SNDMSG, RCVMSG, NTWKER, NTWKBT, NTWKDN

	cseg
first:
	db	'N','C'
	dw	last-first
	dw	descr-first

;	Jump vector for SNIOS entry points
	jmp	NTWKIN	; network initialization
	jmp	NTWKST	; network status
	jmp	CNFTBL	; return config table addr
	jmp	SNDMSG 	; send message on network
	jmp	RCVMSG	; receive message from network
	jmp	NTWKER	; network error
	jmp	NTWKBT	; network warm boot
	jmp	NTWKDN	; network shutdown - extension
	; nothing else in cseg here...
	; SNIOS config table must be here

	dseg
descr:	db	'console',0

	end
