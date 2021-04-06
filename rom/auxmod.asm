; Header module for SNIOS over auxiliary serial port

	extrn	last
	extrn	NTWKIN, NTWKST, CNFTBL, SNDMSG, RCVMSG, NTWKER, NTWKBT, NTWKDN
	extrn	descr	; from I/O personality module

	cseg
first:
	db	'n','A'
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

	end
