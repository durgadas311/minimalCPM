; SNIOS character I/O for console (ROM)
	public	sendby,check,recvby,recvbt

check:	xra	a
	ret

sendby	equ	0049h
recvbt	equ	004ch
recvby	equ	004fh

	end
