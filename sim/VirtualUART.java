// Copyright (c) 2019 Douglas Miller <durgadas311@gmail.com>

public interface VirtualUART {
	static final int SET_CTS = 0x001; // Settable and Readable
	static final int SET_DSR = 0x002; // Settable and Readable
	static final int SET_DCD = 0x004; // Settable and Readable
	static final int SET_RI  = 0x008; // Settable and Readable
	// TODO: need Sync pin as input?
	static final int GET_RTS = 0x010; // Readable
	static final int GET_DTR = 0x020; // Readable
	static final int GET_OT1 = 0x040; // Readable
	static final int GET_OT2 = 0x080; // Readable
	static final int GET_BREAK = 0x100; // e.g. Z80-SIO
	// External pins, e.g. INS8251
	static final int GET_SYN = 0x200; // Readable - Sync pin, if output?
	static final int GET_TXE = 0x400; // Readable - Tx Enabled
	static final int GET_RXR = 0x800; // Readable - Rx Data Ready
	static final int GET_ONLY = (GET_RTS | GET_DTR | GET_OT1 |
			GET_OT2 | GET_SYN | GET_TXE | GET_RXR | GET_BREAK);
	static final int GET_CHR = 0x8000; // flags output as modem ctrl chg
	int available();	// Num bytes available from UART Tx.
	int take();		// Get byte from UART Tx, possibly sleep.
	boolean ready();	// Can UART Rx accept byte without overrun?
	void put(int ch, boolean sleep);	// Put byte into UART Rx.
	void setModem(int mdm);	// Change Modem Control Lines in to UART.
	int getModem();		// Get all Modem Control Lines for UART.
	boolean attach(Object periph);
	void detach();		// Peripheral no longer usable
	String getPortId();	// for properties
	void attachDevice(SerialDevice io);	//
}
