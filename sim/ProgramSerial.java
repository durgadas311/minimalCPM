// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.util.Vector;
import java.util.Properties;
import java.io.*;
import java.awt.Font;

public class ProgramSerial extends InputStream implements SerialDevice, Runnable {
	VirtualUART uart;
	RunProgram prog;
	private java.util.concurrent.LinkedBlockingDeque<Integer> fifo;

	public ProgramSerial(Properties props, Vector<String> argv, VirtualUART uart) {
		this.uart = uart;
		fifo = new java.util.concurrent.LinkedBlockingDeque<Integer>();
		// WARNING! destructive to caller's 'argv'!
		argv.removeElementAt(0);
		prog = new RunProgram(argv, this, true);
		if (prog.excp == null) {
			Thread t = new Thread(this);
			t.start();
			// TODO: allow special program codes to change?
			uart.attachDevice(this);
			uart.setModem(VirtualUART.SET_CTS | VirtualUART.SET_DSR);
		} else {
			prog.excp.printStackTrace();
		}
	}

	// interface SerialDevice
	public int dir() { return SerialDevice.DIR_OUT; }
	public void write(int b) {	// CPU is writing the serial data port
		fifo.add(b);	// TODO: limit buffering?
	}
	// these conflict with InputStream, and are not used for SerialDevice anyway.
	// int read() { return 0; } // CPU is reading the serial data port - not used
	// int available() { return 0; } // returns number available bytes on Rx (0/1) ('')
	public void rewind() {}	// If device supports it, restart stream
				// (e.g. rewind cassette tape)
	// bits a la VirtualUART get/setModem()
	public void modemChange(VirtualUART me, int mdm) {
		// For test purposes:
		//System.err.format("MODEM LINES %04x\n", mdm);
	}
	public String dumpDebug() {
		return "";
	}

	// InputStream interface
	public int read() {
		try {
			int c = fifo.take();
			if (c < 0) { // EOF
				uart.detach(); // will close() do this?
				return c;
			}
			return c;
		} catch (Exception ee) {
			// ee.printStackTrace();
			return -1;
		}
	}
	public int available() {
		return fifo.size();
	}
	public void close() {
		uart.detach();
	}

	// This thread reads program stdout and sends to UART
	public void run() {
		while (true) {
			try {
				// This probably needs to be throttled...
				int c = prog.proc.getInputStream().read();
				uart.put(c, true);
			} catch (Exception ee) {
				ee.printStackTrace();
				uart.detach(); // repetative?
				break;
			}
		}
	}
}
