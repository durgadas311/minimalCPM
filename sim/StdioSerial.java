// Copyright (c) 2021 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.util.Vector;
import java.util.Properties;
import java.io.*;

// Provides a conduit between virtual serial port and stdio.
// Intended use is as attached to virtual serial port in simulators,
// to provide access to the program's stdio.
//
// data_att = StdioSerial
//
// where 'data' is the virtual port name, depends on simulated platform's
// serial ports designations and choice of port.
//

public class StdioSerial implements SerialDevice, Runnable {
	VirtualUART uart;

	public StdioSerial(Properties props, Vector<String> argv, VirtualUART uart) {
		this.uart = uart;
		uart.attachDevice(this);
		uart.setModem(VirtualUART.SET_CTS |
					VirtualUART.SET_DSR |
					VirtualUART.SET_DCD);
		Thread t = new Thread(this);
		t.start();
	}

	// SerialDevice interface:
	//
	public void write(int b) {
		try {
			System.out.write(b);
			System.out.flush();
		} catch (Exception ee) {
			//ee.printStackTrace();
			discon();
		}
	}

	// This should not be used...
	// We push received data from the thread...
	public int read() { return 0; }

	// Not used...
	public int available() { return 0; }

	public void rewind() {}

	// This must NOT call uart.setModem() (or me...)
	public void modemChange(VirtualUART me, int mdm) {
	}
	public int dir() { return SerialDevice.DIR_OUT; }
	public String dumpDebug() {
		String ret = "";
		return ret;
	}
	/////////////////////////////

	private void updateModem() {
	}

	private void discon() {
		uart.detach();
	}

	// This thread reads stdin and sends to UART
	public void run() {
		int c;
		while (true) {
			try {
				c = System.in.read();
				if (c < 0) {
					break;
				}
				uart.put(c, true);
			} catch (Exception ee) {
				break;
			}
		}
		discon();
	}
}
