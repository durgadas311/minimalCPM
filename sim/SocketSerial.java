// Copyright (c) 2021 Douglas Miller <durgadas311@gmail.com>
// Socket, a.k.a. telnet, client

import java.util.Arrays;
import java.util.Vector;
import java.util.Properties;
import java.io.*;
import java.net.*;

public class SocketSerial implements SerialDevice, Runnable {
	VirtualUART uart;
	String host;
	int port;
	Socket conn;
	String dbg;
	InputStream inp;
	OutputStream out;
	boolean modem;

	public SocketSerial(Properties props, Vector<String> argv, VirtualUART uart) {
		this.uart = uart;
		if (argv.size() < 3 || argv.size() > 4) {
			System.err.format("SocketSerial: Invalid args\n");
			return;
		}
		modem = (argv.size() > 3 && argv.get(3).equalsIgnoreCase("modem"));
		host = argv.get(1);
		port = Integer.valueOf(argv.get(2));
		dbg = String.format("SocketSerial %s %d\n", host, port);
		InetAddress ia;
		try {
			if (host.length() == 0 || host.equals("localhost")) {
				ia = InetAddress.getLocalHost();
			} else {
				ia = InetAddress.getByName(host);
			}
			conn = new Socket(ia, port);
			conn.setKeepAlive(true);
			inp = conn.getInputStream();
			out = conn.getOutputStream();
		} catch (Exception ee) {
			ee.printStackTrace();
			return;
		}
		uart.attachDevice(this);
		if (!modem) {
			uart.setModem(VirtualUART.SET_CTS |
					VirtualUART.SET_DSR |
					VirtualUART.SET_DCD);
		}
		// TODO: support dynamic connection status...
		if (modem) {
			uart.setModem(VirtualUART.SET_CTS |
					VirtualUART.SET_DSR |
					VirtualUART.SET_DCD);
		}
		Thread t = new Thread(this);
		t.start();
	}

	// SerialDevice interface:
	//
	public void write(int b) {
		if (conn == null) {
			return;
		}
		try {
			out.write(b);
		} catch (Exception ee) {
			ee.printStackTrace();
			discon();
		}
	}

	// This should not be used...
	// We push received data from the thread...
	public int read() {
		if (conn == null) {
			return -1;
		}
		// prevent blocking? needed?
		try {
			if (inp.available() < 1) return 0;
			return inp.read();
		} catch (Exception ee) {
			return -1;
		}
	}

	// Not used...
	public int available() {
		if (conn == null) {
			return 0;
		}
		try {
			return inp.available();
		} catch (Exception ee) {
			return -1;
		}
	}

	public void rewind() {}

	// This must NOT call uart.setModem() (or me...)
	public void modemChange(VirtualUART me, int mdm) {
// We have no DTR from the ASCI...
//		if (modem && (mdm & VirtualUART.GET_DTR) == 0) {
//			System.err.format("SocketSerial: lost DTR\n");
//			discon();
//		}
	}
	public int dir() { return SerialDevice.DIR_OUT; }

	public String dumpDebug() {
		String ret = dbg;
		if (conn != null) {
			ret += String.format("Connected: %s %d\n", host, port);
		} else {
			ret += "DEAD.\n";
		}
		return ret;
	}
	/////////////////////////////

	private void discon() {
		if (conn == null) {
			return;
		}
		try {
			conn.close();
		} catch (Exception ee) {}
		conn = null;
		if (modem) {
			uart.setModem(0);
		}
	}

	// This thread reads socket and sends to UART
	// When disconnected, just quit.
	public void run() {
		int c;
		while (conn != null) {
			try {
				c = inp.read();
				if (c < 0) {
					System.err.format("SocketSerial: EOF on socket\n");
					discon();
					break;
				}
				uart.put(c, false);
			} catch (Exception ee) {
				ee.printStackTrace();
				discon();
				break;
			}
		}
		uart.attachDevice(null);
		uart.detach();
	}
}
