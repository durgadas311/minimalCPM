// Copyright (c) 2020 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.util.Vector;
import java.util.Properties;
import java.io.*;
// See: https://fazecast.github.io/jSerialComm/
// Currently using: jSerialComm-2.6.2.jar
import com.fazecast.jSerialComm.*;

// Provides a conduit between virtual serial port and a TTY device.
// Intended use is as attached to virtual serial port in simulators,
// to provide access to a real TTY. Typical usage example (in config file):
//
// data_att = TtySerial tty=/dev/ttyUSB0,9600
//
// where 'data' is the virtual port name, depends on simulated platform's
// serial ports designations and choice of port.
//
// Requires invoking the simulator with jSerialComm.jar on the classpath,
// which also precludes using the "-jar" option. Typical invocation:
//
// java -cp <sim-jar>:<jSerialComm-jar> sim-class [sim-args...]

public class TtySerial implements SerialDevice, Runnable {
	VirtualUART uart;
	SerialPort comm;
	String tty = null;
	int baud = -1;
	InputStream inp;
	OutputStream out;
	int flowCtrl = SerialPort.FLOW_CONTROL_DISABLED;
	boolean modem = false;
	int mdms = -1;

	public TtySerial(Properties props, Vector<String> argv, VirtualUART uart) {
		this.uart = uart;
		for (String arg : argv) {
			if (arg.startsWith("tty=")) {
				String[] l = arg.substring(4).split(",");
				tty = l[0];
				if (l.length > 1) {
					baud = Integer.valueOf(l[1]);
				}
			} else if (arg.equalsIgnoreCase("modem")) {
				modem = true;
			} else if (arg.equalsIgnoreCase("rts/cts") ||
					arg.equalsIgnoreCase("cts/rts")) {
				flowCtrl = SerialPort.FLOW_CONTROL_RTS_ENABLED |
					SerialPort.FLOW_CONTROL_CTS_ENABLED;
			} else if (arg.equalsIgnoreCase("xon/xoff") ||
					arg.equalsIgnoreCase("xoff/xon")) {
				flowCtrl = SerialPort.FLOW_CONTROL_XONXOFF_IN_ENABLED |
					SerialPort.FLOW_CONTROL_XONXOFF_OUT_ENABLED;
			}
		}
		if (tty == null) {
			throw new RuntimeException("No valid TTY specified\n");
		}
		comm = getPort(tty, baud);
		if (comm == null) {
			throw new RuntimeException("TTY not found\n");
		}
		try {
			inp = comm.getInputStream();
			out = comm.getOutputStream();
		} catch (Exception ee) {
			throw ee;
		}
		uart.attachDevice(this);
		if (!modem) {
			uart.setModem(VirtualUART.SET_CTS |
					VirtualUART.SET_DSR |
					VirtualUART.SET_DCD);
		} else {
			updateModem();
		}
		Thread t = new Thread(this);
		t.start();
		if (modem) {
			t = new Thread(new ModemControl());
			t.start();
		}
	}

	private SerialPort getPort(String tty, int baud) {
		try {
			SerialPort serialPort = SerialPort.getCommPort(tty);
			if (serialPort == null) {
				return null;
			}
			// TODO: timeout values...
			if (!serialPort.openPort()) {
				return null;
			}
			if (baud > 0) {
				if (!serialPort.setComPortParameters(baud, 8,
							SerialPort.ONE_STOP_BIT,
							SerialPort.NO_PARITY)) {
					serialPort.closePort();
					return null;
				}
			}
			if (!serialPort.setComPortTimeouts(SerialPort.TIMEOUT_READ_BLOCKING,
					0, 0)) {
				serialPort.closePort();
				return null;
			}
			// TODO: always set this?
			if (flowCtrl != SerialPort.FLOW_CONTROL_DISABLED &&
					!serialPort.setFlowControl(flowCtrl)) {
				System.err.format("Failed to set flow control\n");
			}
			return serialPort;
		} catch (Exception ee) {
			ee.printStackTrace();
			return null;
		}
	}

	// SerialDevice interface:
	//
	public void write(int b) {
		try {
			out.write(b);
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
		if (!modem) return;
		if ((mdm & VirtualUART.GET_RTS) == 0) {
			comm.clearRTS();
		} else {
			comm.setRTS();
		}
		if ((mdm & VirtualUART.GET_DTR) == 0) {
			comm.clearDTR();
		} else {
			comm.setDTR();
		}
	}
	public int dir() { return SerialDevice.DIR_OUT; }
	public String dumpDebug() {
		String ret = String.format("%s: %3s %3s %3s %3s %3s %3s\n",
			tty,
			comm.getCTS() ? "CTS" : "cts",
			comm.getDSR() ? "DSR" : "dsr",
			comm.getDCD() ? "DCD" : "dcd",
			comm.getRI()  ? "RI"  : "ri",
			comm.getRTS() ? "RTS" : "rts",
			comm.getDTR() ? "DTR" : "dtr");
		return ret;
	}
	/////////////////////////////

	private void updateModem() {
		int mdm = 0;
		if (comm.getCTS()) {
			mdm |= VirtualUART.SET_CTS;
		}
		if (comm.getDSR()) {
			mdm |= VirtualUART.SET_DSR;
		}
		if (comm.getDCD()) {
			mdm |= VirtualUART.SET_DCD;
		}
		if (comm.getRI()) {
			mdm |= VirtualUART.SET_RI;
		}
		if ((mdms ^ mdm) != 0) {
			mdms = mdm;
			uart.setModem(mdm);
		}
	}

	class ModemControl implements Runnable {
		public void run() {
			while (comm != null) {
				updateModem();
				try {
					Thread.sleep(1); // 1mS too short? long?
				} catch (Exception ee) {}
			}
		}
	}

	private void discon() {
		comm.closePort();
		comm = null;
		if (modem) {
			uart.setModem(0);
		}
		uart.detach();
	}

	// This thread reads TTY and sends to UART
	// TODO: how to monitor CTS, DSR, DCD (and RI)?
	public void run() {
		int c;
		while (true) {
			try {
				c = inp.read();
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
