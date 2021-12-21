// Copyright (c) 2021 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.util.Properties;
import java.util.Random;
import java.io.*;
import java.net.*;
import java.awt.event.*;
import javax.swing.Timer;

public class MT011 implements IODevice {
	private String name;
	private int base;
	private int ctrlReg;
	private int stsReg;

	private static final int CTL_SCS0 = 0x08;
	private static final int CTL_SCS1 = 0x10;
	private static final int CTL_SCS2 = 0x20;

	private SPIDevice[] devs;
	private int[] ctlbits = new int[] {
		CTL_SCS0, CTL_SCS1, CTL_SCS2 };

	public MT011(Properties props, String id, int port, int irq,
				Interruptor intr) {
		name = id;
		base = port;
		String pfx = id.toLowerCase();
		String s = props.getProperty(pfx + "_port");
		if (s != null) {
			base = Integer.decode(s) & 0xff;
		}
		devs = new SPIDevice[3];
		s = props.getProperty(pfx + "_dev0");
		if (s != null) {
			if (s.matches("[Ww][Ii][Zz].*")) {
				devs[0] = new WIZ550io(props, s, irq, intr);
			} else {
				System.err.format("Unknown device \"%s\" on %s dev0\n",
										s, id);
			}
		}
		s = props.getProperty(pfx + "_dev2");
		if (s != null) {
			// TODO: FRAM
			// assume NVRAM chip of some sort...
			// typical, expected, value is "25LC512"
			devs[2] = new NVRAM(props, s);
		}
		s = props.getProperty(pfx + "_dev1");
		if (s != null) {
			if (s.matches("[Ss][Dd].*")) {
				devs[1] = new SDCard(props, s, 0, irq, intr);
			} else {
				System.err.format("Unknown device \"%s\" on %s dev2\n",
										s, id);
			}
		}

		// H8-SPI adapter has an LED for /SCS (any), and PWR.
		// But, adapter card is not visible on front panels.
		// led = lh.registerLED("H8-SPI", LED.Colors.RED);
		reset();
		System.err.format("%s Module at port %02x\n", name, base);
	}

	public void reset() {
		ctrlReg = 0;
		stsReg = 0;
		for (int x = 0; x < 3; ++x) {
			if (devs[x] != null) {
				devs[x].reset();
			}
		}
	}

	public int getBaseAddress() { return base; }
	public int getNumPorts() { return 3; }

	public int in(int port) {
		int val = 0;
		int off = port - base;
		switch (off) {
		case 0:
			val = snoopData();
			break;
		case 1:
			val = readData();
			break;
		case 2:
			break;
		}
		return val;
	}
	public void out(int port, int val) {
		int off = port - base;
		switch (off) {
		case 0:
			writeData(val);
			break;
		case 1:
			break;
		case 2:
			setControl(val);
			break;
		}
	}

	private void setControl(int val) {
		int diff = (ctrlReg ^ val);
		ctrlReg = val;
		for (int x = 0; x < 3; ++x) {
			if (devs[x] != null && (diff & ctlbits[x]) != 0) {
				devs[x].scs((ctrlReg & ctlbits[x]) != 0);
			}
		}
	}

	private int readData() {
		int val = 0;
		for (int x = 0; x < 3; ++x) {
			if (devs[x] != null) {
				if ((ctrlReg & ctlbits[x]) != 0) {
					val |= devs[x].miso();
				} else {
					devs[x].sclk(8);
				}
			}
		}
		return val;
	}

	private int snoopData() {
		int val = 0;
		for (int x = 0; x < 3; ++x) {
			if (devs[x] != null) {
				if ((ctrlReg & ctlbits[x]) != 0) {
					val |= devs[x].miso_();
				} else {
					devs[x].sclk(8);
				}
			}
		}
		return val;
	}

	private void writeData(int val) {
		for (int x = 0; x < 3; ++x) {
			if (devs[x] != null) {
				if ((ctrlReg & ctlbits[x]) != 0) {
					devs[x].mosi(val);
				} else {
					devs[x].sclk(8);
				}
			}
		}
	}

	public String getDeviceName() { return name; }

	public String dumpDebug() {
		String ret = String.format("%s Port %02x\n", name, base);
		for (int x = 0; x < 3; ++x) {
			if (devs[x] != null) {
				ret += '\n';
				ret += devs[x].dumpDebug();
			}
		}
		return ret;
	}
}
