// Copyright (c) 2021 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.util.Vector;
import java.util.Properties;
import java.io.*;
import java.util.concurrent.Semaphore;
import java.lang.reflect.Constructor;
import z80core.ComputerIO;

// No interrupts or DMA supported...
public class Z180ASCI implements ComputerIO {
	static final int fifoLimit = 10; // should never even exceed 2
	private String name = null;
	private BaseSystem sys;

	static final int CNTLA = 0;	// port after shift
	static final int CNTLB = 1;	// port after shift
	static final int STAT = 2;	// port after shift
	static final int TDR = 3;	// port after shift
	static final int RDR = 4;	// port after shift
	static final int ASEXT = 9;	// port after shift
	static final int TCL = 13;	// port after shift
	static final int TCH = 14;	// port after shift

	static final int ctla_st2 = 0x01;	// 2 stop bits (else 1)
	static final int ctla_pen = 0x02;	// parity enable
	static final int ctla_bt8 = 0x04;	// 8 bit data
	static final int ctla_mpbr = 0x08;	// R/O multiprocessor not supported
	static final int ctla_efr = 0x08;	// W/O error reset
	static final int ctla_rts = 0x10;	// /RTS - "0" is asserted (chan 0 only)
	static final int ctla_te = 0x20;	// Tx enable
	static final int ctla_re = 0x40;	// Rx enable
	static final int ctla_mpbt = 0x80;	// multiprocessor not supported

	static final int ctlb_baud = 0x07;	// clk ratio, or ext
	static final int ctlb_dr = 0x08;	// div16/div64
	static final int ctlb_peo = 0x10;	// even/odd parity
	static final int ctlb_cts = 0x20;	// R/O: /CTS - "0" is asserted
	static final int ctlb_ps = 0x20;	// W/O: PS (prescale) BRG
	static final int ctlb_mp = 0x40;	// multiprocessor not supported
	static final int ctlb_mpbt = 0x80;	// multiprocessor not supported

	static final int stat_tie = 0x01;	// (no interrupts)
	static final int stat_tdre = 0x02;	// Tx ready for char
	static final int stat_dcd = 0x04;	// /DCD - "0" is asserted (ch 0 only)
	static final int stat_rie = 0x08;	// no interrupts
	static final int stat_fe = 0x10;	// framing error
	static final int stat_pe = 0x20;	// parity error
	static final int stat_ovrn = 0x40;	// overrun
	static final int stat_rdrf = 0x80;	// Rx char available

	static final int asxt_sbrk = 0x01;	// send BREAK (if brke)
	static final int asxt_brk = 0x02;	// BREAK detected (if brke)
	static final int asxt_brke = 0x04;	// enable BREAK functions
	static final int asxt_brg = 0x08;	// div10/30 or 2x(ctlb_baud)
	static final int asxt_1x = 0x10;	// force 1X clock
	static final int asxt_ctse = 0x20;	// /CTS forces TDRE=0 (ch 0 only)
	static final int asxt_dcde = 0x40;	// /DCD halts Rx (ch 0 only)
	static final int asxt_rdrf = 0x80;	// 

	private Z180ASCIChan[] ports = new Z180ASCIChan[2];
	private boolean z180s;

	public Z180ASCI(Properties props, BaseSystem sys) {
		this.sys = sys;
		name = "Z180_ASCI";
		String s = props.getProperty("cpu");
		z180s = (s != null && s.equalsIgnoreCase("Z80S180"));
		ports[0] = new Z180ASCIChan(props, "asci0", 0, null);
		ports[1] = new Z180ASCIChan(props, "asci1", 1, ports[0]);
		reset();
	}

	///////////////////////////////
	/// Interfaces for IODevice ///
	public int inPort(int port) {
		int x;
		int p;
		if (port >= 0x1a) { // special case TC registers...
			// 1A,1B => [0] 0D,0E; 1C,1D => [1] 0D,0E
			x = (port & 0b00100) >> 2;
			p = (port & 0b01101) | 0b00100;
		} else {
			x = port & 1;
			p = port >> 1;
		}
		return ports[x].in(p);
	}

	public void outPort(int port, int val) {
		int x;
		int p;
		if (port >= 0x1a) { // special case TC registers...
			// 1A,1B => [0] 0D,0E; 1C,1D => [1] 0D,0E
			x = (port & 0b00100) >> 2;
			p = (port & 0b01101) | 0b00100;
		} else {
			x = port & 1;
			p = port >> 1;
		}
		ports[x].out(p, val);
	}

	public void reset() {
		ports[0].reset();
		ports[1].reset();
	}

	public String getDeviceName() {
		return name;
	}

	public VirtualUART port0() { return ports[0]; }
	public VirtualUART port1() { return ports[1]; }

	class Z180ASCIChan implements VirtualUART {
		private java.util.concurrent.LinkedBlockingDeque<Integer> fifo;
		private java.util.concurrent.LinkedBlockingDeque<Integer> fifi;

		private Object attObj;
		private boolean excl = true;
		private long lastTx = 0;
		private long lastRx = 0;
		private long nanoBaud = 0; // length of char in nanoseconds
		private int baud = 0;
		private int bits; // bits per character
		private boolean prescale; // PS bit in reg_ctlb (W/O)
		private int index;
		private Z180ASCIChan chA; // null on Ch A
		private int modem = 0;	// modem inputs, floating
		private Semaphore wait;

		private SerialDevice io = null;
		private boolean io_in;
		private boolean io_out;

		private int reg_ctla = 0;
		private int reg_ctlb = 0;
		private int reg_stat = 0;
		private int reg_asxt = 0;
		private int reg_tc = 0;	// Z80S180 only

		public Z180ASCIChan(Properties props, String pfx, int idx, Z180ASCIChan alt) {
			chA = alt;
			attObj = null;
			index = idx;
			wait = new Semaphore(0);
			fifo = new java.util.concurrent.LinkedBlockingDeque<Integer>();
			fifi = new java.util.concurrent.LinkedBlockingDeque<Integer>();
			String s = props.getProperty(pfx + "_att");
			if (s != null && s.length() > 1) {
				attachClass(props, s);
			}
		}

		private void attachClass(Properties props, String s) {
			String[] args = s.split("\\s");
			Vector<String> argv = new Vector<String>(Arrays.asList(args));
			// try to construct from class...
			try {
				Class<?> clazz = Class.forName(args[0]);
				Constructor<?> ctor = clazz.getConstructor(
						Properties.class,
						argv.getClass(),
						VirtualUART.class);
				// funky "new" avoids "argument type mismatch"...
				Object obj = ctor.newInstance(
						props,
						argv,
						(VirtualUART)this);
				if (attach(obj)) {
					System.err.format("%s-%c attached to \"%s\"\n",
						name, index + '0', s);
				} else {
					System.err.format("%s-%c failed to attached\n",
						name, index + '0');
				}
			} catch (Exception ee) {
				System.err.format("Invalid class in attachment: %s\n", s);
				return;
			}
		}

		private void recalcBaud() {
			bits = (reg_ctla & ctla_bt8) == 0 ? 7 : 8;
			bits += (reg_ctla & ctla_pen) == 0 ? 0 : 1;
			bits += (reg_ctla & ctla_st2) == 0 ? 1 : 2;
			bits += 1;	// always 1 start bit
			int ss = (reg_ctlb & ctlb_baud);
			int dr = ((reg_ctlb & ctlb_dr) != 0 ? 64 : 16);
			if (ss == 0b111) {
				int ck;
				if (index == 0) {
					ck = sys.extClock1();
				} else {
					ck = sys.extClock2();
				}
				// TODO: what does EXT mean for us?
				dr = 1; // TODO: fix this
				baud = ck / dr;
				if (baud == 0) {
					nanoBaud = (long)1000000000;
				} else {
					nanoBaud = ((long)1000000000 * bits) / baud;
				}
			} else {
				int ck = sys.cpuClock();
				int ps = (prescale ? 30 : 10); // TODO: always?
				if ((reg_asxt & asxt_1x) != 0) dr = 1;
				if ((reg_asxt & asxt_brg) == 0) {
					ss = 1 << ss;
				} else {
					// TODO: BRG=1 (only for Z80S180?)
					ss = (reg_tc + 2) * 2;
					ps = 1; // prescale bypassed?
				}
				baud = ck / ps / dr / ss;
				nanoBaud = ((long)1000000000 * bits) / baud;
			}
			//System.err.format("%s-%c: %d bits, %d baud, %d nS/char\n",
			//	name, index + '0', bits, baud, nanoBaud);
			//System.err.print(dumpDebug());
		}

		private void set_ctla(int val) {
			// TODO: implement parity... mask bits...
			reg_ctla = val & ~ctla_mpbr;
			if ((val & ctla_efr) != 0) {
				reg_stat &= ~(stat_fe | stat_pe | stat_ovrn);
			}
			// TODO: only if RTS changed?
			updateModemOut();
			recalcBaud();	// word size affects value
		}

		private void set_ctlb(int val) {
			// ctlb_ps and ctlb_cts are the same bit!
			prescale = ((val & ctlb_ps) != 0);
			reg_ctlb = (val & ~ctlb_ps) | (reg_ctlb & ctlb_cts);
			recalcBaud();	// PS and DR affect value
		}

		private void set_stat(int val) {
			reg_stat = (val & (stat_rie | stat_tie)) |
				(reg_stat & ~(stat_rie | stat_tie));
		}

		private void set_asxt(int val) {
			reg_asxt = val;
			recalcBaud();	// BRG and X1 affect value
		}

		private void set_tc(int val) {
			if (!z180s) return;
			reg_tc = val;
			recalcBaud();	// if BRG=1
		}

		// 'port' has been shifted! 0-4, 9
		public int in(int port) {
			int val = 0;
			switch (port) {
			case CNTLA:
				val = reg_ctla;
				break;
			case CNTLB:
				val = reg_ctlb;
				break;
			case STAT:
				long t = System.nanoTime();
				if (t - lastTx > nanoBaud) {
					if (io_out || fifo.size() < 2) {
						reg_stat |= stat_tdre;
						lastTx = t;
					}
				}
				// TODO: handle lastRx somehow
				val = reg_stat;
				break;
			case TDR:
				// TODO: need this?
				break;
			case RDR:
				synchronized (this) {
					if (fifi.size() > 0) {
						try {
							val = fifi.take();
						} catch (Exception ee) {}
						if (fifi.size() == 0) {
							reg_stat &= ~stat_rdrf;
							wait.release();
						}
					}
				}
				break;
			case ASEXT:
				val = reg_asxt;
				break;
			case TCL:
				val = reg_tc & 0xff;
				break;
			case TCH:
				val = (reg_tc >> 8) & 0xff;
				break;
			}
			return val;
		}

		public void out(int port, int val) {
			val &= 0xff; // necessary?
			switch (port) {
			case CNTLA:
				set_ctla(val);
				break;
			case CNTLB:
				set_ctlb(val);
				break;
			case STAT:
				set_stat(val);
				break;
			case TDR:
				lastTx = System.nanoTime();
				if (io_out) {
					io.write(val);
				}
				if (!io_out || !excl) {
					synchronized (this) {
						fifo.add(val);
					}
				}
				reg_stat &= ~stat_tdre;
				break;
			case RDR:
				// not allowed
				break;
			case ASEXT:
				set_asxt(val);
				break;
			case TCL:
				set_tc((reg_tc & 0xff00) | val);
				break;
			case TCH:
				set_tc((reg_tc & 0x00ff) | (val << 8));
				break;
			}
		}

		public void reset() {
			fifo.clear();
			fifi.clear();
			reg_ctla = ctla_rts; // RTS off by default
			reg_ctlb = ctlb_baud | ctlb_cts; // EXT clock, CTS off, by default
			reg_stat = stat_tdre | stat_dcd; // TDRE on, DCD off, by default
			reg_asxt = 0;
			reg_tc = 0;
			prescale = false;
			// TODO: update other bits?
			wait.release();
			updateModemForce();
			_setModem();
			recalcBaud();
		}

		////////////////////////////////////////////////////
		/// Interfaces for the virtual peripheral device ///
		public int available() {
			return fifo.size();
		}

		// Must sleep if nothing available...
		public int take() {
			try {
				int c = fifo.take(); // might sleep here...
//				// TODO: how does this work with baud rate?
//				if (fifo.size() == 0 || attObj == null) {
//					synchronized (this) {
//						reg_stat |= stat_tdre;
//					}
//				}
				return c;
			} catch(Exception ee) {
				// let caller do detach?
				return -1;
			}
		}

		public boolean ready() {
			// has computer taken last char?
			return (reg_stat & stat_rdrf) == 0;
		}

		public void put(int ch, boolean sleep) {
			// TODO: prevent infinite growth?
			// This must happen outside 'synchronized' block
			wait.drainPermits();
			while (sleep && attObj != null && !ready()) {
				try {
					wait.acquire();
				} catch (Exception ee) {}
			}
			if (attObj == null) {
				return;
			}
			synchronized (this) {
				fifi.add(ch & 0xff);
				lastRx = System.nanoTime();
				reg_stat |= stat_rdrf;
			}
		}
		public void setBaud(int baud) {
			// TODO: implement something.
			// Needed for SYNC modes.
		}

		private void _setModem() {
			// modem controls in registers are inverted...
			reg_ctlb |= ctlb_cts;	// assume OFF
			if ((modem & VirtualUART.SET_CTS) != 0) {
				reg_ctlb &= ~ctlb_cts; // set ON
			}
			reg_stat |= stat_dcd;	// assume OFF
			if ((modem & VirtualUART.SET_DCD) != 0) {
				reg_stat &= ~stat_dcd;	// set ON
			}
			// no one to notify if things change?
			//System.err.format("%c: _setModem() %04x ctlb=%02x stat=%02x\n",
			//	index + '0', modem, reg_ctlb, reg_stat);
		}

		public void setModem(int mdm) {
			//System.err.format("%c: setModem(%04x) start ctlb=%02x stat=%02x\n",
			//	index + '0', mdm, reg_ctlb, reg_stat);
			int nuw = 0;
			// This requires (ctlb_cts != stat_dcd)!
			if ((mdm & VirtualUART.SET_CTS) != 0) {
				nuw |= VirtualUART.SET_CTS;
			}
			if ((mdm & VirtualUART.SET_DCD) != 0) {
				nuw |= VirtualUART.SET_DCD;
			}
			int diff = (modem ^ nuw) & (VirtualUART.SET_CTS | VirtualUART.SET_CTS);
			modem &= ~(VirtualUART.SET_CTS | VirtualUART.SET_CTS);
			modem |= nuw;
			_setModem();
		}
		// For some reason, "synchronized" is required to ensure
		// we always return the latest values. Probably don't
		// need to mutex with data I/O, but this is easiest.
		public synchronized int getModem() {
			int mdm = 0;
			if ((reg_ctla & ctla_rts) == 0) {
				mdm |= VirtualUART.GET_RTS;
			}
			if ((reg_asxt & (asxt_brke | asxt_brk)) ==
						(asxt_brke | asxt_brk)) {
				mdm |= VirtualUART.GET_BREAK;
			}
			if ((modem & VirtualUART.SET_CTS) != 0) {
				mdm |= VirtualUART.SET_CTS;
			}
			if ((modem & VirtualUART.SET_DCD) != 0) {
				mdm |= VirtualUART.SET_DCD;
			}
			return mdm;
		}
		public boolean attach(Object periph) {
			if (attObj != null) {
				return false;
			}
			attObj = periph;
			return true;
		}
		public void detach() {
			System.err.format("%s-%c detaching peripheral\n",
						name, index + '0');
			attObj = null;
			io = null;
			io_in = false;
			io_out = false;
			wait.release();
		}
		public void attachDevice(SerialDevice io) {
			this.io = io;
			if (io == null) {
				io_in = false;
				io_out = false;
				return;
			}
			io_in = (io != null && (io.dir() & SerialDevice.DIR_IN) != 0);
			io_out = (io != null && (io.dir() & SerialDevice.DIR_OUT) != 0);
			updateModemForce();
		}
		public String getPortId() {
			return String.format("%s%c", name, index + '0');
		}

		// Force a changeModem() event.
		private void updateModemForce() {
			//System.err.format("%c: updateModemForce() %04x\n", index + '0', modem);
			modem = getModem() ^ VirtualUART.GET_ONLY; // force event?
			//System.err.format("%c: updateModemForce() modem=%04x\n", index + '0', modem);
			updateModemOut();
		}

		private void updateModemOut() {
			// only SerialDevice can handle these events...
			if (io == null) return;
			int mdm = modem & ~VirtualUART.GET_ONLY;
			if ((reg_ctla & ctla_rts) == 0) {
				mdm |= VirtualUART.GET_RTS;
			}
			if ((reg_asxt & (asxt_brke | asxt_sbrk)) ==
						(asxt_brke | asxt_sbrk)) {
				mdm |= VirtualUART.GET_BREAK;
			}
			int diff = (mdm ^ modem) & VirtualUART.GET_ONLY;
			//System.err.format("%c: updateModemOut() %04x <= %04x\n", index + '0', modem, mdm);
			modem = mdm;
			if (diff != 0) {
				io.modemChange(this, modem);
			}
		}

		public String getDeviceName() { return name; }

		public String dumpDebug() {
			String ret = new String();
			ret += String.format("--- %s-%c ---\n", name, index + '0');
			ret += String.format(" CNTLA = %02x CNTLB = %02x PS=%s\n",
				reg_ctla, reg_ctlb, prescale);
			ret += String.format("  STAT = %02x ASEXT = %02x\n",
				reg_stat, reg_asxt);
			if (z180s) {
				ret += String.format("  ASTC = %d\n", reg_tc);
			}
			ret += String.format(" %d bits, %d baud, %d nS/char\n",
						bits, baud, nanoBaud);
			if (io != null) {
				ret += io.dumpDebug();
			}
			return ret;
		}
	}

	public String dumpDebug() {
		String ret = new String();
		ret += ports[0].dumpDebug();
		ret += '\n';
		ret += ports[1].dumpDebug();
		return ret;
	}
}
