// Copyright (c) 2018 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.util.Properties;
import java.util.Random;
import java.util.Map;
import java.util.HashMap;
import java.io.*;
import java.net.*;
import java.awt.event.*;
import javax.swing.Timer;

// LISTEN support depends on knowing exactly how the W5500 behaves. TBD

public class WIZ550io implements SPIDevice, Runnable {
	private static final int nsocks = 8;

	private static final int STS_READY = 0x01;
	private static final int CTL_SCS = 0x01;

	// meta-commands for FIFO
	private static final int SW_RESET = -1;
	private static final int PHY_RESET = -2;
	private static final int NO_CMD = -3;

	private static final int MR = 0;
	private static final int GAR = 1;	// x4
	private static final int SUBR = 5;	// x4
	private static final int SHAR = 9;	// x6
	private static final int SIPR = 15;	// x4
	private static final int INTLEVEL = 19;	// x2 Intr LowLevel time
	private static final int IR = 21;
	private static final int IMR = 22;
	private static final int SIR = 23;
	private static final int SIMR = 24;
	private static final int RTR = 25;	// x2
	private static final int RCR = 27;
	private static final int PTIMER = 28;
	private static final int PMAGIC = 29;	// used for client ID
	private static final int PHAR = 30;	// x6
	private static final int PSID = 36;	// x2
	private static final int PMRU = 38;	// x2
	private static final int UIPR = 40;	// x4
	private static final int UPORT = 44;	// x2
	private static final int PHYCFG = 46;
	private static final int VERSIONR = 57;

	private static final int defRTR = 2000;
	private static final int defRCR = 8;
	private static final int defPTIMER = 40;
	private static final int defPMRU = 0xffff;
	private static final int defPHYCFG = 0b10111111;

	private static final int Sn_MR = 0;
	private static final int Sn_CR = 1;
	private static final int Sn_IR = 2;
	private static final int Sn_SR = 3;
	private static final int Sn_PORT = 4;	// x2
	private static final int Sn_DHAR = 6;	// x6
	private static final int Sn_DIPR = 12;	// x4
	private static final int Sn_DPORT = 16;	// x2
	private static final int Sn_MSSR = 18;	// x2
	private static final int Sn_TOS = 21;
	private static final int Sn_TTL = 22;
	private static final int Sn_RXBUF_SIZE = 30;
	private static final int Sn_TXBUF_SIZE = 31;
	private static final int Sn_TX_FSR = 32; // x2
	private static final int Sn_TX_RD = 34; // x2
	private static final int Sn_TX_WR = 36; // x2
	private static final int Sn_RX_RSR = 38; // x2
	private static final int Sn_RX_RD = 40; // x2
	private static final int Sn_RX_WR = 42; // x2
	private static final int Sn_IMR = 44;
	private static final int Sn_FRAG = 45;	// x2
	private static final int Sn_KPALVTR = 47;	// 1 = 5sec

	private static final int defTTL = 128;
	private static final int defDHAR = 0xff; // x6
	private static final int defFRAG = 16384;

	// Sn_MR values...
	private static final byte Sn_MR_MULTI = (byte)0x80;
	private static final byte Sn_MR_ND_MC = (byte)0x20;
	private static final byte Sn_MR_PROTO = (byte)0x0f;	// mask
	private static final byte Sn_MR_CLOSED = (byte)0x00;
	private static final byte Sn_MR_TCP = (byte)0x01;
	private static final byte Sn_MR_UDP = (byte)0x02;
	private static final byte Sn_MR_IPRAW = (byte)0x03;
	private static final byte Sn_MR_MACRAW = (byte)0x04;
	private static final byte Sn_MR_PPPOE = (byte)0x05;

	// Sn_CR values...
	private static final byte IDLE = (byte)0x00;
	private static final byte OPEN = (byte)0x01;
	private static final byte LISTEN = (byte)0x02;
	private static final byte CONNECT = (byte)0x04;
	private static final byte DISCON = (byte)0x08;
	private static final byte CLOSE = (byte)0x10;
	private static final byte SEND = (byte)0x20;
	private static final byte SEND_MAC = (byte)0x21;
	private static final byte SEND_KEEP = (byte)0x22;
	private static final byte RECV = (byte)0x40;

	// Sn_SR values...
	private static final byte SOCK_CLOSED = (byte)0x00;
	private static final byte SOCK_INIT = (byte)0x13;
	private static final byte SOCK_LISTEN = (byte)0x14;
	private static final byte SOCK_ESTABLISHED = (byte)0x17;
	private static final byte SOCK_CLOSE_WAIT = (byte)0x1c;
	private static final byte SOCK_UDP = (byte)0x22;
	private static final byte SOCK_MACRAW = (byte)0x42;
	private static final byte SOCK_SYNSENT = (byte)0x15;
	private static final byte SOCK_SYNRECV = (byte)0x16;
	private static final byte SOCK_FIN_WAIT = (byte)0x18;
	private static final byte SOCK_CLOSING = (byte)0x1a;
	private static final byte SOCK_TIME_WAIT = (byte)0x1b;
	private static final byte SOCK_LAST_ACK = (byte)0x1d;

	// Sn_IR bits...
	private static final byte Sn_IR_SENDOK = (byte)0x10;
	private static final byte Sn_IR_TIMEOUT = (byte)0x08;
	private static final byte Sn_IR_RECV = (byte)0x04;
	private static final byte Sn_IR_DISCON = (byte)0x02;
	private static final byte Sn_IR_CON = (byte)0x01;

	private String name;
	private int irq;
	private int src;
	private Interruptor intr;
	private int nconn = 0;

	private Socket[] socks;
	private InputStream[] socki;
	private OutputStream[] socko;
	private int[] txbase;
	private int[] txmask;
	private int[] rxbase;
	private int[] rxmask;
	private byte[] rxmem;
	private byte[] txmem;
	private byte[] com_regs;
	private byte[][] sock_regs;
	private byte[][] dipr;
	private byte[][] dport;
	private byte[] mac;
	private int[] lastCmd;
	private int ints;
	private int adr;
	private int ctl;
	private int spi;
	private int len;
	private int _miso;
	private int _mosi;
	private boolean ready;
	private java.util.concurrent.LinkedBlockingDeque<Integer> cmd;
	private Map<Integer,Listener> lstns;

	// JAVA/OS won't allow ports < 3000. Just set socket to failed/closed
	class Listener implements Runnable {
		private int port;
		private ServerSocket ssock;
		private boolean ready = false;
		public Listener(int port) {
			this.port = port;
			try {
				InetAddress ia;
				// TODO: what to use for 'ia'? Must be localhost?
				ia = InetAddress.getByName(null); // "localhost"
				ssock = new ServerSocket(port, 1, ia);
			} catch (Exception ee) {
				ee.printStackTrace();
				return;	// leave "not valid"
			}
			ready = true;
			Thread t = new Thread(this);
			t.start();
		}
		public synchronized boolean valid() { return ready; }
		public void run() {
			Socket s;
			//synchronized(this) { ready = true; }
			while (true) {
				try {
					s = ssock.accept();
				} catch (Exception ee) {
					//ee.printStackTrace();
					synchronized(this) { ready = false; }
					break;
				}
				boolean	found = false;
				for (int x = 0; x < nsocks; ++x) {
					if (sock_regs[x][Sn_SR] != SOCK_LISTEN) continue;
					if (sockGet16(x, Sn_PORT) != port) continue;
					try {
						setConn(x, s);
						found = true;
						Thread t = new Thread(new Receiver(x));
						t.start();
					} catch (Exception ee) {
						//ee.printStackTrace();
						doDisc(x);
						// TODO: continue search?
					}
					break;
				}
				if (!found) {
					try {
						s.close();
					} catch (Exception ee) {}
				}
			}
		}
	};

	public WIZ550io(Properties props, String id, int irq,
				Interruptor intr) {
		name = id;
		String pfx = id.toLowerCase().replaceFirst("io$", "");
		this.intr = intr;
		this.irq = irq;
		ints = 0;
		String s = props.getProperty(pfx + "_intr");
		if (s != null) {
			this.irq = Integer.decode(s) & 0xff;
		}
		src = intr.registerINT(irq);
		cmd = new java.util.concurrent.LinkedBlockingDeque<Integer>();
		lstns = new HashMap<Integer,Listener>();
		lastCmd = new int[nsocks];
		socks = new Socket[nsocks];
		socki = new InputStream[nsocks];
		socko = new OutputStream[nsocks];
		txbase = new int[nsocks];
		txmask = new int[nsocks];
		rxbase = new int[nsocks];
		rxmask = new int[nsocks];
		rxmem = new byte[16384];
		txmem = new byte[16384];
		com_regs = new byte[58];
		sock_regs = new byte[nsocks][];
		dipr = new byte[nsocks][];
		dport = new byte[nsocks][];
		mac = new byte[6];
		for (int x = 0; x < nsocks; ++x) {
			sock_regs[x] = new byte[48];
			dipr[x] = new byte[4];
			dport[x] = new byte[2];
		}
		Arrays.fill(mac, (byte)0xff);
		s = props.getProperty(pfx + "_mac");
		if (s != null) {
			int x = 0;
			for (String ss : s.split(":")) {
				if (x < mac.length) {
					int v = Integer.valueOf(ss, 16);
					mac[x++] = (byte)v;
				}
			}
		} else if (name.indexOf("550") >= 0) {
			// Only the WIZ550io comes with a MAC address
			generateMAC();
		}
		reset();
		Thread t = new Thread(this);
		t.start();
		System.err.format("%s Ethernet Module at irq %d\n", name, irq);
	}

	private void doSwReset() {
		adr = 0;
		ctl = 0;
		len = 0;
		spi = -1;
		_miso = 0;
		_mosi = 0;
		// Does the W5500 actually do all this on RESET?
		for (int x = 0; x < nsocks; ++x) {
			if (socks[x] != null) {
				try {
					socks[x].close();
				} catch (Exception ee) { }
				socks[x] = null;
			}
			Arrays.fill(dport[x], (byte)0);
			Arrays.fill(dipr[x], (byte)0);
			Arrays.fill(sock_regs[x], (byte)0);
			sock_regs[x][Sn_RXBUF_SIZE] = (byte)2;
			sock_regs[x][Sn_TXBUF_SIZE] = (byte)2;
			sock_regs[x][Sn_TTL] = (byte)defTTL;
			sock_regs[x][Sn_IMR] = (byte)0xff;
			sock_regs[x][Sn_FRAG] = (byte)(defFRAG >> 8);
			sock_regs[x][Sn_FRAG+1] = (byte)defFRAG;
			Arrays.fill(sock_regs[x], Sn_DHAR, Sn_DHAR + 6,
								(byte)defDHAR);
		}
		Arrays.fill(com_regs, (byte)0);
		com_regs[RTR] = (byte)(defRTR >> 8);
		com_regs[RTR+1] = (byte)defRTR;
		com_regs[RCR] = (byte)defRCR;
		com_regs[PTIMER] = (byte)defPTIMER;
		// TODO: simulate PHY reset, LINK UP timing
		com_regs[PHYCFG] = (byte)defPHYCFG;
		com_regs[PMRU] = (byte)(defPMRU >> 8);
		com_regs[PMRU+1] = (byte)defPMRU;
		com_regs[VERSIONR] = (byte)0x04;
		setBuff(Sn_RXBUF_SIZE, false);
		setBuff(Sn_TXBUF_SIZE, true);
	}

	public void reset() {
		// make a more pronounced delay before RDY
		// TODO: differences between h/w and s/w reset?
		ready = false;
		cmd.add(SW_RESET);
	}

	private void generateMAC() {
		int x = 0;
		mac[x++] = (byte)0x00;
		mac[x++] = (byte)0x08;
		mac[x++] = (byte)0xdc;
		Random random = new Random();
		int r = random.nextInt();
		mac[x++] = (byte)r;
		r >>= 8;
		mac[x++] = (byte)r;
		r >>= 8;
		mac[x++] = (byte)r;
	}

	private void chkIntr() {
		int i = (com_regs[IR] & com_regs[IMR]);
		i |= ((com_regs[SIR] & com_regs[SIMR]) << 8);
		if (i == ints) {
			return;
		}
		ints = i;
		if (ints != 0) {
			intr.raiseINT(irq, src);
		} else {
			intr.lowerINT(irq, src);
		}
	}

	private void chkIntr(int ix) {
		int iv = com_regs[SIR] & 0xff;
		int im = (1 << ix);
		if (sock_regs[ix][Sn_IR] != 0) {
			iv |= im;
		} else {
			iv &= ~im;
		}
		com_regs[SIR] = (byte)iv;
		chkIntr();
	}

	public void scs(boolean on) {
		if (on) {
			// start xfer
			spi = 0;
		} else {
			// terminate xfer
			spi = -1;
		}
	}

	public boolean status() { return ready; }

	public int miso_() {
		_miso = _mosi;
		return _miso;
	}

	public int miso() {
		if (spi < 0) {
			return 0xff;
		}
		int val = _miso;
		if (spi != 3 || (ctl & 0x04) != 0 || len <= 0) {
			// Write mode, ignore reads?
			// Full behavior unclear...
			_miso = _mosi;
		} else {
			_miso = read();
		}
		return val;
	}

	public void mosi(int val) {
		if (spi < 0) {
			return;
		}
		_miso = _mosi;
		_mosi = val;
		switch (spi) {
		case 0:
			adr &= 0x00ff;
			adr |= (val & 0x7f) << 8;
			++spi;
			return;
		case 1:
			adr &= 0xff00;
			adr |= (val & 0xff);
			++spi;
			return;
		case 2:
			ctl = val;
			switch (ctl & 0x03) {
			case 0:
				len = 65536;
				break;
			case 1:
				len = 1;
				break;
			case 2:
				len = 2;
				break;
			case 3:
				len = 4;
				break;
			}
			++spi;
			return;
		}
		if ((ctl & 0x04) == 0 || len <= 0) {
			// Read mode, ignore writes?
			return;
		}
		write(val);
	}

	public void sclk(int bits) {}

	private int read() {
		int val = 0;
		int ix = (ctl >> 5) & 0x07;
		int bk = (ctl >> 3) & 0x03;
		int a;
		switch (bk) {
		case 0:	// really only valid if ix==0
			if (adr >= com_regs.length) {
				break;
			}
			val = com_regs[adr] & 0xff;
			break;
		case 1:	// sock regs [ix]
			if (adr >= sock_regs[ix].length) {
				break;
			}
			if (adr >= Sn_DIPR && adr < Sn_DIPR + 4) {
				val = dipr[ix][adr - Sn_DIPR] & 0xff;
				break;
			}
			if (adr >= Sn_DPORT && adr < Sn_DPORT + 2) {
				val = dport[ix][adr - Sn_DPORT] & 0xff;
				break;
			}
			val = sock_regs[ix][adr] & 0xff;
			break;
		case 2:	// Tx buffer
			a = txbase[ix] + (adr & txmask[ix]);
			val = txmem[a] & 0xff;
			break;
		case 3:	// Rx buffer
			a = rxbase[ix] + (adr & rxmask[ix]);
			val = rxmem[a] & 0xff;
			break;
		}
		adr = (adr + 1) & 0xffff;
		if (len > 0) {
			--len;
		}
		return val;
	}

	private void write(int val) {
		int ix = (ctl >> 5) & 0x07;
		int bk = (ctl >> 3) & 0x03;
		int a;
		boolean reset = false;
		switch (bk) {
		case 0:	// really only valid if ix==0
			if (adr >= com_regs.length) {
				break;
			}
			if (adr == IR) {
				val &= 0xf0;
				com_regs[adr] &= ~val;
				com_regs[SIR] = 0;
				chkIntr();
				break;
			}
			if (adr == PHYCFG) {
				reset = ((com_regs[adr] & 0x80) != 0);
				val &= ~0x07;
				if (!reset) {
					val |= com_regs[adr] & 0x07;
				}
				com_regs[adr] = (byte)val;
				if ((val & 0x80) == 0) {
					com_regs[adr] &= ~0x07;
					// TODO: hold PHY in RESET...
				}
				if ((val & 0x80) != 0 && reset) {
					// end of PHY RESET
					cmd.add(PHY_RESET);
				}
				break;
			}
			com_regs[adr] = (byte)val;
			if (adr == IMR) {
				chkIntr();
			}
			if (adr == MR) {
				if ((val & 0x80) != 0) {
					ready = false;
					cmd.add(SW_RESET);
				}
			}
			break;
		case 1:	// sock regs [ix]
			if (adr >= sock_regs[ix].length) {
				break;
			}
			if (adr == Sn_IR) {
				sock_regs[ix][Sn_IR] &= ~val;
				chkIntr(ix);
				break;
			}
			if (adr == Sn_RXBUF_SIZE || adr == Sn_TXBUF_SIZE) {
				if (val > 16 || (val & (val - 1)) != 0) {
					val = 0;
				}
			}
			sock_regs[ix][adr] = (byte)val;
			if (adr == Sn_CR) {
//System.err.format("add cmd[%d] %02x\n", ix, val);
				cmd.add(ix);
			}
			break;
		case 2:	// Tx buffer
			a = txbase[ix] + (adr & txmask[ix]);
			txmem[a] = (byte)val;
			break;
		case 3:	// Rx buffer
			a = rxbase[ix] + (adr & rxmask[ix]);
			rxmem[a] = (byte)val;
			break;
		}
		adr = (adr + 1) & 0xffff;
		if (len > 0) {
			--len;
		}
	}

	private void setBuff(int reg, boolean tx) {
		int b = 0x0000;
		for (int x = 0; x < nsocks; ++x) {
			int m = sock_regs[x][reg] * 1024;
			if (tx) {
				// LSB is always zero...
				sock_regs[x][Sn_TX_FSR] = (byte)(m >> 8);
				txbase[x] = b;
				txmask[x] = m - 1;
			} else {
				rxbase[x] = b;
				rxmask[x] = m - 1;
			}
			b += m;
		}
	}

	private void sockIntr(int ix, int ir) {
		ir &= sock_regs[ix][Sn_IMR];
		if (ir != 0) {
			sock_regs[ix][Sn_IR] |= ir;
			chkIntr(ix);
		}
	}

	private void doOpen(int ix) {
		// anything to do here?
		int proto = sock_regs[ix][Sn_MR] & Sn_MR_PROTO;
		if (proto != Sn_MR_TCP) {
System.err.format("%s: unsupported protocol %d\n", name, proto);
			sock_regs[ix][Sn_SR] = SOCK_CLOSED;
			return;
		}
		sock_regs[ix][Sn_SR] = SOCK_INIT;
		//chkIntr(ix);
	}

	private void doLstn(int ix) {
		// TODO: make this more robust... recoverable...
		// TODO: get better at cleaning up unused Listeners...
		int pt = sockGet16(ix, Sn_PORT);
		if (!lstns.containsKey(pt)) {
			Listener l = new Listener(pt);
			if (!l.valid()) {
				sock_regs[ix][Sn_SR] = SOCK_CLOSED;
				return;
			}
			lstns.put(pt, l);
		} else {
			Listener l = lstns.get(pt);
			if (!l.valid()) {
				sock_regs[ix][Sn_SR] = SOCK_CLOSED;
				lstns.remove(pt);
				return;
			}
		}
		sock_regs[ix][Sn_SR] = SOCK_LISTEN;
	}

	private void doConn(int ix) {
		String ipa = String.format("%d.%d.%d.%d",
			sock_regs[ix][Sn_DIPR] & 0xff,
			sock_regs[ix][Sn_DIPR+1] & 0xff,
			sock_regs[ix][Sn_DIPR+2] & 0xff,
			sock_regs[ix][Sn_DIPR+3] & 0xff);
		int pt = sockGet16(ix, Sn_DPORT);
		try {
			synchronized(this) {
				InetAddress ia = InetAddress.getByName(ipa);
				socks[ix] = new Socket(ia, pt);
				socki[ix] = socks[ix].getInputStream();
				socko[ix] = socks[ix].getOutputStream();
				dipr[ix][0] = sock_regs[ix][Sn_DIPR + 0];
				dipr[ix][1] = sock_regs[ix][Sn_DIPR + 1];
				dipr[ix][2] = sock_regs[ix][Sn_DIPR + 2];
				dipr[ix][3] = sock_regs[ix][Sn_DIPR + 3];
				dport[ix][0] = sock_regs[ix][Sn_DPORT + 0];
				dport[ix][1] = sock_regs[ix][Sn_DPORT + 1];
				sock_regs[ix][Sn_SR] = SOCK_ESTABLISHED;
				sockIntr(ix, Sn_IR_CON);
			}
			Thread t = new Thread(new Receiver(ix));
			t.start();
			//ledConn();
		} catch (Exception ee) {
ee.printStackTrace();
			doDisc(ix);
			// TODO: what is the right error state?
			sock_regs[ix][Sn_SR] = SOCK_CLOSE_WAIT;
		}
	}

	private int sockGet16(int ix, int reg) {
		return ((sock_regs[ix][reg] & 0xff) << 8) |
			(sock_regs[ix][reg + 1] & 0xff);
	}

	private void sockPut16(int ix, int reg, int val) {
		sock_regs[ix][reg] = (byte)(val >> 8);
		sock_regs[ix][reg + 1] = (byte)val;
	}

	private synchronized void setConn(int ix, Socket s) throws Exception {
		socks[ix] = s;
		socki[ix] = s.getInputStream();
		socko[ix] = s.getOutputStream();
		InetAddress ia = s.getInetAddress();
		byte[] ip = ia.getAddress();
		int pt = s.getPort();
		dipr[ix][0] = ip[0];
		dipr[ix][1] = ip[1];
		dipr[ix][2] = ip[2];
		dipr[ix][3] = ip[3];
		dport[ix][0] = (byte)(pt >> 8);
		dport[ix][1] = (byte)pt;
		sock_regs[ix][Sn_SR] = SOCK_ESTABLISHED;
		sockIntr(ix, Sn_IR_CON);
		//ledConn();
	}

	private synchronized void doDisc(int ix) {
		if (socks[ix] != null) try {
			//ledDisc();
			socks[ix].close();
		} catch (Exception ee) {
		}
		Arrays.fill(dport[ix], (byte)0);
		Arrays.fill(dipr[ix], (byte)0);
		socks[ix] = null;
		socki[ix] = null;
		socko[ix] = null;
		sock_regs[ix][Sn_SR] = SOCK_CLOSED;
		sockIntr(ix, Sn_IR_DISCON);
	}

	private void doClose(int ix) {
		sock_regs[ix][Sn_SR] = SOCK_CLOSED;
	}

	private void dump(String pfx, byte[] mem, int rr, int wr, int mask, int base) {
		System.err.format("%s %04x %04x %04x %04x\n", pfx, rr, wr, mask, base);
		rr &= mask;
		wr &= mask;
		int x = 16;
		while (rr != wr) {
			System.err.format(" %02x", mem[base + rr] & 0xff);
			rr = (rr + 1) & mask;
			if (--x == 0) {
				System.err.format("\n");
				x = 16;
			}
		}
		if (x != 0) {
			System.err.format("\n");
		}
	}

	private void doSend(int ix) {
//dump(">", txmem, sockGet16(ix, Sn_TX_RD), sockGet16(ix, Sn_TX_WR), txmask[ix], txbase[ix]);
		if (socko[ix] == null) {
			sockIntr(ix, Sn_IR_TIMEOUT);
			return;
		}
		//startLED();
		int rr = sockGet16(ix, Sn_TX_RD) & txmask[ix];
		int wr = sockGet16(ix, Sn_TX_WR) & txmask[ix];
		if (rr == wr) {
			return; // or... full?
		}
		int a = txbase[ix] + rr;
		int n = (wr - rr) & txmask[ix];
		int f = (txmask[ix] + 1 - n);
		sockPut16(ix, Sn_TX_FSR, f);
		try {
			if (wr < rr) {
				// must combine parts into separate buffer
				int n1 = (txmask[ix] + 1) - rr;
				byte[] buf = new byte[n];
				System.arraycopy(txmem, a, buf, 0, n1);
				System.arraycopy(txmem, txbase[ix], buf, n1, n - n1);
				socko[ix].write(buf);
			} else {
				socko[ix].write(txmem, a, n);
			}
			socko[ix].flush();
			sockPut16(ix, Sn_TX_RD, wr);
			f = txmask[ix] + 1;
			sockPut16(ix, Sn_TX_FSR, f);
			sockIntr(ix, Sn_IR_SENDOK);
		} catch (Exception ee) {
ee.printStackTrace();
			doDisc(ix);
			sockPut16(ix, Sn_TX_FSR, f);
			sockIntr(ix, Sn_IR_TIMEOUT);
		}
	}

	// This does not actually receive, it only acknowledges
	// that the host has taken the receive data and we can update pointers.
	// Host has updated Sn_RX_RD to indicate how much data they took.
	private void doRecv(int ix) {
		int rr = sockGet16(ix, Sn_RX_RD);
		int wr = sockGet16(ix, Sn_RX_WR);
//System.err.format("doRecv %04x %04x (%04x)\n", rr, wr, sockGet16(ix, Sn_RX_RSR));
		int len = (wr - rr) & rxmask[ix];
		sockPut16(ix, Sn_RX_RSR, len);
		sockPut16(ix, Sn_RX_RD, wr);
//System.err.format("doRecv(): RSR = %04x, WR = %04x (%04x) RD = %04x (%04x)\n",
//sockGet16(ix, Sn_RX_RSR), sockGet16(ix, Sn_RX_WR), wr, sockGet16(ix, Sn_RX_RD), rr);
		// User *must* clear this bit!
		if (len != 0) {
			sockIntr(ix, Sn_IR_RECV);
		}
	}

	private void doSock(int ix) {
		lastCmd[ix] = sock_regs[ix][Sn_CR] & 0xff;
		// These commands *are* complete upon "accepted"
		switch (lastCmd[ix]) {
		case OPEN:	// Sn_SR: SOCK_CLOSED => SOCK_INIT
				// (any other indicators?)
				// TODO: does OPEN take more time?
			doOpen(ix);
			break;
		case CLOSE:	// Sn_SR: * => SOCK_CLOSED (no net act?)
				// TODO: does CLOSE take more time?
			doClose(ix);
			break;
		case RECV:	// Sn_SR: no change
				// Sn_IR: no change?
				// TODO: does RECV take more time?
			doRecv(ix);
			break;
		}
		// This indicates "command accepted" -
		// NOT (necessarily) command complete.
		sock_regs[ix][Sn_CR] = IDLE;
		switch (lastCmd[ix]) {
		case LISTEN:
			// TODO: waits forever, or timeout?
			doLstn(ix);
			break;
		case CONNECT:	// Sn_SR: SOCK_INIT => SOCK_ESTABLISHED
				// Sn_IR: + Sn_IR_CON (SIR[n] = 1)
				//     or + Sn_IR_TIMEOUT
			doConn(ix);
			break;
		case DISCON:	// Sn_SR: * => SOCK_CLOSED
				// Sn_IR: + Sn_IR_DISCON (SIR[n] = 1)
				//     or + Sn_IR_TIMEOUT
			doDisc(ix);
			break;
		case SEND:	// Sn_SR: no change
				// Sn_IR: + Sn_IR_SENDOK (SIR[n] = 1)
				//     or + Sn_IR_TIMEOUT
			doSend(ix);
			break;
		case SEND_MAC:	// Sn_SR: no change
			// TODO: implement?
			break;
		case SEND_KEEP:	// Sn_SR: no change
			// TODO: implement?
			break;
		}
	}

	private void dumpMsg(String tag, byte[] msg, int start, int len) {
		for (int x = 0; x < len; ++x) {
			System.err.format("%s%02x", tag, msg[start + x] & 0xff);
			tag = " ";
		}
	}

	// Blocking, thread only runs as long as connection is working.
	private boolean recv(int ix) {
		if (socks[ix] == null) {
			// doDisc(ix) was done by others
			return false;
		}
		// Sn_RX_WR only changes here
		int wr = sockGet16(ix, Sn_RX_WR) & rxmask[ix];
		int n = (rxmask[ix] + 1) - wr;	// space until wrap
		int a = rxbase[ix] + wr;	// address of space
		int l = -1;
		try {
			// sleep here for first char, detect broken connections
			l = socki[ix].read(rxmem, a, n);
			if (l < 0) {
				doDisc(ix);
				return false;
			} else if (l == 0) {
				return true;
			}
//dumpMsg("<", rxmem, a, l);
			if (l == n && socki[ix].available() > 0) {
				wr = 0;
				a = rxbase[ix] + wr;
				l = (rxmask[ix] + 1) - n;
				l = socki[ix].read(rxmem, a, l);
				if (l > 0) {
//dumpMsg(":", rxmem, a, l);
					wr += l;
					l += n;
				} else {
					l = n;
				}
			} else {
				wr += l;
			}
//System.err.format("\n");
			wr &= rxmask[ix];
			synchronized (this) {
				sockPut16(ix, Sn_RX_WR, wr);
				// overflow possible?
				sockPut16(ix, Sn_RX_RSR,
					sockGet16(ix, Sn_RX_RSR) + l);
//System.err.format("recv(): RSR = %04x, WR = %04x RD = %04x\n",
//sockGet16(ix, Sn_RX_RSR), sockGet16(ix, Sn_RX_WR), sockGet16(ix, Sn_RX_RD));
				sockIntr(ix, Sn_IR_RECV);
			}
//dump("<", rxmem, sockGet16(ix, Sn_RX_RD), sockGet16(ix, Sn_RX_WR), rxmask[ix], rxbase[ix]);
		} catch (Exception ee) {
			//ee.printStackTrace();
			doDisc(ix);
			return false;
		}
		return true;
	}

	class Receiver implements Runnable {
		private int ix;
		public Receiver(int ix) {
			this.ix = ix;
		}
		public void run() {
			// TODO: no need for sleep? since recv() is blocking now?
			while (recv(ix)) {
				try {
					Thread.sleep(10);
				} catch (Exception ee) {}
			}
		}
	}

	public void run() {
		while (true) {
			int ix = NO_CMD;
			try {
				ix = cmd.take();
			} catch (Exception ee) { }
			if (ix == SW_RESET) {
				doSwReset();
				for (int x = 0; x < mac.length; ++x) {
					com_regs[SHAR + x] = mac[x];
				}
				try {
					// WIZ850io docs say 50mS...
					Thread.sleep(50);
				} catch (Exception ee) {}
				ready = true;
				continue;
			} else if (ix == PHY_RESET) {
				try {
					// however long it takes for LINK UP...
					Thread.sleep(50);
				} catch (Exception ee) {}
				com_regs[PHYCFG] |= 0x07;	// LINK UP
				continue;
			} else if (ix < 0) {
				// TODO: abort or ignore?
				continue;
			}
			doSock(ix);
		}
	}

	public String dumpDebug() {
		String ret = String.format("%s IRQ %d\n", name, irq);
		ret += String.format("MR = %02x  ADR = %04x  CTL = %02x SPI = %d\n",
			com_regs[MR] & 0xff, adr, ctl, spi);
		ret += String.format("IR = %02x  IMR = %02x  SIR = %02x  SIMR = %02x\n",
			com_regs[IR] & 0xff,
			com_regs[IMR] & 0xff,
			com_regs[SIR] & 0xff,
			com_regs[SIMR] & 0xff);
		ret += String.format("SIPR = %d.%d.%d.%d\n",
			com_regs[SIPR] & 0xff,
			com_regs[SIPR+1] & 0xff,
			com_regs[SIPR+2] & 0xff,
			com_regs[SIPR+3] & 0xff);
		ret += String.format("SUBR = %d.%d.%d.%d\n",
			com_regs[SUBR] & 0xff,
			com_regs[SUBR+1] & 0xff,
			com_regs[SUBR+2] & 0xff,
			com_regs[SUBR+3] & 0xff);
		ret += String.format("GAR  = %d.%d.%d.%d\n",
			com_regs[GAR] & 0xff,
			com_regs[GAR+1] & 0xff,
			com_regs[GAR+2] & 0xff,
			com_regs[GAR+3] & 0xff);
		ret += String.format("SHAR = %02x:%02x:%02x:%02x:%02x:%02x\n",
			com_regs[SHAR] & 0xff,
			com_regs[SHAR+1] & 0xff,
			com_regs[SHAR+2] & 0xff,
			com_regs[SHAR+3] & 0xff,
			com_regs[SHAR+4] & 0xff,
			com_regs[SHAR+5] & 0xff);
		ret += String.format("PMAGIC = %02x\n", com_regs[PMAGIC] & 0xff);
		for (int x = 0; x < nsocks; ++x) {
			ret += String.format("S%d_MR = %02x  CR = %02x (%02x)  " +
				"IR = %02x  SR = %02x %dK/%dK\n",
				x,
				sock_regs[x][Sn_MR] & 0xff,
				sock_regs[x][Sn_CR] & 0xff, lastCmd[x],
				sock_regs[x][Sn_IR] & 0xff,
				sock_regs[x][Sn_SR] & 0xff,
				sock_regs[x][Sn_RXBUF_SIZE] & 0xff,
				sock_regs[x][Sn_TXBUF_SIZE] & 0xff);
			ret += String.format("   DIPR = %d.%d.%d.%d  DPORT = %d (%04x)",
				sock_regs[x][Sn_DIPR] & 0xff,
				sock_regs[x][Sn_DIPR+1] & 0xff,
				sock_regs[x][Sn_DIPR+2] & 0xff,
				sock_regs[x][Sn_DIPR+3] & 0xff,
				sockGet16(x, Sn_DPORT),
				sockGet16(x, Sn_PORT));
			ret += String.format("   ripr = %d.%d.%d.%d  rport = %d KA = %d\n",
				dipr[x][0] & 0xff, dipr[x][1] & 0xff,
				dipr[x][2] & 0xff, dipr[x][3] & 0xff,
				((dport[x][0] & 0xff) << 8) | (dport[x][1] & 0xff),
				sock_regs[x][Sn_KPALVTR] & 0xff);
			ret += String.format("   TX_FSR = %04x  RD = %04x  WR = %04x",
				sockGet16(x, Sn_TX_FSR),
				sockGet16(x, Sn_TX_RD),
				sockGet16(x, Sn_TX_WR));
			ret += String.format("   RX_RSR = %04x  RD = %04x (WR = %04x)\n",
				sockGet16(x, Sn_RX_RSR),
				sockGet16(x, Sn_RX_RD),
				sockGet16(x, Sn_RX_WR));
		}
		return ret;
	}
}
