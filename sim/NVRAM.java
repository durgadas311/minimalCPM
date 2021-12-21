// Copyright (c) 2019 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.util.Properties;
import java.util.Random;
import java.io.*;
import java.net.*;
import java.awt.event.*;
import javax.swing.Timer;

public class NVRAM implements SPIDevice, Runnable {
	// TODO: some are specific to 25LC512 and/or SEEPROM.
	private static final int cmd_READ = 0b00000011;
	private static final int cmd_WRITE= 0b00000010;
	private static final int cmd_WREN = 0b00000110;
	private static final int cmd_WRDI = 0b00000100;
	private static final int cmd_RDSR = 0b00000101;
	private static final int cmd_WRSR = 0b00000001;
	private static final int cmd_PE   = 0b01000010;	// page erase
	private static final int cmd_SE   = 0b11011000;	// sector erase
	private static final int cmd_CE   = 0b11000111;	// chip erase
	private static final int cmd_RDID = 0b10101011;	// rel from deep pwr down
	private static final int cmd_DPD  = 0b10111001;	// deep pwr down

	private static final int sr_WIP  = 0b00000001;	// write in progress
	private static final int sr_WEL  = 0b00000010;	// write enable latch
	private static final int sr_BPM  = 0b00001100;	// block prot mask
	private static final int sr_BP0  = 0b00000000;	// nothing protected
	private static final int sr_BP1  = 0b00000100;	// top 16K protected
	private static final int sr_BP2  = 0b00001000;	// top 32K protected
	private static final int sr_BP4  = 0b00001100;	// 64K (all) protected
	private static final int sr_WPEN = 0b10000000;	// /WP enable
	private static final int sr_RW   = (sr_WPEN|sr_BPM);	// R/W bits in SR

	private String name;

	private byte[] mem;
	private byte[] pag;
	private int pgx;
	private int cr;
	private int sr;
	private int adr;
	private int spi;
	private int prot;	// max address that can be written
	private int _miso;
	private int _mosi;
	private java.util.concurrent.LinkedBlockingDeque<Integer> cmd;
	private RandomAccessFile backingstore;
	private int cap;
	private int pgz;	// page size
	private int scz;	// sector size
	private int twc;
	private int tce;

	public NVRAM(Properties props, String id) {
		name = id;
		String pfx = id.toLowerCase();
		cmd = new java.util.concurrent.LinkedBlockingDeque<Integer>();
		// TODO: decode capacity/characteristics from 'id'
		twc = 5;	// Twc = 5mS (25LC512)
		tce = 10;	// CE/SE = 10mS (25LC512)
		cap = 65536;	// 25LC512 is 64K bytes
		pgz = 128;	// 25LC512 page is 128 bytes
		scz = 16384;	// 25LC512 sector is 16K bytes
		String s = props.getProperty(pfx + "_backingstore");
		if (s == null) {
			s = pfx + ".nvram";
		}
		try {
			backingstore = new RandomAccessFile(s, "rw");
			if (backingstore.length() == 0) {
				System.err.format("%s: Initializing new backingstore %s as %dK\n",
							id, s, cap / 1024);
				mem = new byte[cap];
				Arrays.fill(mem, (byte)0xff);
				backingstore.write(mem);
			} else {
				// force page boundary
				cap = (int)(backingstore.length() & ~(pgz - 1));
				System.err.format("%s: Using existing backingstore %s as %dK\n",
							id, s, cap / 1024);
				mem = new byte[cap];
				// TODO: some chips do this in background
				// (e.g. SRAM+FLASH "restore").
				backingstore.seek(0);
				backingstore.read(mem);
			}
		} catch (Exception ee) {
			System.err.format("%s: Unable to get backingstore %s\n", id, s);
		}
		pag = new byte[pgz];
		sr = 0;	// TODO: non-volatile bits
		prot = 0x10000;
		reset();
		Thread t = new Thread(this);
		t.start();
		System.err.format("%s SPI-NVRAM Module %dK\n", name, cap / 1024);
	}

	public void reset() {
		adr = 0;
		spi = -1;
		sr &= sr_RW;
		_miso = 0;
		_mosi = 0;
		// make a more pronounced delay before RDY
		cmd.add(-1);
	}

	// Only called for cmd_WRITE, cmd_PE, cmd_SE, cmd_CE
	private boolean chkProt(int c) {
		if ((sr & sr_WEL) == 0) {
//System.err.format("no WEL\n");
			return true;
		}
		if (c == cmd_CE) {
//System.err.format("CE prot=%04x\n", prot);
			return (prot != 0x10000);
		}
		return (adr >= prot);
	}

	public void scs(boolean on) {
		if (on) {
			// start xfer
			spi = 0;
		} else {
			// terminate xfer
			spi = -1;
			// phase III of command execution
			// start command... if...
//System.err.format("start command: %02x\n", cr);
			if (cr != cmd_READ && cr != cmd_RDSR) {
				if (cr == cmd_WRITE || cr == cmd_PE ||
						cr == cmd_SE || cr == cmd_CE) {
					setSr(sr_WIP);
				}
//System.err.format("put command: %02x\n", cr);
				cmd.add(cr);
			}
		}
	}

	public boolean status() { return true; }

	public int miso_() {
		_miso = _mosi;
		return _miso;
	}

	public int miso() {
		if (spi < 0) {
//System.err.format("MISO stray %02x\n", 0xff);
			return 0xff;
		}
		int val = _miso;
		if (spi != 3) {
			// Write mode, ignore reads?
			// Full behavior unclear...
			_miso = _mosi;
//System.err.format("xread %02x\n", _miso);
		} else {
			_miso = read();
//System.err.format("read %02x\n", _miso);
		}
		return val;
	}

	public void mosi(int val) {
		if (spi < 0) {
//System.err.format("MOSI stray %02x\n", val);
			return;
		}
		_miso = _mosi;
		_mosi = val;
		switch (spi) {
		case 0:
			cr = (val & 0xff);
//System.err.format("CR = %02x\n", cr);
			++spi;
			// phase I of command execution
			// cmd_WREN, cmd_WRDI, cmd_CE, cmd_RDID, cmd_DPD end here...
			// cmd_RDSR, cmd_WRSR jump to spi_3.
			if (cr == cmd_RDSR || cr == cmd_WRSR) {
				spi = 3;
			} else if (cr == cmd_WREN || cr == cmd_WRDI ||
				cr == cmd_CE || cr == cmd_RDID || cr == cmd_DPD) {
//System.err.format("WREN/WRDI\n");
				spi = 4; // ignore data until /SCS
			}
			return;
		case 1:
			adr &= 0x00ff;
			adr |= (val & 0x7f) << 8;
			++spi;
			return;
		case 2:
			adr &= 0xff00;
			adr |= (val & 0xff);
			++spi;
//System.err.format("CR = %02x adr = %04x (%d)\n", cr, adr, spi);
			if (cr == cmd_WRITE) {
				pgx = adr & (pgz - 1);
				System.arraycopy(mem, adr & ~(pgz - 1), pag, 0, pgz);
//System.err.format("NVRAM preload page %04x\n", adr);
			}
			return;
		}
		// phase II of command execution
		write(val);
	}

	public void sclk(int bits) {}

	private int read() {
		int val = 0;
		if (spi > 3) {
//System.err.format("MISO overrun %02x\n", val);
			return val; // TODO: real behavior?
		}
		switch (cr) {
		case cmd_READ:
//System.err.format("READ %02x\n", adr);
			val = mem[adr] & 0xff;
			adr = (adr + 1) & (mem.length - 1);
			break;
		case cmd_RDSR:
			val = sr;
			spi = 4; // TODO: real behavior?
			break;
		}
		return val;
	}

	private void write(int val) {
		if (spi > 3) {
//System.err.format("MOSI overrun %02x\n", val);
			return;
		}
		switch (cr) {
		case cmd_WRITE:
			pag[pgx] = (byte)val;
			pgx = (pgx + 1) & (pgz - 1);
			break;
		case cmd_WRSR:
			// TODO: sr_RW bits are non-volatile...
			// TODO: support sr_WPEN:
			// external /WP protects status reg only
			clrSr(sr_RW);
			setSr(val & sr_RW);
			switch (val & sr_BPM) {
			case sr_BP0:
				prot = 0x10000;
				break;
			case sr_BP1:
				prot = 0x0c000;
				break;
			case sr_BP2:
				prot = 0x08000;
				break;
			case sr_BP4:
				prot = 0x00000;
				break;
			}
			spi = 4;
			break;
		}
	}

	private void sync(int delay) {
		try {
			backingstore.seek(0);
			backingstore.write(mem);
			if (delay > 0) {
				Thread.sleep(delay);
			}
		} catch (Exception ee) {
			ee.printStackTrace();
		}
	}

	private synchronized void setSr(int bits) {
		sr |= bits;
	}

	private synchronized void clrSr(int bits) {
		sr &= ~bits;
	}

	// Always in thread context - may sleep...
	private void doCmd(int cmd) {
		if (cmd < 0) {
			// Power-up (RESET?)...
			// Some devices might RESTORE from NVRAM to SRAM, etc.
			// Thread.sleep(xxx);
			return;
		}
		switch (cmd) {
		case cmd_WRITE:
//System.err.format("NVRAM write page %04x\n", adr);
			if (chkProt(cmd)) break;
			System.arraycopy(pag, 0, mem, adr & ~(pgz - 1), pgz);
			// TODO: write all or just page?
			sync(twc);
			clrSr(sr_WIP);
			break;
		case cmd_WREN:
//System.err.format("WREN: set WEL\n");
			setSr(sr_WEL);
			return;
		case cmd_WRDI:
			clrSr(sr_WEL);
			return;
		case cmd_PE:
			if (chkProt(cmd)) break;
			adr &= ~(pgz - 1);
			Arrays.fill(mem, adr, adr + pgz, (byte)0xff);
			sync(twc);
			clrSr(sr_WIP);
			break;
		case cmd_SE:
			if (chkProt(cmd)) break;
			adr &= ~(scz - 1);
			Arrays.fill(mem, adr, adr + scz, (byte)0xff);
			sync(tce);
			clrSr(sr_WIP);
			break;
		case cmd_CE:
			if (chkProt(cmd)) break;
			Arrays.fill(mem, (byte)0xff);
			sync(tce);
			clrSr(sr_WIP);
			break;
		// no others?
		}
		clrSr(sr_WEL);
	}

	public void run() {
		while (true) {
			int ix = -1;
			try {
				ix = cmd.take();
			} catch (Exception ee) { }
//System.err.format("get command: %02x\n", ix);
			doCmd(ix);
		}
	}

	public String dumpDebug() {
		String ret = String.format("%s %dK %dpg\n", name, cap / 1024, pgz);
		ret += String.format("CR = %02x  SR = %02x\n",
			cr, sr);
		ret += String.format("ADR = %04x  PGX = %02x SPI = %d\n",
			adr, pgx, spi);
		ret += String.format("cmd fifo = %d\n", cmd.size());
		return ret;
	}
}
