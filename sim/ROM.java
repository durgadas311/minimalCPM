// Copyright (c) 2021 Douglas Miller <durgadas311@gmail.com>

import java.util.Properties;
import java.io.*;

public class ROM implements Memory {
	private byte[] mem;
	private int msk;
	static final int dflt = 8 * 1024;

	public ROM(Properties props) {
		String s = props.getProperty("rom_size");
		int len = getSize(s);
		mem = new byte[len];
		msk = len - 1;
		s = props.getProperty("monitor_rom");
		if (s == null) {
			s = "mincpm.rom";
		}
		try {
			InputStream fi = openROM(s);
			fi.read(mem);
			fi.close();
		} catch (Exception ee) {
			System.err.format("ROM %s: %s\n", s, ee.getMessage());
			return;
		}
		System.err.format("ROM %d bytes \"%s\"\n", len, s);
	}

	private int getSize(String s) {
		if (s == null) {
			return dflt;
		}
		int mult = 1;
		int len;
		if (s.matches("[0-9]+[Kk]")) {
			mult = 1024;
			s = s.substring(0, s.length() - 1);
		}
		try {
			len = Integer.valueOf(s) * mult;
		} catch (Exception ee) {
			System.err.format("Invalid memory size \"%s\", using %d\n",
					s, dflt);
			return dflt;
		}
		if (len < 1024 || (len & (len - 1)) != 0) { // not a power of 2...
			System.err.format("Invalid memory size %d, using %d\n",
					s, dflt);
			return dflt;
		}
		return len;
	}

	private InputStream openROM(String rom) throws Exception {
		Exception ex = null;
		try {
			return new FileInputStream(rom);
		} catch (Exception ee) {
			ex = ee;
		}
		try {
			InputStream fi = this.getClass().getResourceAsStream(rom);
			if (fi != null) {
				return fi;
			}
		} catch (Exception ee) {
			ex = ee;
		}
		if (ex != null) {
			throw ex;
		}
		throw new FileNotFoundException(rom);
	}

	public int read(boolean rom, int bank, int address) {
		address &= 0x1fff; // 8K only
		return mem[address] & 0xff;
	}

	public int read(int address) {
		return read(false, 0, address);
	}

	public void write(int address, int value) {
		// No write to ROM
	}

	public void reset() { }

	public void dumpCore(String file) {
	}
}
