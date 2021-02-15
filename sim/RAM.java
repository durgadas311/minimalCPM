// Copyright (c) 2021 Douglas Miller <durgadas311@gmail.com>

import java.util.Properties;
import java.io.*;

public class RAM implements Memory {
	private byte[] mem;
	private int msk;
	static final int dflt = 64 * 1024;
	int paddr;

	public RAM(Properties props, int paddr) {
		this.paddr = paddr;
		String s = props.getProperty("ram_size");
		int len = getSize(s);
		mem = new byte[len];
		msk = len - 1;
		System.err.format("RAM %d bytes\n", len);
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

	public int read(boolean rom, int bank, int address) {
		address &= msk;
		return mem[address] & 0xff;
	}

	public int read(int address) {
		return read(false, 0, address);
	}

	public void write(int address, int value) {
		address &= msk;
		mem[address] = (byte)value;
	}

	public void reset() { }

	public void dumpCore(String file) {
		try {
			RandomAccessFile core = new RandomAccessFile(file, "rw");
			core.seek(paddr);
			core.write(mem);
			core.close();
		} catch (Exception ee) {
			ee.printStackTrace();
		}
	}
}
