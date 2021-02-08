// Copyright (c) 2021 Douglas Miller <durgadas311@gmail.com>

import java.util.Properties;

public class SimpleRAM_ROM implements Memory {
	private Memory mem;
	private Memory rom;

	public SimpleRAM_ROM(Properties props) {
		mem = new RAM(props);
		rom = new ROM(props);
	}

	public int read(boolean ro, int bank, int address) {
		int val = 0;
		if ((address & 0x80000) == 0) {
			val = rom.read(address);
		} else {
			val = mem.read(address);
		}
		return val;
	}

	public int read(int address) {
		return read(false, 0, address);
	}

	public void write(int address, int value) {
		if ((address & 0x80000) == 0) {
			// no write to ROM
		} else {
			mem.write(address, value);
		}
	}

	public void reset() {
		mem.reset();
		rom.reset();
	}

	public void dumpCore(String file) {
		// TODO:
	}
}
